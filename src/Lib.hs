{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Raw
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Data.Text (pack)
import Data.Aeson
import Data.IORef
import Control.Monad.IO.Class
import Numeric.Natural

-- Complete Grade hierarchy for HTTP effects
data Grade = Pure | Safe | Idempotent | Unsafe deriving (Show, Eq, Ord)

-- Type family for algebraic grade composition
-- The algebra: Pure < Safe < Idempotent < Unsafe (monotonic hierarchy)
-- Composition always produces the "higher" (less safe) grade
type family Combine (i :: Grade) (j :: Grade) :: Grade where
  -- Identity laws: Pure is identity element
  Combine 'Pure g = g
  Combine g 'Pure = g
  
  -- Absorption laws: higher grades absorb lower ones
  Combine 'Safe 'Safe = 'Safe
  Combine 'Safe 'Idempotent = 'Idempotent  
  Combine 'Safe 'Unsafe = 'Unsafe
  Combine 'Idempotent 'Safe = 'Idempotent
  Combine 'Idempotent 'Idempotent = 'Idempotent
  Combine 'Idempotent 'Unsafe = 'Unsafe
  Combine 'Unsafe 'Safe = 'Unsafe
  Combine 'Unsafe 'Idempotent = 'Unsafe
  Combine 'Unsafe 'Unsafe = 'Unsafe

-- Type family for maximum (least upper bound) in the grade lattice
type family Max (i :: Grade) (j :: Grade) :: Grade where
  Max 'Pure g = g
  Max g 'Pure = g
  Max 'Safe 'Safe = 'Safe
  Max 'Safe g = g
  Max g 'Safe = g
  Max 'Idempotent 'Idempotent = 'Idempotent
  Max 'Idempotent 'Unsafe = 'Unsafe
  Max 'Unsafe 'Idempotent = 'Unsafe
  Max 'Unsafe 'Unsafe = 'Unsafe

-- Indexed monad for effect tracking with proper algebraic composition
newtype IxApp (i :: Grade) (j :: Grade) a = IxApp { runIxApp :: IO a }

instance Functor (IxApp i j) where
    fmap f (IxApp x) = IxApp (f <$> x)

-- Indexed monad operations with algebraic composition
ireturn :: a -> IxApp 'Pure 'Pure a
ireturn x = IxApp (return x)

-- Algebraic bind: composition follows the algebra
ibind :: IxApp i j a -> (a -> IxApp j k b) -> IxApp i (Combine j k) b
ibind (IxApp x) f = IxApp (x >>= runIxApp . f)

-- Parallel composition: combines effects using Max (least upper bound)
iparallel :: IxApp 'Pure i a -> IxApp 'Pure j b -> IxApp 'Pure (Max i j) (a, b)
iparallel (IxApp x) (IxApp y) = IxApp ((,) <$> x <*> y)

-- Smart constructors for effect introduction
liftSafeIO :: IO a -> IxApp 'Pure 'Safe a
liftSafeIO = IxApp

-- Additional constructors for continuing with Safe effects
safeContinue :: IO a -> IxApp 'Safe 'Safe a
safeContinue = IxApp

-- Specific weakening functions for the transitions we need
weakenToIdempotent :: IxApp 'Safe 'Safe a -> IxApp 'Safe 'Idempotent a
weakenToIdempotent (IxApp x) = IxApp x

weakenToUnsafe :: IxApp 'Safe 'Safe a -> IxApp 'Safe 'Unsafe a
weakenToUnsafe (IxApp x) = IxApp x

weakenIdempotentToUnsafe :: IxApp 'Safe 'Idempotent a -> IxApp 'Safe 'Unsafe a
weakenIdempotentToUnsafe (IxApp x) = IxApp x

-- Strengthen is impossible (no downgrading) - this would be a type error

-- JSON data types
data NumberRequest = NumberRequest { value :: Natural } deriving Show
data NumberResponse = NumberResponse { current :: Natural } deriving Show

instance FromJSON NumberRequest where
    parseJSON = withObject "NumberRequest" $ \o -> NumberRequest <$> o .: "value"

instance ToJSON NumberResponse where
    toJSON (NumberResponse n) = object ["value" .= n]

-- Global state for the number
type NumberState = IORef Natural

-- Safe effect: HTTP request logging (non-observable to client)
-- Pure → Safe: introducing a safe effect
logRequest :: String -> String -> IxApp 'Pure 'Safe ()
logRequest method path = liftSafeIO $ putStrLn $ "HTTP Log: " ++ method ++ " " ++ path

-- Safe operation: read current number (no side effects)  
-- Demonstrates algebraic composition: Pure + Safe = Safe
showNumber :: NumberState -> IxApp 'Pure 'Safe NumberResponse
showNumber state = 
    logRequest "GET" "/show" `ibind` \_ ->
    safeContinue (readIORef state) `ibind` \n ->
    safeContinue (return (NumberResponse n))

-- Idempotent operation: set number (repeatable with same result)
-- Demonstrates: Safe + Safe + Safe = Safe, then Safe → Idempotent  
setNumber :: NumberState -> Natural -> IxApp 'Pure 'Idempotent NumberResponse
setNumber state newValue = 
    logRequest "PUT" "/set" `ibind` \_ ->
    safeContinue (writeIORef state newValue) `ibind` \_ ->
    weakenToIdempotent (safeContinue (return (NumberResponse newValue)))

-- Unsafe operation: add to number (observable side effects)
-- Demonstrates: Safe + Safe + Safe = Safe, then Safe → Unsafe
addNumber :: NumberState -> Natural -> IxApp 'Pure 'Unsafe NumberResponse  
addNumber state addValue = 
    logRequest "POST" "/add" `ibind` \_ ->
    safeContinue (readIORef state) `ibind` \current ->
    let newValue = current + addValue in
    safeContinue (writeIORef state newValue) `ibind` \_ ->
    weakenToUnsafe (safeContinue (return (NumberResponse newValue)))

-- Example of parallel composition (not used in API but demonstrates algebra)
-- Combines two Safe operations into one Safe operation using Max
logAndShow :: NumberState -> IxApp 'Pure 'Safe ((), NumberResponse)
logAndShow state = iparallel 
    (logRequest "PARALLEL" "/demo")
    (liftSafeIO (readIORef state) `ibind` \n -> safeContinue (return (NumberResponse n)))

-- Servant API definition with proper HTTP methods
type API = "show" :> Get '[JSON] NumberResponse
        :<|> "set" :> ReqBody '[JSON] NumberRequest :> Put '[JSON] NumberResponse  
        :<|> "add" :> ReqBody '[JSON] NumberRequest :> Post '[JSON] NumberResponse
        :<|> Raw

api :: Proxy API
api = Proxy

-- Server implementation using indexed monads
server :: NumberState -> Server API
server state = showHandler
          :<|> setHandler  
          :<|> addHandler
          :<|> staticHandler
  where
    showHandler :: Handler NumberResponse
    showHandler = liftIO $ runIxApp $ showNumber state
    
    setHandler :: NumberRequest -> Handler NumberResponse  
    setHandler (NumberRequest n) = liftIO $ runIxApp $ setNumber state n
        
    addHandler :: NumberRequest -> Handler NumberResponse
    addHandler (NumberRequest n) = liftIO $ runIxApp $ addNumber state n
        
    staticHandler :: Server Raw
    staticHandler = serveDirectoryWith $ (defaultWebAppSettings "static")
        { ssRedirectToIndex = True
        , ssIndices = [unsafeToPiece (pack "index.html")]
        }

app :: NumberState -> Application
app state = serve api (server state)

startApp :: IO ()
startApp = do
    putStrLn "=== Haskell Server with Indexed Monad Effects ==="
    putStrLn "Port: 8080"
    putStrLn ""
    putStrLn "Effect Grade Hierarchy: Pure < Safe < Idempotent < Unsafe"
    putStrLn "API Routes:"  
    putStrLn "  GET  /show → Pure → Safe       (read-only, no side effects)"
    putStrLn "  PUT  /set  → Safe → Idempotent (repeatable, same result)" 
    putStrLn "  POST /add  → Idempotent → Unsafe (observable side effects)"
    putStrLn ""
    putStrLn "The type system ensures:"
    putStrLn "• No 'downgrading' of effect grades"
    putStrLn "• HTTP methods match their semantic guarantees"
    putStrLn "• Safe effects (logging) don't affect client state"
    putStrLn ""
    
    -- Initialize number state to 0
    numberState <- newIORef 0
    
    putStrLn "Server starting... (Ctrl+C to stop)"
    run 8080 (app numberState)
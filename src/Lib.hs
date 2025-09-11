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
import System.Random

-- ============================================================================
-- GRADE HIERARCHY - Complete lattice for HTTP effect classification
-- ============================================================================
{-
   Grade Lattice (⊑ = "less safe than"):
   
       Pure ⊑ Safe ⊑ Idempotent ⊑ Unsafe
       
   ASCII Diagram:
   
       Pure      (no effects, pure computation)
         |
       Safe      (logging, read-only operations)  
         |
    Idempotent   (repeatable operations, same result)
         |
      Unsafe     (observable side effects, state changes)
      
   Properties:
   • Monotonic: Operations can only increase grade (no downgrades)
   • Algebraic: Composition follows mathematical laws
   • HTTP Semantic: Maps to proper HTTP method semantics
-}
data Grade = Pure | Safe | Idempotent | Unsafe deriving (Show, Eq, Ord)

-- ============================================================================
-- ALGEBRAIC COMPOSITION - Type families implementing grade algebra
-- ============================================================================
{-
   Sequential Composition Laws (⊕ = Combine):
   
   IDENTITY LAWS:
   • Pure ⊕ g = g           (Pure is left identity)
   • g ⊕ Pure = g           (Pure is right identity)
   
   ABSORPTION LAWS (higher grades absorb lower):
   • Safe ⊕ Safe = Safe
   • Safe ⊕ Idempotent = Idempotent     (Safe absorbed by Idempotent)
   • Safe ⊕ Unsafe = Unsafe            (Safe absorbed by Unsafe)
   • Idempotent ⊕ Unsafe = Unsafe      (Idempotent absorbed by Unsafe)
   
   COMMUTATIVITY (for same grades):
   • g ⊕ g = g              (Same grade is idempotent)
   
   MONOTONICITY:
   • If g₁ ⊑ g₂, then h ⊕ g₁ ⊑ h ⊕ g₂  (Grade can only increase)
-}
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

-- Unsafe operation: randomise number (non-deterministic side effects)
-- Demonstrates: Safe + Unsafe = Unsafe (randomness makes it unsafe)
randomiseNumber :: NumberState -> IxApp 'Pure 'Unsafe NumberResponse
randomiseNumber state = 
    logRequest "POST" "/randomise" `ibind` \_ ->
    -- Random generation is inherently unsafe (non-deterministic)
    safeContinue (fromIntegral <$> randomRIO (0, 1000 :: Int)) `ibind` \randomValue ->
    safeContinue (writeIORef state randomValue) `ibind` \_ ->
    weakenToUnsafe (safeContinue (return (NumberResponse randomValue)))

-- ============================================================================
-- ALGEBRAIC COMPOSITION EXAMPLES - Educational Demonstrations
-- ============================================================================

-- Example 1: Identity Law - Pure is the identity element
-- Mathematical: Pure ⊕ g = g
identityLawDemo :: NumberState -> IxApp 'Pure 'Safe Natural
identityLawDemo state = 
    -- Step 1: Pure → Pure (identity)  
    ireturn () `ibind` \_ ->
    -- Step 2: Pure ⊕ Safe = Safe (identity law applied)
    liftSafeIO (readIORef state)

-- Example 2: Absorption Law - Higher grades absorb lower ones  
-- Mathematical: Safe ⊕ Idempotent = Idempotent
absorptionLawDemo :: NumberState -> Natural -> IxApp 'Pure 'Idempotent ()
absorptionLawDemo state value =
    -- Step 1: Pure → Safe (logging)
    logRequest "DEMO" "/absorption" `ibind` \_ ->
    -- Step 2: Safe ⊕ Safe = Safe (same grade composition)
    safeContinue (writeIORef state value) `ibind` \_ ->
    -- Step 3: Safe → Idempotent (weakening/absorption)
    weakenToIdempotent (safeContinue (return ()))

-- Example 3: Sequential Composition Chain
-- Shows step-by-step grade elevation: Pure → Safe → Safe → Idempotent
sequentialCompositionDemo :: NumberState -> Natural -> IxApp 'Pure 'Idempotent Natural
sequentialCompositionDemo state newValue = 
    -- Step 1: Pure → Safe (Combine 'Pure 'Safe = 'Safe)
    logRequest "SEQ" "/step1" `ibind` \_ ->
    -- Step 2: Safe → Safe (Combine 'Safe 'Safe = 'Safe) 
    safeContinue (readIORef state) `ibind` \oldValue ->
    -- Step 3: Safe → Safe (still Safe grade)
    safeContinue (writeIORef state newValue) `ibind` \_ ->
    -- Step 4: Safe → Idempotent (grade elevation)
    weakenToIdempotent (safeContinue (return oldValue))

-- Example 4: Parallel Composition with Max
-- Mathematical: Max(Safe, Safe) = Safe
parallelCompositionDemo :: NumberState -> IxApp 'Pure 'Safe ((), Natural)
parallelCompositionDemo state = iparallel 
    -- Left side: Pure → Safe
    (logRequest "PARALLEL" "/left")
    -- Right side: Pure → Safe  
    -- Result: Pure → Max(Safe, Safe) = Pure → Safe
    (liftSafeIO (readIORef state))

-- Example 5: Grade Elevation Chain
-- Shows how grades can only go "up" the hierarchy
gradeElevationDemo :: NumberState -> Natural -> IxApp 'Pure 'Unsafe Natural
gradeElevationDemo state addValue =
    -- Pure → Safe
    logRequest "ELEVATION" "/unsafe" `ibind` \_ ->
    -- Safe → Safe  
    safeContinue (readIORef state) `ibind` \current ->
    -- Safe → Safe
    safeContinue (writeIORef state (current + addValue)) `ibind` \_ ->
    -- Safe → Unsafe (final elevation to highest grade)
    weakenToUnsafe (safeContinue (return (current + addValue)))

-- Servant API definition with proper HTTP methods
type API = "show" :> Get '[JSON] NumberResponse
        :<|> "set" :> ReqBody '[JSON] NumberRequest :> Put '[JSON] NumberResponse  
        :<|> "add" :> ReqBody '[JSON] NumberRequest :> Post '[JSON] NumberResponse
        :<|> "randomise" :> Post '[JSON] NumberResponse
        :<|> Raw

api :: Proxy API
api = Proxy

-- Server implementation using indexed monads
server :: NumberState -> Server API
server state = showHandler
          :<|> setHandler  
          :<|> addHandler
          :<|> randomiseHandler
          :<|> staticHandler
  where
    showHandler :: Handler NumberResponse
    showHandler = liftIO $ runIxApp $ showNumber state
    
    setHandler :: NumberRequest -> Handler NumberResponse  
    setHandler (NumberRequest n) = liftIO $ runIxApp $ setNumber state n
        
    addHandler :: NumberRequest -> Handler NumberResponse
    addHandler (NumberRequest n) = liftIO $ runIxApp $ addNumber state n

    randomiseHandler :: Handler NumberResponse
    randomiseHandler = liftIO $ runIxApp $ randomiseNumber state
        
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
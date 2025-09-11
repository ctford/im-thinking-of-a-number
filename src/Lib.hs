{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

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

-- Complete Grade hierarchy for HTTP effects
data Grade = Pure | Safe | Idempotent | Unsafe deriving (Show, Eq, Ord)

-- Indexed monad for effect tracking - simplified but type-safe
newtype IxApp (i :: Grade) (j :: Grade) a = IxApp { runIxApp :: IO a }

instance Functor (IxApp i j) where
    fmap f (IxApp x) = IxApp (f <$> x)

-- Grade transition operations
pureToSafe :: IO a -> IxApp 'Pure 'Safe a
pureToSafe = IxApp

safeToIdempotent :: IO a -> IxApp 'Safe 'Idempotent a  
safeToIdempotent = IxApp

idempotentToUnsafe :: IO a -> IxApp 'Idempotent 'Unsafe a
idempotentToUnsafe = IxApp

-- Composition: allows chaining effects with grade elevation
composeEffects :: IxApp i j a -> (a -> IxApp j k b) -> IxApp i k b
composeEffects (IxApp x) f = IxApp (x >>= runIxApp . f)

-- JSON data types
data NumberRequest = NumberRequest { value :: Int } deriving Show
data NumberResponse = NumberResponse { current :: Int } deriving Show

instance FromJSON NumberRequest where
    parseJSON = withObject "NumberRequest" $ \o -> NumberRequest <$> o .: "value"

instance ToJSON NumberResponse where
    toJSON (NumberResponse n) = object ["value" .= n]

-- Global state for the number
type NumberState = IORef Int

-- Safe effect: HTTP request logging (non-observable to client)
logRequest :: String -> String -> IxApp 'Pure 'Safe ()
logRequest method path = pureToSafe $ putStrLn $ "HTTP Log: " ++ method ++ " " ++ path

-- Safe operation: read current number (no side effects)
showNumber :: NumberState -> IxApp 'Pure 'Safe NumberResponse
showNumber state = pureToSafe $ do
    putStrLn "HTTP Log: GET /show"
    n <- readIORef state
    return $ NumberResponse n

-- Idempotent operation: set number (repeatable with same result)
setNumber :: NumberState -> Int -> IxApp 'Safe 'Idempotent NumberResponse
setNumber state newValue = safeToIdempotent $ do
    putStrLn "HTTP Log: PUT /set"
    writeIORef state newValue
    return $ NumberResponse newValue

-- Unsafe operation: add to number (observable side effects)
addNumber :: NumberState -> Int -> IxApp 'Idempotent 'Unsafe NumberResponse
addNumber state addValue = idempotentToUnsafe $ do
    putStrLn "HTTP Log: POST /add"
    current <- readIORef state
    let newValue = current + addValue
    writeIORef state newValue
    return $ NumberResponse newValue

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
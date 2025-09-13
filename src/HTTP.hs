{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP
    ( -- Business operations
      showNumber
    , setNumber
    , addNumber
    , randomiseNumber
    , resetNumber
    -- Logging
    , logRequest
    -- API
    , API
    , api
    , server
    , app
    ) where

import Network.Wai
import Servant hiding (GET, PUT, POST, DELETE)
import Servant.API.Raw
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Data.Text (pack)
import Data.Aeson
import Data.IORef
import Control.Monad.IO.Class
import Numeric.Natural
import System.IO (hFlush, stdout)

import Effects
import Repository



-- ============================================================================
-- BUSINESS OPERATIONS - Core number operations with grade composition
-- ============================================================================

-- Safe operation: read current number (no side effects)  
-- Demonstrates algebraic composition with Monoid
showNumber :: NumberState -> Action 'Safe NumberResponse
showNumber state = 
    readState state `bind` \n ->
    logRequest GET "/number" Nothing n `bind` \_ ->
    safe (NumberResponse n)

-- Idempotent operation: set number (repeatable with same result)
-- Demonstrates: Safe <> Idempotent = Idempotent (Monoid composition)
setNumber :: NumberState -> Natural -> Action 'Idempotent NumberResponse
setNumber state newValue = 
    writeState newValue state `bind` \_ ->
    logRequest PUT "/number" (Just newValue) newValue `bind` \_ ->
    idempotent (NumberResponse newValue)

-- Unsafe operation: add to number (observable side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
addNumber :: NumberState -> Natural -> Action 'Unsafe NumberResponse  
addNumber state addValue = 
    addToState addValue state `bind` \_ ->
    readState state `bind` \newValue ->
    logRequest POST "/number/add" (Just addValue) newValue `bind` \_ ->
    unsafe (NumberResponse newValue)

-- Unsafe operation: randomise number (non-deterministic side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
randomiseNumber :: NumberState -> Action 'Unsafe NumberResponse
randomiseNumber state = 
    randomiseState state `bind` \_ ->
    readState state `bind` \randomVal ->
    logRequest POST "/number/randomise" Nothing randomVal `bind` \_ ->
    unsafe (NumberResponse randomVal)

-- Idempotent operation: reset number to zero (repeatable with same result)
-- Demonstrates: Safe <> Idempotent = Idempotent (Monoid composition)
resetNumber :: NumberState -> Action 'Idempotent NumberResponse
resetNumber state = 
    writeState 0 state `bind` \_ ->
    logRequest DELETE "/number" Nothing 0 `bind` \_ ->
    idempotent (NumberResponse 0)

-- ============================================================================
-- LOGGING - HTTP request logging infrastructure
-- ============================================================================

-- Safe effect: HTTP request logging (non-observable to client)  
-- Standard Apache/NCSA Common Log Format style logging
logRequest :: HttpVerb -> String -> Maybe Natural -> Natural -> Action 'Safe ()
logRequest verb path maybeRequestValue currentValue = Action $ do
    let requestStr = case maybeRequestValue of
          Nothing -> ""
          Just v  -> " value=" ++ show v
    let currentStr = " current=" ++ show currentValue
    putStrLn $ "- - [" ++ show verb ++ "] " ++ path ++ " 200 -" ++ requestStr ++ currentStr
    hFlush stdout

-- ============================================================================
-- API - Servant API definition and server implementation
-- ============================================================================

-- Servant API definition with proper HTTP methods
type API = "number" :> Get '[JSON] NumberResponse
        :<|> "number" :> ReqBody '[JSON] NumberRequest :> Put '[JSON] NumberResponse  
        :<|> "number" :> "add" :> ReqBody '[JSON] NumberRequest :> Post '[JSON] NumberResponse
        :<|> "number" :> "randomise" :> Post '[JSON] NumberResponse
        :<|> "number" :> Delete '[JSON] NumberResponse
        :<|> Raw

api :: Proxy API
api = Proxy

-- Server implementation using graded monads
-- Handlers must align positionally with API routes via :<|> operator
server :: NumberState -> Server API
server state = handle (showNumber state)
          :<|> (\(NumberRequest n) -> handle $ setNumber state n)
          :<|> (\(NumberRequest n) -> handle $ addNumber state n)
          :<|> handle (randomiseNumber state)
          :<|> handle (resetNumber state)
          :<|> staticHandler
  where
    handle = liftIO . runAction
        
    staticHandler :: Server Raw
    staticHandler = serveDirectoryWith $ (defaultWebAppSettings "static")
        { ssRedirectToIndex = True
        , ssIndices = [unsafeToPiece (pack "index.html")]
        }

app :: NumberState -> Application
app state = serve api (server state)
module App
    ( startApp
    -- Export main HTTP operations for testing
    , showNumber
    , setNumber
    , addNumber
    , randomiseNumber
    , resetNumber
    -- Export graded monad primitives for testing  
    , Action(..)
    , bind
    , logRequest
    -- Export data types for testing
    , NumberResponse(..)
    , HttpVerb(..)
    ) where

import Network.Wai.Handler.Warp
import Data.IORef

-- Re-export from layered modules
import Effects (Action(..), bind, NumberResponse(..), HttpVerb(..))
import HTTP (showNumber, setNumber, addNumber, randomiseNumber, resetNumber, logRequest, app)


startApp :: IO ()
startApp = do
    putStrLn "=== Haskell Server with Graded Monad Effects ==="
    putStrLn "Port: 8080"
    putStrLn ""
    putStrLn "API Routes with semantic grading:"  
    putStrLn "  GET    /number          → Safe       (read-only operations)"
    putStrLn "  PUT    /number          → Idempotent (repeatable with same result)" 
    putStrLn "  POST   /number/add      → Unsafe     (observable side effects)"
    putStrLn "  POST   /number/randomise → Unsafe     (non-deterministic effects)"
    putStrLn "  DELETE /number          → Idempotent (reset to zero, repeatable)"
    putStrLn ""
    
    -- Initialize number state to 0
    numberState <- newIORef 0
    
    putStrLn "Server starting... (Ctrl+C to stop)"
    run 8080 (app numberState)

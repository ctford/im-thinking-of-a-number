{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( startApp
    -- Export main HTTP operations for testing
    , showNumber
    , setNumber
    , addNumber
    , randomiseNumber
    -- Export graded monad primitives for testing  
    , Action(..)
    , greturn
    , gbind
    , logRequest
    -- Export IORef helper functions
    , readState
    , writeState
    , addToState
    , randomiseState
    -- Export data types for testing
    , NumberResponse(..)
    , NumberState
    , HttpVerb(..)
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
import System.IO (hFlush, stdout)

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
data Grade = Pure | Safe | Idempotent | Unsafe


-- Type-level Monoid operation implementing grade combination
-- Uses the same ordering as the Ord instance: Pure < Safe < Idempotent < Unsafe
type family (g :: Grade) <> (h :: Grade) :: Grade where
    'Pure <> g = g                    -- Pure is identity (left)
    g <> 'Pure = g                    -- Pure is identity (right)
    g <> g = g                        -- Idempotent law: g <> g = g
    'Unsafe <> _ = 'Unsafe            -- Unsafe is absorbing element
    _ <> 'Unsafe = 'Unsafe            -- Unsafe is absorbing element
    'Idempotent <> 'Safe = 'Idempotent     -- Remaining explicit case
    'Safe <> 'Idempotent = 'Idempotent     -- Remaining explicit case

-- ============================================================================
-- MONOID COMPOSITION - Grade forms a join-semilattice
-- ============================================================================
{-
   Monoid Laws for Grade composition:
   
   IDENTITY LAWS:
   • mempty <> g = g        (Pure is left identity)
   • g <> mempty = g        (Pure is right identity)
   
   ASSOCIATIVITY:
   • (g <> h) <> i = g <> (h <> i)    (Composition is associative)
   
   ABSORPTION (via max operation):
   • Safe <> Safe = Safe
   • Safe <> Idempotent = Idempotent     (max takes higher grade)
   • Safe <> Unsafe = Unsafe            (max takes higher grade)
   • Idempotent <> Unsafe = Unsafe      (max takes higher grade)
   
   IDEMPOTENCE:
   • g <> g = g             (max is idempotent)
   
   MONOTONICITY:
   • If g₁ ≤ g₂, then h <> g₁ ≤ h <> g₂  (Grade can only increase)
-}

-- Graded monad for effect tracking with single grade parameter
newtype Action (g :: Grade) a = Action { runAction :: IO a }

instance Functor (Action g) where
    fmap f (Action x) = Action (f <$> x)

-- Graded monad operations with single grade parameter
greturn :: a -> Action 'Pure a
greturn x = Action (return x)

-- Graded bind: composition uses Monoid operation (<>)
gbind :: Action g a -> (a -> Action h b) -> Action (g <> h) b
gbind (Action x) f = Action (x >>= runAction . f)



-- ============================================================================
-- IOREF OPERATIONS - State manipulation with explicit grades
-- ============================================================================

-- Read state operation (safe by nature)
-- Read-only operation, hence Safe grade
readState :: NumberState -> Action 'Safe Natural
readState state = Action (readIORef state)

-- Write state operation (idempotent by nature)
-- Same input produces same result, hence Idempotent grade
writeState :: Natural -> NumberState -> Action 'Idempotent ()
writeState value state = Action (writeIORef state value)

-- Add to state operation (unsafe by nature)
-- Non-idempotent operation, hence Unsafe grade
addToState :: Natural -> NumberState -> Action 'Unsafe ()
addToState addValue state = Action $ do
    current <- readIORef state
    writeIORef state (current + addValue)

-- Randomise state operation (unsafe by nature)
-- Non-deterministic operation, hence Unsafe grade
randomiseState :: NumberState -> Action 'Unsafe ()
randomiseState state = Action $ do
    randomVal <- fromIntegral <$> randomRIO (0, 1000 :: Int)
    writeIORef state randomVal



-- Convenience constructors for different effect grades
safe :: a -> Action 'Safe a
safe = Action . return

idempotent :: a -> Action 'Idempotent a
idempotent = Action . return

unsafe :: a -> Action 'Unsafe a
unsafe = Action . return

-- HTTP verb sum type for type-safe logging
data HttpVerb = HttpGET | HttpPUT | HttpPOST | HttpDELETE deriving Eq

instance Show HttpVerb where
    show HttpGET    = "GET"
    show HttpPUT    = "PUT" 
    show HttpPOST   = "POST"
    show HttpDELETE = "DELETE"

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
-- Standard Apache/NCSA Common Log Format style logging
logRequest :: HttpVerb -> String -> Maybe Natural -> Action 'Safe ()
logRequest verb path maybeValue = Action $ do
    let valueStr = case maybeValue of
          Nothing -> ""
          Just v  -> " value=" ++ show v
    putStrLn $ "- - [" ++ show verb ++ "] " ++ path ++ " 200 -" ++ valueStr
    hFlush stdout

-- Safe operation: read current number (no side effects)  
-- Demonstrates algebraic composition with Monoid
showNumber :: NumberState -> Action 'Safe NumberResponse
showNumber state = 
    logRequest HttpGET "/show" Nothing `gbind` \_ ->
    readState state `gbind` \n ->
    safe (NumberResponse n)


-- Idempotent operation: set number (repeatable with same result)
-- Demonstrates: Safe <> Idempotent = Idempotent (Monoid composition)
setNumber :: NumberState -> Natural -> Action 'Idempotent NumberResponse
setNumber state newValue = 
    logRequest HttpPUT "/set" (Just newValue) `gbind` \_ ->
    writeState newValue state `gbind` \_ ->
    idempotent (NumberResponse newValue)

-- Unsafe operation: add to number (observable side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
addNumber :: NumberState -> Natural -> Action 'Unsafe NumberResponse  
addNumber state addValue = 
    logRequest HttpPOST "/add" (Just addValue) `gbind` \_ ->
    addToState addValue state `gbind` \_ ->
    readState state `gbind` \newValue ->
    unsafe (NumberResponse newValue)

-- Unsafe operation: randomise number (non-deterministic side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
randomiseNumber :: NumberState -> Action 'Unsafe NumberResponse
randomiseNumber state = 
    logRequest HttpPOST "/randomise" Nothing `gbind` \_ ->
    randomiseState state `gbind` \_ ->
    readState state `gbind` \randomVal ->
    unsafe (NumberResponse randomVal)


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
    showHandler = liftIO $ runAction $ showNumber state
    
    setHandler :: NumberRequest -> Handler NumberResponse  
    setHandler (NumberRequest n) = liftIO $ runAction $ setNumber state n
        
    addHandler :: NumberRequest -> Handler NumberResponse
    addHandler (NumberRequest n) = liftIO $ runAction $ addNumber state n

    randomiseHandler :: Handler NumberResponse
    randomiseHandler = liftIO $ runAction $ randomiseNumber state
        
    staticHandler :: Server Raw
    staticHandler = serveDirectoryWith $ (defaultWebAppSettings "static")
        { ssRedirectToIndex = True
        , ssIndices = [unsafeToPiece (pack "index.html")]
        }

app :: NumberState -> Application
app state = serve api (server state)

startApp :: IO ()
startApp = do
    putStrLn "=== Haskell Server with Graded Monad Effects ==="
    putStrLn "Port: 8080"
    putStrLn ""
    putStrLn "API Routes with semantic grading:"  
    putStrLn "  GET  /show     → Safe       (read-only operations)"
    putStrLn "  PUT  /set      → Idempotent (repeatable with same result)" 
    putStrLn "  POST /add      → Unsafe     (observable side effects)"
    putStrLn "  POST /randomise → Unsafe     (non-deterministic effects)"
    putStrLn ""
    
    -- Initialize number state to 0
    numberState <- newIORef 0
    
    putStrLn "Server starting... (Ctrl+C to stop)"
    run 8080 (app numberState)

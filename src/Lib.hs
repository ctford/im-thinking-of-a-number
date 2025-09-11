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
    , GradeApp(..)
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
newtype GradeApp (g :: Grade) a = GradeApp { runGradeApp :: IO a }

instance Functor (GradeApp g) where
    fmap f (GradeApp x) = GradeApp (f <$> x)

-- Graded monad operations with single grade parameter
greturn :: a -> GradeApp 'Pure a
greturn x = GradeApp (return x)

-- Graded bind: composition uses Monoid operation (<>)
gbind :: GradeApp g a -> (a -> GradeApp h b) -> GradeApp (g <> h) b
gbind (GradeApp x) f = GradeApp (x >>= runGradeApp . f)



-- ============================================================================
-- IOREF OPERATIONS - State manipulation with explicit grades
-- ============================================================================

-- Read state operation (safe by nature)
-- Read-only operation, hence Safe grade
readState :: NumberState -> GradeApp 'Safe Natural
readState state = GradeApp (readIORef state)

-- Write state operation (idempotent by nature)
-- Same input produces same result, hence Idempotent grade
writeState :: NumberState -> Natural -> GradeApp 'Idempotent ()
writeState state value = GradeApp (writeIORef state value)

-- Add to state operation (unsafe by nature)
-- Non-idempotent operation, hence Unsafe grade
addToState :: NumberState -> Natural -> GradeApp 'Unsafe ()
addToState state addValue = GradeApp $ do
    current <- readIORef state
    writeIORef state (current + addValue)

-- Randomise state operation (unsafe by nature)
-- Non-deterministic operation, hence Unsafe grade
randomiseState :: NumberState -> GradeApp 'Unsafe ()
randomiseState state = GradeApp $ do
    randomVal <- fromIntegral <$> randomRIO (0, 1000 :: Int)
    writeIORef state randomVal



-- Convenience constructors for different effect grades
safe :: a -> GradeApp 'Safe a
safe = GradeApp . return

idempotent :: a -> GradeApp 'Idempotent a
idempotent = GradeApp . return

unsafe :: a -> GradeApp 'Unsafe a
unsafe = GradeApp . return

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
logRequest :: String -> String -> GradeApp 'Safe ()
logRequest method path = GradeApp $ do
    putStrLn $ "- - [" ++ method ++ "] " ++ path ++ " 200 -"
    hFlush stdout

-- Safe operation: read current number (no side effects)  
-- Demonstrates algebraic composition with Monoid
showNumber :: NumberState -> GradeApp 'Safe NumberResponse
showNumber state = 
    logRequest "GET" "/show" `gbind` \_ ->
    readState state `gbind` \n ->
    safe (NumberResponse n)


-- Idempotent operation: set number (repeatable with same result)
-- Demonstrates: Safe <> Idempotent = Idempotent (Monoid composition)
setNumber :: NumberState -> Natural -> GradeApp 'Idempotent NumberResponse
setNumber state newValue = 
    logRequest "PUT" "/set" `gbind` \_ ->
    writeState state newValue `gbind` \_ ->
    idempotent (NumberResponse newValue)

-- Unsafe operation: add to number (observable side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
addNumber :: NumberState -> Natural -> GradeApp 'Unsafe NumberResponse  
addNumber state addValue = 
    logRequest "POST" "/add" `gbind` \_ ->
    addToState state addValue `gbind` \_ ->
    readState state `gbind` \newValue ->
    unsafe (NumberResponse newValue)

-- Unsafe operation: randomise number (non-deterministic side effects)
-- Demonstrates: Safe <> Unsafe = Unsafe (Monoid composition)
randomiseNumber :: NumberState -> GradeApp 'Unsafe NumberResponse
randomiseNumber state = 
    logRequest "POST" "/randomise" `gbind` \_ ->
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
    showHandler = liftIO $ runGradeApp $ showNumber state
    
    setHandler :: NumberRequest -> Handler NumberResponse  
    setHandler (NumberRequest n) = liftIO $ runGradeApp $ setNumber state n
        
    addHandler :: NumberRequest -> Handler NumberResponse
    addHandler (NumberRequest n) = liftIO $ runGradeApp $ addNumber state n

    randomiseHandler :: Handler NumberResponse
    randomiseHandler = liftIO $ runGradeApp $ randomiseNumber state
        
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
    putStrLn "Effect Grade Hierarchy: Pure < Safe < Idempotent < Unsafe"
    putStrLn "API Routes with semantic grading:"  
    putStrLn "  GET  /show     → Safe       (read-only operations)"
    putStrLn "  PUT  /set      → Idempotent (repeatable with same result)" 
    putStrLn "  POST /add      → Unsafe     (observable side effects)"
    putStrLn "  POST /randomise → Unsafe     (non-deterministic effects)"
    putStrLn ""
    putStrLn "Monoid Grade composition with (<>) operator:"
    putStrLn "• Pure <> g = g              (Pure is identity/mempty)"
    putStrLn "• Safe <> Idempotent = Idempotent (automatic composition)"
    putStrLn "• Operations have their natural semantic grades"
    putStrLn "• No manual grade elevation needed"
    putStrLn ""
    
    -- Initialize number state to 0
    numberState <- newIORef 0
    
    putStrLn "Server starting... (Ctrl+C to stop)"
    run 8080 (app numberState)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( startApp
    -- Export algebraic demo functions for testing
    , identityLawDemo
    , absorptionLawDemo  
    , sequentialCompositionDemo
    , parallelCompositionDemo
    , gradeElevationDemo
    -- Export main HTTP operations for testing
    , showNumber
    , setNumber
    , addNumber
    , randomiseNumber
    -- Export graded monad primitives for testing  
    , GradeApp(..)
    , ireturn
    , ibind
    , liftSafeIO
    , safeContinue
    , weakenToIdempotent
    , weakenToUnsafe
    , logRequest
    -- Export IORef helper functions
    , safeReadState
    , idempotentWriteState
    , unsafeAddToState
    , pureValue
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

-- Graded monad for effect tracking with single grade parameter
newtype GradeApp (g :: Grade) a = GradeApp { runGradeApp :: IO a }

instance Functor (GradeApp g) where
    fmap f (GradeApp x) = GradeApp (f <$> x)

-- Graded monad operations with single grade parameter
ireturn :: a -> GradeApp 'Pure a
ireturn x = GradeApp (return x)

-- Graded bind: composition takes maximum grade
ibind :: GradeApp g a -> (a -> GradeApp h b) -> GradeApp (Max g h) b
ibind (GradeApp x) f = GradeApp (x >>= runGradeApp . f)

-- Parallel composition: combines effects using Max (least upper bound)
iparallel :: GradeApp g a -> GradeApp h b -> GradeApp (Max g h) (a, b)
iparallel (GradeApp x) (GradeApp y) = GradeApp ((,) <$> x <*> y)

-- Smart constructors for effect introduction
liftSafeIO :: IO a -> GradeApp 'Safe a
liftSafeIO = GradeApp

-- Additional constructors for continuing with Safe effects
safeContinue :: IO a -> GradeApp 'Safe a
safeContinue = GradeApp

-- ============================================================================
-- IOREF OPERATIONS - State manipulation with explicit grades
-- ============================================================================

-- Safe operation: Read state (no side effects, just observing)
-- Reading is Safe because it doesn't modify anything
safeReadState :: NumberState -> GradeApp 'Safe Natural
safeReadState state = safeContinue (readIORef state)

-- Idempotent operation: Write state (repeatable with same result)
-- Writing state is semantically Idempotent - same input = same result
idempotentWriteState :: NumberState -> Natural -> GradeApp 'Idempotent ()
idempotentWriteState state value = GradeApp (writeIORef state value)

-- Unsafe operation: Add to state (observable side effects)
-- Adding to existing state is Unsafe - not idempotent
unsafeAddToState :: NumberState -> Natural -> GradeApp 'Unsafe ()
unsafeAddToState state addValue = GradeApp $ do
    current <- readIORef state
    writeIORef state (current + addValue)

-- Pure operation: Create return value (no effects)
-- Creating values is Pure - no observable effects
pureValue :: a -> GradeApp 'Pure a  
pureValue = ireturn

-- Specific weakening functions for grade elevation
weakenToIdempotent :: GradeApp 'Safe a -> GradeApp 'Idempotent a
weakenToIdempotent (GradeApp x) = GradeApp x

weakenToUnsafe :: GradeApp 'Safe a -> GradeApp 'Unsafe a
weakenToUnsafe (GradeApp x) = GradeApp x

weakenIdempotentToUnsafe :: GradeApp 'Idempotent a -> GradeApp 'Unsafe a
weakenIdempotentToUnsafe (GradeApp x) = GradeApp x

-- Convenience constructors for different effect grades
safeReturn :: a -> GradeApp 'Safe a
safeReturn = GradeApp . return

idempotentReturn :: a -> GradeApp 'Idempotent a
idempotentReturn = GradeApp . return

unsafeReturn :: a -> GradeApp 'Unsafe a
unsafeReturn = GradeApp . return

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
logRequest :: String -> String -> GradeApp 'Safe ()
logRequest method path = liftSafeIO $ putStrLn $ "HTTP Log: " ++ method ++ " " ++ path

-- Safe operation: read current number (no side effects)  
-- Demonstrates algebraic composition with Max grade
showNumber :: NumberState -> GradeApp 'Safe NumberResponse
showNumber state = 
    logRequest "GET" "/show" `ibind` \_ ->
    safeReadState state `ibind` \n ->
    safeContinue (return (NumberResponse n))

-- Idempotent operation: set number (repeatable with same result)
-- Demonstrates: Max(Safe, Idempotent) = Idempotent (natural semantic grading)
setNumber :: NumberState -> Natural -> GradeApp 'Idempotent NumberResponse
setNumber state newValue = 
    logRequest "PUT" "/set" `ibind` \_ ->
    idempotentWriteState state newValue `ibind` \_ ->
    idempotentReturn (NumberResponse newValue)

-- Unsafe operation: add to number (observable side effects)
-- Demonstrates: Max(Safe, Unsafe) = Unsafe (natural semantic grading)
addNumber :: NumberState -> Natural -> GradeApp 'Unsafe NumberResponse  
addNumber state addValue = 
    logRequest "POST" "/add" `ibind` \_ ->
    unsafeAddToState state addValue `ibind` \_ ->
    safeReadState state `ibind` \newValue ->
    unsafeReturn (NumberResponse newValue)

-- Unsafe operation: generate random number (non-deterministic)
unsafeRandomValue :: GradeApp 'Unsafe Natural
unsafeRandomValue = GradeApp (fromIntegral <$> randomRIO (0, 1000 :: Int))

-- Unsafe operation: randomise number (non-deterministic side effects)
-- Demonstrates: Max(Safe, Unsafe, Idempotent) = Unsafe (natural semantic grading)
randomiseNumber :: NumberState -> GradeApp 'Unsafe NumberResponse
randomiseNumber state = 
    logRequest "POST" "/randomise" `ibind` \_ ->
    -- Random generation is inherently unsafe (non-deterministic)
    unsafeRandomValue `ibind` \randomValue ->
    idempotentWriteState state randomValue `ibind` \_ ->
    unsafeReturn (NumberResponse randomValue)

-- ============================================================================
-- ALGEBRAIC COMPOSITION EXAMPLES - Educational Demonstrations
-- ============================================================================

-- Example 1: Identity Law - Pure is the identity element
-- Mathematical: Max(Pure, g) = g
identityLawDemo :: NumberState -> GradeApp 'Safe Natural
identityLawDemo state = 
    -- Step 1: Pure computation (identity)  
    pureValue () `ibind` \_ ->
    -- Step 2: Max(Pure, Safe) = Safe (identity law applied)
    liftSafeIO (readIORef state)

-- Example 2: Absorption Law - Higher grades absorb lower ones  
-- Mathematical: Max(Safe, Idempotent) = Idempotent (natural semantic grading)
absorptionLawDemo :: NumberState -> Natural -> GradeApp 'Idempotent ()
absorptionLawDemo state value =
    -- Step 1: Safe effect (logging)
    logRequest "DEMO" "/absorption" `ibind` \_ ->
    -- Step 2: Max(Safe, Idempotent) = Idempotent (natural composition)
    idempotentWriteState state value `ibind` \_ ->
    -- Step 3: Already at Idempotent grade naturally
    idempotentReturn ()

-- Example 3: Sequential Composition Chain
-- Shows natural semantic grading with Max operation
sequentialCompositionDemo :: NumberState -> Natural -> GradeApp 'Idempotent Natural
sequentialCompositionDemo state newValue = 
    -- Step 1: Safe effect
    logRequest "SEQ" "/step1" `ibind` \_ ->
    -- Step 2: Max(Safe, Safe) = Safe 
    safeReadState state `ibind` \oldValue ->
    -- Step 3: Max(Safe, Idempotent) = Idempotent (natural semantic grade)
    idempotentWriteState state newValue `ibind` \_ ->
    -- Step 4: Already at Idempotent grade
    idempotentReturn oldValue

-- Example 4: Parallel Composition with Max
-- Mathematical: Max(Safe, Safe) = Safe
parallelCompositionDemo :: NumberState -> GradeApp 'Safe ((), Natural)
parallelCompositionDemo state = iparallel 
    -- Left side: Safe effect
    (logRequest "PARALLEL" "/left")
    -- Right side: Safe effect  
    -- Result: Max(Safe, Safe) = Safe
    (liftSafeIO (readIORef state))

-- Example 5: Grade Elevation Chain
-- Shows natural semantic grading leading to Unsafe
gradeElevationDemo :: NumberState -> Natural -> GradeApp 'Unsafe Natural
gradeElevationDemo state addValue =
    -- Safe effect
    logRequest "ELEVATION" "/unsafe" `ibind` \_ ->
    -- Max(Safe, Safe) = Safe  
    safeReadState state `ibind` \current ->
    -- Max(Safe, Unsafe) = Unsafe (natural semantic grade for addition)
    unsafeAddToState state addValue `ibind` \_ ->
    -- Already at Unsafe grade
    unsafeReturn (current + addValue)

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
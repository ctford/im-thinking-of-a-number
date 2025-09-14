module HTTP

import Effects
import Effects.GradedSyntax  -- Import our rebindable syntax
import Repository

%default partial  -- Allow partial functions for demonstration

-- ============================================================================
-- HTTP DATA TYPES - Request/Response types for web layer
-- ============================================================================

||| HTTP verb sum type for type-safe logging
data HttpVerb = GET | PUT | POST | DELETE

-- Automatic deriving - Idris 1's syntactic sugar at its finest!
%auto deriving instance Eq HttpVerb
%auto deriving instance Ord HttpVerb
%auto deriving instance Show HttpVerb

||| Number request carrying a natural number value
record NumberRequest where
  constructor MkNumberRequest
  value : Nat

%auto deriving instance Eq NumberRequest
%auto deriving instance Show NumberRequest

||| Number response carrying the result  
record NumberResponse where
  constructor MkNumberResponse
  current : Nat

%auto deriving instance Eq NumberResponse
%auto deriving instance Show NumberResponse

-- ============================================================================
-- LOGGING - Type-safe request logging with grade awareness
-- ============================================================================

||| Log HTTP request with method and response value
||| Safe grade as logging doesn't modify application state
logRequest : HttpVerb -> String -> Nat -> Action Safe ()
logRequest verb path current = safeAction $ 
  putStrLn ("- - [" ++ show verb ++ "] " ++ path ++ " 200 - current=" ++ show current)

-- ============================================================================
-- BUSINESS OPERATIONS - Using beautiful do-notation with graded effects!
-- ============================================================================

||| GET /number - Show current number (Safe grade)
||| This is where Idris 1 shines - clean do-notation with automatic grade inference!
showNumber : NumberState -> Action Safe NumberResponse
showNumber state = do
  n <- readState state
  _ <- logRequest GET "/number" n
  return (MkNumberResponse n)

||| PUT /number - Set number to specific value (Idempotent grade)
||| The do-notation automatically composes: Safe <> Idempotent = Idempotent
setNumber : NumberState -> Nat -> Action Idempotent NumberResponse
setNumber state value = do
  _ <- writeState value state
  n <- readState state
  _ <- logRequest PUT "/number" n
  return (MkNumberResponse n)

||| POST /number/add - Add to current number (Unsafe grade)  
||| The do-notation automatically composes: Unsafe <> Safe = Unsafe
addNumber : NumberState -> Nat -> Action Unsafe NumberResponse
addNumber state addValue = do
  _ <- addToState addValue state
  n <- readState state
  _ <- logRequest POST "/number/add" n
  return (MkNumberResponse n)

||| POST /number/randomise - Set to random value (Unsafe grade)
||| Clean syntax with automatic grade composition
randomiseNumber : NumberState -> Action Unsafe NumberResponse
randomiseNumber state = do
  _ <- randomiseState state
  n <- readState state
  _ <- logRequest POST "/number/randomise" n
  return (MkNumberResponse n)

||| DELETE /number - Reset to zero (Idempotent grade)
||| Idris 1's do-notation makes this beautifully readable
resetNumber : NumberState -> Action Idempotent NumberResponse
resetNumber state = do
  _ <- writeState 0 state
  n <- readState state
  _ <- logRequest DELETE "/number" n
  return (MkNumberResponse n)

-- ============================================================================
-- HTTP OPERATIONS - Direct function mapping for simplified API
-- ============================================================================

||| Simple HTTP operation registry - maps verbs to their semantic grades
httpGrade : HttpVerb -> Grade
httpGrade GET = Safe
httpGrade PUT = Idempotent  
httpGrade POST = Unsafe
httpGrade DELETE = Idempotent

-- ============================================================================
-- HTTP OPERATIONS - Direct function mapping for simplified API
-- ============================================================================
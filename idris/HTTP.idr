module HTTP

import Effects
import Repository

-- ============================================================================
-- HTTP DATA TYPES - Request/Response without JSON dependencies  
-- ============================================================================

||| HTTP verb sum type for type-safe logging
||| Dependent types could extend this with method-grade relationships
public export
data HttpVerb : Type where
  GET    : HttpVerb
  PUT    : HttpVerb  
  POST   : HttpVerb
  DELETE : HttpVerb

-- Show and Eq instances (could be derived, but explicit for compatibility)
public export
Show HttpVerb where
  show GET = "GET"
  show PUT = "PUT" 
  show POST = "POST"
  show DELETE = "DELETE"

public export
Eq HttpVerb where
  GET == GET = True
  PUT == PUT = True
  POST == POST = True
  DELETE == DELETE = True
  _ == _ = False

||| Number request carrying a natural number value
||| Uses Nat for compile-time non-negative guarantee
public export
record NumberRequest where
  constructor MkNumberRequest
  value : Nat

-- NumberResponse eliminated - operations return Nat directly

-- ============================================================================
-- LOGGING - Type-safe request logging with grade awareness
-- ============================================================================

||| Log HTTP request with method and response value
||| Safe grade as logging doesn't modify application state
public export
logRequest : HttpVerb -> String -> Nat -> Action Safe ()
logRequest verb path current = MkAction $ 
  putStrLn "- - [\{show verb}] \{path} 200 - current=\{show current}"

-- ============================================================================
-- BUSINESS OPERATIONS - HTTP handlers with dependent grade composition
-- ============================================================================

||| GET /number - Show current number (Safe grade)
||| Read-only operation with logging, composes to Safe grade
public export
showNumber : NumberState -> Action Safe Nat
showNumber state = 
  readState state `bind` \n =>
  logRequest GET "/number" n `bind` \_ =>
  (MkAction (pure n) : Action Safe Nat)

||| PUT /number - Set number to specific value (Idempotent grade)
||| Same input always produces same result, composes to Idempotent grade
public export
setNumber : NumberState -> Nat -> Action Idempotent Nat
setNumber state value = 
  writeState value state `bind` \_ =>
  readState state `bind` \n =>
  logRequest PUT "/number" n `bind` \_ =>
  (MkAction (pure n) : Action Idempotent Nat)

||| POST /number/add - Add to current number (Unsafe grade)  
||| Non-idempotent operation with observable side effects
public export
addNumber : NumberState -> Nat -> Action Unsafe Nat
addNumber state addValue = 
  addToState addValue state `bind` \_ =>
  readState state `bind` \n =>
  logRequest POST "/number/add" n `bind` \_ =>
  (MkAction (pure n) : Action Unsafe Nat)

||| POST /number/randomise - Set to random value (Unsafe grade)
||| Non-deterministic operation with observable side effects
public export
randomiseNumber : NumberState -> Action Unsafe Nat
randomiseNumber state = 
  randomiseState state `bind` \_ =>
  readState state `bind` \n =>
  logRequest POST "/number/randomise" n `bind` \_ =>
  (MkAction (pure n) : Action Unsafe Nat)

||| DELETE /number - Reset to zero (Idempotent grade)
||| Always resets to same value, repeatable with same result
public export  
resetNumber : NumberState -> Action Idempotent Nat
resetNumber state = 
  writeState 0 state `bind` \_ =>
  readState state `bind` \n =>
  logRequest DELETE "/number" n `bind` \_ =>
  (MkAction (pure n) : Action Idempotent Nat)

-- ============================================================================
-- DIRECT HTTP OPERATIONS - Return Nat directly, no JSON wrapper needed
-- ============================================================================

-- Core operations (showNumber, setNumber, etc.) are now the main HTTP operations
-- No need for separate wrapper functions

-- ============================================================================
-- SIMPLIFIED HTTP OPERATIONS - Direct function calls without boilerplate
-- ============================================================================

||| Simple HTTP operation registry - maps verbs to their semantic grades
||| The interesting dependent types are in the graded monad itself, not here
public export
httpGrade : HttpVerb -> Grade
httpGrade GET = Safe
httpGrade PUT = Idempotent  
httpGrade POST = Unsafe
httpGrade DELETE = Idempotent
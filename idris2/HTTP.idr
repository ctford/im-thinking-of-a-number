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

Show HttpVerb where
  show GET = "GET"
  show PUT = "PUT" 
  show POST = "POST"
  show DELETE = "DELETE"

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

||| Number response carrying current state value
||| Uses Nat for compile-time non-negative guarantee  
public export
record NumberResponse where
  constructor MkNumberResponse
  current : Nat

-- ============================================================================
-- LOGGING - Type-safe request logging with grade awareness
-- ============================================================================

||| Log HTTP request with method and response value
||| Safe grade as logging doesn't modify application state
public export
logRequest : HttpVerb -> String -> Nat -> Action Safe ()
logRequest verb path current = safeAction $ 
  putStrLn "- - [\{show verb}] \{path} 200 - current=\{show current}"

-- ============================================================================
-- BUSINESS OPERATIONS - HTTP handlers with dependent grade composition
-- ============================================================================

||| GET /number - Show current number (Safe grade)
||| Read-only operation with logging, composes to Safe grade
||| Dependent types ensure grade composition is computed correctly
public export
showNumber : NumberState -> Action Safe NumberResponse  
showNumber state = 
  readState state `bind` \n =>
  logRequest GET "/number" n `bind` \_ =>
  safeAction (pure (MkNumberResponse n))

||| PUT /number - Set number to specific value (Idempotent grade)
||| Same input always produces same result, composes to Idempotent grade
||| Grade composition: Idempotent <> Safe <> Safe = Idempotent
public export
setNumber : NumberState -> Nat -> Action Idempotent NumberResponse
setNumber state value = 
  writeState value state `bind` \_ =>
  readState state `bind` \n =>
  logRequest PUT "/number" n `bind` \_ =>
  idempotentAction (pure (MkNumberResponse n))

||| POST /number/add - Add to current number (Unsafe grade)  
||| Non-idempotent operation with observable side effects
||| Grade composition: Unsafe <> Safe <> Safe = Unsafe
public export
addNumber : NumberState -> Nat -> Action Unsafe NumberResponse
addNumber state addValue = 
  addToState addValue state `bind` \_ =>
  readState state `bind` \n =>
  logRequest POST "/number/add" n `bind` \_ =>
  unsafeAction (pure (MkNumberResponse n))

||| POST /number/randomise - Set to random value (Unsafe grade)
||| Non-deterministic operation with observable side effects
||| Grade composition: Unsafe <> Safe <> Safe = Unsafe  
public export
randomiseNumber : NumberState -> Action Unsafe NumberResponse
randomiseNumber state = 
  randomiseState state `bind` \_ =>
  readState state `bind` \n =>
  logRequest POST "/number/randomise" n `bind` \_ =>
  unsafeAction (pure (MkNumberResponse n))

||| DELETE /number - Reset to zero (Idempotent grade)
||| Always resets to same value, repeatable with same result
||| Grade composition: Idempotent <> Safe <> Safe = Idempotent
public export  
resetNumber : NumberState -> Action Idempotent NumberResponse
resetNumber state = 
  writeState 0 state `bind` \_ =>
  readState state `bind` \n =>
  logRequest DELETE "/number" n `bind` \_ =>
  idempotentAction (pure (MkNumberResponse n))

-- ============================================================================
-- DEPENDENT HTTP OPERATIONS - Handlers with compile-time grade verification
-- ============================================================================

||| HTTP handler that proves its grade at compile time
||| Dependent type relates HTTP method to expected grade
public export
data HttpHandler : HttpVerb -> Grade -> Type where
  GetHandler    : (NumberState -> Action Safe NumberResponse) -> 
                  HttpHandler GET Safe
  PutHandler    : (NumberState -> Nat -> Action Idempotent NumberResponse) ->
                  HttpHandler PUT Idempotent  
  PostHandler   : {g : Grade} -> (NumberState -> Nat -> Action g NumberResponse) ->
                  HttpHandler POST g
  DeleteHandler : (NumberState -> Action Idempotent NumberResponse) ->
                  HttpHandler DELETE Idempotent

||| Execute handler with compile-time grade verification
||| Type system ensures handler grade matches expected grade
public export
executeHandler : {verb : HttpVerb} -> {g : Grade} ->
                 HttpHandler verb g -> 
                 NumberState ->
                 (args : List Nat) ->
                 Action g NumberResponse
executeHandler (GetHandler f) state [] = f state
executeHandler (GetHandler f) state (_ :: _) = 
  safeAction (pure (MkNumberResponse 0)) -- Invalid args
executeHandler (PutHandler f) state [value] = f state value
executeHandler (PutHandler f) state _ =
  idempotentAction (pure (MkNumberResponse 0)) -- Invalid args
executeHandler (PostHandler {g} f) state [value] = f state value
executeHandler (PostHandler {g} f) state _ =
  MkAction {g} (pure (MkNumberResponse 0)) -- Use generic constructor
executeHandler (DeleteHandler f) state [] = f state
executeHandler (DeleteHandler f) state (_ :: _) =
  idempotentAction (pure (MkNumberResponse 0)) -- Invalid args

-- ============================================================================
-- HANDLER REGISTRY - Type-safe collection of HTTP handlers
-- ============================================================================

||| Registry of all HTTP handlers with their grades
public export
handlers : (verb : HttpVerb) -> 
           (g : Grade ** HttpHandler verb g)
handlers GET = (Safe ** GetHandler showNumber)
handlers PUT = (Idempotent ** PutHandler setNumber)  
handlers POST = (Unsafe ** PostHandler addNumber)
handlers DELETE = (Idempotent ** DeleteHandler resetNumber)

||| Execute any HTTP handler with automatic grade inference
||| Type system computes the appropriate grade for each method
public export
handleRequest : (verb : HttpVerb) ->
                NumberState ->
                List Nat ->
                (g : Grade ** Action g NumberResponse)
handleRequest verb state args = 
  let (g ** handler) = handlers verb
  in (g ** executeHandler handler state args)
module Demo

import Effects
import Effects.GradedSyntax
import HTTP
import Repository

-- ============================================================================
-- SYNTAX DEMONSTRATION - Show off Idris 1's beautiful do-notation
-- ============================================================================

||| This file demonstrates the beautiful syntax possible with Idris 1's
||| indexed monad support compared to the verbose Idris 2 implementation

-- ============================================================================
-- IDRIS 1 SYNTAX - Clean and readable! 🎉
-- ============================================================================

||| Example: Complex HTTP operation with multiple grade compositions
||| Notice how clean and readable this is compared to Idris 2!
complexHttpOperation : NumberState -> Action Unsafe NumberResponse
complexHttpOperation state = do
  -- Read initial value (Safe grade)
  initial <- readState state
  _ <- logRequest GET "/initial" initial
  
  -- Perform some updates (Idempotent grade)  
  _ <- writeState (initial * 2) state
  doubled <- readState state
  _ <- logRequest PUT "/double" doubled
  
  -- Add random amount (Unsafe grade - this determines final grade)
  _ <- addToState 42 state
  _ <- randomiseState state
  final <- readState state
  _ <- logRequest POST "/final" final
  
  -- Return result with automatic grade: Unsafe
  return (MkNumberResponse final)

||| Example: Conditional operations with grade preservation
conditionalOperation : NumberState -> Nat -> Action Unsafe NumberResponse
conditionalOperation state threshold = do
  current <- readState state
  
  if current > threshold
    then do
      -- High value: randomize it (Unsafe)
      _ <- randomiseState state
      new <- readState state
      _ <- logRequest POST "/randomized" new
      return (MkNumberResponse new)
    else do
      -- Low value: just increment (Unsafe due to addToState)
      _ <- addToState 10 state
      incremented <- readState state
      _ <- logRequest POST "/incremented" incremented
      return (MkNumberResponse incremented)

||| Example: Sequential operations with automatic grade composition
sequentialOperations : NumberState -> Action Unsafe ()
sequentialOperations state = do
  -- Chain of operations with different grades
  _ <- writeState 100 state        -- Idempotent
  _ <- logRequest PUT "/init" 100  -- Safe
  _ <- addToState 25 state         -- Unsafe
  _ <- logRequest POST "/add" 125  -- Safe
  _ <- randomiseState state        -- Unsafe
  final <- readState state         -- Safe
  _ <- logRequest POST "/done" final -- Safe
  return ()  -- Final grade: Unsafe (highest in the chain)

-- ============================================================================
-- GRADE COMPOSITION EXAMPLES - Show automatic inference
-- ============================================================================

||| Pure operation (returns grade Pure automatically)
pureComputation : Nat -> Action Pure Nat
pureComputation x = do
  let doubled = x * 2
  let result = doubled + 10
  return result

||| Safe operation (read-only with logging)
safeOperation : NumberState -> Action Safe Nat  
safeOperation state = do
  n <- readState state
  _ <- logRequest GET "/safe" n
  return (n + 1)  -- Computed value, no state change

||| Idempotent operation (repeatable with same result)
idempotentOperation : NumberState -> Nat -> Action Idempotent Nat
idempotentOperation state value = do
  _ <- writeState value state
  result <- readState state
  _ <- logRequest PUT "/idempotent" result
  return result

||| Unsafe operation (side effects and non-determinism)
unsafeOperation : NumberState -> Action Unsafe Nat
unsafeOperation state = do
  _ <- addToState 1 state
  _ <- randomiseState state
  result <- readState state
  _ <- logRequest POST "/unsafe" result
  return result

-- ============================================================================
-- COMPOSITION EXAMPLES - Multiple operations with automatic grading
-- ============================================================================

||| Compose operations of different grades
||| Final grade is automatically inferred as Unsafe (highest)
composedOperation : NumberState -> Action Unsafe String
composedOperation state = do
  -- Start with pure computation
  pureResult <- pureComputation 5     -- Pure
  
  -- Do safe operations  
  safeResult <- safeOperation state   -- Pure <> Safe = Safe
  
  -- Do idempotent operations
  idempotentResult <- idempotentOperation state 42  -- Safe <> Idempotent = Idempotent
  
  -- Do unsafe operations (this determines final grade)
  unsafeResult <- unsafeOperation state  -- Idempotent <> Unsafe = Unsafe
  
  -- Log final summary
  _ <- logRequest POST "/summary" unsafeResult  -- Unsafe <> Safe = Unsafe
  
  -- Return summary string
  return $ "Pure: " ++ show pureResult ++ 
           ", Safe: " ++ show safeResult ++ 
           ", Idempotent: " ++ show idempotentResult ++ 
           ", Unsafe: " ++ show unsafeResult

-- ============================================================================
-- COMPARISON COMMENT - What this would look like in Idris 2
-- ============================================================================

{-
In Idris 2, the same operations would require verbose `bind` syntax:

complexHttpOperation state = 
  readState state `bind` \initial =>
  logRequest GET "/initial" initial `bind` \_ =>
  writeState (initial * 2) state `bind` \_ =>
  readState state `bind` \doubled =>
  logRequest PUT "/double" doubled `bind` \_ =>
  addToState 42 state `bind` \_ =>
  randomiseState state `bind` \_ =>
  readState state `bind` \final =>
  logRequest POST "/final" final `bind` \_ =>
  (MkAction (pure (MkNumberResponse final)) : Action Unsafe NumberResponse)

This is much less readable and more error-prone! 😢
-}

-- ============================================================================
-- DEMO RUNNER - Show off the syntax in action
-- ============================================================================

||| Demonstrate all the beautiful syntax
demoRunner : IO ()
demoRunner = do
  putStrLn "🎉 Idris 1 Graded Monad Syntax Demo 🎉"
  putStrLn "====================================="
  
  state <- newIORef 50
  putStrLn $ "Initial state: 50"
  
  -- Run complex operation
  NumberResponse result1 <- runAction $ complexHttpOperation state
  putStrLn $ "After complex operation: " ++ show result1
  
  -- Run conditional operation  
  NumberResponse result2 <- runAction $ conditionalOperation state 100
  putStrLn $ "After conditional operation: " ++ show result2
  
  -- Run composed operation
  summary <- runAction $ composedOperation state
  putStrLn $ "Composition summary: " ++ summary
  
  putStrLn "====================================="
  putStrLn "✨ Beautiful syntax with automatic grade inference! ✨"
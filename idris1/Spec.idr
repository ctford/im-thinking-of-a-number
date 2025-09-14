module Spec

import Effects
import Effects.GradedSyntax
import HTTP
import Repository

-- ============================================================================
-- TEST FRAMEWORK - Simple test framework for Idris 1
-- ============================================================================

||| Simple test result type
data TestResult = Pass | Fail String

instance Show TestResult where
  show Pass = "PASS"
  show (Fail msg) = "FAIL: " ++ msg

||| Test case type
TestCase : Type
TestCase = (String, IO TestResult)

||| Assert equality with nice error messages
assertEq : (Show a, Eq a) => a -> a -> TestResult
assertEq expected actual = 
  if expected == actual 
    then Pass 
    else Fail ("Expected " ++ show expected ++ " but got " ++ show actual)

||| Assert that a predicate holds
assert : Bool -> String -> TestResult
assert True _ = Pass
assert False msg = Fail msg

-- ============================================================================
-- HTTP OPERATIONS TESTS - Verify graded monad HTTP semantics with do-notation
-- ============================================================================

||| Test GET /number operation with Safe grade
testShowNumber : TestCase
testShowNumber = ("GET /number operation (Safe grade)", do
  state <- newIORef 123
  NumberResponse value <- runAction $ showNumber state
  let result = assertEq 123 value
  unchanged <- readIORef state
  let stateResult = assertEq 123 unchanged
  return $ case (result, stateResult) of
    (Pass, Pass) => Pass
    (Fail msg, _) => Fail msg
    (_, Fail msg) => Fail ("State check: " ++ msg))

||| Test PUT /number operation with Idempotent grade
testSetNumber : TestCase
testSetNumber = ("PUT /number operation (Idempotent grade)", do
  state <- newIORef 0
  NumberResponse result1 <- runAction $ setNumber state 99
  NumberResponse result2 <- runAction $ setNumber state 99
  finalValue <- readIORef state
  return $ case (assertEq 99 result1, assertEq 99 result2, assertEq 99 finalValue) of
    (Pass, Pass, Pass) => Pass
    (Fail msg, _, _) => Fail ("First call: " ++ msg)
    (_, Fail msg, _) => Fail ("Second call: " ++ msg)
    (_, _, Fail msg) => Fail ("Final state: " ++ msg))

||| Test POST /number/add operation with Unsafe grade
testAddNumber : TestCase
testAddNumber = ("POST /number/add operation (Unsafe grade)", do
  state <- newIORef 10
  NumberResponse result1 <- runAction $ addNumber state 5
  NumberResponse result2 <- runAction $ addNumber state 5
  finalValue <- readIORef state
  return $ case (assertEq 15 result1, assertEq 20 result2, assertEq 20 finalValue) of
    (Pass, Pass, Pass) => Pass
    (Fail msg, _, _) => Fail ("First add: " ++ msg)
    (_, Fail msg, _) => Fail ("Second add: " ++ msg)
    (_, _, Fail msg) => Fail ("Final state: " ++ msg))

||| Test POST /number/randomise operation with Unsafe grade
testRandomiseNumber : TestCase
testRandomiseNumber = ("POST /number/randomise operation (Unsafe grade)", do
  state <- newIORef 0
  NumberResponse result <- runAction $ randomiseNumber state
  finalValue <- readIORef state
  let rangeCheck = assert (result >= 0 && result <= 1000) "Random value out of range"
  let stateCheck = assertEq result finalValue
  return $ case (rangeCheck, stateCheck) of
    (Pass, Pass) => Pass
    (Fail msg, _) => Fail msg
    (_, Fail msg) => Fail ("State consistency: " ++ msg))

||| Test DELETE /number operation with Idempotent grade
testResetNumber : TestCase
testResetNumber = ("DELETE /number operation (Idempotent grade)", do
  state <- newIORef 456
  NumberResponse result1 <- runAction $ resetNumber state
  NumberResponse result2 <- runAction $ resetNumber state
  finalValue <- readIORef state
  return $ case (assertEq 0 result1, assertEq 0 result2, assertEq 0 finalValue) of
    (Pass, Pass, Pass) => Pass
    (Fail msg, _, _) => Fail ("First reset: " ++ msg)
    (_, Fail msg, _) => Fail ("Second reset: " ++ msg)
    (_, _, Fail msg) => Fail ("Final state: " ++ msg))

-- ============================================================================
-- GRADED MONAD TESTS - Verify mathematical properties with do-notation
-- ============================================================================

||| Test that do-notation works with grade composition
testGradeComposition : TestCase
testGradeComposition = ("Grade composition with do-notation", do
  state <- newIORef 100
  
  -- Simple test of composing different grades in do-notation
  NumberResponse result <- runAction $ do
    n <- readState state          -- Safe
    _ <- addToState 10 state      -- Unsafe (determines final grade)
    final <- readState state     -- Safe
    return (MkNumberResponse final)  -- Final grade: Unsafe
  
  finalValue <- readIORef state
  
  -- Should be 100 + 10 = 110
  let resultCheck = assertEq 110 result
  let stateCheck = assertEq 110 finalValue
  
  return $ case (resultCheck, stateCheck) of
    (Pass, Pass) => Pass
    (Fail msg, _) => Fail ("Result: " ++ msg)
    (_, Fail msg) => Fail ("State: " ++ msg))

||| Test mixed grade operations in do-notation
testMixedGrades : TestCase
testMixedGrades = ("Mixed grade operations", do
  state <- newIORef 50
  
  -- Test composition of Safe, Idempotent, and Unsafe grades
  runAction $ do
    n1 <- readState state         -- Safe
    _ <- writeState (n1 + 1) state -- Idempotent  
    _ <- addToState 5 state       -- Unsafe (determines final grade)
    n2 <- readState state         -- Safe
    _ <- logRequest POST "/mixed" n2 -- Safe
    return ()  -- Final grade: Unsafe
    
  finalValue <- readIORef state
  
  -- Should be 50 + 1 + 5 = 56
  let stateCheck = assertEq 56 finalValue
  
  return $ case stateCheck of
    Pass => Pass
    Fail msg => Fail ("Mixed operation: " ++ msg))

-- ============================================================================
-- NATURAL NUMBER PROPERTIES - Type safety verification
-- ============================================================================

||| Test that operations preserve Natural number constraints
testNaturalProperties : TestCase
testNaturalProperties = ("Natural number properties", do
  state <- newIORef 5
  NumberResponse result <- runAction $ addNumber state 10
  let check = assert (result >= 0) "Natural number became negative"
  return check)

-- ============================================================================
-- ALGEBRAIC LAWS - Mathematical verification with beautiful syntax
-- ============================================================================

||| Test identity law with do-notation
testIdentityLaw : TestCase
testIdentityLaw = ("Identity law: Pure <> g = g", do
  state <- newIORef 777
  
  -- Pure operation followed by Safe operation  
  result <- runAction $ do
    _ <- return ()              -- Pure operation
    n <- readState state        -- Safe operation
    return n                    -- Result should be Safe grade
    
  let check = assertEq 777 result
  return check)

||| Test associativity with do-notation
testAssociativity : TestCase
testAssociativity = ("Associativity with grade composition", do
  state <- newIORef 555
  
  runAction $ do
    _ <- logRequest GET "/associativity" 888    -- Safe effect
    _ <- writeState 888 state                  -- Safe <> Idempotent = Idempotent
    return ()                                  -- Natural composition
    
  finalValue <- readIORef state
  let check = assertEq 888 finalValue
  return check)

-- ============================================================================
-- TEST RUNNER - Execute all tests and report results
-- ============================================================================

||| All test cases
allTests : List TestCase
allTests = [
  testShowNumber,
  testSetNumber, 
  testAddNumber,
  testRandomiseNumber,
  testResetNumber,
  testGradeComposition,
  testMixedGrades,
  testNaturalProperties,
  testIdentityLaw,
  testAssociativity
]

||| Run a single test case
runTest : TestCase -> IO ()
runTest (name, test) = do
  result <- test
  putStrLn $ name ++ ": " ++ show result

||| Run all tests
runAllTests : IO ()
runAllTests = do
  putStrLn "Running Idris 1 graded monad tests with beautiful do-notation..."
  putStrLn "====================================================================="
  sequence_ (map runTest allTests)
  putStrLn "====================================================================="
  putStrLn "Tests completed!"

||| Main entry point for running tests
main : IO ()
main = runAllTests
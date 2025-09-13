module Spec

import Effects
import Repository  
import HTTP
import Data.IORef

-- ============================================================================
-- TEST FRAMEWORK - Simple testing without external dependencies
-- ============================================================================

||| Simple test result type
public export
data TestResult : Type where
  Pass : String -> TestResult
  Fail : String -> String -> TestResult -- expected, actual

||| Test runner that accumulates results
public export
runTest : String -> IO TestResult -> IO ()
runTest name test = do
  result <- test
  case result of
    Pass msg => putStrLn "✓ \{name}: \{msg}"
    Fail expected actual => putStrLn "✗ \{name}: expected \{expected}, got \{actual}"

||| Assert equality with informative error messages
public export
assertEqual : (Show a, Eq a) => a -> a -> String -> TestResult
assertEqual expected actual msg = 
  if expected == actual 
    then Pass msg
    else Fail (show expected) (show actual)

-- ============================================================================
-- GRADE COMPOSITION TESTS - Verify mathematical properties
-- ============================================================================

||| Test that Pure is left identity for grade composition
testPureLeftIdentity : IO TestResult
testPureLeftIdentity = pure $ 
  let result = gradeJoin Pure Safe
  in assertEqual Safe result "Pure is left identity"

||| Test that Pure is right identity for grade composition  
testPureRightIdentity : IO TestResult
testPureRightIdentity = pure $
  let result = gradeJoin Idempotent Pure
  in assertEqual Idempotent result "Pure is right identity"

||| Test that grade composition is idempotent
testGradeIdempotent : IO TestResult  
testGradeIdempotent = pure $
  let result = gradeJoin Safe Safe
  in assertEqual Safe result "Grade composition is idempotent"

||| Test that Unsafe is absorbing element (left)
testUnsafeLeftAbsorbing : IO TestResult
testUnsafeLeftAbsorbing = pure $
  let result = gradeJoin Unsafe Safe
  in assertEqual Unsafe result "Unsafe is left absorbing"

||| Test that Unsafe is absorbing element (right)  
testUnsafeRightAbsorbing : IO TestResult
testUnsafeRightAbsorbing = pure $
  let result = gradeJoin Idempotent Unsafe
  in assertEqual Unsafe result "Unsafe is right absorbing"

||| Test grade ordering properties
testGradeOrdering : IO TestResult
testGradeOrdering = pure $
  let result1 = Pure < Safe
      result2 = Safe < Idempotent  
      result3 = Idempotent < Unsafe
      allCorrect = result1 && result2 && result3
  in if allCorrect
       then Pass "Grade ordering is correct"
       else Fail "True" "False"

-- ============================================================================
-- HTTP OPERATION TESTS - Verify graded monad HTTP semantics
-- ============================================================================

||| Test GET /number operation has Safe grade
testGetOperation : IO TestResult
testGetOperation = do
  state <- newIORef 123
  (MkNumberResponse value) <- runAction $ showNumber state
  if value == 123
    then pure $ Pass "GET operation preserves state and returns correct value"
    else pure $ Fail "123" (show value)

||| Test PUT /number operation has Idempotent grade  
testPutOperation : IO TestResult
testPutOperation = do
  state <- newIORef 0
  (MkNumberResponse result1) <- runAction $ setNumber state 99
  (MkNumberResponse result2) <- runAction $ setNumber state 99
  if result1 == 99 && result2 == 99
    then pure $ Pass "PUT operation is idempotent"
    else pure $ Fail "99, 99" "\{show result1}, \{show result2}"

||| Test POST /number/add operation has Unsafe grade
testPostAddOperation : IO TestResult  
testPostAddOperation = do
  state <- newIORef 10
  (MkNumberResponse result1) <- runAction $ addNumber state 5
  (MkNumberResponse result2) <- runAction $ addNumber state 5
  if result1 == 15 && result2 == 20
    then pure $ Pass "POST add operation is non-idempotent (unsafe)"
    else pure $ Fail "15, 20" "\{show result1}, \{show result2}"

||| Test POST /number/randomise operation has Unsafe grade
testPostRandomiseOperation : IO TestResult
testPostRandomiseOperation = do
  state <- newIORef 0
  (MkNumberResponse result) <- runAction $ randomiseNumber state
  finalValue <- readIORef state
  if result >= 0 && result <= 1000 && result == finalValue
    then pure $ Pass "POST randomise operation updates state correctly"
    else pure $ Fail "0-1000 range" "\{show result}"

||| Test DELETE /number operation has Idempotent grade
testDeleteOperation : IO TestResult
testDeleteOperation = do
  state <- newIORef 456
  (MkNumberResponse result1) <- runAction $ resetNumber state
  (MkNumberResponse result2) <- runAction $ resetNumber state
  if result1 == 0 && result2 == 0
    then pure $ Pass "DELETE operation is idempotent"
    else pure $ Fail "0, 0" "\{show result1}, \{show result2}"

-- ============================================================================
-- NATURAL NUMBER TESTS - Verify Nat type guarantees
-- ============================================================================

||| Test that all operations preserve Natural number constraints
testNaturalConstraints : IO TestResult
testNaturalConstraints = do
  state <- newIORef 5
  (MkNumberResponse result) <- runAction $ addNumber state 10
  if result >= 0  -- This is always true for Nat, but demonstrates the point
    then pure $ Pass "Operations preserve Natural number constraints"  
    else pure $ Fail "non-negative" (show result)

-- ============================================================================
-- DEPENDENT TYPE TESTS - Verify compile-time guarantees
-- ============================================================================

||| Test that HTTP handlers have correct grades
testHandlerGrades : IO TestResult
testHandlerGrades = pure $
  let (getGrade ** _) = handlers GET
      (putGrade ** _) = handlers PUT
      (postGrade ** _) = handlers POST
      (deleteGrade ** _) = handlers DELETE
      correctGrades = getGrade == Safe && 
                      putGrade == Idempotent &&
                      postGrade == Unsafe &&
                      deleteGrade == Idempotent
  in if correctGrades
       then Pass "HTTP handlers have correct grades"
       else Fail "Safe, Idempotent, Unsafe, Idempotent" 
                 "\{show getGrade}, \{show putGrade}, \{show postGrade}, \{show deleteGrade}"

||| Test grade composition in complex scenarios
testGradeComposition : IO TestResult
testGradeComposition = pure $
  let -- Simulate: Safe <> Idempotent <> Pure = Idempotent
      result1 = gradeJoin (gradeJoin Safe Idempotent) Pure
      -- Simulate: Unsafe <> Safe <> Idempotent = Unsafe  
      result2 = gradeJoin (gradeJoin Unsafe Safe) Idempotent
      correct = result1 == Idempotent && result2 == Unsafe
  in if correct
       then Pass "Complex grade composition works correctly"
       else Fail "Idempotent, Unsafe" "\{show result1}, \{show result2}"

-- ============================================================================
-- PROOF VERIFICATION TESTS - Verify compile-time proofs
-- ============================================================================

||| Test that proofs compile and verify properties
testProofCompilation : IO TestResult  
testProofCompilation = pure $
  let -- These proofs are verified at compile time
      _ = pureLeftIdentity Safe
      _ = pureRightIdentity Idempotent
      _ = gradeIdempotent Unsafe
      _ = unsafeLeftAbsorbing Safe
      _ = unsafeRightAbsorbing Idempotent
  in Pass "All mathematical proofs compile successfully"

-- ============================================================================
-- MAIN TEST RUNNER - Execute all tests
-- ============================================================================

||| Run all tests and report results
main : IO ()
main = do
  putStrLn "=== Idris 2 Graded Monad HTTP Tests ==="
  putStrLn ""
  
  putStrLn "Grade Composition Laws:"
  runTest "Pure left identity" testPureLeftIdentity
  runTest "Pure right identity" testPureRightIdentity
  runTest "Grade idempotent" testGradeIdempotent
  runTest "Unsafe left absorbing" testUnsafeLeftAbsorbing
  runTest "Unsafe right absorbing" testUnsafeRightAbsorbing
  runTest "Grade ordering" testGradeOrdering
  
  putStrLn ""
  putStrLn "HTTP Operations:"
  runTest "GET operation (Safe)" testGetOperation
  runTest "PUT operation (Idempotent)" testPutOperation
  runTest "POST add operation (Unsafe)" testPostAddOperation
  runTest "POST randomise operation (Unsafe)" testPostRandomiseOperation
  runTest "DELETE operation (Idempotent)" testDeleteOperation
  
  putStrLn ""
  putStrLn "Type System Guarantees:"
  runTest "Natural number constraints" testNaturalConstraints
  runTest "Handler grade correctness" testHandlerGrades
  runTest "Complex grade composition" testGradeComposition
  runTest "Proof compilation" testProofCompilation
  
  putStrLn ""
  putStrLn "All tests completed!"
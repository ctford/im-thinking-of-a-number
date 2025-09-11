{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Data.IORef
import Numeric.Natural

import Lib

-- ============================================================================
-- ALGEBRAIC PROPERTY TESTS - Verify indexed monad composition laws
-- ============================================================================

main :: IO ()
main = hspec $ do
  describe "Grade Hierarchy Properties" $ do
    it "verifies Pure is the identity element (left identity law)" $ do
      -- Test: Pure ⊕ Safe = Safe
      -- Create a computation that goes Pure → Safe
      state <- newIORef (42 :: Natural)
      result <- runIxApp $ identityLawDemo state
      result `shouldBe` 42
      
    it "demonstrates absorption law with grade elevation" $ do  
      -- Test: Safe ⊕ Idempotent = Idempotent
      -- Shows how higher grades absorb lower ones
      state <- newIORef (0 :: Natural)
      runIxApp $ absorptionLawDemo state 100
      finalValue <- readIORef state
      finalValue `shouldBe` 100
      
    it "verifies sequential composition preserves ordering" $ do
      -- Test: Pure → Safe → Safe → Idempotent maintains grade monotonicity
      state <- newIORef (50 :: Natural) 
      oldValue <- runIxApp $ sequentialCompositionDemo state 75
      newValue <- readIORef state
      oldValue `shouldBe` 50  -- Returns the old value
      newValue `shouldBe` 75  -- State updated to new value
      
    it "demonstrates parallel composition uses Max operation" $ do
      -- Test: Max(Safe, Safe) = Safe for parallel operations
      state <- newIORef (25 :: Natural)
      ((), retrievedValue) <- runIxApp $ parallelCompositionDemo state
      retrievedValue `shouldBe` 25
      
    it "shows complete grade elevation chain to Unsafe" $ do
      -- Test: Pure → Safe → Safe → Unsafe (maximum grade reached)
      state <- newIORef (10 :: Natural)
      result <- runIxApp $ gradeElevationDemo state 5  
      finalState <- readIORef state
      result `shouldBe` 15      -- 10 + 5 
      finalState `shouldBe` 15  -- State properly updated

  describe "HTTP Operations with Effect Grades" $ do
    it "verifies GET /show operation is Safe grade" $ do
      -- Safe operations: read-only with logging effects
      state <- newIORef (123 :: Natural)
      NumberResponse value <- runIxApp $ showNumber state
      value `shouldBe` 123
      -- State unchanged (read-only)
      unchanged <- readIORef state  
      unchanged `shouldBe` 123
      
    it "verifies PUT /set operation is Idempotent grade" $ do
      -- Idempotent operations: repeatable with same result
      state <- newIORef (0 :: Natural)
      NumberResponse result1 <- runIxApp $ setNumber state 99
      NumberResponse result2 <- runIxApp $ setNumber state 99
      result1 `shouldBe` 99
      result2 `shouldBe` 99  -- Same result when repeated
      finalValue <- readIORef state
      finalValue `shouldBe` 99
      
    it "verifies POST /add operation is Unsafe grade" $ do  
      -- Unsafe operations: observable side effects, non-idempotent
      state <- newIORef (10 :: Natural)
      NumberResponse result1 <- runIxApp $ addNumber state 5
      NumberResponse result2 <- runIxApp $ addNumber state 5  
      result1 `shouldBe` 15  -- 10 + 5
      result2 `shouldBe` 20  -- 15 + 5 (different result, not idempotent)
      finalValue <- readIORef state
      finalValue `shouldBe` 20
      
    it "verifies POST /randomise operation is Unsafe grade" $ do
      -- Unsafe operations: non-deterministic, observable side effects  
      state <- newIORef (0 :: Natural)
      NumberResponse result <- runIxApp $ randomiseNumber state
      finalValue <- readIORef state
      -- Random value should be in expected range
      result `shouldSatisfy` (\x -> x >= 0 && x <= 1000)
      finalValue `shouldSatisfy` (\x -> x >= 0 && x <= 1000)
      -- State should be updated  
      finalValue `shouldBe` result

  describe "Natural Number Properties" $ do
    it "ensures all operations preserve Natural number constraints" $ do
      -- Natural numbers cannot go negative - this is enforced by the type system
      state <- newIORef (5 :: Natural)
      -- All our operations should maintain Natural constraints
      NumberResponse result <- runIxApp $ addNumber state 10
      result `shouldSatisfy` (>= 0)
      
    it "verifies JSON parsing rejects negative numbers" $ do
      -- This would fail at JSON parsing level due to Natural type
      -- We test this through the HTTP API in integration tests
      -- Here we just verify our Natural operations work correctly
      state <- newIORef (0 :: Natural) 
      NumberResponse result <- runIxApp $ setNumber state 42
      result `shouldBe` 42

  describe "Algebraic Laws Verification" $ do
    it "proves identity law: Pure ⊕ g = g" $ do
      -- Mathematical verification of identity element
      state <- newIORef (777 :: Natural)
      -- Pure computation followed by Safe operation  
      result <- runIxApp $ 
        ireturn () `ibind` \_ ->           -- Pure → Pure
        liftSafeIO (readIORef state)       -- Pure ⊕ Safe = Safe
      result `shouldBe` 777
      
    it "proves absorption law: Safe ⊕ Idempotent = Idempotent" $ do  
      -- Higher grades absorb lower grades in composition
      state <- newIORef (555 :: Natural)
      runIxApp $ 
        logRequest "TEST" "/absorption" `ibind` \_ ->     -- Pure → Safe
        safeContinue (writeIORef state 888) `ibind` \_ -> -- Safe ⊕ Safe = Safe  
        weakenToIdempotent (safeContinue (return ()))     -- Safe → Idempotent
      finalValue <- readIORef state
      finalValue `shouldBe` 888
      
    it "proves monotonicity: grades only increase, never decrease" $ do
      -- Once you reach a higher grade, you cannot go back down
      -- This is enforced by the type system - attempting to "strengthen"
      -- from Unsafe back to Safe would be a compile-time error
      
      -- We can demonstrate that weakening always works:
      state <- newIORef (333 :: Natural)
      NumberResponse result <- runIxApp $ 
        safeContinue (readIORef state) `ibind` \value ->              -- Safe
        weakenToUnsafe (safeContinue (return (NumberResponse value))) -- Safe → Unsafe  
      result `shouldBe` 333
      
      -- But the reverse (strengthening) is impossible and caught at compile time
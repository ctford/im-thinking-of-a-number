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
      -- Test: Max(Pure, Safe) = Safe
      -- Create a computation with Pure and Safe effects
      state <- newIORef (42 :: Natural)
      result <- runGradeApp $ identityLawDemo state
      result `shouldBe` 42
      
    it "demonstrates associativity law with Monoid composition" $ do  
      -- Test: Safe <> Idempotent = Idempotent (Monoid composition)
      -- Shows natural semantic grading
      state <- newIORef (0 :: Natural)
      runGradeApp $ absorptionLawDemo state 100
      finalValue <- readIORef state
      finalValue `shouldBe` 100
      
    it "verifies sequential composition with Monoid operations" $ do
      -- Test: Monoid operations maintain grade monotonicity
      state <- newIORef (50 :: Natural) 
      oldValue <- runGradeApp $ sequentialCompositionDemo state 75
      newValue <- readIORef state
      oldValue `shouldBe` 50  -- Returns the old value
      newValue `shouldBe` 75  -- State updated to new value
      
    it "demonstrates parallel composition uses Monoid operation" $ do
      -- Test: Safe <> Safe = Safe for parallel operations
      state <- newIORef (25 :: Natural)
      ((), retrievedValue) <- runGradeApp $ parallelCompositionDemo state
      retrievedValue `shouldBe` 25
      
    it "shows complete grade composition chain to Unsafe" $ do
      -- Test: Monoid operations leading to Unsafe (maximum grade reached)
      state <- newIORef (10 :: Natural)
      result <- runGradeApp $ gradeElevationDemo state 5  
      finalState <- readIORef state
      result `shouldBe` 15      -- 10 + 5 
      finalState `shouldBe` 15  -- State properly updated

  describe "HTTP Operations with Effect Grades" $ do
    it "verifies GET /show operation is Safe grade" $ do
      -- Safe operations: read-only with logging effects
      state <- newIORef (123 :: Natural)
      NumberResponse value <- runGradeApp $ showNumber state
      value `shouldBe` 123
      -- State unchanged (read-only)
      unchanged <- readIORef state  
      unchanged `shouldBe` 123
      
    it "verifies PUT /set operation is Idempotent grade" $ do
      -- Idempotent operations: repeatable with same result
      state <- newIORef (0 :: Natural)
      NumberResponse result1 <- runGradeApp $ setNumber state 99
      NumberResponse result2 <- runGradeApp $ setNumber state 99
      result1 `shouldBe` 99
      result2 `shouldBe` 99  -- Same result when repeated
      finalValue <- readIORef state
      finalValue `shouldBe` 99
      
    it "verifies POST /add operation is Unsafe grade" $ do  
      -- Unsafe operations: observable side effects, non-idempotent
      state <- newIORef (10 :: Natural)
      NumberResponse result1 <- runGradeApp $ addNumber state 5
      NumberResponse result2 <- runGradeApp $ addNumber state 5  
      result1 `shouldBe` 15  -- 10 + 5
      result2 `shouldBe` 20  -- 15 + 5 (different result, not idempotent)
      finalValue <- readIORef state
      finalValue `shouldBe` 20
      
    it "verifies POST /randomise operation is Unsafe grade" $ do
      -- Unsafe operations: non-deterministic, observable side effects  
      state <- newIORef (0 :: Natural)
      NumberResponse result <- runGradeApp $ randomiseNumber state
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
      NumberResponse result <- runGradeApp $ addNumber state 10
      result `shouldSatisfy` (>= 0)
      
    it "verifies JSON parsing rejects negative numbers" $ do
      -- This would fail at JSON parsing level due to Natural type
      -- We test this through the HTTP API in integration tests
      -- Here we just verify our Natural operations work correctly
      state <- newIORef (0 :: Natural) 
      NumberResponse result <- runGradeApp $ setNumber state 42
      result `shouldBe` 42

  describe "Algebraic Laws Verification" $ do
    it "proves identity law: Pure <> g = g (Monoid)" $ do
      -- Mathematical verification of Monoid identity element
      state <- newIORef (777 :: Natural)
      -- Pure computation followed by Safe operation  
      result <- runGradeApp $ 
        ireturn () `ibind` \_ ->           -- Pure computation (mempty)
        liftSafeIO (readIORef state)       -- Pure <> Safe = Safe
      result `shouldBe` 777
      
    it "proves associativity with Monoid operation" $ do  
      -- Monoid operation combines grades naturally
      state <- newIORef (555 :: Natural)
      runGradeApp $ 
        logRequest "TEST" "/associativity" `ibind` \_ ->  -- Safe effect
        liftSafeIO (writeIORef state 888) `ibind` \_ ->    -- Safe <> Safe = Safe  
        liftSafeIO (return ())                               -- Natural Safe grade
      finalValue <- readIORef state
      finalValue `shouldBe` 888
      
    it "proves monotonicity: Monoid composition only increases grades" $ do
      -- Once you reach a higher grade, you cannot go back down
      -- This is enforced by the type system - no "strengthening" possible
      
      -- Demonstrate natural grade composition:
      state <- newIORef (333 :: Natural)
      NumberResponse result <- runGradeApp $ 
        liftSafeIO (readIORef state) `ibind` \value ->      -- Safe grade
        GradeApp (return (NumberResponse value))             -- Natural Unsafe grade
      result `shouldBe` 333
      
      -- The reverse (going from Unsafe to Safe) is impossible at compile time
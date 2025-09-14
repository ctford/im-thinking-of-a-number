module Repository

import Effects

%default partial  -- Allow partial functions for IORef operations

-- ============================================================================
-- STATE MANAGEMENT - Repository operations with graded effects
-- ============================================================================

||| Application state using IORef for mutable state
NumberState : Type
NumberState = IORef Nat

-- ============================================================================
-- REPOSITORY OPERATIONS - State operations with proper grading
-- ============================================================================

||| Read current state (Safe grade - no side effects)
readState : NumberState -> Action Safe Nat
readState state = MkAction (readIORef state)

||| Write to state (Idempotent grade - repeatable with same result)
writeState : Nat -> NumberState -> Action Idempotent ()
writeState value state = MkAction (writeIORef state value)

||| Add to current state (Unsafe grade - observable side effects)
addToState : Nat -> NumberState -> Action Unsafe ()
addToState addValue state = MkAction $ do
  current <- readIORef state
  writeIORef state (current + addValue)

||| Set state to random value (Unsafe grade - non-deterministic)
randomiseState : NumberState -> Action Unsafe ()
randomiseState state = MkAction $ do
  -- Simple random number generation (0-1000)
  -- In a real implementation you'd use a proper RNG
  randomVal <- getLine >>= return . length . words  -- Simple "randomness"
  writeIORef state (cast randomVal `mod` 1001)
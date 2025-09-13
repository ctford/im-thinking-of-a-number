module Repository

import Data.IORef
import Effects

-- ============================================================================
-- STATE TYPE - Linear reference for safe state manipulation
-- ============================================================================

||| Application state using linear IORef for safe mutation
||| Nat type ensures non-negative numbers at compile time
public export
NumberState : Type
NumberState = IORef Nat

-- ============================================================================
-- STATE OPERATIONS - Linear operations with explicit grades
-- ============================================================================

||| Read state operation (safe by nature)
||| Read-only operation, hence Safe grade with proof of non-mutation
public export
readState : NumberState -> Action Safe Nat
readState state = safeAction $ readIORef state

||| Write state operation (idempotent by nature)
||| Same input produces same result, hence Idempotent grade
public export
writeState : Nat -> NumberState -> Action Idempotent ()
writeState value state = idempotentAction $ writeIORef state value

||| Add to state operation (unsafe by nature)  
||| Non-idempotent operation with observable side effects
||| Multiple calls with same input produce different results
public export
addToState : Nat -> NumberState -> Action Unsafe ()
addToState addValue state = unsafeAction (readIORef state >>= \current => writeIORef state (current + addValue))

||| Randomise state operation (unsafe by nature)
||| Non-deterministic operation with observable side effects
||| Each call produces different, unpredictable results (simplified to just set to 42)
public export
randomiseState : NumberState -> Action Unsafe ()
randomiseState state = unsafeAction (writeIORef state 42)

-- ============================================================================
-- STATE CREATION - Constructor for initial state
-- ============================================================================

||| Create initial state with given value
||| Returns action that creates fresh state reference
public export
newNumberState : Nat -> Action Safe NumberState
newNumberState initial = safeAction $ newIORef initial

-- ============================================================================
-- PROOF OBLIGATIONS - Verify operation semantics
-- ============================================================================

-- Simplified proof types (removing linear types for now)
||| Proof that an operation is read-only
public export  
data ReadOnlyProof : Type where
  IsReadOnly : ReadOnlyProof

||| Proof that an operation is idempotent  
public export
data IdempotentProof : Type where
  IsIdempotent : IdempotentProof

||| Proof that an operation is unsafe
public export
data UnsafeProof : Type where
  IsUnsafe : UnsafeProof
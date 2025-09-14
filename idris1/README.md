# Idris 1 Graded Monad Implementation

This directory contains an implementation of the same graded monad system as the `idris2/` directory, but written for Idris 1 to take advantage of its more flexible syntax and better indexed monad support.

## Key Advantages of the Idris 1 Implementation

### 🎯 **Beautiful Do-Notation**
Unlike Idris 2, this implementation uses clean, readable do-notation throughout:

```idris
-- ✨ Clean and readable! 
showNumber state = do
  n <- readState state
  _ <- logRequest GET "/number" n
  return (MkNumberResponse n)

-- vs. Idris 2's verbose syntax:
-- readState state `bind` \n =>
-- logRequest GET "/number" n `bind` \_ =>
-- (MkAction (pure n) : Action Safe Nat)
```

### 🔧 **Indexed Monad Support**
Idris 1's `IxMonad` interface allows automatic grade composition:

```idris
interface IxMonad (m : i -> Type -> Type) where
  ireturn : a -> m i a
  ibind   : m i a -> (a -> m j b) -> m (gradeJoin i j) b
```

### 🚀 **Rebindable Syntax**
Custom `(>>=)` and `return` operators work seamlessly with do-notation:

```idris
namespace GradedSyntax
  (>>=) : Action i a -> (a -> Action j b) -> Action (gradeJoin i j) b
  (>>=) = ibind
  
  return : a -> Action Pure a
  return = ireturn
```

### 📊 **Automatic Grade Inference**
The type system automatically composes grades:

```idris
mixedGradeOperation : NumberState -> Action Unsafe ()
mixedGradeOperation state = do
  n1 <- readState state           -- Safe
  _ <- writeState (n1 + 1) state  -- Idempotent  
  _ <- addToState 5 state         -- Unsafe ← determines final grade
  n2 <- readState state           -- Safe
  _ <- logRequest POST "/mixed" n2 -- Safe
  return ()  -- Final grade: Unsafe ✨
```

## Project Structure

```
idris1/
├── Effects.idr      -- Graded monad with IxMonad interface
├── Repository.idr   -- State operations with graded effects  
├── HTTP.idr         -- HTTP operations using beautiful do-notation
├── Spec.idr         -- Comprehensive test suite
├── idris1.ipkg      -- Package configuration
├── run-unit-tests   -- Test runner script
└── README.md        -- This file
```

## Running the Tests

```bash
cd idris1
./run-unit-tests
```

This will build the project and run all tests, demonstrating:
- ✅ **HTTP Operations** with proper grade semantics
- ✅ **Grade Composition** with automatic inference
- ✅ **Mathematical Properties** (identity, associativity, idempotence)
- ✅ **Type Safety** with Natural number constraints
- ✅ **Clean Syntax** throughout

## Comparison with Idris 2

| Feature | Idris 1 | Idris 2 |
|---------|---------|---------|
| **Do-notation** | ✨ Clean & automatic | ❌ Verbose `bind` calls |
| **Grade inference** | ✨ Automatic | ❌ Manual composition |
| **IxMonad support** | ✨ Built-in | ❌ Manual implementation |
| **Rebindable syntax** | ✨ Seamless | ❌ Limited support |
| **Readability** | ✨ Excellent | ❌ Poor |

## Key Files

### `Effects.idr`
- Implements `IxMonad` interface for graded effects
- Provides rebindable syntax in `GradedSyntax` namespace
- Mathematical proofs of grade composition laws

### `HTTP.idr`  
- **Clean do-notation** for all HTTP operations
- Automatic grade composition (`Safe <> Unsafe = Unsafe`)
- Readable business logic without syntactic noise

### `Spec.idr`
- Comprehensive test suite using do-notation
- Tests demonstrate both functionality and beautiful syntax
- Mathematical property verification

## Why This Matters

This implementation shows that graded monads can have **beautiful, readable syntax** when the language provides proper indexed monad support. The difference in code quality between Idris 1 and Idris 2 for this use case is dramatic.

The Idris 1 version proves that the verbose `bind` syntax in the Idris 2 version isn't a fundamental limitation of graded monads—it's a limitation of Idris 2's reduced support for advanced type system features.
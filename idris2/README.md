# Idris 2 Graded Monad HTTP Effects

An Idris 2 implementation of the graded monad HTTP effects system, demonstrating how dependent types can provide stronger compile-time guarantees than Haskell.

## Key Improvements over Haskell Version

### Dependent Types & Compile-time Verification
- **Elegant grade composition** - Simply `gradeJoin = max` since grades form a total order!
- **Mathematical laws verified as proofs** (identity, idempotence, absorption)
- **`DecEq Grade`** enables compile-time grade reasoning  
- **HTTP handlers carry proof of their grade** in the type system

### Type Safety Enhancements
- **`Nat` type provides compile-time non-negative guarantees** - no runtime validation needed
- **`HttpHandler` GADT** relates HTTP verbs to their expected grades
- **Handler registry automatically computes correct grades**
- **Impossible to assign wrong grade to HTTP operation**

### Mathematical Rigor
- **Proof-carrying operations** with explicit verification functions
- **Grade laws proven at compile time** with dependent types
- **Type system enforces monotonicity** - grades can only increase

## Architecture

```
idris2/
├── Effects.idr     # Core graded monad with dependent types
├── Repository.idr  # State operations with explicit grades  
├── HTTP.idr        # HTTP handlers with grade verification
├── Spec.idr        # Unit tests with mathematical proofs
└── idris2.ipkg     # Package configuration
```

## Grade Hierarchy

```
Pure ⊑ Safe ⊑ Idempotent ⊑ Unsafe
```

- **Pure**: No effects, pure computation
- **Safe**: Logging, read-only operations
- **Idempotent**: Repeatable operations, same result
- **Unsafe**: Observable side effects, state changes

## HTTP Method Semantics

- `GET /number` → **Safe** (read-only operations)
- `PUT /number` → **Idempotent** (repeatable with same result)  
- `POST /number/add` → **Unsafe** (observable side effects)
- `POST /number/randomise` → **Unsafe** (non-deterministic effects)
- `DELETE /number` → **Idempotent** (reset to zero, repeatable)

## Running Tests

```bash
# Install Idris 2
brew install idris2

# Run unit tests
./run-unit-tests
```

## Test Output

The tests verify:
- **Grade composition laws** (6 tests) - Mathematical properties
- **HTTP operation semantics** (5 tests) - Grade correctness with live operations
- **Type system guarantees** (2 tests) - Dependent type features

All 13 tests demonstrate that the Idris 2 implementation provides stronger compile-time guarantees through dependent types while maintaining the same runtime semantics as the Haskell version.
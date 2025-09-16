# I'm Thinking of a Number - Graded Monad HTTP Effects

A comparative study of graded monad implementations for HTTP method semantics, featuring Haskell and Idris versions to demonstrate different approaches to effect system design.

## Overview

This project explores how graded monads can provide type-safe effect tracking for HTTP operations, ensuring that HTTP methods have semantically correct effect grades:

- **GET**: Safe (read-only operations)
- **PUT/DELETE**: Idempotent (repeatable with same result)  
- **POST**: Unsafe (observable side effects)

## Implementations

### [Haskell Implementation](./haskell/) 
**Traditional graded monads with type-level programming**

- **Approach**: Type families and Monoid composition with `<>` operator
- **Grade System**: `Pure < Safe < Idempotent < Unsafe` with automatic composition
- **Key Features**: Servant web framework, Natural number validation, comprehensive test suite
- **Tests**: 10 tests verifying HTTP semantics and graded monad laws

```bash
cd haskell
cabal build && cabal run im-thinking-of-a-number-exe
```

### [Idris Implementation](./idris/)
**Dependent types with compile-time verification**

- **Approach**: Dependent types with mathematical proofs and compile-time verification
- **Grade System**: Same hierarchy with stronger guarantees via dependent functions
- **Key Features**: Proof-carrying operations, mathematical law verification, `Nat` type safety
- **Tests**: 13 tests including compile-time proof verification
- **Note**: Uses verbose `bind` syntax due to Idris 2's limited indexed monad support

```bash
cd idris  
./run-unit-tests
```


## Key Differences

| Feature | Haskell | Idris |
|---------|---------|---------|
| **Syntax** | Clean with `<>` operators | Verbose `bind` chains |
| **Grade Composition** | Type families at compile time | Dependent functions with proofs |
| **Number Safety** | Runtime validation for Natural | `Nat` type - impossible to be negative |
| **Mathematical Laws** | Unit tests verify properties | Compile-time mathematical proofs |
| **Effect Verification** | Type system prevents downgrades | Dependent types + impossibility proofs |
| **Web Framework** | Full Servant HTTP server | Handler logic only (no server) |
| **Readability** | Good | Poor (verbose bind) |
| **Learning Curve** | Advanced Haskell type programming | Dependent type theory |

## Theoretical Foundation

All implementations are based on:

> Orchard, Dominic, Tomas Petricek, and Alan Mycroft. "[Effect systems via graded monads](https://www.cs.kent.ac.uk/people/staff/dao7/publ/haskell14-effects.pdf)." *Proceedings of the 2014 ACM SIGPLAN symposium on Haskell*. 2014.

**Core Concept**: Grade lattice forms a join-semilattice that is a Monoid:
- `Pure` is `mempty` (identity element)
- `<>` is join operation (least upper bound) 
- Composition: `Safe <> Idempotent = Idempotent` automatically

## Quick Start

### Haskell Version
```bash
# Install dependencies (macOS)
brew install ghc cabal-install

# Run Haskell implementation
cd haskell
cabal build && cabal run im-thinking-of-a-number-exe
open http://localhost:8080
```

### Idris Version  
```bash
# Install dependencies (macOS)
brew install idris2

# Run Idris tests
cd idris
./run-unit-tests
```


## Project Structure

```
├── haskell/           # Haskell implementation with web server
│   ├── src/           # Core graded monad system
│   ├── test/          # Comprehensive test suite  
│   ├── static/        # HTML frontend
│   └── *.cabal        # Build configuration
├── idris/             # Idris implementation with dependent types
│   ├── Effects.idr    # Core system with mathematical proofs
│   ├── Repository.idr # State operations with type safety
│   ├── HTTP.idr       # Handler logic with grade verification (verbose bind syntax)
│   └── Spec.idr       # Tests with compile-time proof verification
└── README.md          # This file
```

## Development

Both implementations provide their own development tooling:

- **Haskell**: Full web application with integration testing
- **Idris**: Proof-based verification with stronger compile-time guarantees

## Syntax Comparison

Here's the same HTTP operation implemented in both languages:

### Haskell (Good)
```haskell
showNumber state = do
  n <- liftIO $ readIORef state  
  liftIO $ logRequest GET "/number" n
  return $ NumberResponse n
```

### Idris (Verbose) 
```idris
showNumber state = 
  readState state `bind` \n =>
  logRequest GET "/number" n `bind` \_ =>
  (MkAction (pure n) : Action Safe Nat)
```

The verbose bind syntax in Idris is due to limited indexed monad support in Idris 2, but provides stronger compile-time guarantees through dependent types.

See individual README files for specific development instructions and architectural details.
# I'm Thinking of a Number - Graded Monad HTTP Effects

A comparative study of graded monad implementations for HTTP method semantics, featuring Haskell, Idris 2, and Idris 1 versions to demonstrate different approaches to effect system design and syntax capabilities.

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

### [Idris 2 Implementation](./idris2/)
**Dependent types with compile-time verification**

- **Approach**: Dependent types with mathematical proofs and compile-time verification
- **Grade System**: Same hierarchy with stronger guarantees via dependent functions
- **Key Features**: Proof-carrying operations, mathematical law verification, `Nat` type safety
- **Tests**: 13 tests including compile-time proof verification
- **Limitation**: Verbose `bind` syntax due to limited indexed monad support

```bash
cd idris2  
./run-unit-tests
```

### [Idris 1 Implementation](./idris1/) ✨
**Beautiful syntax with indexed monads and flexible do-notation**

- **Approach**: IxMonad interface with rebindable syntax for clean do-notation
- **Grade System**: Same hierarchy with automatic grade composition and inference
- **Key Features**: **Beautiful do-notation**, automatic deriving, indexed monad support
- **Tests**: 10 tests demonstrating clean syntax and automatic grade inference
- **Advantage**: Proves graded monads **can** have readable syntax with proper language support!

```bash
cd idris1
./run-unit-tests
```

## Key Differences

| Feature | Haskell | Idris 2 | Idris 1 ✨ |
|---------|---------|----------|-----------|
| **Syntax** | Clean with `<>` operators | Verbose `bind` chains | **Beautiful do-notation** |
| **Grade Composition** | Type families at compile time | Dependent functions with proofs | IxMonad with automatic inference |
| **Number Safety** | Runtime validation for Natural | `Nat` type - impossible to be negative | `Nat` type with deriving |
| **Mathematical Laws** | Unit tests verify properties | Compile-time mathematical proofs | Unit tests with clean syntax |
| **Effect Verification** | Type system prevents downgrades | Dependent types + impossibility proofs | Indexed monads with type safety |
| **Web Framework** | Full Servant HTTP server | Handler logic only (no server) | Handler logic only (no server) |
| **Readability** | Good | Poor (verbose bind) | **Excellent (do-notation)** |
| **Learning Curve** | Advanced Haskell type programming | Dependent type theory | Moderate indexed monads |

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

### Idris 2 Version  
```bash
# Install dependencies (macOS)
brew install idris2

# Run Idris 2 tests
cd idris2
./run-unit-tests
```

### Idris 1 Version (Best Syntax!) ✨
```bash
# Install dependencies (macOS) - Note: Idris 1 is legacy but worth it for the syntax!
brew install idris

# Run Idris 1 tests with beautiful do-notation
cd idris1
./run-unit-tests
```

## Project Structure

```
├── haskell/           # Haskell implementation with web server
│   ├── src/           # Core graded monad system
│   ├── test/          # Comprehensive test suite  
│   ├── static/        # HTML frontend
│   └── *.cabal        # Build configuration
├── idris2/            # Idris 2 implementation with dependent types
│   ├── Effects.idr    # Core system with mathematical proofs
│   ├── Repository.idr # State operations with type safety
│   ├── HTTP.idr       # Handler logic with grade verification (verbose bind syntax)
│   └── Spec.idr       # Tests with compile-time proof verification
├── idris1/ ✨         # Idris 1 implementation with beautiful syntax
│   ├── Effects.idr    # IxMonad interface with rebindable syntax
│   ├── Repository.idr # State operations with automatic deriving
│   ├── HTTP.idr       # Handler logic with gorgeous do-notation
│   ├── Spec.idr       # Tests demonstrating clean syntax
│   └── Demo.idr       # Syntax comparison and examples
└── README.md          # This file
```

## Development

All implementations provide their own development tooling:

- **Haskell**: Full web application with integration testing
- **Idris 2**: Proof-based verification with stronger compile-time guarantees  
- **Idris 1**: Beautiful syntax demonstration with indexed monad support

## Syntax Comparison

Here's the same HTTP operation implemented in all three languages:

### Haskell (Good)
```haskell
showNumber state = do
  n <- liftIO $ readIORef state  
  liftIO $ logRequest GET "/number" n
  return $ NumberResponse n
```

### Idris 2 (Verbose) 
```idris
showNumber state = 
  readState state `bind` \n =>
  logRequest GET "/number" n `bind` \_ =>
  (MkAction (pure n) : Action Safe Nat)
```

### Idris 1 (Beautiful!) ✨
```idris
showNumber state = do
  n <- readState state
  _ <- logRequest GET "/number" n  
  return (MkNumberResponse n)
```

The Idris 1 implementation proves that graded monads **can** have clean, readable syntax when the language provides proper indexed monad support!

See individual README files for specific development instructions and architectural details.
# I'm Thinking of a Number

A Haskell web application demonstrating how to use graded monad effects to typecheck HTTP method semantics.

## Tech Stack

- **Web Framework**: Servant (type-safe HTTP APIs)
- **Effects System**: Graded monad with Monoid composition using `<>` operator
- **Type System**: Grade hierarchy (Pure < Safe < Idempotent < Unsafe) with type-safe transitions  
- **Data Types**: Natural numbers for semantic correctness
- **Server**: Warp (high-performance web server)
- **Frontend**: HTML5 with JavaScript validation
- **Build Tool**: Cabal

## Features

This application demonstrates graded monad effects through a number-thinking game:

### HTTP Operations with Natural Semantic Grades
- **GET /show**: `Action 'Safe` (read-only operations)
- **PUT /set**: `Action 'Idempotent` (repeatable with same result)  
- **POST /add**: `Action 'Unsafe` (observable side effects)
- **POST /randomise**: `Action 'Unsafe` (non-deterministic effects)
- **DELETE /reset**: `Action 'Idempotent` (reset to zero, repeatable)

### Type System Features
- **Monoid Composition**: Grade forms a join-semilattice with `<>` operator
- **Effect Tracking**: Graded monads prevent unsafe grade downgrades
- **HTTP Semantics**: Type system enforces correct HTTP method usage
- **Natural Numbers**: Prevents negative values with validation chain
- **Semantic Grading**: Operations have their natural effect grades

## Quick Start

```bash
# Install GHC and Cabal (macOS)
brew install ghc cabal-install

# Build and run
cabal build
cabal exec im-thinking-of-a-number-exe

# Open browser
open http://localhost:8080
```

## Implementation Status

### Complete ✅
- **Monoid Grade System**: Standard Semigroup/Monoid instances with `<>` composition
- **Grade Hierarchy**: Complete `Pure < Safe < Idempotent < Unsafe` lattice structure
- **HTTP API**: All four routes with natural semantic grading
- **Simplified Operations**: Clean API without manual grade elevation
- **Type Safety**: Compile-time prevention of grade downgrades  
- **Natural Numbers**: Full validation chain (HTML + JS + Haskell)
- **Frontend**: HTML5 interface with client-side validation
- **Error Handling**: Proper HTTP status codes and JSON error responses

### Key Achievements
- **Type-Level Programming**: Uses type-level `<>` operator for Monoid composition
- **Semantic Grading**: Operations reflect their true nature (no artificial elevation needed)
- **Standard Abstractions**: Leverages Haskell's Semigroup/Monoid classes
- **Clean API**: Simple function names (`safe`, `idempotent`, `unsafe` for returns)
- **Effect Tracking**: Each HTTP operation has correct grade transitions
- **Semantic Correctness**: Natural numbers prevent invalid negative states
- **Development Speed**: Complete implementation in ~4 hours from scratch

## Theoretical Foundation

This implementation demonstrates **Grade as Monoid** for effect composition:

> Orchard, Dominic, Tomas Petricek, and Alan Mycroft. "[Effect systems via graded monads](https://www.cs.kent.ac.uk/people/staff/dao7/publ/haskell14-effects.pdf)." *Proceedings of the 2014 ACM SIGPLAN symposium on Haskell*. 2014.

**Key Innovation**: Grade lattice forms a natural join-semilattice that is a Monoid:
- `Pure` is `mempty` (identity element)
- `<>` is `max` operation (least upper bound)
- Composition: `Safe <> Idempotent = Idempotent` automatically
- Operations have natural semantic grades without manual elevation

## Code Structure

```
src/Lib.hs           # Main implementation with graded monads
static/index.html    # Frontend with validation
app/Main.hs         # Application entry point
*.cabal             # Dependencies and build configuration
```
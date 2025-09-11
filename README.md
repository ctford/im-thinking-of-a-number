# I'm Thinking of a Number

A Haskell web application demonstrating how to use graded monad effects to typecheck HTTP method semantics.

## Tech Stack

- **Web Framework**: Servant (type-safe HTTP APIs)
- **Effects System**: Graded monad with single type parameter and algebraic composition
- **Type System**: Grade hierarchy (Pure < Safe < Idempotent < Unsafe) with type-safe transitions  
- **Data Types**: Natural numbers for semantic correctness
- **Server**: Warp (high-performance web server)
- **Frontend**: HTML5 with JavaScript validation
- **Build Tool**: Cabal (with Hpack for package.yaml)

## Features

This application demonstrates graded monad effects through a number-thinking game:

### HTTP Operations with Effect Grades
- **GET /show**: `GradeApp 'Safe` (read-only, logging effects)
- **PUT /set**: `GradeApp 'Idempotent` (repeatable operations)  
- **POST /add**: `GradeApp 'Unsafe` (observable side effects)
- **POST /randomise**: `GradeApp 'Unsafe` (non-deterministic effects)

### Type System Features
- **Algebraic Composition**: Type families implement proper grade algebra
- **Effect Tracking**: Graded monads prevent unsafe grade downgrades
- **HTTP Semantics**: Type system enforces correct HTTP method usage
- **Natural Numbers**: Prevents negative values with validation chain

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

## Experimental Background

This is an experiment in one-shot application construction. The developer (Chris Ford) didn't have a Haskell environment set up beforehand, demonstrating that Claude Code can bootstrap a complete development environment and application from scratch.

## Implementation Status

### Complete ✅
- **Graded Monad System**: Single type parameter with Max operation for composition
- **Grade Hierarchy**: Complete `Pure < Safe < Idempotent < Unsafe` with semantic operation grading
- **HTTP API**: All four routes with proper method semantics
- **Type Safety**: Compile-time prevention of grade downgrades  
- **Natural Numbers**: Full validation chain (HTML + JS + Haskell)
- **Frontend**: HTML5 interface with client-side validation
- **Error Handling**: Proper HTTP status codes and JSON error responses

### Key Achievements
- **Type-Level Programming**: Uses `Max` type family for algebraic composition
- **Semantic Grading**: Operations have their natural grades (write = Idempotent, add = Unsafe)
- **Effect Tracking**: Each HTTP operation has correct grade transitions
- **Semantic Correctness**: Natural numbers prevent invalid negative states
- **Development Speed**: Complete implementation in ~4 hours from scratch

## Theoretical Foundation

This implementation is based on **Graded Monads** as described in:

> Orchard, Dominic, et al. "[Effect systems via graded monads](https://www.cs.kent.ac.uk/people/staff/dao7/publ/haskell14-effects.pdf)." *Proceedings of the 2014 ACM SIGPLAN symposium on Haskell*. 2014.

The key insight is that graded monads use a single type parameter to track effects, with composition via a monoidal structure (Max operation in our grade lattice).

## Code Structure

```
src/Lib.hs           # Main implementation with graded monads
static/index.html    # Frontend with validation
app/Main.hs         # Application entry point
package.yaml        # Dependencies and build configuration
```
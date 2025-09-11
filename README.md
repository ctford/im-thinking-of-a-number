# I'm Thinking of a Number

A Haskell web application demonstrating how to use indexed monad effects to typecheck HTTP method semantics.

## Tech Stack

- **Web Framework**: Servant (type-safe HTTP APIs)
- **Effects System**: Indexed monad with algebraic composition via type families
- **Type System**: Grade hierarchy (Pure < Safe < Idempotent < Unsafe) with type-safe transitions  
- **Data Types**: Natural numbers for semantic correctness
- **Server**: Warp (high-performance web server)
- **Frontend**: HTML5 with JavaScript validation
- **Build Tool**: Cabal (with Hpack for package.yaml)

## Features

This application demonstrates indexed monad effects through a number-thinking game:

### HTTP Operations with Effect Grades
- **GET /show**: `Pure → Safe` (read-only, logging effects)
- **PUT /set**: `Pure → Safe → Idempotent` (repeatable operations)  
- **POST /add**: `Pure → Safe → Unsafe` (observable side effects)

### Type System Features
- **Algebraic Composition**: Type families implement proper grade algebra
- **Effect Tracking**: Indexed monads prevent unsafe grade downgrades
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
- **Indexed Monad System**: Full algebraic composition with type families
- **Grade Hierarchy**: Complete `Pure < Safe < Idempotent < Unsafe` with transitions
- **HTTP API**: All three routes with proper method semantics
- **Type Safety**: Compile-time prevention of grade downgrades  
- **Natural Numbers**: Full validation chain (HTML + JS + Haskell)
- **Frontend**: HTML5 interface with client-side validation
- **Error Handling**: Proper HTTP status codes and JSON error responses

### Key Achievements
- **Type-Level Programming**: Uses `Combine` and `Max` type families for algebraic composition
- **Effect Tracking**: Each HTTP operation has correct grade transitions
- **Semantic Correctness**: Natural numbers prevent invalid negative states
- **Development Speed**: Complete implementation in ~4 hours from scratch

## Code Structure

```
src/Lib.hs           # Main implementation with indexed monads
static/index.html    # Frontend with validation
app/Main.hs         # Application entry point
package.yaml        # Dependencies and build configuration
```
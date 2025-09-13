# Haskell Implementation - Graded Monad HTTP Effects

A Haskell web application demonstrating graded monad effects for HTTP method semantics using type-level programming and Monoid composition.

## Tech Stack

- **Web Framework**: Servant (type-safe HTTP APIs)
- **Effects System**: Graded monad with Monoid composition using `<>` operator
- **Type System**: Grade hierarchy (Pure < Safe < Idempotent < Unsafe) with type-safe transitions  
- **Data Types**: Natural numbers for semantic correctness
- **Server**: Warp (high-performance web server)
- **Frontend**: HTML5 with JavaScript validation
- **Build Tool**: Cabal

## Project Structure

```
haskell/
├── app/Main.hs              # Application entry point
├── src/
│   ├── App.hs               # Main module with server initialization
│   ├── Effects.hs           # Core graded monad system
│   ├── HTTP.hs              # Web layer with business operations
│   └── Repository.hs        # Data access operations
├── test/Spec.hs             # Comprehensive test suite
├── static/index.html        # HTML frontend
├── im-thinking-of-a-number.cabal  # Cabal configuration
├── run-unit-tests           # Unit test script
├── run-integration-tests    # Integration test script
└── start-server             # Full integration test script
```

## Development Commands

### Build and Run
```bash
cd haskell
cabal build                    # Build the project (using system GHC)
cabal run im-thinking-of-a-number-exe  # Run the server
cabal test                     # Run tests
```

### Testing Scripts
- **`./run-unit-tests`** - **Unit tests**: Graded monad laws, HTTP semantics (no server required)
- **`./run-integration-tests`** - **Integration tests**: API endpoints via cURL (requires server running)
- **`./start-server`** - **Full integration**: Starts server → runs API tests → stops server

### Run with Logging
```bash
# Run server with separate stdout and stderr logging
cabal run im-thinking-of-a-number-exe > server.log 2> error.log

# Run server with both stdout and stderr to single log file
cabal run im-thinking-of-a-number-exe > server.log 2>&1

# View logs in real-time while server runs
tail -f server.log      # HTTP request logs
tail -f error.log       # Build output and error messages
```

## HTTP API

Server runs on http://localhost:8080

### HTTP Operations with Semantic Grades
- **GET /number**: `Action 'Safe` (read-only operations)
- **PUT /number**: `Action 'Idempotent` (repeatable with same result)  
- **POST /number/add**: `Action 'Unsafe` (observable side effects)
- **POST /number/randomise**: `Action 'Unsafe` (non-deterministic effects)
- **DELETE /number**: `Action 'Idempotent` (reset to zero, repeatable)

### API Testing
```bash
# Basic operations
curl http://localhost:8080/number
curl -X PUT -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/number
curl -X POST -H "Content-Type: application/json" -d '{"value": 8}' http://localhost:8080/number/add  
curl -X POST http://localhost:8080/number/randomise
curl -X DELETE http://localhost:8080/number
```

Web interface: http://localhost:8080

## Features

### Type System Features
- **Monoid Composition**: Grade forms a join-semilattice with `<>` operator
- **Effect Tracking**: Graded monads prevent unsafe grade downgrades
- **HTTP Semantics**: Type system enforces correct HTTP method usage
- **Natural Numbers**: Prevents negative values with validation chain
- **Semantic Grading**: Operations have their natural effect grades

### Implementation Achievements
- **Type-Level Programming**: Uses type-level `<>` operator for Monoid composition
- **Semantic Grading**: Operations reflect their true nature (no artificial elevation needed)
- **Standard Abstractions**: Leverages Haskell's Semigroup/Monoid classes
- **Clean API**: Simple function names (`safe`, `idempotent`, `unsafe` for returns)
- **Effect Tracking**: Each HTTP operation has correct grade transitions
- **Comprehensive Tests**: 10 tests verifying HTTP semantics and graded monad laws

## Theoretical Foundation

This implementation demonstrates **Grade as Monoid** for effect composition:

> Orchard, Dominic, Tomas Petricek, and Alan Mycroft. "[Effect systems via graded monads](https://www.cs.kent.ac.uk/people/staff/dao7/publ/haskell14-effects.pdf)." *Proceedings of the 2014 ACM SIGPLAN symposium on Haskell*. 2014.

**Key Innovation**: Grade lattice forms a natural join-semilattice that is a Monoid:
- `Pure` is `mempty` (identity element)
- `<>` is `max` operation (least upper bound)  
- Composition: `Safe <> Idempotent = Idempotent` automatically
- Operations have natural semantic grades without manual elevation
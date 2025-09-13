# Development Guide

Comparative study of graded monad implementations with both Haskell and Idris 2 versions.

## Project Structure
```
im-thinking-of-a-number/
├── haskell/                  -- Haskell implementation
│   ├── app/Main.hs           -- Application entry point
│   ├── src/                  -- Core graded monad system
│   ├── test/Spec.hs          -- Comprehensive test suite
│   ├── static/index.html     -- HTML frontend
│   └── *.cabal               -- Cabal configuration
└── idris2/                   -- Idris 2 implementation  
    ├── Effects.idr           -- Core system with proofs
    ├── Repository.idr        -- State operations
    ├── HTTP.idr              -- Handler logic
    ├── Spec.idr              -- Tests with proofs
    └── *.ipkg                -- Package configuration
```

## Development Commands

### Haskell Implementation
```bash
cd haskell
cabal build                    # Build the project (using system GHC)
cabal run im-thinking-of-a-number-exe  # Run the server
cabal test                     # Run tests
```

### Idris 2 Implementation
```bash
cd idris2
idris2 --build idris2.ipkg     # Build the project
./run-unit-tests              # Run tests
```

### Haskell Run with Logging
```bash
cd haskell
# Run server with separate stdout and stderr logging
cabal run im-thinking-of-a-number-exe > server.log 2> error.log

# Run server with both stdout and stderr to single log file
cabal run im-thinking-of-a-number-exe > server.log 2>&1

# View logs in real-time while server runs
tail -f server.log      # HTTP request logs
tail -f error.log       # Build output and error messages
```

**Note**: HTTP request logs are written to stdout, while build output and error messages go to stderr. Separating them makes debugging easier.

### Additional Development Commands
```bash
# Haskell
cd haskell && cabal repl       # Start REPL
cd haskell && cabal clean      # Clean build artifacts

# Idris 2  
cd idris2 && idris2 --repl     # Start REPL
cd idris2 && rm -rf build/     # Clean build artifacts
```

## Implementation

### Features
- **Graded monad system** with type-level grade combination (`Action g a`)
- **HTTP API endpoints** with semantic grading:
  - `GET /number` → `Safe` (read-only)  
  - `PUT /number` → `Idempotent` (repeatable)
  - `POST /number/add` → `Unsafe` (side effects)
  - `POST /number/randomise` → `Unsafe` (non-deterministic)
  - `DELETE /number` → `Idempotent` (reset to zero, repeatable)
- **Grade hierarchy**: `Pure < Safe < Idempotent < Unsafe`
- **Type safety**: Prevents unsafe grade downgrades at compile time
- **Natural numbers**: Full validation chain (HTML + JavaScript + Haskell)
- **Comprehensive test suite**: 10 tests verifying HTTP semantics and graded monad laws

## API Testing

Server runs on http://localhost:8080

```bash
# Basic operations
curl http://localhost:8080/number
curl -X PUT -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/number
curl -X POST -H "Content-Type: application/json" -d '{"value": 8}' http://localhost:8080/number/add  
curl -X POST http://localhost:8080/number/randomise
curl -X DELETE http://localhost:8080/number

# Error case (wrong HTTP method)
curl -X POST http://localhost:8080/number  # Should return 405
```

Web interface: http://localhost:8080

## Claude Code Configuration

### Testing Scripts

#### Haskell Testing
- `cd haskell && ./run-unit-tests` - Unit tests (no server required)
- `cd haskell && ./run-integration-tests` - API tests (requires running server)
- `cd haskell && ./start-server` - Full integration: start server → run tests → stop server

#### Idris 2 Testing
- `cd idris2 && ./run-unit-tests` - Unit tests with mathematical proofs

### Hooks
Configure these hooks in Claude Code settings to automatically run tests after code changes:

```json
{
  "hooks": {
    "tool-call": {
      "Edit": "cd haskell && cabal build && cabal test && ./start-server",
      "Write": "cd haskell && cabal build && cabal test && ./start-server", 
      "MultiEdit": "cd haskell && cabal build && cabal test && ./start-server"
    }
  }
}
```

### Git Permissions
Claude has advance permission to run git commands (`git status`, `git add`, `git commit`, `git diff`) without asking, to streamline the development workflow.
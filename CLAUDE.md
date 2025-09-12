# Development Guide

Haskell web application demonstrating graded monad effects for HTTP method semantics.

## Project Structure
```
im-thinking-of-a-number/
├── app/Main.hs              -- Application entry point
├── src/Lib.hs               -- Graded monad implementation
├── test/Spec.hs             -- Comprehensive test suite
├── static/index.html        -- HTML frontend
└── im-thinking-of-a-number.cabal  -- Cabal configuration
```

## Development Commands

### Build and Run
```bash
cabal build                    # Build the project (using system GHC)
cabal run im-thinking-of-a-number-exe  # Run the server
cabal test                     # Run tests
```

### Run with Logging
```bash
# Run server with separate stdout and stderr logging
cabal run im-thinking-of-a-number-exe > server.log 2> error.log

# Run server with both stdout and stderr to single log file
cabal run im-thinking-of-a-number-exe > server.log 2>&1

# Run server with unbuffered output for immediate logging (macOS: install coreutils)
# brew install coreutils
gstdbuf -oL -eL cabal run im-thinking-of-a-number-exe > server.log 2> error.log

# View logs in real-time while server runs
tail -f server.log      # HTTP request logs
tail -f error.log       # Build output and error messages

# View logs after server stops
cat server.log          # HTTP request logs and server output
cat error.log           # Build errors and system errors
```

**Note**: This project uses Cabal for package management and building. HTTP request logs are written to stdout, while build output and error messages go to stderr. Separating them makes debugging easier.

### Development
```bash
cabal repl                     # Start REPL
cabal clean                    # Clean build artifacts
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
- `./run-tests` - Test API endpoints (requires running server)
- `./start-server` - Start server, run tests, stop server

### Hooks
Configure these hooks in Claude Code settings to automatically run tests and verify server functionality after code changes:

```json
{
  "hooks": {
    "tool-call": {
      "Edit": "cabal build && cabal test && ./start-server",
      "Write": "cabal build && cabal test && ./start-server", 
      "MultiEdit": "cabal build && cabal test && ./start-server"
    }
  }
}
```

### Git Permissions
Claude has advance permission to run git commands (`git status`, `git add`, `git commit`, `git diff`) without asking, to streamline the development workflow.
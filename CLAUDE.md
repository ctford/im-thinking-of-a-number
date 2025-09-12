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
  - `GET /show` → `Safe` (read-only)  
  - `PUT /set` → `Idempotent` (repeatable)
  - `POST /add` → `Unsafe` (side effects)
  - `POST /randomise` → `Unsafe` (non-deterministic)
  - `DELETE /reset` → `Idempotent` (reset to zero, repeatable)
- **Grade hierarchy**: `Pure < Safe < Idempotent < Unsafe`
- **Type safety**: Prevents unsafe grade downgrades at compile time
- **Natural numbers**: Full validation chain (HTML + JavaScript + Haskell)
- **Comprehensive test suite**: 10 tests verifying HTTP semantics and graded monad laws

## API Testing

Server runs on http://localhost:8080

```bash
# Basic operations
curl http://localhost:8080/show
curl -X PUT -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/set
curl -X POST -H "Content-Type: application/json" -d '{"value": 8}' http://localhost:8080/add  
curl -X POST http://localhost:8080/randomise
curl -X DELETE http://localhost:8080/reset

# Error case (wrong HTTP method)
curl -X POST http://localhost:8080/set  # Should return 405
```

Web interface: http://localhost:8080

## Claude Code Permissions

Claude has advance permission to proactively run these commands without asking:

### Build & Test Commands
- `cabal build` - Build project after code changes
- `cabal test` - Run test suite to verify functionality
- `cabal run im-thinking-of-a-number-exe` - Start server for testing

### Git Commands  
- `git status` - Check repository status
- `git add` - Stage files for commit
- `git commit` - Commit changes with descriptive messages
- `git diff` - Review changes before committing

### Testing Commands
- `curl` commands - Test API endpoints (GET /show, POST /add, etc.)
- Server startup/shutdown for endpoint verification

This eliminates the need to ask permission for standard development workflow steps while keeping explanations of what's being done.
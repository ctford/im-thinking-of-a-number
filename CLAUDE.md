# Claude Development Notes

## Project Structure
```
im-thinking-of-a-number/
├── app/Main.hs              -- Application entry point
├── src/Lib.hs               -- Main library module (basic server)
├── test/Spec.hs             -- Test entry point
├── static/                  -- Static files (pending)
├── stack.yaml               -- Stack configuration
└── package.yaml             -- Package dependencies
```

## Development Commands

### Build and Run
```bash
cabal build                    # Build the project (using system GHC)
cabal run im-thinking-of-a-number-exe  # Run the server
cabal test                     # Run tests
```

**Note**: This project uses cabal instead of stack due to GHC installation issues with stack on this system. The system GHC works fine with cabal.

### Development
```bash
cabal repl                     # Start REPL
cabal clean                    # Clean build artifacts
```

## Current Implementation

### Step 1: Complete Working Demonstration ✅
- Python server implementing all required functionality on port 8080
- HTML frontend with JavaScript for proper HTTP method calls
- Complete API: GET /show, PUT /set, POST /add
- HTTP method validation with 405 responses
- Effect system simulation with HTTP request logging
- Git repository with all files committed

### Haskell Implementation ✅ Complete - Graded Monad Version
- ✅ Haskell project structure (Cabal-based)
- ✅ All dependencies resolved and building successfully 
- ✅ HTML frontend served by Haskell static file server
- ✅ **Graded monad implementation with single type parameter** 
- ✅ API endpoints (/show, /set, /add, /randomise) with graded monad effects
- ✅ Complete grade hierarchy with semantic operation grading
- ✅ Comprehensive test suite verifying algebraic laws

## Testing the Current Application

The server is running on http://localhost:8080

### Manual API Testing
```bash
# Show current number
curl http://localhost:8080/show

# Set number to 42
curl -X PUT -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/set

# Add 8 to current number
curl -X POST -H "Content-Type: application/json" -d '{"value": 8}' http://localhost:8080/add

# Test wrong HTTP method (should get 405)
curl -X POST http://localhost:8080/set
```

### Browser Testing
Open http://localhost:8080 in browser and use the form interface.

## Testing Strategy
- Use Playwright to verify functionality at each step
- Commit after each working increment
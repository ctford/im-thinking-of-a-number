# Development Guide

## Project Structure
```
im-thinking-of-a-number/
├── app/Main.hs              -- Application entry point
├── src/Lib.hs               -- Main library module 
├── test/Spec.hs             -- Test entry point
├── static/                  -- Static files
└── im-thinking-of-a-number.cabal  -- Cabal package configuration
```

## Development Commands

### Build and Run
```bash
cabal build                    # Build the project (using system GHC)
cabal run im-thinking-of-a-number-exe  # Run the server
cabal test                     # Run tests
```

**Note**: This project uses Cabal for package management and building.

### Development
```bash
cabal repl                     # Start REPL
cabal clean                    # Clean build artifacts
```

## Current Implementation

### Implementation Status ✅ Complete
- ✅ **Graded monad system** with proper type-level grade combination
- ✅ **HTTP API endpoints** (/show, /set, /add, /randomise) with semantic grading
- ✅ **Grade hierarchy** (Pure < Safe < Idempotent < Unsafe) with automatic composition
- ✅ **Comprehensive test suite** (9 examples) verifying HTTP semantics and graded monad laws
- ✅ **HTML frontend** with client-side validation and proper HTTP method usage
- ✅ **Cabal build system** with clean project structure

### Recent Changes
- ✅ **Refactored random number generation** - Moved random generation logic into `randomiseState` function in IORef operations section
  - `randomiseState :: NumberState -> GradeApp 'Unsafe ()` - generates random number and writes to state
  - `randomiseNumber` now calls `randomiseState` followed by `readState` to demonstrate explicit state reading pattern
  - Maintains proper separation of concerns between state mutation and value retrieval

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

# Generate random number
curl -X POST http://localhost:8080/randomise

# Test wrong HTTP method (should get 405)
curl -X POST http://localhost:8080/set
```

### Browser Testing
Open http://localhost:8080 in browser and use the form interface.

## Testing Strategy
- Use Playwright to verify functionality at each step
- Commit after each working increment
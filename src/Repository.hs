{-# LANGUAGE DataKinds #-}

module Repository
    ( readState
    , writeState
    , addToState
    , randomiseState
    ) where

import Data.IORef
import Numeric.Natural
import System.Random
import Effects

-- ============================================================================
-- IOREF OPERATIONS - State manipulation with explicit grades
-- ============================================================================

-- Read state operation (safe by nature)
-- Read-only operation, hence Safe grade
readState :: NumberState -> Action 'Safe Natural
readState state = Action $ do
    readIORef state

-- Write state operation (idempotent by nature)
-- Same input produces same result, hence Idempotent grade
writeState :: Natural -> NumberState -> Action 'Idempotent ()
writeState value state = Action $ do
    writeIORef state value

-- Add to state operation (unsafe by nature)
-- Non-idempotent operation, hence Unsafe grade
addToState :: Natural -> NumberState -> Action 'Unsafe ()
addToState addValue state = Action $ do
    modifyIORef state (+ addValue)

-- Randomise state operation (unsafe by nature)
-- Non-deterministic operation, hence Unsafe grade
randomiseState :: NumberState -> Action 'Unsafe ()
randomiseState state = Action $ do
    randomVal <- fromIntegral <$> randomRIO (0, 1000 :: Int)
    writeIORef state randomVal
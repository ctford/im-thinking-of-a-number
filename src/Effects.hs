{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Effects
    ( Grade(..)
    , Action(..)
    , bind
    , safe
    , idempotent 
    , unsafe
    , NumberState
    , HttpVerb(..)
    , NumberRequest(..)
    , NumberResponse(..)
    ) where

import qualified Prelude
import Prelude (IO, (.), Show, Eq, ($), (<$>))
import Data.IORef
import Numeric.Natural
import Data.Aeson

-- ============================================================================
-- GRADE HIERARCHY - Complete lattice for HTTP effect classification
-- ============================================================================
{-
   Grade Lattice (⊑ = "less safe than"):
   
       Pure ⊑ Safe ⊑ Idempotent ⊑ Unsafe
       
   ASCII Diagram:
   
       Pure      (no effects, pure computation)
         |
       Safe      (logging, read-only operations)  
         |
    Idempotent   (repeatable operations, same result)
         |
      Unsafe     (observable side effects, state changes)
      
   Properties:
   • Monotonic: Operations can only increase grade (no downgrades)
   • Algebraic: Composition follows mathematical laws
   • HTTP Semantic: Maps to proper HTTP method semantics
-}
data Grade = Pure | Safe | Idempotent | Unsafe

-- Type-level Monoid operation implementing grade combination
-- Uses the same ordering as the Ord instance: Pure < Safe < Idempotent < Unsafe
type family (g :: Grade) <> (h :: Grade) :: Grade where
    'Pure <> g = g                    -- Pure is identity (left)
    g <> 'Pure = g                    -- Pure is identity (right)
    g <> g = g                        -- Idempotent law: g <> g = g
    'Unsafe <> _ = 'Unsafe            -- Unsafe is absorbing element
    _ <> 'Unsafe = 'Unsafe            -- Unsafe is absorbing element
    'Idempotent <> 'Safe = 'Idempotent     -- Remaining explicit case
    'Safe <> 'Idempotent = 'Idempotent     -- Remaining explicit case

-- ============================================================================
-- MONOID COMPOSITION - Grade forms a join-semilattice
-- ============================================================================
{-
   Monoid Laws for Grade composition:
   
   IDENTITY LAWS:
   • mempty <> g = g        (Pure is left identity)
   • g <> mempty = g        (Pure is right identity)
   
   ASSOCIATIVITY:
   • (g <> h) <> i = g <> (h <> i)    (Composition is associative)
   
   ABSORPTION (via <> operation):
   • Safe <> Safe = Safe
   • Safe <> Idempotent = Idempotent     (<> takes higher grade)
   • Safe <> Unsafe = Unsafe            (<> takes higher grade)
   • Idempotent <> Unsafe = Unsafe      (<> takes higher grade)
   
   IDEMPOTENCE:
   • g <> g = g             (<> is idempotent)
   
   MONOTONICITY:
   • If g₁ ≤ g₂, then h <> g₁ ≤ h <> g₂  (Grade can only increase)
-}

-- Graded monad for effect tracking with single grade parameter
newtype Action (g :: Grade) a = Action { runAction :: IO a }

-- Graded bind: composition uses Monoid operation (<>)
bind :: Action g a -> (a -> Action h b) -> Action (g <> h) b
bind (Action x) f = Action (x Prelude.>>= runAction . f)

-- Convenience constructors for different effect grades
safe :: a -> Action 'Safe a
safe = Action . Prelude.return

idempotent :: a -> Action 'Idempotent a
idempotent = Action . Prelude.return

unsafe :: a -> Action 'Unsafe a
unsafe = Action . Prelude.return

-- Global state for the number
type NumberState = IORef Natural

-- HTTP verb sum type for type-safe logging  
data HttpVerb = GET | PUT | POST | DELETE deriving (Show, Eq)

-- JSON data types
data NumberRequest = NumberRequest { value :: Natural } deriving Show
data NumberResponse = NumberResponse { current :: Natural } deriving Show

instance FromJSON NumberRequest where
    parseJSON = withObject "NumberRequest" $ \o -> NumberRequest <$> o .: "value"

instance ToJSON NumberResponse where
    toJSON (NumberResponse n) = object ["value" .= n]
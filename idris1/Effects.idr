module Effects

%default total

-- ============================================================================
-- GRADE HIERARCHY - Dependent types for HTTP effect classification
-- ============================================================================

||| Grade hierarchy representing HTTP method semantics
||| Uses dependent types for compile-time verification of effect composition
||| Ordered as: Pure < Safe < Idempotent < Unsafe
data Grade = Pure | Safe | Idempotent | Unsafe

-- Idris 1's automatic deriving - much cleaner than manual instances!
%auto deriving instance Eq Grade
%auto deriving instance Ord Grade  
%auto deriving instance Show Grade

-- ============================================================================
-- GRADE COMPOSITION - max operation for join semilattice
-- ============================================================================

||| Grade composition implementing join semilattice
||| Since Grade forms a total order, the join operation is simply `max`
gradeJoin : Grade -> Grade -> Grade
gradeJoin = max

-- ============================================================================
-- GRADED/INDEXED MONAD - Taking advantage of Idris 1's flexible syntax
-- ============================================================================

||| Graded monad for effect tracking using indexed monad approach
||| This leverages Idris 1's better support for indexed monads
data Action : Grade -> Type -> Type where
  MkAction : IO a -> Action g a

||| Extract the underlying IO computation
runAction : Action g a -> IO a
runAction (MkAction io) = io

-- ============================================================================
-- INDEXED MONAD IMPLEMENTATION - Using Idris 1's flexible syntax
-- ============================================================================

||| Indexed Monad interface for graded effects
||| This is where Idris 1 shines with its flexible syntax support
interface IxMonad (m : i -> Type -> Type) where
  ireturn : a -> m i a
  ibind   : m i a -> (a -> m j b) -> m (gradeJoin i j) b

||| Implement IxMonad for our Action type
instance IxMonad Action where
  ireturn x = MkAction (return x)
  ibind (MkAction mx) f = MkAction $ do
    x <- mx
    let (MkAction my) = f x
    my

-- ============================================================================
-- REBINDABLE SYNTAX - Idris 1's powerful do-notation support
-- ============================================================================

||| Enable rebindable syntax for graded monads
||| Idris 1 allows us to override the do-notation operators
namespace GradedSyntax
  (>>=) : Action i a -> (a -> Action j b) -> Action (gradeJoin i j) b
  (>>=) = ibind
  
  return : a -> Action Pure a
  return = ireturn

-- ============================================================================
-- CONVENIENCE CONSTRUCTORS - Type-directed construction
-- ============================================================================

||| Construct actions with specific grades
safeAction : IO a -> Action Safe a
safeAction = MkAction

idempotentAction : IO a -> Action Idempotent a
idempotentAction = MkAction

unsafeAction : IO a -> Action Unsafe a
unsafeAction = MkAction

-- ============================================================================
-- PROOFS - Mathematical properties of grade composition
-- ============================================================================

||| Proof that Pure is left identity for grade composition
pureLeftIdentity : (g : Grade) -> gradeJoin Pure g = g
pureLeftIdentity Pure = Refl
pureLeftIdentity Safe = Refl
pureLeftIdentity Idempotent = Refl
pureLeftIdentity Unsafe = Refl

||| Proof that Pure is right identity for grade composition  
pureRightIdentity : (g : Grade) -> gradeJoin g Pure = g
pureRightIdentity Pure = Refl
pureRightIdentity Safe = Refl
pureRightIdentity Idempotent = Refl
pureRightIdentity Unsafe = Refl

||| Proof that grade composition is idempotent
gradeIdempotent : (g : Grade) -> gradeJoin g g = g
gradeIdempotent Pure = Refl
gradeIdempotent Safe = Refl
gradeIdempotent Idempotent = Refl
gradeIdempotent Unsafe = Refl
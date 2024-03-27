module Data.Either.Semigroup
  ( EitherS (LeftS, RightS)
  , toEither
  , unLeftS
  , unRightS
  , eitherS
  , leftsS
  , rightsS
  ) where

import Control.Monad.Fix (MonadFix (mfix))
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifoldable1
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Data (Data, Typeable)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)

-- | The 'EitherS' type represents values with two possibilities:
-- a value of type 'EitherS' @a b@ is either 'LeftS' @a@ or 'RightS' @b@.
--
-- The 'EitherS' type is sometimes used to represent a value which is either correct or an error;
-- by convention, the 'LeftS' constructor is used to hold an error value and the 'RightS' constructor
-- is used to hold a correct value (mnemonic: "right" also means "correct").
--
-- This type differs from 'Data.Either.Either' in the 'Semigroup' instance, requiring
-- the 'LeftS' type to be a 'Semigroup' in the first place.
data EitherS l r = LeftS l | RightS r
  deriving (Eq, Ord, Show, Read, Functor, Generic, Generic1, Data, Typeable)

instance (Semigroup l) => Semigroup (EitherS l r) where
  (<>) :: (Semigroup l) => EitherS l r -> EitherS l r -> EitherS l r
  LeftS l0 <> LeftS l1 = LeftS (l0 <> l1)
  RightS r <> _ = RightS r
  _ <> RightS r = RightS r

instance (Monoid l) => Monoid (EitherS l r) where
  mempty :: (Monoid l) => EitherS l r
  mempty = LeftS mempty

instance (Monoid l) => Applicative (EitherS l) where
  pure :: (Monoid l) => r -> EitherS l r
  pure = RightS

  (<*>) :: (Monoid l) => EitherS l (r0 -> r1) -> EitherS l r0 -> EitherS l r1
  RightS f <*> RightS x = RightS (f x)
  LeftS l <*> _ = LeftS l
  _ <*> LeftS l = LeftS l

instance (Monoid l) => Monad (EitherS l) where
  (>>=) :: (Monoid l) => EitherS l r0 -> (r0 -> EitherS l r1) -> EitherS l r1
  RightS m >>= f = f m
  LeftS l >>= _ = LeftS l

instance (Monoid l) => MonadFix (EitherS l) where
  mfix :: (Monoid l) => (r -> EitherS l r) -> EitherS l r
  mfix f = let x = f (un x) in x
   where
    un = \case
      LeftS _ -> error "mfix EitherS: LeftS"
      RightS r -> r

instance Bifunctor EitherS where
  bimap :: (l0 -> l1) -> (r0 -> r1) -> EitherS l0 r0 -> EitherS l1 r1
  bimap f g = \case
    LeftS l -> LeftS (f l)
    RightS r -> RightS (g r)

instance Foldable (EitherS l) where
  foldMap :: (Monoid m) => (r -> m) -> EitherS l r -> m
  foldMap f = \case
    LeftS _ -> mempty
    RightS r -> f r

instance Bifoldable EitherS where
  bifoldMap :: (Monoid m) => (l -> m) -> (r -> m) -> EitherS l r -> m
  bifoldMap = eitherS

instance Bifoldable1 EitherS where
  bifoldMap1 :: (Semigroup s) => (l -> s) -> (r -> s) -> EitherS l r -> s
  bifoldMap1 = eitherS

instance Traversable (EitherS l) where
  traverse ::
    (Applicative f) =>
    (r0 -> f r1) ->
    EitherS l r0 ->
    f (EitherS l r1)
  traverse f = \case
    LeftS l -> pure (LeftS l)
    RightS r -> RightS <$> f r

instance Bitraversable EitherS where
  bitraverse ::
    (Applicative f) =>
    (l0 -> f l1) ->
    (r0 -> f r1) ->
    EitherS l0 r0 ->
    f (EitherS l1 r1)
  bitraverse f g = \case
    LeftS l -> LeftS <$> f l
    RightS r -> RightS <$> g r

instance (Eq l) => Eq1 (EitherS l) where
  liftEq ::
    (Eq l) =>
    (r0 -> r1 -> Bool) ->
    EitherS l r0 ->
    EitherS l r1 ->
    Bool
  liftEq = liftEq2 (==)

instance Eq2 EitherS where
  liftEq2 ::
    (l0 -> l1 -> Bool) ->
    (r0 -> r1 -> Bool) ->
    EitherS l0 r0 ->
    EitherS l1 r1 ->
    Bool
  liftEq2 f _ (LeftS l0) = \case
    LeftS l1 -> f l0 l1
    RightS _ -> False
  liftEq2 _ g (RightS r0) = \case
    LeftS _ -> False
    RightS r1 -> g r0 r1

instance (Ord l) => Ord1 (EitherS l) where
  liftCompare ::
    (Eq l) =>
    (r0 -> r1 -> Ordering) ->
    EitherS l r0 ->
    EitherS l r1 ->
    Ordering
  liftCompare = liftCompare2 compare

instance Ord2 EitherS where
  liftCompare2 ::
    (l0 -> l1 -> Ordering) ->
    (r0 -> r1 -> Ordering) ->
    EitherS l0 r0 ->
    EitherS l1 r1 ->
    Ordering
  liftCompare2 f _ (LeftS l0) = \case
    LeftS l1 -> f l0 l1
    RightS _ -> LT
  liftCompare2 _ g (RightS r0) = \case
    LeftS _ -> GT
    RightS r1 -> g r0 r1

-- | Turn an 'EitherS' value into a plain 'Either' value.
toEither :: EitherS l r -> Either l r
toEither = \case
  LeftS l -> Left l
  RightS r -> Right r

unLeftS :: EitherS l r -> Maybe l
unLeftS = \case
  LeftS l -> Just l
  RightS _ -> Nothing

unRightS :: EitherS l r -> Maybe r
unRightS = \case
  LeftS _ -> Nothing
  RightS r -> Just r

eitherS :: (l -> x) -> (r -> x) -> EitherS l r -> x
eitherS f g = \case
  LeftS l -> f l
  RightS r -> g r

leftsS :: [EitherS l r] -> [l]
leftsS es = [l | LeftS l <- es]

rightsS :: [EitherS l r] -> [r]
rightsS es = [r | RightS r <- es]

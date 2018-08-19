{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}  -- Just for RestrictedJoin => RestrictedMonad.

module VectorMonad
  where

import Prelude hiding (zip)
import Data.List (foldl')

-- $setup
-- λ import Control.Applicative
-- λ import Data.Monoid
-- λ :set -XFlexibleContexts
-- λ :set -XAllowAmbiguousTypes
-- λ :set -XGADTs
-- λ :set -XTypeApplications


-- Preliminary definitions.
-- ========================


-- Type level Peano naturals.
-- --------------------------
--
-- _(Inductive definition.)_

data N
  where
    Z :: N
    S :: N -> N


-- A class for `join`.
-- -------------------

class RestrictedJoin m a
  where
    join :: m (m a) -> m a

class RestrictedMonad m a
  where
    (>>~) :: m a -> (a -> m a) -> m a

instance ( RestrictedJoin m a
         , Functor m
         ) => RestrictedMonad m a
  where
    x >>~ f = join . fmap f $ x


-- Vector.
-- =======


-- Trivia.
-- -------

data Vector (n :: N) a
  where
    VZ :: Vector Z a
    (:::) :: a -> Vector n a -> Vector (S n) a

infixr 1 :::

deriving instance Show a => Show (Vector n a)


-- Construction.
-- -------------

unit :: a -> Vector (S Z) a
unit x = x ::: VZ

type family Result head tail
  where
    Result a a = Vector (S (S Z)) a
    Result a (Vector (S n) a) = Vector (S (S n)) a

class ConstructVector head tail
  where
    (+:) :: head -> tail -> Result head tail
    infixr 1 +:

instance ConstructVector a a
  where
    x +: y = x ::: unit y

instance Result a (Vector (S n) a) ~ Vector (S (S n)) a => ConstructVector a (Vector (S n) a)
  where
    x +: xs = x ::: xs

-- ^
-- λ (1 +: 2 +: 3)
-- 1 ::: (2 ::: (3 ::: VZ))


-- Zip Vector.
-- -----------

class Zip z
  where
    zip :: z a -> z b -> z (a, b)

instance Zip (Vector n) => Zip (Vector (S n))
  where
    zip (x ::: xs) (y ::: ys) = (x, y) ::: zip xs ys

instance Zip (Vector Z)
  where
    zip _ _ = VZ

-- ^
-- λ :t zip (1 +: 2 +: 3) (4 +: 5 +: 6)
-- zip (1 +: 2 +: 3) (4 +: 5 +: 6)
--   :: (Zip z, ConstructVector head1 (Result head2 tail1),
--       ConstructVector head2 tail1,
--       ConstructVector head3 (Result head4 tail2),
--       ConstructVector head4 tail2, Num head1, Num head2, Num tail1,
--       Num head3, Num head4, Num tail2,
--       Result head1 (Result head2 tail1) ~ z a,
--       Result head3 (Result head4 tail2) ~ z b) =>
--      z (a, b)
-- λ zip (1 +: 2 +: 3) (4 +: 5 +: 6)
-- (1,4) ::: ((2,5) ::: ((3,6) ::: VZ))


-- Functor Vector.
-- ---------------

instance Functor (Vector Z)
  where
    fmap _ VZ = VZ

instance Functor (Vector n) => Functor (Vector (S n))
  where
    fmap f (x ::: xs) = f x ::: fmap f xs

-- ^
-- λ fmap (uncurry (+)) $ zip (1 +: 2 +: 3) (4 +: 5 +: 6)
-- 5 ::: (7 ::: (9 ::: VZ))


-- Applicative Vector.
-- -------------------

instance Applicative (Vector Z)
  where
    pure a = VZ
    _ <*> VZ = VZ

instance ( Applicative (Vector n)
         , Zip (Vector n)
         ) => Applicative (Vector (S n))
  where
    pure x = x ::: pure x
    fs <*> xs = fmap (uncurry ($)) $ zip fs xs

-- ^
-- λ liftA2 (+) (1 +: 2 +: 3) (4 +: 5 +: 6)
-- 5 ::: (7 ::: (9 ::: VZ))


-- Foldable Vector.
-- ----------------

instance Foldable (Vector n) => Foldable (Vector (S n))
  where
    foldr f z (x ::: xs) = foldr f (f x z) xs

instance Foldable (Vector Z)
  where
    foldr _ z VZ = z


-- Monad Vector,
-- -------------

instance ( Monoid a
         , Applicative (Vector n)
         , Zip (Vector n)
         , Foldable (Vector n)
         ) => RestrictedJoin (Vector n) a
  where
    join = foldl' ((fmap (uncurry mappend) .) . zip) (pure mempty)

-- ^
-- λ :{
-- let f = fmap getSum . join . pure . fmap Sum
-- in  f . f . f $ (1 +: 2 +: (3 :: Integer))
-- :}
-- 27 ::: (54 ::: (81 ::: VZ))


-- Matrix.
-- =======


-- Trivia.
-- -------

data Matrix (m :: N) (n :: N) a
  where
    MZ :: Matrix Z n a
    (:-:) :: Matrix m n a -> Vector n a -> Matrix (S m) n a

deriving instance Show a => Show (Matrix m n a)

-- ^
-- λ MZ @_ @Integer :-: (1 +: 2 +: 3) :-: (4 +: 5 +: 6)
-- (:-:) ((:-:) MZ (1 ::: (2 ::: (3 ::: VZ)))) (4 ::: (5 ::: (6 ::: VZ)))


-- Zip Matrix.
-- -----------

instance ( Zip (Vector n)
         , Zip (Matrix m n)
         ) => Zip (Matrix (S m) n)
  where
    zip (m :-: v) (m' :-: v') = zip m m' :-: zip v v'

instance Zip (Matrix Z n)
  where
    zip MZ MZ = MZ

-- ^
-- λ :{
-- let m  = MZ @_ @Integer :-: (1 +: 2 +: 3) :-: (4  +: 5  +: 6 )
--     m' = MZ @_ @Integer :-: (7 +: 8 +: 9) :-: (10 +: 11 +: 12)
-- in  m `zip` m'
-- :}
-- (:-:) ((:-:) MZ ((1,7) ::: ((2,8) ::: ((3,9) ::: VZ)))) ((4,10) ::: ((5,11) ::: ((6,12) ::: VZ)))


-- Functor Matrix.
-- ---------------

instance ( Functor (Matrix m n)
         , Functor (Vector n)
         ) => Functor (Matrix (S m) n)
  where
    fmap f (m :-: v) = fmap f m :-: fmap f v

instance Functor (Matrix Z n)
  where
    fmap _ MZ = MZ

-- ^
-- λ fmap (*7) $ MZ @_ @Integer :-: (1 +: 2 +: 3) :-: (4 +: 5 +: 6)
-- (:-:) ((:-:) MZ (7 ::: (14 ::: (21 ::: VZ)))) (28 ::: (35 ::: (42 ::: VZ)))


-- Applicative Matrix.
-- -------------------

instance ( Applicative (Vector n)
         , Applicative (Matrix m n)
         , Zip (Vector n)
         , Zip (Matrix m n)
         ) => Applicative (Matrix (S m) n)
  where
    pure x = pure x :-: pure x
    mf <*> mx = fmap (uncurry ($)) $ zip mf mx

instance Applicative (Matrix Z n)
  where
    pure x = MZ
    MZ <*> MZ = MZ

-- ^
-- λ :{
-- let m  = MZ @_ @Integer :-: (1 +: 2 +: 3) :-: (4  +: 5  +: 6 )
--     m' = MZ @_ @Integer :-: (7 +: 8 +: 9) :-: (10 +: 11 +: 12)
-- in  pure (*) <*> m <*> m'
-- :}
-- (:-:) ((:-:) MZ (7 ::: (16 ::: (27 ::: VZ)))) (40 ::: (55 ::: (72 ::: VZ)))


-- Foldable Matrix.
-- ----------------

instance ( Foldable (Vector n)
         , Foldable (Matrix m n)
         ) => Foldable (Matrix (S m) n)
  where
    foldr f z (v :-: m) = foldr f z $ elems v ++ elems m
      where
        elems :: Foldable f => f a -> [a]
        elems = foldr (:) []


-- Monad Matrix.
-- -------------

instance ( Applicative (Matrix m n)
         , Zip (Matrix m n)
         , Foldable (Matrix m n)
         , Monoid a
         ) => RestrictedJoin (Matrix m n) a
  where
    join = foldr ((fmap (uncurry mappend) .) . zip) (pure mempty)

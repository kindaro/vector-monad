{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module VectorMonad
  where

import Prelude hiding (zip)

data N
  where
    Z :: N
    S :: N -> N

data Vector (n :: N) a
  where
    VZ :: Vector Z a
    (:::) :: a -> Vector n a -> Vector (S n) a

infixr 1 :::

deriving instance Show a => Show (Vector n a)

class Zip z
  where
    zip :: z a -> z b -> z (a, b)

instance Zip (Vector n) => Zip (Vector (S n))
  where
    zip (x ::: xs) (y ::: ys) = (x, y) ::: zip xs ys

instance Zip (Vector Z)
  where
    zip _ _ = VZ

-- λ :t zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
--   :: (Num a, Num b) => Vector ('S ('S ('S 'Z))) (a, b)
-- λ zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- (1,4) ::: ((2,5) ::: ((3,6) ::: VZ))

instance Functor (Vector Z)
  where
    fmap _ VZ = VZ

instance Functor (Vector n) => Functor (Vector (S n))
  where
    fmap f (x ::: xs) = f x ::: fmap f xs

-- λ fmap (uncurry (+)) $ zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- 5 ::: (7 ::: (9 ::: VZ))

data Matrix (m :: N) (n :: N) a
  where
    MZ :: Matrix Z Z a
    (:-:) :: Matrix m n a -> Vector n a -> Matrix (S m) n a
    Row :: Vector n a -> Matrix (S Z) n a

deriving instance Show a => Show (Matrix m n a)

-- λ Row (1 ::: 2 ::: 3 ::: VZ) :-: (4 ::: 5 ::: 6 ::: VZ)
-- (:-:) (Row (1 ::: (2 ::: (3 ::: VZ)))) (4 ::: (5 ::: (6 ::: VZ)))

instance {-# OVERLAPS #-}
         ( Functor (Matrix m n)
         , Functor (Vector n)
         ) => Functor (Matrix (S m) n)
  where
    fmap f (m :-: v) = fmap f m :-: fmap f v

instance ( Functor (Matrix Z n)
         , Functor (Vector n)
         ) => Functor (Matrix (S Z) n)
  where
    fmap f (Row v) = Row (fmap f v)

instance Functor (Matrix Z n)
  where
    fmap _ MZ = MZ

-- λ fmap (*7) $ Row (1 ::: 2 ::: 3 ::: VZ) :-: (4 ::: 5 ::: 6 ::: VZ)
-- (:-:) (Row (7 ::: (14 ::: (21 ::: VZ)))) (28 ::: (35 ::: (42 ::: VZ)))

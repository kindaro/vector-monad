{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Vector
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

-- ^
-- λ :t zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
--   :: (Num a, Num b) => Vector ('S ('S ('S 'Z))) (a, b)
-- λ zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- (1,4) ::: ((2,5) ::: ((3,6) ::: VZ))

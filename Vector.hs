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
import GHC.TypeLits

data Vector (n :: Nat) a
  where
    VZ :: Vector 0 a
    (:::) :: a -> Vector n a -> Vector (n + 1) a

infixr 1 :::

deriving instance Show a => Show (Vector n a)

class Zip z
  where
    zip :: z a -> z b -> z (a, b)

instance Zip (Vector n) => Zip (Vector (n + 1))
  where
    zip (x ::: xs) (y ::: ys) = (x, y) ::: zip xs ys

instance Zip (Vector 0)
  where
    zip _ _ = VZ

-- ^
-- λ :t zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
--   :: (Num a, Num b) => Vector 3 (a, b)
-- λ zip (1 ::: 2 ::: 3 ::: VZ) (4 ::: 5 ::: 6 ::: VZ)
-- (1,4) ::: ((2,5) ::: ((3,6) ::: VZ))

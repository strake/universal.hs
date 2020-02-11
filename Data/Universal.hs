{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Universal where

import Prelude hiding (Functor (..))
import Control.Categorical.Functor
import Control.Category.Const2
import Data.Functor.Const

-- | Laws:
--
-- * @f = 'map' ('universal' f) . 'getConst' 'morphism'@
class Functor s t f => Universal (s :: α -> α -> *) (t :: β -> β -> *) (x :: β) (f :: α -> β) where
    type Element s t x f :: α
    morphism :: Const (t x (f (Element s t x f))) s
    universal :: t x (f a) -> s (Element s t x f) a

-- Terminal
instance Universal (Const2 () :: () -> () -> *) (->) () (Const ()) where
    type Element (Const2 ()) (->) () (Const ()) = '()
    morphism = Const Const
    universal = \ _ -> Const2 ()

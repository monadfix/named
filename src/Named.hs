{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, FlexibleContexts,
             FunctionalDependencies, TypeFamilies, TypeOperators,
             PatternSynonyms, UndecidableInstances, ConstraintKinds #-}

{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}

module Named
  ( Named(.., Flag),
    Flag,
    Apply,
    (!)
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.OverloadedLabels (IsLabel(..))

newtype Named a (name :: Symbol) = Named { unnamed :: a }

instance (name ~ name', a ~ a') => IsLabel name (a -> Named a' name') where
  fromLabel = Named

type Flag = Named Bool

pattern Flag :: Bool -> Flag name
pattern Flag a = Named a

{-# COMPLETE Flag #-}

(!) :: Apply name a fn fn' => fn -> Named a name -> fn'
(!) = apply

infixl 0 !

--------------------------------------------------------------------------------
--  Do not read further to avoid emotional trauma.
--------------------------------------------------------------------------------

data FAD = FAD_Fail | FAD_Done | FAD_Skip FAD

type family FApplyDecisions (name :: Symbol) (fn :: Type) :: FAD where
  FApplyDecisions name (Named a name -> r) = FAD_Done
  FApplyDecisions name (x -> r) = FAD_Skip (FApplyDecisions name r)
  FApplyDecisions name t = FAD_Fail

class
  ( decisions ~ FApplyDecisions name fn
  ) => Apply' decisions name a fn fn' | name fn -> fn'
  where
    apply :: fn -> Named a name -> fn'

instance
  ( fn ~ (Named a name -> r),
    fn' ~ r,
    FApplyDecisions name fn ~ FAD_Done
  ) => Apply' FAD_Done name a fn fn'
  where
    apply = id

instance
  ( Apply' decisions name a r r',
    FApplyDecisions name fn ~ FAD_Skip decisions,
    fn ~ (x -> r),
    fn' ~ (x -> r')
  ) => Apply' (FAD_Skip decisions) name a fn fn'
  where
    apply fn named = \x -> fn x ! named

instance
  ( TypeError (Text "Named parameter '" :<>: Text name :<>:
               Text "' was supplied, but not expected"),
    FApplyDecisions name fn ~ FAD_Fail,
    fn ~ fn'
  ) => Apply' FAD_Fail name a fn fn'
  where
    apply = const

type ApplyC (name :: Symbol) (a :: Type) (fn :: Type) (fn' :: Type) =
  Apply' (FApplyDecisions name fn) name a fn fn'

class ApplyC name a fn fn' => Apply name a fn fn' | name fn -> fn'
instance ApplyC name a fn fn' => Apply name a fn fn'

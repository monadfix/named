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

import Data.Void (Void)
import Data.Kind (Type)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.OverloadedLabels (IsLabel(..))

newtype Named a (name :: Symbol) = Named { unnamed :: a }

instance (name ~ name', a ~ a') => IsLabel name (a -> Named a' name') where
  fromLabel = Named

type Flag = Named Bool

pattern Flag :: Bool -> Flag name
pattern Flag a = Named a

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

type family FApply (name :: Symbol) (decisions :: FAD) (fn :: Type) :: Type where
  FApply name FAD_Done (x -> r) = r
  FApply name FAD_Fail t = Void
  FApply name (FAD_Skip fad) (x -> r) = x -> FApply name fad r

class decisions ~ FApplyDecisions name fn => Apply' name a fn decisions where
  apply :: fn -> Named a name -> FApply name decisions fn

instance
  ( fn ~ (Named a name -> r),
    FApplyDecisions name fn ~ FAD_Done
  ) => Apply' name a fn FAD_Done
  where
    apply = id

instance
  ( Apply' name a r decisions,
    FApplyDecisions name fn ~ FAD_Skip decisions,
    fn ~ (x -> r)
  ) => Apply' name a fn (FAD_Skip decisions)
  where
    apply fn named = \x -> fn x ! named

instance
  ( TypeError (Text "Named parameter '" :<>: Text name :<>:
               Text "' was supplied, but not expected"),
    FApplyDecisions name fn ~ FAD_Fail
  ) => Apply' name a fn FAD_Fail
  where
    apply = error "FAD_Fail"

type ApplyC (name :: Symbol) (a :: Type) (fn :: Type) (fn' :: Type) =
  ( Apply' name a fn (FApplyDecisions name fn),
    fn' ~ FApply name (FApplyDecisions name fn) fn
  )

class ApplyC name a fn fn' => Apply name a fn fn' | name fn -> fn'
instance ApplyC name a fn fn' => Apply name a fn fn'

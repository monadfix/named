{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, FlexibleContexts,
             FunctionalDependencies, TypeFamilies, TypeOperators,
             PatternSynonyms, UndecidableInstances, ConstraintKinds,
             TypeApplications, ScopedTypeVariables, CPP #-}

module Named.Internal where

import Prelude (Bool, id)
import Data.Kind (Type)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.OverloadedLabels (IsLabel(..))

{- |

Assign a name to a value of type @a@. This is a simple wrapper intended
for use with @-XOverloadedLabels@:

@
#verbose True :: Named Bool "verbose"
@

-}
newtype Named a (name :: Symbol) = Named { unnamed :: a }

instance (name ~ name', a ~ a') => IsLabel name (a -> Named a' name') where
#if MIN_VERSION_base(4,10,0)
  fromLabel = Named
#else
  fromLabel _ = Named
#endif
  {-# INLINE fromLabel #-}

-- | Snake oil to cure boolean blindness.
type Flag = Named Bool

-- | Match on a flag, a version of 'Named' specialized to 'Bool'.
pattern Flag :: Bool -> Flag name
pattern Flag a = Named a

#if MIN_VERSION_base(4,10,0)
{-# COMPLETE Flag #-}
#endif

{- | Supply a keyword argument to a function:

@
function ! #param_name value
@
-}

(!) :: Apply name a fn fn' => fn -> Named a name -> fn'
(!) = apply
{-# INLINE (!) #-}

infixl 9 !

{- |

A proxy for a name, intended for use with @-XOverloadedLabels@:

@
#verbose :: Name "verbose"
@

-}
data Name (name :: Symbol) = Name

instance name ~ name' => IsLabel name' (Name name) where
#if MIN_VERSION_base(4,10,0)
  fromLabel = Name
#else
  fromLabel _ = Name
#endif
  {-# INLINE fromLabel #-}

{- | Supply a keyword argument to a function:

@
with #param_name value function
@
-}
with :: Apply name a fn fn' => Name name -> a -> fn -> fn'
with name a fn = fn ! named name a
{-# INLINE with #-}

-- | Annotate a value with a name.
named :: Name name -> a -> Named a name
named _ = Named
{-# INLINE named #-}

{- |

'arg' unwraps a named parameter with the specified name. One way to use it is to
to match on arguments with @-XViewPatterns@:

@
fn (arg \#t -> t) (arg \#f -> f) = ...
@

This way, the names of parameters can be inferred from the patterns: no type
signature for @fn@ is required. In case a type signature for @fn@ is
provided, the parameters must come in the same order:

@
fn :: Integer ``Named`` "t" -> Integer ``Named`` "f" -> ...
fn (arg \#t -> t) (arg \#f -> f) = ... -- ok
fn (arg \#f -> f) (arg \#t -> t) = ... -- does not typecheck
@

-}
arg :: Name name -> Named a name -> a
arg _ = unnamed
{-# INLINE arg #-}

--------------------------------------------------------------------------------
--  Do not read further to avoid emotional trauma.
--------------------------------------------------------------------------------

data Decision = Done | Skip Decision

type family Decide (name :: Symbol) (fn :: Type) :: Decision where
  Decide name (Named a name -> r) = Done
  Decide name (x -> r) = Skip (Decide name r)
  Decide name t =
    TypeError (Text "Named parameter '" :<>: Text name :<>:
               Text "' was supplied, but not expected")

class
  ( decision ~ Decide name fn
  ) => Apply' decision name a fn fn' | name fn -> fn'
  where
    -- | Apply a function to a keyword argument.
    apply :: fn -> Named a name -> fn'

instance
  ( fn ~ (Named a name -> r),
    fn' ~ r,
    Decide name fn ~ Done
  ) => Apply' Done name a fn fn'
  where
    apply = id
    {-# INLINE apply #-}

instance
  ( Apply' decision name a r r',
    Decide name fn ~ Skip decision,
    fn ~ (x -> r),
    fn' ~ (x -> r')
  ) => Apply' (Skip decision) name a fn fn'
  where
    apply fn a = \x -> apply (fn x) a
    {-# INLINE apply #-}

{- |

Supply a parameter of type @a@ named @name@ to a function @fn@, resulting in
@fn'@.

For example:

@
Apply "x" Char
  (Named Bool "b" -> Named Char "x" -> r)
  (Named Bool "b" -> r)
@

In case the parameter cannot be supplied, this constraint will become a type
error.

-}
type Apply (name :: Symbol) (a :: Type) (fn :: Type) (fn' :: Type) =
  Apply' (Decide name fn) name a fn fn'

{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, FlexibleContexts,
             FunctionalDependencies, TypeFamilies, TypeOperators,
             PatternSynonyms, UndecidableInstances, ConstraintKinds #-}

{- |

Named parameters, also known as keyword arguments, have several advantages
over positional arguments:

* convenience: they can be supplied in arbitrary order
* readability: their names serve as documentation at call site
* safety: it is impossible to accidentally mix them up

Consider a function to replace a substring with another string:

@
Text.replace path "$HOME" "\/home\/username\/"
@

We want to replace references to the @$HOME@ environment variable with a
concrete directory. There is but one problem - we have supplied the text
arguments in the wrong order.

Compare that to a newtype-based solution:

@
Text.replace
  (Needle "$HOME")
  (Replacement "\/home\/username\/")
  (Haystack path)
@

Now that the function requires each argument to be wrapped in a newtype, we
cannot mix them up - the compiler will report an error, and newtype constructors
serve as documentation.

The problem with newtypes is that it is bothersome to create them for each
parameter, they pollute the global namespace, and we still cannot supply wrapped
arguments in arbitrary order.

With keyword arguments, none of that is a problem:

@
Text.replace ! #haystack path
             ! #needle "$HOME"
             ! #replacement "\/home\/username\/"
@

Functions must declare their parameter names in the type signature:

@
replace ::
  Text \`Named\` "needle" ->
  Text \`Named\` "replacement" ->
  Text \`Named\` "haystack"
replace (Named needle) (Named replacement) (Named haystack) =
  ...
@

-}
module Named
  (
    -- * Core interface
    Named(..),
    (!),

    -- * Specialized synonyms
    Flag,
    pattern Flag,

    -- * Internal
    Apply,
    apply
  ) where

import Prelude (Bool, id, const)
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
  fromLabel = Named
  {-# INLINE fromLabel #-}

-- | Snake oil to cure boolean blindness.
type Flag = Named Bool

-- | Match on a flag, a version of 'Named' specialized for 'Bool'.
pattern Flag :: Bool -> Flag name
pattern Flag a = Named a

{-# COMPLETE Flag #-}

-- | Supply a keyword argument to a function.
(!) :: Apply name a fn fn' => fn -> Named a name -> fn'
(!) = apply
{-# INLINE (!) #-}

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
    -- | Apply a function to a keyword argument.
    apply :: fn -> Named a name -> fn'

instance
  ( fn ~ (Named a name -> r),
    fn' ~ r,
    FApplyDecisions name fn ~ FAD_Done
  ) => Apply' FAD_Done name a fn fn'
  where
    apply = id
    {-# INLINE apply #-}

instance
  ( Apply' decisions name a r r',
    FApplyDecisions name fn ~ FAD_Skip decisions,
    fn ~ (x -> r),
    fn' ~ (x -> r')
  ) => Apply' (FAD_Skip decisions) name a fn fn'
  where
    apply fn named = \x -> apply (fn x) named
    {-# INLINE apply #-}

instance
  ( TypeError (Text "Named parameter '" :<>: Text name :<>:
               Text "' was supplied, but not expected"),
    FApplyDecisions name fn ~ FAD_Fail,
    fn ~ fn'
  ) => Apply' FAD_Fail name a fn fn'
  where
    apply = const
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
  Apply' (FApplyDecisions name fn) name a fn fn'

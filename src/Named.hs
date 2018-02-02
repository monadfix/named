{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, FlexibleContexts,
             FunctionalDependencies, TypeFamilies, TypeOperators,
             PatternSynonyms, UndecidableInstances, ConstraintKinds,
             TypeApplications, ScopedTypeVariables #-}

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
Text.replace '!' #haystack path
             '!' #needle "$HOME"
             '!' #replacement "\/home\/username\/"
@

Functions must declare their parameter names in the type signature:

@
replace ::
  Text \``Named`\` "needle" ->
  Text \``Named`\` "replacement" ->
  Text \``Named`\` "haystack" ->
  Text
replace (Named needle) (Named replacement) (Named haystack) =
  ...
@

Keyword arguments have seamless interoperability with positional arguments when
the function takes them last. Consider this function:

@
foo :: A -> B -> C \`Named\` "x" -> IO ()
@

There are several ways to invoke it:

@
(foo a b) ! #x c     -- parentheses for clarity
(foo a ! #x c) b     -- parentheses required
(foo ! #x c) a b     -- parentheses required
@

We can also supply keyword arguments using the 'with' combinator instead of
the '!' operator:

@
(with #x c foo) a b  -- parentheses for clarity
with #x c (foo a b)  -- has the same effect
@

Both '!' and 'with' work in a similar manner: they traverse the spine of
the function and supply the first keyword argument with a matching name.

For example:

@
bar           :: A \`Named\` "x" -> B \`Named\` "y" -> IO ()
bar ! #y b    :: A \`Named\` "x"                  -> IO ()
with #y b bar :: A \`Named\` "x"                  -> IO ()
@

-}
module Named
  (
    -- * Core interface
    Named(..),
    (!),
    Name(..),
    with,

    -- * Specialized synonyms
    Flag,
    pattern Flag,

    -- * Internal
    Apply,
    apply,
    named
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

-- | Match on a flag, a version of 'Named' specialized to 'Bool'.
pattern Flag :: Bool -> Flag name
pattern Flag a = Named a

{-# COMPLETE Flag #-}

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
  fromLabel = Name
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
    apply fn a = \x -> apply (fn x) a
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

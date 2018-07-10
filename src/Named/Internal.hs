{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, FlexibleContexts,
             FunctionalDependencies, TypeFamilies, TypeOperators,
             PatternSynonyms, UndecidableInstances, ConstraintKinds,
             TypeApplications, ScopedTypeVariables, CPP,
             AllowAmbiguousTypes #-}

module Named.Internal where

import Prelude (id, Maybe(..))
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.OverloadedLabels (IsLabel(..))

{- |

Assign a name to a value of type @a@ wrapped in @f@.

@
#verbose True :: NamedF Identity Bool "verbose"
@

-}
newtype NamedF f (a :: Type) (name :: Symbol) =
  ArgF (f a) -- ^ Match on an F-argument without specifying its name.
             -- See also: 'argF'.

-- | Match on an argument without specifying its name. See also: 'arg'.
pattern Arg :: a -> name :! a
pattern Arg a = ArgF (Identity a)

#if MIN_VERSION_base(4,10,0)
{-# COMPLETE Arg #-}
#endif

-- | Infix notation for the type of a named parameter.
type name :! a = NamedF Identity a name

-- | Infix notation for the type of an optional named parameter.
type name :? a = NamedF Maybe a name

class InjValue f where
  injValue :: a -> f a

instance InjValue Identity where
  injValue = Identity

instance InjValue Maybe where
  injValue = Just

instance (name ~ name', a ~ a', InjValue f) => IsLabel name (a -> NamedF f a' name') where
#if MIN_VERSION_base(4,10,0)
  fromLabel a = ArgF (injValue a)
#else
  fromLabel _ a = ArgF (injValue a)
#endif
  {-# INLINE fromLabel #-}

newtype Param p = Param p

instance (p ~ NamedF f a name, InjValue f) => IsLabel name (a -> Param p) where
#if MIN_VERSION_base(4,10,0)
  fromLabel a = Param (fromLabel @name a)
#else
  fromLabel pName a = Param (fromLabel pName a)
#endif
  {-# INLINE fromLabel #-}

{- | Supply a parameter to a function:

@
function '!' \#param_name value
@

@
function '!' \#x 7 '!' #y 42 '!' 'defaults'
@

This is an infix version of 'with'.

-}
(!) :: forall p fn fn'. WithParam p fn fn' => fn -> Param p -> fn'
fn ! p = with p fn
{-# INLINE (!) #-}

infixl 9 !

{- |
Supply a parameter @p@ to a function @fn@, resulting in @fn'@.

For example, when we pass a single named parameter, we get a function without
this parameter:

@
WithParam
                 ("x" :! Char)       -- p
  ("b" :! Bool -> "x" :! Char -> r)  -- fn
  ("b" :! Bool                -> r)  -- fn'
@

In case the parameter cannot be supplied, this constraint will become a type
error.

-}
class WithParam p fn fn' | p fn -> fn' where
  {- | Supply a parameter to a function:

  @
  'with' (\#param_name value) function
  @

  @
  'with' 'defaults' function
  @

  This is a prefix version of the ('!') operator.

  -}
  with :: Param p -> fn -> fn'

instance WithParam' (Decide p fn) p fn fn' => WithParam p fn fn' where
  with (Param p) fn = withParam @(Decide p fn) p fn
  {-# INLINE with #-}

data Defaults = Defaults

{- |
Passing 'defaults' to a function fills all unspecified optional parameters
with 'Nothing':

@
fn            :: "b" ':!' Bool -> "x" ':?' Char -> Int -> IO ()
fn '!' 'defaults' :: "b" ':!' Bool ->                Int -> IO ()
@

-}
defaults :: Param Defaults
defaults = Param Defaults

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

{- |

'arg' unwraps a named parameter with the specified name. One way to use it is
to match on arguments with @-XViewPatterns@:

@
fn (arg \#t -> t) (arg \#f -> f) = ...
@

This way, the names of parameters can be inferred from the patterns: no type
signature for @fn@ is required. In case a type signature for @fn@ is
provided, the parameters must come in the same order:

@
fn :: "t" :! Integer -> "f" :! Integer -> ...
fn (arg \#t -> t) (arg \#f -> f) = ... -- ok
fn (arg \#f -> f) (arg \#t -> t) = ... -- does not typecheck
@

-}
arg :: Name name -> name :! a -> a
arg _ (ArgF (Identity a)) = a
{-# INLINE arg #-}

{- |

'argF' is similar to 'arg': it unwraps a named parameter with the specified name.
The difference is that the result of 'argF' is inside an arity wrapper,
which is 'Identity' for normal parameters and 'Maybe' for optional parameters.

-}
argF :: Name name -> NamedF f a name -> f a
argF _ (ArgF fa) = fa
{-# INLINE argF #-}

{- |

A variation of 'arg' for optional arguments. Requires a default value to handle
the case when the optional argument was omitted:

@
fn (argDef \#answer 42 -> ans) = ...
@

In case you want to get a value wrapped in 'Maybe' instead, use 'argF' or
'ArgF'.

-}
argDef :: Name name -> a -> name :? a -> a
argDef _ d (ArgF fa) = fromMaybe d fa

--------------------------------------------------------------------------------
-- The working horses of the library: Decide and WithParam'
--------------------------------------------------------------------------------

data DApply
data DFill
data DPass

type family Decide (p :: Type) (fn :: Type) :: [Type] where
  Decide (NamedF f' a' name) (NamedF f a name -> r) = '[DApply]
  Decide Defaults (NamedF Maybe a name -> r) = DFill : Decide Defaults r
  Decide p (x -> r) = DPass : Decide p r
  Decide (NamedF f' a' name) t =
    TypeError (Text "Named parameter '" :<>: Text name :<>:
               Text "' was supplied, but not expected")
  Decide Defaults t = '[]

class WithParam' (ds :: [Type]) p fn fn' | ds p fn -> fn' where
  withParam :: p -> fn -> fn'

instance fn ~ fn' => WithParam' '[] p fn fn' where
  withParam _ = id
  {-# INLINE withParam #-}

instance
    ( WithParam' ds p r r',
      fn ~ (p -> r),
      fn' ~ r'
    ) => WithParam' (DApply : ds) p fn fn'
  where
    withParam p fn = withParam @ds p (fn p)
    {-# INLINE withParam #-}

instance
    ( WithParam' ds p r r',
      fn ~ (x -> r),
      fn' ~ (x -> r')
    ) => WithParam' (DPass : ds) p fn fn'
  where
    withParam a fn = \x -> withParam @ds a (fn x)
    {-# INLINE withParam #-}

instance
    ( WithParam' ds p r r',
      fn ~ (NamedF f x name -> r),
      fn' ~ r',
      f ~ Maybe
    ) => WithParam' (DFill : ds) p fn fn'
  where
    withParam p fn = withParam @ds p (fn (ArgF Nothing))
    {-# INLINE withParam #-}

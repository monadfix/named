{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Exports 'withs' which allows the call site to avoid using '!'.

Example ghci session:

> :set -XAllowAmbiguousTypes -XFlexibleContexts -XDataKinds -XOverloadedLabels -XRebindableSyntax
> import Named
> import Named.Varargs
> import Prelude -- needed by -XRebindableSyntax
> let f (arg #x -> x) (argDef #y [1,2,3] -> y) = x:y
> :t withs f #x 3 End
withs f #x 3 End :: Num x => [x]
> withs f #x 3 End
[3,1,2,3]

> withs f #x 3 #y [5] End
[3,5]
> withs f #y [] #x 3 End
[3]

> let g x = withs f x
> g #x 3 End
[3,1,2,3]

> g End ! param #x 4
[4,1,2,3]


-}
module Named.Varargs (WithParams(withs), pattern End, fromLabel) where

import Named
import Named.Internal
import Data.Functor.Identity (Identity)

fromLabel :: Name name
fromLabel = Name

class WithParams a where
  withs :: a

instance (WithParam p a b,
        p ~ NamedF f x name,
        Applicative f,
        v ~ (Name name -> x -> c),
        WithParams (b -> c)) =>
        WithParams (a -> v) where
  withs f n v = withs (with (paramF n (pure v) :: Param p) f)

pattern End = Param Defaults

instance (WithParam Defaults a b)
        => WithParams (a -> Param Defaults -> b) where
  withs x End = x ! defaults

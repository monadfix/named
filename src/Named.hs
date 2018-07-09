{-# LANGUAGE PatternSynonyms #-}

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
concrete directory. There is but one problem – we have supplied the text
arguments in the wrong order.

Compare that to a newtype-based solution:

@
Text.replace
  (Needle "$HOME")
  (Replacement "\/home\/username\/")
  (Haystack path)
@

Now that the function requires each argument to be wrapped in a newtype, we
cannot mix them up – the compiler will report an error, and newtype constructors
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
foo :: A -> B -> C \``Named`\` "x" -> IO ()
@

There are several ways to invoke it:

@
(foo a b) '!' #x c     -- parentheses for clarity
(foo a '!' #x c) b     -- parentheses required
(foo '!' #x c) a b     -- parentheses required
@

We can also supply keyword arguments using the 'with' combinator instead of
the '!' operator:

@
('with' #x c foo) a b  -- parentheses for clarity
'with' #x c (foo a b)  -- has the same effect
@

Both '!' and 'with' work in a similar manner: they traverse the spine of
the function and supply the first keyword argument with a matching name.

For example:

@
bar           :: A \``Named`\` "x" -> B \``Named`\` "y" -> IO ()
bar '!' #y b    :: A \``Named`\` "x"                  -> IO ()
'with' #y b bar :: A \``Named`\` "x"                  -> IO ()
@

-}
module Named
  (
    -- * Core interface
    Named(..),
    (!),
    Name(..),
    with,
    arg,

    -- * Specialized synonyms
    Flag,
    pattern Flag

  ) where

import Named.Internal

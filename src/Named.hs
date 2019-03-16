{-# LANGUAGE ExplicitNamespaces #-}

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
Text.replace '!' \#haystack path
             '!' \#needle "$HOME"
             '!' \#replacement "\/home\/username\/"
@

Functions can declare their parameter names in pattern bindings:

@
replace ('arg' \#needle -> n) ('arg' \#replacement -> r) ('arg' \#haystack -> h) =
  ...
@

Types are inferred, but it is possible to specify them. When the parameter
names are specified in the type signature, they can be omitted from the
pattern bindings:

@
replace ::
  "needle"      ':!' Text ->
  "replacement" ':!' Text ->
  "haystack"    ':!' Text ->
  Text
replace ('Arg' n) ('Arg' r) ('Arg' h) =
  ...
@

Keyword arguments have seamless interoperability with positional arguments when
the function takes them last. Consider this function:

@
foo :: A -> B -> "x" :! C -> IO ()
@

There are several ways to invoke it:

@
(foo a b) '!' \#x c     -- parentheses for clarity
(foo a '!' \#x c) b     -- parentheses required
(foo '!' \#x c) a b     -- parentheses required
@

We can also supply keyword arguments using the 'with' combinator instead of
the '!' operator:

@
('with' (\#x c) foo) a b  -- parentheses for clarity
'with' (\#x c) (foo a b)  -- has the same effect
@

Both '!' and 'with' work in a similar manner: they traverse the spine of
the function and supply the first keyword argument with a matching name.

For example:

@
bar             :: "x" :! A -> "y" :! B -> IO ()
bar '!' \#y b      :: "x" :! A             -> IO ()
'with' (\#y b) bar :: "x" :! A             -> IO ()
@

There is also support for optional parameters. A function can specify default
values for some of its arguments:

@
log ::
  "message"  ':!' Text ->
  "severity" ':?' Severity ->
  "handle"   ':?' Handle ->
  IO ()
log ('arg'    #message          -> msg)
    ('argDef' #severity Error   -> sev)
    ('argDef' #handle   stderr  -> hndl)
  = ...
@

Optional parameters are denoted with (':?') instead of (':!'). Instead of 'arg'
to match on them, we must use either 'argDef' to provide a default value or
'argF' to get a value wrapped in 'Maybe' ('Just' when the parameter was
specified, 'Nothing' when omitted).

At call site, optional parameters are passed using the same ('!') operator:

@
log '!' #message "All your base are belong to us"
    '!' #severity Info
    '!' #handle stdout
@

To use the default values for all unspecified optional parameters, we can pass
'defaults' to the function:

@
log '!' #message "Could not match type 'Int' with type 'Bool'"
    '!' 'defaults'
@

@
log '!' #message "The password must contain a letter, \\
               \\a digit, and a plot twist"
    '!' #severity Warning
    '!' 'defaults'
@

We can also pass 'defaults' using 'with', which has the same effect as the ('!')
operator:

@
'with' 'defaults' $
  log '!' #message "Connection interrupted"
      '!' #handle logfile
@

-}
module Named
  (
    -- * Call site
    (!),
    WithParam(..),
    defaults,

    -- * Definition site
    type (:!),
    type (:?),
    NamedF(Arg, ArgF),
    Name(..),
    arg,
    argDef,
    argF,
  ) where

import Named.Internal

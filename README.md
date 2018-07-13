# `named` – keyword arguments for Haskell

`named` is a lightweight library for named function parameters (keyword
arguments) based on overloaded labels. Keyword arguments have several
advantages over positional arguments:

* they can be supplied in arbitrary order
* their names serve as documentation at call site
* it is impossible to accidentally mix them up

Unlike newtype wrappers, keyword arguments don't pollute the global
namespace, don't require top-level definitions, and don't need to be
exported.

This implementation of named parameters is typesafe, provides good type
inference, descriptive type errors, and has no runtime overhead.

Example usage:

```haskell
import Named

createSymLink :: "from" :! FilePath -> "to" :! FilePath -> IO ()
createSymLink (Arg from) (Arg to) = ...

main = createSymLink ! #from "/path/to/source"
                     ! #to "/target/path"
```

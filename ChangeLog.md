* Bumped upper bounds for GHC 9.2 and older.

## 0.3.0.1

* Bumped upper bounds for GHC 8.8.

## 0.3.0.0

* Added 'param', 'paramF'.
* Export 'NamedF(Arg, ArgF)' as a bundle.

## 0.2.0.0

* Removed 'Flag', 'named', 'Apply', 'apply'.
* Changed notation: 'Named' is now '(:!)' in types, 'Arg' in patterns.
* Added 'arg', 'argF'.
* Support for optional parameters: see 'argDef', 'defaults', '(:?)'.
* 'with #param value' is now 'with (#param value)' to allow 'with defaults'.
* Internals are now exposed from "Named.Internal".

## 0.1.0.0

* First version. Released on an unsuspecting world.

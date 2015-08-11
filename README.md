sets
====

> TODO: Insert & Delete still need to get done :x

This package provides overloaded terms, commonly used with set-like types,
like `Map` and `Set` from `containers`. There are also unorthodox, mostly useless
data types defined here - playing with ordering, uniqueness and finiteness.

## Usage

The following actions are overloaded:

- `union`
- `intersection`
- `difference`
- `complement`
- `size`
- `singleton`
- `empty`
- `total`
- `insert`
- `delete`
- `isSubsetOf` `isProperSubsetOf`

Each with their own typeclass. Also, the following data types are defined:

- Ordered Sets
    - Unique
        - Finite
    - Multiple
- Unordered Sets
    - Unique
    - Multiple

### Proper Usage

To use the overloaded terms, they need to be the only ones in scope. To make this
correct, we still need to import our container particularly:

```haskell
import qualified Data.Set as Set
import Data.Map (Map) -- just to use the type name

foo :: Map
foo = x `union` y

bar :: Set.Set
bar = a `union` b
```

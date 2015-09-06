sets
====

This package provides overloaded terms, commonly used with set-like types,
like `Map` and `Set` from `containers`. There are also unorthodox, mostly useless
data types defined here - playing with ordering, uniqueness and finiteness.

## Usage

The following actions are overloaded:

__Binary Operators__:

- `union`
- `intersection`
- `difference`
- `complement`

__Top and Bottom Elements__:

- `empty`
- `total`

__Element-Wise Operations__:

- `singleton`
- `insert`
- `delete`

__Metrics and Comparison__:

- `size`
- `isSubsetOf` `isProperSubsetOf`

---

Each of the operations has their own typeclass. We have also made newtype wrappers
for lists, for different restrictions:

- Ordered Sets
    - Multiple
- Unordered Sets
    - Unique
    - Multiple

This way, we can expect the following to work:

```haskell
uniqueAppend :: Eq a => [a] -> [a] -> [a]
uniqueAppend xs ys = unUUSet $ fromFoldable xs `union` fromFoldable ys

orderedAppend :: Ord a => [a] -> [a] -> [a]
orderedAppend xs ys = unOMSet $ fromFoldable xs `union` fromFoldable ys
```

---

We've also made a few newtypes to encode our binary operations, for `Monoid`
and `Semigroup` instances:

```haskell
instance (HasUnion s, HasEmpty s) => Monoid (Union s) where
  mempty = empty
  mappend = union
```

### Multiple Set Types

To use the overloaded terms, they need to be the only ones in scope. To make this
correct, we need to import our container with caution:

```haskell
import qualified Data.Set as Set
import Data.Map (Map) -- only the type name to designate behavior

foo :: Map
foo = x `union` y

bar :: Set.Set
bar = a `union` b
```

This way, we can keep our code more readable while still having set-like intuition.

## Testing and Benchmarking

> You can view the results [here](https://github.com/athanclark/sets/raw/master/profile.html)
> (__warning__: it's a 7MB text file - your browser will hate you)

The tests are built with QuickCheck - it's pretty easy to get them working for you:

```bash
cabal install --enable-tests
cabal test --show-details=always
```

(...or for the stack folk...)

```bash
stack build
stack test
```

---

To benchmark (it usually takes about 10 minutes on my laptop), run the command!

```bash
cabal install --enable-benchmarks
cabal bench
```

(...stiggitty-stack is co-wiggity-whack...)

```bash
stack build
stack bench --benchmark-arguments="--output profile.html"
```

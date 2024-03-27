# Changelog for either-semigroup

## Unreleased

- add `Alternative` instance that combines `LeftS` according to the `Monoid` structure
- `applyS` as a variant of `(<*>)` that combines `LeftS` values
- `altS` as a variant of `(<|>)` that does *not* combine `LeftS` values (mimicking `Alternative Either` behaviour)

## 0.0.0 - 2024-03-27

Initial release

# Example pedons for the CPCS 1967 classes

Small, plausible profiles written for the tests and examples, one per
class of the French soil classification (CPCS 1967), with designations
in the old French notation and the properties the key reads. They are
not real profiles.

## Usage

``` r
make_cpcs1967_example(
  class = c("raw_mineral", "weakly_developed", "vertisol", "andosol", "calcimagnesic",
    "isohumic", "brunified", "podzolised", "sesquioxide", "ferrallitic", "hydromorphic",
    "sodic")
)
```

## Arguments

- class:

  One of `"raw_mineral"`, `"weakly_developed"`, `"vertisol"`,
  `"andosol"`, `"calcimagnesic"`, `"isohumic"`, `"brunified"`,
  `"podzolised"`, `"sesquioxide"`, `"ferrallitic"`, `"hydromorphic"`,
  `"sodic"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_cpcs1967(make_cpcs1967_example("podzolised"), on_missing = "silent")$name
#> [1] "Podzols humo-ferrugineux"
```

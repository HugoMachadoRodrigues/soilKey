# Is a `qual_*` function a genuine implementation (not an unconditional `passed = NA` stub)? A real qualifier either delegates to `.q_presence()` or assigns `passed` from a computation.

Is a `qual_*` function a genuine implementation (not an unconditional
`passed = NA` stub)? A real qualifier either delegates to
`.q_presence()` or assigns `passed` from a computation.

## Usage

``` r
.qualifier_is_implemented(name)
```

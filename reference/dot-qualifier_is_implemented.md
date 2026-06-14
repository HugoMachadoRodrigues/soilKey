# Is a `qual_*` function a genuine implementation (not an unconditional `passed = NA` stub)? A real qualifier either calls `.q_presence()`, assigns `passed` from a computation, or **delegates** to a helper that does – e.g. `qual_fibric <- function(pedon) .qual_decomp(pedon, "fibric", "Fibric")`. The earlier detector inspected only the one-line body and so false-flagged such delegations as stubs; this follows one level of delegation (any helper called with `pedon`) before deciding.

Is a `qual_*` function a genuine implementation (not an unconditional
`passed = NA` stub)? A real qualifier either calls `.q_presence()`,
assigns `passed` from a computation, or **delegates** to a helper that
does – e.g.
`qual_fibric <- function(pedon) .qual_decomp(pedon, "fibric", "Fibric")`.
The earlier detector inspected only the one-line body and so
false-flagged such delegations as stubs; this follows one level of
delegation (any helper called with `pedon`) before deciding.

## Usage

``` r
.qualifier_is_implemented(name)
```

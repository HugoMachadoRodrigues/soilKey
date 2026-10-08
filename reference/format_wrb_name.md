# Format a WRB 2022 soil name with qualifiers

Format a WRB 2022 soil name with qualifiers

## Usage

``` r
format_wrb_name(
  rsg_name,
  principal = character(0),
  supplementary = character(0)
)
```

## Arguments

- rsg_name:

  Full RSG name (e.g. "Ferralsols").

- principal:

  Character vector of principal-qualifier names, in the order they are
  written (as
  [`resolve_wrb_qualifiers`](https://hugomachadorodrigues.github.io/soilKey/reference/resolve_wrb_qualifiers.md)
  returns them).

- supplementary:

  Character vector of supplementary-qualifier names, in the order they
  are written.

## Value

The name as WRB 2022 Chapter 2.2 writes it, e.g. "Geric Rhodic Ferralsol
(Clayic, Eutric, Ferric, Humic)".

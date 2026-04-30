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

  Character vector of principal-qualifier names.

- supplementary:

  Character vector of supplementary-qualifier names (default empty in
  v0.9).

## Value

Formatted string per Ch 6 p 154 ("Rhodic Ferralsol (Clayic, Humic,
Dystric)").

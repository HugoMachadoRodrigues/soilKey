# Test the spodic Al/Fe oxalate criterion: (al_ox + 0.5\*fe_ox) \>= threshold

Default 0.5% (WRB 2022 Chapter 3, Spodic horizon). Used by
[`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md).

## Usage

``` r
test_spodic_aluminum_iron(h, min_pct = 0.5, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).

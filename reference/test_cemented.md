# Test that a layer is at least moderately cemented

Used by petric variants (petrocalcic / petroduric / petrogypsic /
petroplinthic). The WRB 2022 ladder is: weakly \< moderately \< strongly
\< indurated. Default threshold is "moderately".

## Usage

``` r
test_cemented(h, min_class = "moderately", candidate_layers = NULL)
```

## Arguments

- h:

  Horizons table.

- min_class:

  One of "weakly", "moderately", "strongly", "indurated".

- candidate_layers:

  Optional restriction.

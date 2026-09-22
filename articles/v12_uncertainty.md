# Provenance-weighted classification uncertainty

A deterministic key gives one answer. But the inputs to that key carry
error – analytical error on a lab value, estimation error on a
VLM-extracted colour, regional-average error on a SoilGrids prior.
[`classify_with_uncertainty()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_uncertainty.md)
propagates that error into a probability distribution over classes, and
– crucially – it scales the error to how each value was actually
obtained.

## The idea

[`classification_robustness()`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md)
(since v0.9.42) perturbs every horizon cell by the same fixed fraction
and reports one number: the share of Monte-Carlo runs that kept the
baseline class. That treats a lab-measured clay value and a
photo-guessed one as equally trustworthy.

[`classify_with_uncertainty()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_uncertainty.md)
does two things differently:

1.  It returns the **whole posterior** – `P(class = x)` for every class
    the profile reached – not just a single robustness percentage.

2.  It **weights the perturbation by the provenance ledger**. Each
    `(horizon, attribute)` cell is perturbed by an amount set by its
    evidence grade:

    | Grade | Source            | Multiplicative half-width |
    |-------|-------------------|---------------------------|
    | A     | measured          | 3 %                       |
    | B     | spectra-predicted | 7 %                       |
    | C     | prior-inferred    | 10 %                      |
    | D     | VLM-extracted     | 17 %                      |
    | E     | user-assumed      | 30 %                      |

    (pH and Munsell columns get additive half-widths instead; see
    [`get_perturbation_scale()`](https://hugomachadorodrigues.github.io/soilKey/reference/get_perturbation_scale.md).)

So a classification that rests on assumptions is reported as genuinely
uncertain, while one resting on measurements is reported as firm – even
when both sit the same distance from a key boundary.

## A first run

``` r

library(soilKey)

p <- make_ferralsol_canonical()
u <- classify_with_uncertainty(p, n = 200, system = "wrb2022")
u
#> <soilkey_uncertainty>  system=wrb2022  level=rsg
#>   baseline    : Ferralsols
#>   MC runs     : 200 (200 successful)
#>   entropy     : 0.000
#>   posterior   :
#>     Ferralsols                       100.0%
#>   most decisive attribute: ...
```

The fields of the returned `soilkey_uncertainty` object:

``` r

u$posterior     # named numeric vector, P(class), sums to 1
u$top1          # the modal class
u$entropy       # Shannon entropy -- 0 for a certain result
u$sensitivity   # data.table(attribute, importance)
```

## Provenance changes the answer

The same profile, classified twice – once with its clay values measured,
once with them merely assumed:

``` r

# Mark every clay value as a bare assumption (grade E).
p_assumed <- make_ferralsol_canonical()
for (i in seq_len(nrow(p_assumed$horizons))) {
  p_assumed$add_measurement(i, "clay_pct",
                            p_assumed$horizons$clay_pct[i],
                            source = "user_assumed", confidence = 0.2,
                            overwrite = TRUE)
}

classify_with_uncertainty(p,         n = 200)$entropy   # low  -- measured
classify_with_uncertainty(p_assumed, n = 200)$entropy   # high -- assumed
```

Nothing about the soil changed; only what we *know* about it did. That
is exactly what a pedometric uncertainty estimate should capture.

## The sensitivity ranking

[`classify_with_uncertainty()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_uncertainty.md)
also runs a leave-one-attribute-out pass: each attribute is held fixed
in turn, and the drop in flip rate measures how much that attribute
drives the instability.

``` r

u$sensitivity
#>      attribute importance
#> 1     clay_pct      0.18
#> 2     cec_cmol      0.06
#> ...
```

The top row is the measurement that would most sharpen the
classification – a direct, defensible answer to “what should I measure
next?”.

## Backward compatibility

[`classification_robustness()`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md)
is unchanged by default. It gains one new argument, `provenance_aware`,
off by default:

``` r

# Identical to every previous release:
classification_robustness(p, system = "wrb2022", n = 100)

# Opt in to grade-scaled perturbation:
classification_robustness(p, system = "wrb2022", n = 100,
                          provenance_aware = TRUE)
```

## In the Shiny app

The Uncertainty tab of the Pro Shiny app
([`run_classify_app()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_classify_app.md))
wraps all of this: pick a system, set the number of Monte-Carlo runs,
and read off the posterior bar chart, the entropy, and the
attribute-sensitivity table.

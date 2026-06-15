# SiBCS B textural relacao-textural (item h)

Implements the verbatim Embrapa (2018) SiBCS Cap 2 p.56 item (h): the
total-clay B/A textural ratio, keyed on the A-horizon clay content,
computed over the footnote-4 control section. This is the SiBCS-specific
PROPORTIONAL clay-increase test, distinct from (and mostly a subset of)
the WRB
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
absolute-increase rule – it differs only for very sandy A horizons (clay
\< ~7.5%), where the ratio test is a smaller absolute jump than argic's
+6 pp.

## Usage

``` r
test_ratio_textural_sibcs(h)
```

## Arguments

- h:

  A horizons `data.table`
  ([`ensure_horizon_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/ensure_horizon_schema.md)).

## Value

A subtest result list (`passed`, `layers`, `missing`, `details`).

## Details

Control section (footnote 4): A clay = thickness-weighted mean of the A
horizons; B clay = thickness-weighted mean of the B horizons (excluding
BC) within a window from the top of B equal to 30 cm if the A is \< 15
cm thick, or twice the A thickness if the A is \\= 15 cm thick.
Thresholds: ratio \\ 1.50 if A clay \\ 400 g/kg; \\ 1.70 if 150-400
g/kg; \\ 1.80 if \\ 150 g/kg.

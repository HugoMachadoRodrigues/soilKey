# Robust per-layer column accessor.

\`h\[\[col\]\]\[i\]\` returns \`NULL\` (length 0) when the column is
absent from the horizon schema entirely (e.g. older fixtures pre-dating
a schema extension). Downstream code then reaches \`is.na(NULL)\`, which
is \`logical(0)\`, and crashes inside \`if (...)\`. This helper converts
an absent column to \`NA\` of the requested mode so the "missing" branch
in every sub-test is exercised cleanly.

## Usage

``` r
.col_at(h, column, i, default = NA)
```

## Details

Added 2026-04-30 after the canonical-fixture benchmark surfaced five
errors of the form "argument is of length zero" coming from
\`test_numeric_above\`, \`test_pattern_match\`,
\`test_shrink_swell_cracks\` on fixtures whose schema predates v0.3.3
column extensions.

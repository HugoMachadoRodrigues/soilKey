# Normalise KSSL USDA subgroup labels for benchmark comparison

KSSL stores \`samp_taxsubgrp\` in lower-case, space-separated form
("typic hapludalfs", "aquic argiudolls"). soilKey's \`classify_usda()\`
returns Title Case names ("Typic Hapludalfs"). The benchmark runner at
\`level = "subgroup"\` lowercases both sides and trims whitespace, but
this helper makes the normalisation explicit when users want to compare
KSSL labels against arbitrary classifier output. Idempotent.

## Usage

``` r
normalise_kssl_subgroup(x)
```

## Arguments

- x:

  Character vector of KSSL subgroup names.

## Value

Lowercase, single-space-separated vector.

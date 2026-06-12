# Filter a pedon list to those carrying \`sys\`'s reference label, THEN cap at \`max_n\` via the reproducible (seed-42) sample. The order matters: capping before filtering (the pre-v0.9.110 bug) starves sparsely-labelled systems – e.g. only a handful of FEBR pedons carry a USDA label, so a head/random cap over the whole set left FEBR-USDA at n=3 despite hundreds of labelled rows.

Filter a pedon list to those carrying \`sys\`'s reference label, THEN
cap at \`max_n\` via the reproducible (seed-42) sample. The order
matters: capping before filtering (the pre-v0.9.110 bug) starves
sparsely-labelled systems – e.g. only a handful of FEBR pedons carry a
USDA label, so a head/random cap over the whole set left FEBR-USDA at
n=3 despite hundreds of labelled rows.

## Usage

``` r
.benchmark_filter_then_cap(pedons, sys, max_n)
```

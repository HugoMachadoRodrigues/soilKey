# Alias \`reference_wrb_from_usda\` -\> \`reference_wrb\` on KSSL pedons

Internal helper used by both
[`load_kssl_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_sample.md)
and
[`load_kssl_nasis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_nasis_sample.md)
since v0.9.91 to populate the canonical `reference_wrb` field from the
KSSL-specific `reference_wrb_from_usda` cross-walk slot. Only sets the
field when it is currently NULL, so explicit annotations are preserved.

## Usage

``` r
.kssl_alias_reference_wrb(pedons)
```

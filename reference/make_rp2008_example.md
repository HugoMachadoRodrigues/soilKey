# Example pedons for the references of the Referentiel pedologique 2008

Small, plausible profiles written for the tests and examples, one per
reference implemented (the 29 of the first nine GER), in the RP notation
(`site$designation_system = "rp2008"`) with the properties their tests
read. They are not real profiles.

## Usage

``` r
make_rp2008_example(
  reference = c("alocrisols_typiques", "alocrisols_humiques", "vitrandosols",
    "silandosols_eutriques", "silandosols_dystriques", "silandosols_perhydriques",
    "aluandosols_hapliques", "aluandosols_perhydriques", "anthroposols_transformes",
    "anthroposols_artificiels", "anthroposols_reconstitues", "anthroposols_construits",
    "anthroposols_archeologiques", "arenosols", "brunisols_eutriques",
    "brunisols_dystriques", "rendosols", "calcosols", "dolomitosols", "rendisols",
    "calcisols", "magnesisols", "calcarisols", "chernosols_hapliques", 
    
    "chernosols_typiques", "chernosols_melanoluviques", "colluviosols",
    "cryosols_histiques", "cryosols_mineraux")
)
```

## Arguments

- reference:

  The code of a reference (see
  `system.file("rules", "rp2008", "key.yaml", package = "soilKey")`):
  `"alocrisols_typiques"`, `"alocrisols_humiques"`, `"vitrandosols"`,
  `"silandosols_eutriques"`, `"silandosols_dystriques"`,
  `"silandosols_perhydriques"`, `"aluandosols_hapliques"`,
  `"aluandosols_perhydriques"`, `"anthroposols_transformes"`,
  `"anthroposols_artificiels"`, `"anthroposols_reconstitues"`,
  `"anthroposols_construits"`, `"anthroposols_archeologiques"`,
  `"arenosols"`, `"brunisols_eutriques"`, `"brunisols_dystriques"`,
  `"rendosols"`, `"calcosols"`, `"dolomitosols"`, `"rendisols"`,
  `"calcisols"`, `"magnesisols"`, `"calcarisols"`,
  `"chernosols_hapliques"`, `"chernosols_typiques"`,
  `"chernosols_melanoluviques"`, `"colluviosols"`,
  `"cryosols_histiques"`, `"cryosols_mineraux"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_rp2008(make_rp2008_example("brunisols_eutriques"), on_missing = "silent")$name
#> [1] "brunisol eutrique"
```

# Example pedons for the references of the Referentiel pedologique 2008

Small, plausible profiles written for the tests and examples, one per
reference implemented (the 71 of the first twenty-one GER), in the RP
notation (`site$designation_system = "rp2008"`) with the properties
their tests read. They are not real profiles.

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
    "cryosols_histiques", "cryosols_mineraux", "ferrallitisols_meubles",
    "ferrallitisols_nodulaires", "ferrallitisols_petroxydiques",
    "ferrallitisols_duroxydiques", "oxydisols_meubles", "oxydisols_nodulaires",
    "oxydisols_petroxydiques", "ferruginosols_meubles", "ferruginosols_semiluviques",
    "ferruginosols_luviques", "ferruginosols_petroxydiques", "fersialsols_carbonates",
    "fersialsols_calciques", "fersialsols_insatures", "fersialsols_eluviques", 
    
    "fluviosols_bruts", "fluviosols_juveniles", "fluviosols_typiques",
    "fluviosols_brunifies", "grisols_eluviques", "grisols_degrades", "grisols_hapliques",
    "gypsosols_hapliques", "gypsosols_petrogypsiques", "histosols_fibriques",
    "histosols_mesiques", "histosols_sapriques", "histosols_composites",
    "histosols_leptiques", "leptismectisols", "lithosols", "neoluvisols",
    "luvisols_typiques", "luvisols_degrades", "luvisols_derniques", "luvisols_tronques",
    "quasi_luvisols", "nitosols", "organosols_holorganiques", 
    
    "organosols_calcaires", "organosols_satures", "organosols_insatures")
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
  `"cryosols_histiques"`, `"cryosols_mineraux"`,
  `"ferrallitisols_meubles"`, `"ferrallitisols_nodulaires"`,
  `"ferrallitisols_petroxydiques"`, `"ferrallitisols_duroxydiques"`,
  `"oxydisols_meubles"`, `"oxydisols_nodulaires"`,
  `"oxydisols_petroxydiques"`, `"ferruginosols_meubles"`,
  `"ferruginosols_semiluviques"`, `"ferruginosols_luviques"`,
  `"ferruginosols_petroxydiques"`, `"fersialsols_carbonates"`,
  `"fersialsols_calciques"`, `"fersialsols_insatures"`,
  `"fersialsols_eluviques"`, `"fluviosols_bruts"`,
  `"fluviosols_juveniles"`, `"fluviosols_typiques"`,
  `"fluviosols_brunifies"`, `"grisols_eluviques"`, `"grisols_degrades"`,
  `"grisols_hapliques"`, `"gypsosols_hapliques"`,
  `"gypsosols_petrogypsiques"`, `"histosols_fibriques"`,
  `"histosols_mesiques"`, `"histosols_sapriques"`,
  `"histosols_composites"`, `"histosols_leptiques"`,
  `"leptismectisols"`, `"lithosols"`, `"neoluvisols"`,
  `"luvisols_typiques"`, `"luvisols_degrades"`, `"luvisols_derniques"`,
  `"luvisols_tronques"`, `"quasi_luvisols"`, `"nitosols"`,
  `"organosols_holorganiques"`, `"organosols_calcaires"`,
  `"organosols_satures"`, `"organosols_insatures"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_rp2008(make_rp2008_example("brunisols_eutriques"), on_missing = "silent")$name
#> [1] "brunisol eutrique"
```

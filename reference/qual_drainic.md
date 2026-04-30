# Drainic qualifier (dr): artificially drained organic soil. v0.9.1: site\$drainage_class or site\$land_use carries an explicit *artificial* drainage marker AND organic_material passes. Natural drainage classes (e.g. "very poorly drained", "well drained") do NOT trigger Drainic on their own.

Drainic qualifier (dr): artificially drained organic soil. v0.9.1:
site\$drainage_class or site\$land_use carries an explicit *artificial*
drainage marker AND organic_material passes. Natural drainage classes
(e.g. "very poorly drained", "well drained") do NOT trigger Drainic on
their own.

## Usage

``` r
qual_drainic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

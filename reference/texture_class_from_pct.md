# NRCS texture-class shorthand from clay / silt / sand percent

aqp's
[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html)
requires an NRCS texture class column (e.g. "SCL", "C", "CL", "FS").
soilKey horizons only carry the percent fractions; this helper derives
the class from the standard USDA texture triangle.

## Usage

``` r
texture_class_from_pct(clay, silt, sand)
```

## Arguments

- clay:

  Numeric vector of clay percent (0-100).

- silt:

  Numeric vector of silt percent.

- sand:

  Numeric vector of sand percent. (clay + silt + sand should sum to
  ~100; mild deviations are tolerated.)

## Value

Character vector of NRCS texture class abbreviations.

## Details

Returns the standard NRCS abbreviation:

|      |                 |
|------|-----------------|
| COS  | Coarse sand     |
| S    | Sand            |
| FS   | Fine sand       |
| VFS  | Very fine sand  |
| LS   | Loamy sand      |
| LFS  | Loamy fine sand |
| SL   | Sandy loam      |
| FSL  | Fine sandy loam |
| L    | Loam            |
| SIL  | Silt loam       |
| SI   | Silt            |
| SCL  | Sandy clay loam |
| CL   | Clay loam       |
| SICL | Silty clay loam |
| SC   | Sandy clay      |
| SIC  | Silty clay      |
| C    | Clay            |

Implementation follows the canonical USDA texture triangle; vector- ised
over the input. NA in / NA out.

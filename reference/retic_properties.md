# Retic properties (WRB 2022)

WRB 2022 Chapter 3.2.11: claric material interfingering into an argic or
natric horizon from its upper limit. soilKey reads criteria 1 and 6 from
the data, the finer-textured parts belonging to an argic or natric
horizon
([`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
[`natric_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/natric_horizon.md))
and the interfingering recorded at its upper limit, and stands in for
criteria 2-5, 7 and 8 (claric coarser parts, colour and clay contrasts,
width, 10-90% of the sections, not in a plough layer), which no column
holds, by the designation the describer gave: `pattern` on the uppermost
argic or natric layer or the layer directly above it.

## Usage

``` r
retic_properties(pedon, pattern = "glossic|retic|albeluvic")
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- pattern:

  Regex (default `"glossic|retic|albeluvic"`).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

`FALSE` without an argic or natric horizon, or when designations are
recorded and none shows the interfingering; `NA` when the argic or
natric horizon cannot be established or there are no designations. Until
v0.9.219 the designation alone decided, anywhere in the profile.

## References

IUSS Working Group WRB (2022), Chapter 3.2.11, Retic properties.

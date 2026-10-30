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
holds, by what the describer recorded on the uppermost argic or natric
layer or the layer directly above it: a designation matching `pattern`;
an FAO designation of a layer with distinct parts of an E and a B
horizon (`E/B`, `2E/Bt`, `B/E`, since v0.9.312); or (since v0.9.312) an
E or claric layer whose lower boundary on the argic or natric horizon is
irregular or broken (`boundary_topography`), the uneven limit that 3.2.1
names as an example of retic properties.

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

`FALSE` without an argic or natric horizon, or when designations or
boundaries are recorded and none shows the interfingering; `NA` when the
argic or natric horizon cannot be established or there are neither
designations nor boundary topographies. Until v0.9.219 the designation
alone decided, anywhere in the profile.

## References

IUSS Working Group WRB (2022), Chapter 3.2.11, Retic properties.

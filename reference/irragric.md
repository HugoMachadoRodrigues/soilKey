# Irragric horizon (WRB 2022): topsoil thickened by irrigation deposits.

v0.9.317: the criteria of WRB 2022 Ch 3.1.18 (p. 50-51), tested on every
stack of contiguous layers from the mineral soil surface (a surface
horizon of mineral material), each against the layer directly below it:

1.  soil aggregate structure or cloddy structure in \\\ge\\ 90% by
    volume;

2.  a clay content \\\ge\\ 10% (relative) and \\\ge\\ 3% (absolute)
    higher than that of the layer directly buried; the fine clay
    alternative is not recorded, so a smaller increase leaves it
    undecided;

3.  differences between subhorizons of \< 20% (relative) or \< 4%
    (absolute) in silt, clay and carbonates (also in the medium, fine
    and very fine sand, which soilKey does not record: undecided for
    more than one layer unless a recorded difference fails);

4.  \\\ge\\ 0.3% SOC in each layer and \\\ge\\ 0.5% on a weighted
    average;

5.  \\\ge\\ 25% traces of soil animal activity (`worm_holes_pct`,
    weighted);

6.  evidence that the land surface has been raised: soilKey records no
    such evidence, so this criterion is undecided;

7.  \\\ge\\ 20 cm thick (`min_thickness`).

The horizon is therefore never established from the data: `FALSE` when
every stack fails a criterion, `NA` otherwise. Until v0.9.316 any Ap or
Au of 20 cm passed.

## Usage

``` r
irragric(pedon, min_thickness = 20)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

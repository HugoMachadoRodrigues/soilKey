# Gleysol RSG gate (WRB 2022 Ch 4, p 103)

WRB-canonical (multi-path):

1.  Layer \\= 25 cm starting \\= 40 cm with gleyic properties throughout
    AND reducing conditions in some parts of every sublayer; OR

2.  Mollic/umbric \> 40 cm thick with reducing conditions some parts of
    every sublayer 40 cm below mineral surface to lower limit, AND
    directly underneath a layer \\= 10 cm with lower limit \\= 65 cm
    having gleyic properties + reducing conditions; OR

3.  Permanent saturation by water \\= 40 cm.

v0.3.4 enforces path 1 (the dominant path) and path 3 via designation (W
/ saturated marker). Path 2 is deferred (requires a depth-of- saturation
column that's not standard).

## Usage

``` r
gleysol(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

# Test the argic clay-increase criterion (WRB 2022)

Tests every consecutive pair of horizons in the profile against the WRB
2022 (4th ed., Chapter 3.1.3, criterion 2.a.iv-vi) clay-increase rules.
v0.3.1 brings these into compliance with the actual text (earlier
versions used loosened thresholds 3/1.2/8 from older WRB editions):

- overlying clay \< 15%: argic horizon must contain at least \*\*6%
  (absolute)\*\* more clay;

- overlying clay 15 to \<50%: argic / overlying clay ratio must be
  \*\*\>= 1.4\*\*;

- overlying clay \>= 50%: argic must contain at least \*\*20%
  (absolute)\*\* more clay.

Returns the indices of horizons that satisfy as argic candidates.

## Usage

``` r
test_clay_increase_argic(h)
```

## Arguments

- h:

  Horizons data.table (canonical schema).

## Value

Sub-test result list.

## References

IUSS Working Group WRB (2022), Chapter 3.1.3, Argic horizon, criteria
2.a.iv-vi (p. 36).

# Types of the Russian soil classification (internal)

The test of the types of the Russian soil classification (Field Guide of
2008, chapter 4), read by
[`classify_kidpr2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_kidpr2008.md)
through the type lists of `inst/rules/kidpr2008/types/`. A type is named
by the formula of its profile ("O-EL-BEL-BT-C"); the profile is matched
against every formula of its division with
[`match_horizon_sequence`](https://hugomachadorodrigues.github.io/soilKey/reference/match_horizon_sequence.md),
three-valued, on the horizons the divisions read
([`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md)):
fresh litter left out, the profile under a deposit of 40 cm or less, the
surface horizons by the thickest-horizon rule, sub-horizons merged,
transitions and small horizons left out, an overlay a feature of its
layer.

## Usage

``` r
type_formula_kidpr(pedon, type)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- type:

  The code of a type, as its rule file writes it (`"[CA01]"`).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md):
TRUE for the type of the profile; FALSE for another type of the division
(one whose formula matches as well is an intergrade, named in the
evidence); NA when an undecided layer could change the match. The
evidence gives the formula, the horizons that fill its slots, the
optional ones absent, the slots missing, the horizons not in the
formula, the horizons left out of the match, the surface horizon and the
edit distance.

## Details

Hugo's decisions of 2026-11-02: the match is closed on the horizons the
formulas of the division write (a horizon foreign to the division is
ignored), BEL only where it is the diagnostic eluvial horizon (CA07,
CA08, CA11, CA20, CA21, CD09; elsewhere a BEL neither blocks a formula
nor is required by it); a horizon in parentheses is optional, and so is
BEL except where it is diagnostic; the g and q marks are expected, not
required; the parent material is not tested; "directly under" passes
over transitions and sub-horizons, and over up to 15 cm with no
diagnostic horizon between AU and BCA. The types are tried most specific
first, agro before natural: the first that matches is the type, and
another that matches is an intergrade, in the evidence. A few
near-identity readings are written on the types they concern (the BF of
CB05, CE06 and CE07 is a BHF of either kind; CJ01 takes BCA or CAT; CH02
takes O, T or H; in CB and CE an EL fills the E of a formula); in CD a
BCA, like BEL, closes no formula that does not write it. A tie or a
missing thickness at the surface, or a layer the data route reads as no
horizon between two horizons of a formula, leaves the type undecided.

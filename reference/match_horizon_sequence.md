# Match a profile against a profile formula

Three-valued match of the identified horizons of a profile against the
profile formula of a type (the Russian Field Guide of 2008 names its
types by such formulas: "O-EL-BEL-BT-C"). Generic: the codes, the
vocabulary and the surface horizons are the caller's.

## Usage

``` r
match_horizon_sequence(
  table,
  spec,
  vocabulary = NULL,
  surface = NULL,
  agro = NULL,
  closure_surface = FALSE,
  max_cover_cm = 40,
  max_gap_cm = 0,
  pair_gaps_cm = NULL,
  buried_within_cm = Inf,
  max_completions = 2000L,
  agro_remnant = c("all", "thickest"),
  ...
)
```

## Arguments

- table:

  The identified horizons: the frame of
  [`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md),
  any data frame with a `code` column (and optionally `horizon_idx`,
  `top_cm`, `bottom_cm`, `passed`, `candidates`, `features`,
  `features_na`, `overlay`, `small`, `transitional`, `buried`,
  `rockflag`, `mark`, `composition`), or a character vector of codes
  from the top (NA an undecided layer, "" no diagnostic horizon). Rows
  are read in depth order when every top is recorded, else as given.

- spec:

  The formula: a string, or one parsed by the package's parser.

- vocabulary:

  Codes the match is closed on; NULL closes it on the formula's own
  codes only.

- surface, agro:

  Natural and agro surface codes for the thickest-horizon rule; both
  NULL: no rule, the first horizon is the surface.

- closure_surface:

  Whether surface and agro codes of the vocabulary that the formula does
  not name count in the closure (default FALSE).

- max_cover_cm:

  Thickness of cover above which no surface run is read.

- max_gap_cm, pair_gaps_cm:

  The gap allowed between two consecutive horizons (cm), and named
  allowances for pairs.

- buried_within_cm:

  Depth within which a buried profile counts.

- max_completions:

  Most completions of undecided layers tried; beyond it the result is
  NA.

- agro_remnant:

  Under an agro surface horizon, `"all"` keeps every natural horizon of
  the surface run in the sequence (the default); `"thickest"` keeps only
  the thickest of them, as the remnant, and reads the others as features
  (the thickest-horizon rule of the Russian Field Guide, p. 37).

- ...:

  When \`spec\` is a string, the arguments of the parser:
  `test_terminals` (rock and parent slots to test, by code, by signature
  such as "C\~~" or "DG", or by text such as "\[ABC\]"; TRUE for all),
  `optional` (codes whose slots are optional), `required_features`
  (features required wherever written), `alternatives` (a named list of
  codes accepted in place of a written one, `list(BCA = "CAT")`),
  `masters` and `features` (vocabularies: an unknown token is then an
  error), `parents`, `rock_flags` and `flag_masters`. An error when
  \`spec\` is already parsed.

## Value

A list: `passed` (TRUE, FALSE or NA), `formula`, `specificity`,
`distance` and `distance_exact` (the edit distance of the closest
reading, 0 for a match: exact when no layer is undecided, else a lower
bound over the completions), `matched` (a data frame of slots and the
horizons that fill them), `missing` (slots not filled), `extra`
(horizons in play not matched), `absent` (optional slots not present),
`ignored` (horizons left out of the match), `surface` and `absorbed`
(the surface horizon and the ones read as its features), `sequence` (the
codes in play), `undecided` (layers whose completion leaves it NA) and
`notes`.

## Details

The profile side is collapsed top-down: small horizons and transitional
rows are dropped; consecutive rows with the same code are merged
(sub-horizons: BT1, BT2 = BT), also across horizons left out of the
match; the rows of a buried profile become one buried token. A horizon
fills a slot by its code or by an overlay (an AU with an S overlay fills
an S slot). With \`surface\` and \`agro\` codes, the surface slot is
read by the thickest-horizon rule: the agro horizon if the surface run
has one, else the thickest natural horizon starting at the surface or
under a cover thinner than \`max_cover_cm\` (rows with no diagnostic
horizon, and horizons neither in play nor surface horizons, such as
litter coded L); the other horizons of that run are read as features and
leave the sequence.

The match is closed on \`vocabulary\` (the codes of every scheme of a
division): a horizon of the vocabulary, or of a tested slot of the
formula, present in the profile must be matched by a slot, in order; a
horizon foreign to both is ignored. Surface and agro codes the formula
does not name are left out of the closure unless \`closure_surface\` is
TRUE. Optional slots (in parentheses) may be absent. A rock or parent
slot not tested (C, D, R unless flagged) takes its horizon when that
code is in play and is left out otherwise. Two horizons matched one
after the other must lie directly one under the other: rows with no
diagnostic horizon between them may sum to \`max_gap_cm\`, or to the
allowance of the pair in \`pair_gaps_cm\`, named "UPPER\>LOWER" by codes
(a buried profile is "\[ABC\]": `c("AU>BCA" = 15, "RY>[ABC]" = 10)`).

A layer whose code is undecided (NA, with or without a list of
candidates) is completed in every way that matters: TRUE when every
completion matches, FALSE when none does, NA otherwise. A required
feature, rock flag or rock mark listed in `features_na`, a turbated
composition not recorded, or an unknown thickness, leave the result NA
when they decide it. More than three undecided layers with no candidates
leave the result NA unless no completion can match.

## Examples

``` r
# a profile as its sequence of horizon codes, from the top
match_horizon_sequence(c("AY", "EL", "BEL", "BT", "C"), "AY-EL-BEL-BT-C")$passed
#> [1] TRUE
# an optional remnant (in parentheses) absent; typical g features not needed
match_horizon_sequence(c("P", "BEL", "BT", "G", "C"), "P-(ELg)-BELg-BTg-G-CG")$passed
#> [1] TRUE
# closed on a vocabulary: a G horizon the formula does not name blocks it
match_horizon_sequence(c("O", "EL", "BEL", "BT", "G", "C"), "O-EL-BEL-BT-C",
                       vocabulary = c("O", "EL", "BEL", "BT", "G"))$passed
#> [1] FALSE
# an undecided layer that could be EL or another horizon
match_horizon_sequence(c("AY", NA, "BT", "C"), "AY-EL-BT-C")$passed
#> [1] NA
# a rock symbol tested where it names the type
match_horizon_sequence(c("O", "C", "R"), "O-(C)-R", test_terminals = "R")$passed
#> [1] TRUE
```

# Neossolos Litolicos (Cap 12): contato litico ou litico fragmentario \\= 50 cm.

v0.9.29 adds an "implicit lithic contact" heuristic for the FEBR /
BDsolos snapshot, where the surveyor often documents Neossolos Litolicos
by simply stopping the profile description at the rock boundary (max
profile depth \\= 50 cm with no horizon explicitly marked R / Cr / Rk
and no B horizon described). Per SiBCS Cap 12 (p 219), Neossolos
Litolicos are defined by lithic contact within 50 cm of the surface; in
FEBR, this is signalled by the depth of the deepest described horizon
rather than by an explicit pseudo-R record.

## Usage

``` r
neossolo_litolico(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

The heuristic fires only when:

1.  the deepest `bottom_cm` value is \\= 50 cm,

2.  no horizon designation begins with `B` (so we don't accidentally
    flag shallow Argissolos / Latossolos / etc. that have a Bt or Bw
    within 50 cm), AND

3.  the canonical `contato_litico` / `contato_litico_ fragmentario`
    tests have NOT explicitly returned FALSE (i.e. the surveyor did not
    describe a non-rock material deeper than 50 cm).

Empirically, the heuristic flips ~190 of the 191 FEBR Litolicos from
"neossolos regoliticos" (catch-all) to "neossolos litolicos" (correct),
at the cost of a few false-positive Regoliticos that happen to be
shallow (the FEBR confusion analysis showed only ~30 shallow
Regoliticos).

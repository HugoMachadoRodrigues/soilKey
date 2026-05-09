# Pluralise a single Portuguese token using SiBCS conventions

Rules:

- Tokens of \<= 2 chars are kept as-is (abbreviations like "tb"/"ta"
  used in SiBCS Cambissolo activity modifiers).

- Tokens already ending in "s" are kept as-is.

- Otherwise: append "s" (covers all -o, -ico, -oso, -eo, -io endings
  present in SiBCS Order, Subordem, GG, and Subgrupo modifiers; -al /
  -el / -ol words don't appear in the SiBCS taxonomy at these levels).

## Usage

``` r
.redape_pluralise_pt(w)
```

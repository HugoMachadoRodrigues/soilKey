# Example pedons for the types of the German KA5 soil systematics

Small, plausible profiles written for the tests and examples, one per
type of Tab. 51 (56), with designations in the KA5 notation (declared in
`site$designation_system`) and the properties soilKey's order of tests
reads on the data. They are not real profiles. `near_miss = TRUE` gives
a twin that misses the type's gate (a depth, a share, a record moved
across it); `route = "data"` drops the designations and the declared
notation, so that the horizons are read on the data.

## Usage

``` r
make_ka5_example(
  type = c("KV", "KM", "KH", "HH", "HN", "JP", "JG", "JS", "JD", "IA", "IW", "UEA", "MR",
    "MC", "MO", "MK", "MD", "MH", "MN", "GH", "GM", "GN", "GG", "AO", "AT", "AQ", "AZ",
    "AB", "FF", "FS", "OO", "OL", "TC", "TT", "RN", "RQ", "RR", "RZ", "XX", "YK", "YE",
    "YY", "YU", "YO", "SG", "SH", "SS", "PP", "LF", "LL", "BB", "CR", "CF", "DD", "VV",
    "VW"),
  near_miss = FALSE,
  route = c("notation", "data"),
  subtype = NULL
)
```

## Arguments

- type:

  The code of a type: `"KV"`, `"KM"`, `"KH"`, `"HH"`, `"HN"` (Moore);
  `"JP"`, `"JG"`, `"JS"`, `"JD"`, `"IA"`, `"IW"` (subhydric); `"UEA"`
  (the Strand, the key's Ue-A), `"MR"`, `"MC"`, `"MO"`, `"MK"`, `"MD"`,
  `"MH"`, `"MN"`, `"GH"`, `"GM"`, `"GN"`, `"GG"`, `"AO"`, `"AT"`,
  `"AQ"`, `"AZ"`, `"AB"` (semiterrestrial); `"FF"`, `"FS"`, `"OO"`,
  `"OL"`, `"TC"`, `"TT"`, `"RN"`, `"RQ"`, `"RR"`, `"RZ"`, `"XX"`,
  `"YK"`, `"YE"`, `"YY"`, `"YU"`, `"YO"`, `"SG"`, `"SH"`, `"SS"`,
  `"PP"`, `"LF"`, `"LL"`, `"BB"`, `"CR"`, `"CF"`, `"DD"`, `"VV"`, `"VW"`
  (terrestrial).

- near_miss:

  Return the twin that misses the type's gate.

- route:

  `"notation"` (default) or `"data"`.

- subtype:

  The code of a subtype, as printed (`"BBn"`, `"GG-BB"`, `"LF-YU"`,
  `"BB-GG"`, `"MRb"`, `"KV-KM"` ...; the 223 of pp. 202-262, the
  Strand's also as `"UEAn"`, and the open `"[GHu]"` and `"[KHm]"`): the
  example profile of that subtype, written from one of its printed
  formulas with depths inside its windows (`type` and `near_miss` are
  then not read). The type examples key to the Norm subtype of their
  type (the Treposol's to an example subtype, the Moorgley's to the
  Niedermoorgley: they have no Norm).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_ka5(make_ka5_example("LL"), on_missing = "silent")$name
#> [1] "Normparabraunerde"
classify_ka5(make_ka5_example("LL", route = "data"), on_missing = "silent")$name
#> [1] "Normparabraunerde"
classify_ka5(make_ka5_example(subtype = "GG-BB"), on_missing = "silent")$name
#> [1] "Gley-Braunerde"
```

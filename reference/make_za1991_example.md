# Example pedons for the soil forms of the South African classification

Small synthetic profiles written for the tests and examples, one per
soil form of the South African soil classification of 1991 (named in
lower case: `"hutton"`, `"clovelly"` ...), each with one layer per
diagnostic horizon or material of the form's sequence and the properties
soilKey's tests read, and five special cases: `"lichtenburg"` (named by
van Huyssteen 2020, outside the 1991 key: provisional), `"lithosol"` (an
orthic A of 80 mm over unconsolidated material: no form, the land class
rock and lithosols), `"undecided"` (an orthic A over a B whose hue is
not recorded: Hutton or Clovelly undecided), `"buried"` (400 mm of
aeolian sand over a Valsrivier: the buried soil keys) and `"truncated"`
(a red apedal B at the surface over a dorbank: Garies, truncated). They
are not real profiles.

## Usage

``` r
make_za1991_example(form = "hutton", notation = FALSE, family = NULL)
```

## Arguments

- form:

  The form, in lower case, or a special case.

- notation:

  Write the name of each horizon in `diagnostic_za` (the South African
  notation) besides the data (default `FALSE`).

- family:

  Optional family code of the form (`"Hu1200"`, or its four digits
  `"1200"`); `NULL` (default) for the first family.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Since v0.9.359 each form example also holds the classes of one of its
families: the first of the form by default (Hu1100 for Hutton), or the
family named in `family`, made by changing the layer each family
criterion reads (its colour, clay, cations, structure, carbonates,
consistence) or adding the layer it reads (rock under the organic O, a
podzolic layer under the yellow-brown apedal B of Constantia).

## Examples

``` r
classify_za1991(make_za1991_example("clovelly"), on_missing = "silent")$name
#> [1] "Clovelly, Cv1100 Twyfelaar, sandy loam"
classify_za1991(make_za1991_example("hutton", family = "Hu2200"), on_missing = "silent")$name
#> [1] "Hutton, Hu2200 Suurbekom, sandy loam"
```

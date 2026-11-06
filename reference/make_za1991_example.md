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
make_za1991_example(form = "hutton", notation = FALSE)
```

## Arguments

- form:

  The form, in lower case, or a special case.

- notation:

  Write the name of each horizon in `diagnostic_za` (the South African
  notation) besides the data (default `FALSE`).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_za1991(make_za1991_example("clovelly"), on_missing = "silent")$name
#> [1] "Clovelly"
```

# Convert BDsolos mottle-quantity ordinal class to percent

BDsolos exports the "Mosqueado - Quantidade" field as an ordinal
Portuguese class (pouco/comum/abundante in singular OR plural, with
various accent / casing variants). The soilKey schema uses
`redoximorphic_features_pct` (numeric volume maps the ordinal to a
representative midpoint percent so that the
[`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
diagnostic can fire on field-described mottles.

## Usage

``` r
.bdsolos_mosqueado_to_pct(x)
```

## Arguments

- x:

  Character vector of mottle-quantity ordinal labels.

## Value

Numeric vector of representative percent values (NA for empty / unknown
labels).

## Details

Mapping (per Embrapa / SiBCS field-description manual):

|                      |                  |               |
|----------------------|------------------|---------------|
| Ordinal              | Percent range    | Midpoint used |
| pouco                | less than 2 pct  | 1             |
| comum                | 2 to 20 pct      | 10            |
| abundante            | more than 20 pct | 30            |
| ausente / empty / NA | 0 pct            | NA (missing)  |

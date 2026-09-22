# Diagnostic inspection of a BDsolos CSV before loading

Reads the CSV header, attempts to map each column to the soilKey horizon
schema via `.bdsolos_match_column`, and prints three sections:

## Usage

``` r
inspect_bdsolos_csv(path, sep = NULL)
```

## Arguments

- path:

  Path to the CSV downloaded from BDsolos.

- sep:

  Field separator (default `","`; some BDsolos exports use `";"` or
  tab).

## Value

Invisibly, a list with `mapped`, `unmapped`, `munsell_present`,
`taxon_column`.

## Details

- **Mapped columns** – BDsolos name -\> soilKey name

- **Unmapped columns** – columns the loader will ignore (review these
  before running `load_bdsolos_csv` to make sure no critical attribute
  is silently dropped)

- **Munsell coverage** – whether matiz / valor / croma are present in
  either umido or seco variants

Run this before
[`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md)
on any new CSV from BDsolos, especially if the export schema looks
unfamiliar (BDsolos has shipped multiple schema versions over the
years).

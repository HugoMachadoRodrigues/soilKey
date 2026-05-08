# Discover Munsell-related columns in a FEBR layer table

Returns a list with elements `moist_string` (single column with the
Munsell string), `moist_hue`, `moist_value`, `moist_chroma` (separate
columns), `dry_string`, `dry_hue`, `dry_value`, `dry_chroma`. Each is
either a column name (character) or `NA_character_`. The loader uses the
parsed columns when available, falls back to parsing the string column.

## Usage

``` r
.detect_febr_munsell_columns(cols)
```

## Details

Recognised conventions (from the May 2026 scan of 249 FEBR datasets):

- `cor_munsell_umida` (ctb0039, ctb0572)

- `cor_cod_munsell_umida` (ctb0032)

- `cor_cod_munsell_umida_1` + `cor_nome_munsell_umida_1` (ctb0005)

- `cor_cod_munsell_umida_i` (ctb0019)

- `cor_munsell_umida_matiz` + `cor_munsell_umida_valor` +
  `cor_munsell_umida_croma` (ctb0562-ctb0700+)

- `cor_munsell_umida_nome` (ctb0562+)

- `cor_matriz_umido_munsell` (canonical, morphology())

Same patterns apply for "seca" (dry).

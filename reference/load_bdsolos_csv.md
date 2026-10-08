# Load a BDsolos CSV export as a list of PedonRecord objects

Reads the long-format BDsolos CSV (one row per horizon, with a
profile-id key) and returns a list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects. Auto-detects the column-name convention via
[`inspect_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/inspect_bdsolos_csv.md)
and maps to the soilKey horizon schema. Texture (argila / silte / areia)
is converted from g/kg to percent (BDsolos canonical unit).

## Usage

``` r
load_bdsolos_csv(path, sep = NULL, verbose = TRUE)
```

## Arguments

- path:

  Path to the BDsolos CSV.

- sep:

  Field separator. Default `","`; BDsolos sometimes exports with `";"`
  or tab – pass it explicitly.

- verbose:

  If `TRUE` (default), prints a one-line summary.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects. Each pedon has `site$id` from the profile-id column, the
taxonomic reference (when present) at `site$reference_sibcs`, and one
horizon row per CSV row matching the profile id.

## Details

Iron (v0.9.221): `"CDB - Ferro (g/kg)"` (dithionite-citrate-
bicarbonate, free Fe as the element) goes to `fe_dcb_pct`, which the WRB
Fe-dith criteria read (nitic horizon, Ferric, Ferritic);
`"Ataque sulfurico - Fe2O3"` (total Fe of the sulfuric attack, as the
oxide) goes to `fe2o3_sulfuric_pct`, which the SiBCS ferrico /
perferrico classes read; `"Oxalato de Amonio - Ferro"` goes to
`fe_ox_pct`. All three are converted from g/kg to %. Until v0.9.220 the
sulfuric Fe2O3 filled `fe_dcb_pct`, unconverted.

Profile-id columns are auto-detected: looks for any column whose
normalised name matches
`"id_perfil|profile_id|cod_perfil|^perfil$|sample_id|^id$"`; falls back
to the first column when none match.

## See also

[`inspect_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/inspect_bdsolos_csv.md),
[`download_bdsolos`](https://hugomachadorodrigues.github.io/soilKey/reference/download_bdsolos.md).

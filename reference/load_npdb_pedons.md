# Read the National Pedon Database of Canada

Reads the pre-packaged CSV files of the National Pedon Database (NPDB,
version 3, Agriculture and Agri-Food Canada, CanSIS) into
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects, with the survey's classification under the Canadian System of
Soil Classification as the reference.

## Usage

``` r
load_npdb_pedons(dir, max_n = NULL, seed = NULL, verbose = TRUE)
```

## Arguments

- dir:

  Folder with the NPDB CSV files.

- max_n:

  Optional maximum number of pedons (a random sample).

- seed:

  Optional seed for that sample.

- verbose:

  Print progress.

## Value

A named list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (names are the NPDB `PEDON_ID`).

## Details

The NPDB is not distributed with soilKey. It is published on the Open
Government Portal of Canada under the Open Government Licence - Canada
(dataset "National Pedon Database"); download the pre-packaged CSV files
and unzip them into a folder. `Info.csv` and `Horizons.csv` are
required; `Morphology.csv`, `Physical.csv`, `Chemical.csv` and
`Profiles.csv` are read when present.

What the reader does with the records:

- records are split on CR-LF only, since some fields hold a lone
  carriage return ("subangular\r blocky");

- the designation is rebuilt in the Canadian notation from its parts
  (lithologic numeral, master, suffixes, numeric modifier: "IICk1"), and
  the site declares `designation_system = "cssc"`;

- depths: organic and water horizons recorded upward from the mineral
  surface (top above bottom) become negative depths; a mineral horizon
  with its depths reversed or negative keeps \|top\| with no bottom when
  it is the deepest layer, and no depths otherwise; a missing top is
  taken from the layer above; every profile is shifted to begin at 0 cm,
  and `site$npdb_depth_order_mismatch` flags profiles whose depth order
  differs from the order of their layers;

- colours: the codes ("100yr4030", "10yr2010", "025y 4020",
  "10.0yr4.0/3.0", "n 50") are read with hue numbers above 0 and up to
  10; of the two matrix colours, the one that is never darker within a
  profile (and lighter at least once) is read as the dry colour and the
  other as the moist one; when they do not separate, `COLOUR_1` is read
  as the moist colour and no dry colour is read;

- total N: the value 9 (a no-data code) and a value repeated over three
  or more horizons are not read; the units are told apart by the median
  C:N of each pedon (of its dataset when the pedon has too few values):
  5 to 40 is read as percent, 0.5 to 4 as g/kg (divided by 10), anything
  else is not read; a horizon whose C:N then lies outside 3 to 60 is not
  read either;

- base saturation: computed from the exchangeable bases and the CEC (at
  most 100 %) when they are recorded, the recorded value being kept when
  it agrees with it (one dataset records something else in that column);

- pH: a dataset whose pH in CaCl2 is mostly above its pH in water has
  the two columns swapped back;

- values outside their possible range are not read;

- the survey's order, great group and subgroup words are turned into the
  names of the 3rd edition (1998) where they match one
  (`reference_cssc_order`, `reference_cssc_great_group`,
  `reference_cssc_subgroup`), including the subgroups the 1998 edition
  merged (Solodic into Solonetzic); names of earlier editions with no
  1998 equivalent are left out, and the raw words are kept in
  `reference_cssc_raw`.

The NPDB records no soil climate (mean annual soil temperature, moisture
subclass), the chemistry of the podzolic B (pyrophosphate-extractable Fe
and Al) or the contrast of mottles. The Chernozemic order and the Gray
Brown Luvisols, which ask for the climate, are undecided on these
records (see
[`benchmark_npdb`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_npdb.md)).

## References

Agriculture and Agri-Food Canada (2021). National Pedon Database,
version 3. Open Government Portal, Open Government Licence - Canada.

## Examples

``` r
if (FALSE) { # \dontrun{
peds <- load_npdb_pedons("path/to/npdb_v3_csv")
b <- benchmark_npdb(peds, level = "order")
b$accuracy
} # }
```

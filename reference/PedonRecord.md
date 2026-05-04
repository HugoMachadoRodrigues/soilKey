# PedonRecord: structured representation of a single pedon

PedonRecord: structured representation of a single pedon

PedonRecord: structured representation of a single pedon

## Details

The central data carrier in soilKey. A PedonRecord bundles everything we
know about one soil profile: site metadata, the horizons table (with a
fixed canonical schema — see
[`horizon_column_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)),
optional Vis-NIR/MIR spectra, profile photographs, source documents, and
a provenance log that records, per (horizon, attribute) pair, where each
value came from (`measured`, `extracted_vlm`, `predicted_spectra`,
`inferred_prior`, `user_assumed`).

All diagnostic functions
([`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
[`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
[`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md),
...) consume a PedonRecord directly. The provenance log is what allows
the final
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
to assign a meaningful evidence grade.

## Public fields

- `site`:

  List. Site-level metadata: `lat`, `lon`, `crs` (default 4326), `date`,
  `country`, `elevation_m`, `slope_pct`, `aspect_deg`, `landform`,
  `parent_material`, `land_use`, `vegetation`, `drainage_class`, plus an
  arbitrary `id`.

- `horizons`:

  data.table with the canonical horizon schema.

- `spectra`:

  List with optional `vnir` matrix (rows = horizons, cols = wavelengths
  in nm), `mir` matrix, and `metadata` list.

- `images`:

  List of named lists describing profile photographs.

- `documents`:

  List of named lists describing source documents.

- `provenance`:

  data.table with columns `horizon_idx`, `attribute`, `source`,
  `confidence`, `notes`.

## Methods

### Public methods

- [`PedonRecord$new()`](#method-PedonRecord-new)

- [`PedonRecord$validate()`](#method-PedonRecord-validate)

- [`PedonRecord$to_aqp()`](#method-PedonRecord-to_aqp)

- [`PedonRecord$from_aqp()`](#method-PedonRecord-from_aqp)

- [`PedonRecord$add_measurement()`](#method-PedonRecord-add_measurement)

- [`PedonRecord$summary()`](#method-PedonRecord-summary)

- [`PedonRecord$print()`](#method-PedonRecord-print)

- [`PedonRecord$clone()`](#method-PedonRecord-clone)

------------------------------------------------------------------------

### Method `new()`

Construct a PedonRecord.

#### Usage

    PedonRecord$new(
      site = NULL,
      horizons = NULL,
      spectra = NULL,
      images = NULL,
      documents = NULL,
      provenance = NULL
    )

#### Arguments

- `site`:

  List of site-level metadata.

- `horizons`:

  data.frame/data.table of horizons.

- `spectra`:

  Optional list with `vnir`, `mir`, `metadata`.

- `images`:

  Optional list of image descriptors.

- `documents`:

  Optional list of document descriptors.

- `provenance`:

  Optional provenance data.table; if NULL, an empty one is created.

------------------------------------------------------------------------

### Method `validate()`

Validate the record against soil-physical sanity rules.

Checks: top \< bottom for every horizon; no overlapping depths;
clay+silt+sand sum to 100 ± 2 where all three are reported; pH values
plausible (1..12); CEC \>= sum of exchangeable bases (Ca, Mg, K, Na);
Munsell value/chroma in plausible ranges; coarse fragments percent in
\[0, 100\]; OC geographic ranges. Returns a list with `valid`, `errors`,
`warnings`, `n_horizons`.

#### Usage

    PedonRecord$validate(strict = FALSE, verbose = TRUE)

#### Arguments

- `strict`:

  If `TRUE`, throws on errors instead of returning.

- `verbose`:

  If `TRUE`, prints messages via cli.

#### Returns

Invisibly, a list summarising the validation outcome.

------------------------------------------------------------------------

### Method `to_aqp()`

Coerce to an aqp `SoilProfileCollection`.

#### Usage

    PedonRecord$to_aqp()

#### Returns

A `SoilProfileCollection`. Requires the `aqp` package.

------------------------------------------------------------------------

### Method [`from_aqp()`](https://hugomachadorodrigues.github.io/soilKey/reference/from_aqp.md)

Populate this record from an aqp `SoilProfileCollection`.

#### Usage

    PedonRecord$from_aqp(spc, top_col = "top_cm", bottom_col = "bottom_cm")

#### Arguments

- `spc`:

  A `SoilProfileCollection`.

- `top_col`:

  Name of the top-depth column in `spc` (mapped to `top_cm`).

- `bottom_col`:

  Name of the bottom-depth column (mapped to `bottom_cm`).

#### Returns

Invisibly self (mutated in place).

------------------------------------------------------------------------

### Method `add_measurement()`

Add a measurement (or extracted/predicted value) and record its
provenance.

#### Usage

    PedonRecord$add_measurement(
      horizon_idx,
      attribute,
      value,
      source = "measured",
      confidence = 1,
      notes = NA_character_,
      overwrite = FALSE
    )

#### Arguments

- `horizon_idx`:

  Integer horizon index (1-based).

- `attribute`:

  Name of the horizon column to set.

- `value`:

  New value for that cell.

- `source`:

  One of "measured", "extracted_vlm", "predicted_spectra",
  "inferred_prior", "user_assumed".

- `confidence`:

  Numeric in \[0, 1\].

- `notes`:

  Optional free-text note.

- `overwrite`:

  If `FALSE` (default) and the cell already has a value from a more
  authoritative source, leave it alone. If `TRUE`, overwrite.

#### Returns

Invisibly self.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Compact summary list (for serialization or testing).

#### Usage

    PedonRecord$summary(...)

#### Arguments

- `...`:

  Ignored (S3 summary signature compatibility).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty-print the record.

#### Usage

    PedonRecord$print(...)

#### Arguments

- `...`:

  Ignored (S3 print signature compatibility).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PedonRecord$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

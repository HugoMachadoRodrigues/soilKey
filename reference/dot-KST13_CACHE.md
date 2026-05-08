# Package-level cache for the parsed KST 13ed JSON files

v0.9.65 (Copilot review \#5):
[`kst13_criteria()`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_criteria.md)
previously parsed the full ~3.1 MB criteria JSON on every call. Looping
over a few hundred codes was crippling. This cache loads each JSON once
per session.

## Usage

``` r
.KST13_CACHE
```

## Format

An object of class `environment` of length 0.

## Details

Kept in a private environment so package-internal code can reach the
cached objects via `.KST13_CACHE$<filename>` but external callers must
go through
[`kst13_codes`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_codes.md)
/
[`kst13_criteria`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_criteria.md).

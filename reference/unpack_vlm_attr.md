# Map a parsed VLM attribute object to a (value, confidence, quote) triple

Both schemas wrap most attributes in
`{"value": ..., "confidence": ..., "source_quote": "..."}`. This helper
unpacks one such entry, returning `NULL` if the field is absent or null
(so callers can skip it cleanly).

## Usage

``` r
unpack_vlm_attr(x)
```

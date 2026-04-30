# Strip surrounding code fences from a model response

Some models wrap JSON output in ```` ```json ... ``` ```` fences despite
being told not to. This helper removes a single leading and trailing
fence pair if present, leaving the inner content.

## Usage

``` r
strip_code_fence(text)
```

# Load and render a packaged prompt template

Reads `inst/prompts/<name>.md` as UTF-8 and substitutes `{varname}`
placeholders with values from `vars`. The substitution is intentionally
simple (literal string replacement, no escaping, no logic) – the prompt
templates are author-curated and the only callers are internal
extraction functions.

## Usage

``` r
load_prompt(name, vars = list())
```

## Arguments

- name:

  Template base name, e.g. `"extract_horizons"`.

- vars:

  Named list of substitution values. Each value is coerced to character
  via `as.character`.

## Value

Character scalar with the rendered prompt.

## Details

Unknown placeholders (i.e. `{foo}` appearing in the template without a
matching entry in `vars`) are left as-is, which makes typos visible at
runtime in the rendered prompt.

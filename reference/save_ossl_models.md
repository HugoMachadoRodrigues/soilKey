# Save / load trained OSSL-backed PLSR models

Thin wrappers around `saveRDS` / `readRDS` that also verify the
deserialised object's shape. The on-disk file carries the soilKey
version, training time and preprocess label as attributes;
`load_ossl_models` preserves them.

## Usage

``` r
save_ossl_models(models, path)

load_ossl_models(path)
```

## Arguments

- models:

  Output of
  [`train_pls_from_ossl`](https://hugomachadorodrigues.github.io/soilKey/reference/train_pls_from_ossl.md).

- path:

  File path. Use `.rds` or `.RData` as the suffix (saveRDS is used
  regardless).

## Value

`save_ossl_models()` returns `path` invisibly. `load_ossl_models()`
returns the model list.

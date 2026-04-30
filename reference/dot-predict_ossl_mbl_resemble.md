# MBL via resemble::mbl

Wraps
[`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html)
so that the public predict_ossl\_\* wrappers stay short. Returns a
data.table with the canonical schema. Only invoked when both `resemble`
and a populated `ossl_library` are present.

## Usage

``` r
.predict_ossl_mbl_resemble(X, properties, k, ossl_library, ...)
```

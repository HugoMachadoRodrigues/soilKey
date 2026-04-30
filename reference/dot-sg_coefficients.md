# Compute Savitzky-Golay coefficients for a derivative

Solves the standard SG least-squares system to derive the kernel
coefficients for a given window `w`, polynomial `p`, and derivative
order `m`. Used only when `prospectr` is unavailable.

## Usage

``` r
.sg_coefficients(w, p, m)
```

# Kr (silica:sesquioxidos molar) – SiBCS Cap 1, p 32

Calcula o indice molar Kr = SiO2 / (Al2O3 + Fe2O3) usando massas molares
60.08 (SiO2), 101.96 (Al2O3) e 159.69 (Fe2O3):

## Usage

``` r
compute_kr(sio2_pct, al2o3_pct, fe2o3_pct)
```

## Arguments

- sio2_pct:

  Teor de SiO2 por ataque sulfurico (%).

- al2o3_pct:

  Teor de Al2O3 por ataque sulfurico (%).

- fe2o3_pct:

  Teor de Fe2O3 por ataque sulfurico (%).

## Value

Kr molar (numeric); NA se algum input for NA ou denominador \\\le\\ 0.

## Details

Kr (molar) = (% SiO2 / 60.08) / (% Al2O3 / 101.96 + % Fe2O3 / 159.69)

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 32.

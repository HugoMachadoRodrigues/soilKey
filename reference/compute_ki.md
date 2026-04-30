# Ki (silica:alumina molar) – SiBCS Cap 1, p 32

Calcula o indice molar Ki = SiO2 / Al2O3 a partir de teores percentuais
por ataque sulfurico-NaOH (Embrapa Manual de Metodos). Massas molares:
60.08 (SiO2), 101.96 (Al2O3):

## Usage

``` r
compute_ki(sio2_pct, al2o3_pct)
```

## Arguments

- sio2_pct:

  Teor de SiO2 por ataque sulfurico (%).

- al2o3_pct:

  Teor de Al2O3 por ataque sulfurico (%).

## Value

Ki molar (numeric); NA se algum input for NA ou Al2O3 \\\le\\ 0.

## Details

Ki (molar) = (% SiO2 / 60.08) / (% Al2O3 / 101.96) \\\approx\\ 1.6973
\\\times\\ (% SiO2 / % Al2O3)

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 32; Embrapa Manual de Metodos de
Analise de Solo (3a ed., 2017).

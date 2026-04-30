# Aceric qualifier (ae): pH (1:1 H2O) \<= 5 in some layer within the upper 50 cm. Used for sub-aerially exposed acid-sulfate soils (Solonchaks, Gleysols on former tidal flats). v0.9.1: numeric pH gate only; v0.9.2 adds the cross-check against `thionic` / sulfidic material to disambiguate from naturally acidic Histosols.

Aceric qualifier (ae): pH (1:1 H2O) \<= 5 in some layer within the upper
50 cm. Used for sub-aerially exposed acid-sulfate soils (Solonchaks,
Gleysols on former tidal flats). v0.9.1: numeric pH gate only; v0.9.2
adds the cross-check against `thionic` / sulfidic material to
disambiguate from naturally acidic Histosols.

## Usage

``` r
qual_aceric(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

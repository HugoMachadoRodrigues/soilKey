# Soil climate of the Canadian system from climate normals

Estimates, from monthly climate normals (as returned by
[`fetch_aafc_normals`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_aafc_normals.md)),
what the Canadian System of Soil Classification reads as the soil
climate (Ch 2, p. 16, criterion 7 of the chernozemic A; the soil
temperature classes and moisture subclasses of its Tables 3 and 4, after
the map Soil Climates of Canada, Clayton et al. 1977):

- the mean annual air temperature (MAAT, the mean of the twelve monthly
  means) and, for the mean annual soil temperature (MAST), the range
  from the MAAT to the MAAT plus `mast_offset` degrees: in Canada the
  soil under snow is warmer than the air;

- the climatic moisture index (CMI) of Table 4, the growing-season
  precipitation as a percentage of the water crops would use: here the
  precipitation over the potential evapotranspiration (PET) of the
  months of the growing season (April to October) whose mean temperature
  is above 5 degrees. The PET is computed in two ways: by Thornthwaite
  (monthly mean temperatures and day length), the central estimate,
  which matches the zones the book gives the Chernozem great groups on
  the NPDB (Brown subarid to semiarid, Dark Brown semiarid, Black and
  Dark Gray subhumid); and by Hargreaves and Samani (monthly minimum and
  maximum temperatures and extraterrestrial radiation), the other end of
  the range (on the NPDB mostly drier, sometimes wetter);

- the moisture subclass from each index (Table 4): perhumid above 84,
  humid 74-84, subhumid 59-73, semiarid 46-58, subarid 25-45, arid below
  25.

## Usage

``` r
npdb_soil_climate(normals, mast_offset = c(0, 4))
```

## Arguments

- normals:

  A data frame with `id`, `lat` and the columns of
  [`fetch_aafc_normals`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_aafc_normals.md).

- mast_offset:

  The range added to the MAAT for the MAST, in degrees (two numbers,
  read from the lower to the higher).

## Value

A data frame: `id`, `maat`, `mast_lo`, `mast_hi`, `p_gs` (mm),
`gs_months`, `pet_gs` and `cmi` and `moisture_subclass` (Thornthwaite),
`pet_gs_hargreaves`, `cmi_hargreaves` and
`moisture_subclass_hargreaves`.

## References

Soil Classification Working Group (1998). The Canadian System of Soil
Classification, 3rd ed., Tables 3 and 4. Clayton, J.S. et al. (1977).
Soils of Canada. Thornthwaite, C.W. (1948). Geographical Review 38:
55-94. Hargreaves, G.H., Samani, Z.A. (1985). Applied Engineering in
Agriculture 1: 96-99. Allen, R.G. et al. (1998). FAO Irrigation and
Drainage Paper 56.

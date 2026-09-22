# Turn one WoSIS profile row into a PedonRecord

The licence and source dataset are carried onto the record's site
metadata, so that attribution survives into anything the user later
exports. CC BY requires it, and an extract that drops it turns a
licensed use into an unattributed one.

## Usage

``` r
wosis_profile_to_pedon(row)
```

## Arguments

- row:

  One row of
  [`read_wosis_profiles_graphql`](https://hugomachadorodrigues.github.io/soilKey/reference/read_wosis_profiles_graphql.md).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
with site metadata and no horizons; the profile's layers are not fetched
here.

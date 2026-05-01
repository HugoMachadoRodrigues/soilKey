# Test for anthric / pretic / hortic / plaggic / terric / irragric horizon properties (full diagnostic)

WRB 2022 specifies five anthropogenic surface horizons that are all
diagnostic for Anthrosols. Rather than relying on the designation
pattern alone, this predicate also checks property-based evidence: a
surface horizon (top_cm \<= 5) with elevated dark colour (Munsell value
\<= 4 moist) AND elevated plant-available P (`p_mehlich3_mg_kg` \>= 50)
AND minimum thickness 20 cm. Either path (designation OR property-based)
qualifies.

## Usage

``` r
test_anthric_horizon_properties(
  h,
  min_thickness_cm = 20,
  min_p_mg_kg = 50,
  max_munsell_value = 4
)
```

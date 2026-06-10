# Assemble the USDA family label from family attributes

Joins the non-NULL `value`s with ", " in canonical USDA order
(particle-size, mineralogy, CEC-activity, reaction, temperature, depth).
The string is meant to be PREPENDED to the subgroup name.

## Usage

``` r
family_label_usda(family)
```

## Arguments

- family:

  Named list of
  [`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md),
  the return of
  [`classify_usda_family`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda_family.md).

## Value

Single string (possibly empty).

# Mollic epipedon (USDA Soil Taxonomy, 13th edition)

A thick, dark-colored, base-rich mineral surface horizon. The principal
diagnostic horizon of the Mollisols order; also qualifies many subgroups
of other orders as "Mollic" or "Pachic".

## Usage

``` r
mollic_epipedon_usda(pedon, min_bs = 50, min_oc_pct = 0.6)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_bs:

  Default 50 percent.

- min_oc_pct:

  Default 0.6 percent.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed required characteristics (Ch. 3, pp 15-17):

- Color: dominant color value \<= 3 (moist) AND \<= 5 (dry) AND chroma
  \<= 3 (moist), with adjustments for CaCO3 content (deferred to v0.9);

- Base saturation (NH4OAc, pH 7) \>= 50 percent throughout;

- Organic carbon \>= 0.6 percent (or 2.5 percent if value is 4-5 moist;
  or 0.6 absolute \> C horizon);

- Thickness: 18 cm general, 25 cm if texture is loamy fine sand or
  coarser, 10 cm if directly above lithic/densic/ paralithic contact
  (`thin_lithic_overlay` branch);

- Structure: peds \<= 30 cm OR rupture-resistance \<= moderately hard;

- Some part moist 90+ days when soil temp at 50 cm is \>= 5 C (deferred
  – requires climatic data).

Implementation notes (v0.8.x):

- Thickness rule is computed dynamically based on texture and presence
  of underlying lithic/paralithic contact.

- N value \< 0.7 / fluidity nonfluid is assumed (laboratory tests rarely
  available);

- 90-day moisture condition is deferred to v0.9.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-17.

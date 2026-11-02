# Trunks and divisions of the Russian soil classification (internal)

The predicates of the keys to trunks and divisions of the Russian soil
classification (Field Guide of 2008, keys 2.2 and 2.3), read by
[`classify_kidpr2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_kidpr2008.md)
through the flattened key of `inst/rules/kidpr2008/key.yaml` and
`divisions/`. They read the identified horizons of the profile
([`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md):
the Russian notation where the profile is written in it, else key 2.1 on
the data) and a few site facts. Each returns a
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md):
TRUE, FALSE, or NA when an undecided layer or a missing record (a depth
among them) could change the answer.

## Usage

``` r
has_horizon_kidpr(pedon, codes, within_cm = NULL, buried = FALSE)

starts_at_surface_kidpr(pedon, codes, from_ground_zero = FALSE)

surface_horizon_kidpr(pedon, codes)

directly_over_kidpr(pedon, upper, lower, gap_cm = 0)

below_kidpr(pedon, upper, lower)

only_horizons_kidpr(pedon, sets, raw = FALSE)

rock_within_kidpr(pedon, max_cm = 30)

turbated_kidpr(pedon, min_cm = 40)

peat_body_kidpr(pedon, min_cm = 50)

surface_deposit_kidpr(pedon, min_cm = 40, humified = NULL)

stratified_body_kidpr(pedon, min_cm = 40)

soil_horizon_within_kidpr(pedon, max_cm = 100)

floodplain_kidpr(pedon)

volcanic_ash_kidpr(pedon)

contaminated_x_kidpr(pedon, within_cm = 30)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- codes, upper, lower:

  Russian horizon codes (`"BT"`, `"G"` ...).

- within_cm:

  A layer starting less than this depth below the ground (NULL: any
  depth); a layer of unknown depth leaves it undecided.

- buried:

  Also read the horizons of buried soils.

- from_ground_zero:

  For `starts_at_surface_kidpr`: a TRUE answer stands only when the
  description starts at 0 cm (fresh litter included), else it is
  undecided (a truncated soil, Hugo's decision Q19).

- gap_cm:

  Thickness of layers with no diagnostic horizon allowed between `upper`
  and `lower`.

- sets:

  A list of code vectors: the profile's horizons must be exactly one of
  them.

- raw:

  For `only_horizons_kidpr`: read every horizon of the profile, before
  the thickest-horizon rule of the surface.

- max_cm, min_cm:

  Depth or thickness limits (see each predicate).

- humified:

  For `surface_deposit_kidpr`: TRUE a deposit with RJ, RY or RU, FALSE
  one without, NULL either.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

The profile is read top-down from the ground. A deposit at the surface
(`surface_deposit_kidpr`) is the run of technogenic formations (TPO),
deposits (horizon key position 38b), stratified humus horizons (RJ, RY,
RU), small deposited layers (r, rh, ur, rr, rx) and loose rock over a
buried soil or an organic or humus horizon; under a deposit of 40 cm or
less the soil below is the one keyed. Sub-horizons are merged, small
horizons and transitions left out (the FAO and WRB transitions AB, BC,
E/B ... too, when the key on the data finds no diagnostic horizon in
them); of the organic, humus and agro horizons that start the profile
the agro horizon is kept and, of the natural ones, the thickest (the
others are features; a W of 5 cm or less over another horizon is the
feature w).

- `has_horizon_kidpr`:

  One of the horizons is in the profile (an overlay S or X counts; a V
  laid over another horizon is a feature of its layer).

- `starts_at_surface_kidpr`:

  The first horizon of the profile (at the surface, or under a deposit
  of 40 cm or less) is one of the horizons.

- `surface_horizon_kidpr`:

  The surface horizon (the agro horizon, else the thickest natural
  organic or humus horizon that starts the profile) is one of the
  horizons.

- `directly_over_kidpr`:

  One of the `upper` horizons lies directly over one of the `lower` ones
  (transitions and small horizons between them do not count; layers with
  no diagnostic horizon up to `gap_cm` thick are passed over).

- `below_kidpr`:

  One of the `lower` horizons lies somewhere below one of the `upper`
  ones.

- `only_horizons_kidpr`:

  The horizons of the profile (rock and parent material left out), read
  after the thickest-horizon rule, are exactly one of the `sets`; with
  `raw = TRUE` read before it.

- `rock_within_kidpr`:

  Hard rock (R) starts within `max_cm` of the soil surface, organic
  horizons included (key C.1).

- `turbated_kidpr`:

  A turbated horizon TUR more than `min_cm` thick (Hugo's decision Q20).

- `peat_body_kidpr`:

  A peat body of more than `min_cm`: the organic layers contiguous from
  the surface (or under a deposit of 40 cm or less), muck and the
  agro-peat horizons included (key 2.2, step 6; Hugo's decision Q22); a
  layer the data leave undecided between peat horizons counts. A
  description that ends in peat short of it leaves it undecided.

- `surface_deposit_kidpr`:

  A deposit at the surface (key 2.2, steps 2-4; Hugo's decision Q21)
  more than `min_cm` thick, with (`humified = TRUE`) or without
  (`FALSE`) a stratified humus horizon RJ, RY or RU.

- `stratified_body_kidpr`:

  A stratified humus body from the surface, the agro horizons P and PU
  over RJ, RY or RU counted with them, of `min_cm` or more (the
  stratozems, B.2; Q21).

- `soil_horizon_within_kidpr`:

  A soil horizon (of the profile or of a buried soil) starts within
  `max_cm` of the ground (key 2.2, step 1). A layer the data leave
  undecided between soil horizons is one; a layer in which the key on
  the data finds no diagnostic horizon (position 66) proves nothing,
  since it may be a transition; only rock, TPO and the deposit of
  position 38b rule a soil horizon out.

- `floodplain_kidpr`:

  The soil lies within a river floodplain on modern alluvium (B.1;
  Hugo's decision Q21): a floodplain recorded (landform or landform
  element; a terrace is none) and alluvial material (the alluvial mark
  of the notation, or a fluvial or lacustrine origin of a layer or of
  the parent material; marine and glaciofluvial material and outwash do
  not count); with no landform recorded, alluvial material that
  soilKey's fluvic material confirms; alluvial material alone leaves it
  undecided. Nothing recorded: not a floodplain soil.

- `volcanic_ash_kidpr`:

  The soil is developed in volcanic ash: the ash mark of the notation,
  an ash, tephra or pyroclastic origin of a layer or of the parent
  material, or 30 no ash. The key does not read it: the ochre horizon
  BAN is formed in ash by its definition, so BAN alone makes the
  volcanic soils (Hugo's decision of 2026-11-02).

- `contaminated_x_kidpr`:

  The chemically contaminated horizon X in a layer starting within
  `within_cm` (C.3; Hugo's decision Q23): an X of the profile, or
  pollutants of the extremely hazardous category
  (`contamination_hazard`); contamination recorded without its category
  (`site$contaminated`, `contamination_type`) leaves it undecided;
  nothing recorded, no X.

# Extracted from test-sibcs-v07.R:150

# test -------------------------------------------------------------------------
expected <- list(
    O = "Organossolos",
    R = "Neossolos",
    V = "Vertissolos",
    E = "Espodossolos",
    S = "Planossolos",
    G = "Gleissolos",
    L = "Latossolos",
    M = "Chernossolos",
    C = "Cambissolos",
    F = "Plintossolos",
    T = "Luvissolos",
    N = "Nitossolos",
    P = "Argissolos"
  )
fixfns <- list(
    O = make_organossolo_canonical,
    R = make_neossolo_canonical,
    V = make_vertissolo_canonical,
    E = make_espodossolo_canonical,
    S = make_planossolo_canonical,
    G = make_gleissolo_canonical,
    L = make_latossolo_canonical,
    M = make_chernossolo_canonical,
    C = make_cambissolo_canonical,
    F = make_plintossolo_canonical,
    T = make_luvissolo_canonical,
    N = make_nitossolo_canonical,
    P = make_argissolo_canonical
  )
for (code in names(fixfns)) {
    pr <- fixfns[[code]]()
    res <- classify_sibcs(pr, on_missing = "silent")
    expect_equal(res$rsg_or_order, expected[[code]],
                 info = sprintf("fixture %s expected %s, got %s",
                                  code, expected[[code]], res$rsg_or_order))
  }

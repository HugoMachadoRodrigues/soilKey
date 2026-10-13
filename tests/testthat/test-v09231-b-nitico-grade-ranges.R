# v0.9.231: in the B nitico, a grade written as a range is read by its lower
# end.
#
# SiBCS 2018 (Cap 2, p. 61-62) asks of a B nitico a structure "de grau de
# desenvolvimento moderado ou forte" and cerosidade "com grau forte ou
# moderado". Both are minima, and a range meets a minimum only if all of it
# does. The grades were read with the pattern "moder|strong|forte", which
# "Fraca a moderada" matches, so a grade that reaches "moderada" only at its
# upper end passed. The amount of cerosidade was already read by its lower end
# since v0.9.227 ("Pouca a comum" is below "comum").
#
# The clay-film gate of the B latossolico is the other kind of criterion, a
# maximum ("no maximo cerosidade pouca e fraca", p. 58-59): there a range
# counts by its upper end, and "Pouca a comum" excludes the B latossolico. It
# is left as it was.

.with_grades <- function(p, structure = NULL, films = NULL) {
  q <- p$clone(deep = TRUE)
  b <- which(grepl("^B", q$horizons$designation))
  if (!is.null(structure)) q$horizons$structure_grade[b] <- structure
  if (!is.null(films)) {
    if (!"clay_films_strength" %in% names(q$horizons))
      q$horizons$clay_films_strength <- NA_character_
    q$horizons$clay_films_strength[b] <- films
  }
  q
}

test_that(".field_grade_class() reads a grade in both languages, and a range by its lower end", {
  grade <- c("Fraca", "Moderada", "Forte", "weak", "moderate", "strong",
             "MODERADA", "Muito forte", "very weak")
  expect_equal(.field_grade_class(grade), c(1L, 2L, 3L, 1L, 2L, 3L, 2L, 3L, 1L))
  expect_equal(.field_grade_class(c("Fraca a moderada", "Moderada a forte",
                              "weak to moderate", "moderate to strong")),
               c(1L, 2L, 1L, 2L))
  # not a grade, or not recorded
  expect_true(all(is.na(.field_grade_class(c("massive", "Maciça", "", NA)))))
})

test_that("a structure grade that only reaches 'moderada' is not 'moderada ou forte'", {
  p <- make_nitossolo_canonical()
  expect_true(isTRUE(B_nitico(p)$passed))
  for (ok in c("Moderada", "Forte", "Moderada a forte", "moderate", "strong",
               "moderate to strong"))
    expect_true(B_nitico(.with_grades(p, structure = ok))$evidence$structure_ok, info = ok)
  for (low in c("Fraca", "Fraca a moderada", "weak", "weak to moderate"))
    expect_false(B_nitico(.with_grades(p, structure = low))$evidence$structure_ok, info = low)
  # a grade that was not recorded does not fail the layer, as before
  expect_true(B_nitico(.with_grades(p, structure = NA_character_))$evidence$structure_ok)
})

test_that("the same holds for the grade of the cerosidade", {
  p <- make_nitossolo_canonical()
  for (ok in c("Moderada", "Forte", "Moderada a forte", "moderate", "strong"))
    expect_true(B_nitico(.with_grades(p, films = ok))$evidence$cerosidade_ok, info = ok)
  for (low in c("Fraca", "Fraca a moderada", "weak", "weak to moderate"))
    expect_false(B_nitico(.with_grades(p, films = low))$evidence$cerosidade_ok, info = low)
  expect_true(B_nitico(.with_grades(p, films = NA_character_))$evidence$cerosidade_ok)
  # the profile is then no Nitossolo
  weak <- .with_grades(p, films = "Fraca a moderada")
  expect_false(isTRUE(B_nitico(weak)$passed))
  expect_false(identical(classify_sibcs(weak, on_missing = "silent")$rsg_or_order,
                         "Nitossolos"))
})

test_that("a range of amounts still excludes the B latossolico by its upper end", {
  # a maximum, not a minimum: "no maximo cerosidade pouca e fraca"
  expect_true(.argic_strong_films_match("Pouca a comum"))
  expect_true(.argic_strong_films_match("Comum a abundante"))
  expect_false(.argic_strong_films_match("Pouca"))
})

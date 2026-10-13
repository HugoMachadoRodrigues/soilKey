# v0.9.240: the "(B)" of the horizon notation in use in Brazil until 1988 is
# read as the B incipiente it names.
#
# Until Larach et al. (1988, "Definicao e notacao de horizontes e camadas do
# solo", the reference of SiBCS 2018 for the notation) the B incipiente was
# written "(B)": "A (B) C", "A1 (B)1 (B)2 C", "A II(B)21 II(B)22". It is "Bi"
# since. B_incipiente() takes its candidate layers from the designation
# ("Bi", "Bw", ...), and .desg_master(), which every SiBCS test of the master
# horizon reads, left "(B)" as written: not a B at all. Of the 640 profiles of
# the national BDsolos export whose reference order is Cambissolos, 294 carry
# the old notation, and the key found no diagnostic B in them: Neossolos.

.old_cambissolo <- function(designation) {
  p <- make_cambissolo_canonical()          # A, Bw, BC, C
  p$horizons$designation <- designation
  p
}

test_that(".desg_master() reads '(B)' as 'Bi', whatever the numbers around it", {
  d <- c("(B)", "(B)1", "(B)21", "(B1)", "(B22)", "(B) 1", "(B)2cn", "(B)/C",
         "II(B)22", "II(B1)", "III(B2)", "2(B)")
  expect_equal(.desg_master(d),
               c("Bi", "Bi1", "Bi21", "Bi1", "Bi22", "Bi1", "Bi2cn", "Bi/C",
                 "Bi22", "Bi1", "Bi2", "Bi"))
  # the horizon data keep the designation as written
  p <- .old_cambissolo(c("A", "(B)1", "(B)2", "C"))
  expect_equal(p$horizons$designation, c("A", "(B)1", "(B)2", "C"))
})

test_that("what was read before is read the same", {
  d <- c("Bt", "2Bt1", "IIBt1", "Bw", "Bi", "B21", "A", "IIC", "Cr", NA, "")
  expect_equal(.desg_master(d),
               c("Bt", "Bt1", "Bt1", "Bw", "Bi", "B21", "A", "C", "Cr", NA, ""))
  # a parenthesis that does not open the designation is left as written
  odd <- c("A3/(B)", "B(2)", "Eh (Bh)", "Bw3 (?)", "(b)")
  expect_equal(.desg_master(odd), odd)
})

test_that("a profile described with '(B)' has a B incipiente and is a Cambissolo", {
  canonical <- classify_sibcs(make_cambissolo_canonical(), on_missing = "silent")$name
  for (d in list(c("A", "(B)", "(B)3", "C"),
                 c("A1", "(B)1", "(B)2", "C"),
                 c("A", "(B1)", "(B2)", "C"),
                 c("A1", "II(B)21", "II(B)22", "IIC"))) {
    p <- .old_cambissolo(d)
    bi <- B_incipiente(p)
    expect_true(isTRUE(bi$passed), info = paste(d, collapse = " "))
    expect_true(2L %in% bi$layers, info = paste(d, collapse = " "))
    expect_false(isTRUE(neossolo(p)$passed), info = paste(d, collapse = " "))
    expect_equal(classify_sibcs(p, on_missing = "silent")$name, canonical,
                 info = paste(d, collapse = " "))
  }
})

test_that("the letter is not the horizon: the other requirements are asked as before", {
  # thinner than 10 cm
  thin <- .old_cambissolo(c("A", "(B)", "C1", "C2"))
  thin$horizons$top_cm    <- c(0, 15, 23, 100)
  thin$horizons$bottom_cm <- c(15, 23, 100, 150)
  expect_false(isTRUE(B_incipiente(thin)$passed))
  # a "(B)" that the data show to be a B latossolico is one: the canonical
  # Latossolo with its Bw written "(B)" keeps its class
  lat <- make_latossolo_canonical()
  name <- classify_sibcs(lat, on_missing = "silent")$name
  lat$horizons$designation <- sub("^Bw", "(B)", lat$horizons$designation)
  expect_true(any(grepl("^\\(B\\)", lat$horizons$designation)))
  expect_true(isTRUE(B_latossolico(lat)$passed))
  expect_equal(classify_sibcs(lat, on_missing = "silent")$name, name)
})

test_that("the reading is the same in the morphological mode", {
  old <- options(soilKey.morphological_inference = TRUE)
  on.exit(options(old), add = TRUE)
  p <- .old_cambissolo(c("A1", "(B)1", "(B)2", "C"))
  expect_true(isTRUE(B_incipiente(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Cambissolos")
})

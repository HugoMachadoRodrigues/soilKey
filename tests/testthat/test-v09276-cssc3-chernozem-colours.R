# v0.9.276: the colour of the Chernozemic great groups at the limits the key
# words with "usually": a dry value of 3.5 with chroma 1.5 or less is Black,
# and a cultivated A (an Ap) of chroma 1.5 or less shows the eluviation of
# the Dark Gray Chernozems.

.gg_rules <- function() {
  r <- load_rules("cssc3")
  r$subgroups <- NULL
  r
}
.chern_gg <- function(d1, value, chroma) {
  p <- make_cssc3_example("chernozemic")
  p$horizons$designation[1] <- d1
  p$horizons$munsell_value_dry[1] <- value
  p$horizons$munsell_chroma_dry[1] <- chroma
  classify_cssc3(p, rules = .gg_rules(), on_missing = "silent")
}

test_that("a dry value of 3.5 is Black with the chroma of the Black", {
  skip_on_cran()
  expect_identical(.chern_gg("Ah", 3.5, 1)$name, "Black Chernozem")
  expect_identical(.chern_gg("Ah", 3.5, 1.5)$name, "Black Chernozem")
  expect_identical(.chern_gg("Ah", 3.5, 2)$name, "Dark Brown Chernozem")
  expect_identical(.chern_gg("Ah", 3.4, 3)$name, "Black Chernozem")      # below 3.5: any chroma
  expect_identical(.chern_gg("Ah", 4, 1)$name, "Dark Brown Chernozem")   # 4, no eluviation
  # an unknown chroma at 3.5 leaves the great group undecided
  r <- .chern_gg("Ah", 3.5, NA)
  expect_true("great_groups:GB" %in% r$provisional_because)
})

test_that("a cultivated A of low chroma shows the eluviation of the Dark Gray", {
  skip_on_cran()
  expect_identical(.chern_gg("Ap", 4, 1.5)$name, "Dark Gray Chernozem")
  expect_identical(.chern_gg("Ap", 4, 2)$name, "Dark Brown Chernozem")
  expect_identical(.chern_gg("Ah", 4, 1.5)$name, "Dark Brown Chernozem")  # an Ah still needs an Ae
  expect_identical(.chern_gg("Ahe", 4, 1.5)$name, "Dark Gray Chernozem")
  expect_identical(.chern_gg("Ap", 3.5, 1)$name, "Black Chernozem")      # Black comes first
  # an Ap of unknown chroma: undecided between Dark Brown and Dark Gray
  r <- .chern_gg("Ap", 4, NA)
  expect_true("great_groups:GB" %in% r$provisional_because)
  cc <- soilKey:::.cssc_chernozemic_colour(soilKey:::.cssc_prepare_pedon({
    p <- make_cssc3_example("chernozemic"); p$horizons$designation[1] <- "Ap"
    p$horizons$munsell_chroma_dry[1] <- 1; p }))
  expect_true(cc$eluviated && cc$ap)
})

test_that("review fixes: an Ahe at 3.5, the Ap's own chroma, means at the limit, a buried Ae", {
  skip_on_cran()
  # an Ahe keeps the value 3.5 for the Dark Gray, whose range holds it
  expect_identical(.chern_gg("Ahe", 3.5, 1)$name, "Dark Gray Chernozem")
  # the chroma of the Ap itself, not of the whole A
  two_a <- function(ap_chroma) {
    p <- make_cssc3_example("chernozemic")
    p$horizons$designation <- c("Ap", "Ah", "Ck")
    p$horizons$top_cm <- c(0, 10, 30); p$horizons$bottom_cm <- c(10, 30, 90)
    p$horizons$munsell_value_moist <- c(2, 2, 5)
    p$horizons$munsell_value_dry <- c(4, 4, 6); p$horizons$munsell_chroma_dry <- c(ap_chroma, 1, 3)
    classify_cssc3(p, rules = .gg_rules(), on_missing = "silent")
  }
  expect_identical(two_a(2)$name, "Dark Brown Chernozem")
  expect_identical(two_a(1)$name, "Dark Gray Chernozem")
  expect_true("great_groups:GB" %in% two_a(NA)$provisional_because)
  # a mean that is 3.5 but for rounding is 3.5
  mean_a <- function(v1, v2, t2, chroma) {
    p <- make_cssc3_example("chernozemic")
    p$horizons$designation <- c("Ah1", "Ah2", "Ck")
    p$horizons$top_cm <- c(0, t2, 34); p$horizons$bottom_cm <- c(t2, 34, 90)
    p$horizons$munsell_value_moist <- c(2, 2, 5)
    p$horizons$munsell_value_dry <- c(v1, v2, 6); p$horizons$munsell_chroma_dry <- c(chroma, chroma, 3)
    classify_cssc3(p, rules = .gg_rules(), on_missing = "silent")$name
  }
  expect_identical(mean_a(2.9, 4.1, 17, 2), "Dark Brown Chernozem")
  expect_identical(mean_a(2.9, 4.1, 17, 1), "Black Chernozem")
  # a buried Ae is not the eluviation of this soil
  p <- make_cssc3_example("chernozemic")
  h <- p$horizons
  extra <- data.table::copy(h[3L, ]); extra$designation <- "Aeb"; extra$top_cm <- 40; extra$bottom_cm <- 55
  h <- rbind(h[1:2, ], extra, h[3L, ]); h$top_cm[4] <- 55
  h$munsell_value_dry[1] <- 4; h$munsell_chroma_dry[1] <- 1
  p$horizons <- h
  expect_identical(classify_cssc3(p, rules = .gg_rules(), on_missing = "silent")$name, "Dark Brown Chernozem")
  p$horizons$designation[3] <- "Ae"
  expect_identical(classify_cssc3(p, rules = .gg_rules(), on_missing = "silent")$name, "Dark Gray Chernozem")
})

# v0.9.196: photo extraction merges into the horizons the user already has.
#
# find_or_append_horizon() recognised an existing row only when BOTH boundaries
# agreed within 1 cm. That is right for a description PDF, where the depths are
# READ. A model looking at a photograph ESTIMATES them from pixels and cannot
# know the surveyor's measured boundaries, so the rule essentially never held:
# every extracted band was appended as a duplicate, leaving the user's own
# horizons colourless beside a second, overlapping set that carried the colour
# (3 horizons in, 6 out, none of the originals coloured).
#
# The photo route now matches by depth overlap. The PDF route keeps the exact
# rule, so its fixtures stay byte-identical.

.pm_pedon <- function() {
  PedonRecord$new(
    site = list(id = "photo-match"),
    horizons = data.frame(designation = c("A", "Bt", "C"),
                          top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120)))
}

.pm_answer <- function(tops, bottoms) {
  hues <- c("10YR", "7.5YR", "5YR")
  paste0('{"horizons":[', paste(vapply(seq_along(tops), function(i) sprintf(
    '{"top_cm":%g,"bottom_cm":%g,"designation":"H%d","munsell_moist":{"hue":"%s","value":3,"chroma":2,"confidence":0.7}}',
    tops[i], bottoms[i], i, hues[(i - 1L) %% 3L + 1L]), character(1)),
    collapse = ","), ']}')
}

.pm_image <- function() {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 120, height = 240)
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  grDevices::dev.off()
  f
}


test_that("estimated depths merge into the user's horizons, not beside them", {
  skip_on_cran()
  pr <- .pm_pedon()
  # what a model actually returns from a photo: close, never within 1 cm
  res <- extract_munsell_from_photo(
    pr, .pm_image(),
    MockVLMProvider$new(responses = list(.pm_answer(c(0, 30, 65), c(30, 65, 120)))))
  h <- res$horizons
  expect_equal(nrow(h), 3L)                       # was 6 before the fix
  expect_equal(sum(!is.na(h$munsell_hue_moist)), 3L)
  # the surveyor's measured depths win; the model only contributes colour
  expect_equal(h$top_cm,    c(0, 25, 60))
  expect_equal(h$bottom_cm, c(25, 60, 120))
  expect_equal(h$designation, c("A", "Bt", "C"))
})

test_that("a band overlapping nothing is still appended", {
  skip_on_cran()
  # A photo showing material below the described profile is new information,
  # not a mis-match: it must not be silently folded into the deepest horizon.
  pr <- .pm_pedon()
  res <- extract_munsell_from_photo(
    pr, .pm_image(),
    MockVLMProvider$new(responses = list(.pm_answer(150, 200))))
  expect_equal(nrow(res$horizons), 4L)
  expect_equal(res$horizons$top_cm[4], 150)
})

test_that("a thin band inside a thick horizon matches it", {
  skip_on_cran()
  # Judged against the SHORTER interval, so 30-40 sits inside 25-60 rather
  # than failing a fraction-of-the-horizon test.
  pr <- .pm_pedon()
  res <- extract_munsell_from_photo(
    pr, .pm_image(),
    MockVLMProvider$new(responses = list(.pm_answer(30, 40))))
  expect_equal(nrow(res$horizons), 3L)
  expect_false(is.na(res$horizons$munsell_hue_moist[2]))   # the Bt
})

test_that("a band straddling two horizons picks the one it covers best", {
  skip_on_cran()
  pr <- .pm_pedon()
  # 20-55: 5 cm of A (of 25) vs 30 cm of Bt (of 35) -> Bt
  res <- extract_munsell_from_photo(
    pr, .pm_image(),
    MockVLMProvider$new(responses = list(.pm_answer(20, 55))))
  expect_equal(nrow(res$horizons), 3L)
  expect_false(is.na(res$horizons$munsell_hue_moist[2]))
  expect_true(is.na(res$horizons$munsell_hue_moist[1]))
})

test_that("the exact strategy is unchanged and stays the default", {
  skip_on_cran()
  # The PDF route depends on it: within 1 cm matches, beyond it appends.
  pr <- .pm_pedon()
  expect_equal(find_or_append_horizon(pr, 0, 25), 1L)          # default = exact
  expect_equal(find_or_append_horizon(pr, 25, 60, "exact"), 2L)
  expect_equal(find_or_append_horizon(pr, 0, 30, "exact"), 4L) # appends
  expect_equal(nrow(pr$horizons), 4L)
})

test_that("overlap matching needs a real overlap, not a touch", {
  skip_on_cran()
  pr <- .pm_pedon()
  # 0-25 and 25-60 share a boundary but no depth: must not match the A
  expect_equal(find_or_append_horizon(pr, 25, 60, "overlap"), 2L)
  # 55-60 is 5 of its own 5 cm inside Bt -> matches Bt
  expect_equal(find_or_append_horizon(pr, 55, 60, "overlap"), 2L)
  # nothing was appended by either
  expect_equal(nrow(pr$horizons), 3L)
})

test_that("an empty pedon still gets its horizons built from the photo", {
  skip_on_cran()
  pr <- PedonRecord$new(site = list(id = "empty"))
  res <- extract_munsell_from_photo(
    pr, .pm_image(),
    MockVLMProvider$new(responses = list(.pm_answer(c(0, 30), c(30, 70)))))
  expect_equal(nrow(res$horizons), 2L)
  expect_equal(res$horizons$top_cm, c(0, 30))
})

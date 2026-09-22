# v0.9.197: the continuous Munsell notation is delegated to
# munsellinterpol::MunsellNameFromHVC() where that function can reproduce it.
#
# soilKey spelled the notation out by hand because until munsellinterpol 3.5-0
# the canonical function could not match it: `digits` was one value applied to
# hue AND value AND chroma, and the achromatic test was a strict 0 < C with no
# tolerance -- which would have re-introduced the "10RP on a grey" leak fixed in
# v0.9.184. G. Davis addressed both (per-component `digits`, plus `ctol`) and
# 3.5-1 reached CRAN.
#
# munsellinterpol is a Suggests, so the hand-rolled spelling stays as the
# fallback. These tests pin the property that actually matters: the two are
# equivalent, so which one runs is invisible to the user.

.mn_fixtures <- function() {
  set <- rbind(
    expand.grid(H = c(5, 12.5, 25, 37.5, 50, 62.5, 75, 87.5),
                V = c(3, 5, 7), C = c(2, 6))[1:24, ],
    # the neutral edge cases: exact zero, a BLAS residual, a neutral sitting on
    # a chromatic hue number, and an ordinary chromatic sample
    data.frame(H = c(0, 0, 50, 93.7), V = c(2.5, 6, 4, 5.2),
               C = c(0, 1e-15, 0, 14)))
  hs <- munsellinterpol::HueStringFromNumber(set$H)
  neutral <- is.finite(set$C) & set$C < 1e-4
  hs[neutral] <- "N"
  set$C[neutral] <- 0
  list(H = set$H, V = set$V, C = set$C, hs = hs, neutral = neutral)
}

# The spelling soilKey used before v0.9.197, kept here as the reference.
.mn_handrolled <- function(d) {
  ifelse(is.na(d$hs), NA_character_,
         ifelse(d$neutral, sprintf("N %g/", d$V),
                sprintf("%s %g/%g", d$hs, d$V, d$C)))
}


test_that("the notation matches the hand-rolled spelling on every fixture", {
  skip_if_not_installed("munsellinterpol")
  d <- .mn_fixtures()
  expect_identical(
    soilKey:::.munsell_notation(d$H, d$V, d$C, d$hs, d$neutral),
    .mn_handrolled(d))
})

test_that("a neutral reports N and never a residual chroma", {
  skip_if_not_installed("munsellinterpol")
  # The v0.9.184 bug: a dark flat grey keys out at C ~ 1e-15 and used to be
  # spelled "10RP". Whichever path runs, it must read "N".
  for (cc in c(0, 1e-15, 7e-15, 5e-5)) {
    got <- soilKey:::.munsell_notation(0, 6, 0, "N", TRUE)
    expect_identical(got, "N 6/")
  }
  expect_identical(soilKey:::.munsell_notation(50, 4, 0, "N", TRUE), "N 4/")
})

test_that("a chromatic sample keeps hue at page precision", {
  skip_if_not_installed("munsellinterpol")
  # 4.33793R would be a widened hue, not a Munsell page notation -- the reason
  # the single-`digits` version was declined in v0.9.185.
  got <- soilKey:::.munsell_notation(93.7, 5.2, 14, "3.7RP", FALSE)
  expect_identical(got, "3.7RP 5.2/14")
  expect_false(grepl("[0-9]{3,}RP", got))
})

test_that("an NA hue yields NA, not a malformed string", {
  skip_if_not_installed("munsellinterpol")
  expect_true(is.na(
    soilKey:::.munsell_notation(NA_real_, 5, 2, NA_character_, FALSE)))
})

test_that("the delegation is used when munsellinterpol is new enough", {
  skip_if_not_installed("munsellinterpol")
  skip_if(utils::packageVersion("munsellinterpol") < "3.5.1",
          "munsellinterpol < 3.5.1: the fallback path is the one under test")
  # Belt and braces: call the canonical function directly with the same
  # arguments soilKey passes, and require it to agree.
  d <- .mn_fixtures()
  direct <- munsellinterpol::MunsellNameFromHVC(
    cbind(d$H, d$V, d$C), digits = c(2, 6, 6), ctol = 1e-4)
  expect_identical(
    soilKey:::.munsell_notation(d$H, d$V, d$C, d$hs, d$neutral),
    as.character(direct))
})

test_that("the achromatic boundary is treated the same by both paths", {
  skip_if_not_installed("munsellinterpol")
  # soilKey collapses at C < 1e-4 and munsellinterpol at ctol = 1e-4. Both
  # compare strictly, so C == 1e-4 is chromatic in both -- verified against the
  # CRAN build, and pinned here because a `<=` on either side would silently
  # move the neutral axis.
  cs <- c(0, 1e-15, 5e-5, 9.99e-5, 1e-4, 1.01e-4, 2e-4)
  for (cc in cs) {
    neutral <- cc < 1e-4
    hs <- if (neutral) "N" else "10G"
    got <- soilKey:::.munsell_notation(50, 5, if (neutral) 0 else cc, hs, neutral)
    expect_identical(got, if (neutral) "N 5/" else sprintf("10G 5/%g", cc))
  }
})

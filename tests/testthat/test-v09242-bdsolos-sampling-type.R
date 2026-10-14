# v0.9.242: the BDsolos loader reads the kind of sampling point, and the
# benchmark reports by it.
#
# The export marks each record, in its column "Tipo", as "Perfil completo",
# "Perfil parcial" or "Amostra extra". The loader did not read it. Of the 7,593
# records of the national export with a reference order, 4,674 are complete
# profiles, 332 partial profiles and 2,587 extra samples: in most cases two
# sampled layers of a point, often with a gap between them, whose thickness is
# that of the samples. One figure for the three mixed what the key does on a
# described profile with what it can do on two samples.

.st_csv <- function() {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "Tipo", "UF", "Classificação Atual",
           "Símbolo Horizonte", "Profundidade Superior", "Profundidade Inferior",
           "Composição Granulométrica da terra fina - Argila (g/kg)")
  row <- function(id, tipo, ref, hz, top, bot, clay) q(id, tipo, "MG", ref, hz, top, bot, clay)
  rows <- c(
    row("1", "Perfil completo",  "NEOSSOLO LITÓLICO", "A", "0", "20", "200"),
    row("1", "Perfil completo",  "NEOSSOLO LITÓLICO", "R", "20", "60", ""),
    row("2", "Perfil completo",  "LATOSSOLO VERMELHO",   "A", "0", "20", "400"),
    row("2", "Perfil completo",  "LATOSSOLO VERMELHO",   "C", "20", "90", "420"),
    row("3", "Amostra extra",    "NEOSSOLO REGOLÍTICO", "A", "0", "20", "150"),
    row("3", "Amostra extra",    "NEOSSOLO REGOLÍTICO", "C", "60", "80", "160"),
    row("4", "Amostra extra",    "LATOSSOLO AMARELO",    "A", "0", "20", "450"),
    row("4", "Amostra extra",    "LATOSSOLO AMARELO",    "C", "60", "80", "500"),
    row("5", "Perfil  parcial",  "NEOSSOLO FLÚVICO",    "A", "0", "30", "200"),
    row("5", "Perfil  parcial",  "NEOSSOLO FLÚVICO",    "C", "30", "90", "180"))
  f <- tempfile(fileext = ".csv")
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  f
}

.st_peds <- function() {
  f <- .st_csv(); on.exit(unlink(f), add = TRUE)
  suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
}

test_that("the loader reads the kind of sampling point as the export words it", {
  peds <- .st_peds()
  expect_length(peds, 5L)
  kinds <- vapply(peds, function(p) p$site$sampling_type, "")
  # the double space of "Perfil  parcial" in the export is one
  expect_equal(kinds, c("Perfil completo", "Perfil completo", "Amostra extra",
                        "Amostra extra", "Perfil parcial"))
})

test_that("a file with no 'Tipo' column gives no kind, and the benchmark counts every record", {
  skip_on_cran()
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior")
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr,
               q("9", "MG", "NEOSSOLO LITÓLICO", "A", "0", "20"),
               q("9", "MG", "NEOSSOLO LITÓLICO", "R", "20", "60")), f, useBytes = FALSE)
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
  expect_true(is.na(peds[[1]]$site$sampling_type))
  r <- suppressMessages(benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE))$per_system$sibcs
  expect_equal(r$n_compared, 1L)
  expect_null(r$by_sampling_type)
  expect_null(r$sampling_type)
})

test_that("the figures of the benchmark are those of the complete profiles, with the table by kind beside them", {
  skip_on_cran()
  peds <- .st_peds()
  r <- suppressMessages(benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE))$per_system$sibcs
  # two complete profiles: the Neossolo agrees, the "Latossolo" with no B does not
  expect_equal(r$sampling_type, "Perfil completo")
  expect_equal(r$n_compared, 2L)
  expect_equal(r$n_correct, 1L)
  expect_equal(r$accuracy, 0.5)
  expect_equal(sum(r$confusion), 2L)
  expect_equal(sum(r$per_class$n_ref), 2L)
  k <- r$by_sampling_type
  expect_equal(k$sampling_type, c("Amostra extra", "Perfil completo", "Perfil parcial", "all"))
  expect_equal(k$n_compared, c(2L, 2L, 1L, 5L))
  expect_equal(k$n_correct,  c(1L, 1L, 1L, 3L))
  expect_equal(k$accuracy, c(0.5, 0.5, 1, 0.6))
})

test_that("sampling_type = NULL counts every record, as before, and other kinds can be asked for", {
  skip_on_cran()
  peds <- .st_peds()
  a <- suppressMessages(benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE,
                                           sampling_type = NULL))$per_system$sibcs
  expect_null(a$sampling_type)
  expect_equal(a$n_compared, 5L)
  expect_equal(a$n_correct, 3L)
  expect_equal(sum(a$confusion), 5L)
  # the table by kind is the same whatever the main figure
  expect_equal(a$by_sampling_type$n_compared, c(2L, 2L, 1L, 5L))
  e <- suppressMessages(benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE,
                                           sampling_type = c("Amostra extra", "Perfil parcial")))$per_system$sibcs
  expect_equal(e$n_compared, 3L)
  expect_equal(e$n_correct, 2L)
  # the terms are matched whatever the case and the accents
  l <- suppressMessages(benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE,
                                           sampling_type = "perfil completo"))$per_system$sibcs
  expect_equal(l$n_compared, 2L)
})

test_that("pedons that carry no kind are all counted (another source, a hand-made list)", {
  skip_on_cran()
  p <- make_latossolo_canonical()
  p$site$reference_sibcs <- "LATOSSOLO VERMELHO Distrofico tipico"
  r <- suppressMessages(benchmark_bdsolos(list(p), systems = "sibcs", verbose = FALSE))$per_system$sibcs
  expect_equal(r$n_compared, 1L)
  expect_equal(r$n_correct, 1L)
  expect_null(r$by_sampling_type)
})

test_that("the panel of the suite is drawn from complete profiles", {
  skip_on_cran()
  d <- tempfile("bds"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(.st_csv(), file.path(d, "MG.csv"))
  r <- .benchmark_one_dataset_one_system(ds = "bdsolos", sys = "sibcs",
                                         paths = list(bdsolos = d), max_n = 800,
                                         verbose = FALSE)
  expect_equal(r$result$n_compared, 2L)
  expect_equal(r$result$sampling_type, "Perfil completo")
  # the extra samples and the partial profile did not reach the benchmark
  expect_equal(r$result$by_sampling_type$sampling_type, c("Perfil completo", "all"))
})

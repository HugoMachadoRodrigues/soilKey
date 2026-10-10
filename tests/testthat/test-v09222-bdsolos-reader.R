# v0.9.222: load_bdsolos_csv() reads every export with base R's reader.
#
# The BDsolos export quotes every field and ends each record with ";. Its
# free-text fields hold line breaks and, in 11 of the 27 state files of the
# national export, quotation marks of their own ('formacao "Camaqua"').
# data.table::fread(), which read the files until v0.9.221, then either
#   - took every line of text for a record, with a warning the loader
#     silenced: RS came out with 3,853 rows for its 897 records, and lines of
#     text became 411 profiles of their own (1,281 over BA, GO, PI and RS); or
#   - stopped with "attempt to set index N/N in SET_STRING_ELT" inside an
#     OpenMP critical section that stayed locked, so the next fread() of the
#     R session never returned (DF.csv, then any file; data.table 1.18.4).
# utils::read.csv2() returns the export's records in all 27 files.

# A BDsolos export in miniature: every field quoted, ";" after each, free
# text with line breaks, and quotation marks of its own in the profiles
# listed in `quoted`.
.bx_csv <- function(n_profiles, quoted = integer(0)) {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "Material de Origem", "Observações", "UF",
           "Símbolo Horizonte", "Profundidade Superior",
           "Profundidade Inferior", "pH - H2O", "Cerosidade - Quantidade")
  rows <- character(0)
  for (p in seq_len(n_profiles)) {
    id <- as.character(100L + p)
    mo <- if (p %in% quoted) 'Arenitos da formação "Camaquã".\n'
          else "Basalto"
    ob <- "- Perfil coletado úmido.\n- Atividade biológica no Ap."
    rows <- c(rows,
      q(id, mo, ob, "RS", "Ap", "0", "20", "5.5", ""),
      q(id, mo, ob, "RS", "Bt", "20", "60", "5.0", "comum"))
  }
  tf <- tempfile(fileext = ".csv")
  con <- file(tf, encoding = "UTF-8")
  writeLines(c("Dados obtidos a partir do BDSOLOS", "", hdr, rows), con)
  close(con)
  tf
}

test_that("a quotation mark inside a field does not split the records", {
  skip_on_cran()
  # 600 profiles of 2 horizons; profile 300 has the quotation marks, beyond
  # the rows fread() samples. Until v0.9.221 this file gave 602 profiles
  # ("- Atividade biologica no Ap." was one) and 2,402 horizons.
  tf <- .bx_csv(600L, quoted = 300L)
  on.exit(unlink(tf), add = TRUE)
  d <- .bdsolos_read_table(tf, sep = ";", skip = 2L)
  expect_equal(nrow(d), 1200L)
  expect_setequal(d[[1L]], 101:700)
  expect_equal(as.vector(table(d[[1L]])), rep(2L, 600L))
  # the text keeps its line breaks; the quotation marks themselves are dropped
  expect_equal(unique(d[[2L]][d[[1L]] == 400L]),
               "Arenitos da formação Camaquã.\n")
  expect_match(d[[3L]][1L], "úmido.\n- Atividade", fixed = TRUE)
})

test_that("the loader returns the export's profiles and horizons", {
  skip_on_cran()
  tf <- .bx_csv(3L, quoted = 2L)
  on.exit(unlink(tf), add = TRUE)
  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_equal(vapply(pedons, function(p) p$site$id, ""), c("101", "102", "103"))
  expect_equal(vapply(pedons, function(p) nrow(p$horizons), 0L), c(2L, 2L, 2L))
  expect_equal(pedons[[2L]]$site$parent_material,
               "Arenitos da formação Camaquã.")
  expect_equal(pedons[[2L]]$horizons$designation, c("Ap", "Bt"))
  expect_equal(pedons[[2L]]$horizons$ph_h2o, c(5.5, 5.0))
  # an empty text field is NA. fread() left it "" in the files it read (it
  # was NA only in the 7 states that fell back to read.csv2()).
  expect_equal(pedons[[1L]]$horizons$clay_films_amount, c(NA, "comum"))
  # and a second file in the same session loads too
  again <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(again, 3L)
})

test_that("the BDsolos loader does not call fread()", {
  for (f in c("load_bdsolos_csv", ".bdsolos_read_table")) {
    body <- paste(deparse(get(f, envir = asNamespace("soilKey"))), collapse = "\n")
    expect_false(grepl("fread", body, fixed = TRUE), info = f)
  }
  expect_match(paste(deparse(.bdsolos_read_table), collapse = "\n"),
               "read.csv2", fixed = TRUE)
})

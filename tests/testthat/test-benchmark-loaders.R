# Tests for the benchmark loaders. We can't ship the real KSSL / LUCAS /
# BDsolos exports (license, size), so each test builds a tiny synthetic
# CSV in tempdir() that mimics the field layout each loader expects.


test_that("load_kssl_pedons returns a list of PedonRecord with reference_usda attached", {
  td <- tempdir()
  pedon_csv <- file.path(td, "kssl_pedon.csv")
  layer_csv <- file.path(td, "kssl_layer.csv")
  write.csv(data.frame(
    pedon_key = c("P001", "P002"),
    latitude_decimal_degrees  = c(38.5, 39.7),
    longitude_decimal_degrees = c(-92.3, -90.1),
    country = c("US", "US"),
    parent_material = c("loess", "till"),
    taxonomic_subgroup = c("Typic Argiudoll", "Typic Hapludalf")
  ), pedon_csv, row.names = FALSE)
  write.csv(data.frame(
    pedon_key = c("P001","P001","P002","P002"),
    hzn_top   = c(0, 25, 0, 30),
    hzn_bot   = c(25, 80, 30, 90),
    hzn_desgn = c("Ap","Bt","Ap","Bt"),
    clay_pct  = c(22, 35, 18, 30),
    silt_pct  = c(60, 50, 65, 55),
    sand_pct  = c(18, 15, 17, 15),
    ph_h2o    = c(6.2, 6.5, 6.5, 6.7),
    oc_pct    = c(2.0, 0.5, 1.5, 0.4),
    cec_nh4   = c(20, 18, 16, 14),
    base_sat  = c(85, 88, 75, 78),
    bulk_density = c(1.3, 1.5, 1.35, 1.55)
  ), layer_csv, row.names = FALSE)

  out <- load_kssl_pedons(pedon_csv, layer_csv, verbose = FALSE)
  expect_length(out, 2L)
  expect_s3_class(out[[1]], "PedonRecord")
  expect_equal(out[[1]]$site$reference_usda, "Typic Argiudoll")
  expect_equal(nrow(out[[1]]$horizons), 2L)
})


test_that("load_lucas_pedons returns single-horizon Ap pedons with reference_wrb", {
  td <- tempdir()
  csv <- file.path(td, "lucas_topsoil.csv")
  write.csv(data.frame(
    POINT_ID = c(1L, 2L),
    TH_LAT   = c(48.5, 50.1),
    TH_LONG  = c(2.3,   8.7),
    NUTS_0   = c("FR", "DE"),
    clay     = c(22, 30),
    silt     = c(50, 45),
    sand     = c(28, 25),
    pH_H2O   = c(6.5, 6.7),
    OC       = c(2.0, 1.5),
    CEC      = c(15, 18),
    CaCO3    = c(0, 5),
    WRB      = c("Cambisols", "Luvisols")
  ), csv, row.names = FALSE)

  out <- load_lucas_pedons(csv, verbose = FALSE)
  expect_length(out, 2L)
  expect_equal(out[[1]]$site$reference_wrb, "Cambisols")
  expect_equal(nrow(out[[1]]$horizons), 1L)
  expect_equal(out[[1]]$horizons$top_cm[1], 0)
  expect_equal(out[[1]]$horizons$bottom_cm[1], 20)
})


test_that("load_embrapa_pedons groups layers by id_perfil", {
  td <- tempdir()
  csv <- file.path(td, "bdsolos.csv")
  write.csv(data.frame(
    id_perfil = c("BR-001","BR-001","BR-001","BR-002","BR-002"),
    horizonte = c("A","Bw1","Bw2","A","Bt"),
    prof_sup  = c(0,15,65,0,20),
    prof_inf  = c(15,65,150,20,80),
    argila_pct = c(50,60,65,30,55),
    silte_pct  = c(15,10,8,20,15),
    areia_pct  = c(35,30,27,50,30),
    ph_agua    = c(4.8,4.9,5.0,5.5,5.7),
    c_org_pct  = c(2.0,0.4,0.2,1.5,0.3),
    ctc_cmol   = c(8,5,4.5,12,10),
    v_pct      = c(20,12,10,50,55),
    classificacao_sibcs = c("Latossolos","Latossolos","Latossolos",
                                "Argissolos","Argissolos"),
    latitude   = c(-22.5,-22.5,-22.5,-19.0,-19.0),
    longitude  = c(-43.7,-43.7,-43.7,-44.0,-44.0)
  ), csv, row.names = FALSE)

  out <- load_embrapa_pedons(csv, verbose = FALSE)
  expect_length(out, 2L)
  expect_equal(out[[1]]$site$reference_sibcs, "Latossolos")
  expect_equal(nrow(out[[1]]$horizons), 3L)
  expect_equal(out[[2]]$site$reference_sibcs, "Argissolos")
})


test_that("benchmark_run_classification computes top-1 accuracy and bootstrap CI", {
  td <- tempdir()
  csv <- file.path(td, "bdsolos2.csv")
  write.csv(data.frame(
    id_perfil = c("BR-001","BR-001","BR-001","BR-002","BR-002"),
    horizonte = c("A","Bw1","Bw2","A","Bt"),
    prof_sup  = c(0,15,65,0,20),
    prof_inf  = c(15,65,150,20,80),
    argila_pct = c(50,60,65,30,55),
    silte_pct  = c(15,10,8,20,15),
    areia_pct  = c(35,30,27,50,30),
    ph_agua    = c(4.8,4.9,5.0,5.5,5.7),
    c_org_pct  = c(2.0,0.4,0.2,1.5,0.3),
    ctc_cmol   = c(8,5,4.5,12,10),
    v_pct      = c(20,12,10,50,55),
    classificacao_sibcs = c("Latossolos","Latossolos","Latossolos",
                                "Argissolos","Argissolos")
  ), csv, row.names = FALSE)

  pedons <- load_embrapa_pedons(csv, verbose = FALSE)
  out <- benchmark_run_classification(pedons, system = "sibcs",
                                          level = "order", boot_n = 50L)
  expect_true(out$n_evaluated >= 1L)
  expect_true(is.numeric(out$accuracy_top1))
  expect_true(out$accuracy_top1 >= 0 && out$accuracy_top1 <= 1)
  expect_length(out$accuracy_ci, 2L)
})

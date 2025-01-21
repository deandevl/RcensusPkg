test_that("tiger_cbsa_sf()", {
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))

  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    cbsa_tx_sf <- RcensusPkg::tiger_cbsa_sf(
      vintage = 2020,
      resol = "20m",
      state_filter = "TX",
      general = TRUE,
      output_dir = output_dir,
      delete_files = FALSE
    )
    a_plot <- RplotterPkg::create_sf_plot(cbsa_tx_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_cbsa_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("tiger_block_groups_sf()", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))

  expect_snapshot({
    dc_fips <- usmap::fips(state = "dc")
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    dc_block_groups_sf <- RcensusPkg::tiger_block_groups_sf(
      state = dc_fips,
      output_dir = output_dir,
      delete_files = FALSE
    )
  })

  a_plot <- RplotterPkg::create_sf_plot(dc_block_groups_sf)
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_block_groups_sf() plot", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

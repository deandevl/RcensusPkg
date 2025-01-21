test_that("tiger_urban_area_sf()", {
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))

  expect_snapshot({
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    us_urban_areas_sf <- RcensusPkg::tiger_urban_area_sf(
      vintage = 2019,
      general = TRUE,
      output_dir = output_dir,
      delete_files = FALSE
    )
    a_plot <- RplotterPkg::create_sf_plot(us_urban_areas_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_urban_area_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("tiger_tracts_sf()", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))

  expect_snapshot({
    nm_los_alamos_fips <- usmap::fips(state = "new mexico", county = "los alamos")
    nm_fips <- substr(nm_los_alamos_fips, 1, 2)
    los_alamos_fips <- substr(nm_los_alamos_fips, 3, 5)
    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    express <- parse(text = paste0("COUNTYFP == ", '"', los_alamos_fips, '"'))
    losalamos_tracts_sf <- RcensusPkg::tiger_tracts_sf(
      state = nm_fips,
      general = TRUE,
      express = express,
      output_dir = output_dir,
      delete_files = FALSE
    )
  })

  a_plot <- RplotterPkg::create_sf_plot(losalamos_tracts_sf)
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_tracts_sf() plot", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("tiger_landmarks_sf()", {
  expect_true(requireNamespace("usmap", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("withr", quietly = TRUE))

  expect_snapshot({
    kentucky_fips <- usmap::fips(state = "kentucky")

    output_dir <- withr::local_tempdir()
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }

    kentucky_landmarks_sf <- RcensusPkg::tiger_landmarks_sf(
      state = kentucky_fips,
      entity = "point",
      check_na = TRUE,
      output_dir = output_dir,
      delete_files = FALSE
    )
    a_plot <- RplotterPkg::create_sf_plot(kentucky_landmarks_sf)
  })

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("tiger_landmarks_sf()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

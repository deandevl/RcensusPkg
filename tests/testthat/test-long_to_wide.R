# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("Census Bureau API key required", {
  expect_true(Sys.getenv("CENSUS_KEY") != "")
})

test_that("long_to_wide()", {
  expect_snapshot({
    B19001_1yr_long_dt <- RcensusPkg::get_vintage_data(
      dataset = "acs/acs1",
      vintage = 2016,
      group = "B19001",
      region = "state",
      wide_to_long = TRUE
    )

    head(RcensusPkg::long_to_wide(
      dt = B19001_1yr_long_dt,
      parameter_col = "variable",
      value_col = c("estimate", "moe")
    ))
  })
})

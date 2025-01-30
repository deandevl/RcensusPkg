# Note: Test requires valid Census Bureau API key to be
#  assigned to "CENSUS_KEY" via usethis::edit_r_environ()

test_that("get_idb_data() Census Bureau API key required", {
  expect_true(Sys.getenv("CENSUS_KEY") != "")
})

test_that("get_idb_data() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr", quietly = TRUE))
})

test_that("get_idb_data() 1year", {
  expect_snapshot(
    head(RcensusPkg::get_idb_data(
      dataset = "1year",
      years = c(2023, 2024),
      countries = c("BW", "NO")
    ))
  )
})

test_that("get_idb_data() 5year", {
  expect_snapshot(
    head(RcensusPkg::get_idb_data(
      dataset = "5year",
      years = 2023,
      group = TRUE,
      countries = "US",
      wide_to_long = TRUE
    ))
  )
})

test_that("get_geography()", {
  expect_snapshot(RcensusPkg::get_geography(
    dataset = "acs/acs1/profile",
    vintage = 2019
  ))
})

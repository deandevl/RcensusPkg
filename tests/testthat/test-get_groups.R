test_that("get_groups()", {
  expect_snapshot(head(RcensusPkg::get_groups(
      dataset = "acs/acs5",
      vintage = 2019
    ))
  )
})

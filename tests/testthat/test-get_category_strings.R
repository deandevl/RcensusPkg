
test_that("get_category_strings() category names",{
  expect_snapshot(RcensusPkg::get_category_strings(get_names = TRUE))
})

test_that("get_category_strings() race", {
  expect_snapshot(RcensusPkg::get_category_strings(name = "race"))
})

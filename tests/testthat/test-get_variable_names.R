test_that("get_dataset_names() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr", quietly = TRUE))
})

test_that("get_variable_names()", {
  expect_snapshot({
    variables_dt <- RcensusPkg::get_variable_names(
      dataset = "acs/acs1/profile",
      vintage = 2019,
      filter_label_str = "educational attainment"
    )
    variables_dt[23:33, .(name, label)]
  })
})

test_that("get_variable_names() category", {
  expect_snapshot({
    variables_dt <- RcensusPkg::get_variable_names(
      category = "acs1",
      vintage = 2023,
      filter_label_str = "computers"
    )
    variables_dt[44:49, .(name, label)]
  })
})

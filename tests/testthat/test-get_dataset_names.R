
test_that("get_dataset_names() acs5", {
  expect_snapshot({
    acs5_datasets_ls <- RcensusPkg::get_dataset_names(
      vintage = 2020,
      filter_name_str = "acs5/"
    )
  })
  expect_snapshot(acs5_datasets_ls$data)
})

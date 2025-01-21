library(usethis)
library(data.table)
library(here)

data_file_path <- file.path(here::here(), "data-raw", "us_vote_2020.csv")
vote2020 <- data.table::fread(file = data_file_path) |>
_[, `:=`(called = as.factor(called))] |>
  data.table::setnames(
    old = c("state", "called"),
    new = c("State", "Party")
  ) |>
_[, .(State, Party)]

usethis::use_data(vote2020, overwrite = TRUE)

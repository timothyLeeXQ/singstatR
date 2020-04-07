context("singstat_tabledata")

test_that("output", {
  expect_s3_class(singstat_tabledata(16102), "data.frame")
  expect_equivalent(ncol(singstat_tabledata(16102)), 8)
  expect_message(singstat_tabledata(16102))
  expect_silent(singstat_tabledata(16102, print_info = FALSE))
})

test_that("arguments", {
  expect_equivalent(nrow(singstat_tabledata(15808, limit = 5)), 5)
  expect_equivalent(nrow(singstat_tabledata(15808, offset = 5)), nrow(singstat_tabledata(15808)) - 5)
})

test_that("SingStat error", {
  expect_error(singstat_tabledata("xxxxxx"))
  expect_error(singstat_tabledata("16102", variables = "M600981.5.2"))
  expect_error(singstat_tabledata("16102", between = "2018"))
  expect_error(singstat_tabledata("16102", between = c("yy","2018")))
  expect_error(singstat_tabledata("16102", time_filter = c("2018 Q1")))
  expect_error(singstat_tabledata(15808,
                                  variables = "M600981.5.2",
                                  between = c(24.0, 28.0),
                                  limit = 5,
                                  time_filter = c(2014, 2016, 2018),
                                  offset = 5,
                                  var_search = "Enterprises")
               )
})

test_that("SingStat error", {
  expect_error(singstat_tabledata("xxxxxx"))
  expect_error(singstat_tabledata("16102", variables = "M600981.5.2"))
  expect_error(singstat_tabledata("16102", between = "2018"))
  expect_error(singstat_tabledata("16102", between = c("yy","2018")))
  expect_error(singstat_tabledata("16102", time_filter = c("2018 Q1")))
})

context("singstat_resource")

test_that("Errors for invalid searchOptions", {
  expect_error(singstat_resource(keyword = "gdp", searchOption = NULL))
  expect_error(singstat_resource(keyword = "gdp", searchOption = "tittle"))
  expect_error(singstat_resource(keyword = "gdp", searchOption = "some"))
  expect_error(singstat_resource(keyword = "gdp", searchOption = "alll"))
  expect_error(singstat_resource(keyword = "gdp", searchOption = "vaariable"))
})

test_that("resourceId output", {
  expect_error(singstat_resource("xxxxxx"))
  expect_s3_class(singstat_resource("gdp"), "data.frame")
  expect_equivalent(ncol(singstat_resource("gdp")), 2)
  expect_output(singstat_resource("gdp", printSummary = TRUE))
})

test_that("SingStat error", {
  expect_error(singstat_resource("xxxxxx"))
})

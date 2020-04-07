context("singstat_endpoint")

test_that("Errors for invalid endpoints", {
  expect_error(singstat_endpoint("resourceIDs"))
  expect_error(singstat_endpoint("meetdata"))
  expect_error(singstat_endpoint("tabledaata"))
})

test_that("URLs constructed properly with case insensitive matching", {
  expect_match(singstat_endpoint("resourceId"),
               "https://www.tablebuilder.singstat.gov.sg/publicfacing/rest/timeseries/resourceId",
               ignore.case = TRUE)
  expect_match(singstat_endpoint("METADATA"),
               "https://www.tablebuilder.singstat.gov.sg/publicfacing/rest/timeseries/metadata",
               ignore.case = TRUE)
  expect_match(singstat_endpoint("tabledata"),
               "https://www.tablebuilder.singstat.gov.sg/publicfacing/rest/timeseries/tabledata",
               ignore.case = TRUE)
})

context("singstat_tidytable")


 test_or <- singstat_search(keywords = c("gdp", "government"), search_options = c("all", "all"))
 test_or_3 <- singstat_search(keywords = c("gdp", "growth", "government"), search_options = c("all", "title", "all"))
 test_and <- singstat_search(keywords = c("gdp", "government"), search_options = c("all", "all"), search_type = "and")


test_that("return object", {
  #df object
  expect_s3_class(test_or, "data.frame")
  #cols for and
  expect_equivalent(ncol(test_and), 2)
  #names for and
  expect_equal(colnames(test_and), colnames(singstat_resource("gdp", printSummary = FALSE)))
  #cols for or 2
  expect_equivalent(ncol(test_or), (2 + 2))
  #names for or 2
  expect_equal(colnames(test_or), c(colnames(singstat_resource("gdp", printSummary = FALSE)), "gdp", "government"))
  #cols for or 3
  expect_equivalent(ncol(test_or_3), (2 + 3))
  #names for or 3
  expect_equal(colnames(test_or_3), c(colnames(singstat_resource("gdp", printSummary = FALSE)), "gdp", "growth", "government"))
})


test_that("error handling", {
  #Inconsistent search option and keywords length
  expect_error(singstat_search(keywords = c("gdp", "growth", "government"), search_options = c("all", "all")))
  #Invalid search
  expect_output(singstat_search(keywords = c("gdp", "xxx", "family"), search_options = c("all", "xxx", "title")))
  #Invalid search type
  expect_error(singstat_search(keywords = c("gdp", "government"), search_options = c("all", "all"), search_type = "aaa"))
})

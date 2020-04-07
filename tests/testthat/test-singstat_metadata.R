context("singstat_metadata")

test_that("metadata output", {
  expect_s3_class(singstat_metadata(16102), "data.frame")
  expect_equivalent(ncol(singstat_metadata(16102)), 5)
  expect_message(singstat_metadata(16102))
  expect_silent(singstat_metadata(16102, print_info = FALSE))
})

attr_test <- function(df, attribute) {
  attribute %in% names(attributes(df))
}

test_df <- singstat_metadata(16102, print_info = FALSE)

test_that("attribute names", {
  expect_setequal(names(attributes(test_df)), c("names",
                                                "row.names",
                                                "class",
                                                "resourceId",
                                                "title",
                                                "frequency",
                                                "uom",
                                                "datasource",
                                                "url",
                                                "footnote",
                                                "startPeriod",
                                                "endPeriod",
                                                "termsOfUse",
                                                "apiTermsOfService"
                                                )
                  )
  expect_false(attr_test(test_df, "xxxx"))
  expect_false(attr_test(test_df, "downloadFormats"))
  expect_true(attr_test(test_df, "resourceId"))
  expect_true(attr_test(test_df, "title"))
  expect_true(attr_test(test_df, "frequency"))
  expect_true(attr_test(test_df, "uom"))
  expect_true(attr_test(test_df, "datasource"))
  expect_true(attr_test(test_df, "url"))
  expect_true(attr_test(test_df, "footnote"))
  expect_true(attr_test(test_df, "startPeriod"))
  expect_true(attr_test(test_df, "endPeriod"))
  expect_true(attr_test(test_df, "termsOfUse"))
  expect_true(attr_test(test_df, "apiTermsOfService"))
})

test_that("attribute values", {
  expect_equal(attr(test_df, "resourceId"), 16102)
  expect_equal(attr(test_df, "datasource"), "SINGAPORE DEPARTMENT OF STATISTICS")
  expect_true(is.numeric(attr(test_df, "startPeriod")))
  expect_true(is.numeric(attr(test_df, "endPeriod")))
  expect_equal(attr(test_df, "termsOfUse"), "https://www.singstat.gov.sg/terms-of-use")
  expect_equal(attr(test_df, "apiTermsOfService"), "https://data.gov.sg/privacy-and-website-terms#api-terms")
})

test_that("SingStat error", {
  expect_error(singstat_metadata("xxxxxx"))
})

filter_level_test <- function(n) {
  expect_lte(nrow(singstat_metadata(16102, print_info = FALSE, filter_level = n)),
             nrow(test_df)
             )
}

test_that("filter_level", {
  filter_level_test(1)
  filter_level_test(2)
  filter_level_test(3)
  filter_level_test(4)
  filter_level_test(5)
  filter_level_test(6)
})

test_df1 <- singstat_metadata(16102, print_info = FALSE, filter_var = "e", ignore_case = TRUE)
test_df2 <- singstat_metadata(16102, print_info = FALSE, filter_var = "e", ignore_case = FALSE)

test_that("filter_var", {
  #Test for use of regex
  expect_equal(nrow(singstat_metadata(16102, print_info = FALSE, filter_var = ".")),
             nrow(test_df)
             )
  #Test for filtering
  expect_lte(nrow(test_df1), nrow(test_df))
  expect_gt(nrow(test_df1), 0)
  expect_true(all(stringr::str_detect(test_df1$variableName, stringr::regex("e", ignore_case = TRUE))
                  )
              )
  #Test for ignore_case
  expect_gt(nrow(test_df1), nrow(test_df2))
})


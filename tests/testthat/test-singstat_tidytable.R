context("singstat_tidytable")

tabledata_test_annual <- singstat_tabledata(15808)
tabledata_test_quarterly <- singstat_tabledata(14506)

tidytable_test_annual <- singstat_tidytable(15808)
tidytable_test_quarterly <- singstat_tidytable(14506)
tidytable_test_quarterly_force <- singstat_tidytable(14506, force_annual = TRUE)

test_that("output correct", {
  #df object
  expect_s3_class(tabledata_test_annual, "data.frame")
  #Correct number of columns for:
  ## basic annual table
  expect_equivalent(length(unique(tabledata_test_annual$variableCode)) + 2, (ncol(tidytable_test_annual)))
  ##sub-annual table
  expect_equivalent(length(unique(tabledata_test_quarterly$variableCode)) + 3, (ncol(tidytable_test_quarterly)))
  ## sub-annual table where force_annual = TRUE
  expect_equivalent(length(unique(tabledata_test_quarterly$variableCode)) + 2, (ncol(tidytable_test_quarterly_force)))
})

test_that("attributes", {
  expect_setequal(names(attributes(tabledata_test_annual)), c("names",
                                                              "row.names",
                                                              "class",
                                                              "url",
                                                              "total",
                                                              "footnotes"
                                                              )
                  )
})

test_that("force_annual", {
  #frequency says Annual
  expect_true(all(tidytable_test_quarterly_force$frequency == "Annual"))
  #warning for annual table
  expect_warning(singstat_tidytable(15808, force_annual = TRUE))
  #Number of rows is quartered
  expect_equivalent(nrow(tidytable_test_quarterly_force), (nrow(tidytable_test_quarterly)/4), tolerance = 4)

})


test_that("force_op", {
  #warning for bad input
  expect_warning(singstat_tidytable(15808, force_annual = TRUE, force_op = "summ", na.rm = TRUE))
})

test_that("inheritance", {
  #singstat_tabledata inherits the variables argument
  expect_equivalent(ncol(singstat_tidytable(15808, variables = c("M600981.1", "M600981.1.2"))), 4)
  #singstat_tabledata inherits the time_filter argument
  expect_equivalent(nrow(singstat_tidytable(15808,
                                            variables = c("M600981.1", "M600981.1.2"),
                                            time_filter = c("2014", "2015", "2017", "2018")
                                            )
                         ), 4
                    )
})

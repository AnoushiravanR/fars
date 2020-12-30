library(testthat)


test_that("erroneous file name", {
  expect_that(fars_read("accident_2012.csv.bz2"), throws_error())
})


test_that("throwing error", {
  expect_that(fars_map_state(2, 2013), throws_error())
})





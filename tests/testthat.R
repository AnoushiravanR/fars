library(testthat)
library(fars)

test_check("fars")

test_that("printing file name", {
  expect_that(make_filename(2013), prints_text())
})

test_that("erroneous file name", {
  expect_that(fars_read("accident_2012.csv.bz2"), throws_error())
})

test_that("invalid years", {
  expect_that(fars_read_years(c(2010, 2011)), gives_warning())
})


test_that("throwing error", {
  expect_that(fars_map_state(2, 2013), throws_error())
})





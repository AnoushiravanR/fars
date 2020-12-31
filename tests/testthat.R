library(testthat)

<<<<<<< HEAD

test_that("printing file name", {
  expect_that(make_filename(2013), prints_text())
})
=======
>>>>>>> a7040d451ce193ac86c7d07ae2e1d985f86ecd01

test_that("erroneous file name", {
  expect_that(fars_read("accident_2012.csv.bz2"), throws_error())
})


test_that("throwing error", {
  expect_that(fars_map_state(2, 2013), throws_error())
})





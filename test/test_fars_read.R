test_that('fars_read function read files in the working directory', {
  # Check for inheritance
  expect_is(fars_read("accident_2013.csv.bz2"), "tbl")

  # Check for functionality
  expect_equal(ncol(fars_read("accident_2013.csv.bz2")), 50)

  # Check for errors
  expect_error(fars_read("this/route/is/innexistent"))
})

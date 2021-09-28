# Test file for spacejam.


test_object <- spacejam("2010-01-01", "2010-12-31")

test_that("Wrong input throws error", {
  expect_error(spacejam("2010-10-01", 2010-11-01))
  expect_error(spacejam(TRUE, "2010-01-01"))
})
test_that("Equal colnames even if different timestamps as input", {
  expect_equal(colnames(spacejam("2010-03-01", "2010-10-06")), colnames(test_object))
})

test_that("Call status ok",{
  msg <- test_object$response$status
  expect_equal(msg, 200)
})


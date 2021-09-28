# Test file for spacejam.


test_object <- spacejam("2010-01-01", "2010-12-31")

test_that("Wrong input throws error", {
  expect_error(spacejam(2011-01-01, "2010-11-01"))
  expect_error(spacejam("2010-10-01", 2010-11-01))
  expect_error(spacejam(TRUE, "2010-01-01"))
  expect_error(spacejam("2011-11-11", "2010-11-11")) #end_date before start_date
})

test_that("Equal colnames even if different timestamps as input", {
  expect_equal(colnames(spacejam("2010-03-01", "2010-10-06")$data), colnames(test_object$data))
})

test_that("Call status ok",{
  msg <- test_object$response$status
  if(msg == 200){expect_equal(msg, 200)}
  else if(msg != 200){expect_error(spacejam("2010-01-01", "2020-01-01"),
                                   "Exceeded call limit:", ignore.case = TRUE)}
})

test_that("Class is correct", {
  expect_s3_class(test_object, "spacejam_API")
})

test_that("Object contains correct data", {
  # Set up expected df
  correct_df <- data.frame("index" = c(1:6),
                           "time" = c("2010-06-13T16:42Z", "2010-07-30T16:08Z", "2010-08-01T08:10Z",
                                      "2010-08-01T10:20Z", "2010-08-01T11:20Z", "2010-08-01T17:00Z"),
                           "speed" = c(500, 750, 760, 1800, 1300, 550),
                           "type" = c("C", "C", "C", "O", "O", "C"))
  expect_length(test_object, 2) # length = Data & Response = 2
  expect_mapequal(head(test_object$data), correct_df) # The head of generic call for data
  expect_equal(colnames(test_object$response), c("usage", "call_time", "status")) # As calls differ each time, test for correct colnames
})

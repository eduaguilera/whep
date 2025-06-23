library("testthat")

test_that("Check my_hello_world says hello correctly", {
  expected_output <- my_hello_world("cow")
  say_msg <- "Hello world!"
  expect_equal(grepl(say_msg, expected_output), TRUE)
  expect_true(grepl(say_msg, expected_output))
  expect_false(grepl(paste0(say_msg, "!"), expected_output))
  len_say_msg <- stringr::str_count(say_msg)
  len_expected_output <- stringr::str_count(expected_output)
  expect_false(len_say_msg == len_expected_output)
  expect_true(len_say_msg < len_expected_output)
})

test_that(
  "Check my_hello_world gives an error when the animal is not in the list",
  {
    expect_error(my_hello_world("dog"))
  }
)

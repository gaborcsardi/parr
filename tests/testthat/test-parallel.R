
context("Simple parallel execution")

test_that("arguments, no assignment", {

  tmp1 <- tempfile()
  tmp2 <- tempfile()
  on.exit(rm(tmp1, tmp2), add = TRUE)

  parallel(
    cat("hello1\n", file = tmp1),
    cat("hello2\n", file = tmp2)
  )

  expect_equal(readLines(tmp1), "hello1")
  expect_equal(readLines(tmp2), "hello2")

  stopCluster(.reg$default)
})

test_that("assignment", {

  parallel(
    x <- 1:10 * 2,
    y <- paste(letters[1:5], 1:5)
  )

  expect_equal(x, 1:10 * 2)
  expect_equal(y, paste(letters[1:5], 1:5))

  stopCluster(.reg$default)
})

test_that("no expressions", {

  expect_silent(
    parallel()
  )

  stopCluster(.reg$default)
})

test_that("error if not a call", {

  expect_error(
    parallel(x <- this_is_good(), "foobar"),
    "Not a call:"
  )

  stopCluster(.reg$default)
})

test_that("errors are handled", {

  f <- function() 1 + "A"
  x <- parallel(f())
  expect_equal(
    x$errors,
    "non-numeric argument to binary operator"
  )

  stopCluster(.reg$default)
})

test_that("spinner is called", {

  with_mock(
    `parr::spin` = function(...) print("here I am"),
    expect_output(
      parallel(Sys.sleep(2)),
      "here I am"
    )
  )

  stopCluster(.reg$default)
})

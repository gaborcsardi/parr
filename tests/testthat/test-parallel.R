
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

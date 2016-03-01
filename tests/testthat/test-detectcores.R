
context("Detecting number of processors")

test_that("number of cores on windows", {

  withr::with_envvar(
    c(NUMBER_OF_PROCESSORS = 4),
    expect_equal(
      detectCoresWin(logical = TRUE),
      4
    )
  )

  with_mock(
    `base::system` = function(...) "DeviceID  NumberOfCores\nCPU0      2\n\n",
    expect_equal(
      detectCoresWin(logical = FALSE),
      2
    )
  )

  with_mock(
    `base::system` = function(...) {
      "DeviceID  NumberOfCores\nCPU0      2\nCPU1      2\n\n"
    },
    expect_equal(
      detectCoresWin(logical = FALSE),
      4
    )
  )

})

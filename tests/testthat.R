library(testthat)
library(parr)

Sys.unsetenv("R_TESTS")

test_check("parr")

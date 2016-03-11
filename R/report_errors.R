
report_errors <- function(result) {

  warn <- vapply(result$warnings, length, 1L)
  err  <- nchar(result$errors)

  if (any(err)) {
    stop(
      "Error(s) during parallel execution\n",
      format(result),
      call. = FALSE
    )

  } else if (any(warn)) {
    warning(
      "Warning(s) during parallel execution\n",
      format(result),
      call. = FALSE
    )
  }
}

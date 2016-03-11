
#' Evaluate R expressions in parallel
#'
#' @param ... The expressions to evaluate. Assignments assign to
#'   the parent environment, see examples below.
#' @return Nothing, currently.
#'
#' @export
#' @examples
#' \dontrun{
#' x <- 1:10
#' parallel(
#'   y1 <- x + 1,
#'   y2 <- x * 2
#' )
#' y1
#' y2
#' }

parallel <- function(...) {

  env <- parent.frame()

  start_if_needed()

  exprs <- as.list(match.call(expand.dots = FALSE)$...)

  if (length(exprs) == 0) return(invisible())

  calls <- parse_calls(exprs, env = env)

  output <- scheduler(calls, env = env)

  res <- structure(
    list(
      calls = exprs,
      output = output$output,
      messages = output$messages,
      warnings = output$warn,
      errors = output$errors
    ),
    class = "parr_output"
  )

  invisible(res)
}

parse_calls <- function(exprs, env) {

  ## Are these calls?
  iscall <- vapply(exprs, is.call, TRUE)
  if (!all(iscall)) {
    stop("Not a call: ", sapply(exprs[!iscall], deparse))
  }

  lapply(exprs, parse_call, env = env)
}

parse_call <- function(call, env) {
  res <- if (identical(call[[1]], quote(`<-`))) {
    list(result = call[[2]], call = call[[3]])
  } else {
    list(result = NULL, call = call)
  }

  if (is.call(res$call[[1]]) &&
      (identical(res$call[[1]][[1]], quote(`::`)) ||
         identical(res$call[[1]][[1]], quote(`:::`)))) {
    env <- asNamespace(as.character(res$call[[1]][[2]]))
    fname <- as.character(res$call[[1]][[3]])
    res$fun <- get(fname, envir = env, mode = "function")
    res$args <- as.list(res$call)[-1]

  } else {
    res$fun <- get(as.character(res$call[[1]]), envir = env, mode = "function")
    res$args <- as.list(res$call)[-1]
  }

  res
}

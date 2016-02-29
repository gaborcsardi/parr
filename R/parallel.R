
#' Evaluate R expressions in parallel
#'
#' @param ... The expressions to evaluate. Assignments assign to
#'   the parent environment, see examples below.
#' @return Nothing, currently.
#'
#' @export
#' @examples
#' x <- 1:10
#' parallel(
#'   y1 <- x + 1,
#'   y2 <- x * 2
#' )
#' y1
#' y2

parallel <- function(...) {

  env <- parent.frame()

  start_if_needed()

  exprs <- as.list(match.call(expand.dots = FALSE)$...)

  if (length(exprs) == 0) return(invisible())

  calls <- parse_calls(exprs, env = env)

  scheduler(calls, env = env)
}

start_if_needed <- function() {
  if (is.null(.reg$default)) {
    .reg$default <- makeCluster(num_workers())
    .reg$state <- NULL
  }
}

num_workers <- function() {
  detectCores()
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
  res$fun <- get(as.character(res$call[[1]]), envir = env, mode = "function")
  res$args <- as.list(res$call)[-1]
  res
}

#' Schedule tasks to workers
#'
#' Worker states:
#' * free
#' * busy
#' * done
#'
#' Call states:
#' * submitted
#' * running
#' * done
#'
#' @param calls List of calls to schedule
#' @param env Environment to assign results to, for assignment
#'   expressions.
#' @return Nothing, currently.
#'
#' @keywords internal

scheduler <- function(calls, env) {
  num_workers <- length(.reg$default)
  state <- .reg$state %||% rep("free", num_workers)

  callstate <- rep("submitted", length(calls))
  myjob <- rep(0, num_workers)

  repeat {
    while (any(callstate == "submitted") && any(state == "free")) {
      ## Submit as many as possible
      call <- which(callstate == "submitted")[1]
      worker <- which(state == "free")[1]

      callstate[[call]] <- "running"
      state[[worker]] <- "busy"
      myjob[[worker]] <- call

      sendCall(
        .reg$default[[worker]],
        calls[[call]]$fun,
        lapply(calls[[call]]$args, eval, envir = env)
      )
    }

    if (any(callstate == "running")) {
      ## Get the result of at least one worker
      res <- recvOneData(.reg$default)
      state[[res$n]] <- "free"
      call <- myjob[[res$n]]
      callstate[[call]] <- "done"

      ## Assign it if needed
      if (!is.null(calls[[call]]$result)) {
        assign(as.character(calls[[call]]$result), res$value$value, envir = env)
      }
    }

    if (all(callstate == "done")) {
      break
    }
  }

  .reg$state <- state

  invisible()
}


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

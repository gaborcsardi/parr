
#' Schedule tasks to workers
#'
#' Worker states:
#' * free
#' * busy
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
  output <- rep(NA_character_, length(calls))
  messages <- rep(NA_character_, length(calls))
  warn <- replicate(length(calls), list())
  errors <- rep(NA_character_, length(calls))
  results <- replicate(length(calls), list())

  start <- proc.time()
  clear_me <- FALSE

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
        lapply(calls[[call]]$args, eval, envir = env),
        wd = getwd(),
        pkgs = search()
      )
    }

    if (any(callstate == "running")) {
      ## Get the result of at least one worker
      res <- recvOneData(.reg$default, timeout = 0.25)

      if (is.null(res)) {
        clear_me <- TRUE
        time = spin(proc.time() - start, states = callstate)
        next
      }

      state[[res$node]] <- "free"
      call <- myjob[[res$node]]
      callstate[[call]] <- "done"

      ## Collect output
      output[call] <- read_file(.reg$outfiles[res$node])
      messages[call] <- read_file(.reg$errfiles[res$node])
      unlink(.reg$outfiles[res$node])
      unlink(.reg$errfiles[res$node])

      ## Collect warnings
      warn[[call]] <- res$value$warnings

      ## Handle error
      if (!res$value$success) {
        errors[call] <- as.character(res$value$value)
        next
      } else {
        errors[call] <- ""
      }

      ## Assign it if needed
      if (!is.null(calls[[call]]$result)) {
        results[[call]] <- res$value$value
      }
    }

    if (all(callstate == "done")) {
      break
    }
  }

  .reg$state <- state

  if (clear_me) clear_line()

  list(
    results = results,
    output = output,
    messages = messages,
    errors = errors,
    warnings = warn
  )
}


spin_factory <- function() {
  spinner <- c("-", "\\", "|", "/")
  state <- 1
  function(time = NULL, states, ...) {

    cat(
      "\r",
      spinner[state], " ",
      length(states), " tasks, ",
      sum(states == "done"), " done, ",
      sum(states == "running"), " running.      ",
      sep = ""
    )

    state <<- if (state < length(spinner)) state + 1 else 1
  }
}

spin <- spin_factory()

clear_line <- function() {
  cat("\r                                          \r")
}

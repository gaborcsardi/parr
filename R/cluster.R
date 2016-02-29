
start_if_needed <- function() {
  if (is.null(.reg$default)) {
    .reg$default <- makeCluster(num_workers())
    .reg$state <- NULL
  }
}

num_workers <- function() {
  detectCores()
}

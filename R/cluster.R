
start_if_needed <- function() {
  if (is.null(.reg$default)) {
    num <- num_workers()
    outstub <- tempfile()
    .reg$outfiles <- paste0(outstub, "-out.", 1:num)
    .reg$errfiles <- paste0(outstub, "-err.", 1:num)
    .reg$default <- makeCluster(
      num,
      outfile = .reg$outfiles,
      errfile = .reg$errfiles
    )
    .reg$state <- NULL
  }
}

num_workers <- function() {
  detectCores()
}

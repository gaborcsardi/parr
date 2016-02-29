
`%||%` <- function(l, r) if (is.null(l)) r else l

read_file <- function(file) {
  readChar(file, nchars = file.info(file)$size, useBytes = TRUE)
}

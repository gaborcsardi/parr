
#' @export

print.parr_output <- function(x, ...) {
  cat(format(x), sep = "", "\n")
}

#' @export

format.parr_output <- function(x, ...) {
  fmt <- vapply(seq_along(x$calls), format_call, "", parr = x)
  paste(fmt, collapse = "\n")
}

format_call <- function(x, parr) {
  paste(
    sep = "",
    format_header(format(parr$calls[[x]])),
    if (nzchar(parr$output[[x]])) format_output(parr$output[[x]]),
    if (nzchar(parr$messages[[x]])) format_messages(parr$messages[[x]])
  )
}

#' @importFrom crayon green underline
#' @importFrom clisymbols symbol

format_header <- function(text) {
  width <- min(getOption("width", 80), 80)
  str <- paste0(
    green(symbol$tick), "  ",
    underline(text),
    "\n"
  )
}

format_output <- function(text, marker = ". ") {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- substr(lines, 1, 75)
  if (length(lines) > 6) {
    lines <- c(head(lines, 3), " --- ", tail(lines, 3))
  }
  paste0("\n", paste0(marker, lines, collapse = "\n"), "\n")
}

#' @importFrom crayon blue
#' @importFrom clisymbols symbol

format_messages <- function(text) {
  blue(format_output(text, marker = paste0(symbol$info, " ")))
}


#' @export

print.parr_output <- function(x, ...) {
  cat(format(x), sep = "", "\n")
}

#' @export

format.parr_output <- function(x, ...) {
  fmt <- vapply(seq_along(x$calls), format_call, "", parr = x)
  paste(fmt, collapse = "\n")
}

#' @importFrom crayon green yellow red
#' @importFrom clisymbols symbol

format_call <- function(x, parr) {
  symb <- green(symbol$tick)
  if (length(parr$warnings[[x]])) symb <- yellow(symbol$warning)
  if (parr$errors[[x]] != "") symb <- red(symbol$cross)
  paste(
    sep = "",
    format_header(format(parr$calls[[x]]), symb),
    if (parr$errors[[x]] != "") format_errors(parr$errors[[x]]),
    if (length(parr$warnings[[x]])) format_warnings(parr$warnings[[x]]),
    if (nzchar(parr$messages[[x]])) format_messages(parr$messages[[x]]),
    if (nzchar(parr$output[[x]])) format_output(parr$output[[x]])
  )
}

#' @importFrom crayon underline

format_header <- function(text, symb) {
  width <- min(getOption("width", 80), 80)
  str <- paste0(
    symb, " ",
    underline(text),
    "\n"
  )
}

format_output <- function(text, marker = "  > ") {
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
  blue(format_output(text, marker = "  MSG: "))
}

format_warnings <- function(warnings) {
  w <- vapply(warnings, format_warning, "")
  paste0("\n", paste(w, collapse = "\n"), "\n")
}

#' @importFrom crayon yellow

format_warning <- function(warn) {
  lines <- strsplit(warn, "\n", fixed = TRUE)[[1]]
  lines[1] <- paste0("  WARNING: ", lines[1])
  lines[-1] <- paste("  ", lines[-1])
  yellow(paste(lines, collapse = "\n"))
}

format_errors <- function(errors) {
  e <- vapply(errors, format_error, "")
  paste0("\n", paste(e, collapse = "\n"), "\n")
}

format_error <- function(error) {
  lines <- strsplit(error, "\n", fixed = TRUE)[[1]]
  lines[1] <- paste0("  ERROR: ", lines[1])
  lines[-1] <- paste("  ", lines[-1])
  red(paste(lines, collapse = "\n"))
}

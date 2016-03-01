
start_if_needed <- function() {
  if (is.null(.reg$default)) suppressMessages(start_cluster())
}

num_workers <- function() {
  detectCores()
}

#' Start the cluster
#'
#' @param workers Number of worker processes. If \code{NULL}, then
#'   the number of (logical) cores of the machine is used.
#'
#' @family cluster management
#' @export

start_cluster <- function(workers = NULL) {
  if (!is.null(.reg$default)) {
    stop("Remove the current cluster before creating a new one", call. = FALSE)
  }

  if (is.null(workers)) workers <- num_workers()
  stopifnot(workers >= 1)

  outstub <- tempfile()
  .reg$outfiles <- paste0(outstub, "-out.", 1:workers)
  .reg$errfiles <- paste0(outstub, "-err.", 1:workers)
  .reg$default <- makeCluster(
    workers,
    outfile = .reg$outfiles,
    errfile = .reg$errfiles
  )

  .reg$state <- rep("free", workers)
  message("Cluster started with ", workers, " workers")

  invisible()
}

#' Stop the cluster
#'
#' @family cluster management
#' @export

stop_cluster <- function() {
  stopCluster(.reg$default)
  message("Cluster stopped")
}

#' Show a brief summary of the current cluster
#'
#' It also returns a machine readable list, invisibly.
#' The return value can be used to test if the cluster is set up:
#' if not, then the \code{num_workers} entry is set to zero.
#'
#' @return An invisible list, with arguments:
#'   \item{num_workers}{The number of worker nodes}
#'   \item{state}{The state of the cluster, a character vector with
#'     one entry for each worker. The state of the worker is either
#'     \sQuote{free} or \sQuote{busy}.}
#'
#' @family cluster management
#' @export

show_cluster <- function() {
  if (is.null(.reg$default)) {
    message("No cluster")
  }

  df <- list(
    num_workers = length(.reg$default),
    state = .reg$state
  )

  print_cluster(.reg)

  invisible(df)
}

print_cluster <- function(cluster) {
  if (is.null(cluster$default)) return()

  cat(
    sep = "",
    "parr cluster with ", length(cluster$default), " workers: ",
    sum(cluster$state == "free"), " free, ",
    sum(cluster$state == "busy"), " busy.",
    "\n"
  )
}

.counters <- new.env(parent = emptyenv())

new_counter <- function(src_ref, parent_functions) {
  key <- key(src_ref)
  .counters[[key]]$value <- 0
  .counters[[key]]$srcref <- src_ref
  .counters[[key]]$functions <- parent_functions
  key
}

#' increment a given counter
#'
#' @param key generated with [key()]
#' @keywords internal
count <- function(key) {
  .counters[[key]]$value <- (.counters[[key]]$value %||% 0L) + 1L
}

#' Delete all previous counters
#'
#' @export

clear_counters <- function() {
  rm(envir = .counters, list = ls(envir = .counters))
}

#' Reset all previous counters
#'
#' @export

reset_counters <- function() {
  for (key in names(.counters)) {
    .counters[[key]]$value <- 0L
  }
}

#' Add externally saved counters
#'
#' @param cnt Counters to add, in an environment.
#'
#' @export

add_counters <- function(cnt) {
  for (key in names(cnt)) {
    .counters[[key]] <- .counters[[key]] %||% cnt[[key]]
  }
}

#' Save counters
#'
#' @export

save_trace <- function() {
  trace_path <- basename(tempfile("covr_trace_"))
  save_rds(.counters, file = trace_path)
}

#' Generate a key for a  call
#'
#' @param x the srcref of the call to create a key for
#' @keywords internal
key <- function(x) {
  paste(collapse = ":", c(get_source_filename(x), x))
}

get_source_filename <- function(
    x,
    full_names = TRUE,
    unique = TRUE,
    normalize = FALSE) {
  res <- utils::getSrcFilename(x, full_names, unique)
  if (length(res) == 0) {
    return("")
  }
  if (normalize) {
    return(normalizePath(res))
  }
  res
}

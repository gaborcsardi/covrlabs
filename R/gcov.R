#' Run `gcov` summarize code coverage
#'
#' Calls `gcov` to create `.gcov` files for all `.gcno` files, which were
#' typically created when running a program instrumented with code
#' coverage.
#'
#' It summarized all `.gcno` files, recursively.
#'
#' @param root Root path, typically the root of an R package tree.
#'
#' @family gcov functions
#' @export

run_gcov <- function(root = ".") {
  gcno <- dir(
    root,
    recursive = TRUE,
    pattern = "[.]gcno$",
    full.names = TRUE
  )
  dirs <- dirname(gcno)
  # TODO: better way to determine the right directoeries for running gcov
  dirs <- ifelse(basename(dirs) == "src", dirs, dirname(dirs))
  udirs <- unique(dirs)
  pxs <- lapply(udirs, function(d) {
    # need relative paths from d
    fnms <- dir(d, recursive = TRUE, pattern = "[.]gcno$")
    processx::process$new("gcov", c("-p", "-b", fnms), wd = d)
  })
  names(pxs) <- udirs

  while (length(pxs) > 0) {
    pr <- processx::poll(pxs, 1000)
    pr <- vapply(pr, "[[", "", "process")
    dn <- pr != "timeout" & pr != "silent"
    st <- vapply(pxs[dn], function(p) p$get_exit_status(), 1L)
    oh <- pxs[dn][st != 0]
    if (length(oh)) {
      warning(
        "gcov failed for directories: ",
        paste(names(pxs)[oh], collapse = ", ")
      )
    }

    pxs <- pxs[!dn]
  }
}

#' Parse a `.gcov` file
#'
#' @param path Path to the `.gcov` file.
#' @return Data frame with columns:
#'   * `file`: Absolute path to source file.
#'   * `line`: Line number, numbering start with 1.
#'   * `coverage`: Number of times this line was executed. If `NA`, then
#'     it is not a code line that can be executed.
#'   * `code`: Character vector, the actual code.
#'
#' @family gcov functions
#' @export

parse_gcov_file <- function(path) {
  path <- normalizePath(path)
  disp <- display_name(path)
  df <- .Call(c_parse_gcov, path, disp)
  class(df) <- c("code_coverage", "tbl", "data.frame")
  attr(df, "row.names") <- seq_len(length(df[[1]]))
  df
}

display_name <- function(x) {
  x <- sub("[.]gcov", "", x)
  if (startsWith(basename(x), "^")) {
    x <- gsub("^", "..", fixed = TRUE, x)
    x <- gsub("#", "/", fixed = TRUE, x)
    x
  } else {
    b <- gsub("#", "/", basename(x))
    paste0(dirname(x), "/", b)
  }
}

#' Parse all `.gcov` files within a directory
#'
#' Finds, parses and summarizes all `.gcov` files with code coverage
#' information, within a root directory.
#'
#' @param root Root path, typically the root of an R package tree.
#' @return Data frame with columns:
#'   * `file`: Absolute path to source file.
#'   * `line`: Line number, numbering start with 1.
#'   * `coverage`: Number of times this line was executed. If `NA`, then
#'     it is not a code line that can be executed.
#'   * `code`: Character vector, the actual code.
#'
#' @family gcov functions
#' @export

parse_gcov <- function(root = ".") {
  root <- normalizePath(root)
  gcov <- dir(
    root,
    recursive = TRUE,
    pattern = "[.]gcov$",
    full.names = TRUE
  )

  # drop files that have an absolute path, typically system headers
  gcov <- gcov[!startsWith(basename(gcov), "#")]

  ps <- lapply(gcov, parse_gcov_file)
  ps <- do.call(rbind, ps)
  class(ps) <- unique(c("code_coverage", class(ps)))
  ps
}

# TODO: most of the time is spent in readRDS() (30%) and merge() (27%),
# then tapply() (17%).

#' Parse an R coverage output file
#'
#' @param path Path to file.
#' @return Data frame with columns:
#'   * `file`: Absolute path to source file.
#'   * `line`: Line number, numbering starts with 1.
#'   * `coverage`: Number of times this line was executed. If `NA`, then
#'     it is not a code line that can be executed.
#'   * `code`: Character vector, the actual code.
#'
#' @family coverage output parser functions
#' @export

parse_covr_file <- function(path) {
  cov <- unname(as.list(readRDS(path)))
  file <- vapply(cov, function(x) attr(x$srcref, "srcfile")$filename, character(1), USE.NAMES = FALSE)
  linenos <- unname(lapply(cov, function(x) {
    nums <- as.integer(x$srcref)
    nums[[1]]:nums[[3]]
  }))
  coverage <- vapply(cov, "[[", double(1), "value", USE.NAMES = FALSE)

  lens <- lengths(linenos)
  df <- data.frame(
    stringsAsFactors = FALSE,
    file = rep(file, lens),
    line = unlist(linenos),
    coverage = rep(coverage, lens)
  )

  rfiles <- unique(df$file)
  code <- do.call(rbind, lapply(rfiles, function(f) {
    l <- read_lines(f)
    data.frame(
      stringsAsFactors = FALSE,
      file = f,
      line = seq_along(l),
      code = l
    )
  }))
  df <- merge(code, df, all = TRUE)

  df <- df[order(df$file, df$line), c("file", "line", "coverage", "code")]

  # remove duplicated lines coming from multiple expressions within a line
  key <- paste(df$file, df$line)
  maxcov <- tapply(df$coverage, key, max)
  df <- df[!duplicated(key), ]
  dfkey <- paste(df$file, df$line)
  df$coverage[match(names(maxcov), dfkey)] <- unname(maxcov)

  class(df) <- c("tbl", class(df))
  df
}

#' Parse all coverage output or R files
#'
#' @param root Root path, typically root of an R package tree.
#' @return Data frame with columns:
#'   * `file`: Absolute path to source file.
#'   * `line`: Line number, numbering starts with 1.
#'   * `coverage`: Number of times this line was executed. If `NA`, then
#'     it is not a code line that can be executed.
#'   * `code`: Character vector, the actual code.
#'
#' @family coverage output parser functions
#' @export

parse_r_coverage <- function(root) {
  fls <- dir(
    root,
    recursive = TRUE,
    pattern = "^covr_trace_",
    full.names = TRUE
  )

  ps <- lapply(fls, parse_covr_file)
  do.call(rbind, ps)
}
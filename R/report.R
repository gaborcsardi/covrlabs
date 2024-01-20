#' Summarize code coverage results
#'
#' @param object Code coverage object, typically from the
#'   [parse_coverage()] function.
#' @param include_paths If not `NULL`, then it may be a character vector of
#'   paths, to only include files within these paths.
#' @param ... Currently ignored.
#' @return Data frame with columns:
#'   - `path`: absolute file path.
#'   - `relpath`: relative file path, calculated by dropping the common
#'     prefix.
#'   - `total`: total number of code lines.
#'   - `coveraged`: number of covered lines.
#'   - `coverage`: code coverage ratio for this this.
#'   - `uncovered`: uncovered lines, see [uncovered_lines()] for the
#'     format.
#'
#' @family code coverage reporting functions
#' @export

summary.code_coverage <- function(object, include_paths = NULL, ...) {
  perfile <- tapply(object$coverage, object$file, simplify = FALSE,
    function(x) {
      x <- na_omit(x)
      if (length(x) == 0) {
        c(0, 0, 1.0)
      } else {
        tot <- length(x)
        ok <- sum(x != 0)
        c(tot, ok, ok / tot)
      }
    }
  )

  ufiles <- names(perfile)
  unc <- uncovered_lines(object)
  df <- data.frame(
    stringsAsFactors = FALSE,
    path = ufiles,
    relpath = remove_common_prefix(ufiles),
    total = vapply(perfile, "[[", 1, 1, USE.NAMES = FALSE),
    covered = vapply(perfile, "[[", 1, 2, USE.NAMES = FALSE),
    coverage = vapply(perfile, "[[", 1, 3, USE.NAMES = FALSE),
    # paths are in the same order, so this works w/o mapping:
    uncovered = unc$uncovered
  )

  df <- filter_coverage(df, "path", include_paths = include_paths)

  class(df) <- unique(c("code_coverage_summary", "tbl", class(df)))
  df
}

format_intervals <- function(u) {
  ret <- character(ncol(u))
  sm <- u[1,] == u[2,]
  ret[sm] <- u[1, sm]
  if (any(!sm)) {
    ret[!sm] <- paste0(u[1,!sm], "-", u[2,!sm])
  }
  paste(ret, collapse = ", ")
}

#' @export

print.code_coverage_summary <- function(x, ...) {
  pth <- format(substr(x$relpath, 1, 40))
  tot <- format(x$total)
  cov <- format(x$covered)
  pct <- paste0(format(as.integer(x$coverage * 100)), "%")
  unc <- vapply(x$uncovered, format_intervals, character(1))

  lines <- paste(
    pth,
    tot,
    cov,
    pct,
    substr(unc, 1, 100)
  )

  writeLines(lines)

  invisible(x)
}

#' Report lines _not_ covered by tests, for each file
#'
#' @param cov Code coverage object, typically from the
#'   [parse_coverage()] function.
#' @return Data frame with columns:
#'   - `path`: full absolute path.
#'   - `uncovered`: Uncovered lines, in a matrix with two rows. Each
#'     column is an uncovered interval, the first row number has the
#'     beginnings, the second the ends of the intervals.
#'
#' @family code coverage reporting functions
#' @export

uncovered_lines <- function(cov) {
  cmt <- is.na(cov$coverage)
  zer <- !cmt & cov$coverage == 0
  stc <- cov$line
  stc[cmt] <- 0
  stc[zer] <- - stc[zer]
  perfile <- tapply(stc, cov$file, simplify = FALSE, function(x) {
    calculate_runs(x)
  })

  df <- data.frame(
    stringsAsFactors = FALSE,
    path = names(perfile),
    uncovered = I(unname(perfile))
  )

  class(df) <- unique(c("code_coverage_uncovered", "tbl", class(df)))
  df
}

# Summarize negative intervals in a vector

calculate_runs <- function(x) {
  x <- x[ x != 0 ]
  runs <- rle(x < 0)
  pos <- utils::head(cumsum(c(0, runs$lengths)), -1) + 1L
  beg <- pos[runs$values]
  end <- beg + runs$lengths[runs$values] - 1L
  abs(rbind(x[beg], x[end]))
}

filter_coverage <- function(cov, colname, include_paths = NULL) {
  if (length(include_paths) == 0) {
    return(cov)
  }

  include_paths <- as.character(include_paths)
  include_paths <- normalizePath(include_paths, mustWork = FALSE)
  keep <- rep(FALSE, nrow(cov))
  for (path in include_paths) {
    keep <- keep | startsWith(cov[[colname]], path)
  }

  cov[keep,]
}

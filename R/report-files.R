#' Show code coverage for one or more files, line by line
#'
#' @param cov Code coverage result, typically from the [parse_coverage()]
#'   function.
#' @param include_paths If not `NULL`, then it may be a character vector of
#'   paths, to only include files within these paths.
#' @return A list with class `files_coverage`. Each list element is a
#'   character vector with attibutes `filename`, `total`, `covered`,
#'   `coverage` and class `file_coverage`.
#'
#'   Both `files_coverage` and `file_coverage` have `format()` and
#'   `print()` methods.
#'
#' @family code coverage reporting functions
#' @export

file_coverage <- function(cov, include_paths = NULL) {
  cov <- filter_coverage(cov, "file", include_paths)
  files <- unique(cov$file)
  structure(
    lapply(files, file_coverage_1, cov = cov),
    names = files,
    class = "files_coverage"
  )
}

#' @param x `file_coverage` or `files_coverage` object.
#' @param ... Currently ignored.
#' @rdname file_coverage
#' @export

format.files_coverage <- function(x, ...) {
  unlist(lapply(x, format, ...))
}

#' @rdname file_coverage
#' @export

format.file_coverage <- function(x, ...) {
  cyan <- cli::make_ansi_style("#1a3939", bg = TRUE)
  header <- paste0(
    " ", format(attr(x, "covered")),
    " / ", format(attr(x, "total")),
    " (", round(attr(x, "coverage") * 100), "%", ")",
    "  ", attr(x, "filename")
  )
  nc <- max(cli::ansi_nchar(x, type = "width"))
  r <- cli::rule(width = nc)
  c(r, cyan(cli::ansi_align(header, width = nc)), r, x, "")
}

#' @rdname file_coverage
#' @export

print.file_coverage <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @param pager Whether to show the result in the system pager.
#'   The default tries to guess when this makes sense. It also tries to
#'   guess whether the default pager can show multiple files.
#' @rdname file_coverage
#' @export

print.files_coverage <- function(x,
    pager = interactive() && length(sys.calls()) <= 2, ...) {
  if (pager) {
    pager(lapply(x, format, ...))
  } else {
    writeLines(format(x, ...))
  }
  invisible(x)
}

file_coverage_1 <- function(cov, file) {
  cov <- cov[cov$file == file, ]

  comm <- is.na(cov$coverage)
  yeah <- !comm & cov$coverage > 0
  nope <- !comm & cov$coverage == 0

  lno <- format(cov$line)
  num <- ansi_format(ifelse(
    !is.na(cov$coverage),
    ifelse(
      cov$coverage == 0,
      cli::bg_magenta("!!"),
      cli::col_green(paste0(format(cov$coverage), "x"))
    ),
    ""
  ))
  if (cli::num_ansi_colors() >= 256) {
    style_yeah <- cli::make_ansi_style("#0b230b", bg = TRUE)
    style_nope <- cli::make_ansi_style("#5f2d40", bg = TRUE)
  } else {
    style_yeah <- style_nope <- function(x) x
  }
  code <- ansi_format(cli::code_highlight(cov$code))
  code <- ifelse(
    comm,
    code,
    ifelse(
      yeah,
      style_yeah(code),
      style_nope(code)
    )
  )

  bar <- cli::col_grey("|")
  lines <- paste(cli::col_grey(lno), bar, num, bar, code)

  nyeah <- sum(yeah)
  nnope <- sum(nope)
  attributes(lines) <- list(
    filename = file,
    total = nyeah + nnope,
    covered = nyeah,
    coverage = if (nnope == 0) 1.0 else nyeah / (nyeah + nnope),
    class = "file_coverage"
  )

  lines
}

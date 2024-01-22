#' Remove ignored code lines from code coverage data
#'
#' It ignores
#'
#' * all files matching the globs in `ignore_files`.
#' * all lines between a `# nocov start` and `# nocov end` comment.
#' * all lines that have a `# nocov` comment.
#'
#' It warns for unpaired `# nocov start` and `# nocov end` comments.
#'
#' @param cov Code coverage object, typically from the
#'   [parse_coverage()] function.
#' @param root Project root, the globs in `ignore_files` are interpreted
#'   relative to this directory.
#' @param ignore_files Character vector of globs
#'
#' @export

ignore_coverage <- function(cov, root = ".", ignore_files = NULL) {

  for (ptn in ignore_files) {
    ign <- normalizePath(Sys.glob(file.path(root, ptn)))
    bad1 <- cov$file %in% ign
    cov <- cov[! bad1, ]

    igndir <- paste0(ign, "/")
    for (ign1 in igndir) {
      bad2 <- startsWith(cov$file, ign1)
      cov <- cov[! bad2, ]
    }
  }

  beg <- grep("# nocov start", fixed = TRUE, cov$code)
  end <- grep("# nocov end", fixed = TRUE, cov$code)
  one <- setdiff(grep("# nocov", fixed = TRUE, cov$code), c(beg, end))

  cov$coverage[one] <- NA_integer_

  bp <- 1L
  ep <- 1L
  bad <- logical(nrow(cov))
  while (bp <= length(beg) && ep <= length(end)) {
    bv <- beg[bp]
    ev <- end[ep]

    if (cov$file[bv] != cov$file[ev]) {
      if (bv < ev) {
        warning(
          "# nocov start w/o # nocov end ignored in ",
          cov$file[bv], ":", cov$line[bv]
        )
        bp <- bp + 1L
      } else {
        warning(
          "# nocov end w/o # nocov start ignored in ",
          cov$file[ev], ":", cov$line[ev]
        )
        ep <- ep + 1L
      }
      next
    }

    if (bv > ev) {
      warning(
        "# nocov end without # nocov start ignored in ",
        cov$file[ev], ":", cov$line[ev]
      )
      ep <- ep + 1L
    }

    bad[bv:ev] <- TRUE
    bp <- bp + 1L
    ep <- ep + 1L
  }

  while (bp <= length(beg)) {
    warning(
      "# nocov start w/o nocov end ignored in ",
      cov$file[beg[bp]], ":", cov$line[beg[bp]]
    )
    bp <- bp + 1L
  }
  while (ep <= length(end)) {
    warning(
      "# nocov end w/o nocov start ignored in ",
      cov$file[end[ep]], ":", cov$line[end[ep]]
    )
    ep <- ep + 1L
  }

  if (any(bad)) {
    cov <- cov[!bad, ]
  }

  cov
}

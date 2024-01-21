pager <- function(lines) {
  if (!is.list(lines)) lines <- list(lines)
  tmp <- paste0(tempfile(), "-", seq_along(lines))
  on.exit(unlink(tmp), add = TRUE)
  mapply(lines, tmp, FUN = function(l, f) writeLines(l, f))

  old <- Sys.getenv("LESS", "")
  Sys.setenv(LESS = paste(old, "-R"))
  on.exit(Sys.setenv(LESS = old), add = TRUE)
  if (.Platform$OS.type == "unix" && !is.function(getOption("pager"))) {
    system2(Sys.getenv("PAGER"), tmp)
  } else {
    file.show(tmp)
  }
}

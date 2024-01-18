run_gcov <- function(path, recursive = TRUE) {
  gcno <- dir(
    path,
    recursive = TRUE,
    pattern = "[.]gcno$",
    full.names = TRUE
  )
  dirs <- dirname(gcno)
  udirs <- unique(dirs)
  pxs <- lapply(udirs, function(d) {
    fnms <- basename(gcno[dirs == d])
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

parse_gcov_file <- function(path) {
  df <- .Call(c_parse_gcov, path)
  as.data.frame(df)
}
`%||%` <- function(l, r) if (is.null(l)) r else l

# Split lines into a list based on the line directives in the file.
split_on_line_directives <- function(lines) {
  matches <- rex::re_matches(
    lines,
    rex::rex(
      start, any_spaces, "#line", spaces,
      capture(name = "line_number", digit), spaces,
      quotes, capture(name = "filename", anything), quotes
    )
  )
  directive_lines <- which(!is.na(matches$line_number))
  file_starts <- directive_lines + 1
  file_ends <- c(directive_lines[-1] - 1, length(lines))
  res <- mapply(function(start, end) lines[start:end], file_starts, file_ends)
  names(res) <- stats::na.omit(matches$filename)
  res
}

try_compile <- function(fn) {
  tryCatch(
    compiler::cmpfun(fn),
    error = function(e) fn
  )
}

is_locked_env <- function(env) {
  .Call(c_is_locked_env, env)
}

lock_env <- function(env) {
  .Call(c_lock_env, env)
}

unlock_env <- function(env) {
  .Call(c_unlock_env, env)
}

pkg_env <- function(package) {
  nm <- paste0("package:", package)
  e <- if (nm %in% search()) as.environment(nm)
  e
}

ns_env <- function(package) {
  e <- if (package %in% loadedNamespaces()) asNamespace(package)
  e
}

read_lines <- function(path) {
  .Call(c_read_lines, path)
}

na_omit <- function(x) {
  x[!is.na(x)]
}

# Common prefix for a bunch of paths

common_prefix <- function(x) {
  x <- unique(x)
  paths <- strsplit(x, "/", fixed = TRUE)
  pre <- ""
  i <- 1L
  while (TRUE) {
    cmp <- vapply(paths, "[", "", i)
    if (any(is.na(cmp))) {
      break;
    }
    ucmp <- unique(cmp)
    if (length(ucmp) != 1L) {
      break
    }
    pre <- paste0(pre, ucmp, "/")
    i <- i + 1L
  }

  pre
}

remove_common_prefix <- function(x) {
  pre <- common_prefix(x)
  remove_prefix(x, pre)
}

remove_prefix <- function(x, pre) {
  bx <- substr(x, nchar(pre) + 1L, nchar(x))
  bx
}

common_prefixes <- function(x) {
  x <- unique(x)
  ret <- sub("/$", "", common_prefix(x))
  nxt <- setdiff(unique(dirname(x)), c(x, ret))
  while (TRUE) {
    ret <- c(ret, nxt)
    if (length(nxt) <= 1) break
    nxt <- setdiff(unique(dirname(nxt)), ret)
  }
  sort(ret)
}

ansi_format <- function(x) {
  nc <- cli::ansi_nchar(x, type = "width")
  cli::ansi_align(x, width = max(nc))
}
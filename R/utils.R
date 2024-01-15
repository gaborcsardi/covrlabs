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

lock_env <- function(env) {
  .Call(c_lock_env, env)
}

unlock_env <- function(env) {
  .Call(c_unlock_env, env)
}

find_functions <- local({
  is_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }

  #' Find all functions in an environment or package
  #'
  #' @param env An environment or package name to find functions in.
  #'
  #' @export

  find_functions <- function(env) {
    stopifnot(is.environment(env) || is_string(env))
    if (is_string(env)) {
      env <- asNamespace(env)
    }
  }

  find_functions
})

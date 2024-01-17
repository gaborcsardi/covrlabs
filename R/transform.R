#' Transform an object
#'
#' This function is experimental.
#'
#' @param object R object to transform.
#' @param closxp_callback If not `NULL`, then it must be a callback
#'   fucntion to transform closures. It is called for every function, but
#'   it is not called for functions nested into other functions.
#'
#' @return The transformed object.
#'
#' @family transform functions
#' @export

transform_object <- function(object, closxp_callback = NULL) {
  .Call(
    c_transform,
    object,
    environment(),
    closxp_callback
  )
}

#' Transform all objects in an environment
#'
#' This function is experimental.
#'
#' @param env Environment to transform
#' @param skip Character vector of object names to skip.
#' @inheritParams transform_object
#'
#' @family transform functions
#' @export

transform_env <- function(env, skip = NULL, closxp_callback = NULL) {
  nms <- setdiff(ls(env, all.names = TRUE), skip)
  unlock_env(env)
  .Call(
    c_transform_env,
    env,
    nms,
    environment(),
    closxp_callback
  )
}

#' Transform all objects in a package
#'
#' @param package Package name of the package to transform. If it is not
#'   loaded, `transform_package` will load it.
#' @inheritParams transform_env
#'
#' @family transform functions
#' @export

transform_package <- function(package, skip = ".__NAMESPACE__.",
                              closxp_callback = NULL) {
  pkgenv <- pkg_env(package) %||% new.env(parent = emptyenv())
  nsenv <- ns_env(package)
  if (is.null(nsenv)) loadNamespace(package)
  pkg_nms <- if (!is.null(pkgenv)) {
    unlock_env(pkgenv)
    setdiff(ls(pkgenv, all.names = TRUE), skip)
  }
  ns_nms <- if (!is.null(nsenv)) {
    unlock_env(nsenv)
    setdiff(ls(nsenv, all.names = TRUE), skip)
  }
  .Call(
    c_transform_pkg,
    pkgenv,
    nsenv,
    pkg_nms,
    ns_nms,
    environment(),
    closxp_callback
  )
}

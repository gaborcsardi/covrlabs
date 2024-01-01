#' @useDynLib covrlabs, .registration = TRUE
#' @import rex
NULL

.onLoad <- function(libname, pkgname) {
  the$native_encoding <- native_encoding()
  rex::register_shortcuts("covrlabs")
}

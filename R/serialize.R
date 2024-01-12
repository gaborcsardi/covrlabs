#' Serialize R objects
#'
#' Equivalent to base R' serialization format, with parameters
#' `connection = NULL`, `ascii = FALSE`, `xdr = FALSE`, `version = 2`,
#' `refhook = NULL`.
#'
#' @param object Object to serialize.
#' @param closxp_callback If not `NULL`, then it must be a callback
#'   function to transform closures. It is called for every function. It is
#'   also called on functions embedded into functions. (First on the outer
#'   function, and if it returns a function that embeds functions, then
#'   on those.)
#' @return `serialize_to_raw()` returns a raw vector.
#'
#' @seealso [base::serialize()], [base::unserialize()].
#' @export

serialize_to_raw <- function(object, closxp_callback = NULL) {
  .Call(
    c_serialize,
    object,
    the$native_encoding,
    environment(),
    closxp_callback
  )
}

#' @rdname serialize_to_raw
#' @param path File to write the serialization to. `serialize_to_file()`
#'   writes to a temporary file first, and then renames that file. This is
#'   avoid creating an invalid output file.
#' @return `serialize_to_file()` returns `NULL`.
#' @export

serialize_to_file <- function(object, path, closxp_callback = NULL) {
  tmp_path <- paste0(path, "-tmp-", Sys.getpid())
  .Call(
    c_serialize_file,
    object,
    path,
    tmp_path,
    the$native_encoding,
    environment(),
    closxp_callback
  )
}
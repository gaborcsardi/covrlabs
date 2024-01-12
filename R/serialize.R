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
#' @return Raw vector, serialization of `object`.
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

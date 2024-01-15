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
  invisible(.Call(
    c_serialize_file,
    object,
    path,
    tmp_path,
    the$native_encoding,
    environment(),
    closxp_callback
  ))
}

#' Serialize an R object into an `.rds` file
#'
#' This function is similar to [base::saveRDS()], but it uses a
#' different serialization implementation. Differences:
#'
#' * Slightly faster implementation, in most cases.
#' * Multi-byte output is in host byte-order (the same as choosing
#'   `xdr = FALSE` for [base::serialize()]).
#' * No compression.
#' * It always uses workspace format 2, and expands ALTREP objects.
#' * Has a hook for closures, the `closxp_callback` argument.
#' * Does not have a hook for reference objects.
#'
#' @param object R object to serialize.
#' @param file Output file.
#' @inheritParams serialize_to_raw
#'
#' @seealso [serialize_to_raw()], [base::saveRDS()], [base::readRDS()].
#' @export

save_rds <- function(object, file, closxp_callback = NULL) {
  serialize_to_file(object, file, closxp_callback)
}

#' Serialize an environment into a raw vector
#'
#' This function is even more experimental than this experimental package.
#'
#' @param env Enviropnment to serialize.
#' @param skip Names of objects in `env` to skip.
#' @inheritParams serialize_to_raw
#'
#' @seealso [base::save()]
#' @export

save_env_to_raw <- function(env, skip = c(".__NAMESPACE__."),
                            closxp_callback = NULL) {
  nms <- setdiff(ls(env, all.names = TRUE), skip)
  .Call(
    c_save_env_to_raw,
    env,
    nms,
    the$native_encoding,
    environment(),
    closxp_callback
  )
}

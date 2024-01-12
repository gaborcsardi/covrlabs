base_serialize <- function(
    object,
    connection = NULL,
    ascii = FALSE,
    xdr = FALSE,
    version = 2) {
  base::serialize(
    object,
    connection = connection,
    ascii = ascii,
    xdr = xdr,
    version = version
  )
}

expect_same_serialization <- function(object) {
  testthat::expect_equal(
    serialize_to_raw(object),
    base_serialize(object)
  )
}

read_bin <- function(path) {
  readBin(path, what = "raw", n = file.size(path))
}

expect_same_serialization_file <- function(object) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  serialize_to_file(object, tmp)
  testthat::expect_equal(
    read_bin(tmp),
    base_serialize(object)
  )
}

remove_source <- function(x) {
  if (is.function(x)) {
    body(x) <- remove_source(body(x))
    x
  } else if (is.call(x)) {
    attr(x, "srcref") <- NULL
    attr(x, "wholeSrcref") <- NULL
    attr(x, "srcfile") <- NULL
    x[] <- lapply(x, remove_source)
    x
  } else {
    x
  }
}

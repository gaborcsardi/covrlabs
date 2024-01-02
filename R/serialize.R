the <- new.env(parent = emptyenv())

native_encoding <- function() {
  out <- base::serialize(NULL, xdr = FALSE, connection = NULL)
  enclen <- readBin(out[15:18], n = 1, what = "integer")
  rawToChar(out[19:(18 + enclen)])
}

serialize <- function(object) {
  .Call(c_serialize, object, the$native_encoding)
}

missing_arg <- function() {
  .Call(c_missing_arg)
}

unbound_value <- function() {
  .Call(c_unbound_value)
}

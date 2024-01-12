the <- new.env(parent = emptyenv())

# Not easy to query the native encoding...

native_encoding <- function() {
  out <- base::serialize(NULL, xdr = FALSE, connection = NULL)
  enclen <- readBin(out[15:18], n = 1, what = "integer")
  rawToChar(out[19:(18 + enclen)])
}

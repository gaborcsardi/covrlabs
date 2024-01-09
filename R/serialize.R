the <- new.env(parent = emptyenv())

native_encoding <- function() {
  out <- base::serialize(NULL, xdr = FALSE, connection = NULL)
  enclen <- readBin(out[15:18], n = 1, what = "integer")
  rawToChar(out[19:(18 + enclen)])
}

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

serialize <- function(object, closxp_callback = NULL) {
  .Call(
    c_serialize,
    object,
    the$native_encoding,
    environment(),
    closxp_callback
  )
}

missing_arg <- function() {
  .Call(c_missing_arg)
}

unbound_value <- function() {
  .Call(c_unbound_value)
}

bnd_cell_int <- function(val = 0L) {
  .Call(c_bnd_cell_int, val)
}

bnd_cell_lgl <- function(val = FALSE) {
  .Call(c_bnd_cell_lgl, val)
}

bnd_cell_real <- function(val = 0) {
  .Call(c_bnd_cell_real, val)
}

charsxp <- function(val = "") {
  .Call(c_charsxp, val)
}

anysxp <- function() {
  .Call(c_anysxp)
}

xptrsxp <- function(tag = NULL, prot = NULL) {
  .Call(c_xptrsxp, tag, prot)
}

weakrefsxp <- function(key = NULL, val = NULL, fin = NULL, onexit = FALSE) {
  .Call(c_weakrefsxp, key, val, fin, onexit)
}

sexprec <- function(x) {
  cres <- .Call(c_sexprec, x)
  ints <- as.integer(cres$bytes)
  cres$header <- list(
    type = bitwAnd(ints[1], 0x1f),
    scalar = bitwAnd(bitwShiftR(ints[1], 5), 0x1),
    obj = bitwAnd(bitwShiftR(ints[1], 6), 0x1),
    alt = bitwAnd(bitwShiftR(ints[1], 7), 0x1),
    gp = ints[2] + 0x100 * ints[3],
    mark = bitwAnd(ints[4], 0x1),
    debug = bitwAnd(bitwShiftR(ints[4], 1), 0x1),
    trace = bitwAnd(bitwShiftR(ints[4], 2), 0x1),
    spare = bitwAnd(bitwShiftR(ints[4], 3), 0x1),
    gcgen = bitwAnd(bitwShiftR(ints[4], 4), 0x1),
    gccls = bitwAnd(bitwShiftR(ints[4], 5), 0xff),
    named = ints[5] + 0x100 * ints[6],
    extra = ints[7] + 0x100 * ints[8]
  )

  structure(cres, class = "sexprec_summary")
}

sexp_types <- read.table(
  header = TRUE,
  stringsAsFactors = FALSE,
  textConnection("
    number type       rname       vector description
    0      NILSXP     NULL        FALSE  NULL
    1      SYMSXP     symbol      FALSE  symbols
    2      LISTSXP    pairlist    FALSE  pairlists
    3      CLOSXP     closure     FALSE  closures
    4      ENVSXP     environment FALSE  environments
    5      PROMSXP    promise     FALSE  promises
    6      LANGSXP    language    FALSE  'language objects'
    7      SPECIALSXP special     FALSE  'special functions'
    8      BUILTINSXP builtin     FALSE  'builtin functions'
    9      CHARSXP    char        FALSE  'internal character strings'
    10     LGLSXP     logical     TRUE   'logical vectors'
    11     NA         NA          NA     NA
    12     NA         NA          NA     NA
    13     INTSXP     integer     TRUE   'integer vectors'
    14     REALSXP    double      TRUE   'numeric vectors'
    15     CPLXSXP    complex     TRUE   'complex vectors'
    16     STRSXP     character   TRUE   'character vectors'
    17     DOTSXP     ...         FALSE  'dot-dot-dot object'
    18     ANYSXP     any         FALSE  'make \"any\" args work'
    19     VECSXP     list        TRUE   'list (generic vector)'
    20     EXPRSXP    expression  TRUE   'expression vector'
    21     BCODESXP   bytecode    FALSE  'byte code'
    22     EXTPTRSXP  externalptr FALSE  'external pointer'
    23     WEAKREFSXP weakref     TRUE   'weak reference'
    24     RAWSXP     raw         TRUE   'raw vector'
    25     S4SXP      S4          FALSE  'S4 classes not of simple type'
")
)

is_vec_sexp <- function(x) {
  tp <- typeof(x)
  wh <- match(tp, sexp_types$rname)
  if (is.na(wh)) {
    stop("Unknown type: ", tp)
  }
  sexp_types$vector[wh]
}

hex <- function(bytes) {
  if (.Platform$endian == "little") {
    bytes <- rev(bytes)
  }
  while (length(bytes) > 1 && bytes[1] == 0L) {
    bytes <- utils::tail(bytes, -1)
  }
  paste0(c("0x", format(bytes)), collapse = "")
}

#' @export

format.sexprec_summary <- function(x, ...) {
  h <- x$header
  ftypename <- sub("SXP", "", format(sexp_types$type)[h$type + 1L])

  df <- data.frame(
    stringsAsFactors = FALSE,
    TYPE = paste(h$type, ftypename),
    SOA = paste0(h$scalar, h$obj, h$alt),
    GP = paste0("0x", format(as.hexmode(h$gp), width = 2)),
    MDTSG = paste0(h$mark, h$debug, h$trace, h$spare, h$gcgen),
    GCC = paste0("0x", as.hexmode(h$gcc)),
    NAMED = paste0("0x", format(as.hexmode(h$named), width = 4)),
    EXTRA = paste0("0x", format(as.hexmode(h$extra), width = 4))
  )

  ps <- .Machine$sizeof.pointer
  typename <- sub("SXP", "", sexp_types$type[h$type + 1L])
  if (x$vector) {
    xls <- x$xlent_size
    df$LEN <- paste0("0x", format(as.hexmode(x$length), width = xls))
    df$TRUELEN <- paste0("0x", format(as.hexmode(x$truelength), width = xls))
  } else if (typename %in% c("BUILTIN", "SPECIAL")) {
    df$OFFSET <- paste0("0x", format(as.hexmode(x$offset), width = 4))
  } else {
    ptr1 <- hex(utils::tail(utils::head(x$bytes, -2 * ps), ps))
    ptr2 <- hex(utils::tail(utils::head(x$bytes, -ps), ps))
    ptr3 <- hex(utils::tail(x$bytes, ps))
    if (typename == "SYM") {
      df$PNAME <- ptr1
      df$VALUE <- ptr2
      df$INTERNAL <- ptr3
    } else if (typename == "ENV") {
      df$FRAME <- ptr1
      df$ENCLOS <- ptr2
      df$HASHTAB <- ptr3
    } else if (typename == "CLO") {
      df$FORMALS <- ptr1
      df$BODY <- ptr2
      df$ENV <- ptr3
    } else if (typename == "PROMISE") {
      df$VALUE <- ptr1
      df$EXPR <- ptr2
      df$ENV <- ptr3
    } else {
      df$CAR <- ptr1
      df$CDR <- ptr2
      df$TAG <- ptr3
    }
  }

  cols <- mapply(colnames(df), df, FUN = c, SIMPLIFY = FALSE)
  cols <- lapply(cols, format)
  c(
    paste(vapply(cols, "[[", "", 1), collapse = " "),
    paste(vapply(cols, "[[", "", 2), collapse = " ")
  )
}

#' @export

print.sexprec_summary <- function(x, ...) {
  writeLines(format(x, ...))
}

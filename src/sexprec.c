#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "bnd-cells.h"
#include "serialize.h"

SEXP c_missing_arg(void) {
  return R_MissingArg;
}

SEXP c_unbound_value(void) {
  return R_UnboundValue;
}

int is_vector(SEXP x) {
  switch (TYPEOF(x)) {
  case CHARSXP:
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case VECSXP:
  case EXPRSXP:
  case RAWSXP:
  case WEAKREFSXP:
    return 1;
    break;
  default:
    return 0;
    break;
  }
}

SEXP c_sexprec(SEXP x) {
  int v = is_vector(x);
  int ss = sizeof(*x);
  int s = v ? ss - 3 * sizeof(SEXP) + sizeof(R_xlen_t) * 2 : ss;
  const char *nms[] = {
    "vector", "bytes", "length", "truelength", "xlent_size",
    "bndcell_type", "offset", "" };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  SET_VECTOR_ELT(res, 0, Rf_ScalarLogical(v));
  SET_VECTOR_ELT(res, 1, Rf_allocVector(RAWSXP, s));
  memcpy(RAW(VECTOR_ELT(res, 1)), x, s);
  if (v) {
    SET_VECTOR_ELT(res, 2, Rf_ScalarReal(XLENGTH(x)));
    SET_VECTOR_ELT(res, 3, Rf_ScalarReal(TRUELENGTH(x)));
  } else {
    SET_VECTOR_ELT(res, 2, Rf_ScalarReal(NA_REAL));
    SET_VECTOR_ELT(res, 3, Rf_ScalarReal(NA_REAL));
  }
  SET_VECTOR_ELT(res, 4, Rf_ScalarInteger(sizeof(R_xlen_t)));
  if (BNDCELL_TAG(x)) {
    SET_VECTOR_ELT(res, 5, Rf_ScalarInteger(BNDCELL_TAG(x)));
  } else {
    SET_VECTOR_ELT(res, 5, Rf_ScalarInteger(NA_INTEGER));
  }
  if (TYPEOF(x) == BUILTINSXP || TYPEOF(x) == SPECIALSXP) {
    SET_VECTOR_ELT(res, 6, Rf_ScalarInteger(x->u.primsxp.offset));
  } else {
    SET_VECTOR_ELT(res, 6, Rf_ScalarInteger(NA_INTEGER));
  }
  UNPROTECT(1);
  return res;
}

SEXP c_charsxp(SEXP x) {
  return Rf_duplicate(STRING_ELT(x, 0));
}

SEXP c_anysxp(void) {
  SEXP res = Rf_allocSExp(ANYSXP);
  return res;
}

SEXP c_xptrsxp(SEXP tag, SEXP prot) {
  SEXP ptr = R_MakeExternalPtr(NULL, tag, prot);
  return ptr;
}

SEXP c_weakrefsxp(SEXP key, SEXP val, SEXP fin, SEXP onexit) {
  int conexit = LOGICAL(onexit)[0];
  SEXP wref = R_MakeWeakRef(key, val, fin, conexit);
  return wref;
}

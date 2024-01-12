#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

// ------------------------------------------------------------------------
// Immediate Binding Values
// ------------------------------------------------------------------------

#include "bnd-cells.h"
#include "serialize.h"

void (SET_BNDCELL_IVAL)(SEXP cell, int v) {
  SET_BNDCELL_IVAL((cell), (v));
}

void (INIT_BNDCELL)(SEXP cell, int type) {
  INIT_BNDCELL((cell), (type));
}

SEXP c_bnd_cell_int(SEXP val) {
  SEXP cell = PROTECT(Rf_allocSExp(LISTSXP));
  // Leaking memory here!
  R_PreserveObject(cell);
  INIT_BNDCELL(cell, INTSXP);
  SET_BNDCELL_IVAL(cell, INTEGER(val)[0]);
  UNPROTECT(1);
  return cell;
}

SEXP c_bnd_cell_lgl(SEXP val) {
  SEXP cell = PROTECT(Rf_allocSExp(LISTSXP));
  // Leaking memory here!
  R_PreserveObject(cell);
  INIT_BNDCELL(cell, LGLSXP);
  SET_BNDCELL_LVAL(cell, LOGICAL(val)[0]);
  UNPROTECT(1);
  return cell;
}

SEXP c_bnd_cell_real(SEXP val) {
  SEXP cell = PROTECT(Rf_allocSExp(LISTSXP));
  // Leaking memory here!
  R_PreserveObject(cell);
  INIT_BNDCELL(cell, REALSXP);
  SET_BNDCELL_DVAL(cell, REAL(val)[0]);
  UNPROTECT(1);
  return cell;
}

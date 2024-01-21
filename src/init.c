#include "serialize.h"

SEXP c_read_file_raw(SEXP path);
SEXP c_read_lines(SEXP path);
SEXP c_parse_gcov(SEXP path);

SEXP c_find_last_line(SEXP bytes);

// ------------------------------------------------------------------------

static const R_CallMethodDef callMethods[]  = {
  { "c_serialize",       (DL_FUNC) &c_serialize,       4 },
  { "c_serialize_file",  (DL_FUNC) &c_serialize_file,  6 },
  { "c_save_env_to_raw", (DL_FUNC) &c_save_env_to_raw, 5 },
  { "c_transform",       (DL_FUNC) &c_transform,       3 },
  { "c_transform_env",   (DL_FUNC) &c_transform_env,   4 },
  { "c_transform_pkg",   (DL_FUNC) &c_transform_pkg,   6 },

  { "c_lock_env",        (DL_FUNC) &c_lock_env,        1 },
  { "c_unlock_env",      (DL_FUNC) &c_unlock_env,      1 },
  { "c_is_locked_env",   (DL_FUNC) &c_is_locked_env,   1 },

  { "c_missing_arg",     (DL_FUNC) &c_missing_arg,     0 },
  { "c_unbound_value",   (DL_FUNC) &c_unbound_value,   0 },
  { "c_bnd_cell_int",    (DL_FUNC) &c_bnd_cell_int,    1 },
  { "c_bnd_cell_lgl",    (DL_FUNC) &c_bnd_cell_lgl,    1 },
  { "c_bnd_cell_real",   (DL_FUNC) &c_bnd_cell_real,   1 },
  { "c_sexprec",         (DL_FUNC) &c_sexprec,         1 },
  { "c_charsxp",         (DL_FUNC) &c_charsxp,         1 },
  { "c_anysxp",          (DL_FUNC) &c_anysxp,          0 },
  { "c_xptrsxp",         (DL_FUNC) &c_xptrsxp,         2 },
  { "c_weakrefsxp",      (DL_FUNC) &c_weakrefsxp,      4 },

  { "c_read_file_raw",   (DL_FUNC) &c_read_file_raw,   1 },
  { "c_read_lines",      (DL_FUNC) &c_read_lines,      1 },
  { "c_parse_gcov",      (DL_FUNC) &c_parse_gcov,      2 },

  { "c_find_last_line",  (DL_FUNC) &c_find_last_line,  1 },

  { NULL, NULL, 0 }
};

void R_init_covrlabs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

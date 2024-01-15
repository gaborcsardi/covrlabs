#include "serialize.h"

// ------------------------------------------------------------------------

static const R_CallMethodDef callMethods[]  = {
  { "c_serialize",       (DL_FUNC) &c_serialize,       4 },
  { "c_serialize_file",  (DL_FUNC) &c_serialize_file,  6 },
  { "c_save_env_to_raw", (DL_FUNC) &c_save_env_to_raw, 5 },

  { "c_lock_env",        (DL_FUNC) &c_lock_env,        1 },
  { "c_unlock_env",      (DL_FUNC) &c_unlock_env,      1 },

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
  { NULL, NULL, 0 }
};

void R_init_covrlabs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

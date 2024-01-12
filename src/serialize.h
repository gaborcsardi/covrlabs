#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP c_serialize(SEXP x, SEXP native_encoding, SEXP calling_env,
                 SEXP closxp_callback);

SEXP c_serialize_file(SEXP x, SEXP path, SEXP tmp_path,
                      SEXP natice_encoding, SEXP calling_env,
                      SEXP closxp_callback);

SEXP c_missing_arg(void);
SEXP c_unbound_value(void);
SEXP c_sexprec(SEXP x);
SEXP c_charsxp(SEXP x);
SEXP c_anysxp(void);
SEXP c_xptrsxp(SEXP tag, SEXP prot);
SEXP c_weakrefsxp(SEXP key, SEXP val, SEXP fin, SEXP onexit);

SEXP c_bnd_cell_int(SEXP val);
SEXP c_bnd_cell_lgl(SEXP val);
SEXP c_bnd_cell_real(SEXP val);

// ------------------------------------------------------------------------

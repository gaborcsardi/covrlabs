#define SERMODE raw
#include "serialize-tmpl.h"

// need to keep this in this file to allow compiler optimizations

R_INLINE void out_stream_write_raw(struct out_stream *os, void *addr,
                                   size_t n) {
  size_t req = os->len + n;
  if (req > os->true_len) {
    // pretty aggressive reallocation, we try to avoid it, if possible
    out_stream_realloc(os, req * 2);
  }
  memcpy(os->buf + os->len, addr, n);
  os->len += n;
}

void out_stream_init_raw(struct out_stream *os, size_t alloc_size) {
  os->buf = NULL;
  os->len = 0;
  os->true_len = 0;
  os->file = NULL;
  os->outfile = NULL;
  os->ignored = -1;
  os->closxp_callback = R_NilValue;
  out_stream_realloc(os, alloc_size);
}

SEXP c_serialize(SEXP x, SEXP native_encoding, SEXP calling_env,
                 SEXP closxp_callback) {
  // const char* cnative_encoding = CHAR(STRING_ELT(native_encoding, 0));
  struct out_stream os;
  out_stream_init_raw(&os, INITIAL_SIZE);
  os.calling_env = calling_env;
  os.closxp_callback = closxp_callback;
  os.smap = hmap_sexp_init();

  write_header_raw(&os);
  write_item_raw(&os, x);

  SEXP out = Rf_allocVector(RAWSXP, os.len);
  memcpy(RAW(out), os.buf, os.len);
  out_stream_drop(&os);
  return out;
}

SEXP c_save_env_to_raw(SEXP env, SEXP nms, SEXP native_encoding,
                       SEXP calling_env, SEXP closxp_callback) {
  struct out_stream os;
  out_stream_init_raw(&os, INITIAL_SIZE);
  os.calling_env = calling_env;
  os.closxp_callback = closxp_callback;
  os.smap = hmap_sexp_init();

  size_t i, len = XLENGTH(nms);
  SEXP pl, tpl;

  PROTECT(pl = Rf_allocList(len));
  for (tpl = pl, i = 0; i < len; i++, tpl = CDR(tpl)) {
    SET_TAG(tpl, Rf_installTrChar(STRING_ELT(nms, i)));
    SEXP val = Rf_findVar(TAG(tpl), env);
    if (TYPEOF(val) == PROMSXP) {
      PROTECT(val);
      val = Rf_eval(val, env);
      UNPROTECT(1);
    }
	  SETCAR(tpl, val);
  }

  write_string_raw(&os, "RDB2\n");
  write_header_raw(&os);
  write_item_raw(&os, pl);

  SEXP out = Rf_allocVector(RAWSXP, os.len);
  memcpy(RAW(out), os.buf, os.len);
  out_stream_drop(&os);
  UNPROTECT(1);
  return out;
}

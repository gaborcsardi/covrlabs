#define SERMODE file
#include "serialize-tmpl.h"

// need to keep this in this file to allow compiler optimizations

R_INLINE void out_stream_write_file(struct out_stream *os, void *addr,
                                    size_t n) {
  ssize_t nw = fwrite(addr, 1, n, os->outfile);
  if (nw < n) {
    out_stream_drop(os);
    R_THROW_POSIX_ERROR("Cannot write to file '%s'.", os->file);
  }
  os->len += n;
}

void out_stream_init_file(struct out_stream *os, const char *file) {
  os->buf = NULL;
  os->len = 0;
  os->true_len = 0;
  os->file = file;
  os->outfile = fopen(file, "wb+");
  if (!os->outfile) {
    R_THROW_POSIX_ERROR(
      "Cannot open output file '%s' for seialization",
      file
    );
  }
  if (BUFSIZ < 1024 * 64) {
    setvbuf(os->outfile, NULL, _IOFBF, 1024 * 64);
  }

  os->ignored = -1;
  os->closxp_callback = R_NilValue;
}

SEXP c_serialize_file(SEXP x, SEXP path, SEXP tmp_path,
                      SEXP natice_encoding, SEXP calling_env,
                      SEXP closxp_callback) {
  // const char* cnative_encoding = CHAR(STRING_ELT(native_encoding, 0));
  const char *cpath = CHAR(STRING_ELT(path, 0));
  const char *ctmp_path = CHAR(STRING_ELT(tmp_path, 0));
  struct out_stream os;
  out_stream_init_file(&os, ctmp_path);
  os.calling_env = calling_env;
  os.closxp_callback = closxp_callback;
  os.smap = hmap_sexp_init();

  write_header_file(&os);
  write_item_file(&os, x);

  out_stream_drop(&os);
  if (rename(ctmp_path, cpath) < 0) {
    R_THROW_POSIX_ERROR(
      "Could not rename serialization file '%s' to '%s'",
      ctmp_path,
      cpath
    );
  }

  return R_NilValue;
}

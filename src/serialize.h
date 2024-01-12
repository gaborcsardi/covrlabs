#ifndef SERIALIZE_H
#define SERIALIZE_H

#include <stdio.h>    // FILE

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
// internals

#define i_TYPE hmap_sexp, SEXP, int
#include "hmap.h"
#undef i_TYPE
#include "errors.h"
#include "bnd-cells.h"

#define REFSXP            255
#define NILVALUE_SXP      254
#define GLOBALENV_SXP     253
#define UNBOUNDVALUE_SXP  252
#define MISSINGARG_SXP    251
#define BASENAMESPACE_SXP 250
#define NAMESPACESXP      249
#define PACKAGESXP        248
#define PERSISTSXP        247
#define CLASSREFSXP       246
#define GENERICREFSXP     245
#define BCREPDEF          244
#define BCREPREF          243
#define EMPTYENV_SXP      242
#define BASEENV_SXP       241

#define CACHED_MASK (1<<5)
#define HASHASH_MASK 1

#define ENCODE_LEVELS(v) ((v) << 12)
#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)

#define PACK_REF_INDEX(i) (((i) << 8) | REFSXP)
#define UNPACK_REF_INDEX(i) ((i) >> 8)
#define MAX_PACKED_INDEX (INT_MAX >> 8)

struct out_stream {
  char *buf;
  size_t len;
  size_t true_len;
  const char *file;
  FILE *outfile;
  hmap_sexp smap;
  struct R_outpstream_st rstream;
  int header_size;
  int ignored;
  SEXP closxp_callback;
  SEXP calling_env;
};

void out_stream_drop(struct out_stream *os);
void out_stream_realloc(struct out_stream *os, size_t newsize);
void out_stream_write_raw(struct out_stream *os, void *addr, size_t n);
void out_stream_write_file(struct out_stream *os, void *addr, size_t n);
void out_stream_init(struct out_stream *os, size_t alloc_size);
void out_stream_init_file(struct out_stream *os, const char *file);

int get_flags(SEXP item);
SEXP findrep(SEXP x, SEXP reps);
SEXP scan_for_circles(SEXP item);
#define cons(a,b) Rf_cons(a,b)

#endif

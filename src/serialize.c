#include <stdio.h>

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#define i_TYPE hmap_sexp, SEXP, int
#include "hmap.h"
#undef i_TYPE
#include "errors.h"

#define REFSXP            255
#define NILVALUE_SXP      254
#define GLOBALENV_SXP     253
#define UNBOUNDVALUE_SXP  252
#define MISSINGARG_SXP    251
#define BASENAMESPACE_SXP 250
#define NAMESPACESXP      249
#define PACKAGESXP        248
#define PERSISTSXP        247

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
  char * buf;
  size_t len;
  hmap_sexp smap;
  FILE *stream;
};

void out_stream_init(struct out_stream *os) {
  os->buf = NULL;
  os->len = 1024 * 1024;
  os->stream = NULL;
}

void out_stream_drop(struct out_stream *os) {
  if (os->buf) free(os->buf);
  hmap_sexp_drop(&os->smap);
  if (os->stream) fclose(os->stream);
  os->buf = NULL;
  os->stream = NULL;
}

#define WRITE_BYTES(s, addr, size)                              \
  if (fwrite((addr), 1 ,(size), (s)->stream) == EOF) {          \
    out_stream_drop(s);                                         \
    R_THROW_POSIX_ERROR("Cannot write bytes to memory buffer"); \
  }

#define WRITE_INTEGER(s, i)              \
  do {                                   \
    int i_ = (i);                        \
    WRITE_BYTES((s), &(i_), sizeof(i_)); \
  } while (0)

#define WRITE_STRING(s, str)                                     \
  if (fputs((str), ((s)->stream)) == EOF) {                      \
    out_stream_drop(s);                                          \
    R_THROW_POSIX_ERROR("Cannot write string to memory buffer"); \
  }

#define WRITE_LENGTH(s, l)                 \
  do {                                     \
    if ((l) <= 2147483647) {               \
      WRITE_INTEGER(s, (l));               \
    } else {                               \
      WRITE_INTEGER(s, -1);                \
      WRITE_INTEGER(s, (l) / 4294967296L); \
      WRITE_INTEGER(s, (l) % 4294967296L); \
    }                                      \
  } while (0)

#define WRITE_VEC(s, addr, size, n)                                \
    if (fwrite((addr), (size), (n), (s)->stream) == EOF) {         \
      out_stream_drop(s);                                          \
      R_THROW_POSIX_ERROR("Cannot write vector to memory buffer"); \
    }

int get_flags(SEXP item) {
  int type = TYPEOF(item);
  int hasflags = type != NILSXP && type != SYMSXP && type != ENVSXP;
  if (!hasflags) return 0;

  int hasattr = hasflags && (type != CHARSXP && ATTRIB(item) != R_NilValue);
  int maytag = hasflags &&
    (type == LISTSXP || type == LANGSXP || type == PROMSXP || type == DOTSXP);
  int hastag = type == CLOSXP || (maytag && TAG(item) != R_NilValue);

  int flags;
  int levels = LEVELS(item);
  if (type == CHARSXP) levels &= (~(CACHED_MASK | HASHASH_MASK));
  flags = type | ENCODE_LEVELS(levels);

  if (OBJECT(item)) flags |= IS_OBJECT_BIT_MASK;
  if (hasattr) flags |= HAS_ATTR_BIT_MASK;
  if (hastag) flags |= HAS_TAG_BIT_MASK;

  return flags;
}

void write_item(struct out_stream *os, SEXP item) {
  int flags = get_flags(item);
  R_xlen_t len;
  int len0;
  size_t mapsize;
  int mapidx;
  const hmap_sexp_value *hashval = NULL;

  // It cannot be zero for non-NULL types
  if (flags != 0) WRITE_INTEGER(os, flags);

  switch (TYPEOF(item)) {

    case SYMSXP:
      hashval = hmap_sexp_get(&os->smap, item);
      if (hashval == NULL) {
        mapsize = hmap_sexp_size(&os->smap);
        hmap_sexp_insert(&os->smap, item, (int) mapsize);
        WRITE_INTEGER(os, SYMSXP);
        write_item(os, PRINTNAME(item));
      } else {
        mapidx = hashval->second + 1;
        if (mapidx > MAX_PACKED_INDEX) {
          WRITE_INTEGER(os, REFSXP);
          WRITE_INTEGER(os, mapidx);
        } else {
          WRITE_INTEGER(os, PACK_REF_INDEX(mapidx));
        }
      }
      break;

    case ENVSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
      break;

    case NILSXP:
      WRITE_INTEGER(os, NILVALUE_SXP);
      break;

    case CHARSXP:
      if (item == NA_STRING) {
		WRITE_INTEGER(os, -1);
      } else {
        len0 = LENGTH(item);
		WRITE_INTEGER(os, len0);
		WRITE_BYTES(os, CHAR(item), len0);
	  }
      break;

    case LGLSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      WRITE_VEC(os, LOGICAL(item), len, sizeof(LOGICAL(item)[0]));
      break;

    case INTSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      WRITE_VEC(os, INTEGER(item), len, sizeof(INTEGER(item)[0]));
      break;

    case REALSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      WRITE_VEC(os, REAL(item), len, sizeof(REAL(item)[0]));
      break;

    case VECSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      for (R_xlen_t i = 0; i < len; i++) {
		write_item(os, VECTOR_ELT(item, i));
      }
      break;

    default:
      REprintf("Ignoring uninmplemented type %i\n", TYPEOF(item));
      break;
  }


}

SEXP c_serialize(SEXP x, SEXP native_encoding) {
  const char*cnative_encoding = CHAR(STRING_ELT(native_encoding, 0));
  struct out_stream os;
  out_stream_init(&os);
  os.stream = open_memstream(&(os.buf), &(os.len));
  if (!os.stream) {
    R_THROW_POSIX_ERROR("Cannot open memory buffer for serialization");
  }

  WRITE_STRING(&os, "B\n");
  WRITE_INTEGER(&os, 3);
  WRITE_INTEGER(&os, R_VERSION);
  WRITE_INTEGER(&os, R_Version(3,5,0));
  WRITE_INTEGER(&os, strlen(cnative_encoding));
  WRITE_STRING(&os, cnative_encoding);

  os.smap = hmap_sexp_init();

  write_item(&os, x);

  fflush(os.stream);
  SEXP out = Rf_allocVector(RAWSXP, os.len);
  memcpy(RAW(out), os.buf, os.len);
  fclose(os.stream);
  free(os.buf);
  return out;
}

static const R_CallMethodDef callMethods[]  = {
  { "c_serialize", (DL_FUNC) &c_serialize, 2 },
  { NULL, NULL, 0 }
};

void R_init_covrlabs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

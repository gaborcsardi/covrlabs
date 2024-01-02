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
  char * buf;
  size_t len;
  hmap_sexp smap;
  FILE *stream;
  struct R_outpstream_st rstream;
  int ignored;
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
  // BCODESXP has flags, but we use base R to write it out, and that will
  // include the flags already.
  int hasflags = type != NILSXP && type != SYMSXP && type != ENVSXP &&
    type != BCODESXP;
  if (!hasflags) return 0;

  int hasattr = hasflags && (type != CHARSXP && ATTRIB(item) != R_NilValue);
  int maytag = hasflags &&
    (type == LISTSXP || type == LANGSXP || type == PROMSXP ||
    type == DOTSXP);
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

int write_hashed(struct out_stream *os, SEXP item) {
  const hmap_sexp_value *hashval = NULL;
  size_t mapsize;
  int mapidx;
  hashval = hmap_sexp_get(&os->smap, item);
  if (hashval == NULL) {
    mapsize = hmap_sexp_size(&os->smap);
    hmap_sexp_insert(&os->smap, item, (int) mapsize);
    return 0;
  } else {
    mapidx = hashval->second + 1;
    if (mapidx > MAX_PACKED_INDEX) {
      WRITE_INTEGER(os, REFSXP);
      WRITE_INTEGER(os, mapidx);
    } else {
      WRITE_INTEGER(os, PACK_REF_INDEX(mapidx));
    }
    return 1;
  }
}

void out_char_mem(R_outpstream_t stream, int c) {
  struct out_stream *os = (struct out_stream*) stream->data;
  if (os->ignored < 14) {
    os->ignored += 1;
  } else {
    char cc = (char) c;
    WRITE_BYTES(os, &cc, 1);
  }
}

void out_bytes_mem(R_outpstream_t stream, void *buf, int length) {
  struct out_stream *os = (struct out_stream*) stream->data;
  if (os->ignored < 14) {
    if (os->ignored + length <= 14) {
      os->ignored += length;
    } else {
      int omit = 14 - os->ignored;
      const char *cbuf = (const char*) buf;
      os->ignored = 14;
      WRITE_BYTES(os, cbuf + omit, length - omit);
    }
  } else {
    WRITE_BYTES(os, buf, length);
  }
}

void write_base_r(struct out_stream *os, SEXP item) {
  struct R_outpstream_st out = os->rstream;
  os->ignored = 0;
  R_Serialize(item, &out);
}

void write_item(struct out_stream *os, SEXP item) {
  R_xlen_t len;
  int len0;
  int hasattr;
  int flags;

tailcall:
  // It cannot be zero for non-NULL types, it also cannot be a type
  // that is hashed. Once we start hashing other types, we should omit
  // writing the flags for the hashed items.
  flags = get_flags(item);
  hasattr = flags & HAS_ATTR_BIT_MASK;
  if (flags != 0) WRITE_INTEGER(os, flags);

  switch (TYPEOF(item)) {
	case LISTSXP:
	case LANGSXP:
	case PROMSXP:
	case DOTSXP:
	    if (hasattr) write_item(os, ATTRIB(item));
	    if (TAG(item) != R_NilValue) write_item(os, TAG(item));
	    // TODO: if (BNDCELL_TAG(item)) R_expand_binding_value(item);
	    write_item(os, CAR(item));
        // recall with CDR
	    item = CDR(item);
	    goto tailcall;

    case CLOSXP:
	    if (hasattr) write_item(os, ATTRIB(item));
	    write_item(os, CLOENV(item));
	    write_item(os, FORMALS(item));
	    item = BODY(item);
	    goto tailcall;

    case SYMSXP:
      if (item == R_MissingArg) {
        WRITE_INTEGER(os, MISSINGARG_SXP);
      } else if (item == R_UnboundValue) {
        WRITE_INTEGER(os, UNBOUNDVALUE_SXP);
      } else if (!write_hashed(os, item)) {
        WRITE_INTEGER(os, SYMSXP);
        write_item(os, PRINTNAME(item));
      }
      break;

    case ENVSXP:
      if (item == R_EmptyEnv) {
        WRITE_INTEGER(os, EMPTYENV_SXP);
      }	else if (item == R_BaseEnv) {
        WRITE_INTEGER(os, BASEENV_SXP);
      } else if (item == R_GlobalEnv) {
        WRITE_INTEGER(os, GLOBALENV_SXP);
      } else if (item == R_BaseNamespace) {
        WRITE_INTEGER(os, BASENAMESPACE_SXP);
      } else if (!write_hashed(os, item)) {
	    if (R_IsPackageEnv(item)) {
	      SEXP name = R_PackageEnvName(item);
          WRITE_INTEGER(os, PACKAGESXP);
          WRITE_STRING(os, CHAR(STRING_ELT(name, 0)));
        } else if (R_IsNamespaceEnv(item)) {
          WRITE_INTEGER(os, NAMESPACESXP);
          WRITE_STRING(
            os,
            CHAR(STRING_ELT(PROTECT(R_NamespaceEnvSpec(item)), 0))
          );
          UNPROTECT(1);
        } else {
          WRITE_INTEGER(os, ENVSXP);
          WRITE_INTEGER(os, R_EnvironmentIsLocked(item) ? 1 : 0);
          write_item(os, ENCLOS(item));
	      write_item(os, FRAME(item));
	      write_item(os, HASHTAB(item));
	      write_item(os, ATTRIB(item));
        }
      }
      break;

    case EXTPTRSXP:
      if (!write_hashed(os, item)) {
        write_item(os, EXTPTR_PROT(item));
        write_item(os, EXTPTR_TAG(item));
      }
      break;

    case WEAKREFSXP:
      // this is just flags, but they have reference semantics
      write_hashed(os, item);
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

    case STRSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      for (R_xlen_t i = 0; i < len; i++) {
		write_item(os, STRING_ELT(item, i));
	  }
      break;

    case VECSXP:
      len = XLENGTH(item);
      WRITE_LENGTH(os, len);
      for (R_xlen_t i = 0; i < len; i++) {
		write_item(os, VECTOR_ELT(item, i));
      }
      break;

    case BCODESXP:
      write_base_r(os, item);
      break;

    case RAWSXP:
      len = XLENGTH(item);
	  WRITE_LENGTH(os, len);
      WRITE_BYTES(os, RAW(item), len);
      break;

    case S4SXP:
      // Nothing to do, attributes come later
      break;

    default:
      REprintf("Ignoring uninmplemented type %i\n", TYPEOF(item));
      break;
  }
  if (hasattr) write_item(os, ATTRIB(item));
}

SEXP c_serialize(SEXP x, SEXP native_encoding) {
  const char*cnative_encoding = CHAR(STRING_ELT(native_encoding, 0));
  struct out_stream os;
  out_stream_init(&os);
  os.stream = open_memstream(&(os.buf), &(os.len));
  if (!os.stream) {
    R_THROW_POSIX_ERROR("Cannot open memory buffer for serialization");
  }

  R_InitOutPStream(
    &os.rstream,
    (R_pstream_data_t) &os,
    R_pstream_binary_format,
    // we use version 2, because that is (almost) the same as version 3,
    // and its header has a fixed size. For ALTREP, we'll need version 3.
    /* version = */ 2,
    out_char_mem,
    out_bytes_mem,
    /* phook = */ NULL,
    /* pdata = */ R_NilValue
  );

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

SEXP c_missing_arg(void) {
  return R_MissingArg;
}

SEXP c_unbound_value(void) {
  return R_UnboundValue;
}

static const R_CallMethodDef callMethods[]  = {
  { "c_serialize",     (DL_FUNC) &c_serialize,     2 },
  { "c_missing_arg",   (DL_FUNC) &c_missing_arg,   0 },
  { "c_unbound_value", (DL_FUNC) &c_unbound_value, 0 },
  { NULL, NULL, 0 }
};

void R_init_covrlabs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

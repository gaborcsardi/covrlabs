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
  int header_size;
  int ignored;
  SEXP closxp_callback;
  SEXP calling_env;
};

void out_stream_init(struct out_stream *os) {
  os->buf = NULL;
  os->len = 1024 * 1024;
  os->stream = NULL;
  os->ignored = -1;
  os->closxp_callback = R_NilValue;
}

void out_stream_drop(struct out_stream *os) {
  if (os->buf) free(os->buf);
  hmap_sexp_drop(&os->smap);
  if (os->stream) fclose(os->stream);
  os->buf = NULL;
  os->stream = NULL;
}

#define WRITE_BYTES(s, addr, size)                                \
  do {                                                            \
    if (fwrite((addr), 1 ,(size), (s)->stream) == EOF) {          \
      out_stream_drop(s);                                         \
      R_THROW_POSIX_ERROR("Cannot write bytes to memory buffer"); \
  } } while (0)

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

void write_bc(struct out_stream *os, SEXP item);

int get_flags(SEXP item) {
  int type = TYPEOF(item);
  int hasflags = type != NILSXP && type != SYMSXP && type != ENVSXP;
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
  if (os->ignored < os->header_size) {
    if (os->ignored + length <= os->header_size) {
      os->ignored += length;
    } else {
      int omit = os->header_size - os->ignored;
      const char *cbuf = (const char*) buf;
      os->ignored = os->header_size;
      WRITE_BYTES(os, cbuf + omit, length - omit);
    }
  } else {
    WRITE_BYTES(os, buf, length);
  }
}

void write_base_r(struct out_stream *os, SEXP item) {
  if (os->ignored == -1) {
    R_InitOutPStream(
      &os->rstream,
      (R_pstream_data_t) os,
      R_pstream_binary_format,
      /* version = */ 2,
      out_char_mem,
      out_bytes_mem,
      /* phook = */ NULL,
      /* pdata = */ R_NilValue
    );
  }
  struct R_outpstream_st out = os->rstream;
  os->ignored = 0;
  R_Serialize(item, &out);
}

// ------------------------------------------------------------------------
// Immediate Binding Values
// ------------------------------------------------------------------------

// See https://github.com/wch/r-source/blob/trunk/doc/notes/immbnd.md

#if ( SIZEOF_SIZE_T < SIZEOF_DOUBLE )
# define BOXED_BINDING_CELLS 1
#else
# define BOXED_BINDING_CELLS 0
#endif

#if BOXED_BINDING_CELLS
/* Use allocated scalars to hold immediate binding values. A little
   less efficient but does not change memory layout or use. These
   allocated scalars must not escape their bindings. */
#define BNDCELL_DVAL(v) SCALAR_DVAL(CAR0(v))
#define BNDCELL_IVAL(v) SCALAR_IVAL(CAR0(v))
#define BNDCELL_LVAL(v) SCALAR_LVAL(CAR0(v))

#define SET_BNDCELL_DVAL(cell, dval) SET_SCALAR_DVAL(CAR0(cell), dval)
#define SET_BNDCELL_IVAL(cell, ival) SET_SCALAR_IVAL(CAR0(cell), ival)
#define SET_BNDCELL_LVAL(cell, lval) SET_SCALAR_LVAL(CAR0(cell), lval)

#define INIT_BNDCELL(cell, type) do {		\
	SEXP val = allocVector(type, 1);	\
	SETCAR(cell, val);			\
	INCREMENT_NAMED(val);			\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)

#else
/* Use a union in the CAR field to represent an SEXP or an immediate
   value.  More efficient, but changes the menory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
typedef union {
    SEXP sxpval;
    double dval;
    int ival;
} R_bndval_t;

#define BNDCELL_DVAL(v) ((R_bndval_t *) &CAR0(v))->dval
#define BNDCELL_IVAL(v) ((R_bndval_t *) &CAR0(v))->ival
#define BNDCELL_LVAL(v) ((R_bndval_t *) &CAR0(v))->ival

#define SET_BNDCELL_DVAL(cell, dval) (BNDCELL_DVAL(cell) = (dval))
#define SET_BNDCELL_IVAL(cell, ival) (BNDCELL_IVAL(cell) = (ival))
#define SET_BNDCELL_LVAL(cell, lval) (BNDCELL_LVAL(cell) = (lval))

#define INIT_BNDCELL(cell, type) do {		\
	if (BNDCELL_TAG(cell) == 0)		\
	    SETCAR(cell, R_NilValue);		\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)

#endif

#define BNDCELL_TAG(e)	((e)->sxpinfo.extra)
#define SET_BNDCELL_TAG(e, v) ((e)->sxpinfo.extra = (v))

#define NAMED_BITS 16

struct sxpinfo_struct {
    SEXPTYPE type      :  TYPE_BITS;
                            /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int scalar:  1;
    unsigned int obj   :  1;
    unsigned int alt   :  1;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* used on closures and when REFCNT is defined */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
    unsigned int named : NAMED_BITS;
    unsigned int extra : 32 - NAMED_BITS; /* used for immediate bindings */
}; /*		    Tot: 64 */

struct primsxp_struct {
    int offset;
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
} SEXPREC;

#define CAR0(e)		((e)->u.listsxp.carval)
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

void write_item(struct out_stream *os, SEXP item);

void write_binding_value(struct out_stream *os, SEXP item) {
  if (BNDCELL_TAG(item) == INTSXP) {
    SEXP item2 = PROTECT(Rf_ScalarInteger(BNDCELL_IVAL(item)));
    write_item(os, item2);
    UNPROTECT(1);
  } else if (BNDCELL_TAG(item) == REALSXP) {
    SEXP item2 = PROTECT(Rf_ScalarReal(BNDCELL_DVAL(item)));
    write_item(os, item2);
    UNPROTECT(1);
  } else if (BNDCELL_TAG(item) == LGLSXP) {
    SEXP item2 = PROTECT(Rf_ScalarLogical(BNDCELL_LVAL(item)));
    write_item(os, item2);
    UNPROTECT(1);
  }
}

#define CALLBACK_MARK 32

void write_item(struct out_stream *os, SEXP item) {
  R_xlen_t len;
  int len0;
  int hasattr;
  int flags;

tailcall:
  flags = get_flags(item);
  hasattr = flags & HAS_ATTR_BIT_MASK;

  switch (TYPEOF(item)) {
	case LISTSXP:
	case LANGSXP:
	case PROMSXP:
	case DOTSXP:
    WRITE_INTEGER(os, flags);
	  if (hasattr) write_item(os, ATTRIB(item));
	  if (TAG(item) != R_NilValue) write_item(os, TAG(item));
	  if (BNDCELL_TAG(item)) {
      write_binding_value(os, item);
    } else {
      write_item(os, CAR(item));
    }
    // recall with CDR
	  item = CDR(item);
	  goto tailcall;

  case CLOSXP:
    if (!Rf_isNull(os->closxp_callback) && LEVELS(item) != CALLBACK_MARK) {
      int lvls = LEVELS(item);
      SEXP call = PROTECT(Rf_lang2(os->closxp_callback, item));
      SEXP item = PROTECT(Rf_eval(call, os->calling_env));
      // mark, so there is no infinite recursion
      SETLEVELS(item, CALLBACK_MARK);
      write_item(os, item);
      SETLEVELS(item, lvls);
      UNPROTECT(2);
      // return to avoid writing out attributes
      return;
    } else {
    WRITE_INTEGER(os, flags);
      if (hasattr) write_item(os, ATTRIB(item));
      write_item(os, CLOENV(item));
      write_item(os, FORMALS(item));
      item = BODY(item);
	    goto tailcall;
    }

  case SYMSXP:
    // no flags!
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
    // no flags!
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
        WRITE_INTEGER(os, 0);
        len = XLENGTH(name);
        WRITE_LENGTH(os, len);
        for (R_xlen_t i = 0; i < len; i++) {
          write_item(os, STRING_ELT(name, i));
        }
      } else if (R_IsNamespaceEnv(item)) {
        WRITE_INTEGER(os, NAMESPACESXP);
        WRITE_INTEGER(os, 0);
        SEXP name = PROTECT(R_NamespaceEnvSpec(item));
        len = XLENGTH(name);
        WRITE_LENGTH(os, len);
        for (R_xlen_t i = 0; i < len; i++) {
          write_item(os, STRING_ELT(name, i));
        }
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
    WRITE_INTEGER(os, flags);
    if (!write_hashed(os, item)) {
      write_item(os, EXTPTR_PROT(item));
      write_item(os, EXTPTR_TAG(item));
    }
    break;

  case WEAKREFSXP:
    WRITE_INTEGER(os, flags);
    // this is just flags, but they have reference semantics
    write_hashed(os, item);
    break;

	case SPECIALSXP:
	case BUILTINSXP:
    // we can't do these because PRIMNAME() is internal
    write_base_r(os, item);
    break;

  case NILSXP:
    // no flags!
    WRITE_INTEGER(os, NILVALUE_SXP);
    break;

  case CHARSXP:
    WRITE_INTEGER(os, flags);
    if (item == NA_STRING) {
      WRITE_INTEGER(os, -1);
    } else {
      len0 = LENGTH(item);
		  WRITE_INTEGER(os, len0);
      WRITE_BYTES(os, CHAR(item), len0);
	  }
    break;

  case LGLSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    WRITE_VEC(os, LOGICAL(item), len, sizeof(LOGICAL(item)[0]));
    break;

  case INTSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    WRITE_VEC(os, INTEGER(item), len, sizeof(INTEGER(item)[0]));
    break;

  case REALSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    WRITE_VEC(os, REAL(item), len, sizeof(REAL(item)[0]));
    break;

  case CPLXSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    WRITE_VEC(os, COMPLEX(item), len, sizeof(COMPLEX(item)[0]));
    break;

  case STRSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    for (R_xlen_t i = 0; i < len; i++) {
      write_item(os, STRING_ELT(item, i));
	  }
    break;

  case VECSXP:
	case EXPRSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    for (R_xlen_t i = 0; i < len; i++) {
      write_item(os, VECTOR_ELT(item, i));
    }
    break;

  case BCODESXP:
    WRITE_INTEGER(os, flags);
    write_bc(os, item);
    break;

  case RAWSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    WRITE_BYTES(os, RAW(item), len);
    break;

  case S4SXP:
    WRITE_INTEGER(os, flags);
    // Nothing to do, attributes come later
    break;

  default:
    Rf_error("Uninmplemented type %i\n", TYPEOF(item));
    break;
  }
  if (hasattr) write_item(os, ATTRIB(item));
}

SEXP c_serialize(SEXP x, SEXP native_encoding, SEXP calling_env,
                 SEXP closxp_callback) {
  // const char* cnative_encoding = CHAR(STRING_ELT(native_encoding, 0));
  struct out_stream os;
  out_stream_init(&os);
  os.calling_env = calling_env;
  os.closxp_callback = closxp_callback;
  os.stream = open_memstream(&(os.buf), &(os.len));
  if (!os.stream) {
    R_THROW_POSIX_ERROR("Cannot open memory buffer for serialization");
  }

  WRITE_STRING(&os, "B\n");
  WRITE_INTEGER(&os, 2);
  WRITE_INTEGER(&os, R_VERSION);
  WRITE_INTEGER(&os, R_Version(2,3,0));
  // WRITE_INTEGER(&os, strlen(cnative_encoding));
  // WRITE_STRING(&os, cnative_encoding);

  fflush(os.stream);
  os.header_size = os.len;
  os.smap = hmap_sexp_init();

  write_item(&os, x);

  fflush(os.stream);
  SEXP out = Rf_allocVector(RAWSXP, os.len);
  memcpy(RAW(out), os.buf, os.len);
  fclose(os.stream);
  free(os.buf);
  return out;
}

// ------------------------------------------------------------------------

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

// ------------------------------------------------------------------------

static const R_CallMethodDef callMethods[]  = {
  { "c_serialize",     (DL_FUNC) &c_serialize,     4 },
  { "c_missing_arg",   (DL_FUNC) &c_missing_arg,   0 },
  { "c_unbound_value", (DL_FUNC) &c_unbound_value, 0 },
  { "c_bnd_cell_int",  (DL_FUNC) &c_bnd_cell_int,  1 },
  { "c_bnd_cell_lgl",  (DL_FUNC) &c_bnd_cell_lgl,  1 },
  { "c_bnd_cell_real", (DL_FUNC) &c_bnd_cell_real, 1 },
  { "c_sexprec",       (DL_FUNC) &c_sexprec,       1 },
  { "c_charsxp",       (DL_FUNC) &c_charsxp,       1 },
  { "c_anysxp",        (DL_FUNC) &c_anysxp,        0 },
  { "c_xptrsxp",       (DL_FUNC) &c_xptrsxp,       2 },
  { "c_weakrefsxp",    (DL_FUNC) &c_weakrefsxp,    4 },
  { NULL, NULL, 0 }
};

void R_init_covrlabs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

// ------------------------------------------------------------------------
// BYTECODE
// ------------------------------------------------------------------------

#define HASHSIZE 1099
#define PTRHASH(obj) (((R_size_t) (obj)) >> 2)
#define cons(a,b) Rf_cons(a,b)
typedef size_t R_size_t;
#define ATTRLANGSXP 240
#define ATTRLISTSXP 239
SEXP R_bcEncode(SEXP bytes);
SEXP R_bcDecode(SEXP code);
#define BCODE_CODE(x)	CAR(x)

SEXP make_circle_hash_table(void) {
  return CONS(R_NilValue, Rf_allocVector(VECSXP, HASHSIZE));
}

Rboolean add_circle_hash(SEXP item, SEXP ct) {
  SEXP table, bucket, list;

  table = CDR(ct);
  R_size_t pos = PTRHASH(item) % LENGTH(table);
  bucket = VECTOR_ELT(table, pos);
  for (list = bucket; list != R_NilValue; list = CDR(list)) {
    if (TAG(list) == item) {
      if (CAR(list) == R_NilValue) {
        /* this is the second time; enter in list and mark */
        SETCAR(list, R_UnboundValue); /* anything different will do */
        SETCAR(ct, CONS(item, CAR(ct)));
      }
      return TRUE;
    }
  }

    /* If we get here then this is a new item; enter in the table */
    bucket = CONS(R_NilValue, bucket);
    SET_TAG(bucket, item);
    SET_VECTOR_ELT(table, pos, bucket);
    return FALSE;
}

void scan_for_circles_(SEXP item, SEXP ct) {
  switch (TYPEOF(item)) {
  case LANGSXP:
  case LISTSXP:
    if (! add_circle_hash(item, ct)) {
	    scan_for_circles_(CAR(item), ct);
	    scan_for_circles_(CDR(item), ct);
	  }
    break;

  case BCODESXP: {
      int i, n;
      SEXP consts = BCODE_CONSTS(item);
      n = LENGTH(consts);
      for (i = 0; i < n; i++)
	    scan_for_circles_(VECTOR_ELT(consts, i), ct);
    }
	  break;

  default:
    break;
  }
}

SEXP scan_for_circles(SEXP item) {
  SEXP ct;
  PROTECT(ct = make_circle_hash_table());
  scan_for_circles_(item, ct);
  UNPROTECT(1);
  return CAR(ct);
}

SEXP findrep(SEXP x, SEXP reps) {
  for (; reps != R_NilValue; reps = CDR(reps)) {
    if (x == CAR(reps)) {
	    return reps;
    }
  }
  return R_NilValue;
}

void write_bc_lang(struct out_stream *os, SEXP item, SEXP reps) {
  int type = TYPEOF(item);
  if (type == LANGSXP || type == LISTSXP) {
    SEXP r = findrep(item, reps);
    int output = TRUE;
    if (r != R_NilValue) {
	    /* we have a cell referenced more than once */
	    if (TAG(r) == R_NilValue) {
		    /* this is the first reference, so update and register
		       the counter */
        int i = INTEGER(CAR(reps))[0]++;
        SET_TAG(r, Rf_allocVector(INTSXP, 1));
        INTEGER(TAG(r))[0] = i;
        WRITE_INTEGER(os, BCREPDEF);
        WRITE_INTEGER(os, i);
	    } else {
        /* we've seen it before, so just put out the index */
        WRITE_INTEGER(os, BCREPREF);
        WRITE_INTEGER(os, INTEGER(TAG(r))[0]);
        output = FALSE;
	    }
	  }

    if (output) {
      SEXP attr = ATTRIB(item);
      if (attr != R_NilValue) {
        switch(type) {
        case LANGSXP: type = ATTRLANGSXP; break;
        case LISTSXP: type = ATTRLISTSXP; break;
        }
	    }
      WRITE_INTEGER(os, type);
	    if (attr != R_NilValue) {
        write_item(os, attr);
      }
      write_item(os, TAG(item));
	    write_bc_lang(os, CAR(item), reps);
	    write_bc_lang(os, CDR(item), reps);
    }
  } else {
    WRITE_INTEGER(os, 0); /* pad */
    write_item(os, item);
  }
}

void write_bc_(struct out_stream *os, SEXP item, SEXP reps) {
  int i, n;
  SEXP code, consts;
  PROTECT(code = R_bcDecode(BCODE_CODE(item)));
  write_item(os, code);
  consts = BCODE_CONSTS(item);
  n = LENGTH(consts);
  WRITE_INTEGER(os, n);
  for (i = 0; i < n; i++) {
	  SEXP c = VECTOR_ELT(consts, i);
    int type = TYPEOF(c);
    switch (type) {
    case BCODESXP:
      WRITE_INTEGER(os, type);
      write_bc_(os, c, reps);
      break;
    case LANGSXP:
    case LISTSXP:
      write_bc_lang(os, c, reps);
      break;
    default:
      WRITE_INTEGER(os, type);
      write_item(os, c);
    }
  }
  UNPROTECT(1);
}

void write_bc(struct out_stream *os, SEXP item) {
  SEXP reps = scan_for_circles(item);
  PROTECT(reps = CONS(R_NilValue, reps));
  WRITE_INTEGER(os, Rf_length(reps));
  SETCAR(reps, Rf_allocVector(INTSXP, 1));
  INTEGER(CAR(reps))[0] = 0;
  write_bc_(os, item, reps);
  UNPROTECT(1);
}

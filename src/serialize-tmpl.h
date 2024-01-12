#include "serialize.h"

#ifndef SERMODE
#error 'SERMODE' must be defined
#endif

#define CONCAT2x(a,b) a ## _ ## b
#define CONCAT2(a,b) CONCAT2x(a,b)
#define FUN(c) CONCAT2(c,SERMODE)

#define WRITE_BYTES(s, addr, size) \
  FUN(out_stream_write)((s), (char*) (addr), (size))

#define WRITE_INTEGER(s, i)                   \
  do {                                        \
    int i_ = (i);                             \
    WRITE_BYTES((s), &(i_), sizeof(i_)); \
  } while (0)

#define WRITE_STRING(s, str) \
  FUN(out_stream_write)((s), (char*) (str), strlen(str))

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

#define WRITE_VEC(s, addr, size, n) \
  FUN(out_stream_write)((s), (addr), size * n)

void FUN(write_bc)(struct out_stream *os, SEXP item);

int FUN(write_hashed)(struct out_stream *os, SEXP item) {
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

void FUN(out_char_mem)(R_outpstream_t stream, int c) {
  struct out_stream *os = (struct out_stream*) stream->data;
  if (os->ignored < 14) {
    os->ignored += 1;
  } else {
    char cc = (char) c;
    WRITE_BYTES(os, &cc, 1);
  }
}

void FUN(out_bytes_mem)(R_outpstream_t stream, void *buf, int length) {
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

void FUN(write_base_r)(struct out_stream *os, SEXP item) {
  if (os->ignored == -1) {
    R_InitOutPStream(
      &os->rstream,
      (R_pstream_data_t) os,
      R_pstream_binary_format,
      /* version = */ 2,
      FUN(out_char_mem),
      FUN(out_bytes_mem),
      /* phook = */ NULL,
      /* pdata = */ R_NilValue
    );
  }
  struct R_outpstream_st out = os->rstream;
  os->ignored = 0;
  R_Serialize(item, &out);
}

void FUN(write_item)(struct out_stream *os, SEXP item);

void FUN(write_binding_value)(struct out_stream *os, SEXP item) {
  if (BNDCELL_TAG(item) == INTSXP) {
    SEXP item2 = PROTECT(Rf_ScalarInteger(BNDCELL_IVAL(item)));
    FUN(write_item)(os, item2);
    UNPROTECT(1);
  } else if (BNDCELL_TAG(item) == REALSXP) {
    SEXP item2 = PROTECT(Rf_ScalarReal(BNDCELL_DVAL(item)));
    FUN(write_item)(os, item2);
    UNPROTECT(1);
  } else if (BNDCELL_TAG(item) == LGLSXP) {
    SEXP item2 = PROTECT(Rf_ScalarLogical(BNDCELL_LVAL(item)));
    FUN(write_item)(os, item2);
    UNPROTECT(1);
  }
}

#define CALLBACK_MARK 32

void FUN(write_item)(struct out_stream *os, SEXP item) {
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
	  if (hasattr) FUN(write_item)(os, ATTRIB(item));
	  if (TAG(item) != R_NilValue) FUN(write_item)(os, TAG(item));
	  if (BNDCELL_TAG(item)) {
      FUN(write_binding_value)(os, item);
    } else {
      FUN(write_item)(os, CAR(item));
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
      FUN(write_item)(os, item);
      SETLEVELS(item, lvls);
      UNPROTECT(2);
      // return to avoid writing out attributes
      return;
    } else {
      WRITE_INTEGER(os, flags);
      if (hasattr) FUN(write_item)(os, ATTRIB(item));
      FUN(write_item)(os, CLOENV(item));
      FUN(write_item)(os, FORMALS(item));
      item = BODY(item);
	    goto tailcall;
    }

  case SYMSXP:
    // no flags!
    if (item == R_MissingArg) {
      WRITE_INTEGER(os, MISSINGARG_SXP);
    } else if (item == R_UnboundValue) {
      WRITE_INTEGER(os, UNBOUNDVALUE_SXP);
    } else if (!FUN(write_hashed)(os, item)) {
      WRITE_INTEGER(os, SYMSXP);
      FUN(write_item)(os, PRINTNAME(item));
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
    } else if (!FUN(write_hashed)(os, item)) {
      if (R_IsPackageEnv(item)) {
        SEXP name = R_PackageEnvName(item);
        WRITE_INTEGER(os, PACKAGESXP);
        WRITE_INTEGER(os, 0);
        len = XLENGTH(name);
        WRITE_LENGTH(os, len);
        for (R_xlen_t i = 0; i < len; i++) {
          FUN(write_item)(os, STRING_ELT(name, i));
        }
      } else if (R_IsNamespaceEnv(item)) {
        WRITE_INTEGER(os, NAMESPACESXP);
        WRITE_INTEGER(os, 0);
        SEXP name = PROTECT(R_NamespaceEnvSpec(item));
        len = XLENGTH(name);
        WRITE_LENGTH(os, len);
        for (R_xlen_t i = 0; i < len; i++) {
          FUN(write_item)(os, STRING_ELT(name, i));
        }
        UNPROTECT(1);
      } else {
        WRITE_INTEGER(os, ENVSXP);
        WRITE_INTEGER(os, R_EnvironmentIsLocked(item) ? 1 : 0);
        FUN(write_item)(os, ENCLOS(item));
        FUN(write_item)(os, FRAME(item));
        FUN(write_item)(os, HASHTAB(item));
        FUN(write_item)(os, ATTRIB(item));
      }
    }
    break;

  case EXTPTRSXP:
    WRITE_INTEGER(os, flags);
    if (!FUN(write_hashed)(os, item)) {
      FUN(write_item)(os, EXTPTR_PROT(item));
      FUN(write_item)(os, EXTPTR_TAG(item));
    }
    break;

  case WEAKREFSXP:
    WRITE_INTEGER(os, flags);
    // this is just flags, but they have reference semantics
    FUN(write_hashed)(os, item);
    break;

	case SPECIALSXP:
	case BUILTINSXP:
    // we can't do these because PRIMNAME() is internal
    FUN(write_base_r)(os, item);
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
      FUN(write_item)(os, STRING_ELT(item, i));
	  }
    break;

  case VECSXP:
	case EXPRSXP:
    WRITE_INTEGER(os, flags);
    len = XLENGTH(item);
    WRITE_LENGTH(os, len);
    for (R_xlen_t i = 0; i < len; i++) {
      FUN(write_item)(os, VECTOR_ELT(item, i));
    }
    break;

  case BCODESXP:
    WRITE_INTEGER(os, flags);
    FUN(write_bc)(os, item);
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
  if (hasattr) FUN(write_item)(os, ATTRIB(item));
}

#define INITIAL_SIZE (1024 * 1024)

void FUN(write_header)(struct out_stream *os) {
  WRITE_STRING(os, "B\n");
  WRITE_INTEGER(os, 2);
  WRITE_INTEGER(os, R_VERSION);
  WRITE_INTEGER(os, R_Version(2,3,0));
  // FUN(WRITE_INTEGER)(&os, strlen(cnative_encoding));
  // FUN(WRITE_STRING)(&os, cnative_encoding);
  os->header_size = os->len;
}

// ------------------------------------------------------------------------
// BYTECODE
// ------------------------------------------------------------------------

#define ATTRLANGSXP 240
#define ATTRLISTSXP 239
SEXP R_bcEncode(SEXP bytes);
SEXP R_bcDecode(SEXP code);
#define BCODE_CODE(x)	CAR(x)

void FUN(write_bc_lang)(struct out_stream *os, SEXP item, SEXP reps) {
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
        FUN(write_item)(os, attr);
      }
      FUN(write_item)(os, TAG(item));
	    FUN(write_bc_lang)(os, CAR(item), reps);
	    FUN(write_bc_lang)(os, CDR(item), reps);
    }
  } else {
    WRITE_INTEGER(os, 0); /* pad */
    FUN(write_item)(os, item);
  }
}

void FUN(write_bc_)(struct out_stream *os, SEXP item, SEXP reps) {
  int i, n;
  SEXP code, consts;
  PROTECT(code = R_bcDecode(BCODE_CODE(item)));
  FUN(write_item)(os, code);
  consts = BCODE_CONSTS(item);
  n = LENGTH(consts);
  WRITE_INTEGER(os, n);
  for (i = 0; i < n; i++) {
	  SEXP c = VECTOR_ELT(consts, i);
    int type = TYPEOF(c);
    switch (type) {
    case BCODESXP:
      WRITE_INTEGER(os, type);
      FUN(write_bc_)(os, c, reps);
      break;
    case LANGSXP:
    case LISTSXP:
      FUN(write_bc_lang)(os, c, reps);
      break;
    default:
      WRITE_INTEGER(os, type);
      FUN(write_item)(os, c);
    }
  }
  UNPROTECT(1);
}

void FUN(write_bc)(struct out_stream *os, SEXP item) {
  SEXP reps = scan_for_circles(item);
  PROTECT(reps = CONS(R_NilValue, reps));
  WRITE_INTEGER(os, Rf_length(reps));
  SETCAR(reps, Rf_allocVector(INTSXP, 1));
  INTEGER(CAR(reps))[0] = 0;
  FUN(write_bc_)(os, item, reps);
  UNPROTECT(1);
}

#undef WRITE_BYTES
#undef WRITE_INTEGER
#undef WRITE_STRING
#undef WRITE_LENGTH
#undef WRITE_VEC

#undef CONCAT2x
#undef FUN
#undef SERMODE

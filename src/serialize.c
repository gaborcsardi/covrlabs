#include "serialize.h"

void out_stream_drop(struct out_stream *os) {
  if (os->buf) free(os->buf);
  if (os->fd >= 0) close(os->fd);
  hmap_sexp_drop(&os->smap);
  os->buf = NULL;
  os->len = os->true_len = 0;
}

R_INLINE void out_stream_realloc(struct out_stream *os, size_t newsize) {
  void *newbuf = realloc(os->buf, newsize);
  if (newbuf == NULL) {
    out_stream_drop(os);
    R_THROW_POSIX_ERROR("Cannot allocate memory for serialization");
  }
  os->buf = newbuf;
  os->true_len = newsize;
}

R_INLINE int get_flags(SEXP item) {
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

// ------------------------------------------------------------------------
// Output independent part of byte code serializer

#define HASHSIZE 1099
#define PTRHASH(obj) (((R_size_t) (obj)) >> 2)
typedef size_t R_size_t;

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

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#define i_TYPE hmap_sexp_sexp, SEXP, SEXP
#include "hmap.h"
#undef i_TYPE

#include "errors.h"
#include "serialize.h"

struct trans_state {
  hmap_sexp_sexp smap;
  SEXP closxp_callback;
  SEXP calling_env;
  int changed;
};

SEXP is_hashed(struct trans_state *st, SEXP item) {
  const hmap_sexp_sexp_value *hashval = NULL;
  hashval = hmap_sexp_sexp_get(&st->smap, item);
  if (hashval == NULL) {
    return R_NilValue;
  } else {
    return hashval->second;
  }
}

// TODO:
// * LANGSXP / PROMSXP / DOTSXP (?)
// * Attributes might contain functions

SEXP transform_item(struct trans_state *st, SEXP item) {
  SEXP item2 = is_hashed(st, item);
  if (!Rf_isNull(item2)) {
    return item2;
  }

  switch (TYPEOF(item)) {
  case CLOSXP:
    if (!Rf_isNull(st->closxp_callback)) {
      SEXP call = PROTECT(Rf_lang2(st->closxp_callback, item));
      item2 = PROTECT(Rf_eval(call, st->calling_env));
      hmap_sexp_sexp_insert(&st->smap, item, item2);
      // this might be the only way to get to cloenv()
      SET_CLOENV(item2, transform_item(st, CLOENV(item2)));
      item = item2;
      UNPROTECT(2);
      st->changed = 1;
    } else {
      // do nothing with it. there might be other stuff in the environment,
      // so once we have callbacks for other SEXP types, we need to
      // iteratate on CLOENV(item).
    }
    break;

  case ENVSXP:
    if (item == R_EmptyEnv || item == R_BaseEnv || item == R_GlobalEnv ||
        item == R_BaseNamespace || R_IsPackageEnv(item) ||
        R_IsNamespaceEnv(item)) {
      // nothing to do with these
      break;
    } else {
      SEXP item2 = PROTECT(Rf_allocSExp(ENVSXP));
      hmap_sexp_sexp_insert(&st->smap, item, item2);
      SET_FRAME(item2, transform_item(st, FRAME(item)));
      SET_ENCLOS(item2, transform_item(st, ENCLOS(item)));
      SET_HASHTAB(item2, transform_item(st, HASHTAB(item)));
      SET_ATTRIB(item2, transform_item(st, ATTRIB(item)));
      item = item2;
      UNPROTECT(1);
    }
    break;

  case VECSXP: {
    size_t i, n = XLENGTH(item);
    SEXP item2 = PROTECT(Rf_allocVector(VECSXP, n));
    for (i = 0; i < n; i++) {
      SET_VECTOR_ELT(item2, i, transform_item(st, VECTOR_ELT(item, i)));
    }
    SET_ATTRIB(item2, transform_item(st, ATTRIB(item)));
    hmap_sexp_sexp_insert(&st->smap, item, item2);
    item = item2;
    UNPROTECT(1);
  }
  break;

  case LISTSXP: {
    size_t i, n = 0;
    SEXP tmp = item;
    while (!Rf_isNull(tmp)) {
      n++;
      tmp = CDR(tmp);
    }

    SEXP item2 = PROTECT(Rf_allocVector(LISTSXP, n));
    for (tmp = item2, i = 0; i < n; i++, tmp = CDR(tmp), item = CDR(item)) {
      SET_TAG(tmp, TAG(item));
      SETCAR(tmp, transform_item(st, CAR(item)));
    }
    SET_ATTRIB(item2, transform_item(st, ATTRIB(item)));
    hmap_sexp_sexp_insert(&st->smap, item, item2);
    item = item2;
    UNPROTECT(1);
  }
  break;

  case LANGSXP:
  case PROMSXP:
  case DOTSXP:
  case SYMSXP:
  case EXTPTRSXP:
  case WEAKREFSXP:
  case SPECIALSXP:
  case BUILTINSXP:
  case NILSXP:
  case CHARSXP:
  case LGLSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case EXPRSXP:
  case BCODESXP:
  case RAWSXP:
  case S4SXP:
  default: {
    if (ATTRIB(item) != R_NilValue) {
      SEXP attr2 = PROTECT(transform_item(st, ATTRIB(item)));
      if (attr2 != ATTRIB(item)) {
        SEXP item2 = PROTECT(Rf_duplicate(item));
        SET_ATTRIB(item2, attr2);
        hmap_sexp_sexp_insert(&st->smap, item, item2);
        item = item2;
        UNPROTECT(1);
      }
      UNPROTECT(1);
    }
    }
    break;
  }

  return item;
}

SEXP new_env(void) {
  SEXP env;
  PROTECT(env = Rf_allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
  UNPROTECT(1);
  return env;
}

SEXP c_transform(SEXP x, SEXP calling_env, SEXP closxp_callback) {
  struct trans_state state;
  state.calling_env = calling_env;
  state.closxp_callback = closxp_callback;
  state.smap = hmap_sexp_sexp_init();
  state.changed = 0;

  SEXP out = transform_item(&state, x);

  hmap_sexp_sexp_drop(&state.smap);
  return out;
}

// NOTES:
// * Axtive bindinds need special treatment. We save their function in
//   another environment and create another active binding.
// * It would be faster to manually iterate over the environment, instead
//   of iterating over the names.

SEXP c_transform_env(SEXP env, SEXP nms, SEXP calling_env,
                     SEXP closxp_callback) {
  struct trans_state state;
  state.calling_env = calling_env;
  state.closxp_callback = closxp_callback;
  state.smap = hmap_sexp_sexp_init();
  state.changed = 0;

  size_t i, len = XLENGTH(nms);
  SEXP pl, tpl, ab;

  PROTECT(pl = Rf_allocList(len));
  PROTECT(ab = new_env());

  for (tpl = pl, i = 0; i < len; i++, tpl = CDR(tpl)) {
    SET_TAG(tpl, Rf_installTrChar(STRING_ELT(nms, i)));
    SEXP val;
    if (R_BindingIsActive(TAG(tpl), env)) {
      Rf_defineVar(
        TAG(tpl),
        transform_item(&state, R_ActiveBindingFunction(TAG(tpl), env)),
        ab
      );
      val = PROTECT(R_NilValue);
    } else {
      val = PROTECT(Rf_findVar(TAG(tpl), env));
      if (TYPEOF(val) == PROMSXP) {
        UNPROTECT(1);
        val = PROTECT(Rf_eval(val, env));
      }
    }
    SETCAR(tpl, transform_item(&state, val));
    UNPROTECT(1);
  }

  for (tpl = pl, i = 0; i < len; i++, tpl = CDR(tpl)) {
    SEXP nm = TAG(tpl);
    R_unLockBinding(nm, env);
    if (!R_BindingIsActive(nm, env)) {
      Rf_defineVar(nm, CAR(tpl), env);
    } else {
      R_MakeActiveBinding(nm, Rf_findVar(nm, ab), env);
    }
  }

  UNPROTECT(2);
  hmap_sexp_sexp_drop(&state.smap);
  return R_NilValue;
}

// See NOTES for c_transform_env above.

SEXP c_transform_pkg(SEXP pkgenv, SEXP nsenv, SEXP pkgnms,
                     SEXP nsnms, SEXP calling_env,
                     SEXP closxp_callback) {
  struct trans_state state;
  state.calling_env = calling_env;
  state.closxp_callback = closxp_callback;
  state.smap = hmap_sexp_sexp_init();
  state.changed = 0;

  size_t i, len = XLENGTH(pkgnms), len2 = XLENGTH(nsnms);
  SEXP pl, pl2, tpl;
  SEXP ab1, ab2;

  PROTECT(pl = Rf_allocList(len));
  PROTECT(pl2 = Rf_allocList(len2));
  PROTECT(ab1 = new_env());
  PROTECT(ab2 = new_env());

  for (tpl = pl, i = 0; i < len; i++, tpl = CDR(tpl)) {
    SET_TAG(tpl, Rf_installTrChar(STRING_ELT(pkgnms, i)));
    SEXP val;
    if (R_BindingIsActive(TAG(tpl), pkgenv)) {
      Rf_defineVar(
        TAG(tpl),
        transform_item(&state, R_ActiveBindingFunction(TAG(tpl), pkgenv)),
        ab1
      );
      val = PROTECT(R_NilValue);
    } else {
      val = PROTECT(Rf_findVar(TAG(tpl), pkgenv));
      if (TYPEOF(val) == PROMSXP) {
        UNPROTECT(1);
        val = PROTECT(Rf_eval(val, pkgenv));
      }
    }
    SETCAR(tpl, transform_item(&state, val));
    UNPROTECT(1);
  }

  for (tpl = pl2, i = 0; i < len2; i++, tpl = CDR(tpl)) {
    SET_TAG(tpl, Rf_installTrChar(STRING_ELT(nsnms, i)));
    SEXP val;
    if (R_BindingIsActive(TAG(tpl), nsenv)) {
      Rf_defineVar(
        TAG(tpl),
        transform_item(&state, R_ActiveBindingFunction(TAG(tpl), nsenv)),
        ab2
      );
      val = PROTECT(R_NilValue);
    } else {
      val = PROTECT(Rf_findVar(TAG(tpl), nsenv));
      if (TYPEOF(val) == PROMSXP) {
        UNPROTECT(1);
        val = PROTECT(Rf_eval(val, nsenv));
      }
    }
    SETCAR(tpl, transform_item(&state, val));
    UNPROTECT(1);
  }

  for (tpl = pl, i = 0; i < len; i++, tpl = CDR(tpl)) {
    SEXP nm = TAG(tpl);
    R_unLockBinding(nm, pkgenv);
    if (!R_BindingIsActive(nm, pkgenv)) {
      Rf_defineVar(nm, CAR(tpl), pkgenv);
    } else {
      R_MakeActiveBinding(nm, Rf_findVar(nm, ab1), pkgenv);
    }
  }

  for (tpl = pl2, i = 0; i < len2; i++, tpl = CDR(tpl)) {
    SEXP nm = TAG(tpl);
    R_unLockBinding(nm, nsenv);
    if (!R_BindingIsActive(nm, nsenv)) {
      Rf_defineVar(nm, CAR(tpl), nsenv);
    } else {
      R_MakeActiveBinding(nm, Rf_findVar(nm, ab2), nsenv);
    }
  }

  UNPROTECT(4);
  hmap_sexp_sexp_drop(&state.smap);
  return R_NilValue;
}

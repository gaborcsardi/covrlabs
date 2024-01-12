#ifndef BND_CELLS_H
#define BND_CELLS_H

// See https://github.com/wch/r-source/blob/trunk/doc/notes/immbnd.md

#define BNDCELL_TAG(e)	((e)->sxpinfo.extra)
#define SET_BNDCELL_TAG(e, v) ((e)->sxpinfo.extra = (v))

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

SEXP c_bnd_cell_int(SEXP val);
SEXP c_bnd_cell_lgl(SEXP val);
SEXP c_bnd_cell_real(SEXP val);

#endif

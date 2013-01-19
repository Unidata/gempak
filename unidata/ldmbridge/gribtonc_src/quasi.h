/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: quasi.h,v 1.1 1995/05/18 22:03:47 russ Exp $ */

/* Stuff to deal with quasi-regular "grids" */

#ifndef _QUASI_H_
#define _QUASI_H_

#include "product_data.h"

#define QUASI_RECT	0	/* ordinary rectangular grid */
#define QUASI_ROWS	1	/* quasi-regular grid with varying rows */
#define QUASI_COLS	2	/* quasi-regular grid with varying columns */

#define QUASI_METH_DEF	0	/* default interpolation */
#define QUASI_METH_LIN	1	/* linear interpolation */
#define QUASI_METH_CUB  3	/* cubic spline */

typedef struct quas {
    int meth;			/* interpolation method */
    double dlat;		/* latitude increment */
    double dlon;		/* longitude increment */
} quas;

#ifdef __cplusplus
extern "C" quas* qmeth_parse (char *);
extern "C" int expand_quasi (quas *, product_data *);
#elif defined(__STDC__)
extern quas* qmeth_parse (char *);
extern int expand_quasi (quas *, product_data *);
#else
extern quas* qmeth_parse ( /* char * */ );
extern int expand_quasi ( /* quas *, product_data * */ );
#endif

#endif /* !_QUASI_H_ */

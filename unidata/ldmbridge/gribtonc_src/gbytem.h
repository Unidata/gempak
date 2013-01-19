/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gbytem.h,v 1.1 1995/02/23 23:56:13 russ Exp $ */

#ifndef GBYTEM_H_
#define GBYTEM_H_

#include "grib1.h"

/* Type for each element of a grid "bytemap". */
enum gbytes {
    MISSING,			/* decode as a "fill value" */
    PRESENT,			/* decode packed value in Binary Data Sect. */
    REPLICATED			/* copy previous decoded value, used to
				   replicate pole value on lat-lon grids */
};

typedef struct gbytem {
    int nb;                     /* number of bytes in map, which is the
				   number of points in the resulting
				   rectangularized grid */
    int keep;                   /* If 1, don't free.  Used when allocated in
				   a static to be reused with later calls,
				   so clients can safely call
				   free_gbytem() anyway */
    char *map;                  /* the byte map, or 0 if would be all 1's */
} gbytem;

#ifdef __cplusplus
extern "C" gbytem* make_gbytem(bms*, pds*, gds*, int); /* make byte map */
extern "C" void free_gbytem(gbytem*); /* free byte map */
#elif defined(__STDC__)
extern gbytem* make_gbytem(bms*, pds*, gds*, int); /* make byte map */
extern void free_gbytem(gbytem*); /* free byte map */
#else
extern gbytem* make_gbytem( /* bms*, pds*, gds*, int */ );  /* make byte map */
extern void free_gbytem( /* gbytem* */ );    /* free byte map */
#endif

#endif /* GBYTEM_H_ */

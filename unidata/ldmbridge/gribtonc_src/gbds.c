/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gbds.c,v 1.4 1995/07/26 21:44:11 russ Exp $ */

#include <stdlib.h>			/* for free(), ... */
#include <math.h>
#include <limits.h>
#include <assert.h>
#include "ulog.h"
#include "emalloc.h"
#include "gbytem.h"
#include "gbds.h"
#include "grib1.h"

#define EXP10(x) exp((x)*2.30258509299404568401799145468436420760110148862877)

/*
 * Return, right justified, an nbits long bitfield that begins at offset
 * bits in source (where offset 0 means most significant bit).  Assumes
 *   0 <= offset
 *   0 <= nbits
 */
static unsigned
bits(source, offset, nbits)
    unsigned char *source;
    int offset;
    int nbits;
{
    unsigned result = 0;
    int masks[] = {0x0, 0x1, 0x3, 0x7, 0xf, 0x1f, 0x3f, 0x7f, 0xff};
    int nleft = nbits;		/* number of bits left to unpack */
    int shift;

    /* These two statements can be removed if we require offset < CHAR_BIT */
    source += offset/CHAR_BIT;
    offset %= CHAR_BIT;

    assert(0 <= offset && offset < CHAR_BIT);
    assert(0 <= nleft);

    shift = CHAR_BIT - (offset + nleft); /* right shift of current byte */
    while (nleft > 0) {
	if (shift >= 0) {	/* get all remaining nleft bits from this byte */
	    assert(nleft <= CHAR_BIT);
	    result |= (*source >> shift) & masks[nleft];
	    return result;
	}
	if (offset) {		/* get CHAR_BIT-offset bits from this byte */
	    nleft -= CHAR_BIT - offset;
	    result |= (*source++ & masks[CHAR_BIT - offset]) << nleft;
	    offset = 0;
	} else {		/* get all bits from this byte */
	    nleft -= CHAR_BIT;
	    result |= *source++ << nleft;
	}
	shift = CHAR_BIT - nleft;
    }
    return result;
}

#define LDEXP(X,N) ldexp(((double)(X)),(N))

float *
unpackbds(bd, bm, npts, scale10)
    gbds *bd;                   /* binary data section parameters */
    gbytem *bm;                 /* byte map for output values */
    int npts;                   /* number of points for output */
    int scale10;                /* extra power-of-ten scaling exponent */
{
    double g10 = EXP10((double) -scale10); /* factor of 10 to scale data */
    float *data = (float *)emalloc(npts * sizeof(float)) ;
    unsigned char *pp = bd->packed;
    int offset = 0;
    int i;

    if (bm->map && bm->map[0] == REPLICATED) {
        uerror("bad byte map, first value should not be REPLICATED");
        return 0;
    }
    if (bd->is_not_simple) {
        uerror("can't decode second-order packing yet");
        return 0;
    }

    for(i=0; i<npts; i++) {
	if(bm->map == 0 || bm->map[i] == PRESENT) {
            data[i] = g10 * (bd->ref +
                             LDEXP(bits(pp,offset,bd->nbits),bd->bscale)) ;
            offset += bd->nbits ;
            pp += offset/CHAR_BIT ;
            offset %= CHAR_BIT ;
	} else if (bm->map[i] == MISSING) {
            data[i] = FILL_VAL;
	} else if (bm->map[i] == REPLICATED) {
            data[i] = data[i-1];
	} else {
	    uerror("Bad value of bm->map[i]: %d", bm->map[i]);
	    return 0;
	}
    }
    return data;
}


/*
 * Make binary data structure from raw GRIB BDS.  Returns 0 if memory
 * cannot be allocated.  User should call free_gbds() on result when
 * done with it.
 */
gbds*
make_gbds(bdsp)
    bds *bdsp;
{
    gbds *ret = (gbds *) emalloc(sizeof(gbds));

    ret->bscale = g2si(bdsp->scale);
    ret->ref = g4f(bdsp->ref);
    ret->nbits = g1i(bdsp->bits); /* if 0, all entries = ref/10**scale10 */
    ret->unused = bdsp->flg & 0xff ;
    if(ret->nbits != 0)
	ret->nvals = (8 * g3i(bdsp->len) - ret->unused) / ret->nbits;
    else
	ret->nvals = 0;
    ret->is_sph_har = ((BDS_KIND & bdsp->flg) != 0);
    ret->is_not_simple = ((BDS_PACKING & bdsp->flg) != 0);
    ret->is_ints = ((BDS_DATATYPE & bdsp->flg) != 0);
    if (BDS_MORE_FLAGS & bdsp->flg) {
	ret->has_matrix = ((BDS_MATRIX & bdsp->data[2]) != 0);
	ret->has_secbm = ((BDS_SECONDARY & bdsp->data[2]) != 0);
	ret->has_dif_widths = ((BDS_WIDTHS & bdsp->data[2]) != 0);
    }
    ret->packed = bdsp->data;
    return ret;
}

void
free_gbds(gb)		/* free binary data structure */
    gbds* gb;
{
    if(gb)
	free(gb);
}

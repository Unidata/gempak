/*
 *   Copyright 1995 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: grib1.c,v 1.7 1995/07/26 21:44:17 russ Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ulog.h"
#include "emalloc.h"
#include "grib1.h"


#define WMO_HEADER_DEFAULT "header not found"


/*
 * Overlay a GRIB structure on top of raw bytes of GRIB product, where
 * address of output is specified.  Returns 0 if everything goes well.
 */
int
make_grib1(prodp, gribp)
    prod *prodp;
    grib1 *gribp;		/* gets filled in */
{
    unsigned char *endp = prodp->bytes +  prodp->len;
    unsigned char *sp ;

    ids	*idsp=0;		/* Indicator section */
    pds	*pdsp=0;		/* Product definition section */
    gds	*gdsp=0;		/* Grid description section (optional) */
    bms	*bmsp=0;		/* Bit map section (optional) */
    bds	*bdsp=0;		/* Binary data section */

    int grib_edition ;
    int grib_len ;		/* length of GRIB message, in ed. 1 only */
    
    if(!gribp)
	return -1;

    /* Locate start of GRIB product */
    for (sp = prodp->bytes; sp < endp - 3; sp++) {
	if (strncmp((char *)sp, "GRIB", 4) == 0) {
	    break;
	}
    }

    if (sp >= endp - 3) {
	uerror("Can't find `GRIB', skipping") ;
	return -2;
    }

    gribp->hdr = prodp->id;

    idsp = (ids *)sp;
    grib_edition = g1i(idsp->edition);
    if (grib_edition > 0) {
	grib_len = g3i(idsp->len) ;
	if ( sp + grib_len > endp ) {
	    uerror("%s: %d bytes too short",
		   gribp->hdr, (sp + grib_len) - endp);
	    return -3;
	}
	if ( strncmp((char*)sp + grib_len - 4, "7777", 4) != 0) {
	    uerror("%s: no end of product",
		   gribp->hdr);
	    return -4;
	}
        sp += 8;
    } else {
        sp += 4;                /* GRIB Edition 0 missing len */
    }

    pdsp = (pds *)sp;

    sp += g3i(pdsp->len) ;
    if (sp >= endp) {
	uerror("%s: PDS length too big", gribp->hdr) ;
	return -5;
    }

    if( pdsp->db_flg & 0x80 ) {	/* Optional Grid Description Section */
	gdsp = (gds *)sp;
	sp += g3i(gdsp->len);
	if (sp >= endp) {
	    uerror("%s: GDS length too big", gribp->hdr) ;
	    return -6;
	}
    }

    if(pdsp->db_flg & 0x40 ) {	/* Optional Bit Map Section */
	bmsp = (bms *) sp;
	sp += g3i(bmsp->len);
	if (sp >= endp) {
	    uerror("%s: BMS length too big", gribp->hdr) ;
	    return -7;
	}
	/* don't yet have any predefined bit maps */
	if( g2i(bmsp->map_flg) != 0) {
	    uerror("%s: bad predefined bit map block: %d",
		   gribp->hdr, g2i(bmsp->map_flg));
	    return -8;
	}
    }

    /* Binary Data Section */
    bdsp = (bds *) sp;
    sp += g3i(bdsp->len);
    if (sp >= endp) {
	uerror("%s: BDS length too big", gribp->hdr) ;
	return -9;
    }

    if( strncmp((char *)sp, "7777", 4) != 0) {
	uerror("%s: no terminating 7777", gribp->hdr) ;
	return -10;
    }

    gribp->idsp = idsp;
    gribp->pdsp = pdsp;
    gribp->gdsp = gdsp;
    gribp->bmsp = bmsp;
    gribp->bdsp = bdsp;
    
    return 0;
}


void
free_grib1(gp)
    grib1* gp;
{
    if (gp) {
	if (gp->hdr)
	    free(gp->hdr);
	free(gp);
    }
}


/*
 * Return a pointer to a GRIB1 structure allocated from the heap,
 * filled in with fields from raw bytes of GRIB product
 */
grib1 *
new_grib1(prodp)
    prod *prodp;		/* input GRIB product */
{
    grib1 *grib1p = (grib1 *) emalloc(sizeof(grib1));
    
    if(make_grib1(prodp, grib1p) != 0) { /* error */
	free_grib1(grib1p);
	return 0;
    }
    
    return grib1p;
}

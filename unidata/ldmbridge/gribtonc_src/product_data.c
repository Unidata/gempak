/*
 *   Copyright 1994 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: product_data.c,v 1.6 1995/07/26 21:44:26 russ Exp $ */

#include <stdlib.h>
#include "ulog.h"
#include <string.h>
#include "emalloc.h"
#include "product_data.h"
#include "gbds.h"		/* for unpackbds() */


/*
 * Free product_data
 */
void
free_product_data(pd)
    product_data *pd;
{
    if (pd) {
	if(pd->header)
	    free(pd->header);
	free_gdes(pd->gd);
	free_gbytem(pd->bm);
	free_gbds(pd->bd);
	if(pd->data)
	    free(pd->data);
	free(pd);
    }
}


/*
 * Create product_data structure from raw GRIB structure.
 * If there is no grid description section, we manufacture one.
 * We manufacture a bytemap, even if there is no bitmap section.
 * Returns 0 if successful.
 */
int
make_product_data(gp, out)
    grib1 *gp;
    product_data *out;
{
    ids *idsp = gp->idsp;	/* indicator section */
    pds *pdsp = gp->pdsp;	/* product description section */
    bms *bmsp = gp->bmsp;	/* may be 0 if optional bitmap section missing */
    gds *gdsp = gp->gdsp;	/* may be 0 if optional GDS section missing */
    bds *bdsp = gp->bdsp;	/* binary data section */

    if(out == 0)
	return -1;

    /* These will be allocated later, but zero them now to make freeing easier if
       something goes wrong */
    out->gd = 0;
    out->bm = 0;
    out->bd = 0;
    out->data = 0;
    
    out->delim[0] = 'G' ;	/* signature for decoded binary GRIB */
    out->delim[1] = 'R' ; 
    out->delim[2] = 'I' ; 
    out->delim[3] = 'B' ; 

    out->header = emalloc(strlen(gp->hdr)+1);
    strcpy(out->header, gp->hdr);

    out->pdslen = g3i(pdsp->len);    
    out->pdsloc = (char *)gp->pdsp;
    out->edition = g1i(idsp->edition) ;
    out->center = g1i(pdsp->center) ;
    out->model = g1i(pdsp->model) ;
    out->grid = g1i(pdsp->grid) ;
    out->param_vers = g1i(pdsp->table_version) ;
    out->param = param_code(out->edition, g1i(pdsp->param)) ;
    out->level_flg = g1i(pdsp->level_flg) ;
    out->level[0] = g1i(pdsp->level.levs[0]) ;
    out->level[1] = g1i(pdsp->level.levs[1]) ;
    if (out->edition == 0)	/* Fix this before 2087, please */
	out->century = ( g1i(pdsp->year) >= 87 || g1i(pdsp->year) == 0)
	    ? 20 : 21;
    else
	out->century = g1i(pdsp->century) ;	/* Note: =20 for 1901-2000 */
    out->year = g1i(pdsp->year) ;
    out->month = g1i(pdsp->month) ;
    out->day = g1i(pdsp->day) ;
    out->hour = g1i(pdsp->hour) ;
    out->minute = g1i(pdsp->minute) ;
    out->tunit = g1i(pdsp->tunit) ;
    out->tr[0] = g1i(pdsp->tr[0]) ;
    out->tr[1] = g1i(pdsp->tr[1]) ;
    out->tr_flg = g1i(pdsp->tr_flg) ;
    out->avg = g2i(pdsp->avg) ;
    out->missing = g1i(pdsp->missing) ;
    if (out->edition == 0)
        out->scale10 = 0 ;      /* not in edition 0, so manufacture it */
    else
        out->scale10 = g2si(pdsp->scale10) ;
    out->bits = g1i(bdsp->bits) ;
    out->has_gds = ((pdsp->db_flg & HAS_GDS) != 0) ;
    out->has_bms = ((pdsp->db_flg & HAS_BMS) != 0) ;

    out->gd = make_gdes(gp);
    if(!out->gd) {
	if(out->has_gds)
	    uerror("GRIB %s has bad GDS, skipping",
		   out->header);
	else
	    uerror("%s: can't make a GDS for center=%d ,grid=%d",
		   out->header, out->center, out->grid);
	return -2;
    }

    out->cols = out->gd->ncols;
    out->npts = out->gd->npts;

    out->bm = make_gbytem(bmsp, pdsp, gdsp, out->npts);
    if(!out->bm) {
	uerror("in GRIB %s, can't make byte map structure, skipping",
	       out->header);
	return -3;
    }
    out->bd = make_gbds(bdsp);
    if(!out->bd) {
	uerror("in GRIB %s, can't make binary data structure, skipping",
	       out->header);
	return -4;
    }
    out->data = unpackbds(out->bd, out->bm, out->npts, out->scale10);
    if(out->data == 0) {
	uerror("in GRIB %s, can't unpack binary data, skipping",
	       out->header);
	return -5;
    }

    return 0;
}


/*
 * Create newly-allocated product_data structure from raw GRIB structure.
 * If there is no grid description section, we manufacture one.  We
 * manufacture a bytemap, even if there is no bitmap section.  Returns 0 if
 * something goes wrong.
 */
product_data *
new_product_data(gp)
    grib1 *gp;
{
    product_data *out = (product_data *)emalloc(sizeof(product_data));

    if (make_product_data(gp, out) != 0) {
	free_product_data(out);
	return 0;
    }

    return out;
}

#include "vgcmn.h"

void vfrnt ( int *np, float x[], float y[], int *iret )
/************************************************************************
 * vfrnt								*
 *									*
 * This subroutine draws a front to device by constructing a VG record  *
 * for the front and writing the record to the file at proper location. *
 *									*
 * vfrnt ( np, x, y, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	x[]		float		X coordinates (latitude)	*
 *	y[]		float		Y coordinates (longitude)	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 *					-31 = invalid front attribute	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * E. Wehner/EAi	 7/97	Added group type and number		*
 * S. Law/GSC		04/98	Added smooth				*
 * M. Li/GSC		07/00	Added kcolr2				*
 * J. Wu/GSC		12/00	Rewrote for new cvgwritelm()		*
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * J. Wu/GSC		02/01	Modified 'unused1' to 'smooth' for the	*
 *				corresponding renaming in VG_DBStruct.h	*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		start, ii, iblk, nblk, npblk, recsize, ipt1;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1;

    if ( *np < 0 || *np > MAXPTS ) {
        *iret = -28;
        return;
    }

/*
 *  Initialize a front element.
 */
    el.hdr.vg_type = FRONT_ELM;
    el.hdr.vg_class = CLASS_FRONTS;

    cvg_initelm( &el );
    
/* 
 *  Load colors, smoothing level and points into the VG record(s).
 */
    el.hdr.maj_col = kcolr;
    el.hdr.min_col = kcolr2;
    el.hdr.smooth = (rdens < 0.001F) ? 0 : (rdens < 1.0F) ? 1 : 2;
	
/*
 *  Split the line into as many blocks as necessary.
 */
    nblk = ( *np - 2 ) / ( MAXPTS - 1 ) + 1;
    for ( iblk = 0; iblk < nblk; iblk++ ) {
	if ( iblk < ( nblk - 1 ) ) {
	    npblk = MAXPTS - 1;
	}
	else {
	    npblk = *np - ( MAXPTS - 1 ) * iblk;
	}
	
	recsize = (int)( ( sizeof(float) * 2 * (size_t)npblk ) + 
		     sizeof(VG_HdrStruct) + sizeof(FrontInfo) );
	ipt1 = iblk * (MAXPTS - 1);
	
	el.hdr.range_min_lat = x[ipt1];
	el.hdr.range_min_lon = y[ipt1];
	el.hdr.range_max_lat = x[ipt1];
	el.hdr.range_max_lon = y[ipt1];

	for ( ii = 0; ii < npblk; ii++ ) {
	    el.elem.frt.latlon[ii      ] = x[ipt1 + ii];
	    el.elem.frt.latlon[ii+npblk] = y[ipt1 + ii];

            /* 
             *  Establish max and mins if they apply. 
             */
	    if ( el.elem.frt.latlon[ii + npblk] < el.hdr.range_min_lon )
		el.hdr.range_min_lon = el.elem.frt.latlon[ii + npblk];
	    if ( el.elem.frt.latlon[ii] < el.hdr.range_min_lat )
		el.hdr.range_min_lat = el.elem.frt.latlon[ii];
	    if ( el.elem.frt.latlon[ii + npblk] > el.hdr.range_max_lon )
		el.hdr.range_max_lon = el.elem.frt.latlon[ii + npblk];
	    if ( el.elem.frt.latlon[ii] > el.hdr.range_max_lat )
	        el.hdr.range_max_lat = el.elem.frt.latlon[ii];
	}
     
        /* 
         *  Save the number of points and other settings info after checking. 
         */
	if ( kfcod >= 0 && kfcod <= 999 && kpipsz > 0  && kpipst > 0 ) {
	    el.elem.frt.info.fcode  = kfcod;
	    el.elem.frt.info.fpipsz = kpipsz;
	    el.elem.frt.info.fpipst = kpipst;
	}
	else {
	    *iret = -31;
	    return;
	}

	el.elem.frt.info.numpts = npblk;
	el.elem.frt.info.fpipdr = kpipdr;
	strcpy ( el.elem.frt.info.frtlbl, "STJ" );
	el.hdr.recsz = recsize;
	el.hdr.grptyp = kgtyp;
	el.hdr.grpnum = kgnum;
	
        /* 
         *  Write element (append to the end of file if start = -1) 
         */
        cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
 	if ( *iret != 0 )  return;
    }
}

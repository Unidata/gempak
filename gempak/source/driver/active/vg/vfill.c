#include "vgcmn.h"

void vfill ( int *np, float x[], float y[], int *iret )
/************************************************************************
 * vfill								*
 *									*
 * This subroutine draws a filled polygon by constructing a VG record   *
 * for it and writing the record to the file at proper location.	*
 *									*
 * vfill ( np, x, y, iret )						*
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
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/97						*
 * S. Jacobs/NCEP	 3/98	Added fill pattern type			*
 * S. Law/GSC		04/98	Added smooth level			*
 * S. Jacobs/NCEP	12/98	Added check for duplicate endpoints	*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()	        *
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * J. Wu/GSC		02/01	Modified 'unused1' to 'smooth' for the	*
 *				corresponding renaming in VG_DBStruct.h	* 
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		npts, lclspt, filled, closed, smooth, recsize;
    int		ii, iblk, nblk, npblk, ipt1, start;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1;
    closed = 1;
    filled = kfillt + 1;
    smooth = (rdens < 0.001F) ? 0 : (rdens < 1.0F) ? 1 : 2;
    
    if ( *np < 0 || *np > MAXPTS ) {
	*iret = -28;
	return;
    }

/*
 *  Make sure that closed lines will remain visually closed and chop 
 *  off the last point if necessary. 
 */
    npts   = *np;
    while ( G_DIFF(x[0], x[npts-1]) && G_DIFF(y[0], y[npts-1]) )  {
        npts--;
    }

    lclspt = 0;
    if ( npts >= MAXPTS && ( closed != 0 || filled != 0) ) lclspt = 1;

/* 
 *  Initialize the element.
 */
    el.hdr.vg_type = LINE_ELM;
    el.hdr.vg_class = CLASS_LINES;
  
    cvg_initelm( &el );

/* 
 *  Load colors, smoothing level and points into the VG record(s).
 */
    el.hdr.maj_col = kcolr;
    el.hdr.min_col = kcolr;
    el.hdr.smooth = (char)smooth;

/*
 *  Split the line into as many blocks as necessary.
 */    
    nblk = ( npts - 2) / ( MAXPTS - 1 ) + 1 + lclspt;
    for ( iblk = 0; iblk < nblk; iblk++ ) {
	if ( iblk < ( nblk - 1 - lclspt ) ) {
	    npblk = MAXPTS - 1;
	}
	else if ( lclspt == 0 || iblk < (nblk - 1) ) {
	    npblk = npts - ( MAXPTS - 1 ) * iblk;
	}
	else {
	    npblk = 2;
	}
	
	recsize = (int)( (sizeof(float) * 2 * (size_t)npblk) + 
		     sizeof(VG_HdrStruct) + 
		     sizeof(LineInfo) );
		     
	ipt1 = iblk * ( MAXPTS - 1 );
	el.hdr.range_min_lat = x[ipt1];
	el.hdr.range_min_lon = y[ipt1];
	el.hdr.range_max_lat = x[ipt1];
	el.hdr.range_max_lon = y[ipt1];

	for ( ii = 0; ii < npblk; ii++ ) {
	    if ( iblk <= nblk - 1 - lclspt ) {
            
	       /*
                *  Not closing off a line, add current point.
                */
		el.elem.lin.latlon[ii      ] = x[ipt1 + ii];
		el.elem.lin.latlon[ii+npblk] = y[ipt1 + ii];
	    }
	    else {
            
	       /*
                *  Closing off the line.
                */
		el.elem.lin.latlon[1      ] = x[0];
		el.elem.lin.latlon[1+npblk] = y[0];
		el.elem.lin.latlon[0      ] = x[npts - 1];
		el.elem.lin.latlon[0+npblk] = y[npts - 1];
	    }

            /* 
             *  Establish max and mins if they apply. 
             */
	    if (el.elem.lin.latlon[ii+npblk] < el.hdr.range_min_lon )
		    el.hdr.range_min_lon = el.elem.lin.latlon[ii+npblk];
	    if (el.elem.lin.latlon[ii      ] < el.hdr.range_min_lat )
		    el.hdr.range_min_lat = el.elem.lin.latlon[ii      ];
	    if (el.elem.lin.latlon[ii+npblk] > el.hdr.range_max_lon )
		    el.hdr.range_max_lon = el.elem.lin.latlon[ii+npblk];
	    if (el.elem.lin.latlon[ii      ] > el.hdr.range_max_lat )
		    el.hdr.range_max_lat = el.elem.lin.latlon[ii      ];
	}
     
        /* 
         *  Save the number of points and other settings information. 
         */
	el.elem.lin.info.numpts = npblk;
	el.elem.lin.info.lintyp = kltyp;
	el.elem.lin.info.lthw = klthw;
	el.elem.lin.info.width = klwid;
	el.elem.lin.info.lwhw = klwhw;
	el.hdr.filled = (char)filled;
	el.hdr.closed = (char)closed;
	el.hdr.recsz = recsize;
	el.hdr.grptyp = kgtyp;
	el.hdr.grpnum = kgnum;

        /*
         *  Use reasonable characteristics for lines that are too long.
         */
	if ( npts >= MAXPTS ) {
	    el.hdr.filled = 0;
	    el.hdr.closed = 0;
	}

       /* 
        *  Write element (append to the end of file if start = -1) 
        */
        cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
	if ( *iret != 0 )  return;
    }
}

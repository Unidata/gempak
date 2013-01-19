#include "vgcmn.h"

void vhash ( int *np, float x[], float y[], float dir[], int *iret )
/************************************************************************
 * vhash								*
 *									*
 * This subroutine draws hash marks to the device by constructing VG    *
 * records for them and writing those records to the file at proper     * 
 * location.								*
 *									*
 * vhash ( np, x, y, dir, iret )					*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	x[np]		float		X coordinates (latitude)	*
 *	y[np]		float		Y coordinates (longitude)	*
 *	dir[np]		float		Wind direction			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 **									*
 * Log:									*
 * I. Durham/GSC	 4/98						*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()	        *
 * E. Safford/GSC	01/01	fix erroneous check on np upper bound	*
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		ii, recsize, start;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;
    
    if ( *np < 0 ) { 
	*iret = -28;
	return;
    }

/*
 *  Initialize a HASH element.
 */
    el.hdr.vg_type = HASH_ELM;
    el.hdr.vg_class = CLASS_WINDS;

    cvg_initelm( &el );

/* 
 *  Get the size of one hash element.
 */
    recsize = ( sizeof( float ) * 4 + 
    	        sizeof(VG_HdrStruct) + 
                sizeof(WindInfo) );

/*
 *  Call cvg_writelm() for each hash element.
 */
    for ( ii = 0 ; ii < *np ; ii++ )  {

        /* 
         *  Load colors, points, and offset points into the VG record.
         */
        el.hdr.maj_col = kcolr;
        el.hdr.min_col = kcolr;
        el.hdr.range_min_lat = x[ii];
        el.hdr.range_min_lon = y[ii];
        el.hdr.range_max_lat = x[ii];
        el.hdr.range_max_lon = y[ii];

        el.elem.wnd.data.latlon[0] = x[ii];
        el.elem.wnd.data.latlon[1] = y[ii];
        el.elem.wnd.data.spddir[0] = 0.0F;
        el.elem.wnd.data.spddir[1] = dir[ii];
    
        /* 
         *  Load other settings and attributes. 
         */
        el.elem.wnd.info.numwnd = 1;
        el.elem.wnd.info.width  = khwid;
        el.elem.wnd.info.size   = rhshsz;
        el.elem.wnd.info.wndtyp = klwidh;
        el.elem.wnd.info.hdsiz  = 0.0F;
        el.hdr.recsz   = recsize;
        el.hdr.grptyp  = kgtyp;
        el.hdr.grpnum  = kgnum;
   
        /* 
         *  Write element (append to the end of file if start = -1) 
         */
        cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
        if ( *iret != 0 )  return;
    }
}

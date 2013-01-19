#include "vgcmn.h"

void vmark ( int *np, float x[], float y[], int *iret )
/************************************************************************
 * vmark								*
 *									*
 * This subroutine draws markers to the device by constructing VG       *
 * records for them and writing those records to the file at proper     *
 * locations.	          	        	                        *
 *									*
 * vmark ( np, x, y, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	x[np]		float		X coordinates (latitude)	*
 *	y[np]		float		Y coordinates (longitude)	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * E. Wehner/EAi	 7/97		Added group type and number	*
 * D.W. Plummer/NCEP	 9/97		Corrected MAX_PTS to MAXPTS	*
 * J. Wu/GSC		12/00	        Rewrote for new cvg_writelm()	*
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
 *  Initialize a MARK element.
 */
    el.hdr.vg_type = MARK_ELM;
    el.hdr.vg_class = CLASS_SYMBOLS;

    cvg_initelm( &el );

/* 
 *  Get the size of one record.
 */
    recsize = ( ( sizeof(float) * 3 + sizeof(int) * 2 ) +
	        sizeof(VG_HdrStruct) + 
                sizeof(SymInfo) );
		      
/*
 *  Call cvg_writelm() for each mark.
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

        el.elem.sym.data.latlon[0] = x[ii];
        el.elem.sym.data.latlon[1] = y[ii];
        el.elem.sym.data.offset_xy[0] = 0;
        el.elem.sym.data.offset_xy[1] = 0;

        el.elem.sym.data.code[0] = (float) kmark; 
        
	/*     
         *  Load other settings and attributes. 
         */
        el.elem.sym.info.numsym = 1;
        el.elem.sym.info.width  = kmkwid;
        el.elem.sym.info.size   = rmksiz;
        el.elem.sym.info.ityp   = kmktyp;
        el.hdr.recsz   = recsize;
        el.hdr.grptyp = kgtyp;
        el.hdr.grpnum = kgnum;
	
        /* 
         *  Write element (append to the end of file if start = -1) 
         */
        cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
	if ( *iret != 0 )  return;
    }	
}

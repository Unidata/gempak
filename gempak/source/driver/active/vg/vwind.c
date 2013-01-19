#include "vgcmn.h"

void vwind ( int *iwnd, int *np, float x[], float y[], float spd[], 
						float dir[], int *iret )
/************************************************************************
 * vwind								*
 *									*
 * This subroutine draws winds to the device by constructing VG records *
 * record for them (barbs, arrows or directionals) and writing those    *
 * records to the file at the proper location.	                        *
 *									*
 * vwind ( iwnd, np, x, y, spd, dir, iret )				*
 *									*
 * Input parameters:							*
 *	*iwnd		int		Symbol category			*
 *					  1 = Barbs			*
 *					  2 = Arrows			*
 *					  3 = Directional arrows        *
 *	*np		int		Number of points		*
 *	x   [np]	float		X coordinates (latitude)	*
 *	y   [np]	float		Y coordinates (longitude)	*
 *	spd [np]	float		Wind speed			*
 *	dir [np]	float		Wind direction			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * E. Wehner/EAi	 7/97	Add group type and number		*
 * D.W.Plummer/NCEP	 9/97	Call cvg_svwnd for each wind		*
 * I. Durham/GSC	 4/98	Added case 3				*
 * S. Jacobs/NCEP	 3/99	Added flag for success in finding case	*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()	        *
 * E. Safford/GSC	01/01	fix erroneous check on np upper bound	*
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		ielem, iwidth, itype, ii, start, recsize, ok;
    float	size, hdsiz;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1;
       
    if ( *np < 0 ) { 
	*iret = -28;
	return;
    }
    

    ok = G_TRUE;
    switch ( *iwnd )  {

        case 1: /* Barbs */
	    ielem  = BARB_ELM;
    	    size   = rbrsiz;
	    iwidth = kbrwid;
	    itype  = kbrtyp;
	    hdsiz  = 0.0F;
        break;

        case 2: /* Arrows */
            ielem  = ARROW_ELM;
	    size   = rarsiz;
	    iwidth = karwid;
	    itype  = kartyp;
	    hdsiz  = rarhsz;
        break;
	    
        case 3: /* Directional arrows */
	    ielem  = DARR_ELM;
	    size   = rarsiz;
	    iwidth = karwid;
	    itype  = kartyp;
	    hdsiz  = rarhsz;
        break;
        
	default:
            ok = G_FALSE;
        break;
    }

/*
 *  Initialize a WIND element
 */
    el.hdr.vg_type = (char)ielem;
    el.hdr.vg_class = CLASS_WINDS;

    cvg_initelm( &el );

/* 
 *  Get the size of one wind element.
 */
    recsize = ( sizeof( float ) * 4 + 
    	        sizeof(VG_HdrStruct) + 
                sizeof(WindInfo) );

/*
 *  Call cvg_writelm() for each wind element.
 */ 
    if ( ok ) {

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
            el.elem.wnd.data.spddir[0] = spd[ii];
            el.elem.wnd.data.spddir[1] = dir[ii];
    
           /* 
            *  Load other settings and attributes. 
            */
            el.elem.wnd.info.numwnd = 1;
            el.elem.wnd.info.width  = iwidth;
            el.elem.wnd.info.size   = size;
            el.elem.wnd.info.wndtyp = itype;
            el.elem.wnd.info.hdsiz  = hdsiz;
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
}

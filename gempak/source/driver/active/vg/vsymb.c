#include "vgcmn.h"

void vsymb ( int *isym, int *np, float code[], float x[], float y[], 
				int ixoff[], int iyoff[], int *iret )
/************************************************************************
 * vsymb								*
 *									*
 * This subroutine draws symbols to the device by constructing VG       *
 * records for them and writings those record to the file at the proper *
 * location.	          	        	                        *
 *									*
 * vsymb ( isym, np, code, x, y, ixoff, iyoff, iret )			*
 *									*
 * Input parameters:							*
 *	*isym		int		Symbol category			*
 *					  1 = Weather symbols		*
 *					  2 = Cloud type symbols	*
 *					  3 = Icing symbols		*
 *					  4 = Pressure tendency symbols	*
 *					  5 = Past weather symbols	*
 *					  6 = Sky cover symbols		*
 *					  7 = Special symbols		*
 *					  8 = Turbulence symbols	*
 *					  9 = Combination wx symbols    *
 *	*np		int		Number of points		*
 *	code  [np]	float		Symbol codes			*
 *	x     [np]	float		X coordinates (latitude)	*
 *	y     [np]	float		Y coordinates (longitude)	*
 *	ixoff [np]	int		X offsets			*
 *	iyoff [np]	int		Y offsets			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * D.W.Plummer/NCEP	 9/97	Call cvgsvsym for each symbol		*
 * G. Krueger/EAI	 3/98	Corrected CODE in CVG_SVSYM arguments	*
 * A. Hardy/GSC         10/98   Added combination symbols case          *
 * S. Jacobs/NCEP	 3/99	Added flag for success in finding case	*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()	        *
 * E. Safford/GSC	01/01	fix erroneous check on np upper bound	*
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		ielem, iwidth, itype, ii, ok, recsize, start;
    float	size;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1;
    
    if ( *np < 0 ) { 
	*iret = -28;
	return;
    }

    ok = G_TRUE;
    switch ( *isym )  {

        case 1: /* Weather */
            ielem  = WXSYM_ELM;
            size   = rwtsiz;
            iwidth = kwtwid;
            itype  = 0;
        break;

        case 2: /* Cloud type */
            ielem  = CTSYM_ELM;
            size   = rctsiz;
            iwidth = kctwid;
            itype  = 0;
        break;

        case 3: /* Icing */
            ielem  = ICSYM_ELM;
            size   = ricsiz;
            iwidth = kicwid;
            itype  = 0;
        break;

        case 4: /* Pressure tendency */
            ielem  = PTSYM_ELM;
            size   = rptsiz;
            iwidth = kptwid;
            itype  = 0;
        break;

        case 5: /* Past weather */
            ielem  = PWSYM_ELM;
            size   = rpwsiz;
            iwidth = kpwwid;
            itype  = 0;
        break;

        case 6: /* Sky cover */
            ielem  = SKSYM_ELM;
            size   = rsksiz;
            iwidth = kskwid;
            itype  = ksktyp;
        break;

        case 7: /* Special */
            ielem  = SPSYM_ELM;
            size   = rspsiz;
            iwidth = kspwid;
            itype  = 0;
        break;

        case 8: /* Turbulence */
            ielem  = TBSYM_ELM;
            size   = rtbsiz;
            iwidth = ktbwid;
            itype  = 0;
        break;

        case 9: /* Combination weather symbols */
            ielem  = CMBSY_ELM;
            size   = rcsysz;
            iwidth = kcsywd;
            itype  = 0;
        break;

        default:
            ok = G_FALSE;
        break;
    }

/*
 *  Initialize a symbol element.
 */
    el.hdr.vg_type = (char)ielem;
    el.hdr.vg_class = CLASS_SYMBOLS;

    cvg_initelm( &el );

/* 
 *  Get the size of one record.
 */
    recsize = ( ( sizeof(float) * 3 + sizeof(int) * 2 ) +
	        sizeof(VG_HdrStruct) + 
                sizeof(SymInfo) );

/*
 *  Call cvg_writelm() for each symbol.
 */

    if  ( ok )  {
    		      
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
            el.elem.sym.data.offset_xy[0] = ixoff[ii];
            el.elem.sym.data.offset_xy[1] = iyoff[ii];

            el.elem.sym.data.code[0] = code[ii];
        
	    /*     
             *  Load other settings and attributes. 
             */
            el.elem.sym.info.numsym = 1;
            el.elem.sym.info.width  = iwidth;
            el.elem.sym.info.size   = size;
            el.elem.sym.info.ityp   = itype;
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
}

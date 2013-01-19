#include "vgcmn.h"

void vtext ( float *x, float *y, char *text, int *lens, int *ixoff, 
				int *iyoff, float *rotat, int *iret )
/************************************************************************
 * vtext								*
 *									*
 * This subroutine draws a text string to the device by constructing a  *
 * VG record for it and writing that record to the file at the proper   *
 * location.	          	        	                        *
 *									*
 * vtext ( x, y, text, lens, ixoff, iyoff, rotat, iret )		*
 *									*
 * Input parameters:							*
 *	*x		float		X coordinate (latitude)		*
 *	*y		float		Y coordinate (longitude)	*
 *	*text		char		Text string			*
 *	*lens		int		Length of the text string	*
 *	*ixoff		int		X offset			*
 *	*iyoff		int		Y offset			*
 *	*rotat		float		Rotation angle			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid text size     	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * E. Wehner/EAi	 7/97	Add group type and number 		*
 * S. Jacobs/NCEP	11/97	Added calc of rotation orientation	*
 * S. Jacobs/NCEP	12/98	Changed element type to SPTX type 0	*
 * S. Jacobs/NCEP	12/98	Changed alignment value to SPTX range	*
 * M. Li/GSC		02/00	Added the declaration of text		*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()	        *
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * S. Jacobs/NCEP	 7/01	Set the text type based on border	*
 * S. Jacobs/NCEP	10/05	Added the overline text types 13 and 14	*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		start, recsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1; 
       
/*  
 *  flag MAX_TEXT exceeded or no text error 
 */
    if ( *lens <= 0 || *lens > MAX_TEXT ) {
	*iret = -28;     /* change to be text size fault */
	return;
    } 

/*
 *  Initialize a special TEXT element.
 */
    el.hdr.vg_type = SPTX_ELM;
    el.hdr.vg_class = CLASS_TEXT;

    cvg_initelm( &el );

/* 
 *  Get the size of a record.
 */
    recsize = (int)( sizeof(VG_HdrStruct) + 
		sizeof(SpTextInfo) + (size_t)*lens + 1 ) ;

/* 
 *  Load the colors and X and Y coordinates into the VG record.
 */
    el.hdr.maj_col = kcolr;
    el.hdr.min_col = kcolr;
    el.hdr.range_min_lat = *x;
    el.hdr.range_min_lon = *y;
    el.hdr.range_max_lat = *x;
    el.hdr.range_max_lon = *y;
    el.elem.spt.info.lat   = *x;
    el.elem.spt.info.lon   = *y;
    el.elem.spt.info.offset_x = *ixoff;
    el.elem.spt.info.offset_y = *iyoff;

/* 
 *  Save the number of points and other settings information. 
 */
    el.elem.spt.info.rotn    = *rotat + (float)( 1000 * ( krrotn - 1 ) );
    el.elem.spt.info.sztext  = rtxsz;
    el.elem.spt.info.itxfn   = ktxfn;
    el.elem.spt.info.ithw    = ktxhw;
    el.elem.spt.info.iwidth  = ktxwid;
    el.elem.spt.info.turbsym = 0;
    el.elem.spt.info.txtcol  = kcolr;    
    el.elem.spt.info.lincol  = kcolr;
    el.elem.spt.info.filcol  = kcolr;
    el.elem.spt.info.ialign  = kjust - 2;
    strcpy ( el.elem.spt.text, text );
    el.hdr.recsz = recsize;
    el.hdr.grptyp = kgtyp;
    el.hdr.grpnum = kgnum;

/*
 *  Set the text type based on the border value.
 */
    switch ( kbrdr )  {
	case 112:
	case 122:
	case 212:
	case 222:
	    el.elem.spt.info.sptxtyp = 1;
	    break;
	case 113:
	case 123:
	case 213:
	case 223:
	    el.elem.spt.info.sptxtyp = 2;
	    break;
	case 211:
	    el.elem.spt.info.sptxtyp = 3;
	    break;
	case 221:
	    el.elem.spt.info.sptxtyp = 4;
	    break;
	case 121:
	    el.elem.spt.info.sptxtyp = 5;
	    break;
	case 114:
	case 124:
	case 214:
	case 224:
	    el.elem.spt.info.sptxtyp = 6;
	    break;
	case 115:
	case 215:
	    el.elem.spt.info.sptxtyp = 10;
	    break;
	case 125:
	case 225:
	    el.elem.spt.info.sptxtyp = 11;
	    break;
	case 116:
	case 216:
	    el.elem.spt.info.sptxtyp = 13;
	    break;
	case 126:
	case 226:
	    el.elem.spt.info.sptxtyp = 14;
	    break;
	default:
	    el.elem.spt.info.sptxtyp = 0;
	    break;
    }

/* 
 *  Write element (append to the end of file if start = -1) 
 */ 
    cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
}

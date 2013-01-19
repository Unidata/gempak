#include "vgcmn.h"

void vtxsy ( int *itype, int *isym, int *ijust, int *ixoff, int *iyoff, 
		float *rotn, float *x, float *y, char *text, int *lens, 
		int *iret )
/************************************************************************
 * vtxsy								*
 *									*
 * This subroutine draws a special text string to the device by         *
 * constructing a VG record for it and writing that record to the file  *
 * at the proper location.	                                        *
 *									*
 * vtxsy ( itype, isym, ijust, ixoff, iyoff, rotn, x, y, text, lens, 	*
 *								iret )	*
 *									*
 * Input parameters:							*
 *	*itype		int		Type of Special Text		*
 *	*isym		int		Turbulence Symbol number	*
 *	*ijust		int		Text justification (-1, 0, 1)	*
 *	*ixoff		int		X offset			*
 *	*iyoff		int		Y offset			*
 *	*rotn		float		Rotation			*
 *	*x		float		X coordinate (latitude)		*
 *	*y		float		Y coordinate (longitude)	*
 *	*text		char		Text string			*
 *	*lens		int		Length of the text string	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	 6/97		Initial Coding (copied vtext)   *
 * E. Wehner/EAi	 7/97		Added group number and type	*
 * S. Jacobs/NCEP	 7/97		Removed text, line, fill colors	*
 * E. Safford/GSC	 7/97		Fixed X & Y misplacement in    	*
 *                                        call to cvg_svstx             *
 * E. Safford/GSC	 7/97		Added ijust, offsets and rotn   *
 * S. Jacobs/NCEP	 7/97		Fixed the order of X and Y	*
 * S. Jacobs/NCEP	 7/97		Temp change to only center text	*
 * S. Jacobs/NCEP	 7/97		Allow left & right justified	*
 * S. Jacobs/NCEP	 7/98		Added N-rel rotation flag	*
 * J. Wu/GSC		12/00	        Rewrote for new cvg_writelm()   *
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
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
    if ( *lens <= 0 ||  *lens > MAX_TEXT ) {
	*iret = -28;     /* change to be text size fault */
	return;
    } 

/*
 *   Initialize a special TEXT element
 */
    el.hdr.vg_type = SPTX_ELM;
    el.hdr.vg_class = CLASS_TEXT;

    cvg_initelm( &el );
    
/* 
 *  Get the size of a record.
 */
    recsize = (int)( sizeof(VG_HdrStruct) + 
		sizeof(SpTextInfo) + strlen(text) + 1 ) ;

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
    el.elem.spt.info.rotn    = *rotn + (float)( 1000 * ( krrotn - 1 ) );
    el.elem.spt.info.sptxtyp = *itype;
    el.elem.spt.info.sztext  = rtxsz;
    el.elem.spt.info.itxfn   = ktxfn;
    el.elem.spt.info.ithw    = ktxhw;
    el.elem.spt.info.iwidth  = ktxwid;
    el.elem.spt.info.turbsym = *isym;
    el.elem.spt.info.txtcol  = kcolr;    
    el.elem.spt.info.lincol  = kcolr;
    el.elem.spt.info.filcol  = kcolr;
    el.elem.spt.info.ialign  = *ijust;
    strcpy ( el.elem.spt.text, text );
    el.hdr.recsz = recsize;
    el.hdr.grptyp = kgtyp;
    el.hdr.grpnum = kgnum;

/* 
 *  Write element (append to the end of file if start = -1) 
 */
    cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
}

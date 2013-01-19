#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  LMULT       1
#define  LDIR        1
#define  LSIZE       0.800
#define  LWID        2
#define  LENBUF      256


void sigatrp ( char *fhour, int rnum, trop_t *ptrr, int hnum, 
               trophi_t *ptrh, int lnum, troplo_t *ptrl, int itime[], 
	       char *chlvl, int *iret )
/************************************************************************
 * sigatrp                                                              *
 *                                                                      *
 * This program encodes the High or Mid Level Significant Weather ASCII *
 * tropospheric information into VG file format. 			*
 *                                                                      *
 * sigatrp ( fhour, rnum, ptrr, hnum, ptrh, lnum, ptrl, itime, chlvl,   *
 *           iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      rnum            int	   Number of regular trop boxes		*
 *      *ptrr		trop_t     Pointer to reg. TROP link list	*
 *      hnum            int	   Number of high trop boxes		*
 *      *ptrh		trophi_t   Pointer to high TROP link list	*
 *      lnum            int	   Number of low trop boxes		*
 *      *ptrl		troplo_t   Pointer to low TROP link list	*
 *      itime[]		int	   Issued date/time 			*
 *      *chlvl 		char	   Chart level				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC     3/02	Created					*
 * A. Hardy/SAIC     5/02	Round height nearest 10; fix H&L boxes  *
 * M. Li/SAIC	     1/05	Added chlvl				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 ***********************************************************************/
 {
    int   	    ii, ij, ier, txtlen, len;
    FILE  	    *fptr;

    int		    numerr, leverr, wflg, tmptm; 
    int             sizrec, start, tmpmod;
    float           level, tmplvl; 
    char 	    ofname[LENBUF], errgrp[8], trpstr[40];
    char            ctime[LENBUF];
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret = 0;
     fptr   = NULL;
     ier     = 0;
     leverr  = 0;
     start   = 0;
     wflg    = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /* 
     * Create output filename.
     */

     if ( strcmp ( chlvl, "SWM" ) == 0 ) {
         strcpy ( ofname, "mtrp_");
     }
     else {
         strcpy ( ofname, "trp_");
     }

     for ( ij = 0; ij < 4; ij++ ){
	 tmptm = itime[ij];
         cst_inch ( tmptm, ctime, &ier );
	 cst_lstr ( ctime, &len, &ier );
	 if ( len == 1 ) strcat ( ofname, "0");
         strcat ( ofname, ctime );
     }
     strcat ( ofname, "_");
     strcat ( ofname, fhour);
     strcat ( ofname, "_final.vgf");

    /* 
     * Open output file and write the VG file header information. 
     */

     cvg_crvgf ( ofname, &ier );
     cvg_open ( ofname, wflg, &fptr, &ier);
     if ( ( ier != 0 ) || ( fptr == NULL ) ) {
         numerr = -5;
         er_lmsg ( &leverr, errgrp, &numerr, ofname, &ier,
                   strlen(errgrp), strlen(ofname) );
		   exit(1);
     }

    /*
     * Loop through the regular tropopause element list and 
     * write each one to VG file.
     */

     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < rnum; ii++) {

	/* 
	 * Fill in regular tropopause TEXT header information. 
	 */

	 level =  ptrr->level;
	 tmplvl = pr_hgmf (&level);
	 tmplvl= tmplvl/ 100.0F;
         tmpmod = ( (int) tmplvl % 10 );
	 if (  tmpmod >= 5 ) {
	     tmplvl = tmplvl + (float)( 10 - tmpmod );
	 }
	 else {
	     tmplvl = tmplvl - (float)tmpmod;
	 }

	 cst_inch ( (int) tmplvl, trpstr, &ier ); 

	 cst_lstr ( trpstr, &txtlen, &ier );
         sizrec = sizeof( VG_HdrStruct ) + sizeof( SpTextInfo ) +
	           txtlen + 1;

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPTX_ELM;
         el.hdr.vg_class = CLASS_TEXT;
         el.hdr.maj_col  = 31;
         el.hdr.min_col  = 31;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = 0;
         el.hdr.grpnum   = 0;
         el.hdr.range_min_lat = ptrr->lat;
         el.hdr.range_min_lon = ptrr->lon;
         el.hdr.range_max_lat = ptrr->lat;
         el.hdr.range_max_lon = ptrr->lon;

	/* 
	 * Fill in regular tropopause TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 4;
	 el.elem.spt.info.turbsym  = 4;
	 el.elem.spt.info.sztext   = 1.25F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = 0;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 31;
	 el.elem.spt.info.filcol   = 31;
	 el.elem.spt.info.lincol   = 31;
	 el.elem.spt.info.lat      = ptrr->lat;
	 el.elem.spt.info.lon      = ptrr->lon;
	 strcpy ( el.elem.spt.text, trpstr );

	/* 
	 * Write tropopause TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptrr = ptrr->next;
     }

    /* 
     * Process high tropopause boxes.
     */

     for ( ii = 0; ii < hnum; ii++) {

	/* 
	 * Fill in high tropopause TEXT header information. 
	 */

	 level =  ptrh->level;
	 tmplvl = pr_hgmf (&level);
	 tmplvl= tmplvl/ 100.0F;
         tmpmod = ( (int) tmplvl % 10 );
	 if (  tmpmod >= 5 ) {
	     tmplvl = tmplvl + (float)( 10 - tmpmod );
	 }
	 else {
	     tmplvl = tmplvl - (float)tmpmod;
	 }

	 cst_inch ( (int) tmplvl, trpstr, &ier ); 

	 cst_lstr ( trpstr, &txtlen, &ier );
         sizrec = sizeof( VG_HdrStruct ) + sizeof( SpTextInfo ) +
	           txtlen + 1;

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPTX_ELM;
         el.hdr.vg_class = CLASS_TEXT;
         el.hdr.maj_col  = 31;
         el.hdr.min_col  = 31;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = 0;
         el.hdr.grpnum   = 0;
         el.hdr.range_min_lat = ptrh->lat;
         el.hdr.range_min_lon = ptrh->lon;
         el.hdr.range_max_lat = ptrh->lat;
         el.hdr.range_max_lon = ptrh->lon;

	/* 
	 * Fill in high tropopause TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 2;
	 el.elem.spt.info.turbsym  = 4;
	 el.elem.spt.info.sztext   = 1.07F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = 0;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 26;
	 el.elem.spt.info.filcol   = 31;
	 el.elem.spt.info.lincol   = 31;
	 el.elem.spt.info.lat      = ptrh->lat;
	 el.elem.spt.info.lon      = ptrh->lon;
	 strcpy ( el.elem.spt.text, trpstr );

	/* 
	 * Write high tropopause TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptrh = ptrh->next;
     }

    /* 
     * Process low tropopause boxes.
     */

     for ( ii = 0; ii < lnum; ii++) {

	/* 
	 * Fill in high tropopause TEXT header information. 
	 */

	 level =  ptrl->level;
	 tmplvl = pr_hgmf (&level);
	 tmplvl= tmplvl/ 100.0F;
         tmpmod = ( (int) tmplvl % 10 );
	 if (  tmpmod >= 5 ) {
	     tmplvl = tmplvl + (float)( 10 - tmpmod );
	 }
	 else {
	     tmplvl = tmplvl - (float)tmpmod;
	 }

	 cst_inch ( (int) tmplvl, trpstr, &ier ); 

	 cst_lstr ( trpstr, &txtlen, &ier );
         sizrec = sizeof( VG_HdrStruct ) + sizeof( SpTextInfo ) +
	           txtlen + 1;

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPTX_ELM;
         el.hdr.vg_class = CLASS_TEXT;
         el.hdr.maj_col  = 31;
         el.hdr.min_col  = 31;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = 0;
         el.hdr.grpnum   = 0;
         el.hdr.range_min_lat = ptrl->lat;
         el.hdr.range_min_lon = ptrl->lon;
         el.hdr.range_max_lat = ptrl->lat;
         el.hdr.range_max_lon = ptrl->lon;

	/* 
	 * Fill in low tropopause TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 1;
	 el.elem.spt.info.turbsym  = 4;
	 el.elem.spt.info.sztext   = 1.07F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = 0;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 2;
	 el.elem.spt.info.filcol   = 31;
	 el.elem.spt.info.lincol   = 31;
	 el.elem.spt.info.lat      = ptrl->lat;
	 el.elem.spt.info.lon      = ptrl->lon;
	 strcpy ( el.elem.spt.text, trpstr );

	/* 
	 * Write low tropopause TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptrl = ptrl->next;
     }

    /* 
     * Close output file. 
     */
     if ( fptr != NULL ) cvg_clos ( fptr, &ier );
     
     return;
}

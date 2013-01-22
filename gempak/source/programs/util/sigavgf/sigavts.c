#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  SYSIZE       1.7F
#define  SYWIDTH      802
#define  SYTYPE       0
#define  STMSYM      25.0F
#define  VOLSYM      201.0F
#define  RADSYM      41.0F

#define  LENBUF      256


void sigavts ( char *fhour, int numstm, storm_t *ptrs, int numvlr, 
               volrad_t *ptrv, int itime[], char grpch, char *chlvl, 
	       int *iret )
/************************************************************************
 * sigavts                                                              *
 *                                                                      *
 * This program encodes the High or Mid Level Significant Weather ASCII *
 * storm, volcano and radiation information into VG file format. 	*
 *                                                                      *
 * sigavts ( fhour, numstm, ptrs, numvlr, ptrv, itime, grpch, chlvl,    *
 *	     iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      numstm          int	   Number of storm symbols		*
 *      *ptrs		storm_t    Pointer to LABEL (storm) link list	*
 *      numvlr          int	   Number of storm symbols		*
 *      *ptrv		volrad_t   Pointer to LABEL (vol/rad) link list	*
 *      itime[]		int	   Issued date/time 			*
 *      grpch           char       Group type				*
 * 	*chlvl		char	   Chart level				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC     3/02	Created					*
 * A. Hardy/NCEP     7/02       Modified volcanic text creation		*
 * M. Li/SAIC	     1/05	Added chlvl				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function 	*
 * J. Lewis/AWC     11/07       Modified text font and font hw/sw flag  *
 * L. Hinson/AWC    09/12       Enable Radiation Symbol Text Label      *
 ***********************************************************************/
 {
    int   	    ii, ij, ier, txtlen, len;
    FILE  	    *fptr;
    storm_t         *ptr; 
    volrad_t        *ptr2;
    char            *ptr_char, *ptr_start;

    int		    numerr, leverr, wflg, tmptm, gpnum; 
    int             sizrec, start, iend;
    float           tmplat, tmplon;

    char 	    ofname[LENBUF], errgrp[8], stmstr[40];
    char            ctime[LENBUF], volstr[40];
    char            ltn[2], lne[2];
    Boolean	    volcano;
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
         strcpy ( ofname, "mvts_");
     }
     else {
         strcpy ( ofname, "vts_");
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
     * Loop through the storm element list and write each one to VG file.
     */

     ptr = ptrs;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < numstm; ii++) {


	/* 
	 * Fill in VG header information.
	 */

	 gpnum = ii+1;
	 sizrec = sizeof( VG_HdrStruct ) + sizeof( SymInfo ) +
	          sizeof (SymData);

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPSYM_ELM;
         el.hdr.vg_class = CLASS_SYMBOLS;
         el.hdr.maj_col  = 2;
         el.hdr.min_col  = 2;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = ptr->lat;
         el.hdr.range_min_lon = ptr->lon;
         el.hdr.range_max_lat = ptr->lat;
         el.hdr.range_max_lon = ptr->lon;

	/* 
	 * Fill in storm VG element information.
	 */

	 el.elem.sym.info.numsym = 1;
	 el.elem.sym.info.width  = SYWIDTH;
	 el.elem.sym.info.size   = SYSIZE; 
	 el.elem.sym.info.ityp   = SYTYPE;
         el.elem.sym.data.code[0] = STMSYM;
         el.elem.sym.data.latlon[0] = ptr->lat;
         el.elem.sym.data.latlon[0+el.elem.sym.info.numsym] = ptr->lon;
         el.elem.sym.data.offset_xy[0] = 0;
         el.elem.sym.data.offset_xy[0+el.elem.sym.info.numsym] = 0;

	/* 
	 * Write storm symbol element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 start = sizrec + start;

        /*
         * Assemble the storm text string. 
         */

	 sprintf( stmstr,"\"%s\"", ptr->name );
	 cst_rmbl ( stmstr, stmstr, &txtlen, &ier );

	/* 
	 * Fill in storm TEXT header information. 
	 */

         sizrec = (int)( sizeof( VG_HdrStruct ) + sizeof( SpTextInfo ) ) +
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
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = ptr->lat;
         el.hdr.range_min_lon = ptr->lon;
         el.hdr.range_max_lat = ptr->lat;
         el.hdr.range_max_lon = ptr->lon;

	/* 
	 * Fill in storm TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 5;
	 el.elem.spt.info.turbsym  = 4;
	 el.elem.spt.info.sztext   = 1.25F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = -4;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 31;
	 el.elem.spt.info.filcol   = 31;
	 el.elem.spt.info.lincol   = 31;
	 el.elem.spt.info.lat      = ptr->lat;
	 el.elem.spt.info.lon      = ptr->lon;
	 strcpy ( el.elem.spt.text, stmstr );

	/* 
	 * Write storm TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptr = ptr->next;
     }

    /*
     * Loop through the volcano and radiation element list and 
     * write each one to VG file.
     */

     ptr2 = ptrv;

     for ( ii = 0; ii < numvlr; ii++) {

	/* 
	 * Fill in VG header information.
	 */

	 gpnum = ii + 2;
	 sizrec = sizeof( VG_HdrStruct ) + sizeof( SymInfo ) +
	          sizeof (SymData);
        /*
         * Determine if volcano or radiation incident. Check if last
	 * character is a '_', then radiation.
	 */

         cst_lstr ( ptr2->name, &iend, &ier );
         if ( strrchr ( ptr2->name+iend-1, (int)'_' ) == NULL ) {
             volcano = True; 
             el.hdr.vg_type  = WXSYM_ELM;
             el.hdr.grptyp   = grpch;
             el.hdr.grpnum   = gpnum;
             el.elem.sym.data.code[0] = VOLSYM;
         }
	 else {
             volcano = False; 
             el.hdr.vg_type  = SPSYM_ELM;
             el.hdr.grptyp   = 0;
             el.hdr.grpnum   = 0;
             el.elem.sym.data.code[0] = RADSYM;
         }



         el.hdr.delete   = 0;
         el.hdr.vg_class = CLASS_SYMBOLS;
         el.hdr.maj_col  = 2;
         el.hdr.min_col  = 2;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.range_min_lat = ptr2->lat;
         el.hdr.range_min_lon = ptr2->lon;
         el.hdr.range_max_lat = ptr2->lat;
         el.hdr.range_max_lon = ptr2->lon;

	/* 
	 * Fill in volcano/radiaton VG element information.
	 */

	 el.elem.sym.info.numsym = 1;
	 el.elem.sym.info.width  = SYWIDTH;
	 el.elem.sym.info.size   = SYSIZE; 
	 el.elem.sym.info.ityp   = SYTYPE;
         el.elem.sym.data.latlon[0] = ptr2->lat;
         el.elem.sym.data.latlon[0+el.elem.sym.info.numsym] = ptr2->lon;
         el.elem.sym.data.offset_xy[0] = 0;
         el.elem.sym.data.offset_xy[0+el.elem.sym.info.numsym] = 0;

	/* 
	 * Write volcano/radiation element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 start = sizrec + start;

        /*
         * Assemble the volcano/radiation text string. 
         */
	 
	 volstr[0] = '\0';
	 tmplat = ptr2->lat;
         if (  tmplat > 0.00F ) {
	     strcpy ( ltn, "N" );
	 }
	 else if ( tmplat == 0.00F ) {
	     strcpy ( ltn, " " );
	 }
	 else {
	     tmplat = -1.0F * ptr2->lat;
	     strcpy ( ltn, "S" );
	 }
	 tmplon =  ptr2->lon;
         if (  tmplon > 0.00F ) {
	     strcpy ( lne, "E" );
	 }
         else if (  tmplon > 0.00F ) {
	     strcpy ( lne, " " );
	 }
	 else {
	     tmplon = -1.0F * ptr2->lon;
	     strcpy ( lne, "W" );
	 }
         if (! volcano) {
           /* Replace trailing "_" in Radiation symbol with space...*/
           ptr_start = ptr2->name;
           ptr_char = &ptr_start[strlen(ptr2->name)];
           while ( ptr_char > ptr_start && strchr("_", *(ptr_char - 1)) != NULL ) {
             ptr_char--;
    
           }
           *ptr_char = '\0';
         }
	 sprintf ( volstr,"%s\n%3.1f%s %4.1f%s", ptr2->name,
	           tmplat, ltn, tmplon, lne ); 
         cst_lstr ( volstr, &txtlen, &ier);

	/* 
	 * Fill in volcano TEXT header information. 
	 */

         sizrec = (int)( sizeof( VG_HdrStruct ) + sizeof( SpTextInfo ) ) 
			    + txtlen + 1;

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
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = ptr2->lat;
         el.hdr.range_min_lon = ptr2->lon;
         el.hdr.range_max_lat = ptr2->lat;
         el.hdr.range_max_lon = ptr2->lon;

 	/* 
	 * Fill in volcano TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 3;
	 el.elem.spt.info.turbsym  = 0;
	 el.elem.spt.info.sztext   = 1.0F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = -5;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 5;
	 el.elem.spt.info.filcol   = 5;
	 el.elem.spt.info.lincol   = 5;
	 el.elem.spt.info.lat      = ptr2->lat;
	 el.elem.spt.info.lon      = ptr2->lon;
	 strcpy ( el.elem.spt.text, volstr );

  	/* 
	 * Write storm TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 
	 ptr2 = ptr2->next;

     }


    /* 
     * Close output file. 
     */
     if ( fptr != NULL ) cvg_clos ( fptr, &ier );
     
     
     return;
}

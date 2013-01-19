#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  TURB_LINE   5 
#define  LTHW        0
#define  LWIDTH      3
#define  LWHW        0
#define  LENBUF      256


void sigatur ( char *fhour, int numtur, turb_t *ptrb, int itime[], 
               char grpch, char *chlvl, int *iret ) 
/************************************************************************
 * sigatur                                                              *
 *                                                                      *
 * This program encodes the High or Mid Level Significant Weather ASCII	*
 * turbulence information into VG file format. 				*
 *                                                                      *
 * sigatur ( fhour, numtur, ptrb, itime, grpch, chlvl, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      numtur          int	   Number of turbulence lines		*
 *      *ptrb		turb_t     Pointer to TURB link list		*
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
 * A. Hardy/SAIC     5/02	Round levels to nearest 10		*
 * J. Wu/SAIC        8/02	replace clo_cetr with cgr_centroid	*
 * M. Li/SAIC	     3/04	Use absolute values on checking aarea	*
 * M. Li/SAIC	     4/04	Modified the degree of turbulence	*
 * M. Li/SAIC	     9/04	Modified check on the degree of turb	*
 * M. Li/SAIC	     1/05	Added chlvl				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 * M. Li/SAIC       11/06       Check for longitude at 0 and 180 degree *
 * M. Hughes/GWDI    5/10	Added range check for height values	*
 ***********************************************************************/
 {
    int   	    ii, ij, jj, kk, ier, txtlen, len, iera, ierb;
    FILE  	    *fptr;
    turb_t          *ptr;

    int		    numerr, leverr, wflg, tmptm, gpnum ; 
    int             sizrec, start, tmpdeg, tmpmod; 
    float           xmxlat, xmnlat, xmxlon, xmnlon;
    float	    dist0, dist1;
    float           base, top, tmplv1, tmplv2, alat[MAXPTS], alon[MAXPTS]; 
    float           minBase, maxTop;
    float           blat[MAXPTS], blon[MAXPTS];
    float           xcent, ycent, area, axcent, aycent, aarea;
    float           bxcent, bycent, barea;
    char            level1[12], level2[12];
    char 	    ofname[LENBUF], errgrp[8], turstr[40];
    char            ctime[LENBUF], iclos;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret = 0;
     fptr   = NULL;
     ier     = 0;
     leverr  = 0;
     start   = 0;
     iclos   = 0;
     wflg    = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /* 
     * Create output filename.
     */

     if ( strcmp ( chlvl, "SWM" ) == 0 ) {
         strcpy ( ofname, "mcat_");
         minBase = 100.0;
         maxTop  = 450.0;
     }
     else {
         strcpy ( ofname, "cat_");
         minBase = 250.0;
         maxTop  = 630.0;
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
     * Loop through the turbulence element list and write each 
     * one to VG file.
     */

     ptr = ptrb;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < numtur; ii++) {

       /* 
	* Determine the maximum and minimum lat. and lon.
	*/

	for ( jj = 0; jj < ptr->npt; jj++ ) {
            if ( jj == 0 ) {
                xmxlat = ptr->lat[jj];
                xmnlat = ptr->lat[jj];
                xmxlon = ptr->lon[jj];
                xmnlon = ptr->lon[jj];
	    }
            else {
                xmxlat = G_MAX ( xmxlat, ptr->lat[jj] );
                xmnlat = G_MIN ( xmnlat, ptr->lat[jj] );
                xmxlon = G_MAX ( xmxlon, ptr->lon[jj] );
                xmnlon = G_MIN ( xmnlon, ptr->lon[jj] );
	    }
         }

	/*
         * Check if the line is closed.
         */

         if ( G_DIFF(ptr->lat[0], ptr->lat[ptr->npt -1]) ||
              G_DIFF(ptr->lon[0], ptr->lon[ptr->npt -1]) ) {
             iclos = 1;
         }
         else {
             iclos = 0;
         }
 


	/* 
	 * Fill in VG header information.
	 */

	 gpnum = ii+1;
	 sizrec = sizeof( VG_HdrStruct ) + sizeof( LineInfo ) +
	         ( ( ptr->npt * 2) * sizeof (float) );

         el.hdr.delete   = 0;
         el.hdr.vg_type  = LINE_ELM;
         el.hdr.vg_class = CLASS_LINES;
         el.hdr.maj_col  = 5;
         el.hdr.min_col  = 5;
         el.hdr.smooth   = 2;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = iclos;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = xmnlat;
         el.hdr.range_min_lon = xmnlon;
         el.hdr.range_max_lat = xmxlat;
         el.hdr.range_max_lon = xmxlon;

	/* 
	 * Fill in VG element information.
	 */

	 el.elem.lin.info.numpts = ptr->npt;
	 el.elem.lin.info.lintyp = TURB_LINE;  
	 el.elem.lin.info.lthw   = LTHW;
	 el.elem.lin.info.width  = LWIDTH;
	 el.elem.lin.info.lwhw   = LWHW;
         for ( kk = 0; kk < el.elem.spl.info.numpts; kk++ ) {
             el.elem.lin.latlon[kk] = ptr->lat[kk];
             el.elem.lin.latlon[kk+el.elem.lin.info.numpts] = ptr->lon[kk];
        }

	/* 
	 * Write turbulence LINE element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 start = sizrec + start;

	/* 
	 * Begin creating the turbulence TEXT string.
         * Determine the turbulence base and top.
         */

         if ( G_DIFF(ptr->level1, SIGRLMS) ) {
             strcpy( level1,"XXX");
	 }
         else if ( !G_DIFF(ptr->level1, SIGRLMS) ) {
	     base = ptr->level1;
             tmplv1 = pr_hgmf (&base);
             tmplv1 = tmplv1/ 100.0F;
	     tmpmod = ( (int) tmplv1 % 10 );
	     if (  tmpmod >= 5 ) {
		tmplv1 = tmplv1 + (float)( 10 - tmpmod );
	     }
	     else {
		tmplv1 = tmplv1 - (float)tmpmod;
	     }
             if (tmplv1 < minBase) {
                strcpy(level1, "XXX");
	     }
             else {
	        cst_inch ( (int) tmplv1, level1, &ier ); 
	     }
         }

         if ( G_DIFF(ptr->level2, SIGRLMS)) {
             strcpy( level2,"XXX");
	 }
         else if ( !G_DIFF(ptr->level2, SIGRLMS) ) {
	     top = ptr->level2;
             tmplv2 = pr_hgmf (&top);
             tmplv2 = tmplv2/ 100.0F;
	     tmpmod = ( (int) tmplv2 % 10 );
	     if (  tmpmod >= 5 ) {
		tmplv2 = tmplv2 + (float)( 10 - tmpmod );
	     }
	     else {
		tmplv2 = tmplv2 - (float)tmpmod;
	     }
             if (tmplv2 > maxTop) {
                strcpy(level2, "XXX");
	     }
             else {
	        cst_inch ( (int) tmplv2, level2, &ier ); 
	     }
         }

        /*
         * Determine the degree of turbulence.
         */

	 /* Severe */
         if ( ptr->tdeg == 7 ) {
             tmpdeg = 6;
         }

	 /* Moderate, occasional severe */
         else if ( ptr->tdeg == 12 ||  ptr->tdeg == 19) {
             tmpdeg = 56;
         }
	
	 /* Moderate */
         else if ( ptr->tdeg == 6 ) {
             tmpdeg = 4;
         }
         else if ( ptr->tdeg == SIGIMSS ) {
             tmpdeg = 0;
         }

        /*
         * Assemble the turbulence text string. 
         */

	 sprintf ( turstr, "%s/%s", level2, level1);

        /*
         * Determine the lat/lon position for the text box. 
         * If line crosses dateline, place box in the larger area.
         */
	 jj = 0;
	 kk = 0;
	 for ( ij = 0; ij < ptr->npt; ij++ ) {
	    if ( ptr->lon[ij] >= 0.0F ) {
	        alat[jj] = ptr->lat[ij]; 
	        alon[jj] = ptr->lon[ij]; 
		jj++;
	    }
	    else {
	        blat[kk] = ptr->lat[ij]; 
	        blon[kk] = ptr->lon[ij]; 
		kk++;
	    }
	 }

	/*
	 * Determing the label placement in the area.
	 */

	 if ( ( jj > 0 ) && ( kk > 0 ) ) {

	   /*
	    * Close the two areas.
	    */

	     alat[jj] = alat[0]; 
	     alon[jj] = alon[0]; 
	     blat[kk] = blat[0]; 
	     blon[kk] = blon[0]; 

	     cgr_centroid (alat, alon, &jj, &axcent, &aycent, &aarea, &iera );
	     cgr_centroid (blat, blon, &kk, &bxcent, &bycent, &barea, &ierb );
	     jj++;
	     kk++;

	     if ( iera != -1 && ierb != -1 ) {
                 if ( G_ABS (aarea) > G_ABS (barea) ) {
                    xcent = axcent;
                    ycent = aycent;
                 }
                 else {
                    xcent = bxcent; 
                    ycent = bycent;
                 }
             } else if ( iera != -1 && ierb == -1 ) {
                xcent = axcent;
                ycent = aycent;
             } else if ( iera == -1 && ierb != -1 ) { 
                xcent = bxcent;
                ycent = bycent;
             } else {
                xcent = 0.5F * ( xmnlat + xmxlat ); 
                ycent = 0.5F * ( xmnlon + xmxlon );

                dist0 = G_ABS(xmnlon) + G_ABS(xmxlon);
                dist1 = G_ABS(G_ABS(xmnlon) - 180.0F) + G_ABS(G_ABS(xmxlon) - 180.0F);

                if ( dist1 < dist0 ) {
                    if ( ycent > 0 )
                        ycent += -180.0F;
                    else
                        ycent += 180.0F;
                }

             }
	 }
	 else {
	     cgr_centroid (ptr->lat,ptr->lon, &ptr->npt, &xcent, &ycent, 
	               &area, &ier );
	 }

	/* 
	 * Fill in turbulence TEXT header information. 
	 */

	 cst_lstr ( turstr, &txtlen, &ier );
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
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = (float)xcent;
         el.hdr.range_min_lon = (float)ycent;
         el.hdr.range_max_lat = (float)xcent;
         el.hdr.range_max_lon = (float)ycent;

	/* 
	 * Fill in turbulence TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp  = 9;
	 el.elem.spt.info.turbsym  = tmpdeg;
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
	 el.elem.spt.info.lat      = (float)xcent;
	 el.elem.spt.info.lon      = (float)ycent;
	 strcpy ( el.elem.spt.text, turstr );

	/* 
	 * Write turbulence TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptr = ptr->next;
     }

    /* 
     * Close output file. 
     */
     if ( fptr != NULL ) cvg_clos ( fptr, &ier );
     
     return;
}

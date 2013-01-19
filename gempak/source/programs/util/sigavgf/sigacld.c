#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  CLOUD_LINE  3
#define  LMULT       1
#define  LDIR        1
#define  LSIZE       0.800F
#define  LWID        2
#define  LENBUF      256


void sigacld ( char *fhour, int numcld, cloud_t *ptrc, int itime[], 
               char grpch, int *iret ) 
/************************************************************************
 * sigacld                                                              *
 *                                                                      *
 * This program encodes the High Level Significant Weather ASCII cloud  *
 * information into VG file format.   					*
 *                                                                      *
 * sigacld ( fhour, numcld, ptrc, itime, grpch, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      numcld          int	   Number of cloud lines		*
 *      *ptrc		cloud_t    Pointer to CLOUD link list		*
 *      itime[]		int	   Issued date/time 			*
 *      grpch           char       Group type				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC     3/02	Created					*
 * A. Hardy/SAIC     5/02	Round levels nearest 10;check +/- area  *
 *                              to see if lat/lon need to be flipped	*
 * J. Wu/SAIC        8/02	replace clo_cetr with cgr_centroid	*
 * A. Hardy/NCEP     4/03       Added check for negative longitudes	*
 * M. Li/SAIC        3/04	Remove check for flipped or clockwise	*
 * M. Li/SAIC       01/05	Removed jtime				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 * M. Li/SAIC       11/06       Check for longitude at 0 and 180 degree *
 * L. Hinson/AWC     3/12       Correct Cloud Distribution Wording on CB*
 ***********************************************************************/
 {
    int   	    ii, ij, jj, kk, ier, txtlen, len, iera, ierb;
    FILE  	    *fptr;
    cloud_t         *ptr;

    int		    numerr, leverr, wflg, tmptm, gpnum; 
    int             sizrec, start, tmpmod;
    float           xmxlat, xmnlat, xmxlon, xmnlon;
    float	    dist0, dist1;
    float           base, top, tmplv1, tmplv2, alat[MAXPTS], alon[MAXPTS];
    float           blat[MAXPTS], blon[MAXPTS];
    float           tmplat[MAXPTS], tmplon[MAXPTS];
    float           xcent, ycent, area, axcent, aycent, aarea;
    float           bxcent, bycent, barea;
    char            level1[12], level2[12], tmpdis[16], tmptyp[16];
    char 	    ofname[LENBUF], errgrp[8], cldstr[40];
    char            ctime[LENBUF], iclos;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret   = 0;
     fptr    = NULL;
     ier     = 0;
     leverr  = 0;
     start   = 0;
     wflg    = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /* 
     * Create output filename.
     */

     strcpy ( ofname, "cld_");

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
     * Loop through the cloud element list and write each one to VG file.
     */

     ptr = ptrc;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < numcld; ii++) {
        ij = 0;
        for ( jj = (ptr->npt - 1); jj >= 0; jj-- ) {
           tmplat[jj] = ptr->lat[ij];
           tmplon[jj] = ptr->lon[ij];
           ij++;
        } 
        for ( jj = 0; jj < ptr->npt; jj++ ) {
           ptr->lat[jj] = tmplat[jj];
           ptr->lon[jj] = tmplon[jj];
   	}

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
	 sizrec = (int)(sizeof( VG_HdrStruct ) + sizeof( SpLineInfo )) +
	         ( ( ptr->npt * 2) * (int)sizeof (float) );

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPLN_ELM;
         el.hdr.vg_class = CLASS_LINES;
         el.hdr.maj_col  = 6;
         el.hdr.min_col  = 6;
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

	 el.elem.spl.info.numpts = ptr->npt;
	 el.elem.spl.info.spltyp = CLOUD_LINE;  
	 el.elem.spl.info.splstr = LMULT; 
	 el.elem.spl.info.spldir = LDIR;
	 el.elem.spl.info.splsiz = LSIZE;
	 el.elem.spl.info.splwid = LWID;
         for ( kk = 0; kk < el.elem.spl.info.numpts; kk++ ) {
             el.elem.spl.latlon[kk] = ptr->lat[kk];
             el.elem.spl.latlon[kk+el.elem.spl.info.numpts] = ptr->lon[kk];
        }

	/* 
	 * Write cloud LINE element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 start = sizrec + start;

	/* 
	 * Begin creating the cloud TEXT string.
         * Determine the cloud base and top.
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

	     cst_inch ( (int) tmplv1, level1, &ier ); 
         }

         if ( G_DIFF(ptr->level2, SIGRLMS) ) {
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
	     cst_inch ( (int) tmplv2, level2, &ier ); 
         }

        /*
         * Determine the cloud distribution.
         */

         if ( ptr->clddist == 12 ) {
             strcpy( tmpdis, "FRQ");
         }
         else if ( ptr->clddist == 11 ) {
             strcpy( tmpdis, "OCNL\n");
             strcat( tmpdis, "EMBD");
         }
         else if ( ptr->clddist == 10 ) {
           strcpy ( tmpdis, "OCNL");
         }
         else if ( ptr->clddist == 9 ) {
             strcpy( tmpdis, "ISOL\n");
             strcat( tmpdis, "EMBD");
         }
         else if ( ptr->clddist == 8 ) {
             strcpy ( tmpdis, "ISOL");
         }

        /*
         * Determine the cloud type. 
         */

         if ( ptr->cldtyp == 9 ) {
             strcpy( tmptyp, "CB");
         }

        /*
         * Assemble the cloud text string. 
         */

	 sprintf( cldstr,"%s\n%s\n%s\n%s", tmpdis, tmptyp, 
	          level2, level1);

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

	 if ( ( jj > 0 ) && ( kk > 0 ) ) {
	     cgr_centroid (alat, alon, &jj, &axcent, &aycent, &aarea, &iera );
             cgr_centroid (blat, blon, &kk, &bxcent, &bycent, &barea, &ierb );

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
	     cgr_centroid (ptr->lat, ptr->lon, &ptr->npt, &xcent, &ycent, 
	               &area, &ier );
	 }

	/* 
	 * Fill in cloud TEXT header information. 
	 */

	 cst_lstr ( cldstr, &txtlen, &ier );
         sizrec = (int)(sizeof( VG_HdrStruct ) + sizeof( SpTextInfo )) +
	           txtlen + 1;

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPTX_ELM;
         el.hdr.vg_class = CLASS_TEXT;
         el.hdr.maj_col  = 31;
         el.hdr.min_col  = 31;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 1;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = (float)xcent;
         el.hdr.range_min_lon = (float)ycent;
         el.hdr.range_max_lat = (float)xcent;
         el.hdr.range_max_lon = (float)ycent;

	/* 
	 * Fill in cloud TEXT element information. 
	 */

	 el.elem.spt.info.sptxtyp = 4;
	 el.elem.spt.info.turbsym  = 4;
	 el.elem.spt.info.sztext   = 1.07F;
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
	 strcpy ( el.elem.spt.text, cldstr );

	/* 
	 * Write cloud TEXT element information.
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

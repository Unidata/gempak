#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  FWID        	4
#define  FSIZE		70
#define  FPDIR		1
#define  FSTRK		1

#define  ITCZ_LINE      23
#define  LMULT           1
#define  LDIR            1
#define  LSIZE         1.0F
#define  LWID            2


#define  LENBUF      	256

void sigafrt ( char *fhour, int numfrt, front_t *ptrf, int itime[], 
               char grpch, char *chlvl, int *iret ) 
/************************************************************************
 * sigafrt                                                              *
 *                                                                      *
 * This program encodes the High or Mid Level Significant Weather ASCII *
 * front information into VG file format.   				*
 *                                                                      *
 * sigafrt ( fhour, numfrt, ptrf, itime, grpch, chlvl, iret )		* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      numfrt          int	   Number of front lines		*
 *      *ptrf		front_t    Pointer to FRONT link list		*
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
 * A. Hardy/SAIC     5/02	Added ITCZ line/text 			*
 * M. Li/SAIC	     3/04	Convert speed and direction		*
 * M. Li/SAIC	    12/04	Checked for slow front			*
 * M. Li/SAIC	    01/05	Added chlvl				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 ***********************************************************************/
 {
    int   	    ii, ij, jj, kk, ier, txtlen, len, middle;
    FILE  	    *fptr;
    front_t         *ptr;

    int		    numerr, leverr, wflg, tmptm, gpnum; 
    int             sizrec, start, majcol, mincol, fcode, ahead, back;
    float           xmxlat, xmnlat, xmxlon, xmnlon;
    float           ang1, ang2, angle, flat, flon, dist, angle3;
    double	    dlta;
    char 	    ofname[LENBUF], errgrp[8], frtstr[10];
    char            ctime[LENBUF];
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret   = 0;
     fptr    = NULL;
     ier     = 0;
     leverr  = 0;
     start   = 0;
     dlta    = 0.001;
     wflg    = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /* 
     * Create output filename.
     */

     if ( strcmp ( chlvl, "SWM") == 0 ) {
         strcpy ( ofname, "mfrt_");
     }
     else {
	 strcpy ( ofname, "frt_");
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
     * Loop through the front element list and write each one to VG file.
     */

     ptr = ptrf;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < numfrt; ii++) {

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
	 * Set front colors for quasi-stationary, warm ,cold, and
	 * occluded fronts and create the front code.
	 */
	 if ( ptr->ftype == 0 ) {
	    majcol = 2;
	    mincol = 4;
	 }
	 else if ( ptr->ftype == 2 ) {
	    majcol = 2;
	    mincol = 2;
	 }
	 else if ( ptr->ftype == 4 ) {
	    majcol = 4;
	    mincol = 4;
	 }
	 else if ( ptr->ftype == 6 ) {
	    majcol = 29;
	    mincol = 29;
	 }
	 else if ( ptr->ftype == 8 ) {
	    majcol = 8;
	    mincol = 8;
	 }
	 fcode = ( ptr->ftype * 100 ) + 40;

	/* 
	 * Fill in VG header information.
	 */

	 gpnum = ii+1;

	 if ( ptr->ftype < 7 ){

	     sizrec = sizeof( VG_HdrStruct ) + sizeof( FrontInfo ) +
                  ( ( ptr->npt * 2) * sizeof (float) );

             el.hdr.delete   = 0;
             el.hdr.vg_type  = FRONT_ELM;
             el.hdr.vg_class = CLASS_FRONTS;
             el.hdr.maj_col  = majcol;
             el.hdr.min_col  = mincol; 
             el.hdr.smooth   = 2;
             el.hdr.version  = 0;
             el.hdr.filled   = 0;
             el.hdr.closed   = 0;
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

	     el.elem.frt.info.numpts = ptr->npt;
	     el.elem.frt.info.fcode  = fcode; 
	     el.elem.frt.info.fpipsz = FSIZE;
	     el.elem.frt.info.fpipst = FSTRK;
	     el.elem.frt.info.fpipdr = FPDIR;
	     el.elem.frt.info.fwidth = FWID;
	     strcpy ( el.elem.frt.info.frtlbl, "STJ"); 
             for ( kk = 0; kk < el.elem.frt.info.numpts; kk++ ) {
                 el.elem.frt.latlon[kk] = ptr->lat[kk];
                 el.elem.frt.latlon[kk+el.elem.frt.info.numpts] = 
		                            ptr->lon[kk];
             }
	 }
         else {
             sizrec = sizeof( VG_HdrStruct ) + sizeof( SpLineInfo ) +
	              ( ( ptr->npt * 2) * sizeof (float) );

             el.hdr.delete   = 0;
             el.hdr.vg_type  = SPLN_ELM;
             el.hdr.vg_class = CLASS_LINES;
             el.hdr.maj_col  = majcol;
             el.hdr.min_col  = mincol;
             el.hdr.smooth   = 2;
             el.hdr.version  = 0;
             el.hdr.filled   = 0;
             el.hdr.closed   = 0;
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
	     el.elem.spl.info.spltyp = ITCZ_LINE;  
	     el.elem.spl.info.splstr = LMULT; 
	     el.elem.spl.info.spldir = LDIR;
	     el.elem.spl.info.splsiz = LSIZE;
	     el.elem.spl.info.splwid = LWID;
             for ( kk = 0; kk < el.elem.spl.info.numpts; kk++ ) {
                 el.elem.spl.latlon[kk] = ptr->lat[kk];
                 el.elem.spl.latlon[kk+el.elem.spl.info.numpts] = ptr->lon[kk];
            }

	 }

	/* 
	 * Write front FRONT/LINE element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 if (  ptr->ftype < 7 ) {
	    /* 
	     * Begin creating the FRONT TEXT string.
             */

             for ( kk = 0; kk < ptr->npt; kk++ ) {

	         if ( ptr->fntdir[kk] >= 0.0F ) {

	             start = sizrec + start;
                     sizrec = sizeof( VG_HdrStruct ) + 
		                sizeof( WindInfo ) + sizeof (WindData);

	            /* 
	             * Fill in front WIND ARROW header information. 
	             */

		     el.hdr.delete   = 0;
                     el.hdr.vg_type  = ARROW_ELM;
                     el.hdr.vg_class = CLASS_WINDS;
                     el.hdr.maj_col  = 1;
                     el.hdr.min_col  = 1;
                     el.hdr.smooth   = 0;
                     el.hdr.version  = 0;
                     el.hdr.filled   = 0;
                     el.hdr.closed   = 1;
                     el.hdr.recsz    = sizrec; 
                     el.hdr.grptyp   = grpch;
                     el.hdr.grpnum   = gpnum;
                     el.hdr.range_min_lat = ptr->lat[kk];
                     el.hdr.range_min_lon = ptr->lon[kk];
                     el.hdr.range_max_lat = ptr->lat[kk];
                     el.hdr.range_max_lon = ptr->lon[kk];

		     el.elem.wnd.info.numwnd = 1;
		     el.elem.wnd.info.width  = 1;
		     el.elem.wnd.info.size   = 1.0F;
		     el.elem.wnd.info.wndtyp = 114;
		     el.elem.wnd.info.hdsiz  = 0.70F;
		     el.elem.wnd.data.latlon[0] =  ptr->lat[kk];
		     el.elem.wnd.data.latlon[0+el.elem.wnd.info.numwnd] = ptr->lon[kk];
		     el.elem.wnd.data.spddir[0] = 10.0F;

		     if ( ptr->fntdir[kk] < 180.0F ) {
	                 ptr->fntdir[kk] += 180.0F;
		     }
		     else {
			 ptr->fntdir[kk] -= 180.0F;
		     } 		
		     el.elem.wnd.data.spddir[0+el.elem.wnd.info.numwnd] = ptr->fntdir[kk];

	            /* 
	             * Write front WIND ARROW element.
	             */

                     cvg_writeD ( &el, start, sizrec, fptr, &ier);
	         }

	         if ( (ptr->fntspd[kk] > 0.0F) || 
		      (ptr->fntspd[kk] == 0.0F && ptr->fntdir[kk] < 0.0F) ) {

	             start = sizrec + start;

	            /* 
	             * Fill in front TEXT header information. 
	             */


		     cst_inch ( (int) pr_mskn ( &(ptr->fntspd[kk]) ), frtstr, &ier );
	             cst_lstr ( frtstr, &txtlen, &ier );
                     sizrec = sizeof( VG_HdrStruct ) + 
		            sizeof( SpTextInfo ) + txtlen + 1;

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
                     el.hdr.range_min_lat = ptr->lat[kk];
                     el.hdr.range_min_lon = ptr->lon[kk];
                     el.hdr.range_max_lat = ptr->lat[kk];
                     el.hdr.range_max_lon = ptr->lon[kk];
    
                    /* 
    	             * Fill in front TEXT element information. 
    	             */

	             el.elem.spt.info.sptxtyp  = 5;
	             el.elem.spt.info.turbsym  = 4;
	             el.elem.spt.info.sztext   = 1.25F;
	             el.elem.spt.info.itxfn    = 2;
	             el.elem.spt.info.iwidth   = 1;
	             el.elem.spt.info.rotn     = 0.0F;
	             el.elem.spt.info.ialign   = 0;
	             el.elem.spt.info.offset_x = 9;
	             el.elem.spt.info.offset_y = 4;
	             el.elem.spt.info.ithw     = 1;
	             el.elem.spt.info.txtcol   = 31;
	             el.elem.spt.info.filcol   = 31;
	             el.elem.spt.info.lincol   = 31;
	             el.elem.spt.info.lat      = ptr->lat[kk];
	             el.elem.spt.info.lon      = ptr->lon[kk];
	             strcpy ( el.elem.spt.text, frtstr );

	            /* 
	             * Write front TEXT element information.
	             */

                     cvg_writeD ( &el, start, sizrec, fptr, &ier);
	         }
	     }
	 }
	 else {

	    /* 
	     * Set up TEXT element for an ITCZ line.
	     */
	             start = sizrec + start;
                     sizrec = sizeof( VG_HdrStruct ) + 
		              sizeof( SpTextInfo ) + 5;

                     /*
		      * Determine up stream angle (angle 1).
		      */

                      middle = ptr->npt/2;
                      back = middle;
                      while ( ( ( fabs((double)(ptr->lat[back] - ptr->lat[back-1])) < dlta ) 
                             && ( fabs ((double)(ptr->lon[back] - ptr->lon[back-1])) < dlta ) )
			     && ( back != 0 ) ) {
                             back = back - 1;
                      }

		      clo_direct ( &ptr->lat[back], &ptr->lon[back], 
		                      &ptr->lat[back-1], &ptr->lon[back-1], 
		                      &ang1, &ier);

		     /*
		      * Determine down stream angle (angle 2).
		      */

                      ahead = middle;
                      while ( ( ( fabs((double)(ptr->lat[ahead] - ptr->lat[ahead+1])) < dlta ) 
                            && ( fabs ((double)(ptr->lon[ahead] - ptr->lon[ahead+1])) < dlta ) )
			    && ( ahead <=  ptr->npt ) ) {
                            ahead = ahead + 1;
                      }

		      clo_direct ( &ptr->lat[ahead], &ptr->lon[ahead], 
		                    &ptr->lat[ahead+1], &ptr->lon[ahead+1], 
		                    &ang2, &ier);

                     /*
		      * Determin text angle.
		      */

		      if ( ang2 > 179.0F ) {
		          ang2 = ang2 - 180.0F;
			  angle = ( ( ang1 + ang2 ) / 2.0F ) + 180.0F;
		      }
                      else {
		   	  if ( ( ang2 - ang1) < 0.0F ) {
			      ang1 = (float)fabs ((double)(ang1 - 180.0F));
		   	  }
		   	  angle =  ( ang1 + ang2 ) / 2.0F ;
		      }
		      dist = 300000.0F;

		     angle3 = angle + 90.0F;
		     angle =  270.0F - angle ;
		     if ( angle < 0.0F ) {
			    angle += 360.0F;
		     }
	             clo_dltln ( &ptr->lat[middle], &ptr->lon[middle], &dist,
		                     &angle3, &flat, &flon, &ier);

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
                     el.hdr.range_min_lat = flat;
                     el.hdr.range_min_lon = flon;
                     el.hdr.range_max_lat = flat;
                     el.hdr.range_max_lon = flon;

	            /* 
	             * Fill in ITCZ TEXT element information. 
	             */

	             el.elem.spt.info.sptxtyp  = 5;
	             el.elem.spt.info.turbsym  = 4;
	             el.elem.spt.info.sztext   = 1.35F;
	             el.elem.spt.info.itxfn    = 2;
	             el.elem.spt.info.iwidth   = 1;
	             el.elem.spt.info.rotn     = angle + 1000.0F;
	             el.elem.spt.info.ialign   = 0;
	             el.elem.spt.info.ithw     = 1;
	             el.elem.spt.info.txtcol   = majcol;
	             el.elem.spt.info.filcol   = 31;
	             el.elem.spt.info.lincol   = 31;
	             el.elem.spt.info.offset_x = 0;
	             el.elem.spt.info.offset_y = 0;
	             el.elem.spt.info.lat      = flat;
	             el.elem.spt.info.lon      = flon;
	             strcpy ( el.elem.spt.text, "ITCZ");

	            /* 
	             * Write ITCZ TEXT element information.
	             */

                     cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 }

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

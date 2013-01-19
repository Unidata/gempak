#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define  EOL  "\n"
#define  LAT_LON_MX   9

void wbc_wcp ( int *ibun, char *dattim, char **wtype, char **wstart, 
	       char **wend, char **wnum, char **wlatlon, int *iret )
/************************************************************************
 * wbc_wcp                                                              *
 *                                                                      *
 * This function formats the Watch Corner Point text product. It can	*
 * create a VG file with the WCP latitudes and longitudes.		*
 *                                                           	        *
 * wbc_wcp ( int ibun, char *dattim, char *wtype, char *wstart, 	*
 *           char *wend, char *wnum, char *wlatlon, int *iret) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *	ibun		int	Number of active watches in the array	*
 *	dattim		char	System time in GEMPAK format		*	
 *	wtype[ibun]	char	Watch type array			*
 *	wstart[ibun]	char	Watch start time array - GEMPAK format	*	
 *	wend[ibun]	char	Watch end time array - GEMPAK format	*	
 *	wnum[ibun]	char	Watch number array			*
 *	wlatlon[ibun]	char	Watch lat/lon string array		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 4/05   Created					*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function     *
 ***********************************************************************/
{
    FILE    *ifpwcp;
    int     ii, jj, ik, narr, npts, itmarr[5], vtime[5], datwk;
    int     ll, ier, etime[5], ilen, sizrec;
    int     istyp, isyr, ismon, isday, ishr, ismin, issec, ijulian;
    int	    icol,loc,npout, inum, txtlen, iervg, ierr;
    char    **arylst, **arypts, tmpstr[84], hold[16], tag[25];
    char    ifname[256], chmon[4], chdwk[4], tmzn[4], value[120];
    float   lat, lon, newlat, newlon, vlat[LAT_LON_MX], vlon[LAT_LON_MX];
    float   alat, alon, xmxlat,xmxlon, xmnlat, xmnlon;
    char    vgfile[128], tblnam[72], dirsym[160], wlabel[80];
    VG_DBStruct     el_lin;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0; 
    istyp = 1;
    inum  = *ibun;

   /*
    *  Create output file for appending.
    */
    sprintf ( ifname, "KWNSWCPSPC");
    ifpwcp = cfl_wopn ( ifname, &ier );

   /*
    * Check to see if the WCP VG file is to be created.
    */

    strcpy ( tblnam, "woudef.tbl" );
    strcpy ( dirsym, "txtprd" );
    
    strcpy ( tag, "WCP_VGF");
    ctb_rdprf ( tblnam, dirsym, tag, value, &ier );

    if (strcmp ( value, "TRUE") == 0 ) {
	strcpy (vgfile, "KWNSWCPSPC.vgf");
        cvg_crvgf ( vgfile, &iervg);
        if ( iervg != 0 )  {
            ierr = -16;
            er_wmsg ( "WBC", &ierr, vgfile, &ier, 3, strlen(vgfile) );
        }
    }

   /*
    * Storing the date/time.
    */

    strcpy ( chmon, " " );
    css_date ( &istyp, &isyr, &ismon, &isday, &ishr, &ismin, &issec,
                   &ijulian, tmzn, &ier);
    if (strcmp ( dattim, " " ) != 0 ) {
        ti_ctoi ( dattim, itmarr, &ier, strlen(dattim));
	isyr = itmarr[0];
	ismon = itmarr[1];
	isday = itmarr[2];
	ishr = itmarr[3];
	ismin = itmarr[4];
    }

    ti_dayw( itmarr, &datwk, &ier );
    utl_gdat ( ismon, datwk, chmon, chdwk, &ier );

   /*
    * Set up the WMO/AWIPS header lines.
    */

    fprintf ( ifpwcp, "WWUS60 KWNS %02d%02d%02d\n", isday, ishr, ismin);
    fprintf ( ifpwcp, "WCPSPC\n\n");

   /* 
    * If there are not any active watches create this message.
    */
    if ( inum == 0 ) {
        fprintf ( ifpwcp, "FILE CREATED %02d-%s-%02d AT %02d:%02d:%02d UTC\n",
                      isday, chmon, isyr % 100, ishr, ismin, issec);
        fprintf ( ifpwcp, "NO WATCHES CURRENTLY ACTIVE.\n\n");
    }
    else {
        for ( ii = 0 ; ii < inum; ii++ ) {
            ti_ctoi ( wstart[ii], vtime, &ier, strlen(wstart[ii]) );
            ti_ctoi ( wend [ii], etime, &ier, strlen(wend[ii]) );

           /*
            * Create the times listing of active watches. 
            */

            if ( strcmp(wtype[ii],"TOR") == 0 ) {
		icol = 2;
                fprintf ( ifpwcp, "SEVR %02d%02d%02d %02d%02d WT%04d %02d%02d\n", 
	              vtime[0] %100, vtime[1], vtime[2], vtime[3], vtime[4], 
		      atoi(wnum[ii]), etime[3], etime[4] );

            }
            else if ( strcmp(wtype[ii],"TSM") == 0 ) {
		icol = 6;
                fprintf ( ifpwcp, "SEVR %02d%02d%02d %02d%02d WS%04d %02d%02d\n", 
	              vtime[0] %100, vtime[1], vtime[2], vtime[3], vtime[4], 
		      atoi(wnum[ii]), etime[3], etime[4] );
            }

           /*
            * Create the lat/lon listing of active watches. 
            */

	    arylst = (char **)malloc(LAT_LON_MX * sizeof(char *));
	    for ( ik = 0; ik < LAT_LON_MX; ik++ ) {
	        arylst[ik] = (char *) malloc(24) ;
	    }
	    cst_clst(wlatlon[ii], ';', " ", LAT_LON_MX, 24, arylst,
			&narr, &ier);

	    arypts = (char **) malloc( LAT_LON_MX * sizeof(char *));
	    for ( ik = 0; ik < LAT_LON_MX; ik++ ) {
	        arypts[ik] = (char *) malloc(24) ;
	    }

            /*
             * Converting lat/lons from hundreths into minutes.
             */

	     tmpstr[0] = '\0';
	     for ( jj = 0; jj < narr-1; jj++) {
	         hold[0] = '\0';

	         cst_clst(arylst[jj], ',', " ", 2, 24, arypts, &npts, &ier);

	         cst_crnm ( arypts[0], &lat, &ier);
	         cst_crnm ( arypts[1], &lon, &ier);
		 vlat[jj] = lat;
		 vlon[jj] = lon;
	         newlat = DDTODM (G_ABS ( lat ) );
	         newlon = DDTODM (G_ABS ( lon ) );

                /*  The statement below will truncate the minutes 
                 *  at the 100's position. No rounding takes place.
                 */

	         sprintf( hold," %05d.%05d", (int) (newlat), (int)(newlon));
	         cst_lstr ( hold, &ilen, &ier );
	         cst_ncat ( tmpstr, hold, &ilen, &ier );

	     }


	    strcat (tmpstr, ";");
	    strcat (tmpstr, EOL );
	    cst_ldsp ( tmpstr, tmpstr, &ll, &ier );
            fprintf ( ifpwcp, tmpstr);
	    fprintf ( ifpwcp, EOL );

	   /*
	    * Write out rebundled lines.
	    */
            if (( strcmp ( value, "TRUE") == 0 ) && ( iervg == 0 ) ) {
               /* 
                * Determine the maximum and minimum lat. and lon.
                */

                for ( jj = 0; jj < narr; jj++ ) {
                    if ( jj == 0 ) {
                        xmxlat = vlat[jj];
                        xmnlat = vlat[jj];
                        xmxlon = vlon[jj];
                        xmnlon = vlon[jj];
                    }
                    else {
                        xmxlat = G_MAX ( xmxlat, vlat[jj] );
                        xmnlat = G_MIN ( xmnlat, vlat[jj] );
                        xmxlon = G_MAX ( xmxlon, vlon[jj] );
                        xmnlon = G_MIN ( xmnlon, vlon[jj] );
                    }
                 }


		npout = narr;
                el_lin.hdr.vg_type = LINE_ELM;
                el_lin.hdr.vg_class = CLASS_LINES;
		el_lin.hdr.smooth = 0;
                cvg_initelm ( &el_lin );
		el_lin.hdr.closed = 0;
                el_lin.hdr.recsz = sizrec;
                el_lin.hdr.filled = (char)0;
                el_lin.hdr.grptyp = (char)0;
                el_lin.hdr.grpnum = 0;
                el_lin.hdr.maj_col = icol;
                el_lin.hdr.min_col = icol;
                el_lin.elem.lin.info.lintyp = 1;
                el_lin.elem.lin.info.width = 3;
		el_lin.elem.lin.info.lwhw = 0;
		el_lin.elem.lin.info.lthw =0;
                el_lin.hdr.range_min_lat = xmnlat;
                el_lin.hdr.range_min_lon = xmnlon;
                el_lin.hdr.range_max_lat = xmxlat;
                el_lin.hdr.range_max_lon = xmxlon;

                el_lin.elem.lin.info.numpts = npout;
                for ( ll = 0; ll < npout; ll++ )  {
                    el_lin.elem.lin.latlon[ll      ] = vlat[ll];
                    el_lin.elem.lin.latlon[ll+npout] = vlon[ll];
                }

               /*
                * Check if the line is closed.
                */

                if ( !G_DIFF(vlat[0], vlat[narr -1]) ||
                     !G_DIFF(vlon[0], vlon[narr -1]) ) {
                    el_lin.elem.lin.latlon[ll-1      ] = vlat[0];
                    el_lin.elem.lin.latlon[(ll-1)+npout] = vlon[0];
                }
                el_lin.hdr.recsz = sizeof( VG_HdrStruct ) + sizeof( LineInfo ) +
                          npout*2*sizeof(float);

                cvg_writefD( &el_lin, -1, el_lin.hdr.recsz, vgfile, &loc,
                            &ier );

               /*
                * Create the text label. First get the position.
                */
                gg_wlbl (&npout, vlat, vlon, &alat, &alon, &ier );
                sprintf (wlabel, "%s\n%02d%02d-%02d%02d",  wnum[ii], 
                         vtime[3], vtime[4], etime[3], etime[4]);

                cst_lstr ( wlabel, &txtlen, &ier );

                el_lin.hdr.delete   = 0;
                el_lin.hdr.vg_type  = SPTX_ELM;
                el_lin.hdr.vg_class = CLASS_TEXT;
                el_lin.hdr.maj_col  = icol;
                el_lin.hdr.min_col  = icol;
                el_lin.hdr.smooth   = 0;
                el_lin.hdr.version  = 0;
                el_lin.hdr.filled   = 0;
                el_lin.hdr.grptyp   = 0;
                el_lin.hdr.grpnum   = 0;
                el_lin.hdr.range_min_lat = alat;
                el_lin.hdr.range_min_lon = alon;
                el_lin.hdr.range_max_lat = alat;
                el_lin.hdr.range_max_lon = alon;

               /*
                * Fill in front TEXT element information.
                */

                el_lin.elem.spt.info.sptxtyp  = 0;
                el_lin.elem.spt.info.turbsym  = 0;
                el_lin.elem.spt.info.sztext   = 1.0F;
                el_lin.elem.spt.info.itxfn    = 21;
                el_lin.elem.spt.info.iwidth   = 1;
                el_lin.elem.spt.info.rotn     = 0.0F;
                el_lin.elem.spt.info.ialign   = -1;
                el_lin.elem.spt.info.offset_x = 0;
                el_lin.elem.spt.info.offset_y = -3;
                el_lin.elem.spt.info.ithw     = 2;
                el_lin.elem.spt.info.txtcol   = icol;
                el_lin.elem.spt.info.filcol   = icol;
                el_lin.elem.spt.info.lincol   = icol;
                el_lin.elem.spt.info.lat      = alat;
                el_lin.elem.spt.info.lon      = alon;
                strcpy ( el_lin.elem.spt.text, wlabel );

                el_lin.hdr.recsz = sizeof( VG_HdrStruct ) +
                            sizeof( SpTextInfo ) + txtlen + 1;
                    /*
                     * Write front TEXT element information.
                     */

                cvg_writefD( &el_lin, -1, el_lin.hdr.recsz, vgfile, &loc,
                            &ier );
            }

        }

       /*
        *  Free lat/lon arrays.
        */

        for (jj = 0; jj < LAT_LON_MX; jj++ ) {
	    free ( arylst[jj] );
        }
        for (jj = 0; jj < 2; jj++ ) {
	    free ( arypts[jj] );
        }
        free ( arylst );
        free ( arypts );

    }

   /*
    *  Close output file.
    */

    cfl_clos ( ifpwcp, &ier );
}

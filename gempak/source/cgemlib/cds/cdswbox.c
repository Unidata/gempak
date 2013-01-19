#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

#define MXPTS 1000
#define BNDS_FILE	"WBCMZ_BNDS"

void cds_wbox ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_wbox								*
 *									*
 * This function displays a watch box to the output device.		*
 *									*
 * cds_wbox ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 * 	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 3/97	Copied from cvg_frdsp			*
 * D. Keiser/GSC	 5/97	Renamed from cvg_lndsp			*
 * D.W.Plummer/NCEP	 7/97	Added cdsColor				*
 * E. Wehner/EAi	 7/97	Handle closed polygons			*
 * S. Jacobs/NCEP	 7/97	Handle filled polygons			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * D.W.Plummer/NCEP	10/97	Bug fix for numpts			*
 * I. Durham/GSC	 5/98	Changed undercsore decl. to an include	*
 * D.W.Plummer/NCEP	 6/98	Added parallelogram watch technique	*
 * D.W.Plummer/NCEP	11/98	Changes for extra pts on side of pgram	*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL to char for LINUX	*
 * D.W.Plummer/NCEP	 1/99	Remove include to clocmn.h		*
 * D.W.Plummer/NCEP	 2/99	Revise how counties are determined	*
 * A. Hardy/GSC		 3/99   Added priority parameter to SF_SNXT     *
 * D.W.Plummer/NCEP	 3/99	Changed length of stid string		*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * D.W.Plummer/NCEP	12/99	Removed cnty plot from GEMPAK sfc file	*
 * D.W.Plummer/NCEP	10/00	Added plot of anchor point location	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * H. Zeng/XTRIA        01/03   added marker attrib. for WatchBox       *
 * H. Zeng/SAIC		10/04	added color fill for counties		*
 * H. Zeng/SAIC		11/04	made county fill color overidable	*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 * H. Zeng/SAIC		12/05	obtained filpat val from prefs.tbl	*
 ***********************************************************************/
{
int		i, ii, np, npx, ier; 
int		filpat, filcol, minpts, lincol, lintyp, linwid;
int		mrkcol, mrktyp, mrkwid, maj_col, min_col;
int		width, zero, one, two, imark;
float		fone;
WatchBoxType	*pwbx;
float		lat[MXPTS], lon[MXPTS];
int		ncnty;
float		smlat, wmlon;
float		tlat[11], tlon[11];
float		filsiz, filter, mrksiz;
char		cchar[8], bndtyp[32], symtyp[5], tag_val[20];
int		ixoff, iyoff;
int		itxfnx, itxhwx, itxwidx, ibrdrx, irrotnx, ijustx, icolrx;
int		iltypx, ilthwx, iwidthx, iwhwx, imarkx, imkhwx, imkwidx;
int		itxfn, itxhw, itxwid, ibrdr, irrotn, ijust;
float		rotat, sztextx, sztext;
float		szmarkx;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Save plot and text attributes.
     */

    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );

    pwbx = &(el->elem.wbx);


/*
 *  Set the color for the feature for markers and outlines of the watch.
 */
    if ( cdsColor == 0 )  
	if ( cdsUattr[indx].maj_col == 0 )
	    gscolr( &(el->hdr.maj_col), &ier);
	else
	    gscolr ( &cdsUattr[indx].maj_col, &ier);
    else
	gscolr( &cdsColor, &ier);


    /*
     *  Plot (markers)|(color fill) first.
     *  Check cn_flag to see whether to plot.
     *  Check filled to see whether to use color fill or marker.
     */

    if ( pwbx->info.cn_flag > 0 && el->hdr.filled <= 0 ) {

        /*
         * Use marker.
         */
        imark = cdsUattr[indx].info.wbx->w_mrktyp == 0 ? 
	        pwbx->info.w_mrktyp : cdsUattr[indx].info.wbx->w_mrktyp;

        zero  = 0;
        one   = cdsUattr[indx].info.wbx->w_mrkwid == 0 ? 
	        pwbx->info.w_mrkwid : cdsUattr[indx].info.wbx->w_mrkwid;

        fone  = G_DIFF(cdsUattr[indx].info.wbx->w_mrksiz, 0.0F) ? 
	        pwbx->info.w_mrksiz : cdsUattr[indx].info.wbx->w_mrksiz;

        gsmrkr( &imark, &one, &fone, &one, &ier );

	ncnty = 0;
	for ( ii = 0; ii < pwbx->info.numcnty; ii++ )  {
	    lat[ncnty] = pwbx->info.cn_ltln[ii];
	    lon[ncnty] = pwbx->info.cn_ltln[pwbx->info.numcnty+ii];
	    ncnty++;
	}
        gmark( sys_M, &ncnty, lat, lon, &ier, strlen(sys_M) );

    }
    else if ( pwbx->info.cn_flag > 0 && el->hdr.filled > 0 ) {

        /*
         * Use color fill.
         */
        if ( cdsColor == 0 ) {

	   maj_col = cdsUattr[indx].maj_col == 0 ?
	             el->hdr.maj_col : cdsUattr[indx].maj_col;     
	   min_col = cdsUattr[indx].min_col == 0 ?
	             el->hdr.min_col : cdsUattr[indx].min_col;
        } 
	else {

	   maj_col = min_col = cdsColor;
        }

        /*
         * Query the value of filpat.
         */
        ctb_pfstr ( "COUNTY_FILL_PATTERN", tag_val, &ier );
        if ( ier == 0 ) {
          filpat = -1;
          sscanf ( tag_val, "%d", &filpat );
          if ( filpat<1 || filpat>7 ) filpat = 7;       
        }
        else {
          filpat = 7;
        }

        filcol = (cdsFill == 1) ? min_col : 0 ;
        filsiz = 1.0;
        filter = 0.0;
        minpts = 0;
        lincol = maj_col;
        lintyp = 1;
        linwid = 1;
        strcat (symtyp, "\0");
        mrkcol = 0;
        mrktyp = 0;
        mrksiz = 0.0;
        mrkwid = 0;

	for ( ii = 0; ii < pwbx->info.numcnty; ii++ )  {

	    strcpy  ( bndtyp, BNDS_FILE );
	    sprintf ( &(bndtyp[strlen(bndtyp)]), "|<FIPS>%-7d", 
			      pwbx->info.cn_fips[ii] );  

	    gplbnd ( bndtyp, &filcol, &filsiz, &filpat, &filter,
	             &minpts, &lincol, &lintyp, &linwid,
	             symtyp, &mrkcol, &mrktyp, &mrksiz, &mrkwid, &ier,
		     strlen(bndtyp), strlen(symtyp) );
	}

    }


    /*
     * Plot the outlines of watch and anchors after the
     * possible county color fill.
     */

/*
 *  Set wbox attributes.
 */
    lintyp = 1;
    width  = 2;
    zero   = 0;
    gsline(&(lintyp), &(zero), &(width), &(zero), &ier);

    np = pwbx->info.numpts;

/*
 *  Add an extra point equal to the first to close the polygon.
 */

    for ( i = np*2-1; i >= np; i-- )  {
                pwbx->latlon[i+1] = pwbx->latlon[i];
        }
    pwbx->latlon[np] = pwbx->latlon[0];
    pwbx->latlon[2*np+1] = pwbx->latlon[np+1];
    np = pwbx->info.numpts + 1;
    pwbx->info.numpts++;

/*
 *  Display the wbox with application layer command.
 */
    gline(sys_M, &np, pwbx->latlon, &(pwbx->latlon[np]),
                &ier, strlen(sys_M));

/*
 *  Add the central axis if watch box is parallelogram.
 */
    if ( pwbx->info.w_style == PGRAM )  {
	tlat[0] = pwbx->latlon[0];
	tlon[0] = pwbx->latlon[np];
	tlat[2] = pwbx->latlon[4];
	tlon[2] = pwbx->latlon[np+4];
	tlat[1] = ( pwbx->latlon[2] + pwbx->latlon[6] ) / 2.0F;
	tlon[1] = ( pwbx->latlon[np+2] + pwbx->latlon[np+6] ) / 2.0F;
	npx = 3;
        gline(sys_M, &npx, tlat, tlon, &ier, strlen(sys_M));
    }

    /*
     *  If el->elem.wbx.info.w_file is NULL, 
     *  get counties inside the watch box, set cnty hotlist and
     *  plot with watch box color.
     *  Otherwise, open w_file and get counties from there and 
     *  set cnty hotlist.  Then, in either case, plot hotlist.
     */

    /*
     *  If watch has a number, plot it below the southern-most and
     *  western-most point, below the box.
     */
    if ( el->elem.wbx.info.w_number > 0 )  {

        sprintf( cchar, "%d", el->elem.wbx.info.w_number );

        /*
         *  Figure southern-most, western-most point.
         */
        np = pwbx->info.numpts;
        smlat = 90.0F;
        wmlon = 180.0F;
        for ( i = 0 ; i < np ; i++ )  {
    	    if ( pwbx->latlon[i] < smlat )  {
		smlat = pwbx->latlon[i];
		wmlon = pwbx->latlon[i+np];
	    }
        }
        for ( i = 0 ; i < np ; i++ )  {
    	    if ( pwbx->latlon[i] <= smlat+.25F && pwbx->latlon[i+np] < wmlon )  {
		smlat = pwbx->latlon[i];
		wmlon = pwbx->latlon[i+np];
	    }
        }

        /*
         *  Set some nice text attributes.
         */
        itxfn  = 1;
        itxhw  = 1;
        sztext = 1.5F;
        itxwid = 1;
        ibrdr  = 110;
        irrotn = 1;
        ijust  = 1;
        gstext( &itxfn, &itxhw, &sztext, &itxwid, &ibrdr, &irrotn, 
	        &ijust, &ier );
        /*
         *  Set some variables
         */
        rotat = 0.0F;
        ixoff = 0;
        iyoff = -2;

        /*
         *  Plot the text
         */
        gtext( sys_M, &smlat, &wmlon, cchar, &rotat, 
	        &ixoff, &iyoff, &ier,
	        strlen(sys_M), strlen(cchar) );

    }


    /*
     *  Plot markers at anchor locations
     */

    imark = 6;
    zero  = 0;
    one   = 1;
    fone  = 1.0F;
    two   = 2;
    gsmrkr( &imark, &one, &fone, &two, &ier );

    gmark( sys_M, &one, &pwbx->info.w_a0lt, &pwbx->info.w_a0ln, 
		&ier, strlen(sys_M) );
    gmark( sys_M, &one, &pwbx->info.w_a1lt, &pwbx->info.w_a1ln, 
		&ier, strlen(sys_M) );


    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );


}

#include "geminc.h"
#include "gemprm.h"

void  wbc_vors ( char *locnam, int type, int *vpdst1, char *vpdir1, 
		char *vpstn1, int *vpdst2, char *vpdir2, char *vpstn2, 
		float *wclat1, float *wclon1, float *wclat2, float *wclon2, 
		float *wclat3, float *wclon3, float *wclat4, float *wclon4,
		int len, char *vorstr, float *vorlat1, float *vorlon1, 
		float *vorlat2, float *vorlon2, int *iret )
/************************************************************************
 * wbc_vors                                                    		*
 *                                                                      *
 * This function finds the latitude and longitude of the VOR points     *
 * from the given verification file.  The text string defining the      *
 * watch area is created from the two VOR points and the 4 corner       *
 * points. Also, the latitude and longitude of the two VOR points is    *
 * returned.								*
 *                                                                      *
 * wbc_vors ( locnam, type, vpdst1, vpdir1, vpstn1, vpdst2, vpdir2,     *
 *            vpstn2, wclat1, wclon1, wclat2, wclon2, wclat3, wclon3,   *
 *            wclat4, wclon4, len, vorstr, vorlat1, vorlon1, vorlat2, 	*
 *            vorlon2, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Locator type				*
 *	type		int	Type of SPC product			*
 *				  1 - PWN				*
 *				  2 - WCP				*
 *				  3 - AWN				*
 * 	*vpdst1 	int	VOR endpoint 1 distance			*
 *	*vpdir1 	char    VOR endpoint 1 direction		*
 *	*vpstn1 	char	VOR endpoint 1 station id		*
 *	*vpdst2         int	VOR endpoint 2 distance			*
 *	*vpdir2 	char    VOR endpoint 2 direction		*
 *	*vpstn2 	char	VOR endpoint 2 station id		*
 *	*wclat1 	float   Watch corner point 1 - lat.		*
 *	*wclon1 	float   Watch corner point 1 - lon.		*
 *	*wclat2 	float   Watch corner point 2 - lat.		*
 *	*wclon2 	float   Watch corner point 2 - lon.		*
 *	*wclat3 	float   Watch corner point 3 - lat.		*
 *	*wclon3 	float   Watch corner point 3 - lon.		*
 *	*wclat4 	float   Watch corner point 4 - lat.		*
 *	*wclon4 	float   Watch corner point 4 - lon.		*
 *	len		int     Max length of 'vorstr'			*
 *									*
 * Output parameters:                                                   *
 *	*vorstr		char	Polygon text string			*
 *	*vorlat1	float   Latitude of first VOR point		*
 *	*vorlon1	float   Longitude of first VOR point		*
 *	*vorlat2	float   Latitude of second VOR point		*
 *	*vorlon2	float   Longitude of second VOR point		*
 *	*iret		int	Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03						*
 ************************************************************************/
{
	int	ii, nstn, maxlen, np, ier, itype, leng, len1;
	float	lat1, lon1, lat2, lon2, vdist1, fdir1, fdir2;
	float   flat1, flon1, flat2, flon2, vdist2;
	char    **aryvor, tmpstr[120], holdstr[120], qstate[1], pstn[180]; 
	char    disdir[20], stn[6], disbeg[20], stnbeg[6], disdirv1[20];
	char    stnv1[6], disdirv2[20], stnv2[6], vorstn1[4], vorstn2[4];
	char	chlat[7],chlon[8];
/*---------------------------------------------------------------------*/

    *iret = 0;
    np = 1;
    qstate[0] = '\0';
    maxlen = sizeof(pstn);
    leng = 3;

   /*
    *      Initialize the clo library values.
    */

    clo_init ( &ier );

    cst_ncpy ( vorstn1, vpstn1, leng, &ier );
    cst_ncpy ( vorstn2, vpstn2, leng, &ier );

    aryvor = (char **) malloc(sizeof(char *) * 5);
    for ( ii = 0; ii < 5; ii++ ) {
        aryvor[ii] = (char *) malloc(40) ;
    }

   /*
    *  Get the lat./lon. of the first for point.
    */

    clo_findstn ( locnam, vorstn1, qstate, np, maxlen,
                  &nstn, pstn, &ier);

    cst_gtag ( "LAT", pstn, " ", chlat, &ier );
    cst_gtag ( "LON", pstn, " ", chlon, &ier );
    cst_crnm ( chlat, &lat1, &ier );
    cst_crnm ( chlon, &lon1, &ier );

    vdist1 = (float)*vpdst1 * NM2M;
    cst_alnm (vpdir1[0], &itype, &ier);
    if ( itype == 1) {
        clo_cmpdir ( vpdir1, &fdir1, &ier);
    }
    else {
	fdir1 = 0.0F;
    }
    clo_dltln ( &lat1, &lon1, &vdist1, &fdir1, &flat1, &flon1, &ier);

   /*
    *  Get the lat./lon. of the second for point.
    */

    clo_findstn ( locnam, vorstn2, qstate, 1, maxlen,
	               &nstn, pstn, &ier);

    cst_gtag ( "LAT", pstn, " ", chlat, &ier );
    cst_gtag ( "LON", pstn, " ", chlon, &ier );
    cst_crnm ( chlat, &lat2, &ier );
    cst_crnm ( chlon, &lon2, &ier );

    vdist2 = (float)*vpdst2 * NM2M;
    clo_cmpdir ( vpdir2, &fdir2, &ier);
    clo_dltln ( &lat2, &lon2, &vdist2, &fdir2, &flat2, &flon2, &ier);


    for ( ii = 0; ii < 5; ii++ ) {
        free ( aryvor[ii] );
    }
    free ( aryvor);


   /*
    *  Fill the distance and direction strings. 
    */

    *vorlat1 = flat1;
    *vorlon1 = flon1;
    *vorlat2 = flat2;
    *vorlon2 = flon2;

    if (  type == 3 ) {
	if ( vpdst1 != 0 ) {
            sprintf( disdirv1, "..%d%s", *vpdst1, vpdir1);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv1, ".." );
	}
            cst_ncpy(stnv1, vpstn1, 4, &ier); 

	if ( vpdst2 != 0 ) {
            sprintf( disdirv2, "..%d%s", *vpdst2, vpdir2);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv2, ".." );
	}
            cst_ncpy(stnv2, vpstn2, 4, &ier); 

    }
    else {
	if ( *vpdst1 != 0 ) {
            sprintf( disdirv1, "%d %s",  *vpdst1, vpdir1);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv1, ".." );
	}
        sprintf(stnv1, "%s;",vpstn1); 

	if ( *vpdst2 != 0 ) {
            sprintf( disdirv2, "%d %s",  *vpdst2, vpdir2);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv2, ".." );
	}
        sprintf(stnv2, "%s;",vpstn2); 
    }

   /*
    * Create the Aviation coordinate string.
    */

    tmpstr[0] = '\0';
    holdstr[0] = '\0';

    utl_avcd ( locnam, wclat1, wclon1, disbeg, stnbeg, &ier);
    if ( type == 3 ) {
	cst_rmbl (disbeg, disbeg, &len1, &ier );
        sprintf ( holdstr, "..%s %s", disbeg, stnbeg);
    }
    else {
       sprintf ( holdstr, "%s %s;", disbeg, stnbeg);
    }
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    utl_avcd ( locnam, wclat2, wclon2, disdir, stn, &ier);
    if ( type == 3 ) {
	cst_rmbl (disdir, disdir, &len1, &ier );
        sprintf( holdstr, "..%s %s", disdir, stn);
    }
    else {
        sprintf( holdstr, "%s %s;", disdir, stn);
    }
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    sprintf( holdstr, "%s %s", disdirv2, stnv2);
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    utl_avcd ( locnam, wclat3, wclon3, disdir, stn, &ier);
    if ( type == 3 ) {
        cst_rmbl (disdir, disdir, &len1, &ier );
        sprintf( holdstr, "..%s %s", disdir, stn);
    }
    else {
        sprintf( holdstr, "%s %s;", disdir, stn);
    }
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    utl_avcd ( locnam, wclat4, wclon4, disdir, stn, &ier);
    if ( type == 3 ) {
	cst_rmbl (disdir, disdir, &len1, &ier );
        sprintf( holdstr, "..%s %s", disdir, stn);
    }
    else {
        sprintf( holdstr, "%s %s;", disdir, stn);
    }
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    sprintf( holdstr, "%s %s", disdirv1, stnv1);
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    holdstr[0] = '\0';
    if ( type == 3 ) {
	cst_rmbl (disbeg, disbeg, &len1, &ier );
        sprintf( holdstr, "..%s %s\n", disbeg, stnbeg);
    }
    else {
       sprintf( holdstr, "%s %s;\n", disbeg, stnbeg);
    }
    cst_ncat ( tmpstr, holdstr, &len, &ier);

    len1 = G_MIN ( len, (int)strlen(tmpstr));
    cst_ncpy ( vorstr, tmpstr, len1, &ier);
    *iret = ier;
}

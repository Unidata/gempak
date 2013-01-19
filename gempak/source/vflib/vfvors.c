#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern 	SpcInfo_t      spcinfo;

void vfvors ( char *locnam, char *type, char *vorstr, 
	      float *vorlat1, float *vorlon1, float *vorlat2, 
	      float *vorlon2, int *iret )
/************************************************************************
 * vfvors                                                    		*
 *                                                                      *
 * This function finds the latitude and longitude of the VOR points     *
 * from the given verification file.  The text string defining the      *
 * watch area is created from the two VOR points and the 4 corner       *
 * points. Also, the latitude and longitude of the two VOR points is    *
 * returned.								*
 *                                                                      *
 * void vfvors ( locnam, type, vorstr, vorlat1, vorlon1, vorlat2,       *
 *		 vorlon2, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Locator type				*
 *	*type		char	Type of SPC product			*
 *									*
 * Output parameters:                                                   *
 *	*vorstr		char	Polygone text string			*
 *	*vorlat1	float   Latitude of first VOR point		*
 *	*vorlon1	float   Longitude of first VOR point		*
 *	*vorlat2	float   Latitude of second VOR point		*
 *	*vorlon2	float   Longitude of second VOR point		*
 *	*iret		int	Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC         03/00   Created					*
 * A. Hardy/GSC         05/00   Added WCP if test                       *
 * A. Hardy/GSC         10/00   Added ck for 0 dist and dir.		*
 * A. Hardy/GSC         12/00   Removed '&' from clo_findstn's np       * 
 * F. J. Yen/NCEP	 8/01	Modified calling sequence for clo_cmpdir*
 * R. Tian/SAIC		06/02	Removed '\r'				*
 * R. Tian/SAIC		07/03	Changed to call cst_gtag		*
 ************************************************************************/
{
	int	nstn, maxlen, np, ier, itype;
	float	lat1, lon1, lat2, lon2, vdist1, fdir1, fdir2;
	float   flat1, flon1, flat2, flon2, vdist2;
	char    tmpstr[120], holdstr[120], qstate[1], pstn[128]; 
	char    disdir[20], stn[6], disbeg[20], stnbeg[6], disdirv1[20];
	char    stnv1[6], disdirv2[20], stnv2[6],vorstn1[4], vorstn2[4];
/*---------------------------------------------------------------------*/

    *iret = 0;
    np = 1;
    qstate[0] = '\0';
    maxlen = sizeof(pstn);

   /*
    *      Initialize the clo library values.
    */
    clo_init ( &ier );

    strcpy ( vorstn1, spcinfo.vorrpt.stn1 );
    strcpy ( vorstn2, spcinfo.vorrpt.stn2 );

   /*
    *  Get the lat./lon. of the first for point.
    */

    clo_findstn ( locnam, vorstn1, qstate, np, maxlen,
                  &nstn, pstn, &ier);
    cst_gtag ( "LAT", pstn, "99999", tmpstr, &ier );
    cst_crnm ( tmpstr, &lat1, &ier );
    cst_gtag ( "LON", pstn, "99999", tmpstr, &ier );
    cst_crnm ( tmpstr, &lon1, &ier );

    vdist1 = (float)spcinfo.vorrpt.dist1 * NM2M;
    cst_alnm (spcinfo.vorrpt.dirct1[0], &itype, &ier);
    if ( itype == 1) {
        clo_cmpdir ( spcinfo.vorrpt.dirct1, &fdir1, &ier);
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
    cst_gtag ( "LAT", pstn, "99999", tmpstr, &ier );
    cst_crnm ( tmpstr, &lat2, &ier );
    cst_gtag ( "LON", pstn, "99999", tmpstr, &ier );
    cst_crnm ( tmpstr, &lon2, &ier );

    vdist2 = (float)spcinfo.vorrpt.dist2 * NM2M;
    clo_cmpdir ( spcinfo.vorrpt.dirct2, &fdir2, &ier);
    clo_dltln ( &lat2, &lon2, &vdist2, &fdir2, &flat2, &flon2, &ier);

   /*
    *  Fill the distance and direction strings. 
    */

    *vorlat1 = flat1;
    *vorlon1 = flon1;
    *vorlat2 = flat2;
    *vorlon2 = flon2;
  
    if ( strcmp ( type, "AWN") == 0 ) {
	if ( spcinfo.vorrpt.dist1 != 0 ) {
            sprintf( disdirv1, "..%d%s", spcinfo.vorrpt.dist1, 
	             spcinfo.vorrpt.dirct1);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv1, ".." );
	}
            strcpy(stnv1, spcinfo.vorrpt.stn1); 

	if ( spcinfo.vorrpt.dist2 != 0 ) {
            sprintf( disdirv2, "..%d%s", spcinfo.vorrpt.dist2, 
	             spcinfo.vorrpt.dirct2);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv2, ".." );
	}
            strcpy(stnv2, spcinfo.vorrpt.stn2); 

    }
    else if ( (strcmp ( type, "PWN") == 0 ) || 
             ( strcmp ( type, "WCP") == 0 ) ){
	if ( spcinfo.vorrpt.dist1 != 0 ) {
            sprintf( disdirv1, "%d %s", spcinfo.vorrpt.dist1, 
	             spcinfo.vorrpt.dirct1);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv1, ".." );
	}
        sprintf(stnv1, "%s;",spcinfo.vorrpt.stn1); 

	if ( spcinfo.vorrpt.dist2 != 0 ) {
            sprintf( disdirv2, "%d %s", spcinfo.vorrpt.dist2, 
	             spcinfo.vorrpt.dirct2);
	}
	else {
	    /*
	     * Print string for zero distance and direction.
	     */
            sprintf( disdirv2, ".." );
	}
        sprintf(stnv2, "%s;",spcinfo.vorrpt.stn2); 
    }

   /*
    * Create the Aviation coordinate string.
    */

    tmpstr[0] = '\0';
    holdstr[0] = '\0';
    vfavcd ( locnam, type, spcinfo.wcpnt1.lat, spcinfo.wcpnt1.lon, 
             disbeg, stnbeg, &ier);
    sprintf ( holdstr, "%s %s", disbeg, stnbeg);
    strcat ( tmpstr, holdstr);
    holdstr[0] = '\0';
    vfavcd ( locnam, type, spcinfo.wcpnt2.lat, spcinfo.wcpnt2.lon, 
             disdir, stn, &ier);
    sprintf( holdstr, "%s %s", disdir, stn);
    strcat ( tmpstr, holdstr);

    holdstr[0] = '\0';
    sprintf( holdstr, "%s %s", disdirv2, stnv2);
    strcat ( tmpstr, holdstr);

    holdstr[0] = '\0';
    vfavcd ( locnam, type, spcinfo.wcpnt3.lat, spcinfo.wcpnt3.lon, 
             disdir, stn, &ier);
    sprintf( holdstr, "%s %s", disdir, stn);
    strcat ( tmpstr, holdstr);

    holdstr[0] = '\0';
    vfavcd ( locnam, type, spcinfo.wcpnt4.lat, spcinfo.wcpnt4.lon, 
             disdir, stn, &ier);
    sprintf( holdstr, "%s %s", disdir, stn);
    strcat ( tmpstr, holdstr);

    holdstr[0] = '\0';
    sprintf( holdstr, "%s %s", disdirv1, stnv1);
    strcat ( tmpstr, holdstr);

    holdstr[0] = '\0';
    sprintf( holdstr, "%s %s\n", disbeg, stnbeg);
    strcat ( tmpstr, holdstr);

    strcpy ( vorstr, tmpstr);
}

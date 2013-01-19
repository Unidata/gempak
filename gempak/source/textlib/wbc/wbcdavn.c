#include "geminc.h"
#include "gemprm.h"

void wbc_davn ( char *wtype, float *hailsz, int *maxgust, int *maxtops,
                int *degree, int *speed, int len1, char *avnstr, int *iret )
/************************************************************************
 * wbc_davn								*
 *                                                                      *
 * This function creates the aviation text string for tornado and	*
 * severe thunderstorm watches.	This paragraph is found in the SPC's	*
 * PWN and SEL text products.						*
 *                                                                      *
 * wbc_davn ( wtype, hailsz, maxgust, maxtops, degree, speed, len1,	*
 *	      avnstr, iret )    					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*wtype		char		Watch type			*
 *	*hailsz		float		Hail size			*
 *	*maxgust	int		Maximum gusts			*
 *	*maxtops	int		Maximum cloud tops		*
 *	*degree		int		Direction of storm motion	*
 *	*speed		int		Speed of storm			*
 *	len1		int		Max length of 'avnstr'		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*avnstr		char		Aviation string			*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 ***********************************************************************/
{
    int	    ier, len;
    char    tmpstr[500], hold[200], hailstr[4];
/*-------------------------------------------------------------------*/
     *iret = 0;

    /*
     * Change hail size to string.
     */

     if (((int)(*hailsz * 10.0F) %2 ) == 0 ) {
         sprintf ( hailstr, "%1.*f", 0, *hailsz );
     }
     else {
         sprintf ( hailstr, "%3.*f", 1, *hailsz );
     }
    tmpstr[0] = '\0'; 
    hold[0] = '\0'; 
    if ( strcmp(wtype,"SEVERE THUNDERSTORM" ) == 0 ) {
        sprintf ( tmpstr, "AVIATION...A FEW SEVERE THUNDERSTORMS WITH");

        if ( (*hailsz > 0.0F) && (*hailsz <= 1.0F ) ) {
            sprintf ( hold, " HAIL SURFACE AND ALOFT TO %s INCH.",
	              hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
	}
        if ( *hailsz > 1.0F ) {
            sprintf ( hold, " HAIL SURFACE AND ALOFT TO %s INCHES.",
	              hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
	}

        sprintf( hold, " EXTREME TURBULENCE AND SURFACE WIND GUSTS TO");
	cst_ncat ( tmpstr, hold, &len, &ier );
        sprintf ( hold, " %d KNOTS. A FEW CUMULONIMBI WITH MAXIMUM TOPS",
                      *maxgust);
	cst_ncat ( tmpstr, hold, &len, &ier );
        sprintf ( hold, " TO %d. MEAN STORM MOTION VECTOR %03d%d.",
                      *maxtops, *degree, *speed );
	cst_ncat ( tmpstr, hold, &len, &ier );

    }

    else if ( strcmp(wtype,"TORNADO" ) == 0 ) {

        sprintf ( tmpstr, "AVIATION...TORNADOES AND A FEW SEVERE THUNDERSTORMS WITH");

	if ( (*hailsz > 0.0F ) && ( *hailsz <= 1.0F ) ){
            sprintf ( hold, " HAIL SURFACE AND ALOFT TO %s INCH.", hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
	}
        if ( *hailsz > 1.0F ) {
            sprintf ( hold, " HAIL SURFACE AND ALOFT TO %s INCHES.",
	              hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
	}

        sprintf  ( hold, " EXTREME TURBULENCE AND SURFACE WIND GUSTS TO");
	cst_ncat ( tmpstr, hold, &len, &ier );
        sprintf ( hold, " %d KNOTS. A FEW CUMULONIMBI WITH MAXIMUM", *maxgust);
	cst_ncat ( tmpstr, hold, &len, &ier );
        sprintf ( hold, " TOPS TO %d. MEAN STORM MOTION VECTOR %03d%d.",
                      *maxtops, *degree, *speed );
	cst_ncat ( tmpstr, hold, &len, &ier );
    }
    len = G_MIN (len1, (int)strlen(tmpstr) );
    cst_ncpy ( avnstr, tmpstr, len, &ier );
}

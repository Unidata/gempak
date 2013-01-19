#include "geminc.h"
#include "gemprm.h"

void wbc_dhwm ( char *sep, float *hailsz, int *maxgust, int *maxtops,
                int *degree, int *speed, int len1, char *hwmstr, 
		int *iret )
/************************************************************************
 * wbc_dhwm								*
 *                                                                      *
 * This function creates the hail, gusts, cloud tops and storm motion 	*
 * text string.  This string is used by SPC's AWN and SAW text products.*
 *                                                                      *
 * Output example:							*
 *                                                                      *
 *	HAIL SURFACE AND ALOFT..2 INCHES. WIND GUSTS..60 KNOTS.		*
 *	MAX TOPS..400. MEAN STORM MOTION VECTOR 25035.			*
 *                                                                      *
 * wbc_dhwm ( sep, hailsz, maxgust, maxtops, degree, speed, len1,       *
 *            hwmstr, iret )    					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		Separator between top and value *
 *	*hailsz		float		Hail size			*
 *	*maxgust	int		Maximum gusts			*
 *	*maxtops	int		Maximum cloud tops		*
 *	*degree		int		Direction of storm motion	*
 *	*speed		int		Speed of storm			*
 *	len1		int		Max length of 'hwmstr'		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*hwmstr		char		Hail, gusts, cloud tops, motion	*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 ***********************************************************************/
{
    int	    ier, len;
    char    tmpstr[500], hold[200], space[5], hailstr[4];
/*-------------------------------------------------------------------*/
     *iret = 0;

    tmpstr[0] = '\0'; 
    hold[0] = '\0'; 


    if (strcmp ( sep, "..") != 0 ) {
	sprintf (space," %s ",sep);
    }
    else {
	sprintf (space,"%s",sep);
    }

   /* 
    * Change hail size to string.
    */

    if ( ( (int) (*hailsz * 10.0F) % 2) == 0 ) {
        sprintf ( hailstr, "%1.*f", 0, *hailsz);
    }
    else {
        sprintf ( hailstr, "%3.*f", 1, *hailsz);
    }

    if (  !G_DIFF(*hailsz, 0.0F) ) {
	if ( ( *hailsz > 0.0F ) && ( *hailsz <= 1.0F ) ) {
            sprintf ( hold, "HAIL SURFACE AND ALOFT..%s INCH.",
	              hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
	}
        if ( *hailsz > 1.0F ) {
            sprintf ( hold, "HAIL SURFACE AND ALOFT..%s INCHES.",
	              hailstr);
	    cst_ncat ( tmpstr, hold, &len, &ier );
        }

        sprintf( hold, " WIND GUSTS..%d KNOTS.", *maxgust);
	cst_ncat ( tmpstr, hold, &len, &ier );
        sprintf( hold, "\nMAX TOPS%s%d. MEAN STORM MOTION VECTOR %03d%d.\n\n",
                  space, *maxtops, *degree, *speed );
	cst_ncat ( tmpstr, hold, &len, &ier );
    }
    else {
        sprintf( hold, "WIND GUSTS..%d KNOTS. \nMAX TOPS%s%d.", 
	           *maxgust, space, *maxtops);
	cst_ncat ( tmpstr, hold, &len, &ier );

        sprintf( hold, "MEAN STORM MOTION VECTOR %03d%d.", 
	        *degree, *speed );
	cst_ncat ( tmpstr, hold, &len, &ier );
    }

    len = G_MIN (len1, (int)strlen(tmpstr) );
    cst_ncpy ( hwmstr, tmpstr, len, &ier );
}

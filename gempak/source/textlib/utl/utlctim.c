#include "geminc.h"
#include "gemprm.h"

void utl_ctim ( int len, char *curtim, int *iret )
/************************************************************************
 * utl_ctim                                                             *
 *                                                                      *
 * This function retrieves the current system time and converts it to   *
 * a character string of the form DDHHMM.				*
 *                                                                      *
 * utl_ctim ( len, curtim, iret )  					*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *	len		int		Max length of 'curtim'		*
 * Output parameters:                                                   *
 *	*curtim		char		Current time as DDHHMM		*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	5/03		From VFCTIM  			*
 * B. Yin/SAIC          3/04   changed css_date calling sequences       *
 ***********************************************************************/
{
    char    dattim[7], zone[4];
    int     itype, iyr, imon, idy, ihr, imin, isc, julian, ier;
/*-------------------------------------------------------------------*/
    *iret = 0;
    itype = 1;
    curtim[0] = '\0';

   /*
    * Get current system time for UTC issue time of cancel product.
    */

    css_date ( &itype, &iyr, &imon, &idy, &ihr, &imin, &isc, &julian, 
               zone, iret);

    if ( (idy >= 1) && (idy <= 9 ) ) {
	cst_ncpy( curtim, "0", 1, &ier);
    }
    cst_inch ( idy, dattim, iret);
    cst_ncat (curtim, dattim, &len, iret);

    if ( (ihr >= 0) && (ihr <= 9 ) ) {
	cst_ncat( curtim, "0", &len, iret);
    }
    cst_inch ( ihr, dattim, iret );
    cst_ncat (curtim, dattim, &len, iret);

    if ( (imin >= 0) && (imin <= 9 ) ) {
	cst_ncat( curtim, "0", &len, iret);
    }
    cst_inch ( imin, dattim, iret );
    cst_ncat (curtim, dattim, &len, iret);
}

#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern	SpcInfo_t	newinfo;
extern	SpcInfo_t	spcinfo;

void vfctim ( int *iret )
/************************************************************************
 * vfctim                                                               *
 *                                                                      *
 * This function retrieves the current system time and converts it to   *
 * a character string.							*
 *                                                                      *
 * vfctim ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC         11/99   Created                                 *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		12/00   Removed '&' from iret			*
 * A. Hardy/NCEP	 6/03   Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{
    char    dattim[7], curtim[7], tmzn[4];
    int     itype, iyr, imon, idy, ihr, imin, isc, julian;
/*-------------------------------------------------------------------*/

    itype = 1;

   /*
    * Get current system time for UTC issue time of cancel product.
    */

    css_date ( &itype, &iyr, &imon, &idy, &ihr, &imin, &isc, &julian, 
               tmzn, iret);

    if ( (idy >= 1) && (idy <= 9 ) ) strcpy( curtim, "0");
    cst_inch ( idy, dattim, iret);
    strcpy ( curtim, dattim);
    if ( (ihr >= 0) && (ihr <= 9 ) ) strcat( curtim, "0");
    cst_inch ( ihr, dattim, iret );
    strcat (curtim, dattim);
    if ( (imin >= 0) && (imin <= 9 ) ) strcat( curtim, "0");
    cst_inch ( imin, dattim, iret );
    strcat (curtim, dattim);
    strcpy ( spcinfo.curtim, curtim);

}

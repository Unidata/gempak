#include "geminc.h"
#include "gemprm.h"

void utl_gdwk ( int daywk, char *pdwk, int *iret )
/************************************************************************
 * utl_gdwk								*
 *									*
 * This function retrieves the three character abbreviation for the day *
 * of the week.								*
 *									*
 * utl_gdwk ( daywk, pdwk, iret )					*
 *									*
 * Input parameters:							*
 *	daywk		int		Numerical day of the week       *
 *									*
 * Output parameters:							*
 *	*pdwk		char		3-character week day name	*
 *	*iret		int		Return code			*
 *					   -7 = Bad day of the week no. *
 **									*
 * Log:									*
 * A. Hardy/NCEP	 7/03						*
 * A. Hardy/NCEP	 8/03		Changed length check 4 -> 3	*
 ***********************************************************************/
{
        int	len1, lend, ier;
	char	dwname[7][4] = { "SUN", "MON", "TUE", "WED", "THU",
				 "FRI", "SAT" };
/*---------------------------------------------------------------------*/
	*iret   = 0;
	lend    = 4;
	pdwk[0] = '\0';
   /*
    *	Retrieve the 3 character day of the week.
    */
	if ( daywk >= 1 && daywk <= 7 ) {
	    len1 = G_MIN ( lend, 3 );
	    cst_ncpy ( pdwk, dwname[daywk-1], len1, &ier);
	}
	else {
	    *iret = -7;
	    return;
	}
}

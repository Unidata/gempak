#include "geminc.h"
#include "gemprm.h"

void utl_gmon ( int dtmonth, char *pmm, int *iret )
/************************************************************************
 * utl_gmon								*
 *									*
 * This function converts the numerical value of a month into the three	*
 * character abbreviation for the month.				*
 *									*
 * utl_gmon ( dtmonth, pmm, iret )					*
 *									*
 * Input parameters:							*
 *	dtmonth		int		Month				*
 *									*
 * Output parameters:							*
 *	*pmm		char		3-character month name		*
 *	*iret		int		Return code			*
 *					   -6 = Bad month number 	*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 7/03						*
 * A. Hardy/NCEP	 8/03		Changed length check 4 -> 3	*
 ***********************************************************************/
{
        int	len1, lenp, ier;
	char	noname[12][4] = { "JAN", "FEB", "MAR", "APR", "MAY",
				 "JUN", "JUL", "AUG", "SEP", "OCT",
				 "NOV", "DEC" };
/*---------------------------------------------------------------------*/
	*iret   = 0;
	lenp    = 4;
	pmm[0]  = '\0';
   /*
    *	Retrieve the 3 character month.
    */
	if ( dtmonth >= 1 && dtmonth <= 12 ) {
	    len1 = G_MIN ( lenp, 3 );
	    cst_ncpy ( pmm, noname[dtmonth-1], len1, &ier);
	}
	else {
	    *iret = -6;
	    return;
	}
}

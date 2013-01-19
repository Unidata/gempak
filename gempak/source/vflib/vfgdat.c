#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

void vfgdat ( int dtmonth, int daywk, char *pmm, char *pdwk, int *iret )
/************************************************************************
 * vfgdat								*
 *									*
 * This function retrieves the three character abbreviations for the    *
 * month and day of the week.						*
 *									*
 * vfgdat ( dtmonth, daywk, pmm, pdwk, iret )				*
 *									*
 * Input parameters:							*
 *	dtmonth		int		Month				*
 *	daywk		int		Numerical day of the week       *
 *									*
 * Output parameters:							*
 *	*pmm		char		3-character month name		*
 *	*pdwk		char		3-character week day name	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 ***********************************************************************/
{
	char	noname[12][4] = { "JAN", "FEB", "MAR", "APR", "MAY",
				 "JUN", "JUL", "AUG", "SEP", "OCT",
				 "NOV", "DEC" };
	char	dwname[7][4] = { "SUN", "MON", "TUE", "WED", "THU",
				 "FRI", "SAT" };
/*---------------------------------------------------------------------*/
	*iret = 0;
   /*
    *	Retrieve the 3 character month.
    */
	if ( dtmonth >= 1 && dtmonth <= 12 )
		strcpy ( pmm, noname[dtmonth-1]);
   /*
    *	Retrieve the 3 character day of the week.
    */
	strcpy ( pdwk, dwname[daywk-1]);
}

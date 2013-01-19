#include "geminc.h"
#include "gemprm.h"

void utl_gdat ( int dtmonth, int daywk, char *pmm, char *pdwk, int *iret )
/************************************************************************
 * utl_gdat								*
 *									*
 * This function retrieves the three character abbreviations for the    *
 * month and day of the week.						*
 *									*
 * utl_gdat ( dtmonth, daywk, pmm, pdwk, iret )				*
 *									*
 * Input parameters:							*
 *	dtmonth		int		Month				*
 *	daywk		int		Numerical day of the week       *
 *									*
 * Output parameters:							*
 *	*pmm		char		3-character month name		*
 *	*pdwk		char		3-character week day name	*
 *	*iret		int		Return code			*
 *                                         -1 - month out of range	*
 *                                         -2 - day out of range	*
 **									*
 * Log:									*
 * A. Hardy/GSC		 4/05  						* 
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
	if ( dtmonth >= 1 && dtmonth <= 12 ) {
		strcpy ( pmm, noname[dtmonth-1]);
        }
        else {
            *iret = -1;
	    strcpy ( pmm, " ");
            return;
        }
   /*
    *	Retrieve the 3 character day of the week.
    */
	if ( daywk >= 1 && daywk <= 7 ) {
	    strcpy ( pdwk, dwname[daywk-1]);
        }
        else {
            *iret = -2;
	    strcpy ( pdwk, " ");
            return;
        }
}

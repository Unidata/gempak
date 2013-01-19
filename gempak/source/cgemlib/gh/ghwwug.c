#include "geminc.h"
#include "gemprm.h"
#include "ghcmn.h"

void gh_wwug ( char *strin, char *dd, char *hr, char *strout, 
               int *ilenout, int *iret) 
/************************************************************************
 * gh_wwug                                                              *
 *                                                                      *
 * This subroutine  transforms a string containing UGC counties and/or 	*
 * marine zones	separated by semicolons into a string of sorted UGC 	*
 * counties and/or marine zones separated by '-' with an ending time	*
 * appended at the end of the string.  It returns the changed string 	*
 * and its length.							*
 *                                                                      *
 * gh_wwug ( strin, dd, hr, strout, ilenout, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*strin		char		UG codes string			*
 *	*dd		char		Date (DD)			*
 *	*hr		char		Time (HHMM)			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*strout		char		UG codes formatted string	*
 *	*ilenout	int		UG codes string	length		*
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/03                                           *
 * D. Kidwell/NCEP	 2/05	Increased num and maxnum, 50 -> 100     *
 ***********************************************************************/
{
     int     ii, ier, numstr, num, iret2;
     int     maxnum, maxch, len, ilen;
     char    **ugcarr, ugcstr[500]; 
/*-------------------------------------------------------------------*/
     *iret  = 0;
     ier    = 0;
     len    = 6;
     num    = 100;
     maxch  = 50; 
     maxnum = 100;

    /*
     * Allocate memory for array size.
     */

     ugcarr = (char **)malloc(num * sizeof(char *));
     for ( ii = 0; ii < num; ii++ ) {
         ugcarr[ii] = (char *)malloc((len+1) * sizeof(char *));

     }

    /*
     * Break apart the county/zones string into an array.
     */

     cst_lstr ( strin, &ilen, &ier );
     cst_rmbl ( strin, strin, &ilen, &ier);
     cst_clst ( strin, ';', " ", maxnum, maxch, ugcarr, &numstr, &ier);

    /*
     * Get list of counties and zones in a UGC formatted string.
     */

     len = 500;
     utl_ugcp ( ugcarr, &numstr, dd, hr, &len, ugcstr, &iret2 );
     cst_lstr ( ugcstr, &ilen, &ier );
     *ilenout = ilen;
     strcpy (strout, ugcstr);

    /*
     * Free memory.
     */

     for( ii = 0; ii < num; ii++ )
         free( ugcarr[ii] );

     if ( ugcarr )
         free( (char **) ugcarr );
}

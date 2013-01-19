#include "geminc.h"
#include "gemprm.h"
#include "ghcmn.h"

void gh_wwat ( char *strin, char *strout, int *ilenout, int *iret) 
/************************************************************************
 * gh_wwat                                                              *
 *                                                                      *
 * This subroutine  transforms a string of WFO's separated by a 	*
 * semicolon into a string of WFOs separated by "..." and returns the 	*
 * changed string and its length.			 		*
 *                                                                      *
 * gh_wwat ( strin, strout, ilenout, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *strin          char            WFOs string 	                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *strout         char            WFOs formatted string	        *
 *      *ilenout        char            WFOs formatted length           *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 10/03                                           *
 ***********************************************************************/
{
     int     ii, ier, numstr, num, iret2;
     int     maxnum, maxch, len, ilen;
     char    **wfoarr, wfostr[200]; 
/*-------------------------------------------------------------------*/
     *iret  = 0;
     ier    = 0;
     len    = 3;
     num    = 50;
     maxch  = 50; 
     maxnum = 50;

    /*
     * Allocate memory for array size.
     */

     wfoarr = (char **)malloc(num * sizeof(char *));
     for ( ii = 0; ii < num; ii++ ) {
          wfoarr[ii] = (char *)malloc((len+1) * sizeof(char *));
     }

    /*
     * Break apart the WFO string into an array.
     */

     cst_lstr ( strin, &ilen, &ier );
     cst_clst ( strin, ';', " ", maxnum, maxch, wfoarr, &numstr, &ier);

    /*
     * Get list of WFOs in a string separated by '...'
     */

     utl_wfos ( wfoarr, numstr, wfostr, &iret2 );

     cst_lstr ( wfostr, &ilen, &ier );
     *ilenout = ilen;
     strcpy (strout, wfostr);

    /*
     * Free memory. 
     */

     for( ii = 0; ii < num; ii++ )
        free( wfoarr[ii] );

     if ( wfoarr )
        free( (char **) wfoarr );
}

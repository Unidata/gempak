#include "geminc.h"
#include "gemprm.h"

void utl_wfos ( char **aryptr, int numwfo, char *wfostr, int *iret )
/************************************************************************
 * utl_wfos								*
 *                                                                      *
 * This function creates the list of active WFO ids string. Each WFO 	*
 * is separated by '...' in the string. A new line character will be	*
 * inserted after the eighth WFO id.					*
 *                                                                      *
 * utl_wfos ( aryptr, numwfo, wfostr, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	**aryptr    	char		WFO id array			*
 *	numwfo		int		Number of WFOs in the array	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*wfostr		char		List of active WFO ids		*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 ***********************************************************************/
{
     int     ii, jj, kk, ier, ier1, fnd, numstr, icnt;
     char    **wfoarr;
/*-------------------------------------------------------------------*/
     *iret = 0;
     ier   = 0;

   /*
    * Initialize memory for WFO array.
    */

    wfoarr = (char **) malloc(sizeof(char *) * 4);
    for ( ii = 0; ii < 4; ii++ ) {
	wfoarr[ii] = (char *) malloc(5);
    }

   /*
    * Store first WFO id.
    */

    strcpy ( wfostr, aryptr[0] );
    icnt = 1;

   /*
    * Compare WFO ids.  Store unique names.
    */

    for ( ii = 0; ii < numwfo; ++ii) {
        for ( jj = numwfo-1; jj > ii; --jj) {
            if ( strcmp(aryptr[ii], aryptr[jj] ) != 0){

	        /*
		 * Break apart multiple WFO IDs.
		 */

	        cst_nocc ( aryptr[jj], '/', 1, 0, &fnd, &ier1);

	        if ( fnd > 0 ) {

                    cst_clst ( aryptr[jj], '/', " ", 4, 5, wfoarr, 
		               &numstr, &ier ); 

                    for ( kk = 0; kk <  numstr; ++kk) {
	                if ( strstr ( wfostr, wfoarr[kk]) == NULL ){
                            strcat ( wfostr, "..." );
                            strcat( wfostr, wfoarr[kk] );
	                }
		    }
	        }
	        else if ( strstr ( wfostr, aryptr[jj] ) == NULL ){
                    strcat ( wfostr, "..." );
	    if ( (icnt % 8 ) == 0 ) {
                strcat ( wfostr, "\n" );
	    }
                    strcat( wfostr, aryptr[jj] );
		    icnt++;
	        }
	    }
        } 
     }
     strcat ( wfostr, "..." );
     for ( ii = 0; ii < 4; ii++ ) {
         free ( wfoarr[ii] );
     }
     free ( wfoarr );

     *iret = ier;
}

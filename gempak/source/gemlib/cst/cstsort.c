#include "geminc.h"
#include "gemprm.h"

void cst_sort ( int itype, int *nstr, char **inpstr, int *nout, 
	        char **outstr, int *iret )
/************************************************************************
 * cst_sort								*
 *									*
 * This subroutine sorts a list of strings. The output list may be	*
 * sorted forward or backward, and may contain only the unique entries.	*
 * The input and output arrays may be the same.				*
 *									*
 * cst_sort  ( itype, nstr, inpstr, nout, outstr, iret )		*
 *									*
 * Input parameters:							*
 *	itype		int		Type of sort			*
 *					   1 = Forward			*
 *					  -1 = Backward			*
 *					   2 = Forward, unique only	*
 *					  -2 = Backward, unique only	*
 *	*nstr		int		Number of input strings		*
 *	**inpstr (nstr)	CHAR*		Input strings			*
 *									*
 * Output parameters:							*
 *	*nout		int		Number of output strings	*
 *	**outstr (nout)	char		Sorted strings			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 7/03						*
 ***********************************************************************/
 {
	char 	swpbuf[160];
	int	ii, jj, istop, ibsrt;
/*---------------------------------------------------------------------*/
	*iret  = 0;
	istop = *nstr;

       /*
	* Load output array.
        */

	for ( ii = 0; ii < *nstr; ii++ ) {
	    strcpy (outstr [ii] , inpstr[ii] );
	}

       /*
	* Perform bubble sort.
	*/

	for ( ii = 0; ii < istop; ii++ ) {
	    for (jj = ii+1; jj < istop; ++jj ) {
	        if ( strcmp(outstr [ii], outstr [jj]) > 0 ) {
 	          strcpy (swpbuf, outstr[ii]); 
	          strcpy (outstr [ii], outstr [jj]);
	          strcpy (outstr [jj], swpbuf );
	        }
	    }
	}

       /*
        * If the sort order is backward, reverse the array.
	*/

	if  ( itype < 0 ){
	    jj = *nstr-1;
	    ibsrt = *nstr/2;
	    for ( ii = 0;  ii < ibsrt; ii++ ) {
		strcpy ( swpbuf, outstr [ii] );
		strcpy ( outstr [ii], outstr [jj] );
		strcpy ( outstr [jj], swpbuf );
		jj--;
	    }
	}

       /*
	* If the user has requested, return only unique entries.
	*/

	if ( G_ABS(itype) == 2 )  {
	    jj = 0;
	    for ( ii = 1; ii < *nstr; ii++) {
	    	if  ( strcmp (outstr [ii], outstr [jj] ) != 0 ) {
		    jj++;
		    strcpy ( outstr [jj], outstr [ii] );
		}
	    }
	    *nout = jj+1;
	    for ( ii = *nout+1; ii < *nstr; ii++ ) {
		strcpy ( outstr [ii], " " );
	    }
        }
	else {
	    *nout = *nstr;
	}
 }

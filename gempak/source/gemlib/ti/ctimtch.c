#include "geminc.h"
#include "gemprm.h"

static int compare ( const void *a, const void *b );

void cti_mtch ( int match, const char *dattim, const char **times,
			int ntime, int minute, int *ipos, int *iret )
/************************************************************************
 * cti_mtch								*
 *									*
 * This subroutine searches for a particular date/time in a list of	*
 * date/times.  The position of the nearest date/time, determined by	*
 * time match scheme used, is returned in IPOS.  If there is no match	*
 * possible, IPOS is set to 0.						*
 *									*
 * cti_mtch  ( match, dattim, times, ntime, minute, ipos, iret )	*
 *									*
 * Input parameters:							*
 *	match		int		flag for time match scheme	*
 *					   1 = exact only		*
 *					   2 = closest before		*
 *					   3 = closest after		*
 *					   4 = closest before or after	*
 *	*dattim		char		Date/time			*
 *	**times		char		Sorted list of date/times	*
 *	ntime		int		Number of date/times in list	*
 *	minute		int		Minutes difference for match	*
 *					   0 = ignore			*
 *									*
 * Output parameters:							*
 *	*ipos		int		Position of nearest date/time	*
 *				 	  -1 = not found		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * R. Curtis/EAI	10/00	Changed LLMXTM to MXNMFL in tsort array *
 * T. Piper/SAIC	09/07	Re-wrote in 'C'				*
 ***********************************************************************/
{
    int ier, ii, imin, jpos, mmin, nmin, nsort;
    char **tsort=NULL, ttemp[DTTMSZ], ttemp2[DTTMSZ];
/*---------------------------------------------------------------------*/
/*
 *  Initialize ipos to not found.
 */
    *ipos = -1;
    *iret = 0;

/*
 *  Check the number of minutes for the difference range.  If the 
 *  value is 0, set the number of minutes to a large value.
 */
    if  ( minute == 0 ) 
/*
 *  Set the range in minutes to more than 1 year.
 */
	imin = 600000;
    else
	imin = minute;

/*
 *  Try for an exact match.
 */
    cst_find ( dattim, times, ntime, ipos, &ier );

/*
 *  If the time is not found, and the match type is not 1,
 *  continue checking.
 */
    if  ( ( *ipos == -1 ) && ( match != 1 ) ) {

/*
 *  Add all the times, including the search time, to a new array.
 */
	nsort = ntime + 1;
	G_MALLOC(tsort, char*, nsort, "cti_tmtch:  tsort");
	for (ii=0; ii<ntime;ii++) {
	    G_MALLOC(tsort[ii], char, strlen(times[ii])+1,
				"cti_tmtch:  tsort[ii]");
	    strcpy(tsort[ii], times[ii]);
	}
	G_MALLOC(tsort[ntime], char, strlen(dattim)+1,
				"cti_tmtch:  tsort[ntime]");
	strcpy(tsort[ntime], dattim);

/*
 *  Sort the new array of times.
 */
	qsort((void*)tsort, (size_t)nsort, sizeof(char*), compare );
	cti_yyyy(nsort, tsort, tsort, &ier ); 
/*
 *  Find the location of the search time in the new array.
 */
	cst_find ( dattim, (const char **)tsort, nsort, &jpos, &ier );

/*
 *  Find the closest time before the search time.
 */
	if  ( match == 2 ) {
/*
 *  If the search time is the first in the array, there is no valid match to be found.
 */
	    if  ( jpos != 0 ) {
		strcpy(ttemp, tsort[jpos-1]);
/*
 *  Check the time difference range.
 */
		ti_diff ( (char *)dattim, ttemp, &nmin, &ier,
				strlen(dattim), strlen(ttemp) );
		if  ( G_ABS(nmin) <= imin ) {
/*
 *  Find the closest time in the original array.
 */
		    cst_find ( ttemp, times, ntime, ipos, &ier );
		}
	    }
	}
/*
 *  Find the closest time after the search time.
 */
	else if  ( match == 3 ) {
/*
 *  If the search time is the last in the array, there is no valid match to be found.
 */
	    if  ( jpos != ntime ) {
		strcpy(ttemp, tsort[jpos+1]);
/*
 *  Check the time difference range.
 */
		ti_diff ( (char *)dattim, ttemp, &nmin, &ier,
				strlen(dattim), strlen(ttemp) );
		if  ( G_ABS(nmin) <= imin ) {
/*
 *  Find the closest time in the original array.
 */
		    cst_find ( ttemp, times, ntime, ipos, &ier );
		}
	    }
	}
/*
 *  Find the closest time before or after the search time.
 */
	else if  ( match == 4 ) {
/*
 *  If the search time is the first in the array, only check the after case.
 */
	    if  ( jpos != 0 ) { 
		strcpy(ttemp, tsort[jpos-1]);
		ti_diff ( (char *)dattim, ttemp, &mmin, &ier,
				strlen(dattim), strlen(ttemp) );
		}
	    else {
		mmin = 610000;
	    }
/*
 *  If the search time is the last in the array, only check the before case.
 */
	    if  ( jpos != ntime ) {
	        strcpy(ttemp2, tsort[jpos+1]);
	        ti_diff ( (char *)dattim, ttemp2, &nmin, &ier,
				strlen(dattim), strlen(ttemp2) );
	    }
	    else {
	        nmin = 610000;
	    }
/*
 *  Check the before and after time differences to find the closest.
 */
	    if  ( G_ABS(mmin) < G_ABS(nmin) ) { 
	        if  ( G_ABS(mmin) <= imin ) {
/*
 *  Find the closest time in the original array.
 */
		cst_find (ttemp, times, ntime, ipos, &ier);
	        }
	    }
	    else {
		if  ( G_ABS(nmin) <= imin ) {
/*
 *  Find the closest time in the original array.
 */
		    cst_find (ttemp2, times, ntime, ipos, &ier);
		}
	    }
	}
	for (ii=0; ii<nsort;ii++) {
	    G_FREE(tsort[ii], char);
	}
	G_FREE(tsort, char*);
    }
}

/*=====================================================================*/

static int compare ( const void *a, const void *b )
{
    return  strcmp( *(char **)a, *(char **)b );
}

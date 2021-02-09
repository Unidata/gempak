#include "geminc.h"
#include "gemprm.h"

void cti_yyyy ( int ntime, char **timin, char **outime, int *iret )
/************************************************************************
 * cti_yyyy								*
 *									*
 * This subroutine reorders a list of GEMPAK times so that times in the *
 * 20th century (YY greater than 40) precede those in the 21st century  *
 * (YY less than or equal to 40).  The input and output arrays may be   *
 * the same.  The input times must be sorted smallest to largest.  The  *
 * output times will be sorted earliest to latest.                      *
 *									*
 * cti_yyyy  ( ntime, timin, outime, iret )				*
 *									*
 * Input parameters:							*
 *	ntime		int		Number of times 		*
 *	*timin		char		GEMPAK times 			*
 *									*
 * Output parameters:							*
 *	*outime 	char		Sorted times			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D. Kidwell/NCEP	 2/99	                                        *
 * D. Kidwell/NCEP	 4/99	Stored to outime; added check for YYYY  *
 * T. Piper/SAIC	 4/02	Fixed UMR; checked for ntime < 1	*
 * T. Piper/SAIC	09/07	Re-write in 'C'				*
 * S. Jacobs/NCEP	11/07	Fix check on counter vs number of times	*
 * B. Hebbard/NCEP	 3/18   Moved century break from 2020 to 2040   *
 ***********************************************************************/
{
    int ier, ii, imove, jj, length;
    char cent1[3], cent2[3], savstr[DTTMSZ], *slash;
    Boolean down, found;
/*---------------------------------------------------------------------*/
    *iret = 0;
    if ( ntime < 1 ) return;

    if ( outime != timin ) {
	for (ii=0; ii<ntime; ii++) {
	    strcpy(outime[ii], timin[ii]);
        }
    }

/*
 *  If input year is four digits, data is already ordered correctly.
 */
    cst_lstr(timin[0], &length, &ier);
    slash = strstr(timin[0], "/" );
    if ( ( length >= DTTMS_SIZE+1 ) && ( (&timin[0]-&slash) == 9 ) )  return;

/*
 *  Reorder the 2-digit years if they span two centuries.
 */
    if ( ntime > 1 ) {
	ti_ccnt ( timin[0], cent1, &ier,
			strlen(timin[0]), sizeof(cent1) );
	cent1[3] = '\0';
	ti_ccnt ( timin[ntime-1], cent2, &ier,
			strlen(timin[ntime-1]), sizeof(cent2) );
	cent2[3] = '\0';

/*
 *  Check for new century.
 */
	if ( strcmp(cent1, cent2) != 0 ) {
	    ii = 0;
	    found = FALSE; 
            while ( ! found ) {
	        if ( strncmp(timin[ii], "40", 2) > 0 ) {
		    found = TRUE;
		    if ( ii > ( ntime / 2 ) ) {
		        down  = TRUE;
		        imove = ntime - ii + 1;
		    }
		    else {
		        down  = FALSE;
		        imove = ii - 1;
		    }
	        }
	        else {
		    ii++;
		    if ( ii >= ntime ) return;
	        }
	    }

/*
 *  Reorder GEMPAK times.
 */
	    for (ii=0;ii<imove;ii++) {
	        if ( down ) {
		    strcpy(savstr, outime[ntime-1]);
		    for (jj=ntime-1; jj>-1; jj--) {
		        strcpy(outime[jj], outime[jj-1]);
		    }
		    strcpy(outime[0], savstr);
	        }
	        else {
		    strcpy(savstr, outime[0]);
		    for (jj=0; jj<ntime-1; jj++ ) {
			strcpy(outime[jj], outime[jj+1]);
		    }
		    strcpy(outime[ntime-1], savstr);
	        }
	    }
	}
    }
}

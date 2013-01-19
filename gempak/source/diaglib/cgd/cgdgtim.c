#include "fortran_wrappers.h"

void cgd_gtim ( int iacss, int maxtim, char **timarr, int *ntimes,
                int *iret )
/************************************************************************
 * cdg_gtim                                                             *
 *                                                                      *
 * This subroutine returns all the times present in a grid file.	*
 * Only the first times are returned.  They are sorted from earliest	*
 * to latest.								*
 *                                                                      *
 * cgd_gtim ( iacss, maxtim, timarr, ntimes, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	iacss		int		Grid access number		*
 *	maxtim		int		Maximum number of times		*
 *                                                                      *
 * Output parameters:                                                   *
 *      **timarr        char            Gempak times			*
 *      *ntime          int             Number of times          	*
 *      *iret           int             Return code                     *
 *                                       0 = normal return              *
 *					-4 = file not open		*
 *					-6 =read/write error		*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          1/06   C wrapper of GD_GTIM                    *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char *timstr;
    int maxchr, len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ntimes = 0;

/*
 *  Allocate space to hode the times in a single string.
 */
    maxchr = maxtim * 21 + 1;
    G_MALLOC ( timstr, char, maxchr, "cgd_gtim - timstr"); 
    if ( timstr == NULL ) {
        return;
    }

/*
 *  Get times in a single string.
 */
    gd_gtimw ( &iacss, &maxchr, ";", timstr, iret );
    timstr[maxchr-1] = '\0';
    cst_lstr ( timstr, &len, &ier );
    timstr[len] = '\0';

/*
 *  Split the time string into an array of times.
 */
    if ( strchr ( timstr, ';' ) == NULL ) {
	strcpy ( timarr[0], timstr );
	*ntimes = 1;
    } else {
	cst_clst ( timstr, ';', "", maxtim, 21, timarr, ntimes, iret );
    }

    G_FREE ( timstr, char );
    return;
}

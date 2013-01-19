#include "fortran_wrappers.h"

void ctg_rinc ( const char *tbegin, const char *tend, const char *tinc,
                int itftyp, int *ntime, char **times, int *iret )
/************************************************************************
 * ctg_rinc                                                             *
 *                                                                      *
 * This subroutine returns the times in a time range with an            *
 * increment.                                                           *
 *                                                                      *
 * ctg_rinc ( tbegin, tend, tinc, itftyp, ntime, times, iret )          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *tbegin		const char	Start time                      *
 *      *tend		const char	Stop time                       *
 *      *tinc		const char	Time increment                  *
 *      itftyp		int		Range type                      *
 *                                        1 = forecast range            *
 *                                        2 = date/time range           *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ntime		int		Number of output times          *
 *      **times		char		Times in range                  *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -14 = invalid time increment    *
 *                                      -15 = too many times            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 1/06	C wrapper of TG_RINC			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char *timstr;
    int maxchr, len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ntime = 0;

/*
 * Build a string to hold the array of times.
 */
    maxchr = LLMXGT * 36 + 1;
    G_MALLOC ( timstr, char, maxchr, "ctg_rinc - timstr" ); 
    if ( timstr == NULL ) return;

/*
 * Call TG_RINC wrapper.
 */
    tg_rincw ( tbegin, tend, tinc, &itftyp, ";", &maxchr, timstr, iret );
    timstr[maxchr-1] = '\0';
    cst_lstr ( timstr, &len, &ier );
    timstr[len] = '\0';

/*
 * Split the time string into an array of times.
 */
    if ( strchr ( timstr, ';' ) == NULL ) {
	strcpy ( times[0], timstr );
	*ntime = 1;
    } else {
    	cst_clst ( timstr, ';', "", LLMXGT, 20, times, ntime, iret );
    }

    G_FREE ( timstr, char );
    return;
}

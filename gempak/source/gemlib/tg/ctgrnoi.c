#include "fortran_wrappers.h"

void ctg_rnoi ( const char *tbegin, const char *tend, int itftyp,
         int ntimef, char **filtim, int *ntime, char **times, int *iret )
/************************************************************************
 * ctg_rnoi                                                             *
 *                                                                      *
 * This subroutine checks which times in a file are within a time	*
 * range without an increment.						*
 *                                                                      *
 * ctg_rnoi ( tbegin, tend, itftyp, ntimef, filtim, ntime, times, iret )*
 *                                                                      *
 * Input parameters:                                                    *
 *	*tbegin		const char	Start time			*
 *	*tend		const char	Stop time			*
 *	itftyp		int		Range type			*
 *					  1 = forecast range		*
 *					  2 = date/time range		*
 *	ntimef		int		Number of times in file		*
 *	**filtim	char		File times			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ntime		int		Number of output times		*
 *	**times		char		Times in range			*
 *      *iret           int             Return code                     *
 *                                       0 = normal return              *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          1/06   C wrapper of TG_RNOI			*
 * T. piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char *timstr;
    int nt, len;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ntime = 0;

/*
 * Build a single string from the array of times.
 */
    len = 0;
    for ( nt = 0; nt < ntimef; nt++ ) {
	len += strlen(filtim[nt]) + 1;
    }
    G_MALLOC ( timstr, char, len, "ctg_rnoi - timstr" ); 
    if ( timstr == NULL ) {
        return;
    }
    cst_lstc ( filtim, ntimef, ";", len, timstr, iret );

/*
 * Call the TG_RNOI wrapper.
 */
    tg_rnoiw ( tbegin, tend, &itftyp, ";", timstr, iret );
    timstr[len-1] = '\0';
    cst_lstr ( timstr, &len, iret );
    timstr[len] = '\0';

/*
 * Break the single string into an array of times.
 */
    if ( strchr ( timstr, ';' ) == NULL ) {
	strcpy ( times[0], timstr );
	*ntime = 1;
    } else {
        cst_clst ( timstr, ';', "", ntimef, len, times, ntime, iret );
    }

    G_FREE ( timstr, char );

    return;
}

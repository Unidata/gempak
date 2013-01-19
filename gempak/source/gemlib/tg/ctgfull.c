#include "fortran_wrappers.h"

void ctg_full ( const char *gdattm, const char *firstm,
	        const char *lasttm, char *fulltm, int *iret )
/************************************************************************
 * CTG_FULL                                                             *
 *                                                                      *
 * This subroutine converts the user input for a single grid time       *
 * into a full grid time string.                                        *
 *                                                                      *
 * CTG_FULL ( gdattm, firstm, lasttm, fulltm, iret )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *gdattm		const char	Input grid time                 *
 *      *firstm		const char	First time in grid file         *
 *      *lasttm		const char	Last time in grid file          *
 *                                                                      *
 * Output parameters:                                                   *
 *      *fulltm		char		Full GEMPAK grid time           *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = invalid date or time      *
 *                                       -2 = invalid forecast type     *
 *                                       -3 = invalid forecast time     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 1/06	C wrapper of TG_FULL			*
 ************************************************************************/
{
    char temptm[MXFLSZ+1];
    int len, ier;
/*----------------------------------------------------------------------*/
    /*
     * Call TG_FULL.
     */
    tg_full ( gdattm, firstm, lasttm, temptm, iret );

    /*
     * Convert Fortran string to C string.
     */
    temptm[MXFLSZ] = '\0';
    cst_lstr ( temptm, &len, &ier );
    temptm[len] = '\0';
    strcpy ( fulltm, temptm );

    return;
}

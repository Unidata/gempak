#include "fortran_wrappers.h"

void cti_stan ( const char *time, const char *lastim, char *dattim,
                int *iret )
/************************************************************************
 * CTI_STAN                                                             *
 *                                                                      *
 * This subroutine takes a user input time and converts it into a       *
 * standard GEMPAK time.  The parts of the date and time that are       *
 * not entered by the user are taken from the LASTIM, which must        *
 * already be in the standard format.                                   *
 *                                                                      *
 * CTI_STAN ( time, lastim, dattim, iret )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      *time		const char	Input time                      *
 *      *lastim		const char	Last time in GEMPAK format      *
 *                                                                      *
 * Output parameters:                                                   *
 *      *dattim		char		GEMPAK standard time            *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = invalid input string      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 1/06	C wrapper of TI_STAN			*
 ************************************************************************/
{
    char dumdtm[MXFLSZ+1];
    int len, ier;
/*----------------------------------------------------------------------*/
    /*
     * CALL TI_STAN.
     */
    ti_stan ( (char *)time, (char *)lastim, dumdtm, iret,
        strlen(time), strlen(lastim), MXFLSZ );

    /*
     * Convert Fortran string to C string.
     */
    dumdtm[MXFLSZ] = '\0';
    cst_lstr ( dumdtm, &len, &ier );
    dumdtm[len] = '\0';
    strcpy ( dattim, dumdtm );

    return;
}

#include "fortran_wrappers.h"

void ctg_vi2f ( const char *vdtm, const char *idtm, char *fdtm, int *iret )
/************************************************************************
 * ctg_vi2f                                                             *
 *                                                                      *
 * This subroutine converts a forecast valid time of the form           *
 * YYMMDD/HHNN and an initial GEMPAK time of the form yymmdd/hhnn       *
 * into a proper GEMPAK forecast time stamp of the form                 *
 * yymmdd/hhnnFhhhnn.                                                   *
 *                                                                      *
 * ctg_vi2f ( vdtm, idtm, fdtm, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *VDTM		const char	Valid GEMPAK time, YYMMDD/HHNN  *
 *      *IDTM		const char	Init. GEMPAK time, yymmdd/hhnn  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *FDTM		char		Forecast time, yymmdd/hhnn      *
 *      *IRET		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = invalid date or time      *
 *                                       -3 = invalid forecast time     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06   C wrapper of TG_VI2F                    *
 ************************************************************************/
{
    char tmpdtm[21];
    int ier, len;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Call TG_VI2F.
     */
    tg_vi2f ( vdtm, idtm, tmpdtm, &len, iret );

    /*
     * Convert Fortran string to C string.
     */
    if ( *iret == 0 ) {
        tmpdtm[20] = '\0';
	cst_lstr ( tmpdtm, &len, &ier );
	tmpdtm[len] = '\0';
	strcpy ( fdtm, tmpdtm );
    }

    return;
}

#include "fortran_wrappers.h"

void ctg_vald ( const char *gdattm, char *vdattm, int *iret )
/************************************************************************
 * ctg_vald                                                             *
 *                                                                      *
 * This subroutine converts a grid time to the valid date/time.         *
 * The input string must be a full grid time.  The output time will     *
 * contain only the date and time.  The input and output strings        *
 * may be the same.                                                     *
 *                                                                      *
 * ctg_vald ( gdattm, vdattm, iret )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *gdattm		const char	Grid date/time                  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *vdattm		char		Valid date/time                 *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = invalid time              *
 *                                       -2 = invalid forecast type     *
 *                                       -3 = invalid forecast time     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06   C wrapper of TG_VALD                    *
 ************************************************************************/
{
    char tmpttm[21];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Call TG_VALD.
     */
    tg_vald ( (char *)gdattm, tmpttm, iret, strlen(gdattm), sizeof(tmpttm) );

    /*
     * Convert Fortran string to C string.
     */
    if ( *iret == 0 ) {
        tmpttm[20] = '\0';
	cst_lstr ( tmpttm, &len, &ier );
	tmpttm[len] = '\0';
	strcpy ( vdattm, tmpttm );
    }
}

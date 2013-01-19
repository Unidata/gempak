#include "fortran_wrappers.h"

void clv_cord ( const char *vcoord, char *vparm, int *ivert, int *iret )
/************************************************************************
 * clv_cord								*
 *									*
 * This subroutine converts the input for VCOORD to upper-case and	*
 * translates it to a numeric value.					*
 *									*
 * clv_cord ( vcoord, vparm, ivert, iret )				*
 *									*
 * Input parameters:							*
 *	*vcoord		const char	Vertical coordinate input	*
 *									*
 * Output parameters:							*
 *	*vparm		char		Upper-case coordinate 		*
 *	*ivert		int		Numeric vertical coordinate	*
 *					  0 = NONE			*
 *					  1 = PRES			*
 *					  2 = THTA			*
 *					  3 = HGHT			*
 *					  4 = SGMA			*
 *					  5 = DPTH			*
 *	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
 *					 -3 = invalid coordinate	*
 **									*
 * Log:									*
 * R. Tian/SAIC		 2/06	C wrapper of LC_CORD			*
 ************************************************************************/
{
    char tmpcrd[13];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Call LV_CORD.
     */
    lv_cord ( (char *)vcoord, tmpcrd, ivert, iret,
        strlen(vcoord), sizeof(tmpcrd) );

    /*
     * Convert Fortran string to C string.
     */
    tmpcrd[12] = '\0';
    cst_lstr ( tmpcrd, &len, &ier );
    tmpcrd[len] = '\0';
    strcpy ( vparm, tmpcrd );

    return;
}

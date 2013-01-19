#include "fortran_wrappers.h"

void clv_ccrd ( int ivcord, char *vcoord, int *iret )
/************************************************************************
 * clv_ccrd								*
 *									*
 * This subroutine translates a numeric value for IVCORD into its 	*
 * character value in VCOORD.						*
 *									*
 * clv_ccrd ( ivcord, vcoord, iret )					*
 *									*
 * Input parameters:							*
 *	ivcord		int		Numeric vertical coordinate	*
 *									*
 * Output parameters:							*
 *	*vcorrd		char		Vertical coordinate 		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -3 = invalid coordinate	*
 **									*
 * Log:									*
 * R. Tian/SAIC		 2/06	C wrapper of LV_CCRD			*
 ************************************************************************/
{
    char tmpcrd[13];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret  = 0;

    /*
     * Call LV_CCRD.
     */
    lv_ccrd ( &ivcord, tmpcrd, iret, sizeof(tmpcrd) );

    /*
     * Convert Fortran string to C string.
     */
    tmpcrd[12] = '\0';
    cst_lstr ( tmpcrd, &len, &ier );
    tmpcrd[len] = '\0';
    strcpy ( vcoord, tmpcrd );

    return;
}

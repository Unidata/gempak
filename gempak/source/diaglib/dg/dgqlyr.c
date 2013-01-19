#include "dg.h"

#define NPARM		15

void dg_qlyr ( const char *gfunc, int *lflag, int *iret )
/************************************************************************
 * dg_qlyr								*
 *									*
 * This subroutine determines if the function specification contains	*
 * a layer-averaged quantity.						*
 *									*
 * dg_qlyr ( gfunc, lflag, iret )					*
 *									*
 * Input parameters:							*
 *	*gfunc		  const char	Grid Function 			*
 *									*
 * Output parameters:							*
 *      *lflag		  int		Flag for layer quantities	*
 *	*iret		  int		Return code			*
 *					  0 = normal return		*
 *					 -3 = GFUNC is blank		*
 **									*
 * Log:									*
 * T. Lee/SAIC		  3/05		Based on GDXCLA			*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 ************************************************************************/
{
    char parms[NPARM][5] = { "LAV(", "LDF(", "MASS", "MDIV", "MSDV",
                             "PVOR", "RICH", "STAB", "LTRN", "VLAV",
			     "VLDF", "QVCL", "WSHR", "BVSQ", "THRM" };
    char pfunc[257];
    int lens, lnx, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *lflag = G_FALSE;

    /*
     * Check to see if GFUNC was specified.
     */
    cst_lstr ( (char *)gfunc, &lens, iret );
    if ( lens == 0 ) {
	*iret = -3;
	return;
    }

    /*
     * Convert GFUNC to upper case.
     */
    cst_lcuc ( (char *)gfunc, pfunc, iret );

    /*
     * Remove blanks from pfunc.
     */
    cst_rmbl ( pfunc, pfunc, &lnx, iret );

    /*
     * Check for the occurrence of a layer quantity.
     */
    for ( i = 0; i < NPARM; i++ ) {
	if ( strstr ( pfunc, parms[i] ) ) {
	    *lflag = G_TRUE;
	    break;
	}
    }

    return;
}

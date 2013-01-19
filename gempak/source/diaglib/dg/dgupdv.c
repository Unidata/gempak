#include "dg.h"

void dg_updv ( const char *func, const int *numu, const int *numv, 
               const int *num1, const int *num2, int *iret )
/************************************************************************
 * dg_updv								*
 *									*
 * This subroutine makes a parameter name from the string FUNC and the  *
 * parameter names of grids NUM1 and NUM2.  It then updates the header	*
 * for both components of a vector grid stored for the grid diagnostics.*
 * It should be called when a grid is made from other grids.		*
 *									*
 * dg_updv ( func, numu, numv, num1, num2, iret )			*
 *									*
 * Input parameters:							*
 *	*func		const char	Function computed		*
 *	*numu		const int	Number of u grid to update	*
 *	*numv		const int	Number of v grid to update	*
 *	*num1		const int	Pointer to first input grid	*
 *	*num2		const int	Pointer to second input grid	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 8/88	Adapted from DG_UPDH			*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], parm[14];
    int level1, level2, jvcord, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    /*
     * Get the information to update grids.
     */
    dg_mhdr ( func, num1, num2, time1, time2, &level1, &level2, &jvcord,
              parm, &ier );

    /*
     * Update grids.
     */
    dg_upvg ( time1, time2, &level1, &level2, &jvcord, &zero, parm,
              numu, numv, &ier );

    return;
}

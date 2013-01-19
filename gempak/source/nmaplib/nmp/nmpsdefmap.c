#include "nmpcmn.h"

void nmp_sdefmap ( int lp, int *iret )
/************************************************************************
 * nmp_sdefmap                                                          *
 *                                                                      *
 * This function sets the default map for a given loop.                 *
 *                                                                      *
 * void nmp_sdefmap ( lp, iret )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int		loop index 			*
 * Output parameters:                                                   *
 *  *iret		int	 	return code			*
 *                                      -1     loop index out of range  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          08/01   initial coding                          *
 ***********************************************************************/
{
int 	 ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Check if loop index is within the range.
     */
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -1;
        return;
    }

    nmp_setmap(map_tbl[0].name, lp, FALSE, &ier); 
    nmp_simf(lp, " ", NO_IMG, &ier);

}

/*=====================================================================*/

#include "nmpcmn.h"

void nmp_szoom ( int lp, nmpstr_t zmarea, int *iret )
/************************************************************************
 * nmp_szoom                                                            *
 *                                                                      *
 * This function sets the zoom area for a specific loop.                *
 *                                                                      *
 * void nmp_szoom ( lp, zmarea, iret )                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int		loop index 			*
 *  zmarea		nmpstr_t	zoom area			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int	 return code				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		12/00	Created					*
 * M. Li/GSC            01/01   Added a check for loop out of range     *
 ***********************************************************************/
{

    if (lp < 0 || lp >= MAX_LOOP ) {
	*iret = -11;
    }
    else {
	*iret = 0;
	strcpy(maps[lp].garea[1], zmarea);
    }

}

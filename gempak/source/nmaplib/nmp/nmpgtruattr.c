#include "nmpcmn.h"

void nmp_gtruattr ( int lp, nmpstr_t tru_proj, 
				nmpstr_t tru_garea[2], int *iret )
/************************************************************************
 * nmp_gtruattr                                                         *
 *                                                                      *
 * This function gets the true map attributes.				*
 *                                                                      *
 * void nmp_gtruattr ( lp, tru_proj, tru_garea, iret )                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop index 				*
 *                                                                      *
 * Output parameters:                                                   *
 *  tru_proj            nmpstr_t        proj                            *
 *  tru_garea[2]        nmpstr_t        garea                           *
 *  *iret		int		return code			*
 *			                = -11  loop index out of range  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		01/02	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    *iret = 0;
    if (lp < 0 || lp >= MAX_LOOP ) {
	*iret = -11;
	return;
    }

    strcpy(tru_proj, maps[lp].tru_proj);
    strcpy(tru_garea[0], maps[lp].tru_garea[0]);
    strcpy(tru_garea[1], maps[lp].tru_garea[1]);
}

/*=====================================================================*/

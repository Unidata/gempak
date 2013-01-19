#include "nmpcmn.h"

void nmp_gmapattr ( int lp, nmpstr_t map, nmpstr_t proj, 
					nmpstr_t garea[2], int *iret )
/************************************************************************
 * nmp_gmapattr                                                         *
 *                                                                      *
 * This function gets the map attributes.				*
 *                                                                      *
 * void nmp_gmapattr ( lp, map, proj, garea, iret )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop index 				*
 *                                                                      *
 * Output parameters:                                                   *
 *  map			nmpstr_t	name of the predefined map	*
 *  proj                nmpstr_t        proj                            *
 *  garea[2]            nmpstr_t        garea                           *
 *  *iret		int		return code			*
 *			                = -11  loop index out of range  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		12/00	Created					*
 * M. Li/GSC		01/01	Added a check for lp out of range	*
 * M. Li/GSC		02/01	Added map to the maps			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    *iret = 0;
    if (lp < 0 || lp >= MAX_LOOP ) {
	*iret = -11;
	return;
    }

    strcpy(map, maps[lp].map);
    strcpy(proj, maps[lp].proj);
    strcpy(garea[0], maps[lp].garea[0]);
    strcpy(garea[1], maps[lp].garea[1]);
}

/*=====================================================================*/

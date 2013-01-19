#include "nmpcmn.h"

void nmp_smapattr ( int lp, nmpstr_t map, nmpstr_t proj, 
			nmpstr_t garea[2], Boolean allp, int *iret )
/************************************************************************
 * nmp_smapattr                                                         *
 *                                                                      *
 * This function sets the proj and garea strings for a specified loop.	*
 * Set for all loops if allp is TRUE.					*
 *                                                                      *
 * void nmp_smapattr ( lp, map, proj, garea, allp, iret )               *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int		loop index 			*
 *  map			nmpstr_t	name of the predifined map	*
 *  proj                nmpstr_t    	proj		              	*
 *  garea[2]           	nmpstr_t	garea				*
 *  allp		Boolean		all loops options               *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int	return code				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		12/00	Created					*
 * M. Li/GSC            01/01   Added a check for lp out of range       *
 * M. Li/GSC		02/01	Added map to maps structure		*
 * T. Piper/SAIC	04/05	Added storing both garea values		*
 ***********************************************************************/
{
int	ii;

/*---------------------------------------------------------------------*/

    *iret = 0;
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }

    if ( maps[lp].map != map ) strcpy(maps[lp].map, map);
    strcpy(maps[lp].proj, proj);
    strcpy(maps[lp].garea[0], garea[0]);
    strcpy(maps[lp].garea[1], garea[1]);
    /*
     * copy the info to all loop 
     */
    if (allp) {
        for (ii = 0; ii < MAX_LOOP; ii++) {
            if (ii != lp ) {
		strcpy(maps[ii].map, map);
                strcpy(maps[ii].proj, proj);
                strcpy(maps[ii].garea[0], garea[0]);
                strcpy(maps[ii].garea[1], garea[1]);
	    }
        }
    }
}

/*=====================================================================*/	

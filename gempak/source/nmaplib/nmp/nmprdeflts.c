#include "nmpcmn.h"

void nmp_rdeflts ( int *iret )
/************************************************************************
 * nmp_rdeflts                                                        	*
 *                                                                      *
 * This function restores the defaults for both the selected map and 	*
 * the overlays.							* 
 *                                                                      *
 * void nmp_rdeflts ( iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC            08/01   initial coding				*
 * J. Wu/SAIC            08/01   restore map with default_map values	*
 ***********************************************************************/
{
    int		lp, ii; 

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /*
     * Restore the default map for all loops.
     */ 
    for (lp = 0; lp < MAX_LOOP; lp++) {

	/*
	 * Restore the default_map array values.
	 */
        strcpy(maps[lp].imgfile, default_map[lp].imgfile);
        maps[lp].imgtyp = default_map[lp].imgtyp;
        strcpy(maps[lp].mapfile, default_map[lp].mapfile);	
        strcpy(maps[lp].mapattr, default_map[lp].mapattr);
	strcpy(maps[lp].map, default_map[lp].map);
	strcpy(maps[lp].proj, default_map[lp].proj);
	strcpy(maps[lp].garea[0], default_map[lp].garea[0]);
	strcpy(maps[lp].garea[1], default_map[lp].garea[1]);
	maps[lp].mode = default_map[lp].mode;

	/*
	 * Restore the default_overlay array values
	 */
	overlay[lp].novl = default_overlay[lp].novl;

        for (ii =0; ii < overlay[lp].novl; ii++ ) {
            overlay[lp].mapovl[ii].ityp = default_overlay[lp].mapovl[ii].ityp;
            strcpy (overlay[lp].mapovl[ii].attr,
                        default_overlay[lp].mapovl[ii].attr);
            strcpy (overlay[lp].mapovl[ii].gname,
                        default_overlay[lp].mapovl[ii].gname);
            strcpy (overlay[lp].mapovl[ii].title,
                        default_overlay[lp].mapovl[ii].title);
            overlay[lp].mapovl[ii].active = default_overlay[lp].mapovl[ii].active;
        }

    }

}

/*=====================================================================*/

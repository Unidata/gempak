#include "nmpcmn.h"

void nmp_setmap ( nmpstr_t map, int lp, Boolean allp, int *iret )
/************************************************************************
 * nmp_setmap                                                           *
 *                                                                      *
 * This function sets necessary attributes to draw a map area           *
 *                                                                      *
 * void nmp_setmap ( map, lp, allp, iret )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *  map			nmpstr_t   	predefined map name		*
 *  lp			int		loop index 			*
 *  allp		Boolean		all loops options		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int	 	return code			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		12/00	Created					*
 * M. Li/GSC            01/01   Added a check for invalid input	       	*
 * M. Li/GSC		04/01	replaced tb_fgeo with lc_gare		*
 * E. Safford/GSC	05/01	use (sometimes) DSET for images		*
 * E. Safford/GSC	05/01	no reset if lc_garea returns an error   *
 ***********************************************************************/
{
int 	 ii, lc_ier, ier, len, instat;
float	 ignore[2], ltln[4];
char	 proj[73];
nmpstr_t geog;
Boolean	 img_set = FALSE;
/*---------------------------------------------------------------------*/


    *iret = 0;

    if ( maps[lp].imgtyp == SAT_IMG || maps[lp].imgtyp == RAD_IMG ) {
	img_set = TRUE;
    }


    /*
     * Check for loop number out of range
     */
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }

    if (num_maps <= 0 ) nmp_init(&ier);   

    instat = 0;
    if (num_maps > 0) {
	for (ii = 0; ii < num_maps; ii++ ) {
	    if (strcmp(map, map_tbl[ii].name) == 0) {
		strcpy(geog, map_tbl[ii].geog);
		instat = 1;
	    }
	}

	/* 
	 * Check for invalid map name
	 */
	if( instat == 0) {
	    *iret = -14;
	    return;
	}

        /*
         * Query garea and proj 
         */
	lc_gare(geog, ltln, proj, ignore, &lc_ier, strlen(geog), sizeof(proj));
	st_null ( proj, proj, &len, &ier, sizeof(proj), sizeof(proj) );
	cst_rmbl(proj, proj, &len, &ier);

	strcpy(maps[lp].map, map);


	/*
	 *  Check for an error on the call to lc_gare.  If there was no 
	 *    error, replace the current proj and garea settings.
	 *  If there was an error, _and_ there is an image source, 
	 *    reset the garea to DSET. 
	 */
	if ( lc_ier >= G_NORMAL ) {

	    /*
	     *  Image sources always use the image (SAT || RAD) for proj
	     */
	    if ( !img_set ) {
  	        strcpy(maps[lp].proj, proj);
	    }

	    sprintf(maps[lp].garea[0], "%.2f;%.2f;%.2f;%.2f", 
		ltln[0], ltln[1], ltln[2], ltln[3]);	
	}
        else if (img_set ) {
	    strcpy (maps[lp].garea[0], "DSET");
	}

	/*
	 *  Wipe any zoom area.
	 */
	strcpy(maps[lp].garea[1], "");


	/*
	 * copy the info to all loop if applicable
	 */
	if (allp) {
	    for (ii = 0; ii < MAX_LOOP; ii++) {
		if (ii != lp ) {
		    strcpy(maps[ii].map, maps[lp].map);
		    strcpy(maps[ii].proj, maps[lp].proj);
		    strcpy(maps[ii].garea[0], maps[lp].garea[0]);
		    strcpy(maps[ii].garea[1], maps[lp].garea[1]);
		}
	    }
	}

    }
    else {
	*iret = -1;
    }

}

/*=====================================================================*/	

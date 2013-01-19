#include "nmpcmn.h"


void nmp_valid ( int lp, nmpstr_t map, nmpstr_t garea[2], 
					nmpstr_t proj, int *iret )
/************************************************************************
 * nmp_valid                                                            *
 *                                                                      *
 * This function validates a garea and projection.  Loop is included to *
 * use any set image file.  A return code of 0 indicates a valid map    *
 * garea, and projection. 						*
 *                                                                      *
 * Note that this routine does not actually assign the param values to  *
 * any of the internal nmp structures.  Use this routine to establish   *
 * the validity of map/garea/proj combinations only.			*
 *									*
 * The mechanism used to test the validity of the map/garea/proj        *
 * combination is gg_maps().  Since this actually tries to change the   *
 * settings in the common block, a second call is made to gg_maps() at  *
 * the end of this routine to re-set the garea/proj settings to those   *
 * previously set in the maps[] structure.				*
 *									*
 *									*
 * void nmp_valid( lp, map, garea, proj, iret )         		*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp                  int		loop index                     	*
 *  map			nmpstr_t	map string			*
 *  garea[2]		nmpstr_t  	garea string			*
 *  proj		nmpstr_t	projection string		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret               int	return code                            	*
 *			  	=   0 = OK				*
 *				= -14 = invalid map name		*
 *				= -18 = invalid garea			*
 *				= -19 = invalid proj			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/01  	Initial coding                          *
 * J. Wu/SAIC		08/01  	Match map names case-insensitively	*
 * T. Piper/SAIC	04/05	made garea two dimensional to get zoom	*
 ***********************************************************************/
{
int    		drpflg, ier, ii;
nmpstr_t	stored_garea, in_map, tbl_map;
Boolean 	map_ok;
/*---------------------------------------------------------------------*/

    *iret = 0;
    map_ok = FALSE;

    cst_lcuc (map, in_map, &ier);
    for (ii=0; ii < num_maps; ii++) {
        cst_lcuc (map_tbl[ii].name, tbl_map, &ier);
	if (strcmp (in_map, tbl_map) == 0 ) {
	    strcpy( map, map_tbl[ii].name );
	    map_ok = TRUE;
	    break;
	}
    }	   


    if (map_ok) {

	if (strlen(garea[1]) > (size_t)1) {
	    gg_maps( proj, garea[1], maps[lp].imgfile, &drpflg, &ier,
		strlen(proj), strlen(garea[1]), strlen(maps[lp].imgfile) );
	}
        else {
	    gg_maps( proj, garea[0], maps[lp].imgfile, &drpflg, &ier,
		strlen(proj), strlen(garea[0]), strlen(maps[lp].imgfile) );
	}
	if ( ier != G_NORMAL ) {

	    if (ier == -2) {   		/* invalid garea */
	        *iret = -18;
	    }
	    else if (ier == -5) {	/* invalid proj  */
	    	*iret = -19;
	    }
	    else {			/* some other error from gg_maps */
	        *iret = ier; 
	    }
        }

	/*
	 *  Restore the previous garea and proj settings in the common block.
	 *  Use the zoomed garea[1], if present, otherwise use garea[0].
	 */
	if (strlen(maps[lp].garea[1]) > (size_t)1) {
	    strcpy (stored_garea, maps[lp].garea[1]);
	}
	else {
	    strcpy (stored_garea, maps[lp].garea[0]);
	}
	gg_maps( maps[lp].proj, stored_garea, maps[lp].imgfile, &drpflg, 
		&ier, strlen(maps[lp].proj), strlen(stored_garea), 
		strlen(maps[lp].imgfile) );
    }
    else {
	*iret = -14;			/* invalid map name */
    }

}

/*=====================================================================*/

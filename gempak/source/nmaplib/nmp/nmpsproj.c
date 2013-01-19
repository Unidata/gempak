#include "nmpcmn.h"
#include "Nxm.h"

void nmp_sproj ( int lp, int *iret )
/************************************************************************
 * nmp_sproj                                                            *
 *                                                                      *
 * This function sets the projection for the specified loop.         	*
 *                                                                      *
 * void nmp_sproj( lp, iret )                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp                  int	loop index                      	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret               int	return code                            	*
 *			  	=   0 - OK				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            03/01   Created                                 *
 * T. Piper/GSC		07/01	Initialized new_proj			*
 * M. Li/SAIC		01/02	Added tru_proj, tru_garea		*
 * T. Piper/SAIC	04/05	Updated for nmp_smapattr CSC		*
 * T. Piper/SAIC	05/06	Greatly simplified			*
 ***********************************************************************/
{
int    	drpflg, ier, lens, which;
char   	cproj[4];
char   	proj[MAX_STR]="\0";
float   ang1=0.0F, ang2=0.0F, ang3=0.0F;
float   latll, lonll, latur, lonur;
nmpstr_t garea[2]= {"DSET", ""};

/*---------------------------------------------------------------------*/

    *iret = 0;
/*
 *  Determine which garea is in use.  garea[1] is the zoomed garea. 
 */
    if ( strlen(maps[lp].garea[1]) > (size_t)0 ) {
        which = 1;
    }
    else {
        which = 0;
    }
/*
 *  Set projection and garea for the image, if present.
 */
    if ( maps[lp].imgtyp == SAT_IMG || maps[lp].imgtyp == RAD_IMG ) {
	if ( maps[lp].imgtyp == SAT_IMG ) {
	    strcpy(cproj, "SAT");
	}
	else if ( maps[lp].imgtyp == RAD_IMG ) {
	   strcpy(cproj, "RAD");
	}
/*
 *  Query the current projection that is in effect in the GPLT common block.
 */
	gqmprj (proj, &ang1, &ang2, &ang3, &latll, &lonll,
		&latur, &lonur, &ier, sizeof(proj));
	st_null ( proj, proj, &lens, &ier, sizeof(proj), sizeof(proj) );
/*
 *  If we have image data and the current map projection does not
 *  match the current projection, then we need to explicitly set
 *  PROJ and GAREA.
 */
	if (strcmp (cproj, maps[lp].proj) != 0) {
/*
 *  Set the new image attributes in nmp structure.
 */
	    nmp_smapattr(lp, maps[lp].map, cproj, garea, False, &ier);
	}
    }
    else {
/*
 *  This 'else' means that the current imgtyp is NOT SAT or RAD.
 *  If the set projection was SAT or RAD and there no longer is any
 *  image data source, reset to the default map.  If the previous
 *  proj was not SAT or RAD, keep the current proj & garea.
 */
	if ( strcmp(maps[lp].proj, "SAT") == 0 || 
	     strcmp(maps[lp].proj, "RAD") == 0 ) {
	    nmp_setmap( map_tbl[0].name, lp, FALSE, &ier);
	}
    }

/*
 *  GG_MAPS called in all cases.  Is this necessary?
 */
    gg_maps( maps[lp].proj, maps[lp].garea[which], maps[lp].imgfile, &drpflg, &ier,
		strlen(maps[lp].proj), strlen(maps[lp].garea[which]),
		strlen(maps[lp].imgfile) );

/*
 *  Save off the true garea and projection that has been set.  Store these
 *  for use in getting and applying map settings to other loops. 
 */
    gqmprj (proj, &ang1, &ang2, &ang3, &latll,
                &lonll, &latur, &lonur, &ier, sizeof(proj));
    st_null ( proj, proj, &lens, &ier, sizeof(proj), sizeof(proj) );

    sprintf (maps[lp].tru_garea[which], "%.2f;%.2f;%.2f;%.2f", 
				latll, lonll, latur, lonur); 
    sprintf (maps[lp].tru_proj, "%s/%.2f;%.2f;%.2f", proj, 
				ang1, ang2, ang3);
/*
 *  No zoom area in use.  Make certain the tru_garea[1] is empty.
 */
    if ( !which ) {
	strcpy (maps[lp].tru_garea[1], "");
    }

    if ( ier != G_NORMAL ) {
	NxmErr_update();
	*iret = ier;
    }
}

#include "na.h"

void na_gtbl ( const char *cpyfil, char *name, char *proj, int *nxgd,
               int *nygd, float *garea, float *rnvblk, float *anlblk,
	       int *iret )
/************************************************************************
 * na_gtbl								*
 *									*
 * This subroutine finds grid INNAME (a numerical or character          *
 * identifier prefaced by '#') in a grid navigation table, then makes 	*
 * the navigation and analysis blocks.  The grid navigation is set up	*
 * in GEMPLT in order to check its validity.				*
 *									*
 * na_gtbl ( cpyfil, name, proj, nxgd, nygd, garea, rnvblk, anlblk,	*
 *           iret )							*
 *									*
 * Input parameters:							*
 *	*cpyfil		const char	Input for CPYFIL		*
 *									*
 * Output parameters:							*
 *	*name		char		Name of selected grid		*
 *	*proj		char		Grid projection			*
 *	*nxgd		int		Number of points in x dir	*
 *	*nygd		int		Number of points in y dir	*
 *	*garea		float		Grid corners			*
 *	*rnvblk		float		Grid navigation block		*
 *	*anlblk		float		Grid analysis block		*
 *	*iret		int		Return code			*
 *					 +1 = EXIT entered		*
 *					  0 = normal return		*
 *					 -4 = invalid navigation	*
 *					 -9 = grid not found in table	*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93		Copied from GDCTBL		*
 * D. Keiser/GSC	12/95		Changed FL_TOPN to FL_TBOP	*
 * R. Tian/SAIC		 7/06		Recoded from Fortran		*
 * S. Gilbert/NCEP      10/06   Added call to GR_VNAV                   *
 ************************************************************************/
{
    char gntrec[81], namgd[5], c2name[9], buffer[LLMXLN];
    float angl1, angl2, angl3, dbnds[4], deln;
    int angflg, found, valid;
    int iebnds[4], ingrdn, numgd, extnd, ier, ier1, iernum, navsz, i;
    FILE *lungrd;
/*----------------------------------------------------------------------*/
    *iret = 0;
    name[0] = '\0';

    /*
     * Get the grid number (INGRDN) out of NAME; a conversion error
     * sets IERNUM .ne. 0 and it is assumed that NAME is a type.
     */
    cst_lcuc ( (char *)cpyfil, c2name, &ier );
    strcpy ( name, &c2name[1] );
    cst_numb ( name, &ingrdn, &iernum );

    /*
     * Open the table of valid grid types.
     */
    lungrd = cfl_tbop ( "grdnav.tbl", "grid", &ier );
    if ( ier != 0 ) {
	er_wmsg ( "CFL", &ier, "grdnav.tbl", &ier1,
	          strlen("CFL"), strlen("grdnav.tbl") );
	*iret = -9;
	return;
    }

    /*
     * List the table contents for the user, if requested.
     */
    if ( strcmp ( name, "LIST" ) == 0 ) {
	while ( ! feof ( lungrd) ) {
	    cfl_trln ( lungrd, sizeof(gntrec), gntrec, &ier );
	    if ( ier != 0 ) break;

	    printf ( "%-79.79s\n", gntrec );
	}

	/*
	 * Rewind the table file.
	 */
	cfl_seek ( lungrd, 0, SEEK_SET, &ier );

	/*
	 * Prompt user for grid choice.
	 */
	printf ( "Enter grid id or number or type EXIT: " );
	scanf ( " %s", name );
	if ( name[0] == 'e' || name[0] == 'E' ) {
	    *iret = +1;
	    return;
	}
	cst_lcuc ( name, name, &ier );
	cst_numb ( name, &ingrdn, &iernum );
    }

    /*
     * Read through the list of valid grid types/numbers to get 
     * navigation/analysis information.
     */
    found = G_FALSE;
    while ( ! feof ( lungrd ) ) {
    	cfl_trln ( lungrd, sizeof(buffer), buffer, &ier );
	if ( ier != 0 ) break;

	sscanf ( buffer, "%s %d %s %f %f %f %f %f %f %f %d %d %f %d",
                 namgd, &numgd, proj, &angl1, &angl2, &angl3,
		 &garea[0], &garea[1], &garea[2], &garea[3],
		 nxgd, nygd, &deln, &extnd );
	if ( strcmp ( name, namgd ) == 0 || ingrdn == numgd ) {
	    found = G_TRUE;
	    break;
	}
    }
    cfl_clos ( lungrd, &ier );

    /*
     * Bail out if NAME wasn't found in the table.
     */
    if ( found == G_FALSE ) {
	*iret = -9;
	return;
    }

    /*
     * Fill navigation block.
     */
    angflg = G_TRUE;
    gr_vnav  ( proj, nxgd, nygd, &garea[0], &garea[1],
                &garea[2], &garea[3], &angl1, &angl2, &angl3,
		&angflg, &valid, &ier, strlen(proj) );

    if ( ier == 0 )
       grc_mnav  ( proj, nxgd, nygd, &garea[0], &garea[1],
                   &garea[2], &garea[3], &angl1, &angl2, &angl3,
                   &angflg, rnvblk, &ier );
    else {
       *iret = -2;
       return;
    }

    /*
     * Set up navigation in GEMPLT to check validity.
     */
    navsz = 13;
    grc_snav  ( &navsz, rnvblk, &ier );
    if ( ier != 0 ) {
	*iret = -4;
	return;
    }

    /*
     * Make an analysis block.
     */
    for ( i = 0; i < 4; i++ ) {
	iebnds[i] = extnd;
	dbnds [i] = RMISSD;
    }
    grc_mbn2 ( &deln, iebnds, dbnds, rnvblk, anlblk, &ier );

    return;
}

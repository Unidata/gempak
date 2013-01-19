#include "na.h"

void na_gnav ( const char *proj, const char *kxky, const char *gdarea,
               char *cprj, int *kx, int *ky,  float *grdout,
	       float *rnvblk, int *iret )
/************************************************************************
 * na_gnav								*
 *									*
 * This subroutine takes the user input for PROJ and KXKY and 		*
 * makes a grid navigation block.    					*
 *									*
 * na_gnav ( proj, kxky, gdarea, cprj, kx, ky, grdout, rnvblk, iret )	*
 *									*
 * Input parameters:							*
 *	*proj		const char	User input for PROJ		*
 *	*kxky		const char	User input for KXKY		*
 *	*gdarea		const char	User input for GDAREA		*
 *									*
 * Output parameters:							*
 *	*cprj		char		Grid projection			*
 *	*kx		int		Number of points in x dir	*
 *	*ky		int		Number of points in y dir	*
 *	*grdout		float		Grid corners			*
 *	*rnvblk		float		Grid navigation block		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = invalid navigation	*
 *					 -5 = invalid grid area		*
 *					 -6 = invalid grid size		*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93	Copied from GDCNAV			*
 * G. Krueger/EAI	 6/96	Add default projection			*
 * R. Tian/SAIC		 7/06	Recoded from Fortran			*
 * S. Gilbert/NCEP      10/06   Added call to GR_VNAV                   *
 ************************************************************************/
{
    char cdproj[31], tprj[31];
    float zmarg[4], rarr[2], rltln[4], angle[3], centrd[2],
          angle1, angle2, angle3, xspace, yspace;
    int angflg, space, valid;
    int iarr[2], len, num, ier, ier1, i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Translate grid projection information.
     */
    gg_proj ( proj, tprj, angle, zmarg, &angflg, &ier,
              strlen(proj), sizeof(tprj) );
    tprj[30] = '\0';
    cst_lstr ( tprj, &len, &ier1 );
    tprj[len] = '\0';
    strcpy ( cprj, tprj );
    angle1 = angle[0];
    angle2 = angle[1];
    angle3 = angle[2];

    /*
     * Check for error.
     */
    if ( ier != 0 ) {
	er_wmsg ( "GG", &ier, (char *)proj, &ier1,
	          strlen("GG"), strlen(proj) );
	*iret = -4;
	return;
    }

    /*
     * Translate grid area.
     */
    lc_gare ( (char *)gdarea, rltln, tprj, centrd, &ier,
              strlen(gdarea), sizeof(tprj) );
    tprj[30] = '\0';
    cst_lstr ( tprj, &len, &ier1 );
    tprj[len] = '\0';
    strcpy ( cdproj, tprj );
    if ( ier != 0 ) {
	*iret = -5;
	er_wmsg ( "NAGRIB", iret, (char *)gdarea, &ier,
	          strlen("NAGRIB"), strlen(gdarea) );
	return;
    }

    /*
     * Translate the input for KXKY.
     */
    if ( ( strcmp ( cprj, "CED" ) == 0 ) && ( kxky[0] == '#' ) ) {
	space = G_TRUE;
    } else {
	space = G_FALSE;
    }

    /*
     * Check for input as grid spacing.
     */
    if ( space == G_TRUE ) {
	/*
	 * Get x- and y- spacing and check that there are two numbers.
	 */
	cst_rlst  ( (char *)&kxky[1], ';', 0., 2, rarr, &num, &ier );
	if ( ( rarr[0] <= 0. ) || ( rarr[1] <= 0. ) ) {
	    *iret = -6;
	    er_wmsg ( "NAGRIB", iret, " ", &ier,
	              strlen("NAGRIB"), strlen(" ") );
	    return;
	} else {
	    xspace = rarr[0];
	    yspace = rarr[1];
	}

	/*
	 * Align on grid points and exit for error.
	 */
	grc_algn ( rltln, &xspace, &yspace, grdout, kx, ky, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "GR", iret, " ", &ier, strlen("GR"), strlen(" ") );
	    *iret = -4;
	    return;
	}

    /*
     * Otherwise, find kx, ky.
     */
    } else {
	for ( i = 0; i < 4; i++ ) {
	    grdout[i] = rltln[i];
	}

	/*
	 * Get two numbers and check for error.
	 */
	cst_ilst ( (char *)kxky, ';', 0, 2, iarr, &num, &ier );
	*kx = iarr[0];
	*ky = iarr[1];
	if ( ( *kx < 2 ) || ( *ky < 2 ) ) {
	    *iret = -6;
	    er_wmsg ( "NAGRIB", iret, " ", &ier,
	              strlen("NAGRIB"), strlen(" ") );
	    return;
	}
    }

    /*
     * Fill navigation block.
     */
    gr_vnav  ( cprj, kx, ky, &grdout[0], &grdout[1], &grdout[2], &grdout[3],
               &angle1, &angle2, &angle3, &angflg, &valid, &ier, strlen(cprj) );

    if ( ier == 0 )
       grc_mnav  ( cprj, kx, ky, &grdout[0], &grdout[1], &grdout[2], &grdout[3],
                   &angle1, &angle2, &angle3, &angflg, rnvblk, &ier );
    else
       *iret = -2;

    return;
}

#include "na.h"

void na_ganl ( const char *anlyss, const float *rnvblk, float *anlblk,
               int *iret )
/************************************************************************
 * na_ganl								*
 *									*
 * This subroutine takes the user input for ANLYSS (the analysis 	*
 * information) and makes a grid analysis block.  The grid navigation	*
 * is first set in GR_SNAV.						*
 *									*
 * na_ganl ( anlyss, rnvblk, anlblk, iret )				*
 *									*
 * Input parameters:							*
 *	*anlyss		const char	User input for ANLYSS		*
 *      *rnvblk		const float	Grid navigation block		*
 *									*
 * Output parameters:							*
 *	*anlblk		float		Grid analysis block		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *                                       -4 = invalid navigation	*
 *					-10 = invalid extend region	*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93		Copied from GDCANL		*
 * R. Tian/SAIC		 7/06		Recoded from Fortran		*
 ************************************************************************/
{
    char stusr[2][49], *cdp[2];
    float dbnds[4], deltan;
    int iebnds[4], n, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Separate user input into two substrings.
     */
    for ( n = 0; n < 2; n++ ) cdp[n] = stusr[n];
    cst_clst ( (char *)anlyss, '/', " ", 2, 48, cdp, &n, &ier );

    /*
     * Extract DELTAN from the first substring.
     */
    cst_crnm ( stusr[0], &deltan, &ier );

    /*
     * Extract extend region from the second substring.
     */
    cst_ilst ( stusr[1], ';', 2, 4, iebnds, &n, &ier );

    /*
     * Set default data area to missing.
     */
    for ( i = 0; i < 4; i++ ) {
	dbnds[i] = RMISSD;
    }

    /*
     * Make an analysis block.
     */
    grc_mbn2  ( &deltan, iebnds, dbnds, rnvblk, anlblk, &ier );

    return;
}

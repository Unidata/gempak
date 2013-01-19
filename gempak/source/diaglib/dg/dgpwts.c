#include "dg.h"

#define compar(xx,yy)		( G_ABS ( (xx) - (yy) ) < RDIFFD )

void dg_pwts ( const int *kx, const int *ky, int *polen, int *poles,
               int *wrapa, int *wrapc, int *iret )
/************************************************************************
 * dg_pwts                                                              *
 *                                                                      *
 * This subroutine tests to see if the grid includes a pole or if the   *
 * grid wraps horizontally. The WRAPA and WRAPC cases are mutually	*
 * exclusive. If either pole is present, then one of the wrap cases is	*
 * also true.								*
 *									*
 * dg_pwts ( kx, ky, polen, poles, wrapa, wrapc, iret )			*
 *									*
 * Input parameters:							*
 *	*kx		const int	Number of grid columns		*
 *	*ky		const int	Number of grid rows		*
 *									*
 * Output parameters:							*
 *	*polen		int		True if grid includes N. Pole	*
 *	*poles		int		True if grid includes S. Pole	*
 *	*wrapa		int		True if grid wraps, no overlap	*
 *	*wrapc		int		True if 1st grid column	is last	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = grid size is too large	*
 **									*
 * Log:									*
 * K. Brill/NMC		05/93						*
 * D. Keiser/GSC	11/95		Moved out of DG_HILO		*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 ************************************************************************/
{
    char gprj[5];
    float rglt[2], rgln[2], ag1, ag2, ag3, aglt1, agln1, aglt2, agln2;
    int mx, my, two, ier;
/*----------------------------------------------------------------------*/
    *iret  = 0;
    two = 2;
    *polen = G_FALSE;
    *poles = G_FALSE;
    *wrapa = G_FALSE;
    *wrapc = G_FALSE;

    if ( strcmp ( _dgfile.cprj, "MER" ) == 0 ||
         strcmp ( _dgfile.cprj, "CED" ) == 0 ) {
	/*
	 * Get information about the grid.
	 */
	gqgprj ( gprj, &ag1, &ag2, &ag3, &mx, &my, &aglt1, &agln1,
	         &aglt2, &agln2, &ier, sizeof(gprj) );
	if ( ier != 0 || ( (*kx) != mx || (*ky) != my ) ) {
	    *iret = -1;
	    return;
	}

	/*
	 * Check to see if first and last grid columns coincide.
	 */
	if ( compar ( agln1, agln2 ) ) {
	    *wrapc = G_TRUE;
	} else {
	    /*
	     * Check to see if the last and first grid columns are adjacent.
	     */
	    rglt[0] = 1.;
	    rglt[1] = (float)( mx + 1 );
	    rgln[0] = 1.;
	    rgln[1] = 1.;
	    gtrans ( sys_G, sys_M, &two, rglt, rgln, rglt, rgln, &ier,
	             strlen(sys_G), strlen(sys_M) );
	    if ( compar ( rgln[0], rgln[1] ) ) {
		*wrapa = G_TRUE;
	    }
	}

	/*
	 * Check for north pole on top grid row or south pole on
	 * bottom grid row.
	 */
	rglt[0] = 1.;
	rglt[1] = 1.;
	rgln[0] = (float)(*ky);
	rgln[1] = 1.;
	gtrans ( sys_G, sys_M, &two, rglt, rgln, rglt, rgln, &ier,
	         strlen(sys_G), strlen(sys_M) );
	if ( compar ( rglt[0], 90. ) &&
	     ( (*wrapa) == G_TRUE || (*wrapc) == G_TRUE ) ) {
	    *polen = G_TRUE;
	}
	if ( compar ( rglt[1], -90. ) &&
	     ( (*wrapa) == G_TRUE || (*wrapc) == G_TRUE ) ) {
	    *polen = G_TRUE;
	}
    }

    return;
}

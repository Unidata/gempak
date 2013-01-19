#include "dg.h"

void dg_cone ( const char *gprj, const float *angr1, const float *angr3,
	       float *ccone, int *iret )
/************************************************************************
 * dg_cone								*
 *									*
 * This subroutine computes the constant of the cone for a projection	*
 * whose name is given in GPRJ and whose first and third angles are 	*
 * ANGR1 and ANGR3.							*
 *									*
 * dg_cone ( gprj, angr1, angr3, ccone, iret )				*
 *									*
 * Input parameters:							*
 *	*gprj		const char	Name of projection		*
 *	*angr1		const float	Angle 1 in radians		*
 *	*angr3		const float	Angle 3 in radians		*
 *									*
 * Output parameters:							*
 *	*ccone		float		Constant of the Cone		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC         03/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float cona, conb, conc;
    int sgn;
/*----------------------------------------------------------------------*/
    *iret   = 0;
    *ccone = 0.0;

    if ( strcmp ( gprj, "STR" ) == 0 ) {
	/*
	 * Polar stereographic.
	 */
	*ccone = 1.;
    } else if ( strcmp ( gprj, "LCC" ) == 0 || 
                strcmp ( gprj, "SCC" ) == 0 ) {
	/*
	 * Lambert conic conformal; ANGLE1, ANGLE3 are the standard
	 * latitudes, ANGLE2 is POLON.
	 */
	if ( G_DIFF(*angr1, *angr3) ) {
	    /* 
	     * Standard latitudes equal.
	     */
	    sgn = (*angr1) >= 0. ? 1 : -1;
	    cona = HALFPI - sgn * (*angr1);
	    *ccone = (float)cos ( cona );
	} else {
	    /*
	     * Standard lats not equal; use negative of standard
	     * lats in south polar projection.
	     */
	    cona = log ( cos ( *angr1 ) ) - log ( cos ( *angr3 ) );
	    if ( strcmp ( gprj, "LCC" ) == 0 ) {
		conb = log ( tan ( PI4TH - (*angr1) / 2.0 ) );
		conc = log ( tan ( PI4TH - (*angr3) / 2.0 ) );
	    } else {
		conb = log ( tan ( PI4TH + (*angr1) / 2.0 ) );
		conc = log ( tan ( PI4TH + (*angr3) / 2.0 ) );
	    }
	    *ccone = cona / ( conb - conc );
	}
    }

    return;
}

#include "nagrib.h"

void nagcut ( const float *sg, const int *ixf, const int *iyf,
              const char *parm, float *sgo, int *iret )
/************************************************************************
 * nagcut								*
 *									*
 * This subroutine removes the padded values on the right side of a	*
 * staggered eta grid labeled 203 in GDS octet 6.			*
 *									*
 * The input and output grids may be the same!				*
 *									*
 * nagcut ( sg, ixf, iyf, parm, sgo, iret )				*
 *									*
 * Input parameters:							*
 *	*sg		const float	Staggered grid			*
 *	*ixf		const int	Filled grid x dimension		*
 *	*iyf		const int	Filled grid y dimension		*
 *	*parm		const char	Parameter name			*
 *									*
 * Output parameters:							*
 *	*sgo		float		Staggered grid w/o pad values	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		 9/98						*
 * R. Tian/SAIC		 8/06		Recoded from Fortran		*
 ************************************************************************/
{
    int ixs, ngmx, ii, imod, imod1, imod2, ig, mod1, mod2;
    int vgrd;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check for wind component.
     */
    if ( strcmp ( parm, "UREL" ) == 0 || strcmp ( parm, "VREL" ) == 0 ) {
	vgrd = G_TRUE;
    } else {
	vgrd = G_FALSE;
    }

    /*
     * Move data to output array.
     */
    ixs = (*ixf) / 2 + 1;
    ngmx = ixs * (*iyf);
    if ( vgrd == G_TRUE ) {
	ii = 0;
	imod1 = ixs;
	imod2 = ixs * 2;
	for ( ig = 1; ig <=ngmx; ig++ ) {
	    mod1 = ig % imod1;
	    mod2 = ig % imod2;
	    if ( mod1 != 0 || mod2 == 0 ) {
		sgo[ii++] = sg[ig-1];
	    }
	}
    } else {
	ii = 0;
	imod = ixs * 2;
	for ( ig = 1; ig <= ngmx; ig++ ) {
	    if ( ( ig % imod ) != 0 ) {
		sgo[ii++] = sg[ig-1];
	    }
	}
    }

    return;
}

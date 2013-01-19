#include "dg.h"

void dg_trig ( float *ugrd, float *vgrd, int *iret )
/************************************************************************
 * dg_trig                                                              *
 *                                                                      *
 * This subroutine converts transfer grid-relative vector components    *
 * into internal grid-relative components.				*
 *                                                                      *
 * dg_trig ( ugrd, vgrd, iret )                            		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *ugrd		float		Grid relative u-component   	*
 *      *vgrd		float		Grid relative v-component   	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          5/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( _hintrp.didint == G_TRUE && _hintrp.wndrot == G_TRUE ) {
	/*
	 * Rotate transfer grid relative components to north relative
	 * components.
	 */
	dg_t2nr ( ugrd, vgrd, ugrd, vgrd, iret );

	/*
	 * Rotate north relative components to internal grid relative
	 * components.
	 */
	dg_grel ( ugrd, vgrd, ugrd, vgrd, iret );
    }

    return;
}

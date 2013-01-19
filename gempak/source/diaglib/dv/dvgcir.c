#include "dv.h"

void dv_gcir ( int *iret )
/************************************************************************
 * dv_gcir								*
 *									*
 * This routine computes, for every gridpoint on a grid, a unit vector, *
 * in the direction away from a point of interest and toward its	*
 * antipodal point, and along a great circle arc.			*
 *									*
 * dv_gcir ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 2/09						*
 ************************************************************************/
{
        int             i, ier, kxd, kyd, ksub1, ksub2, zero=0;
	int		iazst, numu, numv, ilat, ilon;
        float           *grazst, *gru, *grv;
        float           mag = 1.0F;

/*---------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         * Check if the navigation parameters have been computed.
         */
	dg_ltln ( iret );
	if ( *iret != 0 ) return;

        /*
         * Get grids numbers for user input, which is the
	 * point of interest.
         */
	dg_gets ( &ilat, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &ilon, iret );
	if ( *iret != 0 ) return;

        /*
         * Get new grid numbers for the new grids.
         */
	dg_nxts  ( &iazst, iret );
	if ( *iret != 0 ) return;
	dg_nxtv  ( &numu, &numv, iret );
	if ( *iret != 0 ) return;

        /*
         * Calculate the azimuth angle at each grid point 
         * measured from north to the great circle arc.
         */
        dg_azst ( &ilat, &ilon, &iazst, iret );
	if ( *iret != 0 ) return;

        /*
         * Get the output vector component and angle (direction)	
         * grids so that we can work with them.
         */
        dg_getg ( &iazst, &grazst, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );


        /*
         * Compute the components of the unit vectors that we need.
         */
	for ( i = ksub1 - 1; i < ksub2; i++ ) {
           gru[i] = -sin ( grazst[i] ) * mag; 
           grv[i] = -cos ( grazst[i] ) * mag;    
	}

        /*
         * Return them as a vector.
	 * Make a name of the form 'GCIR'//S1//S2
         * and update the headers; update the stack.
         */
        dg_updv ( "GCIR", &numu, &numv, &numu, &numv, iret );
        dg_putv ( &numu, &numv, iret );
        dg_esub ( &numu, &numv, &zero, &zero, &ier );

	if ( ier != 0 ) *iret = ier;

	return;
}

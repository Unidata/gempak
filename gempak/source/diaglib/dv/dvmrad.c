#include "dv.h"

void dv_mrad ( int *iret )
/************************************************************************
 * dv_mrad								*
 *									*
 * This routine computes the magnitude of the radial component of	*
 * the wind.  A unit vector between the center of the storm and the 	*
 * grid point vector is determined using oblique spherical triangles.	*
 * The radial component of the wind is determined using the equation:	*
 *		RAD =   V dot r.					*
 * Inflow to the storm is set to be positive and outflow is set to be	*
 * negative.								*
 * Wind is set to 0 when grid point = storm point, using flagged value  *
 * set in DG_AZST subroutine.                                           *
 *									*
 * dv_mrad ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 6/91	Modified DV_RAD				*
 * K. Tyle/GSC		 9/95	Declared level (2) as integer		*
 * K. Tyle/GSC		 9/95	Set radial wind = 0 at storm point	*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, ier, kxd, kyd, kxyd, ksub1, ksub2, zero=0;
   
	int		numu, numv, ilat, ilon, numout;
        float           *gru, *grv, *grout;
        int             ix, iy, iazst, idir, ispd;
        float           *grx, *gry, *grazst, *grdir, *grspd;

	char	        grid[13], parm[13], time1[21], time2[21];
	int		level1, level2, ignum, ivcord;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the information from the top of the stack.
         */
	dg_tops ( grid, &ignum, time1, time2, &level1, &level2, 
                         &ivcord, parm, iret );

        /*
         *	Get the (wind) vector.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get scalar grids.
         */
	dg_gets ( &ilat, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &ilon, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &idir, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &ispd, iret );
	if ( *iret != 0 ) return;

        /*
         *	Get a new grid number.
         */
	dg_nxts  ( &ix, iret );
	if ( *iret != 0 ) return;
	dg_nxts  ( &iy, iret );
	if ( *iret != 0 ) return;
	dg_nxts  ( &numout, iret );
	if ( *iret != 0 ) return;

        /*
         *	Set the latitude and longitude of the grid points.
         */
	dg_ltln ( iret );

        /*
         * 	Calculate the azimuth angle between the storm and the grid
         *	points.
         */
	dg_azst ( &ilat, &ilon, &iazst, iret );
	if ( *iret != 0 ) return;

        /*
         *	Calculate the x and y components of the directional unit
         *	vector.
         */
        dg_getg ( &ix, &grx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &iy, &gry, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &iazst, &grazst, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grazst[i] ) )  {
		grx[i] = RMISSD;
		gry[i] = RMISSD;
            }

            /*
             *	    Set wind = 0 at storm point.
             */
	    else if ( grazst[i] > ( 2. * PI ) ) {
		grx[i] = 0.0;
		gry[i] = 0.0;
            }
	    else {
		grx[i] = sin ( grazst[i] );
		gry[i] = cos ( grazst[i] );
	    }
	}

        /*
         *	Change the x and y components of the unit vector from 
         *	North relative to Grid relative.
         */
 	dg_grel (  grx, gry, grx, gry, iret );
 	if ( *iret != 0 ) return;

        /*
         *	Compute u and v grid fields for the storm motion then subtract
         *	the storm motion from the wind field.
         */
        dg_getg ( &idir, &grdir, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ispd, &grspd, &kxd, &kyd, &ksub1, &ksub2, iret );

	kxyd = kxd * kyd;
	pd_sduv  ( grspd, grdir, &kxyd, grspd, grdir, iret );
 	if ( *iret != 0 ) return;

        /*
         *	Change storm motion components to grid relative.
         */
 	dg_grel (  grspd, grdir, grspd, grdir, iret );
 	if ( *iret != 0 ) return;

        /*
         *	Subtracting u-component.
         */
	dg_puts ( &ispd, iret );
	if ( *iret != 0 ) return;
	dg_puts ( &numu, iret );
	if ( *iret != 0 ) return;
	df_sub ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &numu, iret );
	if ( *iret != 0 ) return;

        /*
         *	Subtracting v-component.
         */
	dg_puts ( &idir, iret );
	if ( *iret != 0 ) return;
	dg_puts ( &numv, iret );
	if ( *iret != 0 ) return;
	df_sub ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &numv, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute the u and v components of the radial wind.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grx[i] ) || ERMISS ( gry[i] ) ||
     	         ERMISS ( gru[i] ) || ERMISS ( grv[i] ) )
		grout[i] = RMISSD;
	    else

		 grout[i] = ( gru[i] * grx[i] ) + ( grv[i] * gry[i] );
	    
	}
	

        /*
         *	Update grid header.  Use wind type as parameter name.
         */
	dg_updh  ( "MRAD", &numout, &numu, &zero, &ier );

        /*
         *	Update stack.
         */
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}

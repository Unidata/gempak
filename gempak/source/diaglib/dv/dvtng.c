#include "dv.h"

void dv_tng ( int *iret )
/************************************************************************
 * dv_tng								*
 *									*
 * This routine computes the tangential component of the wind.  A unit	*
 * vector is determined using oblique spherical triangles.  Once the	*
 * x and y components of the unit vectors are computed the tangential	*
 * components of the wind are determined using the equation:		*
 *		TNG =  V dot ( k x r ).					*
 * Wind is set to 0 when grid point = storm point, using flagged value  *
 * set in DG_AZST subroutine.                                           *
 *									*
 * dv_tng ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 5/91						*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * K. Tyle/GSC		 9/95	Declared level(2) as integer		*
 * K. Tyle/GSC		 9/95	Set tangential wind = 0 at storm point	*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0; 
	int    		i, ier, nval, kxd, kyd, ksub1, ksub2, kxyd, idlun;

        int             numu, numv;
        float           *gru, *grv;
        int             ilat, ilon, idir, ispd;
        float           *grdir, *grspd;
        int             ix, iy, nvecu, nvecv, iazst;
        float           *grx, *gry, *grvecu, *grvecv, *grazst;

	char    	grid[13], parm[13], time1[21], time2[21];
	int		level1, level2, ignum, ivcord;
        float           vdotr;

/*----------------------------------------------------------------------*/
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
	dg_nxts  ( &nvecu, iret );
	if ( *iret != 0 ) return;
	dg_nxts  ( &nvecv, iret );
	if ( *iret != 0 ) return;

        /*
         *	Get the latitude and longitude of the grid points.
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
            if ( ERMISS ( grazst[i] ) ) {
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
		grx[i] = - cos ( grazst[i] );
		gry[i] =   sin ( grazst[i] );
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
        dg_getg ( &ispd, &grspd, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &idir, &grdir, &kxd, &kyd, &ksub1, &ksub2, iret );

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
         *	Compute the u and v components of the tangential wind.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvecu, &grvecu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvecv, &grvecv, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grx[i] )  || ERMISS ( gry[i] )  || 
     	         ERMISS ( gru[i] )  || ERMISS ( grv[i] ) )  {
	       grvecu[i] = RMISSD;
	       grvecv[i] = RMISSD;
            }
	    else {

		vdotr = ( gru[i] * grx[i] ) + ( grv[i] * gry[i] );
	        grvecu[i] = grx[i] * vdotr;
	        grvecv[i] = gry[i] * vdotr;
            }
	}

        /*
         *	Update both grid headers.  Use wind type as parameter name.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, &ier );
	dg_upvg ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                         "TNG", &nvecu, &nvecv, &ier );

        /*
         *	Update stack.
         */
	dg_putv  ( &nvecu, &nvecv, iret );
	dg_esub  ( &nvecu, &nvecv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}

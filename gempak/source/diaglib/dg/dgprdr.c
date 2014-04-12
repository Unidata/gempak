#include "dg.h"

void dg_prdr ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_prdr								*
 *									*
 * This subroutine reads a precipitation grid from a grid file.  If	*
 * the requested precipitation grid does not exist, an attempt will	*
 * be made to compute it from existing precipitation grids.  There	*
 * are three types of precipitation as summarized in the table:		*
 *									*
 *  Type	Initial Character in PARM	Relation to Others	*
 *									*
 *  Total		P			S + C			*
 *  Stable		S			P - C			*
 *  Convective		C			P - S			*
 *									*
 * The output precipitation grid is place into an internal grid array	*
 * which is in the common area.  The grid will be found in the grid	*
 * location to which NUM points.  If NUM = 0 on input, the next grid	*
 * location will be used and returned.  If NUM > 0, the grid will be	*
 * found at NUM.							*
 *									*
 * dg_prdr ( time1, time2, level1, level2, ivcord, parm, num, iret )    *
 *									*
 * Input parameters:							*
 *      *time1          const char      Date/time                       *
 *      *time2          const char      Date/time                       *
 *      *level1         const int       Level                           *
 *      *level2         const int       Level                           *
 *      *ivcord         const int       Vertical coordinate             *
 *      *parm           const char      Parameter name                  *
 *									*
 * Input and output parameters:						*
 *      *num            int             Location of grid                *
 *									*
 * Output parameters:							*
 *      *iret           int             Return code                     *
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 **									*
 * Log:									*
 * K. Brill/NMC		 2/93   Created from DG_GRDR			*
 * M. desJardins/NMC	 7/93	DG_UHDR --> DG_UPSG			*
 * S. Jacobs/EAI	11/93	Changed GD_RDAT to DG_RDAT		*
 * M. desJardins/NMC	 3/94	Call DG_GRDR, PD_PRCP			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC          2/06   Recoded from Fortran                    *
 * K. Brill/WPC		04/13   Return with -7 if not P, S, or C as	*
 *                              the first character			*
 ************************************************************************/
{
    char prm[14];
    float signp;
    int nl, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Try to read in the grid data.
     */
    dg_grdr ( time1, time2, level1, level2, ivcord, parm, num, iret );
    if ( *iret == 0 ) return;

    /*
     * Check for other names from which to compute the precipitation.
     */
    strcpy( prm, parm );
    if ( parm[0] == 'P' ) {
	prm[0] = 'S';
	dg_nxts ( &nl, &ier );
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, &nl, &ier );
	if ( ier != 0 ) {
	    dg_frig ( &nl, &ier );
	    return;
	}
	prm[0] = 'C';
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, num, &ier );
	if ( ier != 0 ) return;
	signp = 1.;
    } else if ( parm[0] == 'S' ) {
	prm[0] = 'P';
	dg_nxts ( &nl, &ier );
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, &nl, &ier );
	if ( ier != 0 ) {
	    dg_frig ( &nl, &ier );
	    return;
	}
	prm[0] = 'C';
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, num, &ier );
	if ( ier != 0 ) return;
	signp = -1.;
    } else if ( parm[0] == 'C' ) {
	prm[0] = 'P';
	dg_nxts ( &nl, &ier );
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, &nl, &ier );
	if ( ier != 0 ) {
	    dg_frig ( &nl, &ier );
	    return;
	}
	prm[0] = 'S';
	ier = -7;
	dg_grdr ( time1, time2, level1, level2, ivcord, prm, num, &ier );
	if ( ier != 0 ) return;
	signp = -1.;
    } else {
	/*
	 * This must be a precipitation type accumulation beginning with
	 * W, I, Z, A, H, G, N, R, or L; therefore, return with -7.
	 *
	 */
	*iret = -7;
	return;
    }

    /*
     * Add or subtract the precipitation amounts.
     */
    pd_prcp ( _dggrid.dgg[nl-1].grid, _dggrid.dgg[(*num)-1].grid,
	&signp, &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, iret );
    dg_frig ( &nl, &ier );

    return;
}

#include "dg.h"

void dg_mxnt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_mxnt								*
 *									*
 * This subroutine checks for alternate ways to compute mixing		*
 * ratio that don't involve dewpoint temperature.  The grid will be	*
 * found in the grid location pointed to by NUM.  If NUM = 0 on input,	*
 * the next grid location will be used and returned.  If NUM > 0, the	*
 * grid will be found at NUM.						*
 *									*
 * dg_mxnt ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
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
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 5/91						*
 * M. desJardins/NMC	 3/92	Clean up; DG_VCRD instead of DG_PRES	*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char p[5];
    int np, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check for the grid.
     */
    dg_grdr ( time1, time2, level1, level2, ivcord, parm, num, iret );
    if ( *iret == 0 ) return;

    /*
     * Check for alternate name.
     */
    if ( strcmp ( parm, "MIXR" ) == 0 ) {
	strcpy ( p, "RMIX" );
    } else if ( strcmp ( parm, "RMIX" ) == 0 ) {
	strcpy ( p, "MIXR" );
    } else if ( strcmp ( parm, "MIXS" ) == 0 ) {
	strcpy ( p, "SMIX" );
    } else if ( strcmp ( parm, "SMIX" ) == 0 ) {
	strcpy ( p, "MIXS" );
    } else {
	return;
    }
    dg_grdr ( time1, time2, level1, level2, ivcord, p, num, iret );
    if ( *iret == 0 ) return;

    /*
     * If the grid was not found, try to compute MIXR or RMIX from other
     * parameters.
     */
    if ( ( strcmp ( parm, "MIXR" ) == 0 ) ||
         ( strcmp ( parm, "RMIX" ) == 0 ) ) {
	/*
	 * Try the specific humidity.
	 */
	strcpy ( p, "SPFH" );
        dg_grdr ( time1, time2, level1, level2, ivcord, p, num, iret );
	if ( *iret == 0 ) {
	    pd_shmr ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		_dggrid.dgg[(*num)-1].grid, &ier );
	}

	/*
	 * Try the vapor pressure.
	 */
	if ( *iret != 0 ) {
	    strcpy ( p, "PRES" );
	    dg_nxts ( &np, &ier );
	    dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "VAPR" );
		dg_grdr ( time1, time2, level1, level2, ivcord, p, num, iret );
		if ( *iret == 0 ) {
		    pd_vpmr ( _dggrid.dgg[(*num)-1].grid, 
			      _dggrid.dgg[np-1].grid, &_dgfile.kxyd,
			      _dggrid.dgg[(*num)-1].grid, &ier );
		}
	    }
	    dg_frig ( &np, &ier );
	}
    }

    return;
}

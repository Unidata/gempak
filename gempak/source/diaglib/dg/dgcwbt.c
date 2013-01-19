#include "dg.h"

void dg_cwbt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_cwbt								*
 *									*
 * This subroutine computes the wetbulb temperature.  The grid will be	*
 * found in the grid location pointed to by NUM.  If NUM = 0 on input,	*
 * the next grid location will be used and returned.  If NUM > 0, the	*
 * grid will be found at NUM.						*
 *									*
 * dg_cwbt ( time1, time2, level1, level2, ivcord, parm, num, iret )    *
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
 *					 -8 = grid is the wrong size	*
 **									*
 * Log:									*
 * L. Williams/EAI	 8/94	Modified from DG_DWPT			*
 * S. Chiswell/Unidata	 1/96	Fixed call to DG_MXNT			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Lee/GSC		11/96	Switched parameter order in PD_TMWB	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC          2/06   Recoded from Fortran			*
 * S. Jacobs/NCEP	 3/10	Added error checks when collecting grids*
 ************************************************************************/
{
    char ppp[5], p[5];
    int np, nmr, ntd, lenp, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check that the input string length is 4.
     */
    cst_lstr ( (char *)parm, &lenp, &ier );
    if ( lenp != 4 ) return;

    /*
     * Check that this is wetbulb.
     */
    if ( ( strcmp ( parm, "TMWK" ) == 0 ) ||
         ( strcmp ( parm, "TMWC" ) == 0 ) ||
	 ( strcmp ( parm, "TMWF" ) == 0 ) ) {
	strcpy ( ppp, parm );
    } else {
	return;
    }

    /*
     * Check file for grid stored in K, C or F.
     */
    dg_gtmp ( time1, time2, level1, level2, ivcord, ppp, num, iret );
    if ( *iret == 0 ) return;

    /*
     * Compute the wetbulb temperature. 
     * 
     * Get pressure
     */
    strcpy ( p, "PRES" );
    dg_nxts ( &np, &ier );
    dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
    if ( *iret != 0 ) return;

    /*
     * Get Kelvin Temperature
     */
    strcpy ( p, "TMPK" );
    dg_temp ( time1, time2, level1, level2, ivcord, p, num, iret );
    if ( *iret != 0 ) return;

    /*
     * Get mixing ratio
     */
    strcpy ( p, "RMIX" );
    dg_nxts ( &nmr, &ier );
    dg_mxnt ( time1, time2, level1, level2, ivcord, p, &nmr, iret );
    if ( *iret != 0 ) {
	strcpy ( p, "DWPC" );
	dg_nxts ( &ntd, &ier );
	dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	if ( *iret == 0 ) {
	    pd_mixr ( _dggrid.dgg[ntd-1].grid, _dggrid.dgg[np-1].grid,
	        &_dgfile.kxyd, _dggrid.dgg[nmr-1].grid, &ier );
	}
	dg_frig ( &ntd, &ier );
    }
    if ( *iret != 0 ) return;
 
    /*
     * Compute the wetbulb temperature from Kelvin temperature, 
     * mixing ratio and pressure.
     */
    pd_tmwb ( _dggrid.dgg[np-1].grid, _dggrid.dgg[(*num)-1].grid, 
	      _dggrid.dgg[nmr-1].grid, &_dgfile.kxyd,
	      _dggrid.dgg[(*num)-1].grid, iret );

    /*
     * Convert temperatures in one set of degrees to another set of
     * degrees.
     */
    dg_cdeg ( "TMWK", parm, num, iret );

    /*
     * Put correct name in label.
     */
    dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
        parm, num, &ier );

    dg_frig ( &np, &ier );
    dg_frig ( &nmr, &ier );

    return;
}

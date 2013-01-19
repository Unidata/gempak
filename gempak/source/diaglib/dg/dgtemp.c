#include "dg.h"

void dg_temp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_temp								*
 *									*
 * This subroutine checks for alternate ways to compute temperature.	*
 * The grid will be found in the grid location pointed to by NUM.  If 	*
 * NUM = 0 on input, the next grid location will be used and returned.	*
 * If NUM > 0, the grid will be found at NUM.				*
 *									*
 * dg_temp ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
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
 *	*num		int		Location of grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/90						*
 * K. Brill/NMC          9/90	Fix to get DWPC label on rtrn		*
 * J. Whistler/SSAI	 5/91	Added DG_MXNT to find MIXR		*
 * K. Brill/NMC		12/91	Reset SCALE for DWPC from MIXR or MIXS	*
 * M. desJardins/NMC	 3/92	Reorganize; eliminate DWPx here		*
 * M. desJardins/NMC	 3/93	Update file number			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char ppp[5], ptmp[5];
    int np, nm, lenp, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check that the input string length is 4.
     */
    cst_lstr ( (char *)parm, &lenp, &ier );
    if ( lenp != 4 ) return;

    /*
     * Check that this is temperature.
     */
    if ( ( strcmp ( parm, "TEMP" ) == 0 ) ||
         ( strcmp ( parm, "TMPC" ) == 0 ) ||
	 ( strcmp ( parm, "TMPK" ) == 0 ) ||
	 ( strcmp ( parm, "TMPF" ) == 0 ) ) {
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
     * Try alternate methods for temperature.
     * Compute temperature from THTA and PRES.
     */
    strcpy ( ptmp, "THTA" );
    dg_gtmp ( time1, time2, level1, level2, ivcord, ptmp, num, iret );
    if ( *iret == 0 ) {
	strcpy ( ptmp, "PRES" );
	dg_nxts ( &np, &ier );
	dg_vcrd ( time1, time2, level1, level2, ivcord, ptmp, &np, iret );
	if ( *iret == 0 ) {
	    pd_tmpk ( _dggrid.dgg[np-1].grid, _dggrid.dgg[(*num)-1].grid,
	        &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	    dg_cdeg ( "TMPK", parm, num, iret );
	}
	dg_frig ( &np, &ier );
    }

    /*
     * Compute temperature from MIXS and PRES.
     */
    if ( *iret != 0 ) {
	strcpy ( ptmp, "MIXS" );
	dg_nxts ( &nm, &ier );
	dg_mxnt ( time1, time2, level1, level2, ivcord, ptmp, &nm, iret );
	if ( *iret == 0 ) {
	    strcpy ( ptmp, "PRES" );
	    dg_vcrd ( time1, time2, level1, level2, ivcord, ptmp, num, iret );
	    if ( *iret == 0 ) {
		pd_dwpt ( _dggrid.dgg[nm-1].grid, _dggrid.dgg[(*num)-1].grid,
		    &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		dg_cdeg ( "TMPC", parm, num, iret );
	    }
	}
	dg_frig ( &nm, &ier );
    }

    /*
     * Exit if the temperature was not found.
     */
    if ( *iret != 0 ) return;

    /*
     * Put correct name in label.
     */
    dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
              parm, num, &ier );

    return;
}

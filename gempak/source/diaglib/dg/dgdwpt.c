#include "dg.h"

void dg_dwpt ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_dwpt								*
 *									*
 * This subroutine checks for alternate ways to compute dewpoint	*
 * temperature.  The grid will be found in the grid location pointed	*
 * to by NUM.  If NUM = 0 on input, the next grid location will be	*
 * used and returned.  If NUM > 0, the grid will be found at NUM.	*
 *									*
 * dg_dwpt ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
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
 * M. desJardins/NMC	 3/92	Taken from old DG_TEMP; clean up	*
 * M. desJardins/NMC	 7/93	Update file number			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC          2/06   Recoded from Fortran			*
 ************************************************************************/
{
    char ppp[5], ptmp[5];
    int nr, nm, lenp, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check that the input string length is 4.
     */
    cst_lstr ( (char *)parm, &lenp, &ier );
    if ( lenp != 4 ) return;

    /*
     * Check that this is dewpoint.
     */
    if ( ( strcmp ( parm, "DWPT" ) == 0 ) ||
	 ( strcmp ( parm, "DWPK" ) == 0 ) ||
	 ( strcmp ( parm, "DWPC" ) == 0 ) ||
	 ( strcmp ( parm, "DWPF" ) == 0 ) ) {
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
     * Try alternate methods to compute dewpoint.
     * Get dewpoint from relative humidity.
     */
    strcpy ( ptmp, "TMPC" );
    dg_temp ( time1, time2, level1, level2, ivcord, ptmp, num, iret );
    if ( *iret == 0 ) {
	strcpy ( ptmp, "RELH" );
	dg_nxts ( &nr, &ier );
	dg_grdr ( time1, time2, level1, level2, ivcord, ptmp, &nr, iret );
	if ( *iret == 0 ) {
	    pd_rhdp ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[nr-1].grid,
	        &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	    dg_cdeg ( "DWPC", parm, num, iret );
	}
	dg_frig ( &nr, &ier );
    }

    /*
     * Get dewpoint from mixing ratio.
     */
    if ( *iret != 0 ) {
	strcpy ( ptmp, "MIXR" );
	dg_nxts ( &nm, &ier );
	dg_mxnt ( time1, time2, level1, level2, ivcord, ptmp, &nm, iret );
	if ( *iret == 0 ) {
	    strcpy ( ptmp, "PRES" );
	    dg_vcrd (time1, time2, level1, level2, ivcord, ptmp, num, iret);
	    if ( *iret == 0 ) {
		pd_dwpt ( _dggrid.dgg[nm-1].grid, _dggrid.dgg[(*num)-1].grid,
		    &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		dg_cdeg ( "DWPC", parm, num, &ier );
	    }
	}
	dg_frig ( &nm, &ier );
    }

    /*
     * Exit if the dewpoint was not found.
     */
    if ( *iret != 0 ) return;

    /*
     * Put correct name in label.
     */
    dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
        parm, num, &ier );

    return;
}

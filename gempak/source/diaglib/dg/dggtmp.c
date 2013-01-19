#include "dg.h"

void dg_gtmp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_gtmp								*
 *									*
 * This subroutine checks for temperature parameters in units other	*
 * than those requested.  The grid will be found in the grid location	*
 * pointed to by NUM.  If NUM = 0 on input, the next grid location	*
 * will be used and returned.  If NUM > 0, the grid will be found	*
 * at NUM.  Temperatures include temperature (TMP), dewpoint (DWP),	*
 * maximum temperature (TMX), minimum temperature (TMN), sea surface	*
 * temperature (SST), and virtual temperature (TVR). 			*
 *									*
 * dg_gtmp ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
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
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 4/89	Fixed truncation to 4 chars		*
 * M. desJardins/GSFC	 7/89	Replaced PR calls with PA		*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * K. Brill/GSC		 9/89   IFILED					*
 * M. desJardins/GSFC	 2/90	Renamed from DG_TEMP			*
 * M. desJardins/NMC	 3/92	Generalize to translate units for temps	*
 * M. desJardins/NMC	 7/93	DG_UHDR --> DG_UPSG			*
 * L. Williams/EAI	 8/94	Added 'TMW' to temperature field check	*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Piper/SAIC	 2/02	Increased c3 to 4 due to UMR error	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	 2/07	Replaced dg_grdr with dg_vcrd           *
 ************************************************************************/
{
    char ppp[5], c1, c3[4], f[3][2] = { "K", "C", "F" };
    int lenp, knt, itin, itout, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check that the input string length is 4.
     */
    cst_lstr ( (char *)parm, &lenp, &ier );
    if ( lenp != 4 ) return;

    /*
     * Translate special names into names with K, C, or F.
     */
    if ( strcmp ( parm, "TEMP" ) == 0 ) {
	strcpy ( ppp, "TMPC" );
    } else if ( strcmp ( parm, "DWPT" ) == 0 ) {
	strcpy ( ppp, "DWPC" );
    } else if ( strcmp ( parm, "THTA" ) == 0 ) {
	strcpy ( ppp, "THTK" );
    } else if ( strcmp ( parm, "STHA" ) == 0 ) {
	strcpy ( ppp, "STHK" );
    } else if ( strcmp ( parm, "TMAX" ) == 0 ) {
	strcpy ( ppp, "TMXC" );
    } else if ( strcmp ( parm, "TMIN" ) == 0 ) {
	strcpy ( ppp, "TMNC" );
    } else {
	strcpy ( ppp, parm );
    }

    /*
     *Check that this is a temperature field.
     */
    strncpy ( c3, ppp, 3 );
    c3[3] = '\0';
    c1 = ppp[3];
    if ( ( strcmp ( c3, "TMP" ) != 0 ) && ( strcmp ( c3, "DWP" ) != 0 ) &&
	 ( strcmp ( c3, "THT" ) != 0 ) && ( strcmp ( c3, "STH" ) != 0 ) &&
	 ( strcmp ( c3, "SST" ) != 0 ) && ( strcmp ( c3, "TMX" ) != 0 ) &&
	 ( strcmp ( c3, "TMN" ) != 0 ) && ( strcmp ( c3, "TVR" ) != 0 ) &&
	 ( strcmp ( c3, "TMW" ) ) ) return;

    /*
     * Check for valid temperature units requested.
     */
    if ( c1 == 'K' ) {
	itout = 1;
    } else if ( c1 == 'C' ) {
	itout = 2;
    } else if  ( c1 == 'F' ) {
	itout = 3;
    } else {
	return;
    }

    /*
     * Check file for grid stored in K, C or F.
     */
    knt = 1;
    while ( ( *iret != 0 ) && ( knt <= 4 ) ) {
	/*
	 * Get name of parameter to check.
	 */
	if ( knt <= 3 ) {
	    strcpy ( ppp, c3 );
	    strcat ( ppp, f[knt-1] );
	} else if ( strcmp ( c3, "TMP" ) == 0 ) {
	    strcpy ( ppp, "TEMP" );
	    itin = 2;
	} else if  ( strcmp ( c3, "DWP" ) == 0 ) {
	    strcpy ( ppp, "DWPT" );
	    itin = 2;
	} else if  ( strcmp ( c3, "THT" ) == 0 ) {
	    strcpy ( ppp, "THTA" );
	    itin = 1;
	} else if  ( strcmp ( c3, "STH" ) == 0 ) {
	    strcpy ( ppp, "STHA" );
	    itin = 1;
	} else if  ( strcmp ( c3, "TMX" ) == 0 ) {
	    strcpy ( ppp, "TMAX" );
	    itin = 2;
	} else if  ( strcmp ( c3, "TMN" ) == 0 ) {
	    strcpy ( ppp, "TMIN" );
	    itin = 2;
	}

	/*
	 * Check for parameter in file.
	 */
	dg_vcrd ( time1, time2, level1, level2, ivcord, ppp, num, iret );

	/*
	 * Save information on type.
	 */
	if ( ( *iret == 0 ) && ( knt <= 3 ) ) itin = knt;

	/*
	 * Increment knt.
	 */
	knt++;
    }

    /*
     * Exit if thermodynamic field was not found.
     */
    if ( *iret != 0 ) {
	return;
    /*
     * If input and output types are the same, return.
     */
    } else if  ( itout == itin ) {
	return;
    /*
     * Otherwise, compute correct units.
     */
    } else if ( ( itin == 1 ) && ( itout == 2 ) ) {
	pd_tmkc ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier);
    } else if ( ( itin == 1 ) && ( itout == 3 ) ) {
	pd_tmkf ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier);
    } else if ( ( itin == 2 ) && ( itout == 1 ) ) {
	pd_tmck ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
	          _dggrid.dgg[(*num)-1].grid, &ier);
    } else if ( ( itin == 2 ) && ( itout == 3 ) ) {
	pd_tmcf ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier);
    } else if ( ( itin == 3 ) && ( itout == 1 ) ) {
	pd_tmfk ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier);
    } else if ( ( itin == 3 ) && ( itout == 2 ) ) {
	pd_tmfc ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
	 	  _dggrid.dgg[(*num)-1].grid, &ier);
    }

    /*
     * Put correct name in label.
     */
    dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
              parm, num, &ier );

    return;
}

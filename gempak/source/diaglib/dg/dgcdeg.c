#include "dg.h"

void dg_cdeg ( const char *prmin, const char *prmout, const int *num,
               int *iret )
/************************************************************************
 * dg_cdeg								*
 *									*
 * This subroutine converts temperatures in one set of degrees to	*
 * another set of degrees.						*
 *									*
 * dg_cdeg ( prmin, prmout, num, iret )					*
 *									*
 * Input parameters:							*
 *	*prmin		const char	Input parameter			*
 *	*prmout		const char	Output parameter		*
 *	*num		const int	Location of grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 **									*
 * Log:									*
 * M. desJardins/NMC	 3/92						*
 * L. Williams/EAI	 8/94	Added 'TMW' to temperature field check	*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Piper/SAIC	 2/02	Increased c3 & c3out to 4 due to UMR 	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char ppp[5], c3[4], c3out[4], c1;
    int lenp, lens, itin, itout, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Check that the input string and output lengths are 4.
     */
    cst_lstr ( (char *)prmin, &lenp, &ier );
    cst_lstr ( (char *)prmout, &lens, &ier );
    if ( ( lenp != 4 ) || ( lens != 4 ) ) return;

    /*
     * Skip checks if names are identical.
     */
    if ( strcmp ( prmin, prmout ) == 0 ) {
	*iret = 0;
	return;
    }

    /*
     * Translate special input names into names with K, C, or F.
     */
    if ( strcmp ( prmin, "TEMP" ) == 0 ) {
	strcpy ( ppp, "TMPC" );
    } else if ( strcmp ( prmin, "DWPT" ) == 0 ) {
	strcpy ( ppp, "DWPC" );
    } else if ( strcmp ( prmin, "THTA" ) == 0 ) {
	strcpy ( ppp, "THTK" );
    } else if ( strcmp ( prmin, "STHA" ) == 0 ) {
	strcpy ( ppp, "STHK" );
    } else if ( strcmp ( prmin, "TMAX" ) == 0 ) {
	strcpy ( ppp, "TMXC" );
    } else if ( strcmp ( prmin, "TMIN" ) == 0 ) {
	strcpy ( ppp, "TMNC" );
    } else {
	strcpy ( ppp, prmin );
    }

    /*
     * Check that this is a temperature field.
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
	itin = 1;
    } else if ( c1 == 'C' ) {
	itin = 2;
    } else if ( c1 == 'F' ) {
	itin = 3;
    } else {
	return;
    }

    /*
     * Translate special output names into names with K, C, or F.
     */
    if ( strcmp ( prmout, "TEMP" ) == 0 ) {
	strcpy ( ppp, "TMPC" );
    } else if ( strcmp ( prmout, "DWPT" ) == 0 ) {
	strcpy ( ppp, "DWPC" );
    } else if ( strcmp ( prmout, "THTA" ) == 0 ) {
	strcpy ( ppp, "THTK" );
    } else if ( strcmp ( prmout, "STHA" ) == 0 ) {
	strcpy ( ppp, "STHK" );
    } else if ( strcmp ( prmout, "TMAX" ) == 0 ) {
	strcpy ( ppp, "TMXC" );
    } else if ( strcmp ( prmout, "TMIN" ) == 0 ) {
	strcpy ( ppp, "TMNC" );
    } else {
	strcpy ( ppp, prmout );
    }

    /*
     * Check that this is the same temperature field.
     */
    strncpy ( c3out, ppp, 3 );
    c3out[3] = '\0';
    if ( strcmp ( c3, c3out ) != 0 ) return;
    c1 = ppp[3];

    /*
     * Check for valid temperature units requested.
     */
    if ( c1 == 'K' ) {
	itout = 1;
    } else if ( c1 == 'C' ) {
	itout = 2;
    } else if ( c1 == 'F' ) {
	itout = 3;
    } else {
	return;
    }

    /*
     * If input and output types are the same, return.
     */
    if ( itout == itin ) {

    /*
     * Otherwise, compute correct units.
     */
    } else if ( ( itin == 1 ) && ( itout == 2 ) ) {
	pd_tmkc ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    } else if ( ( itin == 1 ) && ( itout == 3 ) ) {
	pd_tmkf ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    } else if ( ( itin == 2 ) && ( itout == 1 ) ) {
	pd_tmck ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    } else if ( ( itin == 2 ) && ( itout == 3 ) ) {
	pd_tmcf ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    } else if ( ( itin == 3 ) && ( itout == 1 ) ) {
	pd_tmfk ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    } else if ( ( itin == 3 ) && ( itout == 2 ) ) {
	pd_tmfc ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, &ier );
    }

    /*
     * Put correct name in label.
     */
    strcpy ( _dggrid.gparmd[(*num)-1], prmout );
    *iret = 0;

    return;
}

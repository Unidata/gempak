#include "dg.h"

void dg_ltln ( int *iret )
/************************************************************************
 * dg_ltln								*
 *									*
 * This subroutine computes the latitude and longitude at each grid 	*
 * point.  These values are stored in internal grids to which IDGLAT	*
 * and IDGLON in the DG common area point.				*
 *									*
 * dg_ltln ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 5/88	Removed call to set up polar lat/lon	*
 *				since these can only be done for	*
 *				a grid, not the whole file		*
 * M. desJardins/GSFC	11/88	Added call to GR_LTLN			*
 * K. Brill/HPC		 5/02	Store lat/lon in internal grids		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int comp1, comp2;
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check to see if navigation has been computed.
     */
    dg_cndg ( "LATITUDE_RAD", &_dgfile.idglat, &comp1, iret );
    if ( *iret != 0 ) return;
    dg_cndg ( "LONGITUD_RAD", &_dgfile.idglon, &comp2, iret );
    if ( *iret != 0 ) return;

    if ( comp1 == G_TRUE && comp2 == G_TRUE ) return;

    /*
     * Call GR_LTLN to compute lat/lon.
     */
    grc_ltln ( &_dgfile.kxd, &_dgfile.kyd, _dggrid.dgg[_dgfile.idglat-1].grid,
              _dggrid.dgg[_dgfile.idglon-1].grid, iret );
    if ( *iret != 0 ) {
	*iret = -16;
	strcpy ( _dgerr.errst, _dgfile.cprj );
	return;
    }

    /*
     * Convert lat/lons to radians.
     */
    for ( i = 0; i < _dgfile. kxyd; i++ ) {
	if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[i] ) ) 
	    _dggrid.dgg[_dgfile.idglat-1].grid[i] *= DTR;
	if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[i] ) )
	    _dggrid.dgg[_dgfile.idglon-1].grid[i] *= DTR;
    }

    return;
}

#include "dg.h"

void dg_fndv ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, char *gvect,
	       int *ignumu, int *ignumv, int *iret )
/************************************************************************
 * dg_fndv								*
 *									*
 * This subroutine checks to see if a vector grid is stored internally.	*
 * If the grid is found, IGNUMU and IGNUMV will be returned and GVECT	*
 * will be blank.  If the grid is not found, IGNUMU=IGNUMV=0, and	*
 * GVECT is unchanged.							*
 *									*
 * dg_fndv ( time1, time2, level1, level2, ivcord, gvect, ignumu,	*
 *           ignumv, iret )						*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *									*
 * Input and output parameters:						*
 *	*gvect		char		Grid to be computed		*
 *									*
 * Output parameters:							*
 *	*ignumu		int		Internal grid number of u comp	*
 *	*ignumv		int		Internal grid number of v comp	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		 4/93						*
 * M. desJardins/NMC	 7/93	Added test on savflg			*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg			*
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Assign ISUBID to IUSESV			*
 * C. Bailey/HPC	12/04	Modify savflg criteria in IF Statement	*
 * T. Lee/SAIC		 1/05	Checked ensemble member number		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char unam[14], vnam[14], gvct[17];
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ignumu = 0;
    *ignumv = 0;

    /*
     * Check for parameter already computed and stored in an internal 
     * grid.
     */
    strcpy ( gvct, gvect );
    if ( strcmp ( gvct, "WND" ) == 0 ) strcpy ( gvct, "OBS" );
    /*
     * Find names of u- and v- components and check for those grids in
     * internal grid area.
     */
    strcpy ( unam, "U" );
    strcat ( unam, gvct );
    strcpy ( vnam, "V" );
    strcat ( vnam, gvct );
    i = 0;
    while ( ( i < _dggrid.maxdgg ) &&
            ( ( *ignumv == 0 ) || ( *ignumu == 0 ) ) ) {
	if ( ( _dggrid.savflg[i] == G_TRUE ) ||
	     ( ( _dgfile.idlun == _dggrid.ifiled[i] ) &&
	       ( _dggrid.iensmb[i] == _nfile.mbrnum[_dggrid.ifiled[i]-1] ) &&
	       ( strcmp ( time1, _dggrid.dttimd1[i] ) == 0 ) &&
	       ( strcmp ( time2, _dggrid.dttimd2[i] ) == 0 ) &&
	       ( *level1 == _dggrid.leveld1[i] ) &&
	       ( *level2 == _dggrid.leveld2[i] ) &&
	       ( *ivcord == _dggrid.ivcrdd[i] ) ) ) {
	    if ( strcmp ( unam, _dggrid.gparmd[i] ) == 0 ) {
		*ignumu = i + 1;
		if ( _dggrid.iusesv[i] == 0 ) {
		    _dggrid.iusesv[i] = _dggrid.isubid;
	        }
	    }
	    if ( strcmp ( vnam, _dggrid.gparmd[i] ) == 0 ) {
		*ignumv = i + 1;
		if ( _dggrid.iusesv[i] == 0 ) {
		    _dggrid.iusesv[i] = _dggrid.isubid;
	        }
	    }
	}
	i++;
    }

    /*
     * Check whether both grids have been found.
     */
    if ( ( *ignumu != 0 ) && ( *ignumv != 0 ) ) {
	gvect[0] = '\0';
	_dgstck.stack[_dgstck.itop][0] = '\0';
	_dgstck.istack[_dgstck.itop] = (*ignumu) * 100 + (*ignumv);
    } else {
	*ignumu = 0;
	*ignumv = 0;
    }

    return;
}

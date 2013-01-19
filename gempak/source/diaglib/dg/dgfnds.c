#include "dg.h"

void dg_fnds ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, char *gfunc,
	       int *ignum, int *iret )
/************************************************************************
 * dg_fnds								*
 *									*
 * This subroutine checks to see if a scalar grid is stored internally. *
 * If the grid is found, IGNUM will be returned and GFUNC will be blank.*
 * If the grid is not found IGNUM=0, and GFUNC is unchanged.		*
 *									*
 * dg_fnds ( time1, time2, level1, level2, ivcord, gfunc, ignum, iret )	*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *									*
 * Input and output parameters:						*
 *	*gfunc		char		Grid to be computed		*
 *									*
 * Output parameters:							*
 *	*ignum		int		Internal grid number		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		 4/93						*
 * L. Sager/NMC		 5/93	Add test on savflg	 		*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg			*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Assign ISUBID to IUSESV			*
 * C. Bailey/HPC	12/04	Modify savflg criteria in IF statement	*
 * T. Lee/SAIC		 1/05	Checked ensemble member number		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ignum = 0;

    /*
     * Check for parameter already computed and stored in an internal
     * grid.
     */
    i=0;
    while ( ( i < _dggrid.maxdgg ) && ( *ignum == 0 ) ) {
	if ( strcmp ( gfunc, _dggrid.gparmd[i] ) == 0 &&
	     ( ( _dggrid.savflg[i] == G_TRUE ) ||
	       ( ( _dgfile.idlun == _dggrid.ifiled[i] ) &&
	         ( _dggrid.iensmb[i] == _nfile.mbrnum[_dggrid.ifiled[i]-1] ) &&
		 ( strcmp ( time1, _dggrid.dttimd1[i] ) == 0 ) &&
		 ( strcmp ( time2, _dggrid.dttimd2[i] ) == 0 ) &&
		 ( *level1 == _dggrid.leveld1[i] ) &&
		 ( *level2 == _dggrid.leveld2[i] ) &&
		 ( *ivcord == _dggrid.ivcrdd[i] ) ) ) ) {
	    *ignum = i + 1;
	    if ( _dggrid.iusesv[i] == 0 ) _dggrid.iusesv[i] = _dggrid.isubid;
	    gfunc[0] = '\0';
	    _dgstck.stack[_dgstck.itop][0] = '\0';
	    _dgstck.istack[_dgstck.itop] = *ignum;
	}
	i++;
    }

    return;
}

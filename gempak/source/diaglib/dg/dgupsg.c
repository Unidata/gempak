#include "dg.h"

void dg_upsg ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const int *ifilen,
	       const char *parm, const int * num, int *iret )
/************************************************************************
 * dg_upsg								*
 *									*
 * This subroutine updates the header for a grid stored for the grid	*
 * diagnostics.								*
 *									*
 * dg_upsg ( time1, time2, level1, level2, ivcord, ifilen, parm, num,	*
 *           iret )							*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *	*ifilen		const int	File number			*
 *	*parm		cosnt char	Parameter name			*
 *	*num		const int	Number of grid to update	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/88	Cleaned up				*
 * M. desJardins/NMC	 7/93	Renamed from DG_UHDR; added ifilen	*
 * T. Lee/SAIC		 1/05	Added ensemble member number		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int gidx;
/*----------------------------------------------------------------------*/
    *iret = 0;
    gidx = (*num) - 1;

    strcpy ( _dggrid.dttimd1[gidx], time1 );
    strcpy ( _dggrid.dttimd2[gidx], time2 );
    _dggrid.leveld1[gidx] = *level1;
    _dggrid.leveld2[gidx] = *level2;
    _dggrid.ivcrdd[gidx]  = *ivcord;
    _dggrid.ifiled[gidx]  = *ifilen;
    _dggrid.iensmb[gidx]  = 0;
    strcpy ( _dggrid.gparmd[gidx], parm );

    return;
}

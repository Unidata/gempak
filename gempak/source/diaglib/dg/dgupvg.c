#include "dg.h"

void dg_upvg ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const int *ifilen,
	       const char *parm, const int *numu, const int *numv,
	       int *iret )
/************************************************************************
 * dg_upvg								*
 *									*
 * This subroutine updates the header for a grid stored for the grid	*
 * diagnostics.								*
 *									*
 * dg_upvg ( time1, time2, level1, level2, ivcord, ifilen, parm, numu,	*
 *           numv, iret )						*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *	*ifilen		const int	File number			*
 *	*parm		const char	Parameter name			*
 *	*numu		const int	Number of u-component grid	*
 *	*numv		const int	Number of v-component grid	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/NMC	 7/93	From DG_UPSG				*
 * T. Piper/GSC		11/98	Updated prolog				*
 * T. Lee/SAIC		 1/05	Added ensemble member number		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char uparm[14], vparm[14];
    int ugidx, vgidx;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Make names for u- and v- components.
     */
    strcpy ( uparm, "U" );
    strcat ( uparm, parm );
    strcpy ( vparm, "V" );
    strcat ( vparm, parm );

    /*
     * Update the u-component grid and then the v-component grid.
     */
    ugidx = (*numu) - 1;
    vgidx = (*numv) - 1;
    strcpy ( _dggrid.dttimd1[ugidx], time1 );
    strcpy ( _dggrid.dttimd2[ugidx], time2 );
    _dggrid.leveld1[ugidx] = *level1;
    _dggrid.leveld2[ugidx] = *level2;
    _dggrid.ivcrdd[ugidx] = *ivcord;
    _dggrid.ifiled[ugidx] = *ifilen;
    _dggrid.iensmb[ugidx] = 0;
    strcpy ( _dggrid.gparmd[ugidx], uparm );

    strcpy ( _dggrid.dttimd1[vgidx], time1 );
    strcpy ( _dggrid.dttimd2[vgidx], time2 );
    _dggrid.leveld1[vgidx] = *level1;
    _dggrid.leveld2[vgidx] = *level2;
    _dggrid.ivcrdd[vgidx] = *ivcord;
    _dggrid.ifiled[vgidx] = *ifilen;
    _dggrid.iensmb[vgidx] = 0;
    strcpy ( _dggrid.gparmd[vgidx], vparm );

    return;
}

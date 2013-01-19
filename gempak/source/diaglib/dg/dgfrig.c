#include "dg.h"

void dg_frig ( const int *num, int *iret )
/************************************************************************
 * dg_frig								*
 *									*
 * This subroutine frees an internal grid regardless of its ownership	*
 * by any subroutine.							*
 *									*
 * dg_frig ( num, iret )						*
 *									*
 * Input parameters:							*
 *	*num		const int	Internal grid number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 2/03						*
 * R. Tian/SAIC		 2/06	Recoded/Modified from Fortran		*
 * T. Piper/SAIC	03/08	Replaced cmm_free1d with G_FREE		*
 ***********************************************************************/
{
    int gidx, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;
    gidx = (*num) - 1;

/*
 * Free memory of the grid.
 */
#ifdef MEM_DEBUG
    printf ( "Freed grid %d at %p with size: %d\n",
        *num, (void *)(_dggrid.dgg[gidx].grid), _dggrid.dgg[gidx].size );
#endif
    G_FREE ( _dggrid.dgg[gidx].grid, float );
    _dggrid.dgg[gidx].size = 0;

/*
 * Reset the grid information.
 */
    _dggrid.iusesv[gidx] = 0;
    _dggrid.savflg[gidx] = G_FALSE;
    _dggrid.dttimd1[gidx][0] = '\0';
    _dggrid.dttimd2[gidx][0] = '\0';
    _dggrid.leveld1[gidx] = 0;
    _dggrid.leveld2[gidx] = 0;
    _dggrid.ivcrdd[gidx] = 0;;
    _dggrid.gparmd[gidx][0] = '\0';
    _dggrid.savflg[gidx] = G_FALSE;

    return;
}

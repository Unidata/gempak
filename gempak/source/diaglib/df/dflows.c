#include "df.h"

void df_lows ( int *iret )
/************************************************************************
 * df_lows								*
 *									*
 * This subroutine finds the relative minima over a grid.  This		*
 * function needs two parameters, the second is a scalar constant	*
 * for the search radius.						*
 *									*
 *     LOWS (S, K)							*
 *									*
 * df_lows ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * K. Brill/NMC          5/93   					*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int hiflag, loflag;
/*----------------------------------------------------------------------*/
    hiflag = G_FALSE;
    loflag = G_TRUE;

    df_hilo ( &hiflag, &loflag, iret );

    return;
}

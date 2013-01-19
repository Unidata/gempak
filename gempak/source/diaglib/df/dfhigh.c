#include "df.h"

void df_high ( int *iret )
/************************************************************************
 * df_high								*
 *									*
 * This subroutine finds the relative extrema over a grid.  This	*
 * function needs two parameters, the second is a scalar constant	*
 * for the search radius.						*
 *									*
 *     HIGH (S, K)							*
 *									*
 * df_high ( iret )							*
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
    hiflag = G_TRUE;
    loflag = G_FALSE;

    df_hilo ( &hiflag, &loflag, iret );

    return;
}    

#include "pd.h"

void pd_sduv ( const float *sped, const float *drct, const int *np,
               float *uwnd, float *vwnd, int *iret )
/************************************************************************
 * pd_sduv								*
 *									*
 * This subroutine computes UWND and VWND from SPED and DRCT for an	*
 * array.  The following equations are used:				*
 *									*
 *                UWND = -SIN ( DRCT ) * SPED				*
 *                VWND = -COS ( DRCT ) * SPED				*
 *									*
 * pd_sduv ( sped, drct, np, uwnd, vwnd, iret )				*
 *									*
 * Input parameters:							*
 *	*sped		const float	Wind speed			*
 *	*drct		const float	Wind direction in degrees	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*uwnd		float		U component			*
 *	*vwnd		float		V component			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * T. Piper/GSC		 3/99	Corrected typo in prolog		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float sss, ddd;
    int npt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
	sss = sped [i];
	ddd = drct [i];

	/*
	 * Check for missing data.
	 */
	if ( ERMISS ( sss ) || ERMISS ( ddd ) ) {
	    uwnd [i] = RMISSD;
	    vwnd [i] = RMISSD;
	} else {
	    uwnd [i] = -sin ( ddd * DTR ) * sss;
	    vwnd [i] = -cos ( ddd * DTR ) * sss;
	}
    }

    return;
}

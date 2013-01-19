#include "pd.h"

static const float VALUE0 = -0.01F;

void pd_prcp ( const float *prc1, const float *prc2, const float *rmult,
               const int *np, float *total, int *iret )
/************************************************************************
 * pd_prcp								*
 *									*
 * This subroutine is used to add or subtract two precipitation 	*
 * amounts.								*
 *									*
 * The total precipitation is computed as:				*
 *									*
 *		TOTAL = PRC1 + RMULT * PRC2				*
 *									*
 * pd_prcp ( prc1, prc2, rmult, np, total, iret )			*
 *									*
 * Input parameters:							*
 *	*prc1		const float	First precipitation amount	*
 *	*prc2		const float	Second precipitation amount	*
 *	*rmult		const float	Multiplier			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*total		float		Total precipitation		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/NMC	 3/94						*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float zerov, fvalue;
    int npt, first, ii;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    first  = G_TRUE;
/*
 * Set a default value for 0.  Change if a negative number is found.
 */
    zerov  = VALUE0;
    fvalue = VALUE0;
/*
 * Loop through all the points.
 */
    for ( ii = 0; ii < npt; ii++ ) {
/*
 * Check for missing data.
 */
	if ( ( ERMISS ( prc1[ii] ) ) || ( ERMISS ( prc2[ii] ) ) ) {
	    total[ii] = RMISSD;
/*
 * Check for amounts less than 0.  These values are used
 * for the 0 value so that 0 can be used as a contour level.
 */
	} else if ( ( prc1[ii] < 0.0F ) && ( prc2[ii] < 0.0F ) ) {
	    zerov = prc1[ii];
	    total[ii] = prc1 [ii];
	} else if ( ( prc1[ii] < 0.0F ) ) {
	    zerov = prc1[ii];
	    total[ii] = prc2[ii];
	} else if ( ( prc2[ii] < 0.0F ) ) {
	    zerov = prc2[ii];
	    total[ii] = prc1[ii];
/*
 * Add the two values together using the multiplier.
 */
	} else {
	    total[ii] = prc1[ii] + (*rmult) * prc2[ii];
	    if ( total[ii] <= 0.0F ) {
	        total[ii] = zerov;
		if ( first ) {
		    fvalue = zerov;
		    first  = G_FALSE;
		}
	    }
	}
    }
/*
 * Check that the first missing value is correct.
 */
    if ( ( !G_DIFF(zerov, VALUE0) ) && ( !G_DIFF(fvalue, zerov) ) ) {
        for ( ii = 0; ii < npt; ii++ ) {
	    if ( ( total[ii] <= 0.0F ) && ( ! ERMISS ( total[ii] ) ) ) {
	        total[ii] = zerov;
	    }
	}
    }

    return;
}

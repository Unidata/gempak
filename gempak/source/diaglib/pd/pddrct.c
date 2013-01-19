#include "pd.h"

void pd_drct ( const float *uwnd, const float *vwnd, const int *np,
               float *drct, int *iret )
/************************************************************************
 * pd_drct								*
 *									*
 * This subroutine computes DRCT from UWND and VWND.  The following	*
 * equation is used:							*
 *									*
 *             DRCT = ATAN2 ( -UWND, -VWND ) * RTD			*
 *									*
 * pd_drct  ( uwnd, vwnd, np, drct, iret )				*
 *									*
 * Input parameters:							*
 *	*uwnd		const float	U component			*
 *	*vwnd		const float    	V component			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*drct		float		Wind direction			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 * H. Zeng/SAIC		09/07   Changed if statement condition		*
 ************************************************************************/
{
    float uuu, vvv;
    int ii, npt;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
/*
 * Loop through the points.
 */
    for ( ii = 0; ii < npt; ii++ ) {
/*
 * Check for missing data.
 */
        uuu = uwnd[ii];
        vvv = vwnd[ii];
	if ( ERMISS ( uuu ) || ERMISS ( vvv ) ) {
	    drct[ii]= RMISSD;
        } 

        /*
         * Don't use G_DIFF instead of "==" to check the values of 
         * uuu and vvv against 0.0F. 
         * Don't change the following if condition codes in the future.
         */
        else if ( ( uuu == 0.0F ) && ( vvv == 0.0F ) ) {
 	    drct[ii] = 0.0F;
        } 
        else {
	    drct[ii] = atan2 ( -uuu, -vvv ) * RTD;
	    if ( drct[ii] <= 0.0F )  drct[ii] += 360.0F;
	}
    }
    
    return;
}

#include "fortran_wrappers.h"

void ctg_cftm ( int ifcast, char *ftype, char *ftime, int *iret )
/************************************************************************
 * ctg_cftm								*
 *									*
 * This subroutine converts an integer grid forecast time into		*
 * the character forecast type and time.  The forecast type 		*
 * is  A (analysis), F (forecast), G (guess) or I (initialize).		*
 * If the forecast time is less than 100 and the minutes are 00,	*
 * only hh is returned.							*
 *									*
 * ctg_cftm ( ifcast, ftype, ftime, iret )				*
 *									*
 * Input parameters:							*
 *	ifcast		int		GEMPAK grid time		*
 *									*
 * Output parameters:							*
 *	*ftype		char		Forecast type ( A,F,G,I )	*
 *	*ftime		char		Forecast time ( hhhmm )		*
 * 	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -2 = invalid forecast type	*
 *					 -3 = invalid forecast time	*
 **									*
 * Log:									*
 * R. Tian/SAIC		 2/06	C wrapper of TG_CFTM			*
 ************************************************************************/
{
    char tmptyp[2], tmptim[9];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Call TG_CFTM.
     */
    tg_cftm ( &ifcast, tmptyp, tmptim, iret );

    if ( *iret == 0 ) {
        *ftype = tmptyp[0];

	tmptim[8] = '\0';
	cst_lstr ( tmptim, &len, &ier );
	tmptim[len] = '\0';
	strcpy ( ftime, tmptim );
    }

    return;
}

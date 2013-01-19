#include "fortran_wrappers.h"

void ctg_itoc ( const int *intdtf, char *gdattm, int *iret )
/************************************************************************
 * ctg_itoc								*
 *									*
 * This subroutine converts an integer time array containing the date,	*
 * time and forecast time into a GEMPAK grid time.			*
 *									*
 * ctg_itoc ( intdtf, gdattm, iret )					*
 *									*
 * Input parameters:							*
 *	*intdtf		const int	Date, time, forecast time	*
 *									*
 * Output parameters:							*
 *	*gdattm		char		GEMPAK grid time		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid date or time	*
 **									*
 * Log:									*
 * R. Tian/SAIC		 2/06	C wrapper of TG_ITOC			*
 ************************************************************************/
{
    char tmptim[21];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret   = 0;

    tg_itoc ( (int *)intdtf, tmptim, iret, sizeof(tmptim) );

    if ( *iret == 0 ) {
        tmptim[20] = '\0';
	cst_lstr ( tmptim, &len, &ier );
	tmptim[len] = '\0';
	strcpy ( gdattm, tmptim );
    }

    return;
}

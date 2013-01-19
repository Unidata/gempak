#include "geminc.h"
#include "gemprm.h"

void css_gtim ( int *itype, char *dattim, int *iret )
/************************************************************************
 *  css_gtim								*
 *									*
 * This subroutine returns the current system time as a GEMPAK 		*
 * date time string.  The time is given as UTC (Universal Coordinated	*
 * Time) or local time.							*
 *									*
 * css_gtim ( itype, dattim, iret )					*
 *									*
 *  Input parameters:							*
 *	*itype		int		Type of time			*
 *					  0 = Local			*
 *					  1 = UTC			*
 *									*
 *  Output parameters:							*
 *	*dattim		char		GEMPAK date/time string		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/99	Created					*
 * A. Hardy/NCEP	 6/03	Added time zone to CSS_DATE		*
 * B. Yin/SAIC  	03/04	Changed itype from int to int*		*
 ***********************************************************************/
{

    int	iyear, imon, iday, ihour, imin, isec, julian;
    char	zone[4];

/*---------------------------------------------------------------------*/

    css_date ( itype, &iyear, &imon, &iday,
		&ihour, &imin, &isec, &julian, zone, iret );

    if ( *iret == 0 ) {
	sprintf ( dattim, "%02d%02d%02d/%02d%02d",
		  iyear%100, imon, iday, ihour, imin );
    }
    else {
	strcpy ( dattim, "\0");
    }
}

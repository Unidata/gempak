#include "geminc.h"
#include "gemprm.h"

void css_date ( int *itype, int *iyear, int *imon, int *iday, 
		int *ihour, int *imin, int *isec, int *julian, 
                char *zone, int *iret )
/************************************************************************
 *  css_date								*
 *									*
 *  This subroutine returns the current system time in integer		*
 *  variables.  The time is given as UTC (Universal Coordinated Time)   *
 *  or local time.							*
 *									*
 * css_date ( itype, iyear, imon, iday, ihour, imin, isec, julian,	*
 *	      iret )							*
 *									*
 *  Input parameters:							*
 *	*itype		int		Type of time			*
 *					  0 = Local			*
 *					  1 = UTC			*
 *									*
 *  Output parameters:							*
 *	*iyear		int		Year:   yyyy			*
 *	*imon		int		Month:	mm			*
 *	*iday		int		Day:	dd			*
 *	*ihour 		int		Hour:	hh			*
 *	*imin		int		Minute:	nn			*
 *	*isec		int		Second:	ss			*
 *	*julian		int		Days since 1 January            *
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/97	Copied from CFDATE			*
 * D. Kidwell/NCEP	 2/99	Corrected prologue for iyear, julian    *
 * A. Hardy/NCEP	 6/03   Added time zone string			*
 * B. Yin/SAIC  	03/04	Changed itype from int to int*		*
 * B. Yin/SAIC  	05/04	Added event clock call			*
 ***********************************************************************/
{
	int 		isdst, ier;
	time_t		timesec;
        Boolean		evtclock = FALSE;
	struct tm	*current;

/*---------------------------------------------------------------------*/
	*iret   = G_NORMAL;

        css_evtison ( &evtclock, &ier );
	if ( evtclock ) {
	    css_evtadvtime( &timesec, &ier );
        }
	else {
	    timesec = time(NULL);
        }
 
	if ( *itype == 1 ) {
	    current = gmtime ( &timesec );
	}
	else {
	    current = localtime ( &timesec );
	}

	*iyear  = (*current).tm_year + 1900;
	*imon   = (*current).tm_mon + 1;
	*iday   = (*current).tm_mday;
	*ihour  = (*current).tm_hour;
	*imin   = (*current).tm_min;
	*isec   = (*current).tm_sec;
	*julian = (*current).tm_yday;
	isdst = (*current).tm_isdst;

       /*
        * Set the time zone string.
	*/

	if ( *itype == 1 ) {
	    cst_ncpy ( zone, "GMT", 3, &ier);
	}
	else {
	    cst_ncpy ( zone, tzname[isdst], 3, &ier);
	}

}

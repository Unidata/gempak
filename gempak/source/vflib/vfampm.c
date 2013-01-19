#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t   newinfo;
extern  SpcInfo_t   spcinfo;

void vfampm ( int time, int *newtime, char *ampm, int *iret )
/************************************************************************
 * vfampm                                                               *
 *                                                                      *
 * This function assigns an AM or PM to the local time.			*
 *                                                                      *
 * vfampm (time, newtime, ampm, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      time		int		input local time                *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 *      *newtime	int		time converted to local hour    *
 *      *ampm		char		AM or PM designation		*
 *      *iret           int             Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 ***********************************************************************/
{
/*-------------------------------------------------------------------*/
        *iret = 0;
	if ( time == 12 ) {
	    *newtime = time;
	    strcpy(ampm, "PM");
	}
	else if ( time == 00 ){
	    *newtime = 12 ;
	    strcpy(ampm, "AM");
	}
	else if ( (time >= 1) && (time <= 11 ) ){
	    *newtime = time; 
	    strcpy(ampm, "AM");
	}
	else if ( ( time > 12) ){
	    *newtime = time - 12;
	    strcpy(ampm, "PM");
	}
}

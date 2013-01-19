#include "geminc.h"
#include "gemprm.h"

void utl_ampm ( int time, int *newtime, char *ampm, int *iret )
/************************************************************************
 * utl_ampm                                                             *
 *                                                                      *
 * This function converts the local 24 hour time to the local 12 hour   *
 * time. The local 12 hour time and the associated  'AM' or 'PM' are	*
 * returned.								*
 *                                                                      *
 * utl_ampm (time, newtime, ampm, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      time		int		input local 24 hour time        *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 *      *newtime	int		time converted to local hour    *
 *      *ampm		char		AM or PM designation		*
 *      *iret           int             Return Code                     *
 *					  -1 = Bad input time		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	5/03   		Copied from VFAMPM		*
 * A. Hardy/NCEP       11/03   		Changed len 3 -> 2		*
 ***********************************************************************/
{
    int 	len, ier;
/*-------------------------------------------------------------------*/
    *iret = 0;
    len   = 2;
    *newtime = 0;
    ampm[0] = '\0';

    if ( (time < 0 ) || ( time > 24 ) ) {
	*iret = -1;
	return;
    }
    if ( time == 12 ) {
	    *newtime = time;
	    cst_ncpy(ampm, "PM", len, &ier);
    }
    else if ( time == 00 ){
	    *newtime = 12 ;
	    cst_ncpy(ampm, "AM", len, &ier);
    }
    else if ( (time >= 1) && (time <= 11 ) ){
	    *newtime = time; 
	    cst_ncpy(ampm, "AM", len, &ier);
    }
    else if ( ( time > 12) ){
	    *newtime = time - 12;
	    cst_ncpy(ampm, "PM", len, &ier);
    }
    *iret = ier;
}

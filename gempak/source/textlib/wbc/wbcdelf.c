#include "geminc.h"
#include "gemprm.h"

void wbc_defl ( int vhour, int vmins, char *vampm, int ehour, 
		int emins, char *eampm, char *lclzn, int len1, 
		int len2, char *efst, char *efen, int *iret )
/************************************************************************
 * wbc_defl								*
 *                                                                      *
 * This function creates the local start and stop times with the 'AM'   *
 * or 'PM' designation appended to the time. The ending time also has   *
 * the local time zone appened to the 'AM' or 'PM' designator.		*
 *                                                                      *
 * wbc_defl ( vhour, vmins, vampm, ehour, emins, eampm, lclzn, len1,    *
 *	      len2, efst, efen, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      vhour		int		Local valid hour		*
 *      vmins		int		Local valid minutes		*
 *      *vampm		char		Local AM or PM designation	*
 *      ehour		int		Local ending hour		*
 *      emins		int		Local ending minutes		*
 *      *eampm		char		Local AM or PM designation	*
 *	*lclzn		char		Local time zone string		*
 *	len1		int		Max length of 'efst'		*
 *	len2		int		Max length of 'efen' 		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*efst		char		Starting local time string 	*
 *	*efen		char		Ending local time string 	*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 ***********************************************************************/
{
    int     ier, len4, len8;
    char    hold[20];
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0;
    len4  = 4;
    len8  = 8;
    efen[0] = '\0';
    efst[0] = '\0';

   /*
    * Find the starting local time. Check for '12 AM' and '12 PM' and 
    * convert to 'MIDNIGHT' or 'NOON'.
    */

    hold[0] = '\0';
    if ( (vhour == 12) && (vmins == 0) && (strcmp(vampm, "AM") == 0) ) { 
        cst_ncpy( efst, "MIDNIGHT", len8, &ier );
    }
    else if ( (vhour == 12) && (vmins == 0) && (strcmp(vampm, "PM") == 0) ) {
        cst_ncpy ( efst, "NOON", len4, &ier );
    }
    else { 
        sprintf ( hold, "%d%02d %s", vhour, vmins, vampm);
        cst_ncpy ( efst, hold, len8, &ier);
    }

   /*
    * Find the ending local time. Check for '12 AM' and '12 PM' and 
    * convert to 'MIDNIGHT' or 'NOON'.
    */

    hold[0] = '\0';
    if ( (ehour == 12) && (emins == 0) && (strcmp(eampm, "AM") == 0) ) { sprintf( hold, "MIDNIGHT %s", lclzn );
        sprintf ( hold, "MIDNIGHT %s", lclzn );
        cst_ncpy ( efen, hold, 12, &ier);
    }

    else if ( (ehour == 12) && (emins == 0) && (strcmp(eampm, "PM") == 0) ) {
        sprintf ( hold, "NOON %s", lclzn );
        cst_ncpy ( efen, hold, 8, &ier );
    }
    else  {
        sprintf ( hold, "%d%02d %s %s", ehour, emins, eampm, lclzn );
        cst_ncpy ( efen, hold, 11, &ier);
    }
}

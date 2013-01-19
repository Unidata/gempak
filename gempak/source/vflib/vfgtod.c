#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

void vfgtod ( int vhour, int ehour, int emin, char *vampm, 
		char *eampm, int daywk, char *genday, int *iret )
/************************************************************************
 * vfgtod                                                               *
 *                                                                      *
 * This function determines the general time day string for SELs and    *
 * SEVs.     								*
 *                                                                      *
 * vfgtod (vhour, ehour, emin, vampm, eampm, daywk, genday, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      vhour		int		valid hour			*
 *      ehour		int		ending hour			*
 *      emin		int		ending minutes			*
 *      vampm		char*		AM or PM designation		*
 *      eampm		char*		AM or PM designation		*
 *      daywk		int		numerical day of the week       *
 *                                                                      *
 * Output parameters:                                                   *
 *      genday		char*		general time of day string      *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * M. Li/GSC		10/99	Removed "AND ... MORNING" if the ending	*
 *				time is 1200 AM				*
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 5/01   Removed 'vmin' from call sequence	*
 * A. Hardy/NCEP	 5/03	Fixed for Sat night to Sun morning	*
 ***********************************************************************/
{
    char    fsttim[11], scndtim[11], wdsdwk[11], dwkwds[11];

    char    dwname[8][11] = { "SUNDAY ", "MONDAY ", "TUESDAY ", 
                              "WEDNESDAY ", "THURSDAY ", "FRIDAY ", 
			      "SATURDAY ", "SUNDAY "  };

/*-------------------------------------------------------------------*/
        *iret = 0;
	genday[0] = '\0';
	fsttim[0] = '\0';
	scndtim[0] = '\0';
	dwkwds[0] = '\0';
	wdsdwk[0] = '\0';

      /*
       * Set up morning combinations.
       */

	if ( strcmp (vampm, "AM") == 0 ) {
	    if ( ( strcmp (eampm, "AM") == 0 ) ||
		 ( ( strcmp (eampm, "PM") == 0 ) &&
		 ( ( ehour == 12 ) && (emin == 0 ) ) ) ){
		strcpy (fsttim,"MORNING ");
	        strcpy ( wdsdwk, dwname[daywk-1]);
	        strcpy ( genday, wdsdwk );
	        strcat ( genday, fsttim);
	    }
	    else if ( strcmp (eampm, "PM") == 0 ) {
		if ( ( ehour == 12 ) || ( (ehour >= 1 ) && 
		    ( ehour < 6 ) ) ) {
		    strcpy (fsttim,"MORNING ");
		    strcpy (scndtim,"AFTERNOON ");
		}
		else if ( ( ehour >= 6 ) && ( ehour <= 11 ) ) {
		    strcpy (fsttim,"MORNING ");
		    strcpy (scndtim,"EVENING ");
		}
	        strcpy ( wdsdwk, dwname[daywk-1]);
	        strcpy ( genday, wdsdwk );
	        strcat ( genday, fsttim);
                if ( !(ehour == 12 && emin == 0 && strcmp(eampm, "AM") == 0) ) { 
                    strcat ( genday, "AND ");
	            strcat ( genday, dwkwds );
	            strcat ( genday, scndtim);
                }
	    }
	}

      /*
       * Set up afternoon/evening combinations.
       */

	if ( strcmp (vampm, "PM") == 0 ) {
	    if ( strcmp (eampm, "PM") == 0 ) {
		if ( ( ( vhour == 12 ) ||  ( ( vhour >= 1 ) && 
		        ( vhour < 6 ) ) ) && ( ( ehour == 12 ) ||  
			( ( ehour >= 1 ) && ( ehour < 6 ) ) ) ) {
		    strcpy (fsttim,"AFTERNOON ");
	            strcpy ( wdsdwk, dwname[daywk-1]);
	            strcpy ( genday, wdsdwk );
	            strcat ( genday, fsttim);
		}
		else if ( ( ( vhour >= 6 ) && ( vhour <= 11 ) ) &&
		     ( ( ehour >= 6 ) && ( ehour <= 11 ) ) ) {
		    strcpy (fsttim,"EVENING ");
	            strcpy ( wdsdwk, dwname[daywk-1]);
	            strcpy ( genday, wdsdwk );
	            strcat ( genday, fsttim);
		}
		else if ( ( (vhour == 12) || ( ( vhour >= 1 ) && 
		      ( vhour < 6 ) ) ) &&
		     ( ( ehour >= 6 ) && ( ehour <= 11 ) ) ) {
		    strcpy (fsttim,"AFTERNOON ");
		    strcpy (scndtim,"EVENING ");

	       /*
                * Retrieve the day of the week.
	        */

	         strcpy ( wdsdwk, dwname[daywk-1]);
	         strcpy ( genday, wdsdwk );
	         strcat ( genday, fsttim);
	         strcat ( genday, "AND ");
	         strcat ( genday, scndtim);
		}
	    }
	    else if ( strcmp (eampm, "AM") == 0 ) {
		    strcpy (scndtim,"MORNING ");
		if  ( ( vhour >= 6 ) && ( vhour <= 11 ) ) {
		    strcpy (fsttim,"NIGHT ");
		}
		if  ( ( vhour >= 1 ) && ( vhour < 6 ) ) {
		    strcpy (fsttim,"AFTERNOON ");
		}

	       /*
                * Retrieve the day of the week.
	        */

	          strcpy ( wdsdwk, dwname[daywk-1]);
	          strcpy ( dwkwds, dwname[daywk]);

	       /*
	        *  Assemble the general time of day string.
	        */

	         strcpy ( genday, wdsdwk );
	         strcat ( genday, fsttim);
	         if ( !(ehour == 12 && emin == 0 && strcmp(eampm, "AM") == 0) ) { 
                     strcat ( genday, "AND ");
	             strcat ( genday, dwkwds );
	             strcat ( genday, scndtim);
                 }     
	    }
	}

}

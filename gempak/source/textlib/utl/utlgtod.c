#include "geminc.h"
#include "gemprm.h"

#define WRDLEN  12

void utl_gtod ( int vhour, int ehour, int emin, char *vampm, 
		char *eampm, int daywk, int len, char *genday, int *iret )
/************************************************************************
 * utl_gtod                                             		*
 *                                                                      *
 * This function determines the general time of day string. This string *
 * consists of the weekday name (Eg. MONDAY, TUESDAY,...) and the time  *
 * of day descriptor (Eg. MORNING, AFTERNOON, EVENING and NIGHT). The   *
 * time of day descriptors may be used in combination to describe how   *
 * long an event wiill last (Eg. TUESDAY MORNING AND AFTERNOON ).	*
 *                                                                      *
 * utl_gtod (vhour, ehour, emin, vampm, eampm, daywk, len, genday, 	*
 *	     iret )							*	
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      vhour		int		valid hour			*
 *      ehour		int		ending hour			*
 *      emin		int		ending minutes			*
 *      *vampm		char		valid AM or PM designation	*
 *      *eampm		char		ending AM or PM designation	*
 *      daywk		int		numerical day of the week       *
 *	len		int		Max length for 'genday'		*
 *                                                                      *
 * Output parameters:                                                   *
 *      genday		char*		general time of day string      *
 *      *iret           int             Return Code                     *
 *					  -7 = Bad day of the week no.	*
 *					  -9 = Bad valid time AM/PM	*
 *					 -10 = Bad ending time AM/PM	*
 *					 -11 = Bad ending minutes	*
 *					 -12 = Bad valid hour		*
 *					 -13 = Bad ending hour		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03		Copied from VFGTOD		*
 ***********************************************************************/
{
    int	    lend, leng, lenc, ier;
    char    fsttim[WRDLEN], scndtim[WRDLEN], wdsdwk[WRDLEN], 
    	    dwkwds[WRDLEN], cand[6];

    char    dwname[8][WRDLEN] = { " SUNDAY", " MONDAY", " TUESDAY", 
                                  " WEDNESDAY", " THURSDAY", " FRIDAY", 
			          " SATURDAY", " SUNDAY" };

/*-------------------------------------------------------------------*/
        *iret = 0;
	ier = 0;
	genday[0] = '\0';
	fsttim[0] = '\0';
	scndtim[0] = '\0';
	dwkwds[0] = '\0';
	wdsdwk[0] = '\0';
      /*
       * Check for valid inputs.
       */
       if ( daywk < 1 || daywk > 7 ) {
	   *iret = -7;
	   return;
       }
       if (strcmp (vampm, "AM") != 0 ) { 
	   if (strcmp (vampm, "PM") != 0 ) {
	       *iret = -9;
	       return;
	   }
       }
       if (strcmp (eampm, "AM") != 0 ) { 
	   if (strcmp (eampm, "PM") != 0 ) {
	       *iret = -10;
	       return;
	   }
       }
       if ( emin < 0 || emin > 60 ) {
	   *iret = -11;
	   return;
       }
       if ( vhour < 1 || vhour > 12 ) {
	   *iret = -12;
	   return;
       }
       if ( ehour < 1 || ehour > 12 ) {
	   *iret = -13;
	   return;
       }

      /*
       * Set up morning combinations.
       */

	cst_ncpy ( cand, " AND", 4, &ier);
	if ( strcmp (vampm, "AM") == 0 ) {
	    lend = G_MIN (sizeof(wdsdwk), strlen(dwname[daywk-1]) );
	    if ( ( strcmp (eampm, "AM") == 0 ) ||
		 ( ( strcmp (eampm, "PM") == 0 ) &&
		 ( ( ehour == 12 ) && (emin == 0 ) ) ) ){

		cst_ncpy (fsttim," MORNING", 8, &ier);
	        cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier);

	        leng = G_MIN ( len, (int)strlen(wdsdwk) );
	        cst_ncpy ( genday, wdsdwk, leng, &ier );
	        cst_ncat ( genday, fsttim, &lenc, &ier );
	    }
	    else if ( strcmp (eampm, "PM") == 0 ) {
		if ( ( ehour == 12 ) || ( (ehour >= 1 ) && 
		    ( ehour < 6 ) ) ) {
		    cst_ncpy (fsttim," MORNING", 8, &ier);
		    cst_ncpy (scndtim," AFTERNOON", 10, &ier);
		}
		else if ( ( ehour >= 6 ) && ( ehour <= 11 ) ) {
		    cst_ncpy (fsttim," MORNING", 8, &ier);
		    cst_ncpy (scndtim," EVENING", 8, &ier);
		}

	        lend = G_MIN (sizeof(wdsdwk), strlen(dwname[daywk-1]) );
	        cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier);

	        leng = G_MIN ( len, (int)strlen(wdsdwk) );
	        cst_ncpy ( genday, wdsdwk, leng, &ier );
	        cst_ncat ( genday, fsttim, &lenc, &ier );

                if ( !(ehour == 12 && emin == 0 && strcmp(eampm, "AM") == 0) ) { 
                    cst_ncat ( genday, cand, &lenc, &ier );
	            cst_ncat ( genday, dwkwds, &lenc, &ier  );
	            cst_ncat ( genday, scndtim, &lenc, &ier );
                }
	    }
	}
      /*
       * Set up afternoon/evening combinations.
       */

	if ( strcmp (vampm, "PM") == 0 ) {
	    lend = G_MIN (sizeof(wdsdwk), strlen(dwname[daywk-1]) );
	    if ( strcmp (eampm, "PM") == 0 ) {
		if ( ( ( vhour == 12 ) ||  ( ( vhour >= 1 ) && 
		        ( vhour < 6 ) ) ) && ( ( ehour == 12 ) ||  
			( ( ehour >= 1 ) && ( ehour < 6 ) ) ) ) {

		    cst_ncpy (fsttim," AFTERNOON", 10, &ier );
	            cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier );

	            leng = G_MIN ( len, (int)strlen(wdsdwk) );
	            cst_ncpy ( genday, wdsdwk, leng, &ier );
	            cst_ncat ( genday, fsttim, &lenc, &ier );
		}
		else if ( ( ( vhour >= 6 ) && ( vhour <= 11 ) ) &&
		     ( ( ehour >= 6 ) && ( ehour <= 11 ) ) ) {
		    cst_ncpy (fsttim," EVENING", 8, &ier );
	            cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier);

	            leng = G_MIN ( len, (int)strlen(wdsdwk) );
	            cst_ncpy ( genday, wdsdwk, leng, &ier );
	            cst_ncat ( genday, fsttim, &lenc, &ier );
		}
		else if ( ( (vhour == 12) || ( ( vhour >= 1 ) && 
		      ( vhour < 6 ) ) ) &&
		     ( ( ehour >= 6 ) && ( ehour <= 11 ) ) ) {
		    cst_ncpy (fsttim," AFTERNOON", 10, &ier );
		    cst_ncpy (scndtim," EVENING", 8, &ier );
	           /*
                    * Retrieve the day of the week.
	            */
	            cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier);

	            leng = G_MIN ( len, (int)strlen(wdsdwk) );
	            cst_ncpy ( genday, wdsdwk, leng, &ier );
	            cst_ncat ( genday, fsttim, &lenc, &ier );
	            cst_ncat ( genday, cand, &lenc, &ier );
	            cst_ncat ( genday, scndtim, &lenc, &ier );
		}
	    }
	    else if ( strcmp (eampm, "AM") == 0 ) {
		    cst_ncpy (scndtim," MORNING", 8, &ier);
		if  ( ( vhour >= 6 ) && ( vhour <= 11 ) ) {
		    cst_ncpy (fsttim," NIGHT", 6, &ier);
		}
		if  ( ( vhour >= 1 ) && ( vhour < 6 ) ) {
		    cst_ncpy (fsttim," AFTERNOON", 10, &ier);
		}

	       /*
                * Retrieve the day of the week.
	        */

	        cst_ncpy ( wdsdwk, dwname[daywk-1], lend, &ier);

	        leng = G_MIN (strlen(dwname[daywk]), sizeof(dwkwds) );
	        cst_ncpy ( dwkwds, dwname[daywk], leng, &ier );
	       /*
	        *  Assemble the general time of day string.
	        */
	        leng = G_MIN ( len, (int)strlen(wdsdwk) );
	        cst_ncpy ( genday, wdsdwk, leng, &ier );

	        cst_ncat ( genday, fsttim, &lenc, &ier );
	        if ( !(ehour == 12 && emin == 0 && strcmp(eampm, "AM") == 0) ) { 
                    cst_ncat ( genday, cand, &lenc, &ier );
	            cst_ncat ( genday, dwkwds, &lenc, &ier  );
	            cst_ncat ( genday, scndtim, &lenc, &ier );
                }     
	    }
	}

}

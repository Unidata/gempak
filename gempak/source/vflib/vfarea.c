#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern 	SpcInfo_t      spcinfo;

void vfarea ( char *locnam, char *vorstr, char *areastr, int *iret )
/************************************************************************
 * vfarea                                                    		*
 *                                                                      *
 * This function converts the VOR stations to county and state          *
 * information and creates the VOR watch area string.			* 
 *                                                                      *
 * void vfarea ( locnam, vorstr, areastr, iret )   			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Locator type				*
 *	*vorstr		char	Polygone text string			*
 *									*
 * Output parameters:                                                   *
 *	*areastr	char	Polygone area text string		*
 *	*iret		int	Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC         03/00   Created					*
 * A. Hardy/GSC         10/00   Check for '0' vor point distances       *
 * A. Hardy/GSC         12/00   Changed clo_findstn np pointer -> int   *
 * A. Hardy/GSC		 5/01   Removed parameter 'type' from call	*
 * R. Tian/SAIC		 7/03 	Changed to call cst_gtag		*
 ************************************************************************/
{
	int	ii, nstn, maxlen, nstr, np, icnt, ier;
	char    tmpstr[420], holdstr[200], qstate[1], pstn[128]; 
	char    arrgrp[15], **aryvor, county[30], state[3];
/*---------------------------------------------------------------------*/

    *iret = 0;
    np = 1;
    icnt = 1;
    qstate[0] = '\0';
    tmpstr[0] = '\0';
    maxlen = sizeof(pstn);

   /*
    * Set up the memory space to break up the VOR area string.
    */

    aryvor = (char **) malloc(sizeof(char *) * 4);
    for ( ii = 0; ii < 4; ii++ ) {
        aryvor[ii] = (char *) malloc(6) ;
    }

   /*
    *  Get the city and state of the VOR station.
    */

    for ( ii = 0; ii < 6; ii++) {

        vorstr = (char *) cst_split ( vorstr, ';', 12, arrgrp, &ier);
        cst_clst ( arrgrp, ' ', " ", 4, 6, aryvor, &nstr, &ier);
	if ( nstr == 3 ) {
            clo_findstn ( locnam, aryvor[2], qstate, np, maxlen,
                      &nstn, pstn, &ier);
	}
	else if ( nstr == 2) {
            clo_findstn ( locnam, aryvor[1], qstate, np, maxlen,
                      &nstn, pstn, &ier);
	}

       /*
        * Create the watch area string.
        */

        holdstr[0] = '\0';

	cst_gtag ( "NAME", pstn, " ", county, &ier );
	cst_gtag ( "ST", pstn, " ", state, &ier );
	cst_rnan (county, county, &ier);
	if ( icnt < 6 ) {
	    if ( strcmp ( aryvor[0], "..") != 0 ) {
	        sprintf( holdstr,"%s %s OF %s, %s..TO ",aryvor[0], 
	                 aryvor[1], county, state);
	    }
	    else {
		/*
		 * Print string for zero distance and no direction.
		 */
	        sprintf( holdstr,"%s, %s..TO ", county, state);
	    }
	}
	else {
	    if ( strcmp ( aryvor[0], "..") != 0 ) {
	        sprintf( holdstr,"%s %s OF %s, %s.",aryvor[0], aryvor[1],
	             county, state);
	    }
	    else {
		/*
		 * Print string for zero distance and no direction.
		 */
	        sprintf( holdstr,"%s, %s.", county, state);
	    }
	}
	icnt++;
        strcat ( tmpstr, holdstr);
    }

    strcpy( areastr, tmpstr);

   /*
    * Free memory space.
    */

    for ( ii = 0; ii < 4; ii++ ) {
        free ( aryvor[ii] );
    }
    free ( aryvor);
}

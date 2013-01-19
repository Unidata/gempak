#include "geminc.h"
#include "gemprm.h"

void wbc_area ( char *locnam, char *vorstr, int len, char *areastr, 
                int *iret )
/************************************************************************
 * wbc_area                                                    		*
 *                                                                      *
 * This function converts the VOR stations string to the VOR watch area *
 * string,containing distance, direction, county names and state ids.	*
 *                                                                      *
 * Input example:							*
 *   17 WNW DEC;26 E FAM;26 WNW FAM;55 SSE COU;22 NNW UIN;47 ENE UIN;	*
 *                                                                      *
 * Output example:							*
 *   17 WNW OF DECATUR, IL..TO 26 E OF FARMINGTON, MO..TO 26 WNW OF 	*
 *   FARMINGTON, MO..TO 55 SSE OF COLUMBIA, MO..TO 22 NNW OF QUINCY, 	*
 *   IL..TO 47 ENE OF QUINCY, IL.					*
 *                                                                      *
 * wbc_area ( locnam, vorstr, len, areastr, iret )   			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Locator type				*
 *	*vorstr		char	Polygon text string			*
 *	len		int	Max length of 'areastr'			*
 *									*
 * Output parameters:                                                   *
 *	*areastr	char	Polygon area text string		*
 *	*iret		int	Return value				*
 *			           -5 = VORSTR too big 			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03   From VF_AREA				*
 ************************************************************************/
{
	int	ii, nstn, maxlen, nstr, np, icnt, ier, exp, max, len1;
	char    tmpstr[420], holdstr[200], qstate[1], pstn[180]; 
	char    arrgrp[15], **aryvor, county[30], state[3];
/*---------------------------------------------------------------------*/
    *iret = 0;
    np = 1;
    icnt = 1;
    max  = 6;
    exp  = 4;
    qstate[0] = '\0';
    tmpstr[0] = '\0';
    maxlen = sizeof(pstn);

    if ( strcmp ( locnam, "VOR") != 0 ) {
        *iret = -2;
        return;
    }
    cst_lstr ( vorstr, &len1, &ier );
    if ( len1 >= 400 ) {
        *iret = -5;
        return;
    }

   /*
    * Set up the memory space to break up the VOR area string.
    */

    clo_init ( &ier );

    aryvor = (char **) malloc(sizeof(char *) * 4);
    for ( ii = 0; ii < 4; ii++ ) {
        aryvor[ii] = (char *) malloc(6) ;
    }

   /*
    *  Get the city and state of the VOR station.
    */

    for ( ii = 0; ii < 6; ii++) {

        vorstr = (char *) cst_split ( vorstr, ';', 12, arrgrp, &ier);

        cst_clst ( arrgrp, ' ', " ", exp, max, aryvor, &nstr, &ier);

	if ( nstr == 3 ) {

            clo_findstn ( locnam, aryvor[2], qstate, np, maxlen,
                      &nstn, pstn, &ier);

	}
	else if ( nstr == 2) {
            clo_findstn ( locnam, aryvor[1], qstate, np, maxlen,
                      &nstn, pstn, &ier);
	}

	cst_gtag ( "NAME", pstn, " ", county, &ier );
	cst_gtag ( "ST", pstn, " ", state, &ier );
	cst_rnan (county, county, &ier);

       /*
        * Create the watch area string.
        */

        holdstr[0] = '\0';

	if ( icnt < 6 ) {
	    if ( strcmp ( aryvor[0], "..") != 0 ) {
	        sprintf( holdstr," %s %s OF %s, %s..TO ",aryvor[0], 
	                 aryvor[1], county, state);
	    }
	    else {
		/*
		 * Print string for zero distance and no direction.
		 */
	        sprintf( holdstr," %s, %s..TO ", county, state);
	    }
	}
	else {
	    if ( strcmp ( aryvor[0], "..") != 0 ) {
	        sprintf( holdstr," %s %s OF %s, %s.",aryvor[0], aryvor[1],
	             county, state);
	    }
	    else {
		/*
		 * Print string for zero distance and no direction.
		 */
	        sprintf( holdstr," %s, %s.", county, state);
	    }
	}
	icnt++;
        cst_ncat ( tmpstr, holdstr, &len1, &ier);
    }

    len1 = G_MIN ( len, (int)strlen(tmpstr) );
    cst_ncpy( areastr, tmpstr, len1, &ier);

   /*
    * Free memory space.
    */

    for ( ii = 0; ii < 4; ii++ ) {
        free ( aryvor[ii] );
    }
    free ( aryvor);
}

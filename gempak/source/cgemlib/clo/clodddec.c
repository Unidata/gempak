#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern 	CLO_t	clo;

void clo_dddec ( char *locnam, int *format, char *string, int *nexp, 
			float flat[], float flon[], int *nstn, int *iret )
/************************************************************************
 * clo_dddec                                                    	*
 *                                                                      *
 * This function returns the latitude and longitude for a decoded	*
 * location string such as ' 50nm ESE Topeka'.				*
 *                                                                      *
 * void clo_dddec ( locnam, format, string, nexp, flat, flon, nstn,     *
 *                  iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Data location name 			*
 *	*format		int	Indicator of format display		*
 *	*string		char	Character string location		*
 *      *nexp           int     Number of expected values returned      *
 *									*
 * Output parameters:                                                   *
 *	flat[nstn]	float   Latitude of point			*
 *	flon[nstn]	float   Longitude of point			*
 *	*nstn		int	Number of lat/lon pairs returned	*
 *	*iret		int	Return value				*
 *                              = -1 - unable to convert lat. or long.  *
 *                              = -2 - unable to match station		*
 *                              = -3 - not enough parms to make calc.   *
 *                              = -4 - missing units, can't do calc.    *
 *                              = -5 - missing/incorrect direction str  * 
 *                              = -6 - negative distance was found      *
 *                              = -7 - missing/incorrect distance str   * 
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/00   Created					*
 * A. Hardy/GSC          9/00   Added check for incorrect distance str  *
 * A. Hardy/GSC          9/00   Added expected num of lat/lon ret values*
 * F. J. Yen/NCEP	 8/01	Changed calling sequence for clo_cmpdir *
 *			 	and cleaned up.				* 
 * R. Tian/SAIC		 7/03	Changed to call cst_gtag		*
 ************************************************************************/
{
	int	inum, maxlen, np, ier, imxchr, inarr, ione;
	int	ii, jj, lens, ityp, ier1, ier2, istuff;
	int     which, ihund;
	float	dist, fdir, lat, lon, dlat, dlon; 
	char    qstate[2], pstn[2480], info[40]; 
	char    units[3], stn[34], sdist[6];
	char    **strary, chstn[256], *cptr;
	size_t	stlen;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *nstn = 0;
    flat[0] = RMISSD; 
    flon[0] = RMISSD;  
    np = 2;
    istuff = 0;
    imxchr = 80;
    units[0] = '\0';
    qstate[0] = '\0';
    maxlen = sizeof(pstn);

    which = clo_which ( locnam );

   /*
    * Parse the location string into distance, direction and station.
    */

    strary = (char **) malloc(sizeof(char *) * 3);
    for ( ii = 0; ii < 3; ii++ ) {
        strary[ii] = (char *) malloc(32) ;
    }
    cst_clst ( string, ' ', " ", 3, imxchr, strary, &inarr, &ier);
    if ( inarr == 3 ) {
	strcpy( stn, strary[2]);
    }
    else if ( inarr == 2 ){
	ier = -3;
	*iret = ier;
	return;
    }
    else if (inarr == 1) {
	strcpy( stn, strary[0]);
    }

   /*
    *  Separate the format code.
    */

    ione = *format % 10; 
    ihund = (int) (*format/100) % 10;

   /*
    *  Get the latitude and longitude of the station.
    */

    if  (clo.loc[which].format == 0 ) { 
        if  ( ione == 2 ) { 
            clo_findstn ( locnam, stn, qstate, np, maxlen, &inum, pstn, &ier);
	}
        if  ( ione == 4 ) { 
	    clo_finddesc (locnam, stn, qstate, np, maxlen, &inum, pstn, &ier);
	}      
	*nstn = G_MIN ( *nexp, inum );
	cptr =(char *)  strtok (pstn, ";");
	for ( ii = 0; ii < *nstn; ii++ ) {
	    strcpy(chstn, cptr);
	    if ( ier >= 0 ) {
		cst_gtag ( "LAT", chstn, "99999", info, &ier );
                cst_crnm ( info, &lat, &ier1 );
		cst_gtag ( "LON", chstn, "99999", info, &ier );
                cst_crnm ( info, &lon, &ier2 );

		ier = ier + ier1 + ier2;
                if ( ( inarr == 3 ) && ( ier == 0 ) ) { 

               /*
                *   Get the distance from the station.
                */
	            jj = 0;
	            cst_lstr ( strary[0], &lens, &ier );
	            while ( ( jj <= lens) && ( istuff != 1 ) ) {
	     	        cst_alnm ( strary[0][jj], &ityp, &ier );
		        if ( ( ityp == 1 ) && (jj != 0 ) ) {
		            istuff = 1;
	                    stlen = (size_t)(lens - 2);
		            strcpy(units, strary[0] + jj); 
		            strncpy(sdist, strary[0], stlen);
		            sdist[stlen] = '\0'; 
	                    cst_lcuc ( units, units, &ier );
                            cst_crnm ( sdist, &dist, &ier2 );
		        }
			else if ( (ityp == 1) && ( jj == 0 ) ) {
			    istuff = 1;
			    jj = lens;
			    ier = -7;
			    *nstn = 0;
			}
		        jj++;
	            }

		    if ( ier == 0 ) {
		       /*
		        * If have format code units, use those and 
		        * convert the distance.
		        */ 

		        if ( (istuff == 0) &&  ( ihund > 0 ) ) {
		            strcpy(sdist, strary[0] );
                            cst_crnm ( sdist, &dist, &ier );
		        }

	                if ( ( (strcmp ( units, "SM" ) ) == 0 ) || 
		           ( (istuff == 0) && ( ihund == 2 ))) {
                            cst_crnm ( sdist, &dist, &ier );
	    	            dist = dist * SM2M;
	                }
	                else if ( ( (strcmp ( units, "NM" ) ) == 0 ) ||
		           ((istuff == 0) && ( ihund == 1 ))) {
                            cst_crnm ( sdist, &dist, &ier );
		            dist = dist * NM2M;
	                }
	                else if ( ( (strcmp ( units, "KM" ) ) == 0 ) || 
		           ((istuff == 0) && ( ihund == 3 ))) {
                            cst_crnm ( sdist, &dist, &ier );
	        	    dist = dist * 1000.0F;
	                }
		        else {
                            *nstn = 0;
			    ier = -4;	
		        }
		        if ( dist < 0.0F ) {
                            *nstn = 0;
			    ier = -6;
		        }

                       /*
                        *   Get the direction from the station.
                        */

                        if ( ier == 0 ) {
	                    cst_alnm ( strary[1][0], &ityp, &ier );
	                    if (ityp == 1 ) {
                                clo_cmpdir ( strary[1], &fdir, &ier);
			    }
	                    else if (ityp == 2 ) {
                                cst_crnm ( strary[1], &fdir, &ier );
	                    }

                           /*
                            *   Get the new latitude and longitude.
                            */
                            if ( ier == 0 ) {
		                fdir = G_ABS(fdir);
                                clo_dltln ( &lat, &lon, &dist, &fdir, 
			                    &dlat, &dlon, &ier);
			    }
			    else {
                                *nstn = 0;
			        ier = -5;
			    }
		        }
		    }
	        }
                if ( ( inarr == 3 ) && ( ier == 0 ) ) {
                     flat[ii] = dlat; 
                     flon[ii] = dlon;
                }
		else if ( ( inarr == 1 ) && ( ier == 0 ) ) {
                     flat[ii] = lat; 
                     flon[ii] = lon;
		}
                else {
                    flat[ii] = RMISSD; 
                    flon[ii] = RMISSD;
                }
	    }
	    cptr =(char *)  strtok (NULL, ";");
	}
    }

   /* 
    * Free memory space.
    */

    for ( jj = 0; jj < 3; jj++ ) {
        free ( strary[jj] );
    }
    free (strary);

    *iret = ier;

}

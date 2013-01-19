#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

void clo_ddenc ( char *type, int format, float lat, float lon, char *str, 
								int *iret )
/************************************************************************
 * clo_ddenc                                                    	*
 *                                                                      *
 * This function returns gets the string for the seek and location      *
 * structure indicating which entry matches the input CLO name for the  *
 * given latitude and longitude. The format code is in a 4-5 digit      *
 * format (Eg. 5212 or 10212).  The columns are formatted as follows : 	*
 *					       				* 
 *									*
 *   ROUNDING        UNITS        DIRECTION     DISPLAY			*
 *                                              			*
 *  5 - nearest 5    0 - omit     0 - omit      0 - degrees		*
 * 10 - nearest 10   1 - NM       1 - 16 point  1 - decimal/minutes   	*
 *                   2 - SM       2 - degrees   2 - 1st column		*
 *                   3 - KM             	4 - 3rd column		*
 *									*
 * For DISPLAY, the 1st column is usually the station id and the 3rd    *
 * column is the name of the station, city or county.			* 
 *                                                                      *
 * clo_ddenc ( type, format, lat, lon, str, iret)                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	*type		char		Name of CLO parameter		*
 *	format		int		Indicator of format to use      *
 *      lat            float            Latitude point                  *
 *      lon            float            Longitude point                 *
 *									*
 * Output parameters:                                                   *
 *	*str		char		Character string location       *
 *	*iret		int		Return value			*
 *					=  < 0 - String not created	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		01/00	Create					*
 * A. Hardy/GSC		01/00	Added new format display option         *
 * A. Hardy/GSC		01/00	Added length chk of str;changed rounding*
 * A. Hardy/GSC		02/00	modified for all variations of formats  *
 * A. Hardy/GSC		02/00	reworked string display; city locations *
 * D.W.Plummer/NCEP	 8/00	changes for clo_ redesign		*
 * A. Hardy/GSC		 8/00   renamed from clo_format			*
 * T. Piper/GSC		 3/01	Fixed IRIX6 compiler warnings		*
 * D.W.Plummer/NCEP	 8/01	Repl clo_bqinfo w/ cst_gtag		*
 * D.W.Plummer/NCEP	 6/05	Tens digit sets # decimals for lat,lon	*
 ***********************************************************************/
{
    int		idist, icmp, nh, ier;
    int		ilat, ilon, imnt, imnn, isit, isin;
    char	stn[80], idx[80], *pidx, sdir[4];
    float	dist, dir;
    int         ione, itens, ihund, irnd, which, inlen;
    int         isln, iwidth, ilftovr;
    char        sdirc[5], sdist[5]; 
    char	info[128], fmt[20];
/*---------------------------------------------------------------------*/
    *iret = 0;
    strcpy ( str, "NULL" );
    iwidth = 16;

   /*
    * Parse out format into it's components.
    */

    ione  = format % 10;
    itens = (int) (format/10) % 10;
    ihund = (int) (format/100) % 10;
    irnd  = format / 1000;

   /*
    * Check one's place for lat-lon or deg-min.
    */

    if ( ione == 0 ) {	/* show lat/lon */
	/*
	 * Tens digit controls the number of decimal digits for the
	 * lat,lon display. The default is 2 digits.
	 */
	if ( itens == 0 )  itens = 2;
	sprintf(fmt,"%%.%df, %%.%df", itens, itens );
        sprintf(str, fmt, lat, lon);
    }
    else if ( ione == 1 ) {   /* show lat/lon as deg-min */
	isit = ( lat < 0.0F ) ? '-' : ' ';
	ilat = (int) G_ABS ( lat ); 
	imnt = G_NINT ( ( G_ABS(lat) - (float)ilat ) * 60.0F );
	if  ( imnt >= 60 )  {
	    imnt = imnt % 60;
	    ilat += 1;
	}

	isin = ( lon < 0.0F ) ? '-' : ' ';
	ilon = (int) G_ABS ( lon );
	imnn = G_NINT ( ( G_ABS(lon) - (float)ilon ) * 60.0F );
	if  ( imnn >= 60 )  {
	    imnn = imnn % 60;
	    ilon += 1;
	}

	sprintf ( str, "%c%3d:%02d, %c%3d:%02d",
	               isit, ilat, imnt, isin, ilon, imnn );
    }

    else {   /* show city/county/stn  */
        which = clo_which ( type );
        
	if ( clo.loc[which].format == 1 ) { 		/* show bound */

	    clo_tqbnd ( type, lat, lon, idx, &ier);
	    pidx = idx;

           /*  
            *   Find and save the county FIPS id.  
	    */

	    if ( ione == 2) {          

	        if (strcmp ( pidx,"-") != 0 ) {
		    clo_bginfo ( type, 0, info, &ier );
		    cst_gtag ( "FIPS", info, "?", str, &ier );
		}
		else {
	            cst_split (pidx, ' ', 14, str, &ier);
		}
	    }
	    if ( ione == 4) {  /* Save the bound name */
	        cst_split (pidx, ' ', 14, str, &ier);
	    }
	}


        else {
	    if ( clo.loc[which].format == 0 ) { 	/* show station */

		/*
		 *  get station ID, distance and direction.
		 */
	        clo_tdirect ( type, lat, lon, stn, &dist, &dir, &ier );

	        if ( ione == 4 ) {
		    /*
		     *  Replace station ID w/ station name.
		     */
		    clo_tgnm ( type, 1, sizeof(stn), &nh, stn, &ier );
	        }
	    }  

	     if ( ihund == 0 ) {
	          strcpy ( sdirc, "" );
	     }
	     else {
	         if ( ihund == 1 ) {           /* get nautical miles */
	    	     dist *= M2NM;
	         }
	         else if ( ihund == 2 ) {      /* get statute miles */
	             dist *= M2SM;
	         }
	         else if ( ihund == 3 ) {      /* get kilometers */
	             dist /= 1000.0F;
	         }

	         if ( irnd > 0 ) {
	             idist = G_NINT ( dist / (float)irnd ) * irnd;
		     sprintf ( sdirc, "%i ", idist);
	         }
	         else if ( irnd < 0 ) {
		     irnd = 1;
	             idist = G_NINT ( dist / (float)irnd ) * irnd;
		     sprintf ( sdirc, "%i ", idist);
	         }
	         else if ( irnd == 0 ) {
	             strcpy ( sdirc, "" );
	         }

	     }

	     if ( itens == 0 ) {  /* omit the direction */
	         strcpy ( sdist, "" );
	     }
             else {
	         if ( itens == 1 ) {      /* use 16 point dir. */
		      clo_compass ( &dir, sdir, &icmp, &ier );
		      sprintf ( sdist, "%s", sdir );
	         }
	         else if  ( itens == 2 ) {      /* use degrees  */
		      sprintf ( sdist, "%.0f", dir );
	         }
	     }

	     sprintf(str, "%s %s",sdirc, sdist);

	    /*
	     * If the stn name is longer than 4 chars, print
	     */

	     inlen = (int)strlen(stn);
	     isln = (int)strlen(str);
	     ilftovr = iwidth - isln;

	     if (inlen > 4 )  {
	         sprintf ( str, "%*s %.*s", isln, str, ilftovr, stn );
	     }
	     else {
	         sprintf ( str, "%s %3s", str, stn );
	     }  

	     if ( (ihund == 0 ) && ( itens == 0 ) ) {
	         sprintf ( str, "%.*s", ilftovr, stn );
	     }
        }
    }
    if ( strcmp ( str, "NULL") != 0 ) *iret = -1;
}

#include "geminc.h"
#include "gemprm.h"

#define  EOL  "\n"
#define  LINE_LEN  66 

void utl_ugcp ( char **ugc_arr, int *nugc, char *eday, char *ehour, 
            int *len1, char *ugcstr, int *iret )
/************************************************************************
 * utl_ugcp    								*
 *                                                                      *
 * This function appends a list of UG codes to one another, separated   *
 * by the '-' character.  The ending time, in the format (DDHHMM), is   *
 * appended to the UG coded string.    					*
 *                                                                      *
 * utl_ugcp ( ugc_arr, nugc, eday, ehour, len1, ugcstr, iret )          *
 *                                                                      *
 * Input parameters:                                                    *
 *    **ugc_arr		char		UG codes array			*
 *    *nugc		int		Number of UG codes 		*
 *    *eday		char		Date (DD)			*
 *    *ehour		char		Hour (HH)			*
 *    len1		int		Max length of 'ugcstr'		*
 *                                                                      *
 * Output parameters:                                                   *
 *    *ugcstr		char		UG codes string			*
 *    *iret		int    		Return Code  			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP     5/03   						*
 * A. Hardy/NCEP     8/03  	Reworked line lengths to be <= 66 chars	*
 * A. Hardy/NCEP    12/03  	Fixed lengths for 62 chars. 		*
 * A. Hardy/NCEP    12/03  	Fixed lengths for marine zones 		*
 * A. Hardy/NCEP    12/03  	Fixed lengths for 65 chars. 		*
 * T. Piper/SAIC    02/04	Removed unused variables jj and done	*
 * D. Kidwell/NCEP   2/05   	Changed itype from 1 to 2               *
 ***********************************************************************/
{
    int     ii, icnt, ier, lenug, iln, itype, inum; 
    int     iugc, iugcnt, lens, ilen, isit, ispot;
    char    stid[3], stabr[3], tmpw[500], holdw[500];
    char    **zon_arr, zor[2], **cty_arr, day[3], tmpstr[120];
    Boolean oneln;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ii    = 0;
    iln   = 2;
    iugc  = 0;
    icnt  = 0;
    inum  = 0;
    lenug = 7;
    itype = 2;
    lens = 7;
    oneln = True;  
    iugcnt = 1;
    tmpw[0] = '\0';
    tmpstr[0] = '\0';
    holdw[0] = '\0';
    ugcstr[0] = '\0';

   /*
    * Forward sorting of the UG code array. 
    */

    zon_arr = (char **)malloc((*nugc) * sizeof(char *));
    cty_arr = (char **)malloc((*nugc) * sizeof(char *));

    cst_sort ( itype, nugc, ugc_arr, nugc, ugc_arr, &ier );

   /*
    * Check first UGC if it is a zone.
    */

    for ( iugc = 0; iugc < *nugc; iugc++ ) {
        cst_ncpy ( zor, ugc_arr[iugc]+2, 1, &ier );
        if (strcmp (zor, "Z") == 0 ) {
	    zon_arr[icnt] = (char *)malloc((7) * sizeof(char));
	    strcpy( zon_arr[icnt], ugc_arr[iugc] );
	    icnt++;
	}
	else {
	    cty_arr[inum] = (char *)malloc((7) * sizeof(char));
	    strcpy( cty_arr[inum], ugc_arr[iugc] );
	    inum++;
	}
    }

   /*
    * Loop through UG COUNTY array.
    */

    iugc = 0;
    if ( inum > 0 ) {
       /*
        * Store the first state/area id.
        */

        cst_ncpy ( stid, cty_arr[0], iln, &ier );

        cst_ncpy ( tmpw, cty_arr[iugc], lenug, &ier);
        strcat( tmpw, "-" );

        while ( iugc < inum ) {
            iugc++;

           /*
            * Create the UGC county portion of the string first.
            */

            if ( iugc < inum ) {
                cst_ncpy ( stabr, cty_arr[iugc], iln, &ier );

                if ( strcmp(stabr, stid) == 0 ) {
                    sprintf( holdw, "%s",cty_arr[iugc]+3 );
    	            strcat( tmpw, holdw );
                    strcat( tmpw, "-" );
                    iugcnt++;
		    ilen = 4;
		    lens = lens + ilen;
                }
    	        else {
                    cst_ncpy ( stid, cty_arr[iugc], iln, &ier );
                    sprintf( holdw, "%s",cty_arr[iugc] );
    	            strcat( tmpw, holdw );
                    strcat( tmpw, "-" );
                    iugcnt++;
		    ilen = 7;
		    lens = lens + ilen;
    	        }

	       /*
	        * Add end of line character if line length will
		* exceed or is 66 characters long.
		*/
		if ( (oneln) && ((lens == LINE_LEN ) || ( lens == 63 ) ) ) {
                    strcat (tmpw, EOL );
                    strcat( ugcstr, tmpw);
                    tmpw[0] = '\0';
                    iugcnt = 0;
    	            oneln = False;
		    lens = 0;
		}
		else if ( lens == 64 ) {
                    strcat (tmpw, EOL );
                    strcat( ugcstr, tmpw);
                    tmpw[0] = '\0';
                    iugcnt = 0;
    	            oneln = False;
		    lens = 0;
		}
		else if ( lens >= 65 )  {
		    ispot = LINE_LEN-1;
		    if ( lens > 65 ) {
		        cst_alnm ( tmpw[ispot], &isit, &ier );
		        while ( isit != 0 ) {
			    ispot--;
		            cst_alnm ( tmpw[ispot], &isit, &ier );
		        }	
		    }

		    cst_ncpy ( tmpstr, tmpw, ispot+1, &ier);
                    strcat (tmpstr, EOL );
                    strcat( ugcstr, tmpstr);
		    lens = lens - ispot;

		   /*
		    * If line length is 65 chars. don't append anything.
		    */

		    if ( lens > 3 ) {
                        strcat( ugcstr, tmpw+(ispot+1));
		    }
                    tmpstr[0] = '\0';
                    tmpw[0] = '\0';
		}
            }
        }
    }

   /*
    * If lenght of line is 62 and going to add zones, add EOL character.
    */

    if ( lens == 62 ){
        strcat (tmpw, EOL );
        strcat( ugcstr, tmpw);
        tmpw[0] = '\0';
        iugcnt = 0;
    	oneln = False;
	lens = 0;
    }

   /*
    * Loop through UG ZONES array.
    */

    iugc = 0;
    if ( icnt > 0 ) {
	if ( iugcnt != 0 ) strcat( ugcstr, tmpw);
       /*
        * Store the first state/area id.
        */

        cst_ncpy ( stid, zon_arr[0], iln, &ier );
        cst_ncpy ( tmpw, zon_arr[iugc], lenug, &ier);
        strcat( tmpw, "-" );
	if ( inum > 0 ) {
	    lens = lens + 7;
	}

       /*
        * This length check adds an EOL character if the last item on
	* a line is a marine zone and the next to be appended will make
	* the line longer than 66 characters.
	*/

        if ( lens >=  61 ){
            strcat (tmpw, EOL );
            strcat( ugcstr, tmpw);
            tmpw[0] = '\0';
            iugcnt = 0;
    	    oneln = False;
	    lens = 0;
        }

        while ( iugc < icnt-1 ) {
            iugc++;

           /*
            * Create the UGC county portion of the string first.
            */

            cst_ncpy ( stabr, zon_arr[iugc], iln, &ier );
            if ( iugc < icnt ) {

                if ( strcmp(stabr, stid) == 0 ) {
                    sprintf( holdw, "%s",zon_arr[iugc]+3 );
    	            strcat( tmpw, holdw );
                    strcat( tmpw, "-" );
                    iugcnt++;
		    ilen = 4;
		    lens = lens + ilen;
                }
    	        else {
                    cst_ncpy ( stid, zon_arr[iugc], iln, &ier );
                    sprintf( holdw, "%s",zon_arr[iugc] );
    	            strcat( tmpw, holdw );
                    strcat( tmpw, "-" );
                    iugcnt++;
		    ilen = 7;
		    lens = lens + ilen;
    	        }

               /*
	        * Add end of line character if line length will
		* exceed or is 66 characters long.
		*/

		if ( (oneln) && ( (lens == LINE_LEN ) || ( lens == 63 ) ) ) {
                    strcat (tmpw, EOL );
                    strcat( ugcstr, tmpw);
                    tmpw[0] = '\0';
                    iugcnt = 0;
    	            oneln = False;
		    lens = 0;
		}
		else if ( lens == 64 ) {
                    strcat (tmpw, EOL );
                    strcat( ugcstr, tmpw);
                    tmpw[0] = '\0';
                    iugcnt = 0;
    	            oneln = False;
		    lens = 0;
		}
		else if ( lens >=  65 )  {
		    ispot = LINE_LEN-1;
		    if ( lens > 65 ) {
		        cst_alnm ( tmpw[ispot], &isit, &ier );
		        while ( isit != 0 ) {
			    ispot--;
		            cst_alnm ( tmpw[ispot], &isit, &ier );
		        }	
		    }

		    cst_ncpy ( tmpstr, tmpw, ispot+1, &ier);
                    strcat (tmpstr, EOL );
                    strcat( ugcstr, tmpstr);
		    lens = lens - ispot;

		   /*
		    * If line length is 65 chars. don't append anything.
		    */

		    if (lens > 3 ) {
                        strcat( ugcstr, tmpw+(ispot+1));
		    }

                    tmpstr[0] = '\0';
                    tmpw[0] = '\0';
		}
            }
        }
    }

   /*
    * Append the ending day, hour and minutes to the end of the UG string.
    */

    cst_lstr ( eday, &lens, &ier );
    if ( lens < 2 ) {
        sprintf( day, "0%s", eday);
    }
    else {
        sprintf( day, "%s", eday);
    }
    sprintf( holdw, "%s%s-", day, ehour);

   /*
    * Check that the time string won't extend beyond 
    * 66 chars. for the line.
    */

    cst_lstr ( tmpw, &lens, &ier );
    if ( lens <= 55 ) {
        strcat( tmpw, holdw );
    }
    else {
        strcat (tmpw, EOL );
        strcat( tmpw, holdw );
    }
    strcat( ugcstr, tmpw);

   /*
    * Free memory from the county and/or zone string arrays.
    */

    for( ii = 0; ii < icnt; ii++ )
        free( zon_arr[ii] );
    for( ii = 0; ii < inum; ii++ )
        free( cty_arr[ii] );

    if ( zon_arr )
         free( (char **) zon_arr ); 
    if ( cty_arr )
         free( (char **) cty_arr ); 
}

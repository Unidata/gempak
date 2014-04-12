#include "geminc.h"
#include "gemprm.h"

/* Internal function prototype */
Boolean PathTimeCheck ( char *time,
                        char *stime,
                        char *etime ); 

/************************************************************************
 * ggrtrk.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/
void gg_rtrk ( char *filtyp, char *filnam, char *stime, char *etime,
		int *itmclr, int *iskip, int *iret );

/*=====================================================================*/

void gg_rtrk ( char *filtyp, char *filnam, char *stime, char *etime,
		int *itmclr, int *iskip, int *iret )
/************************************************************************
 * gg_rtrk								*
 *									*
 * This routine reads the data from a Altimeter Ground Track Prediction *
 * file and plots it.							*
 *									*
 * gg_rtrk ( filtyp, filnam, stime, etime, itmclr, iskip, iret )        *
 *									*
 *									*
 * Input parameters:							*
 *	*filtyp		char		Data type			*
 *	*filnam		char		Data file name			*
 *	*stime		char		Start time of data 		*
 *	*etime		char		End time of data 		*
 * 	*itmclr		int		Time stamp color		*
 *	*iskip		int		Skip value			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * G. McFadden/SAIC	12/08	Modeled after gg_qsrd			*
 * S. Jacobs/NCEP	 6/10	Removed underscore on function name	*
 ***********************************************************************/
{
	int	ier, incr, itarr[5], iyoff = 0,
		ixoff = -10, ntime, np2, pcount, gyear, ymd, hm;
	FILE    *fptr;
	char    buffer[256], textstr[8], final_ts[9], ymd_string[7], hm_string[5];
        char    gempak_dt[12];
        char    *cp, underscore[2];
	float   lat, lon, rotat = 0.0F;
	double	dlat, dlon, dalt;
        size_t  two = 2;
/*---------------------------------------------------------------------*/
/*
 *  Open the data file.
 */
	fptr = cfl_ropn ( filnam, NULL, &ier );
	if ( fptr == NULL  ||  ier != 0 )  {
	    *iret = -1;
	    return;
	}
	*iret = 0;

        underscore[0] = '_';
        underscore[1] = '\0';

/*
 * Set the skip factor.
 */
        if  ( *iskip <= 0 ) {
            incr = 1;
        }
        else {
            incr = *iskip + 1;
        }

        pcount = incr;
        while  ( ier == 0 )  {

/*
 *  Read this prediction's data.
 */
	    cfl_trln( fptr, 256, buffer, &ier );
	    if ( ier != 0 ) {
/*
 *  Bad read...close the file and exit.
 */
     		cfl_clos ( fptr, &ier );
		return;
	    }
/*
 *  Extract this prediction's data.
 */
	    sscanf( buffer, "%d %d %d %d %d %lf %lf %lf",
			&itarr[0], &itarr[1], &itarr[2], &itarr[3],
			&itarr[4], &dlat, &dlon, &dalt );

	    lat = (float)dlat;
            lon = (float)dlon;
/*
 *  Get the gempak date/time of this prediction
 */
            gyear = itarr[0] - 2000;
            ymd  = gyear * 10000 + itarr[1] * 100 + itarr[2];
            sprintf(ymd_string, "%6.6d", ymd );

            hm = itarr[3] * 100 + itarr[4];
            sprintf(hm_string, "%4.4d", hm );

            gempak_dt[0] = '\0';
            strcat(gempak_dt,ymd_string);
            strcat(gempak_dt,"/");
            strcat(gempak_dt,hm_string);

/*
 *  The longitude is stored as 0 -> 360.  PRNLON will correct the longitude to
 *  the range -180 -> 180.
 */
            np2 = 1;
            prnlon( &np2, &lon, &ier );

/*
 *  Plot the prediction if it is not skipped and if it is within the valid
 *  time range...plot "DD_HHMM" on the screen.
 */
            ntime = itarr[2] * 10000 + itarr[3] * 100 + itarr[4];
	    sprintf(textstr, "%6.6d", ntime);
            final_ts[0] = '\0';
            strncpy(final_ts,textstr,two);
            final_ts[2] = '\0';
            strcat(final_ts,underscore);
            cp = textstr + 2;
            strcat(final_ts,cp);
                 
	    gscolr ( itmclr, &ier);
            if ( pcount % incr == 0 && PathTimeCheck( gempak_dt, stime, etime) ) {
	       gtext  ( sys_M, &lat, &lon, final_ts, &rotat, &ixoff, &iyoff,
	                &ier, strlen(sys_M), strlen(final_ts) );
            }

            pcount++;

	}
/*
 *  Close the file.
 */
	cfl_clos ( fptr, &ier );
}

/*=====================================================================*/

Boolean PathTimeCheck ( char *time, char *stime, char *etime )
/************************************************************************
 * PathTimeCheck                                                        *
 *                                                                      *
 * This function uses the start and end times to determine if the       *
 * data is within the time range and may be plotted.                    *
 *                                                                      *
 * Boolean PathTimeCheck ( itime, stime, etime )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      itime[]         short           Data time array                 *
 *      *stime          char            Start time of range             *
 *      *etime          char            End time of range               *
 *                                                                      *
 * Return parameters:                                                   *
 *      PathTimeCheck   Boolean       Indicator for valid time          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. McFadden/SAIC       12/08         Based on QStimeCheck            *
 * G. McFadden/IMSG       01/14         Corrected function name in this *
 *                                      documentation block.            *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

        if ( ( strcmp( time, stime ) >= 0 ) && ( strcmp( time, etime ) <= 0 ) )
           return ( TRUE );
         else
           return ( FALSE ); 
}


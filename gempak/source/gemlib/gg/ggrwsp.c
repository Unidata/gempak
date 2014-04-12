#include "geminc.h"
#include "gemprm.h"

/* Useful parameters */
#define NUMPLT	  20

/************************************************************************
 * ggrwsp.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/
void gg_rwsp_ ( char *filtyp, char *filnam, float shainc[], int icolrs[],
		int *numclr, int *iskip, int *interv, int *itmclr,
		int *iret );

/*=====================================================================*/

void gg_rwsp_ ( char *filtyp, char *filnam, float shainc[], int icolrs[],
		int *numclr, int *iskip, int *interv, int *itmclr,
		int *iret )
/************************************************************************
 * gg_rwsp								*
 *									*
 * This routine reads windspeed data from an altimeter-derived ASCII  	*
 * file and plots it.							*
 *									*
 * gg_rwsp ( filtyp, filnam, shainc, icolrs, numclr, iskip, interv,	*
 *		itmclr, iret )						*
 *									*
 * Input parameters:							*
 *	*filtyp		char		Data type			*
 *	*filnam		char		Data file name			*
 *	shainc []	float		WINDSPEED break points		*
 *	icolrs []	int		WINDSPEED colors		*
 *	*numclr		int		Number of colors		*
 *	*iskip		int		Skip value			*
 *	*interv		int		Time stamp interval		*
 * 	*itmclr		int		Time stamp color		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * G. McFadden/IMSG	11/13	Modeled after gg_rwav			*
 ***********************************************************************/
{
	int	ier, incr, itarr[5], jgrp, iyoff = 0,
		ixoff = -10, jj, ktime, ktime0 = -9999, lincnt = 0,
		ntime, numc;
	FILE    *fptr;
	char    buffer[256], textstr[8];
	float   lat, lon, rotat = 0.0F;
	double	dlat, dlon, data, dum;
	Boolean proc;
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

	numc = *numclr;
	if  ( numc > NUMPLT )  {
	    numc = NUMPLT;
	}

/*
 *  Set the skip factor.
 */
	if  ( *iskip <= 0 ) {
	    incr = 1;
	}
	else {
	    incr = *iskip + 1;
	}

	while  ( ier == 0 )  {

/*
 *  Read the data.
 */
	    cfl_trln( fptr, 256, buffer, &ier );
	    if ( ier != 0 ) {
		if ( fptr != NULL ) cfl_clos ( fptr, &ier );
		return;
	    }
            sscanf( buffer, "%i %i %i %i %i %lf %lf %lf %lf",
			&itarr[0], &itarr[1], &itarr[2], &itarr[3],
			&itarr[4], &dlat, &dlon, &dum, &data );
/*
 *  Check the line count against the skip factor.  If the line count is a
 *  multiple of the increment, then process the line of data.
 *  Otherwise, skip the line.
 */
	    if  ( lincnt % incr == 0 )  {
	        proc = TRUE;
	    }
	    else {
	        proc = FALSE;
	    }
	    lincnt++;

/*
 *  Plot time stamps.
 */
	    lat = (float)dlat;
            lon = (float)dlon;
	    if ( *itmclr > 0 ) {
		ktime = itarr[3] * 60 + itarr[4];
		if ( (ktime % *interv) == 0 && ktime != ktime0) {
		    ntime = itarr[3] * 100 + itarr[4];
		    sprintf(textstr, "%4.4d", ntime); 
		    gscolr ( itmclr, &ier);
		    gtext  ( sys_M, &lat, &lon, textstr, &rotat, &ixoff, &iyoff,
				&ier, strlen(sys_M), strlen(textstr) );
		    ktime0 = ktime;
		}
	    }

/*
 *  Plot wind speed.
 */
	    if  ( proc ) {
	        if ( ERMISS(data) ) {
	 	    sprintf(textstr, "M");
		}
		else {
		    sprintf(textstr, "%4.2lf", data);
		    jgrp = numc - 1;
		    for  ( jj = numc - 1; jj >= 0; jj-- )  {
		        if  ( data <= shainc[jj] )  {
			    jgrp = jj;
		        }
		    }
	  	    gscolr ( &icolrs[jgrp], &ier );
		}
		gtext(sys_M, &lat, &lon, textstr, &rotat, &iyoff, &iyoff,
			&ier, strlen(sys_M), strlen(textstr) );
	    }
	}
/*
 *  Close the file.
 */
	cfl_clos ( fptr, &ier );
}

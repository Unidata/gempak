#include "geminc.h"
#include "gemprm.h"

/* Useful parameters */
#define NUMPLT	  20

/************************************************************************
 * ggrwav.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/
void gg_rwav ( char *filtyp, char *filnam, float shainc[], int icolrs[],
		int *numclr, int *iskip, int *interv, int *itmclr,
		int *iret );

/*=====================================================================*/

void gg_rwav ( char *filtyp, char *filnam, float shainc[], int icolrs[],
		int *numclr, int *iskip, int *interv, int *itmclr,
		int *iret )
/************************************************************************
 * gg_rwav								*
 *									*
 * This routine reads the data from a Significant Wave Height or	*
 * Sea-Surface Height Anamoly file and plots it.			*
 *									*
 * gg_rwav ( filtyp, filnam, shainc, icolrs, numclr, iskip, interv,	*
 *							itmclr, iret )	*
 *									*
 * Input parameters:							*
 *	*filtyp		char		Data type			*
 *	*filnam		char		Data file name			*
 *	shainc []	float		WAVE break points		*
 *	icolrs []	int		WAVE colors			*
 *	*numclr		int		Number of colors		*
 *	*iskip		int		Skip value			*
 *	*interv		int		Time stamp interval		*
 * 	*itmclr		int		Time stamp color		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * T. Piper/SAIC	03/06	Modeled after gg_qsrd			*
 * G. McFadden/SAIC	01/08	Added support for ENVI and GFO 		*
 * 				Significant Wave Height			*
 * G. McFadden/SAIC	06/08	Added support for Jason2 		*
 * 				Significant Wave Height			*
 * G. McFadden/IMSG	07/11	Added support for CRYOSAT		*
 * 				Significant Wave Height			*
 * S. Jacobs/NCEP	 6/13	Added file close, if the read returns	*
 * 				a non-zero value, before returnning	*
 * G. McFadden/IMSG	07/13	Added support for Altika wind speeds	*
 * 				and Significant Wave Height		*
 * G. McFadden/IMSG     11/13   Moved support for Altika wind speeds to *
 * 				gg_rwsp 				*
*************************************************************************/
{
	int	blen, ier, imin, incr, itarr[5], jgrp, iyoff = 0,
		ixoff = -10, jj, ktime, ktime0 = -9999, lincnt = 0,
		ntime, numc, one = 1, pntnum,
		reftime[5] = { 1985, 1, 1, 0, 0 };
	FILE    *fptr;
	char    buffer[256], textstr[8];
	float   lat, lon, rotat = 0.0F;
	double	day2min = 1440.0, days, dlat, dlon, minuts, data, dum;
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
	    if ( strcmp(filtyp, "SGWH") == 0 || strcmp(filtyp, "SGWHE") == 0 ||
                 strcmp(filtyp, "SGWHG") == 0 || strcmp(filtyp, "SGWH2") == 0 ||
                 strcmp(filtyp, "SGWHC") == 0 || strcmp(filtyp, "SGWHA") == 0 ) {
		sscanf( buffer, "%i %i %i %i %i %lf %lf %lf %lf",
			&itarr[0], &itarr[1], &itarr[2], &itarr[3],
			&itarr[4], &dlat, &dlon, &data, &dum );
	    }
	    else {
		cst_lstr (  buffer, &blen, &ier );
		if ( ier == 0 && blen > 40 && blen < 60 ) {
		    sscanf( buffer, "%d %lf %lf %lf %lf",
			    &pntnum, &dlat, &dlon, &days, &data );
		    data = data * 100.0;  /* Convert data from meters to cm */
		}
	        else {
		    continue;
	        }
	    }
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
		if ( strcmp(filtyp, "SSHA") == 0 ) {
		    minuts = days*day2min;
		    imin = (int)minuts;
		    ti_addm(reftime, &imin, itarr, &ier);
		}
		ktime = itarr[3] * 60 + itarr[4];
		if ( (ktime % *interv) == 0 && ktime != ktime0) {
/*
 *  The longitude is stored as 0 -> 360.  PRNLON will correct the longitude to
 *  the range -180 -> 180.
 */
		    ntime = itarr[3] * 100 + itarr[4];
		    sprintf(textstr, "%4.4d", ntime); 
		    gscolr ( itmclr, &ier);
		    gtext  ( sys_M, &lat, &lon, textstr, &rotat, &ixoff, &iyoff,
				&ier, strlen(sys_M), strlen(textstr) );
		    ktime0 = ktime;
		}
	    }

	    if  ( proc ) {
		if ( strcmp(filtyp, "SGWH") == 0 || strcmp(filtyp, "SGWHE") == 0 ||
                     strcmp(filtyp, "SGWHG") == 0 || strcmp(filtyp, "SGWH2") == 0 ||
                     strcmp(filtyp, "SGWHC") == 0 || strcmp(filtyp, "SGWHA") == 0 ) { 
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
		else {
		    prnlon ( &one, &lon, &ier );
		    jgrp = numc - 1;
		    for  ( jj = numc - 1; jj >= 0; jj-- )  {
			if  ( data <= shainc[jj] )  {
			    jgrp = jj;
			}
		    }
		    gscolr ( &icolrs[jgrp], &ier );
		    gmark(sys_M, &one, &lat, &lon, &ier, strlen(sys_M));
		}
	    }
	}
/*
 *  Close the file.
 */
	cfl_clos ( fptr, &ier );
}

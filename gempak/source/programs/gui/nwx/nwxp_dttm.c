#include "nwx_cmn.h"

/************************************************************************
 * nwxp_dttm.c                                                          *
 *                                                                      *
 * This module deals with date/time data structure.          		*
 *                                                                      *
 * CONTENTS:                                                            *
 *      dttm_cnvt() converts character date/time into integer year,     *
 *			month, day, hour. 				*
 *      dttm_cmp()  compare two date/time structures.     		*
 *      dttm_cpy()  copy the second date/time structure into the first. *
 ***********************************************************************/

/*=====================================================================*/

void dttm_cnvt ( char *dattm, int *iyear, int *imonth, int *iday, 
						int *ihour, int *iret )
/************************************************************************
 * dttm_cnvt								*
 *									*
 * This routine will convert a character date/time to individual	*
 * integers for year, month, day and hour.				*
 *									*
 * dttm_cnvt ( dattm, iyear, imonth, iday, ihour, iret )		*
 *									*
 * Input parameters:							*
 *	*dattm		char		Requested date/time		*
 *									*
 * Output parameters:							*
 *	*iyear		int		Year of century			*
 *	*imonth		int		Month				*
 *	*iday		int		Day				*
 *	*ihour		int		Hour 				*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 ***********************************************************************/
{
int		itmp, ier;
char		*tpos, tstr[21];

/*---------------------------------------------------------------------*/
	cst_lcuc( dattm, dattm, &ier );

/*
 * Check for a slash in the string.
 */
	if  ( ( tpos = strchr( dattm, '/' ) ) != NULL ) {

/*
 * The slash is present. Parse the input around the slash.
 *
 * Convert the first part to an integer and separate the
 *	 year, month and day.
 */
	    strncpy( tstr, dattm, (size_t)(tpos-dattm) );
	    tstr[tpos-dattm+1] = CHNULL;

	    itmp = atoi ( tstr );
	    *iyear  =   itmp / 10000;
	    *imonth = ( itmp % 10000 ) / 100;
	    *iday   =   itmp % 100;

/*
 * Convert the second part to an integer and get the hour.
 */
	    if  ( strcmp( tstr, "LAST" ) == 0 ) {
		*ihour = 99;
	    }
	    else {
		strcpy( tstr, tpos+1 );
		itmp = atoi ( tstr );
		if  ( itmp < 100 )
		    *ihour = itmp;
		else
		    *ihour = itmp / 100;
	    }
	}
	else {

/*
 * The slash is not present. Assume that the input is only
 * the hour.
 */
	    *iyear  = 0;
	    *imonth = 0;
	    *iday   = 0;

	    if  ( strcmp( dattm, "LAST" ) == 0 ) {
		*ihour = 99;
	    }
	    else {
		itmp = atoi( dattm );
		if  ( itmp < 100 )
		    *ihour = itmp;
		else
		    *ihour = itmp / 100;
	    }
	}
	*iret = 0;
}

/*=====================================================================*/

int dttm_cmp ( struct date_time_info dttm1, struct date_time_info dttm2 )
/************************************************************************
 * dttm_cmp								*
 *									*
 * This routine will compare two date/time structures. 			*
 *									*
 * int dttm_cmp ( dttm1, dttm2 )					*
 *									*
 * Input parameters:							*
 *	dttm1		struct date_time_info	date/time structure	*
 *	dttm2		struct date_time_info	date/time structure	*
 *									*
 * Return code:								*
 * dttm_cmp	int		0  -- dttm1=dttm2 (equal)		*
 *				1  -- dttm1>dttm2 (greater than)	*
 *	       -1  -- dttm1>dttm2 (smaller than)			*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	 	9/95						*
 ***********************************************************************/
{
    if ((dttm1.year == dttm2.year) &&
	(dttm1.month == dttm2.month) &&
	(dttm1.day == dttm2.day) &&
	(dttm1.hour == dttm2.hour) )
	return(0);
    else {
	if ( ( dttm1.year > dttm2.year) ||
		(dttm1.month > dttm2.month) ||
		(dttm1.day > dttm1.day) ||
		(dttm2.hour > dttm2.hour) )
	    return(1);
	else
	    return(-1);
    }
}

/*=====================================================================*/

void dttm_cpy ( struct date_time_info *dttm1, struct date_time_info dttm2 )
/************************************************************************
 * dttm_cpy								*
 *									*
 * This routine will copy the second date/time structure into the first.*
 *									*
 * dttm_cpy ( dttm1, dttm2 )						*
 *									*
 * Input parameters:							*
 *	*dttm1          struct date_time_info   date/time structure	*
 *	dttm2		struct date_time_info	date/time structure	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	 	 9/95						*
 * S. Danz/AWC		12/00	Changed output parm to a pointer	*
 ***********************************************************************/
{
	dttm1->year  = dttm2.year;
	dttm1->month = dttm2.month;
	dttm1->day   = dttm2.day;
	dttm1->hour  = dttm2.hour;
}

#include "nsncmn.h"

void nsn_gtim ( int index, dttms_t endtim, int mrange, int intrvl,
		Boolean jflag, int *idelrt, int *ntime, dttms_t timarr[],
		int *iret )
/************************************************************************
 * nsn_gtim								*
 *									*
 * This routine returns an array of times for the requested SND data.	*
 *									*
 * nsn_gtim ( index, endtim, mrange, intrvl, jflag, idelrt, ntime,	*
 * 		timarr,	iret)						*
 *									*
 * Input parameters:							*
 *	index		int		Index to attribute arrays	*
 *	endtim		dttms_t		End time of range		*
 *	mrange		int		Minutes in time range		*
 *	intrvl		int		Minutes in time interval	*
 *	jflag		Boolean		Reference time flag		*
 *									*
 * Input and output parameters:						*
 *	*idelrt		int		Minutes in delta reference time	*
 *									*
 * Output parameters:							*
 *	*ntime		int		Number of times in array	*
 *	timarr[ntime]	dttms_t		Array of times			*
 *	*iret		int		Return code			*
 *					   As for NIM_TLST		*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSND -> MAXTMPLT			*
 * R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
 * T. Lee/SAIC		08/03	Added time interval to calling sequence	*
 * T. Lee/SAIC		01/04	Added reference time flag		*
 * T. Lee/SAIC		04/04	Added delta reference time		*
 ***********************************************************************/
{

	int		iflag, nt, lenstr, ipos1, ipos2, ier, ierr;
	char		timstr[MXNMFL*20];

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Check for a valid index value.
 */
	if  ( index < 0 || index >= MAXTMPLT )  {
	    *iret = -1;
	    return;
	}

/*
 *	Clear the output string.
 */
	memset ( timstr, 0, sizeof(timstr) );

/*
 *	Given the image type and info, get a list of the MAXTIM latest
 *	times in a single string, separated by semi colons.
 */
	iflag = (int) jflag; 
	nsn_tlst ( snddt[index].alias, snddt[index].cycle,
		   &(snddt[index].isbcat), endtim, &mrange, &intrvl,
		   &iflag, idelrt, timstr, &lenstr, &nt, iret,
		   strlen ( snddt[index].alias ),
		   strlen ( snddt[index].cycle ),
		   strlen ( endtim ), sizeof ( timstr ) );

/*
 *	If there is an error, set the number of times to 0 and return.
 */
	if  ( *iret != 0 )  {
	    *ntime = 0;
	    return;
	}

/*
 *	Add a NULL to the end of the string of times.
 */
	timstr[lenstr] = CHNULL;

/*
 *	Parse the string of times into an array of times.
 */
	ierr   = 0;
	*ntime = 1;
	ipos1  = 0;
	while ( ierr == 0 ) {

/*
 *	    Find the next semi colon.
 */
	    cst_nocc ( timstr, ';', *ntime, 1, &ipos2, &ierr );
	    if  ( ierr != 0 )  ipos2 = strlen(timstr) + 1;

/*
 *	    Copy the time into the array.
 */
	    cst_ncpy ( timarr[*ntime-1], &timstr[ipos1],
		       ipos2-ipos1, &ier );

/*
 *	    Update the string position counter.
 */
	    ipos1 = ipos2 + 1;

/*
 *	    If there is no error, increment the time counter.
 */
	    if  ( ierr == 0 )  (*ntime)++;

	}

}

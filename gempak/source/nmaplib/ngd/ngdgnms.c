#include "ngdcmn.h"

void ngd_gnms ( char *alias, int *nname, nmlst_t namarr[], int *iret )
/************************************************************************
 * ngd_gnms								*
 *									*
 * This routine returns an array of storm or volcano names for the	*
 * requested GRID data.							*
 *									*
 * ngd_gnms ( alias, nname, namarr, iret )				*
 *									*
 * Input parameters:							*
 *	*alias		char		Data type alias name		*
 *									*
 * Output parameters:							*
 *	*nname		int		Number of names in array	*
 *	namarr[nname]	nmlst_t		Array of names			*
 *	*iret		int		Return code			*
 *					   As for NGD_NLST		*
 *									*
 **									*
 * S. Jacobs/NCEP	 3/01	Created					*
 ***********************************************************************/
{

	int		lenstr, ipos1, ipos2, ier, ierr;

	char		namstr[MXNMFL*MXFLSZ];

/*---------------------------------------------------------------------*/

	*iret  = 0;

/*
 *	Given the grid data type, get the list of names from the
 *	file names.
 */
 	ngd_nlst ( alias, namstr, &lenstr, iret,
		   strlen ( alias ), sizeof ( namstr ) );

/*
 *	If there is an error, set the number of names to 0 and return.
 */
 	if  ( *iret != 0 )  {
	    *nname = 0;
	    return;
	}

/*
 *	Add a NULL to the end of the string of names.
 */
	namstr[lenstr] = CHNULL;

/*
 *	Parse the string of names into an array of names.
 */
	ierr   = 0;
	*nname = 1;
	ipos1  = 0;
	while ( ierr == 0 ) {

/*
 *	    Find the next semi colon.
 */
	    cst_nocc ( namstr, ';', *nname, 1, &ipos2, &ierr );
	    if  ( ierr != 0 )  ipos2 = strlen(namstr) + 1;

/*
 *	    Copy the name into the array.
 */
	    cst_ncpy ( namarr[*nname-1], &namstr[ipos1],
		       ipos2-ipos1, &ier );

/*
 *	    Update the string position counter.
 */
	    ipos1 = ipos2 + 1;

/*
 *	    If there is no error, increment the name counter.
 */
	    if  ( ierr == 0 )  (*nname)++;

	}

}

#include "geminc.h"
#include "gemprm.h"

void grc_gtim  ( char *gdattm, char *firstm, char *lasttm, 
				char *gdtim1, char *gdtim2, int *iret )
/************************************************************************
 * grc_gtim								*
 *									*
 * This subroutine changes the user input for grid time into two	*
 * GEMPAK times.  These two times are separated with a colon (:)	*
 * and indicate the two times used to compute the grid function. 	*
 *									*
 * grc_gtim  ( gdattm, firstm, lasttm, gdtim1, gdtim2, iret )		*
 *									*
 * Input parameters:							*
 *	gdattm		CHAR*		Grid time input			*
 *	firstm		CHAR*		First time in grid file		*
 *	lasttm		CHAR*		Last time in grid file		*
 *									*
 * Output parameters:							*
 *	gdtim1		CHAR*		First input time		*
 *	gdtim2		CHAR*		Second input time		*
 *	iret		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid input time	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/85						*
 * M. desJardins/GSFC	 7/88	Eliminated LIST option			*
 * M. desJardins/GSFC	 4/89	Added first time option			*
 * D.W.Plummer/NCEP      1/05   Translated from FORTRAN                 *
 ***********************************************************************/
{
	char time1[21], time2[21];
	int nc, ier;
/*---------------------------------------------------------------------*/
	gr_gtim ( gdattm, firstm, lasttm, time1, time2, iret,
	    strlen(gdattm), strlen(firstm), strlen(lasttm),
	    sizeof(time1), sizeof(time2) );
	time1[20] = '\0';
	time2[20] = '\0';
	cst_lstr ( time1, &nc, &ier );
	time1[nc] = '\0';
	cst_lstr ( time2, &nc, &ier );
	time2[nc] = '\0';
	strcpy ( gdtim1, time1 );
	strcpy ( gdtim2, time2 );

	return;
}

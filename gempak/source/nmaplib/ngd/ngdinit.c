#define NGD_GLOBAL
#include "ngdcmn.h"

void ngd_init ( int *iret )
/************************************************************************
 * ngd_init								*
 *									*
 * This routine initializes the attributes for the GRID data.		*
 *									*
 * ngd_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Law/GSC		06/00	MAXGRID -> MAXTMPLT			*
 * T. Piper/SAIC        06/03   Removed ngrd                            *
 ***********************************************************************/
{

	int	ii;
/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( ii = 0; ii < MAXTMPLT; ii++ ) {

/*
 *	    Initialize the indices.
 */
	    indgrd[ii] = -1;

/*
 *	    Initialize the data attributes for each structure.
 */
	    grddt[ii].alias[0]  = CHNULL;
	    grddt[ii].cycle[0]  = CHNULL;
	    grddt[ii].rstfil[0] = CHNULL;
	    grddt[ii].isbcat    = SCAT_NIL;

	}

}

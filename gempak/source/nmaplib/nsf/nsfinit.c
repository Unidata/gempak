#define NSF_GLOBAL
#include "nsfcmn.h"

void nsf_init ( int *iret )
/************************************************************************
 * nsf_init								*
 *									*
 * This routine initializes the attributes for the SFC data.		*
 *									*
 * nsf_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSFC -> MAXTMPLT			*
 * T. Piper/SAIC        06/03   Removed nsfc                            *
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( ii = 0; ii < MAXTMPLT; ii++ ) {

/*
 *	    Initialize the indices.
 */
	    indsfc[ii] = -1;

/*
 *	    Initialize the data attributes for each structure.
 */
	    sfcdt[ii].alias[0]  = CHNULL;
	    sfcdt[ii].cycle[0]  = CHNULL;
	    sfcdt[ii].parms[0]  = CHNULL;
	    sfcdt[ii].color[0]  = CHNULL;
	    sfcdt[ii].filter[0] = CHNULL;
	    sfcdt[ii].txtatt[0] = CHNULL;
	    sfcdt[ii].isbcat    = SCAT_NIL;

	}

}

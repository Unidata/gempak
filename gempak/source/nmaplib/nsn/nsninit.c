#define NSN_GLOBAL
#include "nsncmn.h"

void nsn_init ( int *iret )
/************************************************************************
 * nsn_init								*
 *									*
 * This routine initializes the attributes for the SND data.		*
 *									*
 * nsn_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSND -> MAXTMPLT			*
 * T. Piper/SAIC        06/03   Removed nsnd                            *
 ***********************************************************************/
{

	int	ii;
/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( ii = 0; ii < MAXTMPLT; ii++ ) {

/*
 *	    Initialize the indices.
 */
	    indsnd[ii] = -1;

/*
 *	    Initialize the data attributes for each structure.
 */
	    snddt[ii].alias[0]  = CHNULL;
	    snddt[ii].cycle[0]  = CHNULL;
	    snddt[ii].parms[0]  = CHNULL;
	    snddt[ii].color[0]  = CHNULL;
	    snddt[ii].level[0]  = CHNULL;
	    snddt[ii].vcord[0]  = CHNULL;
	    snddt[ii].filter[0] = CHNULL;
	    snddt[ii].txtatt[0] = CHNULL;
	    snddt[ii].isbcat    = SCAT_NIL;

	}

}

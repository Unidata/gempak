#define NMS_GLOBAL
#include "nmscmn.h"

void nms_init ( int *iret )
/************************************************************************
 * nms_init								*
 *									*
 * This routine initializes the attributes for the MISC data.		*
 *									*
 * nms_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Jacobs/NCEP	11/99	Changed arrays of info to structures	*
 * S. Jacobs/NCEP	12/99	Updated data structures			*
 * S. Jacobs/NCEP	 3/00	Updated data structures			*
 * S. Law/GSC		06/00	MAXMISC -> MAXTMPLT			*
 * M. Li/SAIC		04/03	Added icolr2				*
 * T. Piper/SAIC        06/03   Removed nmsc                            *
 * F. J. Yen/NCEP	 6/04	Added arrow type			*
 ***********************************************************************/
{

	int	ii, jj, kk;
/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( ii = 0; ii < MAXTMPLT; ii++ ) {

/*
 *	    Initialize the indices.
 */
	    indmsc[ii] = -1;

/*
 *	    Initialize the data attributes for each structure.
 */
	    mscdt[ii].alias[0]  = CHNULL;
	    mscdt[ii].filnam[0] = CHNULL;

	    for ( jj = 0; jj < MAXPLT; jj++ )  {
		mscdt[ii].msctyp[jj].name[0] = CHNULL;
		mscdt[ii].msctyp[jj].ionoff = -1;
		mscdt[ii].msctyp[jj].icolr  = -1;
		mscdt[ii].msctyp[jj].icolr2  = -1;
		mscdt[ii].msctyp[jj].value  = RMISSD;

		mscdt[ii].msctyp[jj].line.size = -1.0F;
		mscdt[ii].msctyp[jj].line.iwid = -1;

		for ( kk = 0; kk < 2; kk++ )  {
		    mscdt[ii].msctyp[jj].symb[kk].code = -1.0F;
		    mscdt[ii].msctyp[jj].symb[kk].size = -1.0F;
		    mscdt[ii].msctyp[jj].symb[kk].iwid = -1;
		}

		mscdt[ii].msctyp[jj].arrw.size = -1.0F;
		mscdt[ii].msctyp[jj].arrw.hdsz = -1.0F;
		mscdt[ii].msctyp[jj].arrw.iwid = -1;
		mscdt[ii].msctyp[jj].arrw.ityp = -1;

		mscdt[ii].mscflg[jj].name[0] = CHNULL;
		mscdt[ii].mscflg[jj].iflg = -1;
	    }

	    mscdt[ii].isbcat = SCAT_NIL;

	}

}

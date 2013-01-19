#define CRG_GLOBAL

#include "crgcmn.h"

void crg_init ( int *iret )
/************************************************************************
 * crg_init								*
 *									*
 * This function instantiates and clears the range record for the 	*
 * CRG sublibrary.							*
 *									*
 * crg_init ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * F.Y.Yen/NCEP		 1/98	Cleaned up.				*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * J. Wu/SAIC	 	 7/07	initialize second range record to none	*
 ***********************************************************************/
{
    int 	i;
/*---------------------------------------------------------------------*/

    *iret = 0;

    for (i = 0; i < MAX_EDITABLE_ELEMS; i++)
    {
        range[ i ].assocRec = NO_ASSOC_REC;
        range[ i ].auxRec = PRIMARY_REC;   /* primary range record */
	
	crg_clear(i, iret);
    }    
    
}

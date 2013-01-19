#include "crgcmn.h"

void crg_soffset ( int elnum,  int joffset, int *iret )
/************************************************************************
 * crg_soffset								*
 *                                                                      *
 * This function updates the location information for a record without	*
 * changing any other settings.						*
 *                                                                      *
 * crg_soffset ( elnum, joffset, iret)                       	 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *	joffset		int		record location in vgf file	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       -2 = Element no. out of bounds	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/98	copied from crg_get			*
 ***********************************************************************/
{
    *iret = 0;

    if ((elnum >=0 ) && ( elnum < MAX_EDITABLE_ELEMS) )
    {
	range[elnum].ioffset = joffset;
    }
    else
    {
	*iret = -2;
    }
}

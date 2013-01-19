#include "crgcmn.h"


void crg_newinx ( int *elnum, int *iret )
/************************************************************************
 * crg_newinx                                                           *
 *                                                                      *
 * This function returns the index into the array of range records that	*
 * is empty (the file position ioffset is -1.)				*
 *                                                                      *
 * crg_newinx ( elnum, iret ) 						*
 *                                                                      *
 * Output parameters:                                                   *
 *	*elnum		int	    Element number			*
 *      *iret           int         Return code                     	*
 *                                   -3 = Range array is full		*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_gethol.  Cleaned up.	*
 ***********************************************************************/
{
    *iret = 0;
    (*elnum) = 0;
    while ( ((*elnum) < MAX_EDITABLE_ELEMS) && 
	    (range[(*elnum)].ioffset !=  -1) )
    {
        (*elnum)++;
    }

    if ((*elnum) >= MAX_EDITABLE_ELEMS)
    {
        *iret = -3;
	*elnum = -1;
    }

}

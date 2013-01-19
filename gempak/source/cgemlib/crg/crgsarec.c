#include "crgcmn.h"


void crg_sarec ( int elnum, int assocRec, int *iret )
/************************************************************************
 * crg_sarec                                                            *
 *                                                                      *
 * This function sets up a one-way linkage between two range records	*
 * for an element.							*
 *                                                                      *
 * void crg_clear ( elnum, assocRec, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number 			*
 *	assocRec	int		Range record to be linked to	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-2 = Element is out of bounds	*
 *									*
 * Log:                                                                 *
 * J. Wu/SAIC		07/07	initial coding				*
 ***********************************************************************/
{
    *iret = 0;

    if ( elnum < 0 || elnum >= MAX_EDITABLE_ELEMS ) {
	*iret = -2;
    }
    else {
        range[elnum].assocRec = assocRec;    
    }

}

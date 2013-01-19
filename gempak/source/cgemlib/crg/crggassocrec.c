#include "crgcmn.h"

void crg_gassocrec ( int elnum, int *assocRec, int *iret )
/************************************************************************
 * crg_gassocrec                                                        *
 *                                                                      *
 * This function returns the second range record number for an element	*
 * based on the element number in the range array.			*
 *                                                                      *
 * crg_gassocrec ( elnum, assocRec, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*assocRec	int		element number for second record*
 *      *iret           int             Return code                     *
 *					 -2 = elnum out of bounds	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/07	initial coding 				*
 ***********************************************************************/
{
    *iret    = 0;
    *assocRec = NO_ASSOC_REC;

    if ( ( elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0 ) ) {
	if ( range[ elnum ].assocRec >= 0  ) {
            *assocRec = range[ elnum ].assocRec;
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }
        
}

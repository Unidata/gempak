#include "crgcmn.h"


void crg_setauxrec ( int elnum, int *iret )
/************************************************************************
 * crg_setauxrec                                                   	*
 *                                                                      *
 * This function sets the flag for a secondary range record.		*
 *                                                                      *
 * crg_setauxrec ( elnum, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      -2 - elnum out of bound 	*
 *                                                   			*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    *iret  = 0;
    
    if ( ( elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0 ) ) {
         range[ elnum ].auxRec = SECONDARY_REC;   
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }
    
}

#include "crgcmn.h"

Boolean crg_isauxrec ( int elnum, int *iret )
/************************************************************************
 * crg_isauxrec                                               		*
 *                                                                      *
 * This function checks if an record is a primary or secondary record.	*
 *                                                                      *
 * crg_isauxrec ( elnum, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					 -2 = elnum out of bounds	*
 * Return parameters:                                                   *
 *      crg_isauxrec()	Boolean         True/False               	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/07	initial coding 				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    if ( ( elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0 ) ) {
	
	if ( range[ elnum ].auxRec == SECONDARY_REC ) {
	    return True;
	}
	
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }
    
    return False;
        
}

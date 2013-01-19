#include "crgcmn.h"


void crg_ggrp ( int elnum, char *grptyp, int *grpnum, int *iret )
/************************************************************************
 * crg_ggrp                                                          	*
 *                                                                      *
 * This function returns the group type and group number info for the   *
 * specified element.							*
 *                                                                      *
 * crg_ggrp ( elnum, grptyp, grpnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*grptyp		char		group type			*
 * 	*grpnum		int		group number			*
 *      *iret           int             Return code                     *
 *					 -2 = elnum out of bounds	*
 *					 -5 = elnum is deleted		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            4/98						*
 * E. Safford/SAIC	04/02	return error codes, init output params	*
 ***********************************************************************/
{
    *iret   = 0;
    *grptyp = 0;
    *grpnum = 0;

    if ((elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0) ) {
	if ( range[elnum].ioffset >= 0 ) {
            *grptyp = range[elnum].grptyp;
            *grpnum = range[elnum].grpnum;
	}
	else {
	    *iret = -5;			/* elnum is deleted */
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }

}

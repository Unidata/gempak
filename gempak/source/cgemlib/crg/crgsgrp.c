#include "crgcmn.h"


void crg_sgrp ( int elnum, char grptyp, int grpnum, int *iret )
/************************************************************************
 * crg_sgrp                                                          	*
 *                                                                      *
 * This function sets the group type and group number info for the      *
 * specified element.							*
 *                                                                      *
 * crg_sgrp ( elnum, grptyp, grpnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 * 	grptyp		char		group type			*
 * 	grpnum		int		group number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					  -2 elnum out of range		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            4/98						*
 * E. Safford/SAIC	04/02	return error if elnum out of range	*
 ***********************************************************************/
{
    *iret = 0;

    if ((elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0) ) {
        range[elnum].grptyp = grptyp;
        range[elnum].grpnum = grpnum;
    }
    else {
	*iret = -2;
    }

}

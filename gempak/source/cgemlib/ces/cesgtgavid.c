#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


int ces_gtgavid ( char grptyp )
/************************************************************************
 * ces_gtgavid							        *
 *									*
 * This function returns the associated index number of the input       *
 * grptyp from the avail_grp_t structure.  If no match is found, or the *
 * input grptyp value is out of range, 0 is returned.			*
 *									*
 * Because this is returning values from the avail_grp_t structure      *
 * and available group type menus are also built using the contents of  *
 * this structure, this routine can be used to get the appropriate      *
 * menu index for the input grptyp.					*
 *									*
 * int ces_gtgavid ( grptyp )                				*
 *									*
 * Input parameters:							*
 *	grptyp		char	group type to match in avail_grp_t	*
 *									*
 * Output parameters:							*
 *			NONE						*	
 * Return value:                                                        *
 *	ces_gtgavid	int	index number of the associated grptyp   *
 *				 in the avail_grp_t structure		*
 **									*
 * Log:									*
 *  E. Safford/SAIC	03/02	initial coding                   	*
 * M. Li/SAIC		01/03	delete vgstruct.h			*
 ***********************************************************************/
{
char	grpnam[MAX_GRP_STR];
int     ii, index, ier;
/*---------------------------------------------------------------------*/

    index = 0;

    ces_gtgnam (grptyp, grpnam, &ier);

    if ( ier >= 0 ) {

        for (ii=0; ii< _grpTbl.ngrp; ii++) {
            if ( strcmp ( _grpTbl.grps[ii].name, grpnam ) == 0 ) {
	        index = ii;
	        break;
	    }
        }

    }

    return (index);
}

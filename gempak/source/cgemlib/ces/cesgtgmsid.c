#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


int ces_gtgmsid ( int index )
/************************************************************************
 * ces_gtgmsid							        *
 *									*
 * This function returns the group type value of the input index number *
 * to the _grpTbl.grps structure.  If no match is found, 0 is returned.	*
 *									*
 * char ces_gtgmsid ( index )                				*
 *									*
 * Input parameters:							*
 *	index		int	index number of the associated grptyp	*
 *                               in the avail_grp_t structure           *
 * Output parameters:							*
 *			NONE						*	
 * Return value:                                                        *
 *	ces_gtgmsid	int	group type value                        *
 **									*
 * Log:									*
 * H. Zeng/EAI 	04/02	initial coding                   		*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int  grptyp, ier;
/*---------------------------------------------------------------------*/

    if ( index < 0 || index >= _grpTbl.ngrp ) { 
         grptyp = 0;
    }
    else {
         ces_gtgid (_grpTbl.grps[index].name, &grptyp, &ier);
         if ( ier < 0 ) {
              grptyp = 0;
         }
    }

    return ( grptyp );

}

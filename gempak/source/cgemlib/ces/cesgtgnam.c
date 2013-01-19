#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtgnam ( int grpid, char *grpnam, int *iret )
/************************************************************************
 * ces_gtgnam							        *
 *									*
 * This function queries the group type name for the specified group 	*
 * type ID.  Returned group name is NULL if ID is not found.		*
 *									*
 * void ces_gtgnam (grpid, grpnam, iret)				*
 *									*
 * Input parameters:							*
 *	grpid		int	group type id				*
 *									*
 * Output parameters:							*
 *	*grpnam	char	group type name  				*
 *      *iret   int     return code:                                    *
 *                        -1 = group id not found			*
 *                                                                      *
 **									*
 * Log:									*
 * H. Zeng/EAI		 2/01	copied from pggrpw_getGrpName()		*
 * D.W.Plummer/NCEP	 3/01	set return code; improve logic		*
 * E. Safford/SAIC	02/02	use mstr struct, not grps		*
 * E. Safford/SAIC	03/02	use strcpy 				*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    grpnam[0] = '\0';

    for (ii=0; ii < _grpTbl.nmstr; ii++) {
        if (_grpTbl.mstr[ii].type == grpid) {
            strcpy (grpnam, _grpTbl.mstr[ii].name);
	    return;
        }
    }

    *iret = -1;

}

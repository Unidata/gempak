#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtgid ( char *grpnam, int *grpid, int *iret )
/************************************************************************
 * ces_gtgid							        *
 *									*
 * This function queries the group type ID for the specified group 	*
 * type name.  If not found, the ID is set to 0 and an error return 	*
 * code	is set.								*
 *									*
 * void ces_gtgid (grpnam, grpid, iret)					*
 *									*
 * Input parameters:							*
 *	*grpnam	char	group type name					*
 *									*
 * Output parameters:							*
 *	*grpid	int	group type ID					*
 *      *iret   int     return code:                                    *
 *                        -1 = group name not found			*
 *                                                                      *
 **									*
 * Log:									*
 * H. Zeng/EAI		 2/01	copied from pggrpw_getGrpTyp()		*
 * D.W.Plummer/NCEP	 3/01	return grpid = 0 if grp name not found	*
 * E. Safford/SAIC	02/02	use mstr struct, not grps		*
 * M. Li/SAIC		01/03	delete vgstruct.h			*
 * S. Danz		04/06	Added check of grpnam			*
 ***********************************************************************/
{
int ii;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    *grpid = 0;

    if (grpnam != (char*)NULL) {
        for (ii = 0; ii < _grpTbl.nmstr; ii++) {
            if ( strcmp(_grpTbl.mstr[ii].name, grpnam) == 0 ) {
                *grpid = _grpTbl.mstr[ii].type;
                return;
            }
        }
    }

    *iret = -1;
  
}

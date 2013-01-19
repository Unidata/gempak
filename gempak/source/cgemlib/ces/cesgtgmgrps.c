#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtgmgrps ( Boolean incl_dev, int *ngrp, char **names, int *iret )
/************************************************************************
 * ces_gtgmgrps							        *
 *									*
 * This function queries the total number of group type and group names *
 * on the master group type list.					*
 *									*
 * Note:  To protect against new table entries exceeding the size of    *
 * the names parameter and causing a crash via seg fault, names is      *
 * allocated within this routine.  Any calling routine must free this   *
 * string when finished with it.					*
 *									*
 * void ces_gtgmgrps ( incl_dev, ngrp, names, iret )			*
 *									*
 * Input parameters:							*
 *      incl_dev        Boolean	whether to include dev. types		*
 *									*
 * Output parameters:							*
 *	*ngrp		int	total # of group type	                *
 *      **names  	char    group names separated with ";"          *
 *      *iret   	int     return code:                            *
 *                        		-1 = error occured              *
 *                                                                      *
 **									*
 * Log:									*
 * H. Zeng/EAI		05/02	initial coding		                *
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;

        *ngrp = 0;

        /*
         *  Required space for the names string is 8 chars for each
         *  grptype name plus one for the ';' for a total of 9 * the 
         *  number of entries found in the grptyp.tbl.
         */
	*names = (char *) malloc ( _grpTbl.nmstr * sizeof(char) * 9 );
        (*names)[0] = '\0';

        for(ii = 1; ii < _grpTbl.nmstr; ii++) {
	   if ( incl_dev == FALSE && 
                strstr(_grpTbl.mstr[ii].name, "DEV") != (char*)NULL ) {

                continue;
           }
           strcat(*names, _grpTbl.mstr[ii].name);
           strcat(*names, ";");
           (*ngrp)++;
        }

}

#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtggrps ( int *ngrp, char **names, int *iret )
/************************************************************************
 * ces_gtggrps							        *
 *									*
 * This function queries the total number of group type and group names *
 * on the group type table.						*
 *									*
 * Note:  To protect against new table entries exceeding the size of    *
 * the names parameter and causing a crash via seg fault, names is      *
 * allocated within this routine.  Any calling routine must free this   *
 * string when finished with it.					*
 *									*
 * void ces_gtggrps ( ngrp, names, iret )				*
 *									*
 * Input parameters:							*
 *	        none					                *
 *									*
 * Output parameters:							*
 *	*ngrp		int	total # of group type	                *
 *      **names  	char    group names separated with ";"          *
 *      *iret   	int     return code:                            *
 *                        		-1 = error occured              *
 *                                                                      *
 **									*
 * Log:									*
 *  H. Zeng/EAI		03/01	initial coding		                *
 *  E. Safford/SAIC	03/02	added cesgtcmn.h			*
 *  E. Safford/SAIC	03/02	allocate space for names string		*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;
         names[0] = '\0';

        *ngrp = _grpTbl.ngrp;

        /*
         *  Required space for the names string is 8 chars for each
         *  grptype name plus one for the ';' for a total of 9 * the 
         *  number of entries found in the grptyp.tbl.
         */
	*names = (char *) malloc ( *ngrp * sizeof(char) * 9 );

        if( _grpTbl.ngrp > 0 ) {
            strcpy(*names, _grpTbl.grps[0].name);
            strcat(*names, ";");
            for(ii = 1; ii < _grpTbl.ngrp; ii++) {
               strcat(*names, _grpTbl.grps[ii].name);
               strcat(*names, ";");
            }

        }

}

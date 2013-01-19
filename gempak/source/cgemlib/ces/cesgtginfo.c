#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtginfo ( int grpid, char *label, char *info, int *iret )
/************************************************************************
 * ces_gtginfo							        *
 *									*
 * This function queries the attribute info. for a particular label of  *
 * a group type.                                                        *
 *									*
 * void ces_gtginfo (grpid, label, info, iret)			        *
 *									*
 * Input parameters:							*
 *	 grpid	int	group type id				        *
 *      *label  char    label string                                    *
 *									*
 * Output parameters:							*
 *	*info	char	attribute info. for the label			*
 *      *iret   int     return code:                                    *
 *                        -1 = error occured                            *
 *                                                                      *
 **									*
 * Log:									*
 *  H. Zeng/EAI		03/01	copied from ces_gtgcolr()	        *
 * E. Safford/SAIC	03/02	added cesgtcmn.h include		*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int	ii, jj, index;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    for( ii = 0; ii < _grpTbl.ngrp; ii++) {
       if(_grpTbl.grps[ii].type == (char)grpid) {
             index = ii;
         
             for(jj = 0; jj < _grpTbl.grps[index].nitems; jj++) {
                 if( strcmp(_grpTbl.grps[index].label[jj], label) == 0 ) {
                   strcpy(info, _grpTbl.grps[index].info[jj]);
                   return;
                 }
             }

             break;
       }
    }

    *iret = -1;

}

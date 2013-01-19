#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


void ces_gtglbls ( int grpid, int *nlbl, char *lbls, int *iret )
/************************************************************************
 * ces_gtglbls							        *
 *									*
 * This function queries the total number of labels and label strings   *
 * for a certain group type.						*
 *									*
 * void ces_gtglbls ( grpid, nlbl, lbls, iret )			        *
 *									*
 * Input parameters:							*
 *	grpid   int     group type id			                *
 * Output parameters:							*
 *	*nlbl	int	total # of labels		                *
 *      *lbls   char    label strings separated with ";"                *
 *      *iret   int     return code:                                    *
 *                        -1 = error occured                            *
 *                                                                      *
 **									*
 * Log:									*
 *  H. Zeng/EAI		03/01	initial coding		                *
 *  H. Zeng/EAI         03/01   rewrote for new group type table        *
 *  E. Safford/SAIC	03/02	add cesgtcmn.h include			*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int     ii, index;
/*---------------------------------------------------------------------*/

        *iret = G_NORMAL;
        index = 0;
        lbls[0] = '\0';
        *nlbl = 0;
       
	for( ii = 0; ii < _grpTbl.ngrp; ii++) {
	  if(_grpTbl.grps[ii].type == (char)grpid) {
             index = ii;
             *nlbl  = _grpTbl.grps[ii].nitems;
             break;
          }

        }

        if( *nlbl > 0 ) {
            strcpy(lbls, _grpTbl.grps[index].label[0]);
            strcat(lbls, ";");
            for(ii = 1; ii < _grpTbl.grps[index].nitems; ii++) {
               strcat(lbls, _grpTbl.grps[index].label[ii]);
               strcat(lbls, ";");
            }

        }
        else {
            *iret = -1;
        }

}

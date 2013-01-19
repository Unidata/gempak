#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"


void ctb_pfbool ( char *tag, Boolean *bval, int *iret )
/************************************************************************
 * ctb_pfbool							        *
 *									*
 * This function queries the boolean for the given tag.                 *
 *									*
 * void ctb_pfstr (tag, bval, iret)				        *
 *									*
 * Input parameters:							*
 *	tag		char*	tag name				*
 *									*
 * Output parameters:							*
 *	bval	        Boolean*  boolean value of the tag  		*
 *      iret            int*    return code:                            *
 *                              -1 = tag not found			*
 *                              -2 = unrecognizable value               *
 *                              -3 = unknown error                      *
 *                                                                      *
 **									*
 * Log:									*
 * H. Zeng/EAI		08/02	initial coding		                *
 * H. Zeng/EAI          08/02   added call to cst_lcuc                  *
 ***********************************************************************/
{
int     ier, ier2;
char    cval[MAX_PREF_STR];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *bval = FALSE;
     ctb_pfstr ( tag, cval, &ier );

     if ( ier == -1 ) {
          *iret = -1;
          return;
     }

     if ( ier == 0 ) {
          
          cst_lcuc(cval, cval, &ier2);

          if ( strcmp(cval, "ON"   ) == 0 ||
               strcmp(cval, "TRUE" ) == 0 ||
               strcmp(cval, "YES"  ) == 0 ||
               strcmp(cval, "1"    ) == 0    ) {
  
               *bval = TRUE;
               return;
          }
          if ( strcmp(cval, "OFF"  ) == 0 ||
               strcmp(cval, "FALSE") == 0 ||
               strcmp(cval, "NO"   ) == 0 ||
               strcmp(cval, "0"    ) == 0    ) {
  
               *bval = FALSE;
               return;
          }
          *iret = -2;
          return;
     }

     *iret = -3;
     return;

}

/*=====================================================================*/

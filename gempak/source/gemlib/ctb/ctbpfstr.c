#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"


void ctb_pfstr ( char *tag, char *cval, int *iret )
/************************************************************************
 * ctb_pfstr							        *
 *									*
 * This function queries the string for the given tag. Returned string  *
 * is NULL if the tag is not found.		                        *
 *									*
 * void ctb_pfstr (tag, cval, iret)				        *
 *									*
 * Input parameters:							*
 *	tag		char*	tag name				*
 *									*
 * Output parameters:							*
 *	cval	        char*	string value of the tag  		*
 *      iret            int*    return code:                            *
 *                              -1 = tag not found			*
 *                                                                      *
 **									*
 * Log:									*
 * H. Zeng/EAI		08/02	initial coding		                *
 * H. Zeng/EAI          08/02   changed tag str to upper case           *
 * T. Piper/SAIC	06/03	added ltag				*
 ***********************************************************************/
{
int     ii, ier;
char	ltag[MAX_PREF_STR];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cst_lcuc(tag, ltag, &ier);
    cval[0] = '\0';

    for (ii=0; ii < _prefTbl.npref; ii++) {
        if ( strcmp(_prefTbl.prefs[ii].tag, ltag) == 0 ) {
             strcpy (cval, _prefTbl.prefs[ii].val);
	     return;
        }
    }

    *iret = -1;

}

/*=====================================================================*/

#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dtlist ( int *catgry, char *list, int *count )
/************************************************************************
 * ctb_dtlist								*
 *									*
 * This function returns a semi-colon delimited list of aliases		*
 * associated with a data category.  This returns 'NONE' in the case	*
 * of either CAT_GRD or CAT_VGF.					*
 *									*
 * ctb_dtlist (catgry, list, count)					*
 *									*
 * Input parameters:							*
 *	*catgry		int	data category number			*
 *									*
 * Output parameters:							*
 *	*list		char	Alias list				*
 *	*count		int	number of matches			*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * E. Safford/GSC	01/00	don't write alias to list w/ GRD & VGF  *
 ***********************************************************************/
{
    int		ii, ier, len;
    char	alias[32];
/*---------------------------------------------------------------------*/

    if ( DtReadin == G_FALSE )  {
        ctb_dtrd (&ier);
    }

    strcpy (list, "");
    *count = 0;
    for (ii = 0; ii < DtTable.numtmpl; ii++) {
	if (DtTable.info[ii].catgry == *catgry) {
	    if (*catgry != CAT_GRD && *catgry != CAT_VGF) {
	        cst_lcuc (DtTable.info[ii].alias, alias, &ier );
	        strcat (list, alias);
	        strcat (list, ";");
	    }
	    (*count)++;
	}
    }

    if (*count == 0 || *catgry == CAT_GRD || *catgry == CAT_VGF) {
	strcpy (list, "NONE");
    }
    else {
	len = strlen (list);
	list[--len] = '\0';
    }

    return;
}

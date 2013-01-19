#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dtcat ( char *alias_i, int *catgry, int *subcat, int *iret )
/************************************************************************
 * ctb_dtcat								*
 *									*
 * This function returns the category and subcategory associated with	*
 * a data alias.  							*
 *									*
 * ctb_dtcat ( alias_i, catgry, subcat, iret )				*
 *									*
 * Input parameters:							*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *      *catgry         int 	Alias data category			*
 *      *subcat         int 	Alias data subcategory			*
 * 	*iret		int	Return code				*
 *				  -1 - alias not found			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/99	Created					*
 * S. Jacobs/NCEP	 3/01	Added parsing of storm name from alias	*
 ***********************************************************************/
{
int	i, ipos, ier;
char	alias[49];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( DtReadin == G_FALSE )  {
        ctb_dtrd ( iret );
    }

    cst_lcuc( alias_i, alias, &ier );

    /*
     * Remove the name of the storm/volcano from the alias.
     */
    cst_nocc ( alias, ':', 1, 0, &ipos, &ier );
    if  ( ier == 0 )  alias[ipos] = CHNULL;

    /*
     * Find a match for the alias.
     */
    for ( i = 0; i < DtTable.numtmpl; i++ )  {

	if ( strcmp( DtTable.info[i].alias, alias ) == 0 )  {

	    *catgry = DtTable.info[i].catgry;
	    *subcat = DtTable.info[i].subcat;

	    return;

	}

    }

    *iret = -1;
    return;

}

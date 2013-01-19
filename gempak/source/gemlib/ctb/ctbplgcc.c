#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Prmlst_t	PlTable;
extern	int		PlReadin;

void ctb_plgcc ( char *dtype_i, char *alias_i, char *colcod, int *iret )
/************************************************************************
 * ctb_plgcc								*
 *									*
 * This function returns the value of the color code.			*
 *									*
 * ctb_plgcc ( dtype_i, alias_i, colcod, iret )				*
 *									*
 * Input parameters:							*
 *      *dtype_i	char	Data type				*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *      *colcod         char	Color code value			*
 * 	*iret		int	Return code				*
 *				  -1 - prmlst table not read in		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
int	i, ier;
char	alias[32], dtype[16];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( PlReadin == G_FALSE )  {
	*iret = -1;
	return;
    }

    cst_lcuc( dtype_i, dtype, &ier );
    cst_lcuc( alias_i, alias, &ier );

    *colcod = 0;

    for ( i = 0; i < PlTable.nalias; i++ )  {

	if ( ( strcmp( PlTable.info[i].alias, alias ) == 0 ) &&
	     ( strcmp( PlTable.info[i].dtype, dtype ) == 0 ) )  {

	    *colcod = PlTable.info[i].colcod;

	    return;

	}

    }

    *iret = -1;
    return;

}

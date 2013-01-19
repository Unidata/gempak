#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Prmlst_t	PlTable;
extern	int		PlReadin;

void ctb_pldump ( int *iret )
/************************************************************************
 * ctb_pldump								*
 *									*
 * This function dumps the contents of the prmlst structure.		*
 *									*
 * ctb_pldump ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *iret		int	Return code				*
 *				-1 - table not read in			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 ***********************************************************************/
{
int	i, j;

/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( PlReadin == G_FALSE )  {
        *iret = -1;
	return;
    }

    for ( i = 0; i < PlTable.nalias; i++ )  {

	printf("%2d %-16s %-12s\n", i, PlTable.info[i].alias, 
		PlTable.info[i].dtype );	

	for ( j = 0; j < PlTable.info[i].npe; j++ )  {

	    printf("%20s  %s\n", PlTable.info[i].parmele[j].name,
				   PlTable.info[i].parmele[j].value );

	}

	printf("              COLCOD  %d\n", PlTable.info[i].colcod );

    }

    return;

}

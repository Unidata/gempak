#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Prmlst_t	PlTable;
extern	int		PlReadin;

void ctb_plset ( char *dtype_i, char *alias_i, char *parm_i, char *str, int *iret )
/************************************************************************
 * ctb_plset								*
 *									*
 * This function assigns a parameter string given an alias, datatype	*
 * and the parameter.							*
 *									*
 * ctb_plset ( dtype_i, alias_i, parm_i, str, iret )			*
 *									*
 * Input parameters:							*
 *	*dtype_i	char	Data type				*
 *	*alias_i	char	Alias name				*
 *	*parm_i		char	Parameter name				*
 *									*
 * Output parameters:							*
 *	*str		char	New parameter value			*
 * 	*iret		int	Return code				*
 *				  -1 					*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 ***********************************************************************/
{
int	i, j, ier;
char	alias[32], dtype[16], parm[16], *ptrlp, *ptrrp;

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( PlReadin == G_FALSE )  {
	*iret = -1;
	return;
    }

    cst_lcuc( dtype_i, dtype, &ier );
    cst_lcuc( alias_i, alias, &ier );
    cst_lcuc( parm_i, parm, &ier );

    for ( i = 0; i < PlTable.nalias; i++ )  {

	if ( ( strcmp( PlTable.info[i].alias, alias ) == 0 ) &&
	     ( strcmp( PlTable.info[i].dtype, dtype ) == 0 ) )  {

	    for ( j = 0; j < PlTable.info[i].npe; j++ )  {

		if ( strcmp( PlTable.info[i].parmele[j].name, parm ) == 0 )  {

		    strcpy( PlTable.info[i].parmele[j].value, str );

		    if ( strcmp( parm, "COLORS" ) == 0 )  {
			ptrlp = strchr(str, '(');
                        ptrrp = strchr(str, ')');
                        if ( ptrlp != NULL && ptrrp != NULL )
                            PlTable.info[i].colcod = 1;
                        else
                            PlTable.info[i].colcod = 0;

		    }

		}

	    }

	    return;

	}

    }

    *iret = -1;
    return;

}

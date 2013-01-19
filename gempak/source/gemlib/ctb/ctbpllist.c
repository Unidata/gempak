#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Prmlst_t	PlTable;
extern	int		PlReadin;

void ctb_pllist ( char *dtype_i, int maxals, char list[][16], 
						int *nele, int *iret )
/************************************************************************
 * ctb_pllist								*
 *									*
 * This function returns a list of alias names associated with a 	*
 * given datatype.  The number of aliases is also returned.		*
 *									*
 * ctb_pllist ( dtype_i, maxals, list, nele, iret )			*
 *									*
 * Input parameters:							*
 *	*dtype_i	char	Data type				*
 *	maxals		int	Max number of alias names returned	*
 *									*
 * Output parameters:							*
 *	list [][16]	char	Alias list for given dtype		*
 *	*nele		int	Number of alias names			*
 * 	*iret		int	Return code				*
 *				  -1 - prmlst table not read in		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * S. Jacobs/NCEP	 1/01	Added maxals to calling sequence	*
 ***********************************************************************/
{
int	i, ier;
char	dtype[16];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( PlReadin == G_FALSE )  {
	*iret = -1;
	return;
    }

    *nele = 0;
    cst_lcuc( dtype_i, dtype, &ier );

    for ( i = 0; i < PlTable.nalias; i++ )  {

	if  ( ( strcmp ( PlTable.info[i].dtype, dtype ) == 0 ) &&
	      ( *nele < maxals ) )  {

	    strcpy( list[*nele], PlTable.info[i].alias );
	    (*nele)++;

	}

    }

    return;

}

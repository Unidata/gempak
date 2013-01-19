#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dttime ( char *alias_i, int *nframe, int *range, 
					int *intrvl, int *iret )
/************************************************************************
 * ctb_dttime								*
 *									*
 * This function returns the time parms associated with a data alias.   *
 *									*
 * ctb_dttime ( alias_i, nframe, range, intrvl, iret )			*
 *									*
 * Input parameters:							*
 *      *alias_i	char	Alias name				*
 *									*
 * Output parameters:							*
 *	*nframe		int	Alias number of frames			*
 *	*range		int	Alias time range			*
 *	*intrvl		int	Alias time interval			*
 * 	*iret		int	Return code				*
 *				  -1 - alias not found			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC	 	 3/99	Corrected prolog			*
 * S. Jacobs/NCEP	 8/99	Changed format of data.tbl		*
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

	    *nframe = DtTable.info[i].nframe;
	    *range  = DtTable.info[i].range;
	    *intrvl = DtTable.info[i].intrvl;

	    return;

	}

    }

    *iret = -1;
    return;

}

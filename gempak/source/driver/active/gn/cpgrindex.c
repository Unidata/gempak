#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"

#define MAX_STRING_LEN 100
#define DELIM ":"


void pg_rindex ( char *pname, PrdRec *prec, int *iret )
/************************************************************************
 * pg_rindex								*
 *									*
 * This function reads the Index table to determine			*
 * the filename for the product requested in the pname variable.	*
 *									*
 * pg_rindex ( pname, prec, iret )					*
 *									*
 * Input parameters:							*
 *	*pname		char		Name of the product requested	*
 *									*
 * Output parameters:							*
 *	*prec		PrdRec		Record of product parameters	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    *iret = 0;

    /* save number of requested wheel */

    strcpy(prec->pwheel, pname);

    *iret = pg_getfname(pname, prec->pfname,prec->pdesc);

    return;
}

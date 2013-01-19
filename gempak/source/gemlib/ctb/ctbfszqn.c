#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern Fontsz_t		FszTbl;
extern char 		FszReadin;

void ctb_fszqn ( int *ntotal, int *iret )
/************************************************************************
 * ctb_fszqn								*
 *									*
 * This function queries the total number of font sizes defined in the  *
 * font table.								*
 *									*
 * ctb_fszqn ( ntotal, iret )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *ntotal		int	Total number of font size defined	*
 * *iret		int	Return code				*
 *				  -1 - font size table is not read	*
 **									*
 * Log:									*
 * C. Lin/EAI	 8/98							*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    	*iret = G_NORMAL;

    	if ( FszReadin == 0 )  {
		*iret = -1;
		return;
	}

	*ntotal = FszTbl.nfsz;

}

#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern Fontsz_t		FszTbl;
extern char 		FszReadin;

void ctb_fsznam ( int index, char *name, int *iret )
/************************************************************************
 * ctb_fsznam								*
 *									*
 * This function gets the font size name for the given font size index. *
 *									*
 * ctb_fsznam ( index, name, iret )					*
 *									*
 * Input parameters:							*
 *  index	int	index to the font size info array		*
 *									*
 * Output parameters:							*
 * *name	char	Font size name 					*
 * *iret	int	Return code					*
 *				  -1 - font size file hasn't been read	*
 *				  -2 - index out of range		*
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

	if ( index < 0 || index >= FszTbl.nfsz ) {
		*iret = -2;
		return;
	}

	strcpy(name, FszTbl.info[index].name);

}

#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern Fontsz_t		FszTbl;
extern char 		FszReadin;

void ctb_fszval ( int index, float *value, int *iret )
/************************************************************************
 * ctb_fszval								*
 *									*
 * This function gets the font size value for the given font index.   	*
 *									*
 * ctb_fszval ( index, value, iret )					*
 *									*
 * Input parameters:							*
 *  index	int	index to the font size info array		*
 *									*
 * Output parameters:							*
 * *value	float	font size value (defined in GEMPAK)		*
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

	*value = FszTbl.info[index].value;

}

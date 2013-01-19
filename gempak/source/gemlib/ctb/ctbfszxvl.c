#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern Fontsz_t		FszTbl;
extern char 		FszReadin;

void ctb_fszxvl ( int index, int *xfont, int *mult, int *iret )
/************************************************************************
 * ctb_fszxvl								*
 *									*
 * This function gets the font size value for the given font index.   	*
 *									*
 * ctb_fszxvl ( index, xfont, mult, iret )				*
 *									*
 * Input parameters:							*
 *  index	int	index to the font size info array		*
 *									*
 * Output parameters:							*
 * *xfont	int	X font size 					*
 * *mult	int	Multiplier font size 				*
 * *iret	int	Return code					*
 *				  -1 - font size file hasn't been read	*
 *				  -2 - index out of range		*
 **									*
 * Log:									*
 * C. Lin/EAI	 	 8/98						*
 * A. Hardy/GSC		 4/00		Added font size multiplier      *
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

	*xfont = FszTbl.info[index].xval;

	if ( *xfont >= 24 ) {
	    *mult = 240;
	}
	else {
	    *mult = *xfont * 10;
	}
}

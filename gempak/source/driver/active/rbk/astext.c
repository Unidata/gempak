#include "ardcmn.h"

void astext ( int *itxfn, float *sztext, int *ijust, float *txsize, int *iret )
/************************************************************************
 * astext 								*
 * 									*
 * This subroutine sets the text size for hardware text	generation.     *
 * For the UTF driver only sizes of 0 - 3 have meaning, as that is all  *
 * that can be encoded into the 2 bits alotted to text size in the UTF  *
 * format.  The value of sztext is converted to an integer and stored	*
 * in nfntsz for use by atext.						*
 * 									*
 * Note that the text font has no meaning to the UTF driver.  AFOS has  *
 * only one text font.							*
 *									*
 * astext  ( itxfn, sztext, ijust, txsize, iret )			*
 *									*
 * Input parameters:							*
 * 	*itxfn		int		Text font			*
 * 	*sztext		float		Text size			*
 * 	*ijust		int		Text justification		*
 *									*
 * Output parameters:							*
 *	*txsize		float		Actual text size set		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		9/98		Modified from USTEXT            *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Correlate the AWIPS text size to the floating point size
 *	requested by the user. There are only 2 AFOS font sizes,
 *	therefore, any size < 1.5 is set to 1.0 and any size => 1.5
 *	is set to 2.0.
 */
	if  ( *sztext < 1.5 )  {
	    nfntsz  = 0;
	    asize   = 1.0;
	    *txsize = 1.0;
	}
	else  {
	    nfntsz  = 3;
	    asize   = 2.0;
	    *txsize = 2.0;
	}

/*
 *	Save the justification.
 */
	kjust = *ijust;

}

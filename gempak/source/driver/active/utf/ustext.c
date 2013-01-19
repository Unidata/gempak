#include "utfcmn.h"

void
ustext ( int *itxfn, float *sztext, int *ijust, float *txsize, int *iret )
/************************************************************************
 * ustext 								*
 * 									*
 * This subroutine sets the text size for hardware text	generation.     *
 * For the UTF driver only sizes of 0 - 3 have meaning, as that is all  *
 * that can be encoded into the 2 bits alotted to text size in the UTF  *
 * format.  The value of sztext is converted to an integer and stored	*
 * in nfntsz for use by utext.						*
 * 									*
 * Note that the text font has no meaning to the UTF driver.  AFOS has  *
 * only one text font.							*
 *									*
 * ustext  ( itxfn, sztext, ijust, txsize, iret )			*
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
 * E. Safford/GSC	03/97	Added to UTF driver			*
 * S. Jacobs/NCEP	 6/97	Changed font size computation		*
 * S. Jacobs/NCEP	 7/97	Added global variable asize		*
 * S. Jacobs/NCEP	 8/97	Cleaned up header			*
 * S. Jacobs/NCEP	11/97	Added justification to calling seq	*
 * S. Jacobs/NCEP	 7/98	Added actual text size			*
 * S. Jacobs/NCEP	 7/98	Changed to only set two different sizes	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Correlate the AFOS text size to the floating point size
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

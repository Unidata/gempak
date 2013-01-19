#include "pscmn.h"

void pstext ( int *itxfn, float *sztext, int *ijust, float *txsize, int *iret )
/************************************************************************
 * pstext 								*
 * 									*
 * This subroutine sets the text font number and size for hardware text	*
 * generation.  For some devices, the font number and size information	*
 * must be saved in a common area to be retrieved when the test is 	*
 * drawn.  This subroutine will be called only when hardware text is 	*
 * available.								*
 * 									*
 * pstext  ( itxfn, sztext, ijust, txsize, iret )			*
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
 * A. Chang/EAI		 2/94						*
 * S. Jacobs/NCEP	11/97	Added justification to calling seq	*
 * S. Jacobs/NCEP	 7/98	Added actual text size			*
 ***********************************************************************/
{

	int	iftyp1, iftyp2, nmfont;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Get valid text font and save in common.
 */
	iftyp1 = *itxfn / 10;
	iftyp2 = *itxfn % 10;
	if  ( ( iftyp1 < 0 ) || ( iftyp1 > 3 ) ) iftyp1 = 0;
	if  ( ( iftyp2 < 1 ) || ( iftyp2 > 3 ) ) iftyp2 = 1;
	nmfont = iftyp1 * 3 + iftyp2;

/*
 *	Save font number and size in common.
 */
	irfont = nmfont;
	if  ( *sztext > 0. )  txsizr = *sztext;
	*txsize = txsizr;

/*
 *	Save the justification.
 */
	kjust = *ijust;

}

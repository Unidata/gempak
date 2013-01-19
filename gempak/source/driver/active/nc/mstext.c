#include "nccmn.h"

void mstext ( int *font, float *size, int *ijust, float *txsize, int *iret )
/************************************************************************
 * mstext								*
 *									*
 * This subroutine sets text attributes for the metafile.		*
 *									*
 * mstext  ( font, size, ijust, txsize, iret )				*
 *									*
 * Input parameters:							*
 *	*font 		int 		Text index			*
 *	*size		float		Text size			*
 *	*ijust		int		Text justification		*
 *									*
 * Output parameters:							*
 *	*txsize		float		Actual text size set		*
 *	*iret		int 		Return code 			*
 **									*
 * Log:									*
 * C. Lin/EAI		 1/93						*
 * C. Lin/EAI		 4/93	Add Internal Buffer			*
 * A. Chang/EAI          1/94	Modified to buffered I/O		*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Jacobs/NCEP	 9/97	Added text justification		*
 * S. Jacobs/NCEP	 7/98	Added actual text size			*
 * A. Hardy/GSC		 4/00   Added point size 34			*
 ***********************************************************************/
{

	static int	fontsz[] = { 8, 10, 12, 14, 18, 24, 34 };
	int		i, iftyp1, iftyp2, nfnt;
	float		avgsiz;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *      Get valid text font and save.
 */
	iftyp1 = (*font) / 10;
	iftyp2 = (*font) % 10;
	if  ( ( iftyp1 < 0 ) || ( iftyp1 > 3 ) ) iftyp1 = 0;
	if  ( ( iftyp2 < 1 ) || ( iftyp2 > 3 ) ) iftyp2 = 1;
	txfont_req = 3 * iftyp1 + iftyp2;

/*
 *      Get valid text size and save.
 */
	nfnt = sizeof(fontsz) / sizeof(fontsz[0]);
	txsize_req = fontsz[0];
	tsize = *size;
	for ( i = 1; i < nfnt; i++ ) {
	    avgsiz = ( (fontsz[i-1]/14.0) + (fontsz[i]/14.0) ) / 2.0;
	    if ( *size > avgsiz ) {
		txsize_req = (short) fontsz[i];
		tsize = fontsz[i] / 14.0;
		*txsize = tsize;
	    }
	}

/*
 *	Set the text jusification.
 */
	txalgn_req = *ijust;

}

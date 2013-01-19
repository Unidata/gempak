#include "xwcmn.h"


int		nfont = 0;
Font_str	loadfonts[MAX_LOAD_FONTS];

/************************************************************************
 * xstext.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*=====================================================================*/

void xstext ( int *itxfn, float *txtsz, int *ijust, float *txsize, 
							int *iret )
/************************************************************************
 * xstext								*
 *									*
 * This subroutine gets the name of the hardware font to be used	*
 * and computes the actual font size to be used.			*
 *									*
 * void xstext ( itxfn, txtsz, ijust, txsize, iret )			*
 *									*
 * Input parameters:							*
 *	*itxfn		int		Number of desired font		*
 *	*txtsz		float		Requested text size		*
 *	*ijust		int		Text justification		*
 *									*
 * Output parameters:							*
 *	*txsize		float		Actual text size set		*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91						*
 * S. Jacobs/EAI	10/92	Changed comp of font name and size	*
 * C. Lin/EAI		 9/93	Added check for loaded fonts		*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window, multi-pixmap		*
 * M. desJardins/NMC	10/94	Eliminate check for current font	*
 * C. Lin/EAI           11/94   use XQueryTextExtents instead of        *
 *                              of XQueryFont to avoid font_info,       *
 *				therefore possible memory leak.		*
 * S. Jacobs/NCEP	 6/97	Changed calculation of text point sizes	*
 * S. Jacobs/NCEP	11/97	Added justification to calling seq	*
 * S. Jacobs/NCEP	 2/98	Changed calc of Y size of font		*
 * S. Jacobs/NCEP	 7/98	Removed calc of txszx and txszy		*
 * S. Jacobs/NCEP	 7/98	Added actual text size			*
 * S. Jacobs/NCEP	 4/00	Added point size 34			*
 * B. Yin/SAIC    	 3/04   Modified code to use iso-8859-1		*
 ***********************************************************************/
{
	char		fontname[80];
	int		i, size, mult, iftyp1, iftyp2, nfnt;
	float		avgsiz;
	Font		fontid;

	static char	*fonts[12] = {
				"-adobe-courier-medium-r-normal--",
				"-adobe-helvetica-medium-r-normal--",
				"-adobe-times-medium-r-normal--",
				"-adobe-courier-medium-o-normal--",
				"-adobe-helvetica-medium-o-normal--",
				"-adobe-times-medium-i-normal--",
				"-adobe-courier-bold-r-normal--",
				"-adobe-helvetica-bold-r-normal--",
				"-adobe-times-bold-r-normal--",
				"-adobe-courier-bold-o-normal--",
				"-adobe-helvetica-bold-o-normal--",
				"-adobe-times-bold-i-normal--"
			};

	static int	fontsz[] = {  8,  10,  12,  14,  18,  24,  34 };
	static int	fntmul[] = { 80, 100, 120, 140, 180, 240, 240 };

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	/*
 	 * Get valid text font and save the values.
 	 */

	iftyp1 = *itxfn / 10;
	iftyp2 = *itxfn % 10;

	if  ( ( iftyp1 < 0 ) || ( iftyp1 > 3 ) ) iftyp1 = 0;
	if  ( ( iftyp2 < 1 ) || ( iftyp2 > 3 ) ) iftyp2 = 1;

	txfont_req = iftyp1 * 3 + iftyp2;

	if  ( *txtsz > 0. )  {
	    txsize_req = *txtsz;
	}
	else {
	    txsize_req = 1.;
	}

	/*
 	 * Get the size of the font in point values.
 	 */

	nfnt = sizeof(fontsz) / sizeof(fontsz[0]);
	size = fontsz[0];
	mult = fntmul[0];
	for ( i = 1; i < nfnt; i++ ) {
	    avgsiz = ( (fontsz[i-1]/14.0) + (fontsz[i]/14.0) ) / 2.0;
	    if ( txsize_req > avgsiz ) {
		size = fontsz[i];
		mult = fntmul[i];
	    }
	}
	*txsize = size / 14.0;
	txsize_req = *txsize;

	/*
 	 * Get the name of the font.
 	 */

	sprintf ( fontname, "%s%d-%d-*-*-*-*-iso8859-1",
				fonts[txfont_req-1], size, mult );

	/*
 	 * Check if the font is already loaded.
 	 * Load the font and set the font character height and widths.
 	 */

	fontid = -1;

	for ( i = 0; i < nfont; i++ ) {
	    if ( strcmp ( fontname, loadfonts[i].name ) == 0 ) {
		fontid = loadfonts[i].id;
	    }
	}

	if ( fontid == (Font)-1 ) {

	    fontid = XLoadFont ( gemdisplay, fontname );

	    if ( nfont < MAX_LOAD_FONTS ) {
		strcpy ( loadfonts[nfont].name, fontname );
		loadfonts[nfont].id = fontid;
		nfont ++;
	    }
	    else {
		xrmfnt ( 0 );
		strcpy ( loadfonts[MAX_LOAD_FONTS-1].name, fontname );
		loadfonts[MAX_LOAD_FONTS-1].id = fontid;
		nfont = MAX_LOAD_FONTS;
	    }
	}

	XSetFont ( gemdisplay, gemwindow[current_window].gc, fontid ); 

	txfont_set = txfont_req;
	txsize_set = txsize_req;

/*
 *	Save the text justification.
 */
	kjust = *ijust;

}

/*=====================================================================*/

void xrmfnt ( int font_no )
/************************************************************************
 * xrmfnt								*
 *									*
 * This function will unload the numbered font in the internal table	*
 * and move the rest of the fonts forward in the array.			*
 *									*
 * xrmfnt ( font_no )							*
 *									*
 * Input parameters:							*
 *	font_no		int		Font number to unload 		*
 **									*
 * Log:									*
 * C. Lin/EAI            9/93        					*
 * C. Lin/EAI            2/95   Add check for font_no		        *
 ***********************************************************************/
{
	int	i;

/*---------------------------------------------------------------------*/

	if ( font_no > MAX_LOAD_FONTS )
		return;

	XUnloadFont ( gemdisplay, loadfonts[font_no].id );

	for ( i = font_no+1; i < nfont; i++ ) {
	    strcpy( loadfonts[i-1].name, loadfonts[i].name );
	    loadfonts[i-1].id = loadfonts[i].id;
	}

}

#include "xwcmn.h"
#include "color.h"

void xscnam ( int *cbank, int *cindex, char *cname, int *len, int *iret )
/************************************************************************
 * xscnam								*
 *									*
 * This routine defines the color by name				*
 *									*
 * xscnam  ( cbank, cindex, cname, len, iret )				*
 *									*
 * Input parameters:							*
 *	*cbank		int		Color bank ID 			*
 *	*cindex		int		Color number			*
 *	*cname		char		Color name			*
 *	*len		int		length of color name		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *			G_NORMAL   = Normal return			*
 *			G_NCBALOC  = bank not allocated			*
 *			G_NICBANK  = invalid color bank ID    		*
 *			-1	   = invalid color index		*
 *			-2	   = invalid color name			*
 **									*
 * Log:									*
 * L. Williams/EAI	 3/96						*
 * S. Jacobs/NCEP	 9/02	Added support for 16 and 24 bit graphics*
 * T. Piper/SAIC	07/04	Added use of COLR_SCAL everywhere	*
 ***********************************************************************/
{
GmcColor	*gmc;
XColor		color, exact_def;
XColor		screen_def;
int		ityp, ier;
int		red, green, blue;
int		ctmp, indx, tindex;
char		tname[40];
int		xdpth;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	gmc = &gemCmap.color[0];

	/*
	 * Exit if GColor is not initialized
	 */
	if ( !GColorIsInitialized ) {
		*iret = G_NCBALOC;
		return;
	}

	/*
	 * Exit if color bank number invalid
	 */
	if ( *cbank != 0 ) { 
	    *iret = G_NICBANK;
	    return;
	}

	/*
	 * Check for background color.
	 */
	if  ( ( *cindex == 101 ) || ( *cindex == 0 ) ) {
		ctmp = ibkcol;
	}
	else if  ( ( *cindex >= 1 ) && 
			( *cindex < ColorBanks.banks[GraphCid] ) ) {
	    ctmp = *cindex;
	}
	else {

	   /*
	    * Otherwise, do nothing.
	    */
	    *iret = -1;
	    return;
	}

	xdpth = DefaultDepth ( (XtPointer)gemdisplay,
				DefaultScreen((XtPointer)gemdisplay) );

	cname[*len] = '\0';

	/*
	 * Save original data.
	 */
	tindex = gemColrs[ctmp].index;
	strcpy(tname, gemColrs[ctmp].name);
	red = gemColrs[ctmp].red;
	green = gemColrs[ctmp].green;
	blue = gemColrs[ctmp].blue;
	
	cscnam( &ctmp, cname, &ier );

	indx = gemColrs[ctmp].index;

	if ( ier == 0 ) {

	   /*
	    * Retrieve color name from the GEMPAK color table.
	    */
	   red = gmc[indx].red;
	   green = gmc[indx].green;
	   blue = gmc[indx].blue;
	}

	else {

	   if ( ( ier == 2 ) && (gemColrs[ctmp].red == -1) &&
		    (gemColrs[ctmp].green == -1) &&
		    (gemColrs[ctmp].blue == -1) ) {

	      /*
	       * Retrieve color name from the XColor table
	       */
	      if ( XLookupColor( gemdisplay, gemmap, cname, 
				 &exact_def, &screen_def ) ) {

		 red =  exact_def.red / COLR_SCAL; 
		 green = exact_def.green / COLR_SCAL; 
		 blue = exact_def.blue / COLR_SCAL; 

		 /*
		  * Set RBG in current table
		  */
		 cscrgb(&ctmp, &red, &green, &blue, iret); 

		 if ( *iret == 2 )

		    /*
		     * Set xcolor name
		     */
		    strcpy(gemColrs[ctmp].name, cname);
		 else 
		    if ( ier > 0 )
			return;

	      }
	      else {
		 /*
		  * Color name not found in XColor table.  Set the
		  * original index, color name, and RGB values.
		  */
		 gemColrs[ctmp].index = tindex;
		 strcpy(gemColrs[ctmp].name, tname);
		 gemColrs[ctmp].red = red;
		 gemColrs[ctmp].green = green;
		 gemColrs[ctmp].blue = blue;
		 *iret = -2;
	      }
	   }
	} 


	/*
 	 * Scale color components to integers 0 - 255.
 	 */

	color.flags = DoRed | DoGreen | DoBlue;
	color.red   = (red + 1) * COLR_SCAL - 1;
	color.green = (green + 1) * COLR_SCAL - 1;
	color.blue  = (blue + 1) * COLR_SCAL - 1;

	/*
 	 * Set the color in the specified color pixel.
 	 */
	if( xdpth == 8 ) {
	   color.pixel  = ColorBanks.colrs[GraphCid][ctmp];
	   XStoreColor( gemdisplay, gemmap, &color);
	}
	else {  /* 16-bit and 24-bit screens */
	   /* Allocate read-only color cell defined in "color" */
	   if( !XAllocColor( gemdisplay, gemmap, &color ) ) {
	      printf("xscrgb:  Error calling XAllocColor!\n");
	      fflush(stdout);
	   }
	   else {
	      /* Save the color in ColorBanks.colrs */
	      ColorBanks.colrs[GraphCid][ctmp]=color.pixel;
	   }
	}


	/*
 	 * If this is the current foreground color, 
	 *	it must be reset.
 	 */

	if  (  ifrcol == ctmp  )  {
	    	ityp = GraphCid;
	    	xscolr ( &ityp, &ifrcol, &ier );
	}

}

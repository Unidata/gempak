#include "xwcmn.h"
#include "color.h"

void xscrgb ( int *cbank, int *jcolr, int *ired, int *igreen,
					int *iblue, int *iret )
/************************************************************************
 * xscrgb								*
 *									*
 * This subroutine defines the color components of a color by		*
 * specifying the values of red, green, and blue.  The color components	*
 * must be in the range 0 - 255.					*
 *									*
 * xscrgb  ( cbank, jcolr, ired, igreen, iblue, iret )			*
 *									*
 * Input parameters:							*
 *	*cbank		int		Color bank ID 			*
 *	*jcolr		int		Color number			*
 *	*ired		int		Red color component		*
 *	*igreen		int		Green color component		*
 *	*iblue		int		Blue color component		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *			G_NORMAL   = Normal return			*
 *			G_NCBALOC  = bank not allocated			*
 *			G_NICBANK  = invalid color bank ID    		*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/91	GEMPAK 5.1				*
 * C. Lin/EAI            1/93   Change color allocation; Added type	*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI            1/95   Added radar type			*
 * C. Lin/EAI            3/95   Restructure; clean up; error handling	*
 *				Use ColorBanks structure		*
 * G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
 * J. Cowie/COMET       11/95   Check color bank range, allocation      *
 * C. Lin/EAI           12/95	clrsalloc -> allocflag[cbank] 		*
 * L. Williams/EAI	 2/96	Added call to CSCRGB; check for gempak's*
 *				color number 101			*
 * R. Tian/SAIC		05/02	Added FaxCid				*
 * A. Person/Penn State 06/02	Updated to support 16- and 24-bit	* 
 *				graphics				*
 * T. Piper/SAIC	02/05	Added allocflag[GraphCid] check		*
 ***********************************************************************/
{
GmcColor	*gmc;
XColor		color;
int		ctmp, ityp, ier;
int		indx;
int		xdpth;

/*---------------------------------------------------------------------*/

	xdpth = DefaultDepth ( (XtPointer)gemdisplay,
				DefaultScreen((XtPointer)gemdisplay) );

	*iret = G_NORMAL;

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
	if ( (*cbank >= ColorBanks.nbank ) ||
	     ( *cbank < 0 ) ) {
	    *iret = G_NICBANK;
	    return;
	}


	/*
	 * Update colors components currently used in driver
	 */
	if ( *cbank == 0 ) {
	   
	   /*
	    * Check for background color.
	    */

	    if  ( ( *jcolr == 101 ) || ( *jcolr == 0 ) ) {
		ctmp = ibkcol;

	    }
	    else if  ( ( *jcolr >= 1 ) && 
				( *jcolr < ColorBanks.banks[GraphCid] ) ) {
		ctmp = *jcolr;
	    }
	    else {

	    /*
	     * Otherwise, do nothing.
	     */
	     *iret = -1;
	     return;
	    }

	   cscrgb(&ctmp, ired, igreen, iblue, &ier);

	   if ( ier < 0 ) {
	     *iret = -1;
	     return;
	   }
	   else 
	      if ( ( ier == 0 ) && ( *ired == -1 ) && ( *igreen == -1 )
			 && ( *iblue == -1) ) {
		 gmc = &gemCmap.color[0];
		 indx = gemColrs[ctmp].index;

		 *ired = gmc[indx].red;
		 *igreen = gmc[indx].green;
		 *iblue = gmc[indx].blue;
	      }
	}

	/*
 	 * Scale color components to X window range.
 	 */

	color.flags = DoRed | DoGreen | DoBlue;

	color.red   = (unsigned short)(*ired * COLR_SCAL);
	color.green = (unsigned short)(*igreen * COLR_SCAL);
	color.blue  = (unsigned short)(*iblue * COLR_SCAL);

	/*
 	 * Set the color in the specified color pixel.
 	 */

	if ( (*cbank == GraphCid) && allocflag[GraphCid] ) {

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
	else if ( (*cbank == SatCid) && allocflag[SatCid] ) {

		if (( *jcolr >= 0) && ( *jcolr < ColorBanks.banks[SatCid])) {
		   if( xdpth == 8 ) {
	    	      color.pixel  = ColorBanks.colrs[SatCid][*jcolr];
		      XStoreColor( gemdisplay, gemmap, &color);
		   }
		   else {  /* 16-bit and 32-bit screens */
		      /* Allocate read-only color cell defined in "color" */
		      if( !XAllocColor( gemdisplay, gemmap, &color ) ) {
		         printf("xscrgb:  Error calling XAllocColor!\n");
		         fflush(stdout);
		      }
		      else {
		         /* Save the color in ColorBanks.colrs */
		         ColorBanks.colrs[SatCid][*jcolr]=color.pixel;
		      }
		   }
		}

	}
	else if ( (*cbank == RadCid) && allocflag[RadCid] ) {

		if (( *jcolr >= 0) && ( *jcolr < ColorBanks.banks[RadCid])){
		   if( xdpth == 8 ) {
		      color.pixel = ColorBanks.colrs[RadCid][*jcolr];
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
		         ColorBanks.colrs[RadCid][*jcolr]=color.pixel;
		      }
		   }

		}

	}
	else if ( (*cbank == FaxCid) && allocflag[FaxCid] ) {

		if (( *jcolr >= 0) && ( *jcolr < ColorBanks.banks[FaxCid])){
		   if( xdpth == 8 ) {
			color.pixel  = ColorBanks.colrs[FaxCid][*jcolr];
			XStoreColor( gemdisplay, gemmap, &color);
		   }
		else {  /* 16-bit and 24-bit screens *//* Allocate read-only color cell defined in "color" */
                      if( !XAllocColor( gemdisplay, gemmap, &color ) ) {
                         printf("xscrgb:  Error calling XAllocColor!\n");
                         fflush(stdout);
                      }
                      else {
                         /* Save the color in ColorBanks.colrs */
                         ColorBanks.colrs[FaxCid][*jcolr]=color.pixel;
                      }
                   }
		}

	}


}

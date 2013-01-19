#include "xwcmn.h"
#include "color.h"

void xcamgr ( Display *dpy, Colormap cmap, int nbank, int *banks,
					int *return_banks, int *iret )
/************************************************************************
 * xcamgr								*
 *									*
 * This subroutine manages color allocation.  It allocates read/write   *
 * color cells, fills in the shared color structure and stores the	*
 * information into the X server. Therefore, the read/write color cells *
 * can be shared with other applications.  It is intended to be called  *
 * by a color manager program. 						*
 *									*
 * xcamgr ( dpy, cmap, nbank, banks, return_banks, iret )		*
 *									*
 * Input parameters:							*
 *	*dpy           Display    Connection to the server		*
 *      cmap           Colormap   specifies the ID of the colormap.     *
 *	nbank	       int        Number of color banks 		*
 *	*banks	       int        Number of colors for each color bank  *
 *									*
 * Output parameters:							*
 * 	*return_banks	int						*
 *	*iret		int		Return code			*
 *              G_NORMAL    = normal return.                            *
 *									*
 *              G_NCLRAL    = color allocation failure                  *
 *              G_BADATOM   = error in interning an atom. (xcsdat)  	*
 **									*
 * Log:									*
 * C. Lin/EAI		 3/95						*
 * C. Lin/EAI		 7/95	add checking if banks[i] = 0		*
 * A. Person/Penn State 06/02	Updated to support 16- and 24-bit	*
 *				graphics				*
 * S. Chiswell/Unidata	06/02	Init all colors to black for 16,24-bit	*
 * T. Piper/SAIC	01/04	changed type of pxls to Pixel		*
 ***********************************************************************/

{
unsigned char   sendcolr[MAXCOLORDATA];
unsigned char   *dptr;
Pixel		pxls[MAXCOLORS];
int             nbyte;
char            shared_color_flag;
int             cbank, jj, xdpth;

unsigned char   ignore;
XColor		black;
/*---------------------------------------------------------------------*/

/*
 * fill in the shared color structure
 */
	dptr = sendcolr;

	ColorBanks.nbank = nbank;
	*dptr++ = (unsigned char )nbank;

	ColorBanks.banks =(int *)malloc( nbank * sizeof(int ));

	for ( cbank = 0; cbank < nbank; cbank++) { 
		return_banks[cbank] = 0;
		ColorBanks.banks[cbank] = banks[cbank];
		*dptr++ = (unsigned char )banks[cbank];
	}

	nbyte = 1 + nbank;

	ColorBanks.colrs =(Pixel **)malloc( nbank * sizeof(Pixel *));

	xdpth = DefaultDepth( (XtPointer)dpy,
				DefaultScreen((XtPointer)dpy) );

	if ( xdpth > 8 ) {
           /*
            * Get the black pixel value for 16/24/32 bit background.
            */
           black.flags = DoRed | DoGreen | DoBlue;
           black.red   = black.green = black.blue = 0;
	   XAllocColor( gemdisplay, gemmap, &black );
	}

	/*
	 * fill in the color indicies for each color bank
	 */
	for ( cbank = 0; cbank < nbank; cbank++) {

	    if ( banks[cbank] > 0 ) {

		if( xdpth == 8 ) {
		    /*
		     * allocate color cells for each color bank
		     */
		    *iret = xgrwc ( gemdisplay, gemmap, pxls,
                                banks[cbank]);
		    if  ( *iret == G_NORMAL ) {
			    return_banks[cbank] = banks[cbank];
			    ColorBanks.colrs[cbank] = (Pixel *)
				    malloc(banks[cbank] * sizeof(Pixel));

			    for (jj = 0; jj < banks[cbank]; jj++) { 
				    ColorBanks.colrs[cbank][jj] = (unsigned char)pxls[jj];
				    *dptr++ = (unsigned char)pxls[jj];
			    }

			    nbyte += banks[cbank];
		    } 
		    else {
			    /*
			     * no more color resources
			     */ 
			    return_banks[cbank] = 0;
			    *iret = G_NCLRAL;
                	    return;
		    }
		}
		/* 16-bit, 24-bit, and 32-bit screen always returns available 
		   colors but the values are actually defined later.  Initialize
		   to the background color here.
		 */
		else if ( xdpth == 16 || xdpth == 24 || xdpth == 32 ) {
		    return_banks[cbank] = banks[cbank];
		    ColorBanks.colrs[cbank] = (Pixel *)
				malloc(banks[cbank] * sizeof(Pixel));
		    for (jj = 0; jj < banks[cbank]; jj++) {
		       ColorBanks.colrs[cbank][jj] = black.pixel;
		    }
		    *iret = G_NORMAL;
		}
		else {
		    *iret = G_NCLRAL;  /* Disallow other depths */
		}
	    }
	    else {
		*iret = G_ZEROCB;
	    }
	}

	/* Perform only for 8-bit since 16-, 24-, 32-bit are not yet defined */
	if(xdpth == 8 ) xcsdat(dpy, ShareColorData, (char *)sendcolr, nbyte);
		
	/*
	 * set the share color flag
	 */
	shared_color_flag = 1;
	xcsdat(dpy, ShareColorFlag, &shared_color_flag, 1);
	nbyte = 1;
	xgsdat(dpy, ShareColorFlag, &ignore, (unsigned int *)&nbyte);

}

void xdsclr ( Display *dpy )
/************************************************************************
 * xdsclr                                          			*
 *                                                                      *
 * This function signals there is no shared color mechanism among the   *
 * applications. It is intended to be called by the color manager       *
 * program.  								*
 *                                                                      *
 * void xdsclr( dpy )							*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dpy             Display       specifies a connection to            *
 *                                      an X server.                    *
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       03/95                                               *
 ***********************************************************************/
{
unsigned char ignore;
char shared_color_flag;
int  nbyte;


        /*
         *  reset the shared_color_flag.
         *  The flag is stored via an atom identified
         *  by ShareColorFlag.
         */
        shared_color_flag = 0;
        xcsdat(dpy, ShareColorFlag, &shared_color_flag, 1);
	nbyte = 1;
	xgsdat(dpy, ShareColorFlag, &ignore, (unsigned int *)&nbyte);
}
 

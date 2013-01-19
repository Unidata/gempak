#include "xwcmn.h"

void xsncolr ( char *cname, Pixel *pcolor, int *iret )
/************************************************************************
 * xsncolr                                                              *
 *                                                                      *
 * This subroutine allocates a named color for the X Windows device   	*
 * driver.                                                              *
 *                                                                      *
 * xsncolr  ( cname, pcolor, iret )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *cname		char	X Color name				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *pcolor         Pixel   Color pixel number                      *
 *      *iret           int     Return code                     	*
 *                              G_NORMAL = normal return        	*
 *                              G_NCLRAL = color allocation failure     *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	05/03	Created					*
 ***********************************************************************/
{
    XColor      xcolor, xrgb;
/*---------------------------------------------------------------------*/

    if ( XAllocNamedColor(gemdisplay, gemmap, cname, &xcolor, &xrgb) ) {
	*pcolor = xcolor.pixel;
	*iret = G_NORMAL;
    }
    else {
	*pcolor = (Pixel)IMISSD;
	*iret = G_NCLRAL;
    }
    return;
}	

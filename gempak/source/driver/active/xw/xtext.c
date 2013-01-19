#include "xwcmn.h"

void xtext ( float *xr, float *yr, char strout[], int *lenstr, 
		int *ixoff, int *iyoff, float *rotat, int *ispanx, 
		int *ispany, int *icleft, int *icrght, int *icbot, 
		int *ictop, int *iret )
/************************************************************************
 * xtext								*
 *									*
 * This subroutine draws a character string for the X window driver.	*
 * Location of the given text string is determined by the given point   *
 * and the alignment type. Rotation has not been implemented in current *
 * version.								*
 *									*
 * xtext ( xr, yr, strout, lenstr, ixoff, iyoff, rotat,			*
 *	   ispanx, ispany, icleft, icrght, icbot, ictop, iret )		*
 *									*
 * Input parameters:							*
 *	*xr		float		X coordinate			*
 *	*yr		float		Y coordinate			*
 *	strout[]	char		String				*
 *	*lenstr		int		Length of string		*
 *	*ixoff		int		X offset			*
 *	*iyoff		int		Y offset			*
 *	*rotat		float		Rotation angle			*
 *	*ispanx		int		Direction of increasing x	*
 *	*ispany		int		Direction of increasing y	*
 *	*icleft		int		Left clipping bound		*
 *	*icrght		int		Right clipping bound		*
 *	*icbot		int		Bottom clipping bound		*
 *	*ictop		int		Top clipping bound		*
 *									*
 * Output parameters:							*
 *	*iret		int		Reutrn code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI		 7/94	Multi-window, multi-pixmap		*
 * S. Jacobs/NCEP	 9/97	Added justification code from XTEXTC	*
 * S. Jacobs/NCEP	12/97	Added clipping flag to justification	*
 * S. Jacobs/NCEP	 1/98	Added adjustment for starting location	*
 * S. Jacobs/NCEP	 7/98	Added alignment and bound checks	*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ipxm, lp, direction, ascent, descent;
    int		width, jx, jy, jxoend;
    float	xo, yo, xx, yy, xadj;

    Pixmap	gempixmap; 
    GC		gemgc; 
    Window_str	*cwin;

    XCharStruct	overall;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin	= &(gemwindow[current_window]);
    lp		= cwin->curr_loop;
    ipxm	= cwin->curpxm[lp]; 
    gempixmap	= cwin->pxms[lp][ipxm];
    gemgc	= gemwindow[current_window].gc; 

    /*
     * Get string width for alignment
     */

    XQueryTextExtents ( gemdisplay, XGContextFromGC(gemgc),
			strout, *lenstr, &direction, &ascent,
			&descent, &overall );
    width = overall.width;

    /*
     * Set the alignment.
     */
    if  ( kjust == 1 )  {
	xadj = 0.0;
    }
    else if  ( kjust == 3 )  {
	xadj = width - ( width / *lenstr );
    }
    else {
	xadj = ( width - ( width / *lenstr ) ) / 2.0;
    }

    /*
     * Convert the text location to integers.
     */
    xo = ( ( *ixoff - 0.75 ) / 2.0 ) * txszx * txsize_req - xadj;
    xx = *xr + xo * *ispanx;
    jx = G_NINT ( xx );

    yo = ( ( *iyoff - 0.75 ) / 2.0 ) * txszy * txsize_req ;
    yy = *yr + yo * *ispany;
    jy = G_NINT ( yy );

    /*
     * Check to see if the start point is outside the clipping
     * window. Then the end point is checked. Rotation is not
     * taken into account.
     */
    jxoend = jx + width;
    if  ( ( *ispanx * ( jx - *icleft ) < 0 )  ||
	  ( *ispanx * ( jx - *icrght ) > 0 )  ||
	  ( *ispany * ( jy - *icbot  ) < 0 )  ||
	  ( *ispany * ( jy - *ictop  ) > 0 )  ||
	  ( *ispanx * ( jxoend - *icrght ) > 0 ) )  return;

    /*
     * Draw string on window
     */
    XDrawString (gemdisplay, gempixmap, gemgc, jx, jy, strout, *lenstr);

}

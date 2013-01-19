#include "xwcmn.h"

void xtextc ( float *xr, float *yr, char strout[], int *lenstr, 
		int *ixoff, int *iyoff, float *rotat, int *ispanx, 
		int *ispany, int *icleft, int *icrght, int *icbot, 
		int *ictop, int *iret )
/************************************************************************
 * xtextc								*
 *									*
 * This subroutine draws a character string for the X window driver.	*
 * Location of the given text string is determined by the given point   *
 * and the alignment type. Rotation has not been implemented in current *
 * version.								*
 *									*
 * xtextc ( xr, yr, strout, lenstr, ixoff, iyoff, rotat,		*
 *	    ispanx, ispany, icleft, icrght, icbot, ictop, iret )	*
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
 * A. Chang/EAI	 	 8/94	Modified from XTEXT			*
 * M. desJardins/NMC	10/94	Move *ix into temporary variable	*
 * C. Lin/EAI           11/94   use XQueryTextExtents instead of        *
 *                              of XQueryFont to avoid font_info,       *
 *                              therefore avoid memory leak.            *
 * C. Lin/EAI		 4/95   bug fix for left justified              *
 * S. Jacobs/NCEP	10/96	Changed lenstr from long int to int	*
 * S. Jacobs/NCEP	 7/98	Updated to be like XTEXT		*
 * S. Jacobs/NCEP	 7/98	Recompute jx for long titles		*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 ***********************************************************************/
{
    int		ipxm, lp, direction, ascent, descent, width, jx, jy; 
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

    /*
     * If the start point for the text is outside of the plot
     * area, reset it to 10 pixels inside the plot area.
     */
    if  ( *ispanx * ( jx - *icleft ) < 0 )  {
	jx = *icleft + 10;
    }

    yo = ( ( *iyoff - 0.75 ) / 2.0 ) * txszy * txsize_req ;
    yy = *yr + yo * *ispany;
    jy = G_NINT ( yy );

    /*
     * Check to see if the start point is outside the clipping
     * window. Then the end point is not checked. Rotation is not
     * taken into account.
     */
    if  ( ( *ispanx * ( jx - *icleft ) < 0 )  ||
	  ( *ispanx * ( jx - *icrght ) > 0 )  ||
	  ( *ispany * ( jy - *icbot  ) < 0 )  ||
	  ( *ispany * ( jy - *ictop  ) > 0 ) )  return;

    /*
     * Draw string on window
     */
    XDrawString ( gemdisplay, gempixmap, gemgc, jx, jy, strout,
		  *lenstr );

}

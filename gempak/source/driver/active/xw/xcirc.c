#include "xwcmn.h"

void xcirc ( float *xcen, float *ycen, float *xrad, float *yrad, int *iret )
/************************************************************************
 * xcirc								*
 *									*
 * This subroutine draws circles.  					*
 *									*
 * xcirc ( xcen, ycen, xrad, yrad, iret )				*
 *									*
 * Input parameters:							*
 *	*xcen 		float		X center coordinate		*
 *	*ycen		float		Y center coordinate		*
 *	*xrad 		float		X radius point                  *
 *	*yrad 		float		Y radius point                  *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		11/98                                           * 
 * E. Safford/GSC	12/99	update for nmap2, add cwin shortcut	*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ixnew, iynew, lp, ipxm, ilwid, ix, iy;
    double	dx, dy;
    float	rad;

    Window_str	*cwin;
    GC		gemgc; 
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    cwin  = &(gemwindow[current_window]);
    lp = cwin->curr_loop;    

    /*
     *	Calculate radius of circle to draw.
     */
    dx = *xcen - *xrad;
    dy = *ycen - *yrad;
    rad = ( float ) sqrt ( dx * dx  +  dy * dy  );
    ilwid = G_NINT ( rad );
    ix = G_NINT ( *xcen );
    iy = G_NINT ( *ycen );


    /*
     * Transform x and y to the upper left corner of bounding
     * rectangle.
     */
    ixnew = ix - ilwid;
    iynew = iy - ilwid;

    /*
     * Fill in entire circle.  Note that start and end angles of
     * arc are in 64ths of degrees.
     */
    gemgc = cwin->gc;
    ipxm  = cwin->curpxm[cwin->curr_loop]; 

    XDrawArc ( gemdisplay, cwin->pxms[lp][ipxm], gemgc,
	       ixnew, iynew, ilwid*2, ilwid*2, 0, 360*64 ); 
}

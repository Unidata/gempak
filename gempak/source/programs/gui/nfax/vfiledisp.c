#include "geminc.h"
#include "cpgcmn.h"

extern Widget draw_area;

extern GC gc;      /* graphics context declaration */
extern int raster_up;


int vfiledisp ( char *fname, Cardinal xsz, Cardinal ysz, int samp_fact, 
				int mirror, int flip, Pixmap *pxmap )
/************************************************************************
 * VFILEDISP								*
 *									*
 * This function controls reading of a pixmap and display to a draw	*
 * area.								*
 *									*
 * int vfiledisp  ( fname, xsz, ysz, samp_fact, mirror, flip, pxmap )	*
 *									*
 * Input parameters:							*
 *	FNAME	char *		Name of file containing raster to dipl.	*
 *	XSZ	Cardinal	X size of map				*
 * 	YSZ	Cardinal	Y size of map				*
 *	SAMP_FACT int		Sampling factor of map			*
 *	MIRROR	int		True if horizontal flip needed		*
 * 	FLIP	int		True if vertical flip needed		*
 *									*
 * Output parameters:							*
 *	PXMAP	Pixmap *	Pixel map to get the data		*
 * 	vfiledisp	int						*
 **									*
 * Log:									*
 *  E. Wehner/EAi	6/96	Created					*
 *  R. Tian/SAIC        05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    int iret;
/* if an old map is allocated, delete it */
    if (raster_up)
    {
        XFreePixmap(XtDisplay(draw_area), *pxmap);
    }

/* read the bitmap file into the pixmap */
    if (vrdbmap( fname, xsz, ysz, samp_fact, mirror, flip, 
			pxmap, &iret) != 0)
        return -1;

    vshowmap(0, 0, 1, 1, gc, pxmap, draw_area);

    raster_up = 1;

    return 0;
}

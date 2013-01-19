#include "geminc.h"
#include "gemprm.h"

#include "ctbcmn.h"


void ctb_hfgetfont ( int *fn, int *mxpt, int *nc, int *ascv,
		     int *ixmin, int *ixmax,
		     int *npnts, int *ixc, int *iyc, int *iret )
/************************************************************************
 * ctb_pfgetfont							*
 *									*
 * This function returns the character information for the Hershey Font	*
 * that	matches the user requested font number.				*
 * 									*
 * ctb_hfgetfont ( fn, nc, ascv, ixmin, ixmax, npnts, ixc, iyc, iret )	*
 *									*
 * Input parameters:							*
 * 	*fn		int		Font number to find and return	*
 * 	*mxpt		int		Max number of points per char	*
 *									*
 * Output parameters:							*
 * 	*nc		int		Number of characters		*
 * 	*ascv		int		Array of ASCII values		*
 * 	*ixmin		int		Array of X minimum		*
 * 	*ixmax		int		Array of X maximum		*
 * 	*npnts		int		Array of number of points	*
 * 	*ixc		int		Array of X coordinates		*
 * 	*iyc		int		Array of Y coordinates		*
 *	*iret		int		Return code			*
 *					  -11 = font not found		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/07	Created					*
 ***********************************************************************/
{
    int		ifont, ichr, ipnt, idx;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Loop over the fonts checking the font number for a match.
     */
    for ( ifont = 0; ifont < _nhfont; ifont++ ) {

	/*
	 * If there is a match set all of the output arrays.
	 */
	if ( _hfontTbl[ifont].font_code == *fn )  {

	    /*
	     * Set the number of characters
	     */
	    *nc = _hfontTbl[ifont].numchr;

	    /*
	     * For each character, set the information needed to
	     * identify and draw that character.
	     */
	    for ( ichr = 0; ichr < _hfontTbl[ifont].numchr; ichr++ ) {

		ascv[ichr]  = _hfontTbl[ifont].character[ichr].ascii_val;
		npnts[ichr] = _hfontTbl[ifont].character[ichr].npts;
		ixmin[ichr] = _hfontTbl[ifont].character[ichr].xmin;
		ixmax[ichr] = _hfontTbl[ifont].character[ichr].xmax;

		/*
		 * Set the coordinates for the character.
		 */
		for ( ipnt = 0;
		      ipnt < _hfontTbl[ifont].character[ichr].npts;
		      ipnt++ ) {
		    idx = (ichr * *mxpt) + ipnt;
		    ixc[idx] = _hfontTbl[ifont].character[ichr].point[ipnt].x;
		    iyc[idx] = _hfontTbl[ifont].character[ichr].point[ipnt].y;
		}
	    }

	    /* Since the font was found, return. */
	    return;
	}
    }

    /*
     * If a font was not found, set the return code to an error.
     */
    *iret = -11;

}

/*=====================================================================*/

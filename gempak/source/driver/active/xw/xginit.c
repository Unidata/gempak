#include "xwcmn.h"

void xginit ( Widget wid, int *iret)
/************************************************************************
 * xginit								*
 *									*
 * This subroutine sets initial values in xwcmn.h for GUI applications	*
 *									*
 * xginit ( wid, iret )							*
 *									*
 * Input parameters:							*
 * wid		Widget		Widget ID				*
 * 									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	    G_NORMAL = normal return					*
 *	    G_NDISP   = DISPLAY not set or invalid			*
 **									*
 * Log:									*
 * T. Piper/SAIC	08/03	created					*
 * T. Piper/SAIC	07/04	removed xgbank and allocflag		*
 * T. Piper/SAIC	01/05	Added check on XmVersion		*
 * T. Piper/SAIC	04/05	Added initializing of allocflag		*
 ***********************************************************************/
{
    int		ii, gemscreen;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if ( XmVersion < 2001 )	{
	    printf("Your version of Motif, %s, is no longer supported.\n"
		"Please upgrade to at least Motif 2.1.\n", XmVERSION_STRING);
	    *iret = G_MOTIF;
	    return;
	}

/*
 * Set gemdisplay in xwcmn.h
 */
	gemdisplay = XtDisplay(wid);
	if ( gemdisplay == NULL ) {
	    *iret = G_NDISP;
	    return;
	}

/*
* Set default color map and create graphics contexts
*/
	gemscreen = DefaultScreen( (XtPointer)gemdisplay );
	gemmap    = DefaultColormap( (XtPointer)gemdisplay, gemscreen );
	gemvis    = DefaultVisual( (XtPointer)gemdisplay, gemscreen );
	root      = DefaultRootWindow( (XtPointer)gemdisplay );
	
/*
 *  Initialize allocflag for GUI applications
 */
	for ( ii = 0; ii < 4; ii++ ) {
	    allocflag[ii] = 0;
	}

}

#define XWGUI_GLOBAL
#define XWCMN_GLOBAL
#define XWPCMN_GLOBAL
#include "xwgui.h"
#include "color.h"

static int _setpxm = G_TRUE;

void xinita ( char *dev, int *lend, int *iunit, char *filnam, int *lenf,
		int *itype, float *xsize, float *ysize, int *ixsize,
		int *iysize, int *isxsiz, int *isysiz, int *ncurwn, 
		int *iret )
/************************************************************************
 * xinita								*
 *									*
 * This subroutine opens the graphics window and sets the initial	*
 * graphics context along with basic window attributes.			*
 *									*
 * xinita ( dev, lend, iunit, filnam, lenf, itype, xsize, ysize,	*
 *	    ixsize, iysize, isxsiz, isysiz, ncurwn, iret )		*
 *									*
 * Input parameters:							*
 *	*dev		char		Device name			*
 *	*lend		int		Length of device name		*
 *	*iunit		int		Type of output device		*
 *					  For XW:			*
 *					    1 = GEMPAK window		*
 *					    2 = Motif window		*
 *	*filnam		char		Window name			*
 *	*lenf		int		Length of window name		*
 *	*itype		int		Color type for the device	*
 *	*xsize		float		X size (pixels or screen fract)	*
 *	*ysize		float		Y size (pixels or screen fract)	*
 *									*
 * Output parameters:							*
 *	*ixsize		int		device X size in pixels		*
 *	*iysize		int		device Y size in pixels		*
 *	*isxsiz		int		screen X size in pixels		*
 *	*isysiz		int		screen Y size in pixels		*
 *	*ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 *	    G_NORMAL = normal return					*
 *          G_NWSIZE = new window size (xselwin)            		*
 *                                                                      *
 *	    G_NDISP   = DISPLAY not set or invalid			*
 *          G_NCLRAL  = color allocation failure (xcaloc)           	*
 *          G_NWINDW  = maximum # of windows opend(xselwin)  		*
 *          G_NIWNAM  = invalid window name (xselwin)     		*
 *          G_NIMGTBL = image table not found (xsattbl)             	*
 *									*
 *          G_NDWTBL  = cannot find dwell table, use defaults (xdwtbl)  *
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91						*
 * J. Whistler/SSAI	10/91	Added call to XSetFillRule		*
 * M. desJardins/NMC	12/91	GEMPAK 5.1 version			*
 * M. desJardins/NMC	 1/92	Changed definition of colors		*
 * S. Jacobs/NMC	 3/94	Added call to xsattbl			*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * C. Lin/EAI	         3/95	Add xgbank(); use new xcaloc()		*
 *				Error handling				*
 *			bug fix: allocate color before creating window  *
 * C. Lin/EAI	        12/95	clrsalloc -> allocflag[]  		*
 * M. Linda/GSC		 3/96	Added ncurwn				*
 * S. Jacobs/NCEP	 4/96	Added iret for case iunit not equal 1	*
 * S. Jacobs/NCEP	 5/96	Added new global variables for queries	*
 * S. Jacobs/NCEP	10/96	Added checks for curdev = XWP		*
 * C. Lin/EAI		 6/97	Added screen size in calling sequence	*
 * 				set the size info when iunit = 2	*
 * S. Wang/GSC		11/97   Take out color initializaition codes	*
 * S. Jacobs/NCEP	 7/98	Added setting of txszx and txszy	*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	02/99	intialize incr_pxmCnt flag		*
 * S. Jacobs/NCEP	11/99	Check to init pixmap counts only once	*
 * E. Safford/GSC	12/99	update for new xwcmn.h    		*
 * E. Safford/GSC	01/00	initialize _loopSet flags 		*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * E. Safford/GSC	05/00	add init of master, rfrsh, & bad_frm	*
 * E. Safford/GSC	05/00	fix initalization bug               	*
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * S. Law/GSC		06/00	removed master, xw_refresh, & pixmaps	*
 * T. Piper/GSC		11/00	added DISPLAY check & removed xsattbl	*
 * T. Piper/GSC		 3/01	removed xwcmn.h, in xwgui.h		*
 * R. Tian/SAIC		05/02	Added FaxCid				*
 * T. Piper/SAIC	07/03	removed initialization of *Cid		*
 * T. Piper/SAIC	07/04	Added xscint from xinitclr; this 	*
 *                              solves an initialization problem in the *
 *                              16/24 bit case and solves a double      *
 *                              initialization problem in GUI programs  *
 *                              when NOT sharing colors.                *
 * T. Piper/SAIC	02/05	Moved setting of allocflag into xcaloc	*
 * T. Piper/SAIC	04/05	Corrected where allocflag is initialized*
 ***********************************************************************/
{
    int		gemscreen, ii, jj, lp;
    char	cdev[13], *display_name=NULL;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

/*
 * Set the global output and color scheme types.
 */
    kctype = *itype;
    kunit  = *iunit;

/*
 * Set the current device.
 */
    strncpy ( cdev, dev, (size_t)*lend );
    cdev[*lend] = CHNULL;

/*
 * Set hardware text variables in case they are used.
 */
    txfont_req = 1;
    txfont_set = 0;
    txsize_req = 1.0F;
    txsize_set = 0.0F;

/*
 * Initialize foreground and background colors.
 *
 * Note that the background color index is 0 and not NNCOLR.
 * A pixel index of 0 is used for the X windows color
 * allocation routines.  The user will still access the
 * background color as color number 101.
 */
    ibkcol = 0;
    ifrcol = 1;

/*
 * Read the dwell rate table.
 */
    xdwtbl ( iret );
    dwell_rate = (int)(dwell[2]*1000.0F);  /* in milliseconds */

/*
 * If this is a GEMPAK window, continue.
 */
    if  ( *iunit == 1 ) {
	if  ( strcmp ( cdev, "XWP" ) == 0 ) {
	    /*
	     * If XWP device - Do nothing.
	     */
	    *iret = G_NEWWIN;
	}
	else {
	    /*
	     * Initialize the GEMPAK window structure.
	     */
	    current_window = 0; 

	    for ( ii = 0; ii < MAX_WINDOW; ii++ ) {

		gemwindow[ii].name[0]	= '\0';
		gemwindow[ii].npxms	= 0;
		gemwindow[ii].curr_loop	= 0;

		for (lp = 0; lp < MAX_LOOP; lp++) {
		    for (jj = 0; jj < MAX_PIXMAP; jj++) {
		        gemwindow[ii].pxms[lp][jj]     = (Pixmap)NULL;
			gemwindow[ii].xw_rfrsh[lp][jj] = FALSE;
		        gemwindow[ii].bad_frm[lp][jj]  = FALSE;
		    }
		}

		for ( jj = 0; jj < MAX_PIXMAP; jj++) {
		    gemwindow[ii].mstr[jj]        = (Pixmap)NULL;
		}
  
		for ( jj = 0; jj < MAX_LOOP; jj++) {
		    gemwindow[ii].loop[jj].roamflg = 0;
		    gemwindow[ii].curpxm[jj]	= 0;
	        }                 
	    }                 


	    /*
	     * Set color initialization flag
	     */
	    GColorIsInitialized = 0;

	    /*
	     * Set gemdisplay in xwcmn.h
	     */
	    gemdisplay = XOpenDisplay ( display_name );
	    if ( gemdisplay == NULL ) {
		*iret = G_NDISP;
		return;
	    }

	    /*
	     * Set default color map and create graphics contexts.
	     */
	    gemscreen = DefaultScreen ( (XtPointer)gemdisplay );
	    gemmap    = DefaultColormap ( (XtPointer)gemdisplay, gemscreen );
	    gemvis    = DefaultVisual ( (XtPointer)gemdisplay, gemscreen );
	    root      = DefaultRootWindow ( (XtPointer)gemdisplay );

	    /*
	     * Initialize allocflag for GEMPAK applications
	     */
	    for ( ii = 0; ii < 4; ii++ ) {
		allocflag[ii] = 0;
	    }

	    /*
	     * Allocate the graphic colors for GEMPAK applications
	     */
	    xcaloc ( GraphCid, iret );
	    if ( *iret != G_NORMAL ) {
		return;
	    }
	    xscint ( iret );

	}  /* end of XW device case */

    }  /* end of GEMPAK window case */ 
    else {
	/*
	 * GUI window is set through xmotifw by GUI
	 */
	*iret = G_NEWWIN;
    }

    /*
     *  Initialize the incr_pxmCnt
     */
    for ( ii = 0; ii < MAX_WINDOW; ii++ ) {
	gemwindow[ii].incr_pxmCnt = TRUE;
    }


    if  ( _setpxm )  {

	for (ii = 0; ii < MAX_LOOP; ii++) {
	    _numPxm[ii]	    = MAX_PIXMAP -1;
	    _fstPxm[ii]	    = 0;
	    _lstPxm[ii]	    = MAX_PIXMAP - 2;
	    _blankPxm[ii]   = MAX_PIXMAP - 1;
	    _loopSet[ii]    = FALSE;
	    _allFrmsBad[ii] = FALSE;
	}

	_setpxm = G_FALSE;
    }

    xselwin ( filnam, lenf, *xsize, *ysize, ixsize, iysize, iret );
    *ncurwn = current_window;
    *isxsiz = gemwindow[current_window].width;
    *isysiz = gemwindow[current_window].height;

/*
 * Set txszx, txszy (hardware text sizes) here if hardware
 * text is used.
 * txszx = bscalc * 7
 * txszy = bscalc * 9
 */
    txszx =  9.1F;
    txszy = 11.7F;
}

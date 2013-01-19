#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"


#define ICON_WIDTH  32  /* action icon width, hight */
#define ICON_HEIGHT 32  

#define ICON_FGNAME       "white"
#define ICON_BGNAME       "blue"
#define ICON_WORK_BGNAME  "red"

#define ZOOM_NONE	0
#define ZOOM_IN		1

#define ICON_DIR    "$NAWIPS/icons/nmap" 

#define BUTTON_TYPE     xmPushButtonWidgetClass


Cardinal        _zoomBtnW, _zoomBtnH;   /* zoom button width, height*/
Pixmap          _zoomBtnBxm;	        /* zoom button pixmap */

char            _zoomGarea[MAX_LOOP][73];  /* save the original GAREA */

int             _lastAct[MAX_LOOP];  	/* ZOOM_IN   - last acton is zoom
		  		         * ZOOM_NONE - last action is unzoom 
					 *			or none
		  		         */

Boolean	        _zoomActv[MAX_LOOP];

Widget _zoombtn; 
struct pxmBuf   _zmPxm[2];               /* pixmap label buffer      */


/*
 *  Private Callback functions
 */
void zoomw_unzoomCb ( Widget, XtPointer, XtPointer );
void zoomw_zoomCb (   Widget, XtPointer, XtPointer );


/************************************************************************
 * nmap_zoomw.c                                                         *
 *                                                                      *
 * This module creates the zoom/unzoom buttons widgets and defines      *
 * the callback functions for nmap.					*
 *                                                                      *
 * CONTENTS:                                                            *
 *      zoomw_create()        create the zoom/unzoom buttons.         	*
 *      zoomw_loadData()      load data under zoom.   			*
 *	zoomw_clearZoom()     clear any saved zoom settings		*
 *	zoomw_zoomFlagSave()  save main window zoom flag		*
 *	zoomw_setZoom()       set a zoom area for a loop   		*
 *									*
 *	zoomw_isZoomActv()    return TRUE if zoom has been activated	*
 *									*
 *      zoomw_zoomCb()        callback function for the zoom button. 	*
 *      zoomw_unzoomCb()      callback function for the unzoom button.  *
 *									*
 ***********************************************************************/

/*=====================================================================*/

Widget zoomw_create ( Widget parent )
/************************************************************************
 * zoomw_create                                              		*
 *                                                                      *
 * This function creates the icon push_buttons for zoom/unzoom.    	*
 *                                                                      *
 * Widget zoomw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 *  zoomw_create	Widget     Widget ID of the RowColumn container	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * I. Durham/GSC   	05/98	Changed underscore decl. to an include	*
 * E. Safford/GSC	08/00	correct _zoomActv initilization		*
 * R. Tian/SAIC         01/03   add True flag to NxmBxmBtn_create(Multi)*
 * T. Piper/SAIC	05/03	removed xwcmn.h and added XtDisplay()	*
 * M. Li/SAIC		12/07	Add background/foregraound to zoom btns	*
 ***********************************************************************/
{
int    ii, iret, lp;
long   ignore;

char   iconf[256], iconfile[256], insnsfile[256];
char   *btnstr[]= {"zoom", "unzoom"};
static char text_label[2][30];
static char _first=1;

Widget rc;
XtCallbackProc callback;
Drawable screen;

struct bxmInfo  zmbxm[2];
/*---------------------------------------------------------------------*/


    rc = XtVaCreateWidget( "zoomw_zoomRc", xmRowColumnWidgetClass, parent,
			XmNorientation,         XmHORIZONTAL,
                        NULL);

    cfl_inqr("insns.xbm", ICON_DIR, &ignore, insnsfile, &iret);

    for ( ii = 0; ii < (int)XtNumber(btnstr); ii++ ) {

        sprintf(iconf, "%s.xbm", btnstr[ii]);
        cfl_inqr(iconf, ICON_DIR, &ignore, iconfile, &iret);
	strcpy( text_label[ii], btnstr[ii] );

        if ( ii == 0 ) {

	    if ( _first ) {
	       /*
 	        *  Save zoom button pixmap
	        */
		screen = RootWindowOfScreen(XtScreen(parent));
        	XReadBitmapFile(XtDisplay(parent), screen, iconfile,
                        &_zoomBtnW, &_zoomBtnH, &_zoomBtnBxm, NULL, NULL);
		_first = 0;
	    }

	    callback = zoomw_zoomCb;

	    strcpy( zmbxm[0].fgcolor, ICON_FGNAME ); 
            strcpy( zmbxm[0].bgcolor, ICON_BGNAME );
	    zmbxm[0].insens_bits = insnsfile;
            zmbxm[1].insens_bits = insnsfile;

	    zmbxm[0].sens_bits   = iconfile;
            zmbxm[1].sens_bits   = iconfile;

            strcpy( zmbxm[1].fgcolor, ICON_FGNAME);
            strcpy( zmbxm[1].bgcolor, ICON_WORK_BGNAME );


            _zoombtn = (Widget) NxmBxmBtn_createMulti( rc, btnstr[ii], BUTTON_TYPE, 32, 32, zmbxm,
                     2, text_label[ii], True, callback, NULL, _zmPxm);


	}
	else {
	    callback = zoomw_unzoomCb;

	    NxmBxmBtn_create( rc, btnstr[ii], NULL, 32, 32,
                        ICON_FGNAME , ICON_BGNAME, insnsfile,
                        iconfile, text_label[ii], True, callback, NULL );
	}	    
    }

    XtManageChild(rc);


    /*
     *  Initialize the zoom flag
     */
    for (lp=0; lp < MAX_LOOP; lp++) {
       	_zoomActv[lp] = FALSE;
	_lastAct[lp]  = ZOOM_NONE;
    }
    return(rc);
}

/*=====================================================================*/

void zoomw_loadData ( void )
/************************************************************************
 * zoomw_loadData                                              		*
 *                                                                      *
 * This function loads data under zoom.    				*
 *                                                                      *
 * void zoomw_loadData()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96  						*
 * C. Lin/EAI      02/97  add fadeflg parm in calling dataw_loadFrame   *
 * S. Jacobs/NCEP  06/97  Added write flag of TRUE to cvg_load		*
 * C. Lin/EAI      07/97  Added color of 0 to cvg_load			*
 * C. Lin/EAI      07/97  call new mapw module				*
 * E. Wehner/EAi   08/97  Remove display of centroids through cpg	*
 * E. Wehner/EAi   09/97  Remove graphics info record			*
 * D.W.Plummer/NCEP 9/97  Changed &icolor to icolor in call to cvg_load	*
 * C.Lin/EAI       10/97  Changed product generation related functions	*
 * C.Lin/EAI       10/97  Add call to mcanvw_disarmDynamic()		*
 * C.Lin/EAI       01/98  remove extra cvg_load				*
 * E. Safford/GSC       10/99  mod for multiple loops of nmap2		*
 * E. Safford/GSC	10/98	dataw_getCurLoop -> loop_getCurLoop	*
 * E. Safford/GSC	11/98	use dsp_reloadLoop for data load 	*
 * E. Safford/GSC	12/98	param change to dsp_reloadLoop   	*
 * E. Safford/GSC	01/00	xpgsvfrm -> xpgsvfrm2			*
 * E. Safford/GSC	01/00	fix vgf refresh problem			*
 * E. Safford/GSC	04/00	fix no-load problem w/ mapw 		*
 * E. Safford/GSC	05/00	update for frame tags         		*
 * E. Safford/GSC	05/00	fix zoom bug in map window		*
 * S. Jacobs/NCEP	 5/00	Removed check for blank map frame zoom	*
 * E. Safford/GSC	07/00	add dsp_setBusy()          		*
 * S. Jacobs/NCEP	 7/00	Moved calls to dsp_setBusy		*
 * M. Li/GSC		03/01	removed mapw_setMap			*
 * J. Wu/SAIC		01/02	correct mis-defined variable type	*
 ***********************************************************************/
{
char    wname[73];
int     iunit, iwin, ityp, lp, ier;
float   size[2];
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    /*
 *  Check the current window
     */
    gqdatt(&iunit, wname, &ityp, &size[0], &size[1], &iwin, &ier, 72);

    if ( ier != 0 ) {
	NxmErr_update();
    }

    if ( strncmp(wname,MCANVW_NAME,strlen(MCANVW_NAME)) == 0 ) { 

/*  Main window */

       dsp_setBusy (TRUE);

        /*
 *  Save bad frame tag settings
         */
	xmfrmtg_saveFrmTag ( lp, &ier );

	/*
 *  Load the files selected for the loop
	 */
	dsp_reloadLoop(lp, &ier);

        /*
 *  Restore bad frame tag settings
         */
	xmfrmtg_restoreFrmTag ( lp, &ier );

        dsp_updtDisplay();
        dsp_setBusy (FALSE);

    }
       else if ( strncmp(wname,MAPW_NAME,strlen(MAPW_NAME)) == 0 )  {

        /*
	 *  Mark this loop as changed
	 */
	loop_setDataChngd (lp, TRUE);

        mapw_redrawMap();
	geplot(&ier);
    }
}

/*=====================================================================*/

void zoomw_clearZoom ( int lp )
/************************************************************************
 * zoomw_clearZoom                                                      *
 *                                                                      *
 * This subroutine clears the _zoomActv flag for the specified loop.    *
 * This should be called if data has been loaded and it superceeds a	*
 * zoom setting.							*
 *                                                                      *
 * void zoomw_clearZoom ( lp )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int		loop to check for zoom			*
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	02/00	initial coding         			*
 ***********************************************************************/
{
    _zoomActv[lp] = ZOOM_NONE;
    _lastAct[lp]  = ZOOM_NONE;
}

/*=====================================================================*/

void zoomw_setZoom ( int lp )
/************************************************************************
 * zoomw_setZoom 	                                                *
 *                                                                      *
 * This function sets the zoom area for a loop.                         *
 *                                                                      *
 * void zoomw_setZoom(lp)		               		        *
 *                                                                      *
 * Input parameters:                                                    *
 *  	lp	int	loop number					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/00	initial coding                      	*
 * H. Zeng/EAI          05/02   removed "garea"                         *
 ***********************************************************************/
{
    _zoomActv[lp] = TRUE;
    _lastAct[lp] = ZOOM_IN ;
}

/*=====================================================================*/

Boolean	zoomw_isZoomActv ( int lp )
/************************************************************************
 * zoomw_isZoomActv                                                     *
 *                                                                      *
 * This subroutine returns the current status of zoom.  True is         *
 * returned if the user has invoked zoom, False if no zoom.		*
 *                                                                      *
 * Boolean zoomw_isZoomActv ( lp )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int		loop to check for zoom			*
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 * zoomw_isZoomActv	Boolean	True if an active zoom, False if not	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding         			*
 ***********************************************************************/
{
    return ( _zoomActv[lp] );
}

/*=====================================================================*/
/* ARGSUSED */
void zoomw_zoomCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * zoomw_zoomCb                                                         *
 *                                                                      *
 * Callback function for zoom button.        				*
 *                                                                      *
 * void zoomw_zoomCb(w, clnt, call)                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget     widget ID                                    *
 *  clnt        XtPointer  not used  					*
 *  call        XtPointer  callback struct                              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      05/96                                                *
 * S. Wang/GSC	   11/96  add _lastAct flag and save original GAREA     *
 * C. Lin/EAI	   07/97  call new mapw module     			*
 * C. Lin/EAI	   08/97  change 'D' to 'S'     			*
 * S. Jacobs/NCEP   6/98  Removed np from call to GGTPNT		*
 * E. Safford/GSC	06/98	disable mouse setups before zooming	*
 * S. Law/GSC		09/99	changed cursor functions		*
 * E. Safford/GSC	10/98	dataw_getCurLoop -> loop_getCurLoop	*
 * S. Law/GSC		12/99	added check for seek window		*
 * S. Law/GSC		05/00	changed to use seekw_saveGhost		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * T. Lee/GSC		01/01	changed ityp to 13			*
 * T. Lee/GSC		01/01	added zoom option			*
 * M. Li/GSC		03/01   added nmp_szoom				*
 * H. Zeng/EAI          07/01   added check by using pgpalw_isUp()      *
 * S. Jacobs/NCEP	 6/02	Fixed variable type for bw and bh	*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 * H. Zeng/XTRIA	12/03   re-position pgpalw_rfrshObjPal()	*
 * E. Safford/SAIC	02/04	add mcanvw_getDpth			*
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 * M. Li/SAIC		12/07	Added center zoom type			*
 ***********************************************************************/
{
    int		ii, ityp, np, ier, iret, lp;
    float	xpts[2], ypts[2], xdev[2], ydev[2];
    char	garea[73];
    XGCValues	gc_values[2];
    char	bg_name[2][40];
    Drawable	screen;
    Dimension	bw, bh;

    static int		_zoomxy[2];
    static Boolean	zflag = FALSE; /* flag for the zoom toggle button */
    static Boolean	org_flg = FALSE;
    static Boolean	zoomtyp;
    static Pixmap	_zoompxm[2];
    static GC		_zoomgc[2];

/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    /*
 *  Disable the mouse button setups
     */
    mcanvw_disarmDynamic();

    if ( pgpalw_isUp() ) {
         pgpalw_classPopdown();
         pgpalw_setupOper(); 
         pgpalw_rfrshObjPal();
    }

    _zoomActv[lp] = TRUE;

    /*
     * Toggle the Zoom Button
     */
    if (zflag) {  /* zoom off */

	if (mpcstw_isUp()) {
	    mpcstw_popdown();
	}
        zflag = FALSE;

        /*
         * Restore the button color
         */
        XCopyArea(XtDisplay(w), _zoompxm[0], XtWindow(w), 
			_zoomgc[0], 0, 0, _zoomBtnW, _zoomBtnH, 
			_zoomxy[0], _zoomxy[1]);
       	XFlush(XtDisplay(w));

        /*
         * Restore the cursor
         */
	mcanvw_setCursor(CURS_DEFAULT);

        /*
         * Restore mouse to be selection status
         */
        XSelectInput( XtDisplay(w), XtWindow(mcanvw_getDrawingW()),
                        ButtonPressMask | ButtonReleaseMask |
                        ExposureMask | PointerMotionMask | KeyPressMask |
			KeyReleaseMask | KeymapStateMask |
			EnterWindowMask | LeaveWindowMask);

	XmProcessTraversal(mcanvw_getDrawingW(), XmTRAVERSE_CURRENT);
	_lastAct[lp] = ZOOM_IN ;

    }
    else { /* zoom on */
        zflag = TRUE;

        /*
 	 *  Get the original zoom button color and
         * the color in zoom status when needed.
         */
        if (!org_flg) {

	    strcpy(bg_name[0], ICON_BGNAME);
	    strcpy(bg_name[1], "red");

	    for ( ii = 0; ii < 2; ii++ ) {
		xsncolr(bg_name[ii], &gc_values[ii].background, &ier);
        	xsncolr(ICON_FGNAME, &gc_values[ii].foreground, &ier);
			    
        	_zoomgc[ii] = XtGetGC( w, GCBackground | GCForeground,
                        		&gc_values[ii] );
		screen = RootWindowOfScreen(XtScreen(w));
        	_zoompxm[ii] = XCreatePixmap(XtDisplay(w),
                       screen, _zoomBtnW, _zoomBtnH, (Cardinal)mcanvw_getDpth());
        	XCopyPlane(XtDisplay(w), _zoomBtnBxm, 
				     _zoompxm[ii], _zoomgc[ii], 0, 0, 
				     _zoomBtnW, _zoomBtnH, 0, 0, 1);
		XtVaGetValues(w, XmNwidth,  &bw,
					     XmNheight, &bh, NULL);
		_zoomxy[0] = (int)((float)((Cardinal)bw - _zoomBtnW)/2.0F);
		_zoomxy[1] = (int)((float)((Cardinal)bh - _zoomBtnH)/2.0F);
	    }

            org_flg = TRUE;
        }

        /*
 	 *  Change to the zoom cursor
         */
	mcanvw_setCursor (CURS_POINT_SELECT);

        /*
 	 *  Change zoom button color
         */
        XCopyArea(XtDisplay(w), _zoompxm[1], XtWindow(w), 
				_zoomgc[1], 0, 0, _zoomBtnW, 
				_zoomBtnH, _zoomxy[0], _zoomxy[1]);
        XFlush(XtDisplay(w));

        /*                 
 	 *  Get two corner points
         */
	mmenuw_extendedZoomSet ( &zoomtyp );
	if ( zoomtyp ) {
	    ityp = 13;
	}
	else {
	    ityp = 3;
	}
	if ( !mmenuw_zoomModeGet() ) {
	    ityp = zoomtyp ? 14 : 4;
	}
        np = 2;
        for ( ii = 0; ii < np; ii++) {
            xpts[ii] = ypts[ii] = 0.0F;
	}

        ggtpnt( sys_S, &ityp, xdev, ydev, &iret, strlen(sys_S)); 
	if ( iret != 0 )  {
	    NxmErr_update();
	}

	seekw_saveGhost (TRUE);

        /*
         * check if the box is big enough
         */
        if ( fabs((double)(xdev[0] - xdev[1])) > 20. &&
                        fabs((double)(ydev[0] - ydev[1])) > 20. ) {

            gtrans(sys_S, sys_M, &np, xdev, ydev, xpts, ypts,
                                &iret, strlen(sys_S), strlen(sys_M));

            if ( iret == 0 ) {
	        NxmErr_update();
                sprintf(garea, "%.2f;%.2f;%.2f;%.2f",   
				xpts[0], ypts[0], xpts[1], ypts[1]);
		nmp_szoom(lp, garea, &iret);
            }
        }

	zoomw_loadData();

        /*
         * Reset the zoom button
         */
	zoomw_zoomCb( w, NULL, NULL);

	aodtw_refresh(TRUE);
	cldhgtw_refresh(TRUE);
	seekw_saveGhost (FALSE);
    }

}


/*=====================================================================*/
/* ARGSUSED */
void zoomw_czoom ( Widget wdgt, XEvent *evt, String *parms, Cardinal *num )
/************************************************************************
 * zoomw_czoom                                                          *
 *                                                                      *
 * Center zoom hot key function.             				*
 *                                                                      *
 * void zoomw_czoom( wdgt, evt, parms, num )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt       Widget    widget that caused action to be called        *
 *  *evt        XEvent    event that caused action to be called         *
 *  *parms     	String    pointer to list of strings specified as action*
 *                            arguments (not used)                      *
 *  *num  	Cardinal  number of strings (not used)                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC          	12/07	Created                                 *
 ***********************************************************************/
{
    int		ii, ityp, np, iret, lp;
    float	xpts[2], ypts[2], xdev[2], ydev[2];
    char	garea[73];

    static Boolean	zflag = FALSE; 
    static Boolean	zoomtyp;

/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    /*
     * disable the mouse button setups
     */
    mcanvw_disarmDynamic();

    if ( pgpalw_isUp() ) {
         pgpalw_classPopdown();
         pgpalw_setupOper(); 
         pgpalw_rfrshObjPal();
    }

    _zoomActv[lp] = TRUE;

    /*
     * Zoom action 
     */
    if (zflag) {  /* zoom off */

	if (mpcstw_isUp()) {
	    mpcstw_popdown();
	}

        zflag = FALSE;
        XFlush(XtDisplay(wdgt));

        /*
         * Restore the cursor
         */
	mcanvw_setCursor(CURS_DEFAULT);

        /*
         * Restore mouse to be selection status
         */
        XSelectInput( XtDisplay(wdgt), XtWindow(mcanvw_getDrawingW()),
                        ButtonPressMask | ButtonReleaseMask |
                        ExposureMask | PointerMotionMask | KeyPressMask |
			KeyReleaseMask | KeymapStateMask |
			EnterWindowMask | LeaveWindowMask);

	XmProcessTraversal(mcanvw_getDrawingW(), XmTRAVERSE_CURRENT);
	_lastAct[lp] = ZOOM_IN ;

    }
    else { /* zoom on */

        zflag = TRUE;

        /*
         * change to the zoom cursor
         */
	mcanvw_setCursor (CURS_POINT_SELECT);

	/*
	 * Make the zoom and unzoom buttons busy
	 */
	mbtnw_zoomSensitive(FALSE);
        XFlush(XtDisplay(wdgt));

        /*                 
	 * get two corner points
         */
	mmenuw_extendedZoomSet ( &zoomtyp );
	if ( zoomtyp ) {
	    ityp = 14;
	}
	else {
	    ityp = 4;
	}
        np = 2;
        for ( ii = 0; ii < np; ii++) {
            xpts[ii] = ypts[ii] = 0.0F;
	}

        ggtpnt( sys_S, &ityp, xdev, ydev, &iret, strlen(sys_S)); 

	if ( iret != 0 )  {
	    NxmErr_update();
	}

	seekw_saveGhost (TRUE);

        /*
	 *  Check if the box is big enough
         */
        if ( fabs((double)(xdev[0] - xdev[1])) > 20. &&
                        fabs((double)(ydev[0] - ydev[1])) > 20. ) {

            gtrans(sys_S, sys_M, &np, xdev, ydev, xpts, ypts,
                                &iret, strlen(sys_S), strlen(sys_M));

            if ( iret == 0 ) {
	        NxmErr_update();
                sprintf(garea, "%.2f;%.2f;%.2f;%.2f",   
				xpts[0], ypts[0], xpts[1], ypts[1]);
		nmp_szoom(lp, garea, &iret);
            }
        }

	zoomw_loadData();

        /*
         * Reset the zoom button
         */
	zoomw_czoom( wdgt, NULL, NULL, NULL );

	aodtw_refresh(TRUE);
	cldhgtw_refresh(TRUE);
	seekw_saveGhost (FALSE);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void zoomw_zzoom ( Widget wdgt, XEvent *evt, String *parms, Cardinal *num )
/************************************************************************
 * zoomw_zzoom                                                          *
 *                                                                      *
 * Corner zoom hot key function.             				*
 *                                                                      *
 * void zoomw_zzoom( wdgt, evt, parms, num )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt       Widget    widget that caused action to be called        *
 *  *evt        XEvent    event that caused action to be called         *
 *  *parms     	String    pointer to list of strings specified as action*
 *                            arguments (not used)                      *
 *  *num  	Cardinal  number of strings (not used)                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC          	12/07	Created                                 *
 ***********************************************************************/
{
    int		ii, ityp, np, iret, lp;
    float	xpts[2], ypts[2], xdev[2], ydev[2];
    char	garea[73];

    static Boolean	zflag = FALSE; 
    static Boolean	zoomtyp;

/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    /*
     * disable the mouse button setups
     */
    mcanvw_disarmDynamic();

    if ( pgpalw_isUp() ) {
         pgpalw_classPopdown();
         pgpalw_setupOper(); 
         pgpalw_rfrshObjPal();
    }

    _zoomActv[lp] = TRUE;

    /*
     * Zoom action 
     */
    if (zflag) {  /* zoom off */

	if (mpcstw_isUp()) {
	    mpcstw_popdown();
	}

        zflag = FALSE;

        /*
         * Restore the cursor
         */
	mcanvw_setCursor(CURS_DEFAULT);

        /*
         * Restore mouse to be selection status
         */
        XSelectInput( XtDisplay(wdgt), XtWindow(mcanvw_getDrawingW()),
                        ButtonPressMask | ButtonReleaseMask |
                        ExposureMask | PointerMotionMask | KeyPressMask |
			KeyReleaseMask | KeymapStateMask |
			EnterWindowMask | LeaveWindowMask);

	XmProcessTraversal(mcanvw_getDrawingW(), XmTRAVERSE_CURRENT);
	_lastAct[lp] = ZOOM_IN ;

    }
    else { /* zoom on */

        zflag = TRUE;

        /*
         * change to the zoom cursor
         */
	mcanvw_setCursor (CURS_POINT_SELECT);

	/*
	 * Make the zoom and unzoom buttons busy
	 */
	mbtnw_zoomSensitive(FALSE);

        /*                 
	 * get two corner points
         */
	mmenuw_extendedZoomSet ( &zoomtyp );
	if ( zoomtyp ) {
	    ityp = 13;
	}
	else {
	    ityp = 3;
	}
        np = 2;
        for ( ii = 0; ii < np; ii++) {
            xpts[ii] = ypts[ii] = 0.0F;
	}

        ggtpnt( sys_S, &ityp, xdev, ydev, &iret, strlen(sys_S)); 

	if ( iret != 0 )  {
	    NxmErr_update();
	}

	seekw_saveGhost (TRUE);

        /*
	 *  Check if the box is big enough
         */
        if ( fabs((double)(xdev[0] - xdev[1])) > 20. &&
                        fabs((double)(ydev[0] - ydev[1])) > 20. ) {

            gtrans(sys_S, sys_M, &np, xdev, ydev, xpts, ypts,
                                &iret, strlen(sys_S), strlen(sys_M));

            if ( iret == 0 ) {
	        NxmErr_update();
                sprintf(garea, "%.2f;%.2f;%.2f;%.2f",   
				xpts[0], ypts[0], xpts[1], ypts[1]);
		nmp_szoom(lp, garea, &iret);
            }
        }

	zoomw_loadData();

        /*
         * Reset the zoom button
         */
	zoomw_zzoom( wdgt, NULL, NULL, NULL );

	aodtw_refresh(TRUE);
	cldhgtw_refresh(TRUE);
	seekw_saveGhost (FALSE);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void zoomw_unzoomCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * zoomw_unzoomCb                                                       *
 *                                                                      *
 * Callback function for unzoom button.                    		*
 *                                                                      *
 * void zoomw_unzoomCb(w, clnt, call)                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *  w       Widget     widget ID                                        *
 *  clnt    XtPointer  not used                                         *
 *  call    XtPointer  not used                                         *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI        	 5/96                                           *
 * S. Wang/GSC	   	11/96	_lastAct flag checks			*
 * C. Lin/EAI        	 7/97   call new mapw module                    *
 * E. Safford/GSC  	06/98	disable mouse button setups		*
 * E. Safford/GSC  	10/99	mod for multiple loops of nmap2		*
 * E. Safford/GSC	10/98	dataw_getCurLoop -> loop_getCurLoop	*
 * S. Law/GSC		12/99	added check for seek window		*
 * S. Law/GSC		05/00	changed to use seekw_saveGhost		*
 * E. Safford/GSC	09/00	move _saveGhost after first exit check 	*
 * M. Li/GSC		03/01	added nmp_szoom				*
 * H. Zeng/EAI          07/01   added check by using pgpalw_isUp()      *
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 ***********************************************************************/
{
    int		lp, iret;
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    if ( _lastAct[lp] == ZOOM_NONE ) {
        return;
    }

    seekw_saveGhost (TRUE);

/*
 *  Disable the mouse button setups
 */
    mcanvw_disarmDynamic();

    if ( pgpalw_isUp() ) {
       pgpalw_classPopdown();
       pgpalw_rfrshObjPal();
       pgpalw_setupOper();
    }  

    _zoomActv[lp] = FALSE;

/*
 *  Reset the area to see whole image
 */
    nmp_szoom(lp, "", &iret);

    zoomw_loadData();

    _lastAct[lp] = ZOOM_NONE;

    aodtw_refresh(TRUE);
    cldhgtw_refresh(TRUE);
    seekw_saveGhost (FALSE);
}

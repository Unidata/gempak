#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "xwcmn.h"


#define	CANVAS_HGT	851
#define CANVAS_WIDTH	1150
#define	MAX_ITEM	10


static	Widget  _canvasW;	      /* drawing canvas widget     */
static	Widget	_menuBar;             /* menubar widget		   */	


typedef	struct {
    char   btn_name[10];	 /* item name, maximum 7 letters   */
    void   (*create_func)();	 /* item creating function	   */
    void   (*callback_func)(Widget, XtPointer, XtPointer);	 /* item callback function	   */
}  user_item_t;			 /* user item data structure       */


typedef	struct {
    int	 num;			/* total number of items	*/
    user_item_t  useritem[MAX_ITEM];	/* item array		*/
}  user_info_t;			/* user information sturcture 	*/

static	user_info_t  _userInfo;   /* user information variable	*/

void guitst_createMain ( Widget );
void guitst_createCanvas ( Widget );
void guitst_createUserItems ( Widget );
void guitst_rgstrCanvas ( void );
int guitst_gmpkInit ( Widget );
void guitst_exposeCb ( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void guitst_resizeCb ( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void guitst_exitCb ( Widget, XtPointer, XtPointer );

/* utility functions for user	*/
void guitst_AddUserItem ( char btn_name[20], void (*create_func)(),
						void (*callback_func)(Widget, XtPointer, XtPointer) );
void guitst_Display ( void );
Widget guitst_GetCanvasId ( void );



/************************************************************************
 * guitst.c                                                             *
 *                                                                      *
 * This module contains functions that creates the guitst program and	*
 * some utility functions that programmers who use guitst may need to	*
 * call.								*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *	guitst_createMain() 	creates main window			*
 *	guitst_createCanvas()	creates the main drawing canvas		*
 *	guitst_createUserItems()  add user info				*
 *	guitst_rgstrCanvas()	regester canvas as a GEMPAK window	*
 *	guitst_gmpkInit()	gempak initialization function		*
 *	guitst_exposeCb()	canvas expose callback function		*
 *	guitst_resizeCb()	resize callback function		*
 *	guitst_exitCb()		exit callback function			*
 *                                                                      *
 *	guitst_GetCanvasId()	obtain canvas widget id			*
 *	guitst_Display()	display current pixmap 			*
 *	guitst_AddUserItem()	add a testing item			*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of guitst.                                              *
 *                                                                      *
 * void main()                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	  10/97							*
 * I. Durham/GSC  05/98		Changed underscore call			*
 * S. Jacobs/NCEP  9/02		Fixed compiler warnings			*
 ***********************************************************************/
{

Widget  	toplevel;
XtAppContext	app;

/*--------------------------------------------------------------------*/

        toplevel = XtVaAppInitialize( &app, "Guitst", 
                        NULL, 0, &argc, argv, NULL, NULL );

	if ( guitst_gmpkInit(toplevel)!= 0 ) {
	    printf(" guitst_gmpkInit fails, exit TESTER \n");
	    exit(1);
	}

	guitst_createMain(toplevel);

	guitst_createUserItems(toplevel);

	XtRealizeWidget(toplevel);

	guitst_rgstrCanvas();

	XtAppMainLoop(app);

	return(0);

}

/*=====================================================================*/

void guitst_createMain ( Widget parent )
/************************************************************************
 * guitst_createMain   	                                                *
 *                                                                      *
 * This function creates the main window				*
 *                                                                      *
 * void	guitst_createMain( parent )					*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      parent    Widget    parent widget id                        	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
Widget	main_w, button;
Pixel	bg;
long	ii;

/*---------------------------------------------------------------------*/

	main_w = XtVaCreateWidget("mainw_pane",
                        xmPanedWindowWidgetClass, parent,
                        XmNsashWidth,             1,
                        XmNsashHeight,            1,
                        NULL, 0);

	_menuBar = XmCreateMenuBar(main_w, "menubar", NULL, 0);
        XtVaGetValues(parent, XmNbackground, &bg, NULL);
        XtVaSetValues(_menuBar, XmNbackground, bg, NULL);

	/*
	 * create Exit button
	 */

        button = XmCreateCascadeButton( _menuBar, 
					"Exit", NULL, 0 );

        XtVaSetValues( button, XmNbackground, bg, NULL );
        XtManageChild( button );

        XtAddCallback( button, XmNactivateCallback,
         	        guitst_exitCb, (XtPointer)ii );

	XtManageChild( _menuBar );

	guitst_createCanvas( main_w );

	XtManageChild( main_w );

}

/*=====================================================================*/

void guitst_createCanvas ( Widget parent )
/************************************************************************
 * guitst_createCanvas                                                  *
 *                                                                      *
 * This function create the drawing canvas of the guitst program	*
 *                                                                      *
 * void	guitst_createCanvas(parent)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget   parent widget ID                       *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
Widget	frame;
/*---------------------------------------------------------------------*/

	frame = XtVaCreateManagedWidget( "frame",
			xmFrameWidgetClass, parent,
			XmNshadowType,	    XmSHADOW_IN,
			NULL );
	
	_canvasW = XtVaCreateManagedWidget("guitst_canvas",
                        xmDrawingAreaWidgetClass, frame,
			XmNwidth,     		(CANVAS_WIDTH), 
                        XmNheight,    		(CANVAS_HGT), 
                        XmNresizable,           TRUE,
			NULL);

	XtAddCallback(_canvasW, XmNexposeCallback, 
			(XtCallbackProc)guitst_exposeCb, NULL);
        XtAddCallback(_canvasW, XmNresizeCallback, 
			(XtCallbackProc)guitst_resizeCb, NULL);

}

/*======================================================================*/

void guitst_createUserItems ( Widget parent )
/************************************************************************
 * guitst_createUserItems						*
 *                                                                      *
 * This function first calls user function named user_func() which 	*
 * calls guitst_AddUserItem() to save user items in buffer and then 	*
 * adds them to the program						*
 *                                                                      *
 * void	guitst_createUserItems(parent)					*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      parent    Widget    parent widget id                        	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
int	i, num;
Pixel	bg;
Widget	button;
user_item_t	*item_pt;
/*---------------------------------------------------------------------*/

	/*
	 * user_func() is a user function that is manditory
	 * by guitst.c 
	 */

	/* user_func(); */

        XtVaGetValues(parent, XmNbackground, &bg, NULL);

	num = _userInfo.num;

	for ( i=0;i< num ;i++ ) {
	    item_pt = &(_userInfo.useritem[i]);

	    if ( item_pt->create_func != NULL )
		item_pt->create_func(parent);

       	    button = XmCreateCascadeButton( _menuBar, 
				item_pt->btn_name, NULL, 0 );
       	    XtVaSetValues( button, XmNbackground, bg, NULL );
       	    XtManageChild( button );

	    if ( item_pt->callback_func != NULL )
        	XtAddCallback( button, XmNactivateCallback,
         	        item_pt->callback_func, NULL );

	}
	
	return;	
}

/*======================================================================*/

void guitst_rgstrCanvas ( void )
/************************************************************************
 * guitst_rgstrCanvas   	                                        *
 *                                                                      *
 * This function registers the canvas as a gempak window		*
 *                                                                      *
 * void	guitst_rgstrCanvas()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 *   C. Lin/EAI      04/96                                              *
 *   S. Wang/GSC     10/97	modified for guitst                     *
 *   H. Zeng/EAI     04/00      changed cursor change function          *
 * T. Piper/SAIC	10/04	Moved gg_panl after gclear		*
 ***********************************************************************/
{

Window    gwin;
GC	  gemgc;
char      wname[20];
int       iret, xdpth;
Dimension width, height;

/*---------------------------------------------------------------------*/

        /*
         * set the cursor to the default
         */
        NxmCursor_setCursor(_canvasW, CURS_DEFAULT);

        strcpy(wname, "guitst");

        /*
         * get the window info
         */
        gwin = XtWindow(_canvasW);
        gemgc = XCreateGC(gemdisplay, gwin, 0, 0);
        xdpth = DefaultDepth((XtPointer)gemdisplay,
			     DefaultScreen((XtPointer)gemdisplay));

        XtVaGetValues(_canvasW, XmNwidth,  &width,
                         XmNheight, &height,
                         NULL );

        /*
         * call GEMPAK to register the window (drawing area)
         * from now on, this drawing area will be GEMPAK's
         * window identified by name = wname.
         */
        xmotifw( gwin, wname, gemgc, (int)width, (int)height,
              			  xdpth, &iret );

        /*
         * set DEVICE in gempak
         */
        gg_motf(wname, &iret, strlen(wname));

        /*
         * clear screen and set panel
         */

	gclear(&iret);
        gg_panl("0", &iret, 1);

	
}

/*=====================================================================*/

int guitst_gmpkInit ( Widget w )
/************************************************************************
 * guitst_gmpkInit                                                      *
 *                                                                      *
 * This function initializes GEMPAK variables.                          *
 *                                                                      *
 * int guitst_gmpkInit(w)                                               *
 *                                                                      *
 * Input parameters:                                                    *
 *  w   Widget      widget ID                                           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      G_NORMAL  successful                            *
 *                      -1        error occured                         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      02/93                                                *
 * S. Wang/GSC     10/97        modified for guitst                     *
 * S. Jacobs/NCEP   9/02	Updated to support 16 and 24 bit graphic*
 ***********************************************************************/
{
int  gemscreen, iret=0, mode, ignore, respond;
char msg[]="Fatal Error: no graphic colors. Exit guitst.\n";

/*---------------------------------------------------------------------*/

        /*
         * Get root, colormap, visual, depth
         */

        gemdisplay = XtDisplay(w);
        gemscreen  = DefaultScreen((XtPointer)gemdisplay);
        root       = DefaultRootWindow((XtPointer)gemdisplay);
        gemmap     = DefaultColormap((XtPointer)gemdisplay, gemscreen);
        gemvis     = DefaultVisual((XtPointer)gemdisplay, gemscreen);

        /*
         * Initialize the global color bank structure
         */
        xgbank(gemdisplay, &iret);
        if ( iret != 0 )  return(-1);

        /*
         * allocate read and write color cells for
         * graphics
         */
        xcaloc( GraphCid, &iret );
        if ( iret == G_NCLRAL ) {
                er_wmsg("gemplt", &iret, NULL, &ignore,
                                strlen("gemplt"), 0 );
                return(-1);
        }
        else {
                if ( iret == G_ZEROCB ) {
                  printf(msg);
                  return(-1);
                }
        }

	xscint ( &iret );

        /*
         * initialize GEMPAK common blocks other than graphics
         */
        ip_init( &respond, &iret );

        /*
         * initialize GEMPAK plotting package
         */
        mode = 1;
        gg_init( &mode, &iret );

        return(G_NORMAL);

}

/*=====================================================================*/

void guitst_exposeCb ( Widget wdgt, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call )
/************************************************************************
 * guitst_exposeCb    	                                                *
 *                                                                      *
 * This is the expose callback function for the canvas widget		*
 *                                                                      *
 * void	guitst_exposeCb(wdgt, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      wdgt    Widget          calling widget id                       *
 *      clnt	XtPointer   	resource	                        *
 *   	*call	XmDrawingAreaCallbackStruct	resource		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *	C. Lin/EAI     04/96                                            *
 *      S. Wang/GSC    10/97	     rename and used in guitst          *
 ***********************************************************************/
{
XEvent	*event;
/*---------------------------------------------------------------------*/

	event = call->event;

        if(event->xexpose.count == 0) 
	    xmexpo(event);

}

/*=====================================================================*/

void guitst_resizeCb ( Widget wdgt, XtPointer clnt, 
					XmDrawingAreaCallbackStruct *call )
/************************************************************************
 * guitst_resizeCb    	                                                *
 *                                                                      *
 * This is the resize callback function for the canvas widget		*
 *                                                                      *
 * void	guitst_resizeCb(wdgt, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      wdgt    Widget          calling widget id                       *
 *      clnt	XtPointer   	resource	                        *
 *   	*call	XmDrawingAreaCallbackStruct	resource		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
Dimension	width, height;
int		inx;

/*---------------------------------------------------------------------*/

        inx = gemwindow[0].curpxm[0];

	XtVaGetValues( _canvasW,
                        XmNwidth,  	&width,
                        XmNheight, 	&height,
                        NULL );

        XClearArea(gemdisplay, call->window, 0, 0, 0, 0, False);

	XCopyArea (gemdisplay, gemwindow[0].pxms[0][inx], 
			gemwindow[0].window, gemwindow[0].gc, 
			0, 0, width, height, 0, 0);

        XFlush(gemdisplay);
}

/*======================================================================*/

void guitst_exitCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * guitst_exitCb   	                                                *
 *                                                                      *
 * This is the callback function of the exit button			*
 *                                                                      *
 * void	guitst_exitCb(wdgt, clnt, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      wdgt    Widget          calling widget id                       *
 *      clnt	XtPointer   	resource	                        *
 *      call	XtPointer   	resource	                        *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
	exit(0);
}

/*======================================================================*/

void guitst_AddUserItem ( char btn_name[20], void (*create_func)(), 
		void (*callback_func)(Widget, XtPointer, XtPointer) )
/************************************************************************
 * guitst_AddUserItem  	                                                *
 *                                                                      *
 * This function adds an user item to the user information buffer. 	*
 *                                                                      *
 * void	guitst_AddUserItem(btn_name, create_func, callback_func )	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *      btn_name[20]		char	user item button name		*
 *      *create_func()		void	user item creating function	*
 *      *callback_func()	void	user item callback function	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
int	n;

/*----------------------------------------------------------------------*/

	n = _userInfo.num;

	if ( n > MAX_ITEM ) {
	    printf(" Maximum number of items reached. %s is dropped \n",
		btn_name );
	    return;
	}
	
	strncpy( _userInfo.useritem[n].btn_name, btn_name, 7 );
	_userInfo.useritem[n].btn_name[7] = '\0';

	if ( create_func != NULL )
	    _userInfo.useritem[n].create_func = create_func;
	if ( callback_func != NULL )
	    _userInfo.useritem[n].callback_func = callback_func;

	_userInfo.num++;
}

/*======================================================================*/

Widget guitst_GetCanvasId ( void )
/************************************************************************
 * guitst_GetCanvasId	                                                *
 *                                                                      *
 * This function get the widget id of the drawing canvas		*
 *                                                                      *
 * Widget guitst_GetCanvasId()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *	guitst_GetCanvasId   Widget	 widget id of _canvasW 		* 
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
    return( _canvasW );
}

/*=====================================================================*/

void guitst_Display ( void )
/************************************************************************
 * guitst_Display	                                                *
 *                                                                      *
 * This function display the current pixmap to the screen		*
 *                                                                      *
 * void	guitst_Display()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 *      S. Wang/GSC    10/97	                                        *
 ***********************************************************************/
{
Window  win;
int     inx;
/*---------------------------------------------------------------------*/

        inx = gemwindow[0].curpxm[0];
        win = XtWindow(_canvasW);

        XCopyArea ( gemdisplay, gemwindow[0].pxms[0][inx], win,
                gemwindow[0].gc, 0, 0, gemwindow[0].width,
                gemwindow[0].height, 0, 0 );

        XFlush(gemdisplay);
}

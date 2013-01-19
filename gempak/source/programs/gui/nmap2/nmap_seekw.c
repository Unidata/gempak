#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "pgprm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"


#define MAX_SEEK	 25
#define MAX_TEXT	 65
#define MAX_MENU	 10
#define MAX_STATE	 59	/* includes 'any', 'DC', and possesions */
#define MAX_REL_TEXT	 15

#define BUTTON_HEIGHT	 25
#define LIST_HEIGHT	(BUTTON_HEIGHT + 115)
#define LIST_WIDTH	400
#define ARROW_WIDTH	BUTTON_HEIGHT
#define TYPE_WIDTH	 75
#define LARGER_WIDTH	(TYPE_WIDTH + 25)
#define VERT_OFF	  0
#define HORZ_OFF	  9
#define LABEL_WIDTH	 115
#define LABEL_HEIGHT	 25

#define DIST_SM		  0	/* use statute  miles */
#define DIST_NM		  1	/* use nautical miles */
#define DIST_KM		  2	/* use kilometers */
#define DIR_16PT	  0	/* use 16 point compass */
#define DIR_DEG		  1	/* use degrees */

#define FORMAT_COUNTY	  0
#define FORMAT_DEFAULT	  1

#define MAX_POINTS	  2
#define PT_L		 -1	/* line connecting points */
#define PT_1		  0
#define PT_2		  1

#define ICON_DIR	"$NAWIPS/icons/nmap"
#define ICON_HILITE	"blue"
#define FRAC_MULT	100
#define HIT_SIZE	300

#define	REL_1TO2	  0
#define	REL_2TO1	  1

#define NSYMB_PTS	  1
#define NLINE_PTS	  2
#define SYMB_OFFSET	 10.0F

#define LOC_TBL  "locator.tbl"
#define IMPVAL   -9999.0F

struct ptInfoStrc {
    Widget	ptpb;		/* pushbutton to select this point */
    Widget	namew;		/* search name text widget */
    int		ctype0;		/* current search type (for arrays)*/
    int		ctype1;		/* current search type (for calls) */
    Widget	tpmenu;		/* search type menu */
    WidgetList	tppbs;		/* search type menu push buttons */
    int		cstate;		/* current state */
    Widget	stmenu;		/* states menu */
    Widget	stpbs[MAX_STATE];/* states menu push buttons */
    float	lat;
    float	lon;
    int		format;
};

static struct ptInfoStrc _ptInfo[MAX_POINTS];
static int	_currPt		= PT_1;
static int	_hideCount	= 0;	/* counter for seekw_saveGhost */

static float	_currLat	=  IMPVAL;
static float	_currLon	=  IMPVAL;

static Boolean	_isStation[]	= {FALSE, FALSE};
static Boolean	_useCurrLoc	= FALSE;
static Boolean	_havePt[]	= {FALSE, FALSE};
static Boolean  _skipRemove	= FALSE;
static Boolean  _seekIsUp	= FALSE;
static Boolean  _do_not_modify_currLat	= FALSE;

static int	_distType	= DIST_NM;
static char	*_distLabel[]	= {"sm", "nm", "km"};
static int	_dirType	= DIR_16PT;
static char	*_dirLabel[]	= {"16-pt", "DEG"};

static int	_currLimit 	= 1;	/* Current hits limit */
static char	*_stateLabel[]	= {"Any", "AK", "AL", "AR",  "AZ", 
			       "CA", "CO", "CT", "DC", "DE", "FL", 
			       "GA", "HI", "IA", "ID", "IL", "IN", 
			       "KS", "KY", "LA", "MA", "MD", "ME",
			       "MI", "MN", "MO", "MS", "MT", "NC", 
			       "ND", "NE", "NH", "NJ", "NM", "NV", 
			       "NY", "OH", "OK", "OR", "PA", "RI",
			       "SC", "SD", "TN", "TX", "UT", "VA", 
			       "VT", "WA", "WI", "WV", "WY",
			       "AS", "GU", "MP", "PR", "PW", "UM", "VI"};

static int	_relDirection	= REL_1TO2;

static int  *_fmCod;
static char **_locArea;
static char **_usrLst;
static int  _whichLoc;

static Pixel	_iconTShadow;
static Pixel	_iconBShadow;
static Pixel	_iconSelect;

static Widget	_mainWin;
static Widget	seek_pane;
static Widget	_seekForm;
static Widget	_seekLabel;
static Widget	_seekList;
static Widget	_limitLabel;
static Widget	_searchForm;
static Widget	_searchList;
static Widget	_seekwParent;
static Widget	_relForm;
static Widget	_relLabel1;
static Widget	_relLabel2;
static Widget	_relText;
static Widget   _distWid;
static Widget   _distLbl;


/*
 * Private callback functions
 */
void seekw_clickPbCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void seekw_ctlBtnCb ( Widget wid, long which, XtPointer cbs );
void seekw_dirCb ( Widget wid, long unit, XtPointer cbs );
void seekw_distCb ( Widget wid, long unit, XtPointer cbs );
void seekw_limitArrowCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void seekw_pointerEh ( Widget wid, long  which, XEvent *event, Boolean *ctdr );
void seekw_pointFormEh ( Widget wid, long which, XEvent *event, Boolean *ctdr );
void seekw_relDirCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void seekw_searchListCb ( Widget wid, XtPointer clnt, 
						XmListCallbackStruct *cbs );
void seekw_searchNameCb ( Widget wid, Boolean activate, XtPointer cbs );
void seekw_seekListCb ( Widget wid, XtPointer clnt, 
						XmListCallbackStruct *cbs );
void seekw_stateCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void seekw_typeCb ( Widget wid, XtPointer clnt, XtPointer cbs );

/*
 * private functions -- action
 */
void seekw_getVectStr ( float lat1, float lon1, float lat2, float lon2,
							char *vectstr );
void seekw_ghostPts ( Boolean make_new );
void seekw_manageSearch ( int which );
void seekw_searchName ( Boolean activate );
void seekw_setRelText ( void );
void seekw_setSeekSens ( void );
void seekw_setState (char state[] );
void seekw_updatePt ( Boolean state );
void seekw_warp ( void );
void seekw_saveCPF ( void );


/************************************************************************
 * nmap_seekw.c								*
 *									*
 * This module contains fuctions to search for geographic locations.	*
 *									*
 * CONTENTS:                                                            *
 * seekw_create()	creates the seek popup widget			*
 * seekw_popup()	manages the seek popup widget			*
 * seekw_popdown()	unmanages the seek popup widget			*
 * seekw_update()	updates the seek list				*
 * seekw_refresh()	refreshes the ghosts				*
 * seekw_saveGhost()	preserves the ghosts during map changes		*
 *									*
 * seekw_isUp()		query if the window is up 			*
 *									*
 * seekw_pointerEh()	event handler for input from the map widget	*
 * seekw_seekListCb()	callback for the seek list widget		*
 * seekw_typeCb()	callback for the type menu			*
 * seekw_limitArrowCb()	callback for the limit arrows			*
 * seekw_ctlBtnCb()	callback for the control buttons		*
 * seekw_distCb()	callback for the distance unit menu		*
 * seekw_dirCb()	callback for the direction unit menu		*
 * seekw_searchNameCb()	callback for the search text widget		*
 * seekw_stateCb()	callback for the state menu			*
 * seekw_clickPbCb()	callback for the click point push button	*
 * seekw_searchListCb()	callback for the search list widget		*
 * seekw_pointFormEh()	event handler for the point forms		*
 * seekw_relDirCb()	callback for the relative point push button	*
 *									*
 * seekw_warp()		moves the mouse pointer to another point	*
 * seekw_manageSearch() manages the search results pane			*
 * seekw_searchName()	does a search on the new text string		*
 * seekw_setState()	sets the state selection menu			*
 * seekw_setRelText()	sets reletive point text and labels		*
 * seekw_updatePt()	updates a points status and redraws		*
 * seekw_ghostPts()	draws or removes the ghosting			*
 * seekw_getVectStr()	builds the distance/direction vector string	*
 * seekw_setSeekSens()	sets the seek form's sensitivity		*
 * seekw_saveCPF()	save Cursor Point lat/lon to a file (CPF)	*
 * seekw_destroyWidget()destroy the existing overriding wid.		*
 ***********************************************************************/

/*=====================================================================*/

Widget seekw_create ( Widget parent )
/************************************************************************
 * seekw_create								*
 *									*
 * This function creates the seek popup window.				*
 *									*
 * Widget seekw_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * seekw_create	Widget	Widget ID of the seek popup window		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		04/99	added distance popup			*
 * S. Law/GSC		05/99	moved distance creation to _pointerEh	*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * S. Law/GSC		12/99	added _seekForm global			*
 * A. Hardy/GSC         01/00   added ability to read loc types from tbl*
 * A. Hardy/GSC         02/00   modified how seek gets 'CITY' default   *
 * H. Zeng/EAI          02/00   did minor changes to the appearance     *
 * H. Zeng/EAI          03/00   unified the use of font                 *
 * D.W.Plummer/NCEP      8/00   only include station type in GUI        *
 * T. Piper/GSC		 7/01	freed flentry				*
 * R. Tian/SAIC		 2/03	added Save CPF button			*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 * M. Li/SAIC           07/03   Allow to accept unlimit entries in      *
 *                              table "locator.tbl"                     *
 * H. Zeng/XTRIA	12/03   changed the size of bitmap button	*
 * T. Piper/SAIC	12/03	initialized seekw_label string		*
 * E. Safford/SAIC	04/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii long			*
 * T. Piper/SAIC        02/06   Removed XmCreate due to AWIPS problem   *
 ***********************************************************************/
{
    Widget	uarrow, darrow, label, button, tempw, tempmenu;
    Widget	paneform, ptform, menuform, distform, dirform, limitform;
    Widget	btnarr[MAX_MENU];
    int		jj, nitems;
    long	ii, ignore;
    char	lstr[5], relflnm[256];
    char	relpxnm[] = "dblarr.xbm";
    char	*ctlstrs[] = {"Take Control", "Save CPF", "Close"};
    char	fontname[] = "-adobe-courier-bold-r-normal-*-*-100-*-*-m-*-*-*";
    Display	*dsp;
    Pixel	fg, bg;
    Pixmap	relpxm;
    XmFontListEntry flentry;
    XmFontList	fontlist;

    FILE       *fp;
    int        bufsiz, format, nlocs, ier, ierr;
    char       buffer[80], name[15], usrsee[15], locstr[60][15];
    static  int     readLOC = 0;
/*---------------------------------------------------------------------*/

    _seekwParent = parent;

    /*
     * read in locator.tbl for location type and format codes.
     */
     if ( readLOC  == 0 ) {
         fp = cfl_tbop ( LOC_TBL, "nmap", &ier );

	 if ( ier == 0 ) {
	     bufsiz = sizeof(buffer);

	     cfl_tbnr ( fp, &nlocs, &ier );

	     _usrLst = (char **)malloc( (size_t) nlocs * sizeof(char *) );
	     _locArea = (char **)malloc( (nlocs+1) * sizeof(char *) );
	     _fmCod = (int *) malloc (nlocs * sizeof (int));

	     cfl_trln ( fp, bufsiz, buffer, &ierr ); 

	     ii = 0;
	     while ( ierr == 0 && ii < nlocs ) {

		 sscanf ( buffer,"%s %s %d",usrsee, name, &format);

		 if ( clo_qformat(name) == 0 || 
		      strcmp ( usrsee, "LATLON" ) == 0 )  {
	         _usrLst[ii] = (char *) malloc( (strlen(usrsee)+1) * sizeof(char) );
		 _locArea[ii] = (char *) malloc( (strlen(name)+1) * sizeof(char) );
		 _fmCod[ii] = format;
		 strcpy ( _usrLst[ii], usrsee);
		 strcpy ( _locArea[ii], name);
		 strcpy ( locstr[ii], name );
		     ii++;
	     	 }

		 cfl_trln ( fp, bufsiz, buffer, &ierr ); 

	     }
	 }
	 cfl_clos ( fp, &ier );
	 nlocs = ii;
     }
     readLOC = 1;

    _ptInfo[PT_1].ctype0 = 0;	
    _ptInfo[PT_1].ctype1 = 0; 
    _ptInfo[PT_2].ctype0 = 0;
    _ptInfo[PT_2].ctype1 = 0; 

    _ptInfo[PT_1].cstate = 0;
    _ptInfo[PT_2].cstate = 0;

    _ptInfo[PT_1].lat = IMPVAL;
    _ptInfo[PT_1].lon = IMPVAL;
    _ptInfo[PT_2].lat = IMPVAL;
    _ptInfo[PT_2].lon = IMPVAL;

    seekw_ghostPts (TRUE);

    _ptInfo[PT_1].format = FORMAT_DEFAULT;
    _ptInfo[PT_2].format = FORMAT_DEFAULT;

    /*
     * create dialog shell
     */
    _mainWin = XtVaCreatePopupShell("seekw_popup",
                        xmDialogShellWidgetClass,       parent,
                        XmNallowShellResize,            True,
		  XmNtitle, "Seek Results",
		  NULL);

    dsp = XtDisplay (_mainWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);
    /*
     * create a parent pane widget
     */
    seek_pane = XtVaCreateWidget("seekw_pane",
			xmPanedWindowWidgetClass,	_mainWin,
			XmNallowResize,                 True,
			XmNmarginHeight,                0,
			XmNmarginWidth,                 0,
			XmNsashHeight,			1,
			XmNsashWidth,                   1,
			XmNspacing,                     1,
			NULL);

    /*******************************************************************
     * create seek list form *******************************************
     *******************************************************************/
    _seekForm = XtVaCreateWidget("seekw_listform",
				 xmFormWidgetClass, seek_pane,
				 NULL);

    /*
     * create the seek result list label 
     */
    _seekLabel = XtVaCreateManagedWidget("seekw_label",
					 xmLabelWidgetClass,	_seekForm,
					 XmNtopAttachment,	XmATTACH_FORM,
					 XmNmarginWidth,	0,
					 XmNfontList,		fontlist,
					 NULL);
    sprintf (lstr, "%s", "");
    NxmLabel_setStr (_seekLabel, lstr);


    /*
     * create the seek result list 
     */
    _seekList = XmCreateScrolledList(_seekForm, "seekw_list", NULL, 0);
    XtVaSetValues(XtParent(_seekList),
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		_seekLabel,
		  XmNtopOffset,		VERT_OFF,
		  XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		  XmNleftWidget,	_seekLabel,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  NULL);

    XtVaSetValues(_seekList,
                  XmNtraversalOn,               FALSE,
                  XmNscrollBarDisplayPolicy,    XmSTATIC,
		  XmNitemCount,			0,
                  XmNvisibleItemCount,          1,
		  XmNfontList,			fontlist,
		  NULL);
    
    XtManageChild(_seekList);

    XtAddCallback(_seekList, XmNbrowseSelectionCallback,
		  (XtCallbackProc)seekw_seekListCb, (XtPointer)NULL);

    XtManageChild(_seekForm);

    /*******************************************************************
     * create options form *********************************************
     *******************************************************************/
    paneform = XtVaCreateWidget("seekw_optform",
			    xmFormWidgetClass,	seek_pane,
			    NULL);

    /*
     * dist button
     */
    nitems = XtNumber (_distLabel);
    if (nitems > MAX_MENU) nitems = MAX_MENU;
    pgutls_createOptionMenu (paneform, nitems, (XtPointer)&_distType, 
			     "Dist\nUnits", (XtCallbackProc)seekw_distCb, &distform,
			     &label, &(tempmenu), 
			     btnarr, _distLabel);

    XtVaSetValues (distform, 
		   XmNleftAttachment,	XmATTACH_FORM,
		   XmNleftOffset,	HORZ_OFF,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	VERT_OFF,
		   NULL);

    XtVaSetValues (label, 
		   XmNfontList,		fontlist,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	VERT_OFF,
		   NULL);

    for(ii = 0; ii < nitems; ii++) {
        XtVaSetValues (btnarr[ii],
		       XmNfontList,	  fontlist,
                       NULL);
    }

    /*
     * dir button
     */
    nitems = XtNumber (_dirLabel);
    if (nitems > MAX_MENU) nitems = MAX_MENU;
    pgutls_createOptionMenu (paneform, nitems, (XtPointer)&_dirType, 
			     "Dir\nUnits", (XtCallbackProc)seekw_dirCb, &dirform,
			     &label, &(tempmenu), 
			     btnarr, _dirLabel);

    XtVaSetValues (dirform, 
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	distform,
		   XmNleftOffset,	HORZ_OFF,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	VERT_OFF,
		   NULL);

    XtVaSetValues (label, 
		   XmNfontList,	        fontlist,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	VERT_OFF,
		   NULL);

    for(ii = 0; ii < nitems; ii++) {
        XtVaSetValues (btnarr[ii],
		       XmNfontList,	  fontlist,
                       NULL);
    }

    /*
     *  Limit widgets
     */
    limitform = XtVaCreateWidget("seekw_limitform",
			       xmFormWidgetClass,	paneform,
			       XmNleftAttachment,	XmATTACH_WIDGET,
			       XmNleftWidget,		dirform,
			       XmNleftOffset,		HORZ_OFF,
			       XmNtopAttachment,	XmATTACH_FORM,
			       XmNtopOffset,		VERT_OFF,
			       NULL);

    _limitLabel = XtVaCreateManagedWidget("xxxx",
					  xmLabelWidgetClass,	limitform,
		                          XmNfontList,	        fontlist,
					  XmNheight,		BUTTON_HEIGHT,
					  XmNmarginHeight,	1,
					  XmNrecomputeSize,	False,
					  XmNrightAttachment,	XmATTACH_FORM,
					  XmNtopAttachment,	XmATTACH_FORM,
					  XmNtopOffset,		VERT_OFF+4,
					  NULL);

    darrow = XtVaCreateManagedWidget ("seekw_arrowdown",
				      xmArrowButtonWidgetClass,	limitform,
				      XmNarrowDirection,	XmARROW_DOWN,
				      XmNheight,		BUTTON_HEIGHT,
				      XmNwidth,			ARROW_WIDTH,
				      XmNrightAttachment,	XmATTACH_WIDGET,
				      XmNrightWidget,		_limitLabel,
				      XmNtopAttachment,		XmATTACH_FORM,
				      XmNtopOffset,		VERT_OFF,
				      NULL);

    XtAddCallback(darrow, XmNactivateCallback,
		  seekw_limitArrowCb, (XtPointer) -1);


    uarrow = XtVaCreateManagedWidget ("seekw_arrowup",
				      xmArrowButtonWidgetClass,	limitform,
				      XmNarrowDirection,	XmARROW_UP,
				      XmNheight,		BUTTON_HEIGHT,
				      XmNwidth,			ARROW_WIDTH,
				      XmNrightAttachment,	XmATTACH_WIDGET,
				      XmNrightWidget,		darrow,
				      XmNrightOffset,		HORZ_OFF,
				      XmNtopAttachment,		XmATTACH_FORM,
				      XmNtopOffset,		VERT_OFF,
				      NULL);

    XtAddCallback(uarrow, XmNactivateCallback,
		  seekw_limitArrowCb, (XtPointer) 1);
    

    XtVaCreateManagedWidget("Limit:",
			    xmLabelWidgetClass,	limitform,
		            XmNfontList,        fontlist,
			    XmNheight,		BUTTON_HEIGHT,
			    XmNrightAttachment,	XmATTACH_WIDGET,
			    XmNrightWidget,	uarrow,
			    XmNrightOffset,	HORZ_OFF,
			    XmNtopAttachment,	XmATTACH_FORM,
			    XmNtopOffset,	VERT_OFF,
			    NULL);

    sprintf (lstr, "%d", _currLimit);
    NxmLabel_setStr (_limitLabel, lstr);

    XtManageChild (limitform);
    
    XtManageChild(paneform);
    
    /*******************************************************************
     * create point form ***********************************************
     *******************************************************************/
    paneform = XtVaCreateWidget("seekw_ptsform",
			    xmFormWidgetClass,	seek_pane,
			    NULL);

    nitems = nlocs;
    for (ii = 0; ii < MAX_POINTS; ii++) {
	if (ii == 0) {
	    ptform = XtVaCreateWidget("seekw_optform",
				       xmFormWidgetClass, paneform,
				       XmNtopAttachment,	XmATTACH_FORM,
				       XmNleftAttachment,	XmATTACH_FORM,
				       NULL);
	}
	else {
	    tempw = ptform;
	    ptform = XtVaCreateWidget("seekw_optform",
				       xmFormWidgetClass, paneform,
				       XmNtopAttachment,  XmATTACH_WIDGET,
				       XmNtopWidget,	  tempw,
				       XmNleftAttachment, XmATTACH_FORM,
				       NULL);
	}

	/*
	 * select point push button
	 */
	sprintf (lstr, "%ld", (ii + 1));
	_ptInfo[ii].ptpb = 
	    XtVaCreateManagedWidget(lstr,
				    xmPushButtonWidgetClass,	ptform,
		                    XmNfontList,	        fontlist,
                                    XmNtraversalOn,             FALSE,
				    XmNheight,			35,
				    XmNleftAttachment,		XmATTACH_FORM,
				    NULL);

	/*
	 * search text widget
	 */
	_ptInfo[ii].namew = 
	    XtVaCreateManagedWidget("seekw_searchname",
				    xmTextWidgetClass,	ptform,
		                    XmNfontList,	fontlist,
				    XmNmaxLength,	MAX_TEXT,
				    XmNcolumns,		15,
				    XmNleftAttachment,	XmATTACH_WIDGET,
				    XmNleftWidget,	_ptInfo[ii].ptpb,
				    XmNleftOffset,	HORZ_OFF,
				    XmNtopAttachment,	XmATTACH_FORM,
				    NULL);

	XtAddCallback(_ptInfo[ii].namew, XmNvalueChangedCallback,
		      (XtCallbackProc)seekw_searchNameCb, (XtPointer)FALSE);
	XtAddCallback(_ptInfo[ii].namew, XmNactivateCallback,
		      (XtCallbackProc)seekw_searchNameCb, (XtPointer)TRUE);

	/*
	 *  type menu
	 */

	_ptInfo[ii].tppbs = (WidgetList)XtMalloc(nitems *sizeof(Widget));
 	pgutls_createOptionMenu (ptform, nitems, (XtPointer)&_ptInfo[ii].ctype0, 
				 "", seekw_typeCb, &menuform, 
				 &label, &(_ptInfo[ii].tpmenu), 
				 _ptInfo[ii].tppbs, _usrLst );

	XtVaSetValues (menuform, 
		       XmNleftAttachment,	XmATTACH_WIDGET,
		       XmNleftWidget,		_ptInfo[ii].namew,
		       NULL);

        for(jj = 0; jj < nitems; jj++) {
           XtVaSetValues (_ptInfo[ii].tppbs[jj],
		       XmNfontList,	  fontlist,
                       NULL);
        }
	XtFree((XtPointer)_ptInfo[ii].tppbs);


	/*
	 * click point push button
	 */
	button = 
	    XtVaCreateManagedWidget("Click Point",
				    xmPushButtonWidgetClass,	ptform,
		                    XmNfontList,	fontlist,
				    XmNheight,		35,
				    XmNleftAttachment,	XmATTACH_WIDGET,
				    XmNleftWidget,	menuform,
				    XmNleftOffset,	HORZ_OFF+25,
				    NULL);

	XtAddCallback (button, XmNactivateCallback,
		       seekw_clickPbCb, (XtPointer) ii);

	/*
	 * set up event handlers for changing current point
	 */
	XtAddEventHandler(ptform, ButtonPressMask, FALSE, 
			  (XtEventHandler)seekw_pointFormEh, (XtPointer) ii);
	XtAddEventHandler(_ptInfo[ii].ptpb, ButtonPressMask, FALSE, 
			  (XtEventHandler)seekw_pointFormEh, (XtPointer) ii);
	XtAddEventHandler(_ptInfo[ii].namew, ButtonPressMask, FALSE, 
			  (XtEventHandler)seekw_pointFormEh, (XtPointer) ii);
	XtAddEventHandler(_ptInfo[ii].tpmenu, ButtonPressMask, FALSE, 
			  (XtEventHandler)seekw_pointFormEh, (XtPointer) ii);
	XtAddEventHandler(button, ButtonPressMask, FALSE, 
			  (XtEventHandler)seekw_pointFormEh, (XtPointer) ii);

	XtManageChild (_ptInfo[ii].tpmenu);
	XtManageChild (ptform);
    }

    xsncolr(ICON_HILITE, &_iconSelect, &ier);

    XtVaGetValues (_ptInfo[_currPt].ptpb,
		   XmNtopShadowColor,	&_iconTShadow,
		   XmNbottomShadowColor,&_iconBShadow, 
		   NULL);

    XtVaSetValues (_ptInfo[_currPt].ptpb,
		   XmNtopShadowColor,		_iconSelect,
		   XmNbottomShadowColor,	_iconSelect,
		   NULL);

    XtManageChild (paneform);


    /*******************************************************************
     * create search form **********************************************
     *******************************************************************/
    _searchForm = XtVaCreateWidget("seekw_searchform",
				 xmFormWidgetClass,	seek_pane,
				 XmNheight,		LIST_HEIGHT,
				 NULL);

    _searchList = XmCreateScrolledList(_searchForm, "seekw_list", NULL, 0);

    XtVaSetValues(XtParent(_searchList),
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNleftAttachment,	XmATTACH_FORM,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  NULL);

    XtVaSetValues(_searchList,
		  XmNscrollingPolicy,		XmAPPLICATION_DEFINED,
		  XmNscrollBarDisplayPolicy,	XmSTATIC,
		  XmNvisualPolicy,		XmCONSTANT,
		  XmNitemCount,			0,
		  XmNfontList,			fontlist,
		  NULL);
    
    XtAddCallback(_searchList, XmNbrowseSelectionCallback,
		  (XtCallbackProc)seekw_searchListCb, (XtPointer)NULL);

    XtManageChild(_searchList);


    /*******************************************************************
     * create point relationship form **********************************
     *******************************************************************/
    _relForm = XtVaCreateWidget("seekw_ptrelform",
				xmFormWidgetClass,	seek_pane,
				NULL);

    _relLabel1 = XtVaCreateManagedWidget("Point 1 is",
					 xmLabelWidgetClass,	_relForm,
		                         XmNfontList,		fontlist,
					 XmNleftAttachment,	XmATTACH_FORM,
					 XmNtopAttachment,	XmATTACH_FORM,
					 XmNtopOffset,		VERT_OFF,
					 NULL);

    _relText = 
	XtVaCreateManagedWidget("seekw_searchname",
				xmTextWidgetClass,	_relForm,
		                XmNfontList,		fontlist,
				XmNmaxLength,		MAX_REL_TEXT,
				XmNcolumns,		MAX_REL_TEXT,
				XmNeditable,		FALSE,
				XmNleftAttachment,	XmATTACH_WIDGET,
				XmNleftWidget,		_relLabel1,
				XmNleftOffset,		HORZ_OFF,
				XmNtopAttachment,	XmATTACH_FORM,
				NULL);

    _relLabel2 = 
	XtVaCreateManagedWidget("of point 2",
				xmLabelWidgetClass,	_relForm,
		                XmNfontList,		fontlist,
				XmNleftAttachment,	XmATTACH_WIDGET,
				XmNleftWidget,		_relText,
				XmNleftOffset,		HORZ_OFF,
				XmNtopAttachment,	XmATTACH_FORM,
				XmNtopOffset,		VERT_OFF,
				NULL);

    cfl_inqr (relpxnm, ICON_DIR, &ignore, relflnm, &ier);
    XtVaGetValues (_relForm, 
		   XmNforeground, &fg,
		   XmNbackground, &bg, 
		   NULL);
    relpxm = XmGetPixmap (XtScreen (_relForm), relflnm, fg, bg);
    button = 
	XtVaCreateManagedWidget ("seekw_reldir",
				 xmPushButtonWidgetClass, _relForm,
				 XmNlabelType,		XmPIXMAP,
				 XmNlabelPixmap,	relpxm,
				 XmNheight,		30,
				 XmNwidth,		30,
				 XmNleftAttachment,	XmATTACH_WIDGET,
				 XmNleftWidget,		_relLabel2,
				 XmNleftOffset,		HORZ_OFF,
				 XmNtopAttachment,	XmATTACH_FORM,
				 XmNtopOffset,		VERT_OFF,
				 NULL );

    XtAddCallback (button, XmNactivateCallback, 
		   seekw_relDirCb, (XtPointer) NULL);


    XtSetSensitive (_relForm, FALSE);
    XtManageChild (_relForm);

    /*******************************************************************
     * create control buttons form *************************************
     *******************************************************************/
    nitems = XtNumber (ctlstrs);
    paneform = XtVaCreateWidget("seekw_ctlform",
				xmFormWidgetClass,	seek_pane,
				XmNfractionBase,	(nitems * FRAC_MULT),
				NULL);

    for (ii = 0; ii < nitems; ii++) {
	button = 
	    XtVaCreateManagedWidget (ctlstrs[ii],
				     xmPushButtonWidgetClass,	paneform,
		                     XmNfontList,	fontlist,
				     XmNheight,		BUTTON_HEIGHT,
				     XmNwidth,		LARGER_WIDTH,
				     XmNleftAttachment,	XmATTACH_POSITION,
				     XmNleftPosition,	(ii * FRAC_MULT),
				     XmNrightAttachment,XmATTACH_POSITION,
				     XmNrightPosition,	((ii + 1) * FRAC_MULT),
				     NULL);

	XtAddCallback (button, XmNactivateCallback,
		       (XtCallbackProc)seekw_ctlBtnCb, (XtPointer)ii);
    }

    XtManageChild (paneform);



    /*******************************************************************
     * Finshed forms ***************************************************
     *******************************************************************/


    XmFontListFree( fontlist );

    return(seek_pane);

}

/*=====================================================================*/

void seekw_popup ( void )
/************************************************************************
 * seekw_popup								*
 *									*
 * This function pops up the seek popup widget.				*
 *									*
 * void seekw_popup ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		04/99	added call to pggst_clearGhost		*
 * S. Law/GSC		05/99	mcanvw_setPressFunc -> setDynamicFunc	*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * S. Law/GSC		12/99	added call to seekw_ghostPts		*
 * S. Law/GSC		05/00	added _seekIsUp				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtManageChild (seek_pane);
    _seekIsUp = TRUE;

    mcanvw_setDynamicFunc ( (XtEventHandler)&seekw_pointerEh,
			    (XtEventHandler)NULL,
			    (XtEventHandler)NULL, CURS_POINT_SELECT);

    seekw_ghostPts (TRUE);

    seekw_update ();
}

/*=====================================================================*/

void seekw_popdown ( void )
/************************************************************************
 * seekw_popdown							*
 *									*
 * This function pops down the seek popup widget.			*
 *									*
 * void seekw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		09/99	added call to mcanvw_setCursor		*
 * S. Law/GSC		12/99	added _useCurrLoc			*
 * S. Law/GSC		12/99	added call to seekw_ghostPts		*
 * S. Law/GSC		05/00	changed to use _seekIsUp and _hideCount	*
 * J. Wu/SAIC		12/01	reset PG operation after popdown	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (_seekIsUp) {
	if (_hideCount == 0) {
	    seekw_ghostPts (FALSE);
	}
	else {
	    _hideCount = 0;
	}

	XtUnmanageChild (seek_pane);
	_seekIsUp = FALSE;
    }

    _useCurrLoc = FALSE;

    mcanvw_disarmDynamic ();
    mcanvw_setCursor (CURS_DEFAULT);

    if ( pgpalw_isUp() ) pgpalw_setupOper();
}

/*=====================================================================*/

void seekw_update ( void )
/************************************************************************
 * seekw_update								*
 *									*
 * This function updates the seek list.					*
 *									*
 * void seekw_update ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		03/99	changed county ids to county names	*
 * M. Li/GSC		10/99	modified clo_direct, clo_dist, and	*
 *				clo_compass codes			*
 * M. Li/GSC		10/99	added multi-point cal. to clo_dist	*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * S. Law/GSC		12/99	added latlon type			*
 * S. Law/GSC		12/99	removed calls to XtSetSensitive		*
 * A. Hardy/GSC		01/00   modified to use _whichLoc               *
 * E. Safford/GSC	01/00	avoid calling split on a null string	*
 * H. Zeng/EAI          02/00   added check to _currLat&_currLon        *
 * H. Zeng/EAI          03/00   changed clo_tgid() to clo_tgnm()        *
 * H. Zeng/EAI          03/00   added _skipRemove flag                  *
 * D.W.Plummer/NCEP	 8/00	replaced clo_qcounty w/ clo_tqbnd	*
 * D.W.Plummer/NCEP	 8/00	add chk of clo_qformat to eliminate bnds*
 * J. Wu/GSC		05/01	free XmStrings			        *
 * T. Piper/SAIC	12/03	fix display for format code 2		*
 ***********************************************************************/
{
    int		nn, ier, ii;
    float	lats[MAX_SEEK], lons[MAX_SEEK];
    char	idx[9*MAX_SEEK+1], inm[33*MAX_SEEK+1], stid[9], name[33],
		*pidx, *pinm, lstr[128], dist_dir[20], tmpnam[21];
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    _whichLoc = clo_which( _locArea[_ptInfo[_currPt].ctype1]);

    if (_whichLoc == -1 ) {
	if (_useCurrLoc) {
	    _ptInfo[_currPt].lat = _currLat;
	    _ptInfo[_currPt].lon = _currLon;

	    XmTextSetString (_ptInfo[_currPt].namew, "latlon");

	    _useCurrLoc = FALSE;
	}

	return;
    }
    sprintf(lstr, " %-24sLat    Long   Distance",
	     _usrLst[_ptInfo[_currPt].ctype0]);
    NxmLabel_setStr (_seekLabel, lstr);

    /*
     * remove current list items
     */
    XtVaGetValues(_seekList, XmNitemCount, &nn, NULL);
    if (nn != 0) {
	XmListDeleteAllItems(_seekList);
    }

    if ( !G_DIFF(_currLat, (float)IMPVAL) && !G_DIFF(_currLon, (float)IMPVAL) ) {
	if ( clo_qformat(_locArea[_ptInfo[_currPt].ctype1]) == 0 )  {

	    clo_tclosest(_locArea[_ptInfo[_currPt].ctype1], _currLat,
					_currLon, _currLimit, &ier);
	    clo_tgltln(  _locArea[_ptInfo[_currPt].ctype1],
					_currLimit, &nn, lats, lons, &ier);
	    clo_tgnm(    _locArea[_ptInfo[_currPt].ctype1], _currLimit,
					sizeof(inm), &nn, inm, &ier);
	    clo_tgid(    _locArea[_ptInfo[_currPt].ctype1], _currLimit,
					sizeof(idx), &nn, idx, &ier);

	    pidx = idx;
	    pinm = inm;
	    _whichLoc = clo_which ( _locArea[_ptInfo[_currPt].ctype1]);

	    for (ii = 0; ii < _currLimit; ii++) {
	        seekw_getVectStr (_currLat, _currLon, lats[ii], 
				lons[ii], dist_dir);


	        if ( strlen(pidx) > (size_t)0  && strlen(pinm) > (size_t)0 ) {
		    pidx = (char *) cst_split (pidx, ';', 9, stid, &ier);
		    pinm = (char *) cst_split (pinm, ';', 33, name, &ier);
	        }
	        else {
		    return;
	        }
		cst_ncpy(tmpnam, name, 20, &ier);
		sprintf (lstr, " %-20s%7.2f%8.2f%s", 
			tmpnam, lats[ii], lons[ii], dist_dir);
		xmstr = XmStringCreateLocalized (lstr);
		XmListAddItemUnselected (_seekList, xmstr, 0);
		XmStringFree (xmstr);
	    
		if (_useCurrLoc) {
		    _ptInfo[_currPt].lat = _currLat;
		    _ptInfo[_currPt].lon = _currLon;

		    _do_not_modify_currLat = TRUE;
		    _skipRemove = TRUE;
		    if ( _isStation[_currPt] ) {
			XmTextSetString (_ptInfo[_currPt].namew, stid);
		    }
		    else {
			XmTextSetString (_ptInfo[_currPt].namew, name);
		    } 
		    _do_not_modify_currLat = FALSE;
		    _skipRemove = FALSE;

		    _useCurrLoc = FALSE;
	        }
	    }
	}
    }
}

/*=====================================================================*/

void seekw_refresh ( void )
/************************************************************************
 * seekw_refresh							*
 *									*
 * This function redraws the ghosting.					*
 *									*
 * void seekw_refresh ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * A. Hardy/GSC		02/00   added NULL check			*
 * S. Law/GSC		05/00	changed to use _seekIsUp and _hideCount	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (_seekIsUp && _hideCount == 0) {
	seekw_ghostPts (FALSE);
    }
}

/*=====================================================================*/

void seekw_saveGhost ( Boolean hide_flag )
/************************************************************************
 * seekw_saveGhost							*
 *									*
 * This function hides or shows the ghosting to conserve it during	*
 * changes to the map.							*
 *									*
 * void seekw_saveGhost (hide_flag)					*
 *									*
 * Input parameters:							*
 *	hide_flag	Boolean		whether to hide or show ghosts	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/


    if (hide_flag) {
	if (_seekIsUp) {
	    if (_hideCount == 0) {
		seekw_ghostPts (FALSE);
	    }
	    _hideCount++;
	}
    }
    else if (_hideCount > 0) {
	if (_hideCount == 1) {
	    seekw_ghostPts (TRUE);

	    mcanvw_setDynamicFunc ((XtEventHandler)&seekw_pointerEh, 
		    		(XtEventHandler)NULL, (XtEventHandler)NULL, 
				   CURS_POINT_SELECT);
	}

	_hideCount--;
    }
}

/*=====================================================================*/

void seekw_ghostPts ( Boolean make_new )
/************************************************************************
 * seekw_ghostPts							*
 *									*
 * This function shows or hides the ghosting based on make_new		*
 *									*
 * void seekw_ghostPts ( make_new )					*
 *									*
 * Input parameters:							*
 *	make_new	Boolean		recalculate cursor positions	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 ***********************************************************************/
{
    int			np, ier;
    static float	pt1x[NSYMB_PTS], pt1y[NSYMB_PTS];
    static float	pt2x[NSYMB_PTS], pt2y[NSYMB_PTS];
    static float	linex[NLINE_PTS], liney[NLINE_PTS];
/*---------------------------------------------------------------------*/

    pggst_veilGhost (FALSE);

    if (_havePt[PT_1] || _havePt[PT_2]) {
	if (_havePt[PT_1] && _havePt[PT_2]) {
	    if (make_new) {
		np = 1;
		gtrans (sys_M, sys_D, &np, 
			&(_ptInfo[PT_1].lat), &(_ptInfo[PT_1].lon), 
			pt1x, pt1y, &ier, strlen(sys_M), strlen(sys_D));
		linex[0] = pt1x[0];
		liney[0] = pt1y[0];

		gtrans (sys_M, sys_D, &np, 
			&(_ptInfo[PT_2].lat), &(_ptInfo[PT_2].lon), 
			pt2x, pt2y, &ier, strlen(sys_M), strlen(sys_D));
		linex[1] = pt2x[0];
		liney[1] = pt2y[0];
	    }

	    pggst_cursorGhost ( pt1x, pt1y, &ier);
	    pggst_cursorGhost ( pt2x, pt2y, &ier);

	    pggst_clearGhost (TRUE);
	    pggst_addGhostPts (2, linex, liney, &ier);
	    pggst_drawGhost (GST_NORMAL);

	    pggst_clearGhost (TRUE);
	}
	else if (_havePt[PT_1]) {
	    if (make_new) {
		np = 1;
		gtrans (sys_M, sys_D, &np, 
			&(_ptInfo[PT_1].lat), &(_ptInfo[PT_1].lon), 
			pt1x, pt1y, &ier, 
			strlen(sys_M), strlen(sys_D) );
	    }

	    pggst_cursorGhost ( pt1x, pt1y, &ier);
	    pggst_clearGhost (TRUE);
	}
	else {
	    if (make_new) {
		np = 1;
		gtrans (sys_M, sys_D, &np, 
			&(_ptInfo[PT_2].lat), &(_ptInfo[PT_2].lon), 
			pt2x, pt2y, &ier, 
			strlen(sys_M), strlen(sys_D) );
	    }

	    pggst_cursorGhost ( pt2x, pt2y, &ier);
	    pggst_clearGhost (TRUE);
	}
    }
}

/*=====================================================================*/

Boolean seekw_isUp ( void ) 
/************************************************************************
 * seekw_isUp								*
 *									*
 * This function queries whether the seek window is up.			*
 *									*
 * Boolean seekw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * seekw_isUp	Boolean		True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * S. Law/GSC		05/00	changed to use _seekIsUp		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (_seekIsUp);
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_pointerEh ( Widget wid, long which, XEvent *event, Boolean *ctdr )
/************************************************************************
 * seekw_pointerEh							*
 *									*
 * This is the event handler for input from the map widget by the	*
 * mouse.								*
 *									*
 * void seekw_pointerEh (wid, which, event, ctdr)			*
 *									*
 * Input parameters:							*
 *	wid	Widget	the widget calling this function		*
 *	which	long	press, drag, or drop				*
 *	*event	XEvent	the event callback structure			*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean	continue to dispatch return flag		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * E. Safford/GSC	03/99	add offset for roam/zoom correction	*
 * S. Law/GSC		04/99	added drag and drop functions		*
 * S. Law/GSC		05/99	changed to create/destroy distance	*
 *				popup on demand				*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * M. Li/GSC		10/99	Modified clo_dist code			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * E. Safford/GSC	10/99	update for new xwcmn.h             	*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * H. Zeng/EAI          03/00   Added positioning for dist_popup        *
 * H. Zeng/EAI          03/00   Added direction into dist_popup         *
 * S. Law/GSC		05/00	removed redundant pggst_clearGhost call	*
 * S. Law/GSC		05/00	added check for _hideCount		*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * H. Zeng/EAI          07/00   removed seekw_ghostPts() from DROP Eh   *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * T. Piper/SAIC	12/04	removed if check in case MCANVW_DROP	*
 * H. Zeng/SAIC		03/07	changed to use _distWid&_distLbl	*
 ***********************************************************************/
{
    int		npts, ier, xoff, yoff;
    float	xx[2], yy[2], lat, lon;
    char	loco[20];
    XmString	xmstr;
    Position	pos_x, pos_y, center_x, center_y;
    Dimension   w_width, w_height;
    Widget      draw_w;
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);

    switch (which) {
      case MCANVW_PRESS:
	if (event->xbutton.button == Button1) {
	    xx[0] = xx[1] = (float) (event->xbutton.x + xoff);
	    yy[0] = yy[1] = (float) (event->xbutton.y + yoff);

	    npts = 1;
	    gtrans (sys_D, sys_M, &npts, xx, yy, &_currLat, &_currLon, 
		    &ier, strlen(sys_D), strlen(sys_M) );

	    /*
	     * create dialog shell
	     */

	    if (!_useCurrLoc) {

	        /*
                 * First destroy any previous overriding widget.
                 */
	        seekw_destroyWidget ();

		_distWid = XtVaCreateWidget("dist_popup",
					     overrideShellWidgetClass,
					     _seekwParent,
					     XmNheight, LABEL_HEIGHT,
					     XmNwidth,	LABEL_WIDTH,
					     NULL);

		_distLbl = XtVaCreateManagedWidget("      0",
						     xmLabelWidgetClass,
						     _distWid, NULL);

                draw_w   = mcanvw_getDrawingW();
		XtVaGetValues (draw_w,
                               XmNwidth,     &w_width, 
                               XmNheight,    &w_height, 
                               NULL);

                center_x = (Position)(w_width / 2);
                center_y = (Position)(w_height / 2);

		XtVaGetValues (_seekwParent, XmNx, &pos_x, XmNy, &pos_y, NULL);

		pos_x += (Position)((float)center_x +
		                    (float)(event->xbutton.x - center_x)*0.80F); 
		pos_y += (Position)((float)center_y +
		                    (float)(event->xbutton.y - center_y)*1.0F);

		XtVaSetValues (_distWid, 
			       XmNx,	pos_x,
			       XmNy,	pos_y,
			       NULL);

		sprintf (loco, "    0 %s", _distLabel[_distType] );
		xmstr = XmStringGenerate(loco, XmFONTLIST_DEFAULT_TAG, XmCHARSET_TEXT, NULL);
		XtVaSetValues (_distLbl, 
			       XmNlabelString,	xmstr, 
			       NULL);
		XmStringFree (xmstr);
		XtManageChild (_distWid);

		mcanvw_setDragFunc ((XtEventHandler)&seekw_pointerEh, CURS_POINT_SELECT);
		mcanvw_setDropFunc ((XtEventHandler)&seekw_pointerEh, CURS_POINT_SELECT);

		npts = 2;
		pggst_clearGhost (TRUE);
		pggst_addGhostPts (npts, xx, yy, &ier);
		pggst_drawGhost (0);
	    }

	    seekw_update ();
	}
	break;

      case MCANVW_DRAG:
	mcanvw_setDynActFlag (TRUE);
	pggst_drawGhost (0);

	xx[0] = (float) (event->xmotion.x + xoff);
	yy[0] = (float) (event->xmotion.y + yoff);

	npts = 1;
	gtrans (sys_D, sys_M, &npts, xx, yy, &lat, &lon, 
	        &ier, strlen(sys_D), strlen(sys_M) );

	seekw_getVectStr (_currLat, _currLon, lat, lon, loco);
	xmstr = XmStringGenerate(loco, XmFONTLIST_DEFAULT_TAG, XmCHARSET_TEXT, NULL);
	XtVaSetValues (_distLbl, 
		       XmNlabelString,	xmstr,
		       NULL);
	XmStringFree (xmstr);

	pggst_replaceGhostPts (npts, xx, yy, &ier);
	pggst_drawGhost (0);
	break;

      case MCANVW_DROP:
	    pggst_drawGhost (0);
	    mcanvw_setDynActFlag (FALSE);
	    mcanvw_disarmDrag ();
	    mcanvw_disarmDrop ();
	    seekw_destroyWidget ();
	break;
    }
    *ctdr = TRUE;
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_seekListCb ( Widget wid, XtPointer clnt, 
					XmListCallbackStruct *cbs )
/************************************************************************
 * seekw_seekListCb							*
 *									*
 * This is the callback function for seek list selection.		*
 *									*
 * void seekw_seekListCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt	XtPointer	not used			*
 *	*cbs	XmListCallbackStruct	event callback structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * T. Piper/SAIC	12/03	modified to set text box 		*
 ***********************************************************************/
{
    char	*listitem;
/*---------------------------------------------------------------------*/

    listitem = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
				XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    sscanf (listitem, "%*s %f %f", &_currLat, &_currLon);
    XtFree (listitem);

    _useCurrLoc = TRUE;
    seekw_update ();
    _useCurrLoc = FALSE;
/*    seekw_warp(); */
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_typeCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * seekw_typeCb								*
 *									*
 * This is the callback function for the seek type widget.		*
 *									*
 * void seekw_typeCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		03/99	added _currType1 and _isStation		*
 * D.W.Plummer/NCEP	 4/99	added _currTypeMap array		*
 * S. Law/GSC		10/99	added _currFormat			*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * S. Law/GSC		12/99	added latlon type			*
 * S. Law/GSC		12/99	added call to seekw_setSeekSens		*
 * A. Hardy/GSC		01/00   modified to use _whichLoc               *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _ptInfo[_currPt].ctype0 = (long) clnt;
    _ptInfo[_currPt].ctype1 = _ptInfo[_currPt].ctype0; 

    _whichLoc = clo_which ( _locArea[_ptInfo[_currPt].ctype1] );

    _isStation[_currPt] = (Boolean)(
			( _fmCod[ _ptInfo[_currPt].ctype1 ] % 10) == 2);

    _ptInfo[_currPt].format = FORMAT_DEFAULT;

    if ( _whichLoc == -1 ) {
    	seekw_setState ("Any");
    }

    _currLat = _ptInfo[_currPt].lat;
    _currLon = _ptInfo[_currPt].lon;

    seekw_setSeekSens ();

    if (_havePt[_currPt]) {
	_useCurrLoc = TRUE;

	seekw_update();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_limitArrowCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * seekw_limitArrowCb							*
 *									*
 * This is the callback function for the limit arrow widgets		*
 *									*
 * void seekw_limitArrowCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	increment/decrement		*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * H. Zeng/EAI          03/00   did adjustment to visible item count    *
 * T. Piper/SAIC	02/06	Added XtVa for AWIPS bluecurve bug	*
 ***********************************************************************/
{
    char lstr[5];
#ifdef AWIPS
    Dimension	height;
#endif
/*---------------------------------------------------------------------*/

    _currLimit += (long) clnt;

    if (_currLimit < 1) _currLimit = 1;
    if (_currLimit > MAX_SEEK) _currLimit = MAX_SEEK;

    sprintf (lstr, "%d", _currLimit);
    NxmLabel_setStr (_limitLabel, lstr);
#ifdef AWIPS
    XtUnmanageChild (seek_pane);
#endif
    if(_currLimit <=5 ) {
      XtUnmanageChild(_seekForm);
      XtUnmanageChild(_seekList);
      XtVaSetValues(_seekList, XmNvisibleItemCount, _currLimit, NULL);
      XtManageChild(_seekList);
      XtManageChild(_seekForm);
    }
    else {
      XtUnmanageChild(_seekForm);
      XtUnmanageChild(_seekList);
      XtVaSetValues(_seekList, XmNvisibleItemCount, 5, NULL);
      XtManageChild(_seekList);
      XtManageChild(_seekForm);
    }
#ifdef AWIPS
    XtVaGetValues(seek_pane, XmNheight, &height, NULL);
    XtVaSetValues(_mainWin, XmNheight, height, NULL);
    XtManageChild(seek_pane);
    XmUpdateDisplay(seek_pane);
#endif
    seekw_update ();
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_distCb ( Widget wid, long unit, XtPointer cbs )
/************************************************************************
 * seekw_distCb								*
 *									*
 * This is the callback function for the button to change the distance	*
 * units.								*
 *									*
 * void seekw_distCb (wid, unit, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	unit	long		which unit				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/99	initial coding				*
 * S. Law/GSC		12/99	changed to use a pulldown menu		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _distType = (int)unit;

    seekw_update();
    seekw_setRelText();
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_dirCb ( Widget wid, long unit, XtPointer cbs )
/************************************************************************
 * seekw_dirCb								*
 *									*
 * This is the callback function for the button to change the direction	*
 * units.								*
 *									*
 * void seekw_dirCb (wid, unit, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	unit	long		which unit				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _dirType = (int)unit;

    seekw_update();
    seekw_setRelText();
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * seekw_ctlBtnCb							*
 *									*
 * This is the callback function for the control buttons.		*
 *									*
 * void seekw_ctlBtnCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * H. Zeng/EAI          03/00   removed last point button callback      *
 * R. Tian/SAIC		02/03	added save CPF				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch (which) {
      case 0:		/* take control */
	pgpalw_classPopdown ();

	mcanvw_setDynamicFunc ( (XtEventHandler)&seekw_pointerEh, 
				(XtEventHandler)NULL, 
				(XtEventHandler)NULL, CURS_POINT_SELECT);

	pggst_clearGhost (TRUE);

	seekw_update ();
	break;

      case 1:		/* save CPF */
	seekw_saveCPF ();
	break;

      case 2:		/* close */
	seekw_popdown ();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_searchNameCb ( Widget wid, Boolean activate, XtPointer cbs )
/************************************************************************
 * seekw_searchNameCb							*
 *									*
 * This is the callback function for value changes to the search	*
 * text widget.								*
 *									*
 * void seekw_searchNameCb (wid, activate, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget	      the widget calling this function	*
 *	activate	Boolean	      activate or not to activate	*
 *	cbs		XtPointer     not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		03/99	changed to search for _currType		*
 * E. Safford/GSC	04/99	fix irix6 compiler warnings		*
 * D.W.Plummer/NCEP	 4/99	fix bug limiting values from findmatch	*
 * S. Law/GSC		10/99	changed sscanf call			*
 * S. Law/GSC		10/99	moved innards to seekw_searchName	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    seekw_searchName (activate);
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_stateCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * seekw_stateCb							*
 *									*
 * This is the callback function for the pick state widget.		*
 *									*
 * void seekw_stateCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	new state			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		12/99	changed over to dual point system	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _ptInfo[_currPt].cstate = (long)clnt;

    seekw_searchName (TRUE);
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_clickPbCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * seekw_clickPbCb							*
 *									*
 * This is the callback function for the click point push button.	*
 *									*
 * void seekw_clickPbCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	not used			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _useCurrLoc = TRUE;
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_searchListCb ( Widget wid, XtPointer clnt, 
					XmListCallbackStruct *cbs )
/************************************************************************
 * seekw_searchListCb							*
 *									*
 * This is the callback function for the search list widget.		*
 *									*
 * void seekw_searchListCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt	XtPointer	not used			*
 *	*cbs	XmListCallbackStruct	event callback structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		03/99	changed to search for _currType		*
 * E. Safford/GSC	04/99	fix irix6 compile warnings		*
 * S. Law/GSC		10/99	changed sscanf and added error check 	*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * S. Law/GSC		12/99	added latlon type and clean up		*
 * A. Hardy/GSC		01/00   modified to use _whichLoc               *
 * T. Piper/SAIC	12/03	simplified & set _useCurrLoc = FALSE	*
 ***********************************************************************/
{
    size_t	ncount;
    char	*listitem, name[MAX_TEXT], state[8];
/*---------------------------------------------------------------------*/

    _whichLoc = clo_which ( _locArea[_ptInfo[_currPt].ctype1]);
    if ( _whichLoc == -1 ) {
	return;
    }

    listitem = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
				XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    sscanf (listitem, "%s %s", name, state);
    XtFree (listitem);

    /* remove comma */
    ncount = strlen (name);
    name[ncount - 1] = '\0';

    seekw_manageSearch (-1);

    seekw_setState (state);

    _useCurrLoc = FALSE;

    XmTextSetString(_ptInfo[_currPt].namew, name);

    seekw_searchName (TRUE);
}

/*=====================================================================*/
/* ARGSUSED */
void seekw_pointFormEh ( Widget wid, long which, XEvent *event, Boolean *ctdr )
/************************************************************************
 * seekw_pointFormEh							*
 *									*
 * This is the event handler for input from the map widget by the	*
 * mouse.								*
 *									*
 * void seekw_pointFormEh (wid, which, event, ctdr)			*
 *									*
 * Input parameters:							*
 *	wid	Widget	the widget calling this function		*
 *	which	long	press, drag, or drop				*
 *	*event	XEvent	the event callback structure			*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean	continue to dispatch return flag		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		12/99	changed over to dual point system	*
 * S. Law/GSC		12/99	added call to seekw_setSeekSens		*
 * H. Zeng/EAI          03/00   changed the para. for seekw_searchName()*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (_currPt == which) return;

    _useCurrLoc = FALSE;

    XtVaSetValues (_ptInfo[_currPt].ptpb,
		   XmNtopShadowColor,		_iconTShadow,
		   XmNbottomShadowColor,	_iconBShadow, 
		   NULL);

    _currPt = (int)which;

    XtVaSetValues (_ptInfo[_currPt].ptpb,
		   XmNtopShadowColor,		_iconSelect,
		   XmNbottomShadowColor,	_iconSelect,
		   NULL);

    seekw_setSeekSens ();
    seekw_searchName (TRUE);

    *ctdr = TRUE;

}

/*=====================================================================*/
/* ARGSUSED */
void seekw_relDirCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * seekw_relDirCb							*
 *									*
 * This is the callback function for the limit arrow widgets		*
 *									*
 * void seekw_relDirCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	clnt	XtPointer	increment/decrement			*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _relDirection = (_relDirection == REL_1TO2) ? REL_2TO1 : REL_1TO2;

    seekw_setRelText ();
}

/*=====================================================================*/

void seekw_warp ( void )
/************************************************************************
 * seekw_warp								*
 *									*
 * This function moves the mouse pointer to the current lat/lon		*
 * on the main map.							*
 *									*
 * void seekw_warp ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * E. Safford/GSC	03/99	correct for offset if roamed/zoomed	*
 * E. Safford/GSC	10/99	update for new xwcmn.h             	*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 ***********************************************************************/
{
    int			npts, ier, xoff, yoff;
    float		xx, yy;
    Widget		mapwid;
    static Display	*mapdsp;
    static Window	mapwin;
    static Boolean	first = True;
/*---------------------------------------------------------------------*/

    npts = 1;
    gtrans (sys_M, sys_D, &npts, &_currLat, &_currLon, 
	    &xx, &yy, &ier, strlen(sys_M), strlen(sys_D) );

    xgtoff (&xoff, &yoff, &ier);
    xx -= (float) xoff; 
    yy -= (float) yoff; 

    if (first) {
	mapwid = mcanvw_getDrawingW ();
	mapdsp = XtDisplay (mapwid);
	mapwin = XtWindow  (mapwid);
	first = False;
    }
	
    XWarpPointer (mapdsp, None, mapwin, 0, 0, 0, 0, (int) xx, (int) yy);

}

/*=====================================================================*/

void seekw_manageSearch ( int which )
/************************************************************************
 * seekw_manageSearch							*
 *									*
 * This function manages the search results pane.			*
 *									*
 * void seekw_manageSearch (which)					*
 *									*
 * Input parameters:							*
 *	which	int		-1 unmanage, 0 invert, 1 manage		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * T. Piper/SAIC	02/06	Added XtVa for AWIPS bluecurve bug	*
 ***********************************************************************/
{
    Boolean	state;
/*---------------------------------------------------------------------*/

    state = XtIsManaged (_searchForm);

    if (state && which <= 0) {
	XtUnmanageChild (_searchForm);
    }
    else if (!state && which >= 0) {
#ifdef AWIPS
	Dimension   height;
	XtUnmanageChild(seek_pane);
#endif
	XtManageChild (_searchForm);
#ifdef AWIPS
	XtVaGetValues(seek_pane, XmNheight, &height, NULL);
	XtVaSetValues(_mainWin, XmNheight, height, NULL);
	XtManageChild(seek_pane);
	XmUpdateDisplay(seek_pane);
#endif
    }
}

/*=====================================================================*/

void seekw_searchName ( Boolean activate )
/************************************************************************
 * seekw_searchName							*
 *									*
 * This function searchs the new text string and updates it and the	*
 * search results list.							*
 *									*
 * void seekw_searchName (activate)					*
 *									*
 * Input parameters:							*
 *	activate	Boolean	    activate or not to activate		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	moved from seekw_searchNameCb and	*
 *				changed over to dual point system	*
 * S. Law/GSC		12/99	added latlon type and clean up		*
 * S. Law/GSC		12/99	cleaned up distance checking		*
 * A. Hardy/GSC		01/00   modified to use _whichLoc               *
 * A. Hardy/GSC		02/00   added degmin conversion for input       *
 * H. Zeng/EAI          03/00   cleared out the scroll list when input  *
 *                              is invalid or implies multi. choices    *
 * H. Zeng/EAI          03/00   added error checking for invalid lat/lon*
 * H. Zeng/EAI          03/00   added checking of _skipRemove flag      *
 * D.W.Plummer/NCEP	 8/00	add chk of clo_qformat to eliminate bnds*
 * D.W.Plummer/NCEP	 8/00	bug fixes				*
 * R. Tian/SAIC		 7/03	changed to call cst_gtag		*
 * T. Piper/SAIC	12/03	fixed use of _isStation			*
 ***********************************************************************/
{
    int		ii, jj, nn, ncount, ier, nocc, iret;
    int         isit, isin, nstlat, nstlon;
    int		deglat, deglon, minlat, minlon;
    char        strlat[7], strlon[8];
    char	name[MAX_TEXT], state[8], tnm[MAX_TEXT], tst[8];
    char	*phit, *pchar, search_range[5];
    char	hitlist[5 * HIT_SIZE], hit[HIT_SIZE], firstname[MAX_TEXT];
    char        **arrlat, **arrlon;
    float	tlat, tlon, tdist, lat, lon, dist;
    size_t	maxsize, namelen;
    Boolean	got_point = FALSE, is_latlon, got_latlon = FALSE;
    XmString	xmstr;
    static size_t lastlen[] = {0, 0};
    static Boolean block = FALSE;
    int	icol;
/*---------------------------------------------------------------------*/
    dist = 0.0F;
    /*
     * block if currently in callback to prevent recursion
     * when _searchName gets updated
     */
    if (block) return;

    /* 
     * This needs to be set to FALSE before returning
     */
    block = TRUE;

    seekw_manageSearch (-1);

    XtVaGetValues(_searchList, XmNitemCount, &ncount, NULL);

    if (ncount != 0) {
	XmListDeleteAllItems (_searchList);
    }

    pchar = XmTextGetString (_ptInfo[_currPt].namew);
    strcpy (name, pchar);
    XtFree (pchar);

    _whichLoc = clo_which ( _locArea[_ptInfo[_currPt].ctype1]);

    icol = ( _fmCod[ _ptInfo[_currPt].ctype1 ] % 10 ) / 2 - 1;
    is_latlon = (Boolean)(( _whichLoc  == -1 ));
    namelen = strlen (name);
    if ((activate && namelen <= (size_t)0) || 
	(!activate && namelen < (size_t)3) ||
	(!activate && !_useCurrLoc && is_latlon)) {
	block = FALSE;
	lastlen[_currPt] = namelen;
	return;
    }

    if (_useCurrLoc || _ptInfo[_currPt].cstate == 0) {
	strcpy (state, "");
    }
    else {
	strcpy (state, _stateLabel[_ptInfo[_currPt].cstate]);
    }

    ncount = 0;
    strcpy (search_range, "");
    if (!is_latlon) {
	if ( clo_qformat(_locArea[_ptInfo[_currPt].ctype1]) == 0 )  {
	    if (activate) {
	        clo_findmatch (_locArea[_ptInfo[_currPt].ctype1],
                               name, state, icol, 1, 
			       5*HIT_SIZE, &ncount, hitlist, &iret);
	    }

	    if (ncount != 1) {

            /*
             * If found more than 1 item, remove current _seekList 
             * items; but if _skipRemove=TRUE, skip the removal 
             * part.
             */
                XtVaGetValues(_seekList, XmNitemCount, &nn, NULL);
                if (nn != 0 && !(_skipRemove)) {
	            XmListDeleteAllItems(_seekList);
                }

	        clo_findmatch (_locArea[_ptInfo[_currPt].ctype1], 
                               name, search_range, icol, 2, 
			       5*HIT_SIZE, &ncount, hitlist, &iret);
	    }
	}
    }

    if (is_latlon) {
	if (!_useCurrLoc) {
            cst_nocc (name, ':', 1, 0, &nocc, &ier );
            if ( ier < 0 ) {
               if(sscanf (name, "%f %f", &lat, &lon)==2) {
                  got_latlon = TRUE;
               }
	    }
	    else {  /* point entered in degrees/minutes */
	        isit = 1;
	        isin = 1;
                if(sscanf (name, "%s %s", strlat, strlon)==2) {
                   got_latlon = TRUE;
                }

                arrlat = (char **) malloc(sizeof(char *) * 2);
                arrlon = (char **) malloc(sizeof(char *) * 2);
                for ( ii = 0; ii < 2; ii++ ) {
                    arrlat[ii] = (char *) malloc(7);
                    arrlon[ii] = (char *) malloc(8);
                }

                cst_clst ( strlat, ':', " ", 2, 7, arrlat, &nstlat, &ier);
                cst_clst ( strlon, ':', " ", 2, 8, arrlon, &nstlon, &ier);

		cst_numb ( arrlat[0], &deglat, &ier);
		cst_numb ( arrlat[1], &minlat, &ier);
		cst_numb ( arrlon[0], &deglon, &ier);
		cst_numb ( arrlon[1], &minlon, &ier);

                for ( ii = 0; ii < 2; ii++ ) {
                        free ( arrlat[ii] );
                        free ( arrlon[ii] );
                }
                free ( arrlat);
                free ( arrlon);

		if ( deglat < 0 ) {
                    isit = -1;
		}
		if ( deglon < 0 ) {
                    isin = -1;
		}

                lat = ( G_ABS((float)deglat) + ((float) minlat / 60.0F) ) * (float)isit;
                lon = ( G_ABS((float)deglon) + ((float) minlon / 60.0F) ) * (float)isin;

	    }

	    if (got_latlon &&
                -90.0F <= lat && lat <= 90.0F &&
		-180.0F <= lon && lon <= 180.0F) {
		_ptInfo[_currPt].lat = _currLat = lat;
		_ptInfo[_currPt].lon = _currLon = lon;

		got_point = TRUE;
	    }
	}
	else {
	    got_point = TRUE;
	}

	if (got_point) {
	    sprintf (name, "%.2f %.2f", _currLat, _currLon);
	    lastlen[_currPt] = strlen(name);

	    XmTextSetString (_ptInfo[_currPt].namew, name);
	    XmTextSetCursorPosition (_ptInfo[_currPt].namew, 
				     (long)lastlen[_currPt]);

	    seekw_warp();
	}
    }  /*  End of is_latlon section  */
    else if (_useCurrLoc) {
	if (ncount > 0) {
	    phit = hitlist;
	    jj = 1;

	    for (ii = 0; ii < ncount; ii++) {
		phit = (char *) cst_split (phit, ';', HIT_SIZE, hit, &ier);
		if (_isStation[_currPt]) {
		    cst_gtag ("STID", hit, " ", tnm, &ier);
		}
		else  {
		    cst_gtag ("NAME", hit, " ", tnm, &ier);
		}
		cst_gtag ("ST",   hit, " ", tst, &ier);
		cst_gtag ("LAT",  hit, "9999", strlat, &ier);
		cst_gtag ("LON",  hit, "9999", strlon, &ier);
		cst_crnm (strlat, &tlat, &ier);
		cst_crnm (strlon, &tlon, &ier);

		clo_dist (&(_ptInfo[_currPt].lat), &(_ptInfo[_currPt].lon),
			  &jj, &tlat, &tlon, &tdist, &ier);

		if (ii == 0 || tdist < dist) {
		    strcpy (name,  tnm);
		    strcpy (state, tst);
		    dist = tdist;
		    lat = tlat;
		    lon = tlon;
		}
	    }

	    if ( !_do_not_modify_currLat ) {
	        _ptInfo[_currPt].lat = lat;
	        _ptInfo[_currPt].lon = lon;
	    }
	}

	lastlen[_currPt] = strlen (name);

	XmTextSetString (_ptInfo[_currPt].namew, name);
	XmTextSetCursorPosition (_ptInfo[_currPt].namew, (long)lastlen[_currPt]);

	seekw_setState (state);

	got_point = TRUE;
    }  /*  End of _useCurrLoc  */
    else if (ncount == 0) {
	strcpy (name, "No match was found.");

	xmstr = XmStringCreateLocalized (name);
	XmListAddItemUnselected (_searchList, xmstr, 0);
	XmStringFree (xmstr);

	got_point = FALSE;
    }
    else if (ncount == 1) {
        if (_isStation[_currPt]) {
            cst_gtag ("STID", hitlist, " ", name, &ier);
        }
        else  {
            cst_gtag ("NAME", hitlist, " ", name, &ier);
        }
	cst_gtag ("ST",   hitlist, " ", state, &ier);
	cst_gtag ("LAT",  hitlist, "9999", strlat, &ier);
	cst_gtag ("LON",  hitlist, "9999", strlon, &ier);
	cst_crnm (strlat, &_currLat, &ier);
	cst_crnm (strlon, &_currLon, &ier);

	_ptInfo[_currPt].lat = _currLat;
	_ptInfo[_currPt].lon = _currLon;

	namelen = strlen (name);
	if (lastlen[_currPt] < namelen) {
	    lastlen[_currPt] = namelen;

	    XmTextSetString (_ptInfo[_currPt].namew, name);
	    XmTextSetCursorPosition (_ptInfo[_currPt].namew, (long)namelen);

	    seekw_update ();
	    seekw_warp();

	    lastlen[_currPt] = namelen;
	    got_point = TRUE;
	}
	else if (lastlen[_currPt] == namelen) {
	    seekw_update ();
	    got_point = TRUE;
	}
	else {
	    lastlen[_currPt] = namelen;
	    got_point = FALSE;
	}
    }
    else {  /*  !useCurrLoc && multiple hits  */
	phit = hitlist;

	for (ii = 0; ii < ncount; ii++) {
	    phit = (char *) cst_split (phit, ';', HIT_SIZE, hit, &ier);

            if (_isStation[_currPt]) {
                cst_gtag ("STID", hit, " ", name, &ier);
            }
            else  {
                cst_gtag ("NAME", hit, " ", name, &ier);
            }
	    cst_gtag ("ST",   hit, " ", state, &ier);

	    if (ii == 0) {
		strcpy (firstname, name);
		maxsize = strlen(firstname);
	    }
	    else if (maxsize > namelen) {
		while (maxsize > namelen && 
		       (strncmp (name, firstname, maxsize) != 0)) {
		    maxsize--;
		}
	    }

	    strcat (name, ", ");
	    strcat (name, state);

	    xmstr = XmStringCreateLocalized (name);
	    XmListAddItemUnselected (_searchList, xmstr, 0);
	    XmStringFree (xmstr);

	    XtVaSetValues (_searchForm, XmNheight, LIST_HEIGHT, NULL);
	}

	/*
	 * copy in maximum common characters
	 */
	if (maxsize > namelen && lastlen[_currPt] <= namelen) {
	    strncpy (name, firstname, maxsize);
	    name[maxsize] = '\0';

	    lastlen[_currPt] = strlen (name);

	    XmTextSetString (_ptInfo[_currPt].namew, name);
	    XmTextSetCursorPosition (_ptInfo[_currPt].namew, (long)lastlen[_currPt]);

	}
	else {
	    lastlen[_currPt] = namelen;
	}

	seekw_manageSearch (1);

	got_point = FALSE;
    }

    seekw_updatePt (got_point);

    block = FALSE;
}

/*=====================================================================*/

void seekw_setState ( char state[] )
/************************************************************************
 * seekw_setState							*
 *									*
 * This function searches for the correct state index and sets the 	*
 * state selection menu.						*
 *									*
 * void seekw_setState (state)						*
 *									*
 * Input parameters:							*
 *	state[]		char	new state				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    for (ii = MAX_STATE - 1; ii > 0; ii--) {
	if (strcmp (state, _stateLabel[ii]) == 0) break;
    }

    _ptInfo[_currPt].cstate = ii;

}

/*=====================================================================*/

void seekw_setRelText ( void )
/************************************************************************
 * seekw_setRelText							*
 *									*
 * This function sets the relative point labels and text based on	*
 * _relDirection.							*
 *									*
 * void seekw_setRelText ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 ***********************************************************************/
{
    char	dist_dir[20];
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if (_havePt[PT_1] && _havePt[PT_2]) {
	XtSetSensitive (_relForm, TRUE);

	switch (_relDirection) {
	  case REL_1TO2:
	    xmstr = XmStringCreateLocalized ("Point 1 is");
	    XtVaSetValues (_relLabel1, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);

	    seekw_getVectStr (_ptInfo[PT_1].lat, _ptInfo[PT_1].lon, 
			      _ptInfo[PT_2].lat, _ptInfo[PT_2].lon, 
			      dist_dir);
	    XmTextSetString (_relText, dist_dir);

	    xmstr = XmStringCreateLocalized ("of point 2");
	    XtVaSetValues (_relLabel2, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);
	    break;

	  case REL_2TO1:
	    xmstr = XmStringCreateLocalized ("Point 2 is");
	    XtVaSetValues (_relLabel1, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);
    
	    seekw_getVectStr (_ptInfo[PT_2].lat, _ptInfo[PT_2].lon, 
			      _ptInfo[PT_1].lat, _ptInfo[PT_1].lon, 
			      dist_dir);
	    XmTextSetString (_relText, dist_dir);

	    xmstr = XmStringCreateLocalized ("of point 1");
	    XtVaSetValues (_relLabel2, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);
	    break;
	}
    }
    else {
	XtSetSensitive (_relForm, FALSE);
    }
}

/*=====================================================================*/

void seekw_updatePt ( Boolean state )
/************************************************************************
 * seekw_updatePt							*
 *									*
 * This function updates the current point's state and redraws the	*
 * ghosting.								*
 *									*
 * void seekw_updatePt (state)						*
 *									*
 * Input parameters:							*
 *	state		Boolean	new state of point			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * S. Law/GSC		05/00	added check for _hideCount		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * unghost current stuff
     */
    if (_hideCount == 0) {
	seekw_ghostPts (FALSE);
    }
    _havePt[_currPt] = state;

    if (state) {
	_currLat = _ptInfo[_currPt].lat;
	_currLon = _ptInfo[_currPt].lon;

	if (!_useCurrLoc) seekw_update ();
    }

    /*
     * ghost if necessary
     */
    if (_hideCount == 0) {
	seekw_ghostPts (TRUE);
    }
    seekw_setRelText ();
}

/*=====================================================================*/

void seekw_getVectStr ( float lat1, float lon1, float lat2, float lon2, 
							char *vectstr )
/************************************************************************
 * seekw_getVectStr							*
 *									*
 * This function builds the distance/direction vector string.		*
 *									*
 * void seekw_getVectStr (lat1, lon1, lat2, lon2, vectstr)		*
 *									*
 * Input parameters:							*
 *	lat1		float	first latitude				*
 *	lon1		float	first longitude				*
 *	lat2		float	second latitude				*
 *	lon2		float	second longitude			*
 *									*
 * Output parameters:							*
 *	*vectstr	char	vector string				*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	moved from seekw_update			*
 ***********************************************************************/
{
    int		ier, icmp, nn;
    float	dist, dir;
    char	rose[10];
/*---------------------------------------------------------------------*/

    nn = 1;
    clo_dist (&lat1, &lon1, &nn, &lat2, &lon2, &dist, &ier);

    switch (_distType) {
      case DIST_SM:
	dist *= M2SM;
	break;

      case DIST_NM:
	dist *= M2NM;
	break;

      case DIST_KM:
	dist /= 1000.0F;
	break;
    }

    dist = (float)G_NINT (dist);

    if (dist < 1.0F) {
	dir = 0.0F;
	strcpy (rose, "-");
    }
    else {
	clo_direct (&lat1, &lon1, &lat2, &lon2, &dir, &ier);

	if (_dirType == DIR_16PT) {
	    clo_compass (&dir, rose, &icmp, &ier);
	}
	else {
	    sprintf (rose, "%3ddeg", (int) dir);
	}
    }

    sprintf (vectstr, "%6d%s %-3s", 
	     (int) dist, _distLabel[_distType], rose);
}

/*=====================================================================*/

void seekw_setSeekSens ( void )
/************************************************************************
 * seekw_setSeekSens							*
 *									*
 * This function sets the seek form's sensitivity.			*
 *									*
 * void seekw_setSeekSens ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * A. Hardy/GSC		01/00   modified to use _whichLoc		*
 ***********************************************************************/
{
    int		nn;
/*---------------------------------------------------------------------*/

    _whichLoc = clo_which ( _locArea[_ptInfo[_currPt].ctype1]);
    if ( _whichLoc == -1 ) {
	XtSetSensitive (_seekForm, FALSE);

	/*
	 * remove current list items
	 */
	XtVaGetValues(_seekList, XmNitemCount, &nn, NULL);
	if (nn != 0) {
	    XmListDeleteAllItems(_seekList);
	}
    }
    else {
	XtSetSensitive (_seekForm, TRUE);
    }
}

/*=====================================================================*/

void seekw_saveCPF ( void )
/************************************************************************
 * seekw_saveCPF    							*
 *									*
 * This function saves the Cursor Point lat/lon to a file (CPF)		*
 *									*
 * void seekw_saveCPF ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * R. Tian/SAIC		02/03	initial coding				*
 * D.W.Plummer/NCEP	 3/03	Add cpf filename to ctb_wrcpf call seq	*
 * T. Piper/SAIC	 1/04	Add capability for multiple points	*
 * T. Piper/SAIC	06/04	Added check for _havePt			*
 ***********************************************************************/
{
    Widget	draw_w;
    static char *mesg = "No Cursor Point Selected\n";
    float	cpf_lat[MAX_POINTS], cpf_lon[MAX_POINTS];
    int		ii, np, iret;
/*---------------------------------------------------------------------*/

    np = 0;
    if ( !_havePt[PT_1] && !_havePt[PT_2] ) {
        draw_w = (Widget)mcanvw_getDrawingW();
        NxmWarn_show(draw_w, mesg);
    }
    else {
	for (ii = 0; ii < MAX_POINTS; ii++) {
	    if ( _havePt[ii] ) {
 	        cpf_lat[np] = _ptInfo[ii].lat;
	        cpf_lon[np] = _ptInfo[ii].lon;
		np++;
	    }
	}

        ctb_wrcpf ( "nmap2.cpf", np, cpf_lat, cpf_lon, &iret ); 
    }
}

/*=====================================================================*/

void seekw_destroyWidget ( void )
/************************************************************************
 * seekw_destroyWidget    						*
 *									*
 * This function destroys the overriding widget that shows dist. & dir.	*
 *									*
 * void seekw_destroyWidget ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		03/07   intital coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * Destroy the widget that _distWid points to.
     */
    if ( _distWid != NULL ) {

         XtDestroyWidget (_distWid);
         _distWid = NULL;
	 _distLbl = NULL;
    }

}

/*=====================================================================*/


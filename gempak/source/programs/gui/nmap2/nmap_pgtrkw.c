#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "NxmTxt.h"
#include "proto_xw.h"

#define DEFAULT_NPTS	 	5
#define MAX_INTERV_TYPES	21
#define NUMB_BTS		3
#define MAX_SKIP		4

static Widget	_mainFormW;
static Widget	_initColorW;
static Widget	_extrColorW;
static Widget	_frmsetRc;
static Widget	_frmsetBtnW[2];
static Widget   _skipsetBtnW[MAX_SKIP];
static Widget   _skipsetTxt;
static Widget	_timeTxtW[3];
static Widget	_ctlFormW;
static WidgetList _ctlBtns;
static Widget	_resultsFormW;
static Widget	_resultsTextW;
static Widget	_arrow;
static Widget	_arrowTrkBx;
static Widget	_skipForm;
static Widget	_top, _top0;

static Widget	_interv_formW;
static Widget	_interv_optW;
static Widget	_interv_pbW[MAX_INTERV_TYPES];
static Widget	_interv_menuW;
static Widget	_interv_submenuW;
static Widget	_intervTxt;

static Widget	_fontMenu, _sizeMenu, _styleMenu;
static WidgetList       _labName;       /* size, font, style */ 
static WidgetList       _fontWid, _sizeWid, _styleWid;

#define  FONT           0
#define  SIZE           1
#define  STYLE          2

static int	_vgType		= TRKSTORM_ELM;
static int	_nExtraPts	= DEFAULT_NPTS;
static int	_incrTime	= 30;	/* interval default to 30 minutes */
static int      _skipFactor     = 0;
static int	_initColor	= 2;
static int	_extrColor	= 5;
static int	_arrowDir 	= XmARROW_DOWN;
static int      _arrowDir0	= XmARROW_DOWN;
static int 	_textLine	= 1;
static int	_intvIndx	= 1;  		/* Default to 00:30	*/

static int              _txtFont        = 0;	/* default to Courier	*/
static int              _txtStyle       = 2;	/* default to Bold	*/
static float            _txtFontSz      = 1.0F;


static fdttms_t	_firstTime	= "";
static fdttms_t	_secondTime	= "";
static fdttms_t	_firstFrame	= "";
static fdttms_t	_secondFrame	= "";

static Boolean	_useFrameTime	= TRUE;


/*
 *  private callback functions
 */
static void pgtrkw_colorCb     ( Widget, XtPointer, XtPointer );
static void pgtrkw_fontCb      ( Widget, XtPointer, XtPointer );
static void pgtrkw_frmsetCb    ( Widget, long, XtPointer );
static void pgtrkw_incrementCb ( Widget, long, XtPointer );
static void pgtrkw_intervTxtCb ( Widget, XtPointer, XtPointer );
static void pgtrkw_sizeCb      ( Widget, XtPointer, XtPointer );
static void pgtrkw_skipsetCb   ( Widget, long, XtPointer );
static void pgtrkw_skipTxtCb   ( Widget, XtPointer, XtPointer );
static void pgtrkw_styleCb     ( Widget, XtPointer, XtPointer );
static void pgtrkw_textExpCb   ( Widget, XtPointer, XtPointer );
static void pgtrkw_timesCb     ( Widget, long, XtPointer );
static void pgtrkw_trkBxExpCb  ( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
static void pgtrkw_checkPoints ( int nipts, int *npts );
static void pgtrkw_getTimes ( fdttms_t first, fdttms_t second );
static void pgtrkw_setTable ( void );

/************************************************************************
 * nmap_pgtrkw.c							*
 *									*
 * This module creates and displays the VG fronts setting box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pgtrkw_create()	creates track attribute window			*
 *  pgtrkw_createResults()	creates track results window		*
 *  pgtrkw_popup()	manages the attribute window			*
 *  pgtrkw_resultsPopup()         popup track results window            *
 *  pgtrkw_popdown()	unmanages the attribute window			*
 *  pgtrkw_setAttr()	sets the track attributes			*
 *  pgtrkw_setFrmTime()	sets the frame time				*
 *  pgtrkw_validateTimes()  check if the first&second times are valid   *
 *  pgtrkw_getInterv()	gets the interval				*
 *									*
 *  pgtrkw_getAttr	gets the track attributes			*
 *  pgtrkw_extrapolate	find extrapolated points			*
 *  pgtrkw_isUp()	query whether the window is up			*
 *  pgtrkw_getNumExtra  gets the number of extrapolated points in track	*
 *									*
 *  pgtrkw_incrementCb	callback for the incremenet buttons		*
 *  pgtrkw_frmsetCb	callback for the frame/set time buttons		*
 *  pgtrkw_timesCb	callback for the times text widgets		*
 *  pgtrkw_skipsetCb    callback for the skip factor choice buttons     *
 *  pgtrkw_skipTxtCb    callback for the skip factor textfield widget   *
 *  pgtrkw_colorCb	callback for the color pushbuttons		*
 *  pgtrkw_textExpCb	callback for text result expandable		*
 *  pgtrkw_trkBxExpCb	callback for track box expandable		*
 *  pgtrkw_intervTxtCb	callback for the interval text widget		*
 *  pgtrkw_fontCb	callback for the text font value		*
 *  pgtrkw_sizeCb 	callback for the text size value		*
 *  pgtrkw_styleCb	callback for the text style value		*
 *									*
 *  pgtrkw_setTable	sets the CES table values			*
 *  pgtrkw_updResults	updates the CES table values			*
 *  pgtrkw_checkPoints	insures the number of points is not over max	*
 *  pgtrkw_getTimes	gets the current and current + increment times	*
 ***********************************************************************/

/*=====================================================================*/

void pgtrkw_create ( Widget parent )
/************************************************************************
 * pgtrkw_create							*
 *									*
 * This function creates a Fronts attribute selection box.		*
 *									*
 * void pgtrkw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * H. Zeng/EAI          02/00   did minor changes to the appearance     *
 * S. Law/GSC		07/00	added frame/set time buttons		*
 * H. Zeng/EAI          07/00   removed activateCallback for text fields*
 * H. Zeng/EAI          08/00   added skip factor choice                *
 * M. Li/GSC		10/00	added more options to interval & label 	*
 *				and made the GUI expandable	 	*
 * J. Wu/GSC		05/01	free XmString				*
 * T. Piper/GSC		 7/01	freed menu_forms, menu_rowcol, 		*
 *				_labName, edit_menuForms, edit_forms	*
 * J. Wu/SAIC		02/02	modify call to ctb_trkitv()		*
 * J. Wu/SAIC           05/02   verify num. of times/skip factor fields	*
 * E. Safford/SAIC	06/02	rm verify -- need to vrfy times not int *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		jj, iret, voff = 5, hoff = 5,  width;
    long	ii, nn;
    char	*frmset_labels[] = {"Frame time", "Set time"};
    char	*time_labels[] = {"First time: ", "Second Time:", 
				  "Number of times: "};
    char        **interv_name;
    char        *skipset_labels[] = {"Skip factor", "Show first&last",
				     "On hour", "On half-hour"};
    char	*cntl_labels[] = {"Apply", "Cancel"};
    char        *style[] = {"Regular", "Italic", "Bold", "B_Italic"};
    char        *fonts[] = {"Courier", "Helvetica ", "Times",
                                "soft ware", "SOFT W."};
    char        *lab_name[] = {"Font:", "Size:", "Style:"};
    char        cc[10], fsznam[20];
    Widget	form, label, attach, interv_menubar;
    Widget      skipset_rc, skiptxt_form, interv_form;

    Widget      menu_bar_fonts, menu_bar_size, menu_bar_style;
    Widget	edit_menu_rc;
    WidgetList  menu_forms, menu_rowcol, edit_menuForms, edit_forms;

    XmString    xmstr, null_string, title_string;;
/*---------------------------------------------------------------------*/

    _mainFormW = XmCreateFormDialog (parent, "tracks_edit", NULL, 0);

    XtVaSetValues(_mainFormW, 
		  XmNnoResize,          True, 
		  NULL);

    XtVaSetValues(XtParent (_mainFormW), XmNtitle, "Track Attributes", NULL);

    /*
     * frame/set time buttons
     */
    _frmsetRc = XtVaCreateManagedWidget ("trkfrmset", 
					 xmRowColumnWidgetClass, _mainFormW,
					 XmNorientation,	XmHORIZONTAL,
					 XmNradioBehavior,	FALSE,
					 XmNtopAttachment,  	XmATTACH_FORM,
					 XmNtopOffset,		voff,
					 NULL);

    nn = XtNumber (frmset_labels);
    for (ii = 0; ii < nn; ii++) {
	_frmsetBtnW[ii] = 
	    XtVaCreateManagedWidget (frmset_labels[ii], 
				     xmToggleButtonWidgetClass, _frmsetRc,
                                     XmNtraversalOn,            FALSE,
				     NULL);

	XtAddCallback (_frmsetBtnW[ii], XmNvalueChangedCallback, 
		       (XtCallbackProc)pgtrkw_frmsetCb, (XtPointer) ii);
    }

    /*
     * number of times
     */
    nn = XtNumber (time_labels);
    width = 110;
    attach = _frmsetRc;
    for (ii = 0; ii < nn; ii++) {
	form = XtVaCreateManagedWidget (time_labels[ii],
					xmFormWidgetClass, 	_mainFormW,
					XmNtopAttachment,  	XmATTACH_WIDGET,
					XmNtopWidget,      	attach,
					XmNtopOffset,	   	voff,
					XmNleftAttachment, 	XmATTACH_FORM,
					XmNrightAttachment,	XmATTACH_FORM,
					XmNwidth,		250,
					NULL);

	attach = form;

	label = XtVaCreateManagedWidget 
	    (time_labels[ii],
	     xmLabelWidgetClass,	form,
	     XmNleftAttachment,		XmATTACH_FORM,
	     XmNleftOffset,		hoff,
	     NULL);

	_timeTxtW[ii] = XtVaCreateManagedWidget 
	    (time_labels[ii],
	     xmTextWidgetClass,		form,
	     XmNrightAttachment,	XmATTACH_FORM,
	     XmNrightOffset,		hoff,
	     XmNmaxLength,		(FDTTMS_SIZE - 1),
	     XmNwidth,			width,
	     NULL);

	XtAddCallback (_timeTxtW[ii], XmNlosingFocusCallback, 
		       (XtCallbackProc)pgtrkw_timesCb, (XtPointer) ii);

	if (ii ==1 ) width = 50;
    }



    /*
     * create interval widget input
     */
    _interv_formW = XtVaCreateWidget ("form",
                         xmFormWidgetClass,     _mainFormW,
			 XmNleftAttachment,	XmATTACH_FORM,
			 XmNtopAttachment,	XmATTACH_WIDGET,
                         XmNtopWidget,		attach,
                         XmNtopOffset,		voff,
			 XmNwidth,		250,
                         NULL);

    attach = _interv_formW;

    interv_form = XtVaCreateManagedWidget("_interv_formW",
                                 xmFormWidgetClass,     _interv_formW,
                                 NULL);

    _interv_menuW = XtVaCreateManagedWidget ("_interv_menu",
         xmRowColumnWidgetClass,        interv_form,
         XmNleftAttachment,		XmATTACH_FORM,
         XmNorientation,                XmHORIZONTAL,
         NULL);

    XtVaCreateManagedWidget ("Interval: ",
         xmLabelGadgetClass,            _interv_menuW,
	 XmNleftAttachment,		XmATTACH_FORM,
	 XmNleftOffset,			hoff,
         NULL);

    _interv_submenuW = XtVaCreateManagedWidget("_interv_submenu",
         xmRowColumnWidgetClass,        _interv_menuW,
         NULL);

    interv_menubar = XmCreatePulldownMenu (_interv_submenuW,
                                          "Interv", NULL, 0);

    _interv_optW = XmCreateOptionMenu (_interv_submenuW,
                                          "interv", NULL, 0);

    ctb_trkqn ( &jj, &iret );
    
    interv_name = (char **) malloc( jj * sizeof( char *) ) ;
    for ( ii = 0; ii < jj; ii++ ) {
 	*(interv_name+ii) = (char *) malloc( 6 * sizeof( char ) );       
    }    
    
    ctb_trkitv ( jj, interv_name, &iret );
    
    for ( ii = 0; ii <= jj ; ii++) {
        if ( ii < jj ) {
	    sprintf ( cc, "%s", interv_name[ii] );
	} 
	else {
	    strcpy ( cc, "Other" );                 
        }
	
        xmstr = XmStringCreateLocalized (cc);
        _interv_pbW[ii] = XtVaCreateManagedWidget(cc,
             xmPushButtonWidgetClass,       interv_menubar,
             NULL);
        XtVaSetValues(_interv_pbW[ii],
            XmNlabelString,             xmstr,
            XmNalignment,               XmALIGNMENT_CENTER,
            NULL);
        XmStringFree (xmstr);

        XtAddCallback(_interv_pbW[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtrkw_incrementCb, (XtPointer) ii);

    }
    
    for ( ii = 0; ii < jj; ii++ ) {
	free( *(interv_name+ii) );
    }
		    
    free( interv_name );		
    
    XtVaSetValues (_interv_optW,
        XmNsubMenuId,                   interv_menubar,
        XmNmenuHistory,                 _interv_pbW[_intvIndx],
        NULL);

    _intervTxt = XtVaCreateManagedWidget
            ("interval text",
             xmTextFieldWidgetClass,    _interv_menuW,
	     XmNrightAttachment,        XmATTACH_FORM,
	     XmNrightOffset,            hoff,
	     XmNcolumns,		6,
	     XmNmaxLength,		5,
             NULL);

    XtSetSensitive(_intervTxt, FALSE);


    XtAddCallback (_intervTxt, XmNactivateCallback, 
		   (XtCallbackProc)pgtrkw_intervTxtCb, (XtPointer)NULL );
    XtAddCallback (_intervTxt, XmNlosingFocusCallback,
		   (XtCallbackProc)pgtrkw_intervTxtCb, (XtPointer)NULL );


    XtManageChild (_interv_optW);
    XtManageChild (_interv_formW); 



    /*
     * create color widget
     */
    form = XtVaCreateManagedWidget("trkcolorform",
                                   xmFormWidgetClass,   _mainFormW,
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        attach,
                                   XmNtopOffset,        voff,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   NULL);

    attach = form;

    label = XtVaCreateManagedWidget ("Colors:",
                                     xmLabelWidgetClass, form,
                                     XmNleftAttachment,  XmATTACH_FORM,
                                     XmNleftOffset,      hoff,
                                     NULL);

    _extrColorW = XtVaCreateManagedWidget("",
                                  xmPushButtonWidgetClass, form,
                                  XmNrightAttachment,      XmATTACH_FORM,
                                  XmNrightOffset,          hoff,
                                  XmNwidth,                25,
                                  XmNheight,               20,
                                  NULL);

    XtAddCallback (_extrColorW, XmNactivateCallback,
                   (XtCallbackProc)pgtrkw_colorCb, (XtPointer) (1));

    _initColorW = XtVaCreateManagedWidget("",
                                   xmPushButtonWidgetClass, form,
                                   XmNrightAttachment,      XmATTACH_WIDGET,
                                   XmNrightWidget,          _extrColorW,
                                   XmNrightOffset,          0,
                                   XmNwidth,                25,
                                   XmNheight,               20,
                                   NULL);

    XtAddCallback (_initColorW, XmNactivateCallback,
                   (XtCallbackProc)pgtrkw_colorCb, (XtPointer) (0));


    /*
     *  Lable options 
     */
    _skipForm = XtVaCreateWidget ("SKIP",
			   xmFormWidgetClass, 	_mainFormW,
                           XmNtopAttachment,  	XmATTACH_WIDGET,
			   XmNtopWidget,      	attach,
			   XmNtopOffset,	voff+10,
			   XmNleftAttachment, 	XmATTACH_FORM,
			   XmNrightAttachment,	XmATTACH_FORM,
			   XmNwidth,		250,
			   NULL);

    label = XtVaCreateManagedWidget ("Label Options",
                                     xmLabelWidgetClass,_skipForm,
                                     XmNleftAttachment,	XmATTACH_FORM,
				     XmNleftOffset,	hoff,
                                     NULL);

    skipset_rc = XtVaCreateWidget ("trkskipset", 
				         xmRowColumnWidgetClass, _skipForm,
				         XmNorientation,	XmVERTICAL,
					 XmNradioBehavior,	FALSE,
					 XmNtopAttachment,  	XmATTACH_WIDGET,
					 XmNtopWidget,		label,
					 XmNtopOffset,		voff,
                                         XmNleftAttachment,     XmATTACH_FORM,
					 NULL);

    nn = XtNumber (skipset_labels);
    for (ii = 0; ii < nn; ii++) {
	_skipsetBtnW[ii] = 
	    XtVaCreateManagedWidget (skipset_labels[ii], 
				     xmToggleButtonWidgetClass, skipset_rc,
                                     XmNtraversalOn,            FALSE,
				     NULL);
	
	XtAddCallback (_skipsetBtnW[ii], XmNvalueChangedCallback, 
		       (XtCallbackProc)pgtrkw_skipsetCb, (XtPointer) ii);
       
    }
  
    XtManageChild(skipset_rc);

    skiptxt_form = XtVaCreateWidget ("trkskiptxt",
			    xmFormWidgetClass, 	_skipForm,
			    XmNtopAttachment,  	XmATTACH_WIDGET,
			    XmNtopWidget,	label,
			    XmNleftAttachment, 	XmATTACH_WIDGET,
                            XmNleftWidget,      skipset_rc,
                            XmNrightAttachment, XmATTACH_FORM,
			    NULL);

    _skipsetTxt = XtVaCreateManagedWidget 
	    ("skip text",
	     xmTextFieldWidgetClass,	skiptxt_form,
	     XmNwidth,			50,
             XmNmaxLength,              4,
             XmNtopAttachment,          XmATTACH_FORM,
	     XmNrightAttachment,	XmATTACH_FORM,
	     XmNrightOffset,		hoff,
	     NULL);
    XtAddCallback(_skipsetTxt, XmNmodifyVerifyCallback, 
                   (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_skipsetTxt, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgtrkw_skipTxtCb, (XtPointer)NULL );

    XtManageChild(skiptxt_form);



    /*
     * creat a rolcol for text editing and menus
     */

    null_string = XmStringCreateLocalized ("");

    edit_menu_rc =  XtVaCreateManagedWidget ("edit_menuRowCol",
                xmRowColumnWidgetClass,         _skipForm,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			skipset_rc,
		XmNtopOffset,			voff+10,		
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			hoff,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     0,
                XmNpacking,                     XmPACK_TIGHT,
                XmNnumColumns,                  1,
                XmNorientation,                 XmVERTICAL,
                XmNisAligned,                   TRUE,
                NULL);


    /*
     *  create form for text editing
     */

    nn = NUMB_BTS;

    edit_menuForms = (WidgetList) XtMalloc( nn * sizeof(Widget));
    edit_forms     = (WidgetList) XtMalloc( nn * sizeof(Widget));
    menu_forms     = (WidgetList) XtMalloc( nn * sizeof(Widget));
    menu_rowcol    = (WidgetList) XtMalloc( nn * sizeof(Widget));


    ii = 0;
    edit_menuForms[ii] = XtVaCreateManagedWidget ("edit_form",
                xmFormWidgetClass,              edit_menu_rc,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);

    edit_forms[ii] = XtVaCreateManagedWidget ("edit_forms",
                xmFormWidgetClass,              edit_menuForms[ii],
                XmNleftAttachment,              XmATTACH_FORM,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);

    menu_forms[ii] =  XtVaCreateManagedWidget ("menu_forms",
                xmFormWidgetClass,              edit_menuForms[ii],
                XmNleftAttachment,              XmATTACH_WIDGET,
                XmNleftWidget,                  edit_forms[ii],
                XmNrightAttachment,             XmATTACH_FORM,
                XmNrightOffset,                 5,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);

    for (ii=1; ii<nn; ii++){

        edit_menuForms[ii] = XtVaCreateManagedWidget ("edit_form",
                xmFormWidgetClass,              edit_menu_rc,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);

        edit_forms[ii] = XtVaCreateManagedWidget ("edit_forms",
                xmFormWidgetClass,              edit_menuForms[ii],
                XmNleftAttachment,              XmATTACH_FORM,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);

        menu_forms[ii] =  XtVaCreateManagedWidget ("menu_forms",
                xmFormWidgetClass,              edit_menuForms[ii],
                XmNleftAttachment,              XmATTACH_WIDGET,
                XmNleftWidget,                  edit_forms[ii],
                XmNrightAttachment,             XmATTACH_FORM,
                XmNrightOffset,                 5,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                NULL);
    }

    XtFree((XtPointer)edit_menuForms);
    XtFree((XtPointer)edit_forms);

    /*
     *  creat labels and RowColums for Size, Font, and Style
     */

    nn = XtNumber(lab_name);
    _labName = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){

        _labName[ii]  =  XtVaCreateManagedWidget (lab_name[ii],
                xmLabelGadgetClass,             menu_forms[ii],
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  10,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNtopOffset,                   5,
                NULL);

        menu_rowcol[ii] = XtVaCreateManagedWidget ("menu_rowcol",
                xmRowColumnWidgetClass,         menu_forms[ii],
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNpacking,                     XmPACK_COLUMN,
                XmNnumColumns,                  1,
                XmNorientation,                 XmHORIZONTAL,
                XmNisAligned,                   TRUE,
                XmNentryAlignment,              XmALIGNMENT_END,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  70,
                XmNrightAttachment,             XmATTACH_FORM,
                XmNrightOffset,                 5,
                NULL);
    }
    XtFree((XtPointer)_labName); 
    XtFree((XtPointer)menu_forms);

    /*
     *  Font Menu
     */


    menu_bar_fonts = XmCreatePulldownMenu(menu_rowcol[FONT],"Fonts",NULL,0);
    _fontMenu = XmCreateOptionMenu (menu_rowcol[FONT], "fontMenu", NULL, 0);
    XtVaSetValues (_fontMenu,
                XmNlabelString,             null_string,
                NULL);

    jj = XtNumber(fonts);
    _fontWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        title_string = XmStringCreateLocalized (fonts[ii]);
        _fontWid[ii] = XtVaCreateManagedWidget(fonts[ii],
                xmPushButtonWidgetClass,    menu_bar_fonts,
                XmNlabelString,             title_string,
                NULL);
        XmStringFree (title_string);

        XtAddCallback(_fontWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtrkw_fontCb, (XtPointer)ii );

    }

    XtVaSetValues (_fontMenu,
                XmNsubMenuId,               menu_bar_fonts,
                XmNmenuHistory,             _fontWid[_txtFont],
                NULL);
    XtManageChild(_fontMenu);


    /*
     *  Size Menu
     */

    menu_bar_size = XmCreatePulldownMenu(menu_rowcol[SIZE],"Size",NULL,0);
    _sizeMenu = XmCreateOptionMenu(menu_rowcol[SIZE],"sizeMenu", NULL, 0);
    XtVaSetValues (_sizeMenu,
                XmNlabelString,             null_string,
                NULL);

    ctb_fszqn(&jj, &iret);
    _sizeWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        ctb_fsznam(ii, fsznam, &iret);
        _sizeWid[ii] = XtVaCreateManagedWidget(fsznam,
                xmPushButtonWidgetClass,    menu_bar_size,
                NULL);

        XtAddCallback(_sizeWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtrkw_sizeCb, (XtPointer)ii );

    }

    ctb_fszfnd(_txtFontSz, &jj, &iret);
    XtVaSetValues (_sizeMenu,
                XmNsubMenuId,               menu_bar_size,
                XmNmenuHistory,             _sizeWid[jj],
                NULL);
    XtManageChild(_sizeMenu);


    /*
     *  Style Menu
     */

    menu_bar_style = XmCreatePulldownMenu(menu_rowcol[STYLE],"Style",NULL, 0);
    _styleMenu = XmCreateOptionMenu(menu_rowcol[STYLE], "styleMenu", NULL, 0);
    XtVaSetValues (_styleMenu,
                XmNlabelString,             null_string,
                NULL);
    XtFree((XtPointer)menu_rowcol);
    XmStringFree ( null_string );

    jj = XtNumber(style);
    _styleWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        title_string = XmStringCreateLocalized (style[ii]);
        _styleWid[ii] = XtVaCreateManagedWidget(style[ii],
                xmPushButtonWidgetClass,    menu_bar_style,
                XmNlabelString,             title_string,
                NULL);
        XmStringFree (title_string);

        XtAddCallback(_styleWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtrkw_styleCb, (XtPointer)ii);

    }

    XtVaSetValues (_styleMenu,
                XmNsubMenuId,               menu_bar_style,
                XmNmenuHistory,             _styleWid[_txtStyle],
                NULL);
    XtManageChild(_styleMenu);



    /*
     * Arrow button
     */
    _top0 = attach;
    _arrowTrkBx = XtVaCreateManagedWidget ("trackw_arrowdown",
                                      xmArrowButtonWidgetClass, _mainFormW,
                                      XmNarrowDirection,        _arrowDir0,
                                      XmNheight,                25,
                                      XmNwidth,                 25,
                                      XmNrightAttachment,       XmATTACH_FORM,
				      XmNtopAttachment,		XmATTACH_WIDGET,
				      XmNtopWidget,		_top0,
				      XmNtopOffset,		voff,
                                      NULL);

    XtAddCallback(_arrowTrkBx, XmNactivateCallback,
                  (XtCallbackProc)pgtrkw_trkBxExpCb, (XtPointer) 0);

    XtManageChild(_arrowTrkBx);

    /*
     * Create control buttons
     */
    attach = _arrowTrkBx;

    _ctlFormW  = XtVaCreateManagedWidget ("trkctlform",
                                         xmFormWidgetClass,	_mainFormW,
                                         XmNtopAttachment,	XmATTACH_WIDGET,
                                         XmNtopWidget,		attach,
                                         XmNtopOffset,		voff,
                                         NULL);

    _ctlBtns = (WidgetList) XtMalloc (XtNumber(cntl_labels) * sizeof(Widget));
    NxmCtlBtn_create (_ctlFormW, 1, "ctlBtns", XtNumber(cntl_labels),
                      cntl_labels, NULL, _ctlBtns);


    pgtrkw_createResults (parent);
}

/*=====================================================================*/

void pgtrkw_createResults ( Widget parent )
/************************************************************************
 * pgtrkw_createResults							*
 *									*
 * This function creates the track results popup.			*
 *									*
 * void pgtrkw_createResults (parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		04/99	added distance popup			*
 * S. Law/GSC		05/99	moved distance creation to _pointerEh	*
 * M. Li/GSC		10/00	made the window expandable		*
 * H. Zeng/EAI          08/02   redesigned ScrolledText widget          *
 ***********************************************************************/
{
    Widget	scrolled_w;
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _resultsFormW = XmCreateFormDialog(parent, "tracks_results", NULL, 0);

    XtVaSetValues(_resultsFormW, 
		  XmNnoResize,		TRUE, 
		  XmNdefaultPosition,	FALSE, 
		  NULL);

    XtVaSetValues(XtParent (_resultsFormW), XmNtitle, "Track Results", NULL);


    /*
     * create the result text 
     */
    scrolled_w = XtVaCreateManagedWidget ("scrolled_w",
                  xmScrolledWindowWidgetClass,    _resultsFormW,
                  XmNscrollingPolicy,             XmAPPLICATION_DEFINED,
                  XmNvisualPolicy,                XmVARIABLE,
                  XmNscrollBarDisplayPolicy,      XmSTATIC,
                  XmNshadowThickness,             0,
		  XmNtopAttachment,               XmATTACH_FORM,
		  XmNleftAttachment,	          XmATTACH_FORM,
                  NULL );

    _resultsTextW = XtVaCreateManagedWidget ("track_rtext",
                  xmTextWidgetClass,              scrolled_w,
		  XmNeditable,			  FALSE,
		  XmNeditMode,			  XmMULTI_LINE_EDIT,
		  XmNcolumns,			  28, 
		  XmNrows,			  _textLine,
		  NULL);


    /*
     * create arrow for expandable text results
     */
    _arrow = XtVaCreateManagedWidget ("trackw_arrowdown",
    				      xmArrowButtonWidgetClass, _resultsFormW,
                                      XmNarrowDirection,        _arrowDir,
                                      XmNheight,                25,
                                      XmNwidth,                 25,
				      XmNtopAttachment,      	XmATTACH_WIDGET,
				      XmNtopWidget,		_resultsTextW,
				      XmNrightAttachment,	XmATTACH_FORM,
                                      NULL);

    XtAddCallback(_arrow, XmNactivateCallback,
                  (XtCallbackProc)pgtrkw_textExpCb, (XtPointer) 0);

    
    XtManageChild(_resultsTextW);
}

/*=====================================================================*/

void pgtrkw_popup ( int vgtype, Boolean show_ctl, XtCallbackProc callback )
/************************************************************************
 * pgtrkw_popup								*
 *									*
 * This function shows the tracks attribute box.			*
 *									*
 * pgtrkw_popup (vgtype, show_ctl, callback)				*
 *									*
 * Input parameters:							*
 *	vgtype	        int	GEMPAK vgtype code			*
 *	show_ctl 	Boolean	show control buttons flag		*
 *	callback	XtCallbackProc	callback for Apply/Cancel btns	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		07/00	set _useFrameTime			*
 * H. Zeng/EAI          08/00   added skip factor for track             *
 * H. Zeng/EAI          08/00   added call to resultsPopup()            *
 * M. Li/GSC		10/00	added text attributes			*
 ***********************************************************************/
{
    VG_DBStruct		el;
    int			ier;
    long		ii;
/*---------------------------------------------------------------------*/

    pgtrkw_popdown ();

    if (show_ctl) {
        XtManageChild (_ctlFormW);
        if (callback) {
	    for (ii = 0; ii < 2; ii++) {
                XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
                XtAddCallback (_ctlBtns[ii], XmNactivateCallback,
			       (XtCallbackProc)callback, (XtPointer) ii);
	    }
        }

        pgtrkw_resultsPopup();

	_useFrameTime = FALSE;
    }
    else {
        XtUnmanageChild (_ctlFormW);
    }

    /* 
     * Save parameters to local variables
     */
    _vgType  = (char) vgtype;

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_TRACKS;

    ces_get(-99, &el, &ier);

    el.elem.trk.info.npts = _nExtraPts;
    el.elem.trk.info.nipts = 0;
    el.elem.trk.info.skip = 0;

    if (_txtFont > 2) {
        el.elem.trk.info.itxfn = _txtFont - 2 + (10 * _txtStyle);
        el.elem.trk.info.ithw = SOFTWARE;
    }
    else {
        el.elem.trk.info.itxfn = _txtFont + 1 + (10 * _txtStyle);;
        el.elem.trk.info.ithw = HARDWARE;
    }

    el.elem.trk.info.sztext = _txtFontSz;

    pgtrkw_setAttr (&el);

    XtManageChild ( _mainFormW );
}

/*=====================================================================*/

void pgtrkw_resultsPopup ( void )
/************************************************************************
 * pgtrkw_resultsPopup							*
 *									*
 * This function pops up the track results window	                *
 *									*
 * void pgtrkw_resultsPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		08/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (!XtIsManaged (_resultsFormW)) {
	    XtManageChild (_resultsFormW);
    }

}

/*=====================================================================*/

void pgtrkw_popdown ( void )
/************************************************************************
 * pgtrkw_popdown							*
 *									*
 * This function unmanages the lines dialog box				*
 *									*
 * void pgtrkw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *				NONE					*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		07/00	moved pgtrkw_setTable call		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    NxmClrW_popdown ();

    if (XtIsManaged (_mainFormW)) {
    	XtUnmanageChild (_mainFormW);

	pgtrkw_setTable ();
    }

    if (XtIsManaged (_resultsFormW)) {
	XtUnmanageChild (_resultsFormW);
    }
}

/*=====================================================================*/

void pgtrkw_setAttr ( VG_DBStruct *el )
/************************************************************************
 * pgtrkw_setAttr							*
 *									*
 * This function sets the values in the tracks dialog box		*
 *									*
 * void pgtrkw_setAttr (el)						*
 *									*
 * Input parameters:                                                    *
 *	*el	VG_DBStruct	element structure			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		11/99	changed to call pgtrkw_checkPoints	*
 * S. Law/GSC		07/00	added frame/set time setup		*
 * H. Zeng/EAI          08/00   added skip factor for track             *
 * M. Li/GSC		10/00	added text font, size and style		*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
    int			ibtn, npts, jj, ii, iret;
/*---------------------------------------------------------------------*/

    /* 
     * colors
     */
    _initColor = (int) el->hdr.maj_col;

    XtVaSetValues(_initColorW,
		  XmNbackground,	NxmColrP_getColorPixel (_initColor),
		  XmNtopShadowColor,	NxmColrP_getColorPixel (_initColor),
		  XmNbottomShadowColor,	NxmColrP_getColorPixel (_initColor),
		  NULL);

    _extrColor = (int) el->hdr.min_col;

    XtVaSetValues(_extrColorW,
		  XmNbackground,	NxmColrP_getColorPixel (_extrColor),
		  XmNtopShadowColor,	NxmColrP_getColorPixel (_extrColor),
		  XmNbottomShadowColor,	NxmColrP_getColorPixel (_extrColor),
		  NULL);

    /*
     * time increment
     */
    _incrTime = el->elem.trk.info.incr;

    ctb_trkfnd(&_incrTime, &_intvIndx, &iret);
    pgtrkw_incrementCb(_interv_pbW[_intvIndx], (int)_intvIndx, NULL);
    XtVaSetValues (_interv_optW,
        XmNmenuHistory,                 _interv_pbW[_intvIndx],
        NULL);


    /*
     * skip factor
     */
    _skipFactor = (int) el->elem.trk.info.skip;
    if(_skipFactor < 0) {
	for (ii = 0; ii < MAX_SKIP; ii++) {
            if ( ii == -_skipFactor ) {
              	XmToggleButtonSetState (_skipsetBtnW[ii], TRUE, TRUE);
            }
            else {
            	XmToggleButtonSetState (_skipsetBtnW[ii], FALSE, FALSE);
            }
    	}

      	pgtrkw_skipsetCb(_skipsetBtnW[-_skipFactor], (int)(-_skipFactor), NULL);
    }
    else {
	for (ii = 0; ii < MAX_SKIP; ii++) {
            if ( ii == 0 ) {
                XmToggleButtonSetState (_skipsetBtnW[ii], TRUE, TRUE);
            }
            else {
                XmToggleButtonSetState (_skipsetBtnW[ii], FALSE, FALSE);
            }
        }

      	pgtrkw_skipsetCb(_skipsetBtnW[0], (int)0, NULL);
    }


    /*
     * frame/set time
     */
    ibtn = (_useFrameTime) ? 0 : 1;
    for (ii = 0; ii < 2; ii++) {
    	if ( ii == ibtn ) {
            XmToggleButtonSetState (_frmsetBtnW[ii], TRUE, TRUE);
    	}
    	else {
            XmToggleButtonSetState (_frmsetBtnW[ii], FALSE, FALSE);
	}
    }


    /*
     * number of extroplated times
     */
    pgtrkw_checkPoints (el->elem.trk.info.nipts, &(el->elem.trk.info.npts));

    /*
     * times
     */
    npts = el->elem.trk.info.nipts;
    if (npts == 0) {
	strcpy (_firstTime, "");
	strcpy (_secondTime, "");

	pgtrkw_getTimes (_firstTime, _secondTime);
    }
    else {
	strcpy (_firstTime,  el->elem.trk.info.times[(npts - 2)]);
	strcpy (_secondTime, el->elem.trk.info.times[(npts - 1)]);
    }

    XmTextSetString (_timeTxtW[0], _firstTime);
    XmTextSetString (_timeTxtW[1], _secondTime);

    /*
     * text font, size and style
     */
    _txtFont = el->elem.trk.info.itxfn % 10;
    _txtFont = (el->elem.trk.info.ithw == SOFTWARE) ?
                _txtFont + 2 : _txtFont - 1;
    XtVaSetValues (_fontMenu,
                XmNmenuHistory,             _fontWid[_txtFont],
                NULL);

    _txtFontSz = el->elem.trk.info.sztext;
    ctb_fszfnd(_txtFontSz, &jj, &iret);
    XtVaSetValues (_sizeMenu,
                XmNmenuHistory,             _sizeWid[jj],
                NULL);

    _txtStyle = (int) el->elem.trk.info.itxfn / 10;
    XtVaSetValues (_styleMenu,
                XmNmenuHistory,             _styleWid[_txtStyle],
                NULL);

}
 

/*=====================================================================*/

void pgtrkw_setFrmTime ( Boolean first_point )
/************************************************************************
 * pgtrkw_setFrmTime							*
 *									*
 * This function sets the current frame time				*
 *									*
 * void pgtrkw_setFrmTime (first_point)					*
 *									*
 * Input parameters:                                                    *
 *	first_point	Boolean		whether this is the first point	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/00	initial coding				*
 * M. Li/GSC		10/00	changed the order of the time frame	*
 ***********************************************************************/
{
    int		cframe, nframe;
    fdttms_t	ctime;
/*---------------------------------------------------------------------*/

    xqcpxm (&nframe, &cframe);
    dataw_getFrameTime (cframe, ctime);

    /*
     * if there is no data, use the current time
     */
    if (strlen (ctime) == (size_t)0) {
	strcpy (_firstFrame, "");
	strcpy (_secondFrame, "");

	pgtrkw_getTimes (_firstFrame, _secondFrame);
    }
    /*
     * if first point, fill the first and blank the second
     */
    else if (first_point) {
	strcpy (_firstFrame, ctime);
	strcpy (_secondFrame, "");
    }
    /*
     * if the second is the same as the first, 
     * second has first plus _incrTime
     */
    else if (strcmp (ctime, _firstFrame) == 0) {
	pgtrkw_getTimes (_firstFrame, _secondFrame);
    }
    /*
     * fill second frame
     */
    else {
	if (strlen (_secondFrame) > (size_t)0) {
	    strcpy (_firstFrame, _secondFrame);
	}

	strcpy (_secondFrame, ctime);
    }

    if (_useFrameTime) {
	XmTextSetString (_timeTxtW[0], _firstFrame);
	strcpy (_firstTime, _firstFrame);

	XmTextSetString (_timeTxtW[1], _secondFrame);
	strcpy (_secondTime, _secondFrame);
    }
}

/*=====================================================================*/

void pgtrkw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pgtrkw_getAttr							*
 *									*
 * This function gets the values in the tracks dialog box		*
 *									*
 * void pgtrkw_getAttr (el)						*
 *									*
 * Input parameters:                                                    *
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el	VG_DBStruct	element structure			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		11/99	added call to pgtrkw_checkPoints	*
 * H. Zeng/EAI          08/00   added skip factor for track             *
 * M. Li/GSC		10/00	added text attributes			*
 * T. Piper/SAIC	 4/02	fixed UMR				*
 * H. Zeng/EAI          08/02   added longitude re-positioning          *
 ***********************************************************************/
{
    int		ii, nipts, old_npts, new_npts;
    float	lon[MAX_TRACKS];
/*---------------------------------------------------------------------*/

    /* 
     * colors
     */
    el->hdr.maj_col = _initColor;
    el->hdr.min_col = _extrColor;

    /*
     * skip factor
     */
    el->elem.trk.info.skip = _skipFactor;

    /*
     * time increment
     */
    el->elem.trk.info.incr = _incrTime;

    /*
     * times
     */
    nipts = el->elem.trk.info.nipts;

    if (0 < nipts && nipts <= MAX_TRACKS) {
	old_npts = el->elem.trk.info.npts;
	new_npts = nipts + _nExtraPts;

	pgtrkw_checkPoints (nipts, &new_npts);
	el->elem.trk.info.npts = new_npts;

	for (ii = 0; ii < nipts; ii++) {
	    lon[ii] = el->elem.trk.latlon[old_npts + ii];
	}

	for (ii = 0; ii < nipts; ii++) {
	    el->elem.trk.latlon[new_npts + ii] = lon[ii];
	}

	strcpy (el->elem.trk.info.times[(nipts - 2)], _firstTime);
	strcpy (el->elem.trk.info.times[(nipts - 1)], _secondTime);
    }

    /*
     * text font, size, style
     */
    if (_txtFont > 2) {
	el->elem.trk.info.itxfn = _txtFont - 2 + (10 * _txtStyle);
	el->elem.trk.info.ithw = SOFTWARE;
    }
    else {
	el->elem.trk.info.itxfn = _txtFont + 1 + (10 * _txtStyle);;
 	el->elem.trk.info.ithw = HARDWARE;
    }

    el->elem.trk.info.sztext = _txtFontSz;
}
 

/*=====================================================================*/

void pgtrkw_extrapolate ( float lat[], float lon[], VG_DBStruct *el )
/************************************************************************
 * pgtrkw_extrapolate							*
 *									*
 * This functions fills the element with the attributes and extroplates	*
 * the _nExtrPts latitude and longitudes.  This uses the lat and lon	*
 * arrays for the initial points unless NULL, in which case it uses	*
 * the values currently stored in the element.				*
 *									*
 * void pgtrkw_extrapolate (lat, lon, el)				*
 *									*
 * Input parameters:							*
 *	lat[]	float		latitude array				*
 *	lon[]	float		longitude array				*
 *									*
 * Output parameters:							*
 *	*el	VG_DBStruct	track element structure			*
 *									*
 * Return parameters:							*
 *				NONE					*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		11/99	added call to popup results window	*
 * S. Law/GSC		11/99	added call to pgtrkw_checkPoints	*
 * S. Law/GSC		07/00	changed to use GEMPAK times		*
 * H. Zeng/EAI          08/00   removed managing of _resultsFormW       *
 ***********************************************************************/
{
    int		ii, jj, ipt1, ipt2, ier;
    float	*plat, *plon;
    fdttms_t	times[MAX_TRACKS];
    TrackInfo	*ptrk;
/*---------------------------------------------------------------------*/

    ptrk = &el->elem.trk.info;

    /*
     * defining points for extrapolated track
     */
    ipt1 = ptrk->nipts - 2;
    ipt2 = ptrk->nipts - 1;

    /*
     * if new initial data
     */
    if (lat != NULL && lon != NULL) {
	ptrk->npts = ptrk->nipts + _nExtraPts;

	/*
	 * set up initial points
	 */
	for (ii = 0, jj = ptrk->npts; ii < ptrk->nipts; ii++, jj++) {
	    el->elem.trk.latlon[ii] = lat[ii];
	    el->elem.trk.latlon[jj] = lon[ii];
	    strcpy (ptrk->times[ii], "");
	}

	strcpy (ptrk->times[ipt1], _firstTime);
	strcpy (ptrk->times[ipt2], _secondTime);
    }
    else {
	_nExtraPts = ptrk->npts - ptrk->nipts;
    }

    pgtrkw_checkPoints (ptrk->nipts, &(ptrk->npts));

    /* 
     * get extrapolated points
     */
    plat = &(el->elem.trk.latlon[0]);
    plon = &(el->elem.trk.latlon[ptrk->npts]);
    clo_track (plat[ipt1], plon[ipt1], ptrk->times[ipt1], 
	       plat[ipt2], plon[ipt2], ptrk->times[ipt2],
	       ptrk->incr, _nExtraPts, &ptrk->speed, &ptrk->dir,
	       &(plat[ptrk->nipts]), &(plon[ptrk->nipts]), times, &ier);

    /*
     * set up times
     */
    for (ii = ptrk->nipts, jj = 0; ii < ptrk->npts; ii++, jj++) {
	strcpy (ptrk->times[ii], times[jj]);
    }

    pgtrkw_updResults (el);


}

/*=====================================================================*/

Boolean pgtrkw_isUp ( void )
/************************************************************************
 * pgtrkw_isUp								*
 *									*
 * This function returns a boolean value specifying whether the circles	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgtrkw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pgtrkw_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	Copied from pgcirc_isUp			*
 ***********************************************************************/
{
    return (XtIsManaged(_mainFormW) );
}

/*=====================================================================*/

int pgtrkw_getNumExtra ( void )
/************************************************************************
 * pgtrkw_getNumExtra							*
 *									*
 * This function returns the number of extrapolated points that will	*
 * be plotted in the track.  This is held in the _nExtraPts variable.	*
 *									*
 * int pgtrkw_getNumExtra ( )                         			*
 *									*
 * Input/Output parameters:						*
 * Output parameters:							*
 *	      		None						*
 *									*
 * Return parameters:							*
 *			int	number of extrapolated points requested	*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/03	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
   return ( _nExtraPts );
}


/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_incrementCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_incrementCb							*
 *									*
 * This function is the callback for the increments buttons.		*
 *									*
 * void pgtrkw_incrementCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		long		client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * M. Li/GSC		10/00	reorganized the GUI			*
 * M. Li/GSC		10/00	changed the output format for _intervTxt*
 * S. Jacobs/NCEP	11/00	Used variable "text" for Other case	*
 * J. Wu/SAIC		09/01	eliminate extra '\0' in sprintf format	*
 ***********************************************************************/
{
    XmString    xmstr;
    char        *text;

/*---------------------------------------------------------------------*/
    _intvIndx = (int)clnt;
    XtVaSetValues (_interv_optW,
        XmNmenuHistory,                 _interv_pbW[_intvIndx],
        NULL);

    XtVaGetValues(_interv_pbW[_intvIndx],
            XmNlabelString,            &xmstr,
            NULL);

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text);
    XmStringFree(xmstr);

    if (strcmp(text, "Other") == 0) {   /* Other */
	XtSetSensitive(_intervTxt, TRUE);
	sprintf(text, "%02d:%02d", _incrTime/60, _incrTime%60);
	XmTextFieldSetString (_intervTxt, text);
    } else {
	XmTextFieldSetString (_intervTxt, "\0");
	XtSetSensitive(_intervTxt, FALSE);
    }

    pgtrkw_getInterv(text);

    XtFree(text);
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_frmsetCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_frmsetCb							*
 *									*
 * This function is the callback for the frame/set time buttons.	*
 *									*
 * void pgtrkw_frmsetCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		long		client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/00	initial coding				*
 * M. Li/GSC		10/00	changed the order of time frame		*
 ***********************************************************************/
{
int ii;
/*---------------------------------------------------------------------*/

    if (XmToggleButtonGetState (wid)) {
	_useFrameTime = (clnt == 0);

	if (_useFrameTime && strlen (_firstFrame) > (size_t)0) {
	    XmTextSetString (_timeTxtW[0], _firstFrame);
	    XmTextSetString (_timeTxtW[1], _secondFrame);
	}
    }

    ii = (int)clnt;

    if ( ii == 0 ) {
	XmToggleButtonSetState (_frmsetBtnW[0], TRUE, TRUE);
	XmToggleButtonSetState (_frmsetBtnW[1], FALSE, FALSE);
    }
    else {
	XmToggleButtonSetState (_frmsetBtnW[0], FALSE, FALSE);
        XmToggleButtonSetState (_frmsetBtnW[1], TRUE, TRUE);
    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_timesCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_timesCb							*
 *									*
 * This function is the callback time text widgets.			*
 *									*
 * void pgtrkw_timesCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		long		client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * S. Law/GSC		07/00	rewrote to consider full date/time	*
 * H. Zeng/EAI          07/00   added validation of time                *
 * M. Li/GSC		10/00	changed the order of cases		*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
    int		npts, time_array[5], nmin, iret;
    char	*pstr;
    fdttms_t	new_time, result_time;
/*---------------------------------------------------------------------*/

    pstr = XmTextGetString (_timeTxtW[clnt]);
    strcpy (new_time, pstr);
    XtFree (pstr);

    switch(clnt) {

      case 0:

        /*
         * set new time for _firstTime
         */
	if ( strcmp(new_time, _firstTime) == 0 )  break;

        ti_stan(new_time, _firstTime, result_time, &iret,
                strlen(new_time), strlen(_firstTime),
                (FDTTMS_SIZE - 1) );
        if( iret < 0 ) {
	  sprintf (new_time, "%s", _firstTime);
          break;      
        }

        result_time[11] = '\0';
        ti_ctoi(result_time, time_array, &iret, 11 );
        if( iret < 0 ) {
	  sprintf (new_time, "%s", _firstTime);
          break;      
        }

        /*
         * verify that result_time < _secondTime
         */
        ti_diff(_secondTime, result_time, &nmin, &iret,
                strlen(_secondTime), strlen(result_time) );
        if( nmin <= 0 ) {
	  sprintf (new_time, "%s", _firstTime);
          break;      
        }

	sprintf (new_time, "%s", result_time);
	sprintf (_firstTime, "%s", new_time);
	XmToggleButtonSetState (_frmsetBtnW[1], TRUE, TRUE);
	XmToggleButtonSetState (_frmsetBtnW[0], FALSE, FALSE);

        break;    

      case 1:

        /*
         * set new time for _secondTime
         */
	if ( strcmp(new_time, _secondTime) == 0 )  break;

        ti_stan(new_time, _secondTime, result_time, &iret,
                strlen(new_time), strlen(_secondTime),
                (FDTTMS_SIZE - 1) );
        if( iret < 0 ) {
	  sprintf (new_time, "%s", _secondTime);
          break;      
        }

        result_time[11] = '\0';
        ti_ctoi(result_time, time_array, &iret, 11 );
        if( iret < 0 ) {
	  sprintf (new_time, "%s", _secondTime);
          break;      
        }

        /*
         * verify that result_time > _firstTime
         */
        ti_diff(result_time, _firstTime, &nmin, &iret,
                strlen(result_time), strlen(_firstTime) );
        if( nmin <= 0 ) {
	  sprintf (new_time, "%s", _secondTime);
          break;      
        }

	sprintf (new_time, "%s", result_time);
	sprintf (_secondTime, "%s", new_time);
	XmToggleButtonSetState (_frmsetBtnW[1], TRUE, TRUE);
	XmToggleButtonSetState (_frmsetBtnW[0], FALSE, FALSE);

        break;    

      case 2:

        /*
         * set number of extra points
         */
        sscanf (new_time, "%d", &npts);

        if (npts == _nExtraPts)  break;

        if (npts > 0) {
            _nExtraPts = npts;
        }
        else {
            sprintf (new_time, "%d", _nExtraPts);
        }

        break;

    }/* the end of switch() */


    XmTextSetString (_timeTxtW[clnt], new_time);
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_skipsetCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_skipsetCb							*
 *									*
 * This function is the callback for the skip factor choice  buttons.	*
 *									*
 * void pgtrkw_skipsetCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		long		client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          08/00   initial coding                          *
 * M. Li/GSC		10/00	Added hour and half-hour options	*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
char   new_factor[10];
int	ii;
/*---------------------------------------------------------------------*/

    if (XmToggleButtonGetState (wid)) {
      switch(clnt) {
         case 0:
               XtSetSensitive(_skipsetTxt, TRUE);
               if(_skipFactor < 0) {
                  _skipFactor = 0;
               }
               sprintf (new_factor, "%d", _skipFactor);
               XmTextFieldSetString (_skipsetTxt, new_factor);
               XmTextFieldSetInsertionPosition(_skipsetTxt, 
	             XmTextFieldGetLastPosition(_skipsetTxt) );
               break;

         case 1:
               XmTextFieldSetString(_skipsetTxt, "\0");
               _skipFactor = -1;
               XtSetSensitive(_skipsetTxt, FALSE);
               break;

         case 2:
               XmTextFieldSetString(_skipsetTxt, "\0");
               _skipFactor = -2;
               XtSetSensitive(_skipsetTxt, FALSE);
               break;

         case 3:
               XmTextFieldSetString(_skipsetTxt, "\0");
               _skipFactor = -3;
               XtSetSensitive(_skipsetTxt, FALSE);
               break;

         default:
               break;

      }/* the end of switch() */

    }

    for (ii = 0; ii < MAX_SKIP; ii++) {
        if ( ii == (int)clnt ) {
            XmToggleButtonSetState (_skipsetBtnW[ii], TRUE, TRUE);
        }
        else {
            XmToggleButtonSetState (_skipsetBtnW[ii], FALSE, FALSE);
        }
    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_skipTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_skipTxtCb							*
 *									*
 * This function is the callback for skip factor text widgets.		*
 *									*
 * void pgtrkw_skipTxtCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer		client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          08/00   initial coding                          *
 ***********************************************************************/
{
    int		value;
    char	*pstr, new_factor[10];
/*---------------------------------------------------------------------*/

    pstr = XmTextGetString (_skipsetTxt);
    strcpy (new_factor, pstr);
    XtFree (pstr);

    if(sscanf (new_factor, "%d", &value) == 1 ) {
       if(value >= 0 && value <= (_nExtraPts-2) ) {
          _skipFactor = value;
       }

    }

    if( XtIsSensitive(_skipsetTxt) ) {
      sprintf (new_factor, "%d", _skipFactor);
      XmTextFieldSetString (_skipsetTxt, new_factor);
    }


}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_colorCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_colorCb							*
 *									*
 * Callback for color button widget.					*
 *									*
 * void pgtrkw_colorCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	0 = major, 1 = minor		*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 ***********************************************************************/
{
    clnt = ((long)clnt == 0) ? (XtPointer) &_initColor : 
	(XtPointer) &_extrColor;

    NxmClrW_popup (wid, clnt, cbs);
}

/*=====================================================================*/

static void pgtrkw_setTable ( void )
/************************************************************************
 * pgtrkw_setTable							*
 *									*
 * This function sets the CES table values.				*
 *									*
 * void pgtrkw_setTable ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()			*
 ***********************************************************************/
{
    int		ier;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_TRACKS;

    ces_get (-99, &el, &ier);

    el.hdr.maj_col = _initColor;
    el.hdr.min_col = _extrColor;
    el.elem.trk.info.incr = _incrTime;

    ces_set (-99, &el, &ier);
}

/*=====================================================================*/

void pgtrkw_updResults ( VG_DBStruct *el )
/************************************************************************
 * pgtrkw_updResults							*
 *									*
 * This function updates the results text widget.			*
 *									*
 * void pgtrkw_updResults ( el )                     			*
 *									*
 * Input parameters:							*
 *	*el	VG_DBStruct	track element structure			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	initial coding				*
 * G. Grosshans/SPC	06/99	Fixed bug in direction and speed to knts*
 * S. Law/GSC		11/99	removed extra initial points from list	*
 * S. Law/GSC		07/00	cleaned up string chopping		*
 ***********************************************************************/
{
    int                 ii, npts, nipts, idir, start;
    float               *plat, *plon, speed, fdir;
    char                text[40], *pstroke;
    char                bar[] = "----------------------------\n";
    fdttms_t            *ptime, one_time;
    XmTextPosition      textpos;
/*---------------------------------------------------------------------*/

    /*
     * convert speed to kts and round direction to 5 degrees
     */
    speed = el->elem.trk.info.speed * MS2NMH;

    idir = (int) el->elem.trk.info.dir / 5;
    fdir = (float)(idir * 5);

    /*
     *  Correct direction
     */
    if ( fdir >= 0.0F && fdir <= 180.0F ) {
      fdir = fdir + 180.0F;
    }
    else {
      fdir = fdir - 180.0F;
    }

    sprintf (text, "Spd: %6.2f kt\tDir: %6.2f deg.\n", speed, fdir);
    XmTextSetString(_resultsTextW, text);

    sprintf (text, "\nTime              Latitude Longitude\n");
    textpos = XmTextGetLastPosition (_resultsTextW);
    XmTextInsert (_resultsTextW, textpos, text);

    textpos = XmTextGetLastPosition (_resultsTextW);
    XmTextInsert (_resultsTextW, textpos, bar);

    npts  = el->elem.trk.info.npts;
    nipts = el->elem.trk.info.nipts;
    ptime = &(el->elem.trk.info.times[0]);
    plat  = &(el->elem.trk.latlon[0]);
    plon  = &(el->elem.trk.latlon[npts]);

    start = ((nipts - 2) < 0) ? 0 : nipts - 2;
    for (ii = start; ii < npts; ii++) {
        if (ii == nipts) {
            textpos = XmTextGetLastPosition (_resultsTextW);
            XmTextInsert (_resultsTextW, textpos, bar);
        }

        pstroke = strchr (ptime[ii], '/');
        if (pstroke == NULL) {
            strcpy (one_time, ptime[ii]);
        }
        else {
            strcpy (one_time, ++pstroke);
        }

        sprintf (text, "%-20s%-9.2f%-9.2f\n", one_time, plat[ii], plon[ii]);
        textpos = XmTextGetLastPosition (_resultsTextW);
        XmTextInsert (_resultsTextW, textpos, text);
    }
}

/*=====================================================================*/

static void pgtrkw_checkPoints ( int nipts, int *npts )
/************************************************************************
 * pgtrkw_checkPoints							*
 *									*
 * This function ensures that the total number of points does not	*
 * exceed the array bounds.						*
 *									*
 * void pgtrkw_checkPoints ( nipts, npts )             			*
 *									*
 * Input parameters:							*
 *	nipts		int	number of initial points		*
 *									*
 * Output parameters:							*
 *	*npts		int	number of total points			*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * M. Li/GSC		10/00	changed the order of time frame		*
 ***********************************************************************/
{
    char	extra_pts_str[10];
/*---------------------------------------------------------------------*/

    if (*npts > MAX_TRACKS) {
	*npts = MAX_TRACKS;

	NxmWarn_show (mcanvw_getDrawingW(), 
		      "Reducing number of times to fit within array\n");
    }

    _nExtraPts = *npts - nipts;
    if (0 > _nExtraPts || _nExtraPts > (MAX_TRACKS - 2)) {
	_nExtraPts = DEFAULT_NPTS;
    }
    sprintf (extra_pts_str, "%d", _nExtraPts);
    XmTextSetString (_timeTxtW[2], extra_pts_str);
}

/*=====================================================================*/

static void pgtrkw_getTimes ( fdttms_t first, fdttms_t second )
/************************************************************************
 * pgtrkw_getTimes							*
 *									*
 * This function replaces first with the current time, if first is a	*
 * blank string, and the replaces second with first plus _incrTime	*
 * minutes.								*
 *									*
 * void pgtrkw_getTimes (first, second)             			*
 *									*
 * Input/Output parameters:						*
 *	first		fdttms_t	current time (if blank)		*
 *									*
 * Output parameters:							*
 *	second		fdttms_t	first plus _incrTime		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/00	initial coding				*
 * B. Yin/SAIC		03/04	changed css_gtim calling sequences	*
 ***********************************************************************/
{
    int		tarry[5], ier, itype = 1;
/*---------------------------------------------------------------------*/

    /*
     * set up first time string, if blank
     */
    if (strlen (first) == (size_t)0) {
	css_gtim (&itype, first, &ier);
    }

    /*
     * set up first time string
     */
    ti_ctoi (first, tarry, &ier, (FDTTMS_SIZE - 1));
    ti_addm (tarry, &_incrTime, tarry, &ier);
    ti_itoc (tarry, second, &ier, (FDTTMS_SIZE - 1));
    second[DTTMS_SIZE - 1] = '\0';
}

/*=====================================================================*/

Boolean pgtrkw_validateTimes ( void )
/************************************************************************
 * pgtrkw_validateTimes							*
 *									*
 * This function checks whether the second time is ahead of the first   *
 * time. If yes, return TRUE; Otherwise return FALSE. The function      *
 * assumes that locally global strings _firstTime and _secondTime are   *
 * either valid GEMPAK time string or NULL.                             *
 *									*
 * Boolean pgtrkw_validateTimes ()             			        *
 *									*
 * Input/Output parameters:						*
 *                      NONE                                            *
 *									*
 * Return parameters:							*
 * pgtrkw_validateTimes		Boolean       check result	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          08/00   initial coding                          *
 ***********************************************************************/
{
    int	  nmin, iret;
/*---------------------------------------------------------------------*/
    
    if(_firstTime[0]=='\0' || _secondTime[0]=='\0') {
       return(FALSE);
    }

    ti_diff(_secondTime, _firstTime, &nmin, &iret,
            strlen(_secondTime), strlen(_firstTime) );
    if( nmin <= 0 ) {
       return(FALSE);     
    }
    else {
       return(TRUE);
    }


}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_textExpCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_textExpCb                                                     *
 *                                                                      *
 * This is the callback function for the text result expandable         *
 *                                                                      *
 * void pgtrkw_textExpCb(wid, clnt, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          the widget calling this function*
 *      clnt     	XtPointer             arrow direction			*
 *      cbs             XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            10/00   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( _arrowDir == XmARROW_DOWN ) {
	_arrowDir = XmARROW_UP;
	_textLine = 10;
    } else {
	_arrowDir = XmARROW_DOWN;
	_textLine = 1;
    }

    XtVaSetValues(_resultsTextW, XmNrows, _textLine, NULL);
    XtVaSetValues(_arrow, XmNarrowDirection, _arrowDir, NULL);
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_trkBxExpCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_trkBxExpCb                                                    *
 *                                                                      *
 * This is the callback function for the track box expandable  	        *
 *                                                                      *
 * void pgtrkw_trkBxExpCb(wid, clnt, cbs)   	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          the widget calling this function*
 *      clnt          XtPointer             arrow direction                 *
 *      cbs             XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            10/00   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/


    if ( _arrowDir0 == XmARROW_DOWN ) {
        XtManageChild(_skipForm);
	_arrowDir0 = XmARROW_UP;
	_top = _skipForm;
    } else {
        XtUnmanageChild(_skipForm);
	_arrowDir0 = XmARROW_DOWN;
	_top = _top0;
    }

    XtVaSetValues(_arrowTrkBx,
		XmNarrowDirection,      _arrowDir0,
		XmNtopAttachment,       XmATTACH_WIDGET,
		XmNtopWidget,           _top,   
		NULL);


}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_intervTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrkw_intervTxtCb                                                   *
 *                                                                      *
 * This function is the callback for interval text widgets.		*
 *                                                                      *
 * void pgtrkw_intervTxtCb (wid, clnt, cbs)                           *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      clnt          XtPointer             client data                     *
 *      cbs             XtPointer       callback struct                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/00   initial coding                          *
 * M. Li/GSC		10/00	changed 30 to 00:30			*
 ***********************************************************************/
{
    char        *pstr, new_factor[10];
/*---------------------------------------------------------------------*/

    pstr = XmTextGetString (_intervTxt);
    strcpy (new_factor, pstr);

    pgtrkw_getInterv(pstr);
    XtFree (pstr);

    if( XtIsSensitive(_intervTxt) ) {
        if (_incrTime == 30)    
      	    XmTextFieldSetString (_intervTxt, "00:30");
	else
	    XmTextFieldSetString (_intervTxt, new_factor);
    }


}

/*=====================================================================*/

void pgtrkw_getInterv ( char *text )
/************************************************************************
 * pgtrkw_getInterv							*
 *                                                                      *
 * This function gets the interval times for the given interval text.	*
 *                                                                      *
 * void pgtrkw_getInterv(text)				                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*text		char		input text			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            10/00   initial coding                          *
 * M. Li/GSC		10/00	removed check for 1440			*
 ***********************************************************************/
{
    int		ii, ier, min, iarr[2];

/*---------------------------------------------------------------------*/

    cst_ilst(text, ':', 0, 2, iarr, &ii, &ier);

    if ( ii == 2 ) {
	min = iarr[0] * 60 + iarr[1];
    } 
    else {
	min = iarr[0];
    }

    if ( min <= 0 ) {
        _incrTime = 30;
    }  else {
        _incrTime = min;
    }

} 

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_fontCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtrkw_fontCb                                                        *
 *                                                                      *
 * This function is the call back to update the font value.             *
 *                                                                      *
 * void pgtrkw_fontCb ( w, clnt, call )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call       XtPointer                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/00	Initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _txtFont = (long)clnt;
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_sizeCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtrkw_sizeCb                                                        *
 *                                                                      *
 * This function is the call back to update the size value.             *
 *                                                                      *
 * void pgtrkw_sizeCb ( w, clnt, call )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call       XtPointer                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            10/00   Initial coding                          *
 ***********************************************************************/
{
int which, iret;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    ctb_fszval(which, &_txtFontSz, &iret);
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtrkw_styleCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtrkw_styleCb                                                       *
 *                                                                      *
 * This function is the call back to update the style value.            *
 *                                                                      *
 * void pgtrkw_styleCb ( w, clnt, call )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call       XtPointer                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            10/00   Initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _txtStyle = (long)clnt;
}

#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"


#define MAX_SHAPES        3
#define NO_LOCATION      -1
#define TOTAL_MARKER     21  
#define ICON_DIR         "$NAWIPS/icons/nmap"   

#define MIN_SIZE_SCALE	0.1F
#define MAX_SIZE_SCALE	 10

#define MIN_WIDTH_SCALE	  1
#define MAX_WIDTH_SCALE	 10 

static Widget	  _pgwbxW;
static Widget     _shapeForm;
static Widget	  _shapeBtns[MAX_SHAPES];
static Widget     _colorBtn;
static Widget	  _colorBtn2;
static Widget	  _fillBtn;
static Widget	  _fillColrForm;
static Widget	  _markerRc;
static Widget     _displayBtn;
static Widget     _displayForm;
static Widget     _mrkTypOptW;
static Widget     _mrkSizTxtW;
static Widget     _mrkSizSldW;
static Widget     _mrkWdthTxtW;
static Widget     _mrkWdthSldW;
static Widget	  _ctlForm;
static WidgetList _mrkTypBtn, _ctlBtns;

/*
 * active watch box attribute variables
 */
static int        _wbxStyle = PGRAM;	/* watch style, WBC or PGRAM 	*/
static int        _wbxColor = 7;	/* watch color 			*/
static int	  _cntyColor= 3;	/* county fill color		*/
static int        _wbxShape = NONE;	/* PGRAM watch shape 		*/

static int        _mrkType  = 1;	/* watch marker type		*/
static float	  _mrkSize  = 1.0F;     /* watch marker size            */
static int	  _mrkWdth  = 1;        /* watch marker width           */

static char	  _cntyFill = 0;        /* Flag indicating whether to fill 
					   county or not. 1 -- yes 0 -- no   */

static int	  _watchOffset = -1;	/* file offset for watch line parent */

static Boolean	  _wbxWaitFlag = False;

static	char	_a0id[9], _a1id[9];
static	float	_a0lt, _a1lt;
static	float	_a0ln, _a1ln;
static	int	_a0dis, _a1dis;
static  int     _wtch_offset;
static	char    _a0dir[4], _a1dir[4];
/*
 * current watch box 
 */
static int		_currWbxLoc = NO_LOCATION;
static VG_DBStruct	_currWbxElm;


/*
 *  private callback functions
 */
void pgwbxw_colorCb    ( Widget, XtPointer, XtPointer );
void pgwbxw_displayCb  ( Widget, XtPointer, XtPointer );
void pgwbxw_shapeCb    ( Widget, long, XtPointer );
void pgwbxw_mrkTypCb   ( Widget, long, XtPointer );
void pgwbxw_sizSldCb   ( Widget, XtPointer, XmScaleCallbackStruct* );
void pgwbxw_sizTxtCb   ( Widget, XtPointer, XtPointer );
void pgwbxw_wdthSldCb  ( Widget, XtPointer, XmScaleCallbackStruct* );
void pgwbxw_wdthTxtCb  ( Widget, XtPointer, XtPointer );
void pgwbxw_fillBtnCb  ( Widget, XtPointer, XmToggleButtonCallbackStruct* );
void pgwbxw_updtYesNoCb( Widget, XtPointer, XtPointer );

/*
 * private functions
 */
void pgwbxw_updtIssuedFlag ( int value, int *watch_offset, 
					int *new_idx, int *iret );
void pgwbxw_crtMrkTyp ( Widget parent );
void pgwbxw_crtSzWth  ( Widget parent );


/************************************************************************
 * nmap_pgwbxw.c							*
 *									*
 * This module defines a watch attribute editing popup window for	*
 * product generation.							*
 *									*
 * CONTENTS:								*
 *	pgwbxw_create()		create the watchbox editing window	*
 *	pgwbxw_popup()		pop up the watchbox editing window	*
 *	pgwbxw_popdown()	pop down the watchbox editing window	*
 *	pgwbxw_setWlst()	saves element for watchlist		*
 *	pgwbxw_setDspLabel()	sets the display label			*
 *	pgwbxw_setUpWtchLn()	sets up prod gen to add a watch line	*
 *	pgwbxw_addWtchLn()	group a watch and watch line         	*
 *									*
 *	pgwbxw_getAttr()	get attributes				*
 *	pgwbxw_setAttr()	set attributes				*
 *      pgwbxw_getWtchOffset()  get the offset of parent watch for line *
 *      pgwbxw_setWtchOffset()  set the offset of parent watch for line *
 *      pgwbxw_isUp		returns status of window		*
 *									*
 *	pgwbxw_shapeCb()	callback for the PGRAM shape		*
 *	pgwbxw_colorCb()	callback for the watchbox & fill colors *
 *	pgwbxw_displayCb()	callback for the county diplay		*
 *	pgwbxw_waitEh()		wait callback until shape is picked	*
 *      pgwbxw_fillBtnCb()	callback for county fill btn		*
 *									*
 *	pgwbxw_updtIssuedFlag()	update the w_issued flag of a watch	*
 *									*
 *      pgwbxw_updtWcnFlag()	pop up WCN county update question box	*
 *      pgwbxw_updtYesNoCb() 	callback for WCN county update		*
 *									*
 ***********************************************************************/

/*=====================================================================*/

Widget pgwbxw_create ( Widget parent )
/************************************************************************
 * pgwbxw_create                                                     	*
 *                                                                      *
 * This function creates the watch attribute edit window.  		*
 *                                                                      *
 * Widget pgwbxw_create(parent)                                		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent      Widget      parent widget               		*
 *                                                                      *
 * Output parameters:                                             	*
 * pgwbxw_create	Widget	Widget ID of the edit popup window	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     	10/97                                           *
 * C. Lin/EAI     	12/97  	remove type selection, add color select *
 * C. Lin/EAI     	 5/98  	add watch type and shape selection     	*
 * D.W.Plummer/NCEP	 9/98	add assignment from ...Sv variables	*
 * S. Law/GSC		11/98	rearranged watchbox widget		*
 * S. Law/GSC		12/98	added display button			*
 * S. Law/GSC		01/99	added call to pgwfmt_create		*
 * H. Zeng/EAI          02/00   did minor changes to the appearance     *
 * H. Zeng/EAI          11/01   added call to pgwfmt_createWCC          *
 * E. Safford/SAIC      06/02   added call to pgwfmt_createWCL          *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * H. Zeng/XTRIA        01/03   added marker selections                 *
 * H. Zeng/SAIC		10/04	added county color fill attrib.		*
 ***********************************************************************/
{
    Widget	pane, color_form, shape_frame, shape_rc;
    Widget	fill_frame, fill_form, label;
    long	ii;
    char	*shapestr[] = {"NS", "EW", "ESOL"}; /* MAX_SHAPES limit */
    char	*btnstr[] = {"Apply", "Cancel"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgwbxW = XmCreateFormDialog(parent, "pgwbxw_popup",
				 NULL, 0);
    XtVaSetValues(_pgwbxW, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgwbxW),
		  XmNtitle, "Watch Attributes",
		  NULL);

/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgwbxw_pane",
			    xmPanedWindowWidgetClass, _pgwbxW,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

/*
 * create color selection area 
 */
    color_form  = XtVaCreateWidget("color_form",
			     xmFormWidgetClass, pane,
			     XmNpaneMinimum,    20,
			     NULL);

    label = XtVaCreateManagedWidget("Color:",
			     xmLabelWidgetClass,   color_form,
			     XmNleftAttachment,    XmATTACH_FORM,
			     XmNleftOffset,	   5,
			     NULL);

    _colorBtn = XtVaCreateManagedWidget(" ",
                xmPushButtonWidgetClass,   color_form,
                XmNleftAttachment,         XmATTACH_WIDGET,
		XmNleftWidget,             label,
                XmNleftOffset,             30,
                XmNwidth,		   25,
                XmNheight,		   20,
                NULL);

    XtAddCallback(_colorBtn, XmNactivateCallback, 
		        (XtCallbackProc)pgwbxw_colorCb, (XtPointer)0 );

    XtManageChild(color_form);

/*
 * create watch shape and fill selection area 
 */
    _shapeForm  = XtVaCreateWidget("pgwbxw_shapeform",
				   xmFormWidgetClass, pane,
				   NULL);

    shape_frame = XtVaCreateWidget("shape_frame",
				   xmFrameWidgetClass, _shapeForm,
				   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_FORM,
				   NULL);

    XtVaCreateManagedWidget("Shape",
				   xmLabelGadgetClass,  shape_frame,
				   XmNchildType,	XmFRAME_TITLE_CHILD,
                                   XmNchildHorizontalAlignment,
                                                        XmALIGNMENT_CENTER,
				   XmNchildVerticalAlignment, 
				                        XmALIGNMENT_CENTER,
				   NULL);



    shape_rc = XtVaCreateWidget("pgwbxw_shaperc",
			     xmRowColumnWidgetClass, shape_frame,
			     XmNorientation,         XmVERTICAL,
                             XmNpacking,             XmPACK_TIGHT,
			     XmNradioBehavior,       False,
			     NULL);

    for ( ii = 0; ii < MAX_SHAPES; ii++ ) {
	_shapeBtns[ii] = 
	    XtVaCreateManagedWidget(shapestr[ii],
                                xmToggleButtonGadgetClass,  shape_rc, 
                                XmNtraversalOn,             False,  
                                NULL);

	XtAddCallback(_shapeBtns[ii], XmNarmCallback, 
		      (XtCallbackProc)pgwbxw_shapeCb,
		      (XtPointer)ii);
    }

    XtManageChild(shape_rc);
    XtManageChild(shape_frame);


    fill_frame = XtVaCreateWidget("fill_frame",
				   xmFrameWidgetClass, _shapeForm,
				   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
				   XmNleftWidget,      shape_frame,
				   XmNleftOffset,      10,
				   NULL);

    XtVaCreateManagedWidget("Fill",
				   xmLabelGadgetClass,  fill_frame,
				   XmNchildType,	XmFRAME_TITLE_CHILD,
                                   XmNchildHorizontalAlignment,
                                                        XmALIGNMENT_CENTER,
				   XmNchildVerticalAlignment, 
				                        XmALIGNMENT_CENTER,
				   NULL);



    fill_form = XtVaCreateWidget("pgwbxw_fillform",
			     xmFormWidgetClass, fill_frame, NULL);

    _fillBtn = XtVaCreateManagedWidget("Use Fill",
                             xmToggleButtonGadgetClass,  fill_form, 
			     XmNtopAttachment,           XmATTACH_FORM,
			     XmNtopOffset,		 3,
                             XmNleftAttachment,          XmATTACH_FORM,
                             XmNtraversalOn,             False,  
                             NULL);
 
    XtAddCallback(_fillBtn, XmNvalueChangedCallback, 
		      (XtCallbackProc)pgwbxw_fillBtnCb, (XtPointer)0);
 
/*
 * create fill color selection area 
 */
    _fillColrForm  = XtVaCreateWidget("fill_color_form",
			     xmFormWidgetClass,    fill_form, 
			     XmNtopAttachment,     XmATTACH_WIDGET,
			     XmNtopWidget,	   _fillBtn,
			     XmNtopOffset,	   3,
                             XmNleftAttachment,    XmATTACH_FORM,
			     NULL);

    label = XtVaCreateManagedWidget("Color:",
			     xmLabelWidgetClass, _fillColrForm, NULL);

    _colorBtn2 = XtVaCreateManagedWidget(" ",
                xmPushButtonWidgetClass,   _fillColrForm,
                XmNleftAttachment,         XmATTACH_WIDGET,
		XmNleftWidget,             label,
                XmNleftOffset,             10,
                XmNwidth,		   25,
                XmNheight,		   20,
                NULL);

    XtAddCallback(_colorBtn2, XmNactivateCallback, 
		        (XtCallbackProc)pgwbxw_colorCb, (XtPointer)1 );

    XtManageChild  ( _fillColrForm);

    label = XtVaCreateManagedWidget("",
			     xmLabelWidgetClass,   fill_form, 
			     XmNtopAttachment,     XmATTACH_WIDGET,
			     XmNtopWidget,	   _fillColrForm,
			     XmNtopOffset,	   30,
                             XmNleftAttachment,    XmATTACH_FORM,
			     NULL);

    XtManageChild(fill_form);
    XtManageChild(fill_frame);


    XtManageChild(_shapeForm);

/*
 * Create watch marker type, size and width selection area.
 */
    _markerRc = XtVaCreateManagedWidget ( "rc1",
		xmRowColumnWidgetClass,		pane,
		XmNorientation,			XmVERTICAL,
		XmNspacing,			10,
		NULL );

    pgwbxw_crtMrkTyp ( _markerRc );
    pgwbxw_crtSzWth  ( _markerRc );

/*
 * create show display button
 */
    _displayForm  = XtVaCreateWidget("display_form",
			     xmFormWidgetClass, pane,
			     NULL);

    _displayBtn = XtVaCreateManagedWidget("Show Display",
                xmPushButtonWidgetClass, _displayForm,
                XmNleftAttachment,       XmATTACH_FORM,
                XmNleftOffset,           45,     
		XmNlabelType,		 XmSTRING,
                XmNheight,		 25,
                NULL);

    XtAddCallback(_displayBtn, XmNactivateCallback, (XtCallbackProc)pgwbxw_displayCb, NULL);

/*
 * Create control buttons
 */
    _ctlForm  = (Widget)XtVaCreateManagedWidget ("_frt_ctl_formW",
		xmFormWidgetClass,	pane,
	 	NULL);

    _ctlBtns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));

    NxmCtlBtn_create (_ctlForm, 0, "ctlBtns", XtNumber(btnstr), btnstr,
		      NULL, _ctlBtns);


    XtManageChild (pane);

    pgwlst_create(parent);
    pgwfmt_create(parent);

/*
 * Be sure pgwfmt_createWCC() is placed after pgwfmt_create().
 */
    pgwfmt_createWCC(parent);
    pgwfmt_createWCL(parent);

    return(_pgwbxW);
}

/*=====================================================================*/

void pgwbxw_popup ( int style, int color, int shape, int mtype,
                    float msize, int mwidth, signed char cnty_fill, 
		    int cnty_colr, int show_ctl, XtCallbackProc callback )
/************************************************************************
 * pgwbxw_popup								*
 *									*
 * This function pops up the watch editing window.			*
 *									*
 * void pgwbxw_popup (style, color, shape, mtype, msize, mwidth,        *
 *			cnty_fill, cnty_colr, show_ctl, callback)	*
 *									*
 * Input parameters:							*
 *	style		int	Sets either county or parallelagram	*
 *	color		int	Sets color or uses default color on -1	*
 *	shape		int		NS, EW, ESOL, or saved on -1	*
 *	mtype		int		Set watch marker type		*
 *	msize		float		Set watch marker size		*
 *	mwidth		int		Set watch marker width		*
 *	cnty_fill	signed char	Set couty fill			*
 *	cnty_colr	int		Set county color		*
 *	show_ctl	int	flag whether to show control buttons	*
 *	callback	XtCallbackProc	Callback for control buttons	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * C. Lin/EAI		12/97	modified for color selection		*
 * D.W.Plummer/NCEP	 9/98	add _wbxStyle,_wbxColor,_wbxShape reset	*
 * S. Law/GSC		11/98	added attributes and callback to params	*
 * S. Law/GSC		12/98	added calls to _setDspLabel,pgwlst_popup*
 * H. Zeng/EAI		03/00	added _displayForm manage&unmanage      *
 * J. Wu/GSC		03/01	canceled the pop-up of watch Spec. Win. *
 * H. Zeng/XTRIA	01/03	used ces_get() to get marker info.      *
 * H. Zeng/XTRIA	12/03	used default color when color == -1	*
 * H. Zeng/SAIC		11/04	changed para. list for pgwbxw_setAttr	*
 ***********************************************************************/
{
    int		level, ier1, ier;
    long	ii;
    char	grp[4], logstr[10];
    Boolean     reset;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (show_ctl) {
        XtManageChild (_displayForm);
        XtManageChild (_ctlForm);
        if (callback) {
	    for (ii = 0; ii < 2; ii++) {
                XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
                XtAddCallback (_ctlBtns[ii], XmNactivateCallback,
                                        callback, (XtPointer)ii);
	    }
        }
    }
    else {
        XtUnmanageChild (_displayForm);
        XtUnmanageChild (_ctlForm);
    }

    XtManageChild(_pgwbxW);

    if (style != -1) _wbxStyle = style;
    if (shape != -1) _wbxShape = shape;

    if (style == WBC) _wbxShape = NONE;

    if (pgpalw_getCurOperId () == FUNC_SELECT) {
        reset = (Boolean)(show_ctl ? FALSE : TRUE);
	if (_wbxShape) {
            pgwlst_popup(reset);
        }
    }

    if (pgwlst_isUp ()) {
	pgwbxw_setDspLabel (1);
    }
    else {
	pgwbxw_setDspLabel (0);
    }

/*
 * Retrieve marker info. by calling ces_get().
 * Also, get default color info.
 */
    el.hdr.vg_type = WBOX_ELM;
    el.hdr.vg_class = CLASS_WATCHES;
    ces_get( -99, &el, &ier);

    if (ier  != 0) {
        sprintf(logstr, "WATCH with subtyp %i ", -99);
        strcpy(grp, "CES");
        level = 2;
        er_lmsg ( &level, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
   	NxmErr_update();
        return;
    }

    _mrkType = (mtype == -1)    ? el.elem.wbx.info.w_mrktyp : mtype;
    _mrkSize = (G_DIFF(msize, (-1.0F))) ? el.elem.wbx.info.w_mrksiz : msize;
    _mrkWdth = (mwidth== -1)    ? el.elem.wbx.info.w_mrkwid : mwidth;

    _wbxColor= (color == -1)    ? el.hdr.maj_col : color;
    _cntyColor=(cnty_colr==-1)  ? el.hdr.min_col : cnty_colr;

    _cntyFill =(cnty_fill==-1 ) ? el.hdr.filled  : cnty_fill;

    pgwbxw_setAttr (_wbxColor, _wbxStyle, _wbxShape, 
                    _mrkType,  _mrkSize,  _mrkWdth,  
		    _cntyFill, _cntyColor );
}

/*=====================================================================*/

void pgwbxw_popdown ( void ) 
/************************************************************************
 * pgwbxw_popdown							*
 *									*
 * This function pops down watch and warning editing window.		*
 *									*
 * void pgwbxw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * S. Law/GSC		11/98	added call to NxmClrW_popdown		*
 ***********************************************************************/
{
    NxmClrW_popdown();

    if (XtIsManaged(_pgwbxW)) {
	XtUnmanageChild(_pgwbxW);
    }
}

/*=====================================================================*/

void pgwbxw_setWlst ( int location, Boolean select ) 
/************************************************************************
 * pgwbxw_setWlst							*
 *									*
 * This function saves the given element location, unless it has the	*
 * value of NO_LOCATION.  If the watchlist is up, it selects the	*
 * element and updates the list.					*
 *									*
 * void pgwbxw_setWlst (location, select)				*
 *									*
 * Input parameters:							*
 *	location	int	location of watchbox			*
 *	select		Boolean	selection flag				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/98	initial coding				*
 * S. Law/GSC		01/99	added in location reset			*
 * S. Law/GSC		01/99	moved pgwlst_update to be called more	*
 * D.W.Plummer/NCEP	 4/99	add call to pgwatch_save		*
 * S. Law/GSC           01/00   removed call to pgwatch_savecnty        *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/

    if (location == -99) {
	_currWbxLoc = NO_LOCATION;
	return;
    }

    if (location != NO_LOCATION) {
	_currWbxLoc = location;
	cvg_rdrec (cvg_getworkfile(), location, &_currWbxElm, &ier);
    }

    if (_currWbxLoc != NO_LOCATION) {
	if (pgwlst_isUp () && select) {
	    pghdlb_deselectAll ();
	    pghdlb_select (&_currWbxElm, _currWbxLoc);
	}

	pgwatch_save (&_currWbxElm);
	pgwlst_update (&_currWbxElm);
    }
}

/*=====================================================================*/

void pgwbxw_setDspLabel ( int label_number )
/************************************************************************
 * pgwbxw_setDspLabel							*
 *									*
 * This function sets the display label based on the label_number.	*
 *									*
 * void pgwbxw_setDspLabel (label_number)				*
 *									*
 * Input parameters:							*
 *	label_number	int	which label to use			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/98	initial coding				*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    switch (label_number) {
      case 0:
	xmstr = XmStringCreateLocalized ("Show Display");
	break;

      case 1:
	xmstr = XmStringCreateLocalized ("Hide Display");
	break;

      default:
	label_number = -1;
	break;
    }

    if (label_number != -1) {
	XtVaSetValues(_displayBtn, XmNlabelString, xmstr, NULL);

	XmStringFree (xmstr);
    }
}

/*=====================================================================*/

void pgwbxw_setUpWtchLn ( int watch_offset )
/************************************************************************
 * pgwbxw_setUpWtchLn							*
 *									*
 * This function sets up the product generation palatte to add a watch	*
 * line.  This is called after the user selects the parent watch for    *
 * the status line.
 *									*
 * void pgwbxw_setUpWtchLn (watch_offset)				*
 *									*
 * Input parameters:							*
 *	watch_offset	int	file offset (WORK_FILE) for parent watch*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/99	initial coding				*
 * H. Zeng/EAI          07/00   added a new para. for pgline_popup()    *
 * E. Safford/GSC       01/01   added a new param for pgline_popup()    *
 * S. Jacobs/NCEP        3/01   Added a new param for pgline_popup()    *
 * J. Wu/SAIC       	10/01   added a new param for pgline_popup()    *
 ***********************************************************************/
{
int	elmid, gemtyp, subtyp;
/*---------------------------------------------------------------------*/
/*
 *  Store the watch offset for grouping with the new line.
 */
    pgwbxw_setWtchOffset(watch_offset);

/*
 *  Switch the palatte to arrowed lines and popup the 
 *  line attribute edit box.
 */
    pgpalw_dsplyObjPal (CLASS_LINES);
    pgpalw_setCurBtns (-1, CLASS_LINES, OBJ_SPLN4); 

    pgobj_getId(CLASS_LINES, OBJ_SPLN4, &elmid, &gemtyp, &subtyp);
    pgline_popup(gemtyp, elmid, TRUE, TRUE, FALSE, FALSE, NULL, NULL, 0, FALSE);

/*
 *  Tell pgnew we're making a watch line, and arm the drawing dynamics.
 */
    pgnew_setWatchLn(TRUE);
    pgnew_setArmDynamic();
}

/*=====================================================================*/

void pgwbxw_addWtchLn ( int line_offset )
/************************************************************************
 * pgwbxw_addWtchLn							*
 *									*
 * This function groups a newly added watch line with the previously  	*
 * selected watch into GRPTYP_WATCH.                  			*
 *									*
 * void pgwbxw_addWtchLn (line_offset)   				*
 *									*
 * Input parameters:							*
 *	line_offset	int	file offset (WORK_FILE) for watch line  *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/99	initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
int	grpnum, line_idx, ier; 
int	watch_offset, watch_idx;
char	grptyp;
/*---------------------------------------------------------------------*/


    watch_offset = pgwbxw_getWtchOffset();
    crg_getinx (watch_offset, &watch_idx, &ier);

    pgwbxw_updtIssuedFlag (WATCH_WITHLINE, 
    					&watch_offset, &watch_idx, &ier);

    crg_ggrp (watch_idx, &grptyp, &grpnum, &ier);


    if (grptyp != GRPTYP_WATCH) { 

/*
 *  Watch is ungrouped -- both it and the watch line
 *  will be grouped together.
 */
        crg_ggnxt(GRPTYP_WATCH, &grpnum, &ier);

	crg_sgrp( watch_idx, GRPTYP_WATCH, grpnum, &ier);
	cvg_setginf (cvg_getworkfile(), watch_offset, GRPTYP_WATCH, grpnum, &ier);
		
	crg_getinx (line_offset, &line_idx, &ier);

	crg_sgrp( line_idx, GRPTYP_WATCH, grpnum, &ier);
	cvg_setginf (cvg_getworkfile(), line_offset, GRPTYP_WATCH, grpnum, &ier);

    }
    else { 

/*
 *  Watch is already grouped -- add the watch line to the same group. 
 */
	crg_sgrp( line_idx, GRPTYP_WATCH, grpnum, &ier);
	cvg_setginf (cvg_getworkfile(), line_offset, GRPTYP_WATCH, grpnum, &ier);
    }
}

/*=====================================================================*/

void pgwbxw_getAttr ( int *wcolr,   int *wstyle, int *wshape,
                      int *mtype, float *msize,  int *mwidth,
		      signed char *cnty_fill, int *cnty_colr )
/************************************************************************
 * pgwbxw_getAttr                                                     	*
 *                                                                      *
 * This function gets the attributes for watch box.   			*
 *                                                                      *
 * void pgwbxw_getAttr( wcolr, wstyle, wshape, mtype, msize, mwidth     *
 *			cnty_fill, cnty_colr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *			NONE						*
 *                                                                      *
 * Output parameters:                                             	*
 *	*wcolr		int		watch color			*
 *	*wstyle		int		watch style			*
 *	*wshape		int		watch shape			*
 *      *mtype		int     	watch marker type		*
 *	*msize		float		watch marker size		*
 *	*mwidth		int     	watch marker width		*
 *      *cnty_fill	signed char	county color fill flag		*
 *	*cnty_colr	int		county color index		*
 *                                                                      *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     10/97                                                 *
 * C. Lin/EAI     12/97	    get color instead of type                   *
 * C. Lin/EAI     05/98	    add style and shape                   	*
 * H. Zeng/XTRIA  01/03	    added marker info.				*
 * H. Zeng/SAIC	  10/04	    added county fill info			*
 ***********************************************************************/
{
    *wcolr	= _wbxColor;
    *wstyle	= _wbxStyle;
    *wshape	= _wbxShape;
    *mtype	= _mrkType;
    *msize	= _mrkSize;
    *mwidth	= _mrkWdth;
    *cnty_fill	= _cntyFill;
    *cnty_colr	= _cntyColor;
}

/*=====================================================================*/

void pgwbxw_setAttr ( int wcolr, int wstyle,   int wshape,
                      int mtype, float msize,  int mwidth,
		      signed char cnty_fill, int cnty_colr )
/************************************************************************
 * pgwbxw_setAttr                                                       *
 *                                                                      *
 * This function sets the attributes for watch box.                     *
 *                                                                      *
 * void pgwbxw_setAttr ( wcolr, wstyle, wshape, mtype, msize, mwidth,   *
 *			 cnty_fill, cnty_colr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wcolr		int     	watch color                     *
 *      wstyle		int     	watch style                     *
 *      wshape		int     	watch shape                     *
 *      mtype		int     	watch marker type		*
 *	msize		float   	watch marker size		*
 *	mwidth		int     	watch marker width		*
 *      cnty_fill	signed char	county fill flag		*
 *	cnty_colr	int		county fill color		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/98                                           *
 * S. Law/GSC           11/98   added shape buttons updates             *
 * S. Law/GSC           01/99   made changes to force choosing shape    *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * H. Zeng/XTRIA        01/03   added marker info.			*
 * H. Zeng/SAIC		10/04	added cnty_fill & cnty_color		*
 ***********************************************************************/
{
    unsigned long       ii;
    char                str[8];
/*---------------------------------------------------------------------*/

    if ( wcolr <= 0 || wcolr == UNDWTCH )  {
        _wbxColor  = 7;
    }
    else  {
        _wbxColor  = wcolr;
    }

    XtVaSetValues(_colorBtn,
                  XmNbackground,        NxmColrP_getColorPixel(_wbxColor),
                  XmNtopShadowColor,    NxmColrP_getColorPixel(_wbxColor),
                  XmNbottomShadowColor, NxmColrP_getColorPixel(_wbxColor),
                  NULL);

    if ((_wbxStyle = wstyle) == WBC) {
        XtSetSensitive(_shapeForm, False);
    }
    else {
        XtSetSensitive(_shapeForm, True);
    }

    _wbxShape = wshape;
    if (_wbxShape > NONE) {
	for (ii = 0L; ii < (unsigned long)MAX_SHAPES; ii++ ) {
	    if ( ii == (unsigned long)_wbxShape - 1 )
        	XmToggleButtonSetState (_shapeBtns[ii], True, False);
	    else
	      	XmToggleButtonSetState (_shapeBtns[ii], False, False);
	}
    }
    else {
        XmToggleButtonSetState (_shapeBtns[0], False, False);
        XmToggleButtonSetState (_shapeBtns[1], False, False);
        XmToggleButtonSetState (_shapeBtns[2], False, False);

        _wbxWaitFlag = True;
    }

/*
 *  Watch marker type
 */
    if ( mtype >= 1 && mtype <= TOTAL_MARKER ) {
        _mrkType = mtype;
        XtVaSetValues ( _mrkTypOptW, 
		XmNmenuHistory,		_mrkTypBtn[_mrkType - 1], 
		NULL );
    }

/*
 *  Watch marker size
 */
    if ( msize >= MIN_SIZE_SCALE && 
         msize <= ((float)MAX_SIZE_SCALE*1.0F)    ) {

         sprintf ( str, "%.1f", msize );
         XmTextFieldSetString ( _mrkSizTxtW, str );
         XmScaleSetValue ( _mrkSizSldW, (int) (msize * 10.0F) );
	 _mrkSize = msize;
    }

/* 
 *  Watch marker width
 */
    if ( mwidth >= MIN_WIDTH_SCALE && mwidth <= MAX_WIDTH_SCALE ) {

         sprintf ( str, "%i", mwidth );
         XmTextFieldSetString ( _mrkWdthTxtW, str );
         XmScaleSetValue ( _mrkWdthSldW, mwidth );
	 _mrkWdth = mwidth;
    }

/*
 * County Fill Color
 */
    if ( cnty_colr <= 0 )  {

        _cntyColor  = 3;
    }
    else  {

        _cntyColor  = cnty_colr;
    }

    XtVaSetValues(_colorBtn2,
                  XmNbackground,        NxmColrP_getColorPixel(_cntyColor),
                  XmNtopShadowColor,    NxmColrP_getColorPixel(_cntyColor),
                  XmNbottomShadowColor, NxmColrP_getColorPixel(_cntyColor),
                  NULL);

/*
 * County Fill Flag
 */
    if ( cnty_fill <= 0 ) {

         XmToggleButtonSetState (_fillBtn, False, False);
         XtSetSensitive ( _fillColrForm, False);
         XtSetSensitive ( _markerRc    , True );
    }
    else {

         XmToggleButtonSetState (_fillBtn, True,  False);
         XtSetSensitive ( _fillColrForm, True  );
         XtSetSensitive ( _markerRc    , False );
    }
}

/*=====================================================================*/

void pgwbxw_setAnchor ( int which, char *id, float flat, float flon, 
					int dis, char *dir, int *iret )
/************************************************************************
 * pgwbxw_setAnchor                                                     *
 *                                                                      *
 * This function sets the anchor attributes for watch box.   		*
 *                                                                      *
 * void pgwbxw_setAnchor ( which, id, flat, flon, dis, dir, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	which	int	which anchor point to set			*
 *	*id	char	anchor station id				*
 *	flat	float	anchor station latitude				*
 *	flon	float	anchor station longitude			*
 *	dis	int	distance (sm)					*
 *	*dir	char	direction (16-pt compass)			*
 *                                                                      *
 * Output parameters:                                             	*
 *	*iret	int	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	10/00						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;
    switch ( which )  {

	case	0:

    	    strcpy( _a0id, id);
    	    _a0lt = flat;
    	    _a0ln = flon;
    	    _a0dis = dis;
    	    strcpy( _a0dir, dir);

	    break;

	case	1:

    	    strcpy( _a1id, id);
    	    _a1lt = flat;
    	    _a1ln = flon;
    	    _a1dis = dis;
    	    strcpy( _a1dir, dir);

	    break;

    }
}

/*=====================================================================*/

void pgwbxw_getAnchor ( int which, char *id, float *flat, float *flon, 
					int *dis, char *dir, int *iret )
/************************************************************************
 * pgwbxw_getAnchor                                                     *
 *                                                                      *
 * This function gets the anchor attributes for watch box.   		*
 *                                                                      *
 * void pgwbxw_getAnchor ( which, id, flat, flon, dis, dir, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	which	int	which anchor point to set			*
 *                                                                      *
 * Output parameters:                                             	*
 *	*id	char	anchor station id				*
 *	*flat	float	anchor station latitude				*
 *	*flon	float	anchor station longitude			*
 *	*dis	int	distance (sm)					*
 *	*dir	char	direction (16-pt compass)			*
 *	*iret	int	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	10/00						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    switch ( which )  {

	case	0:

    	    strcpy( id, _a0id);
    	    *flat = _a0lt;
    	    *flon = _a0ln;
    	    *dis = _a0dis;
    	    strcpy( dir, _a0dir);

	    break;

	case	1:

    	    strcpy( id, _a1id);
    	    *flat = _a1lt;
    	    *flon = _a1ln;
    	    *dis = _a1dis;
    	    strcpy( dir, _a1dir);

	    break;

    }
}

/*=====================================================================*/

int pgwbxw_getWtchOffset ( void )
/************************************************************************
 * pgwbxw_getWtchOffset                                               	*
 *                                                                      *
 * This function gets the file offset for a watch status line.          *
 * Selection of a watch is required to draw a watch status line.  When  *
 * the watch status line is saved, it must be grouped with the watch.   *
 * This routine provides the file location (in the WORK_FILE) so that   *
 * grouping can be done.  If there is no saved location, -1 is returned.*
 *                                                                      *
 * int pgwbxw_getWtchOffset ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 * pgwbxw_getWtchOffset	int  file offset or position of the watch elem	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	12/99	initial coding                        	*
 ***********************************************************************/
{
    if (_watchOffset > 0) {	
	return (_watchOffset);
    }
    else {
	return (-1);
    }	    
}

/*=====================================================================*/

void pgwbxw_setWtchOffset ( int file_location )
/************************************************************************
 * pgwbxw_setWtchOffset                                               	*
 *                                                                      *
 * This function sets the file offset for a watch status line.          *
 *                                                                      *
 * void pgwbxw_setWtchOffset ( file_location )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	file_location	int	file offset to watch in WORK_FILE	*
 *									*
 * Output parameters:                                             	*
 *			NONE	                                 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	12/99	initial coding                        	*
 ***********************************************************************/
{
    _watchOffset = file_location;
}

/*=====================================================================*/

Boolean pgwbxw_isUp ( void )
/************************************************************************
 * pgwbxw_isUp                                                        	*
 *                                                                      *
 * This function returns the status of the watch box attribute window.  *
 *                                                                      *
 * Boolean pgwbxw_isUp ( )                                         	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *			NONE	                                 	*
 * Return:								*
 *	Boolean		True if _pgwbxW is managed			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	2/01	initial coding                        	*
 ***********************************************************************/
{
    return ( XtIsManaged( _pgwbxW ) );
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_shapeCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgwbxw_shapeCb                                                     	*
 *                                                                      *
 * This function is the callback for parallelogram shape.       	*
 *                                                                      *
 * void pgwbxw_shapeCb(w, which, call)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which color                                *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	 5/98                                           *
 * D.W.Plummer/NCEP	 9/98	add _wbxShapeSv				*
 * S. Law/GSC		01/99	made changes to force choosing shape	*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
	
    switch( which ) {

      case 0:	/* NS */
	_wbxShape   = NS;
	break;

      case 1:	/* EW */
	_wbxShape   = EW;
	break;

      case 2:	/* ESOL */
	_wbxShape   = ESOL;
	break;
    }

    if (_wbxWaitFlag && _wbxShape != NONE) {
	_wbxWaitFlag = False;

	pgnew_setArmDynamic();
	mbotw_mouseSet(LMHINT_START, MMHINT_DONE);
    }

    for (ii = 0; ii < MAX_SHAPES; ii++ ) 
	XmToggleButtonGadgetSetState (_shapeBtns[ii], False, False);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_colorCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwbxw_colorCb							*
 *									*
 * Callback for color button widgets.					*
 *									*
 * void pgwbxw_colorCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		color			*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		11/98	Copied from pgline			*
 * H. Zeng/SAIC		10/04	extended for fill color			*
 ***********************************************************************/
{
    if ( clnt == (XtPointer)0 ) {

       NxmClrW_popup (wid, (XtPointer)&_wbxColor, call);
    }
    else if ( clnt == (XtPointer)1 ) {

       NxmClrW_popup (wid, (XtPointer)&_cntyColor, call);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_displayCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwbxw_displayCb							*
 *									*
 * Callback for display button widget					*
 *									*
 * void pgwbxw_displayCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		color			*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		12/98	inital coding				*
 * D.W.Plummer/NCEP	 3/00	rm extraneous call to pgwbxw_setWlst	*
 * H. Zeng/EAI          08/00   added a new para. to pgwlst_popup()     *
 ***********************************************************************/
{
    if (pgwlst_isUp ()) {
	pgwlst_popdown ();

	pgwbxw_setDspLabel (0);
    }
    else {
	pgwlst_popup (FALSE);

	pgwbxw_setDspLabel (1);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_waitEh ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgwbxw_waitEh							*
 *									*
 * Callback for display button widget					*
 *									*
 * void pgwbxw_waitEh (wid, clnt, event, ctdr )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	clnt		XtPointer	not used			*
 *	*event		XEvent		event structure			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		01/99	inital coding				*
 ***********************************************************************/
{
    if (event->xbutton.button == Button1) {
	NxmWarn_show (wid, "You need to choose a shape before drawing.");
    }
    else {
	pgevt_unsetOper (FALSE);
    }
}

/*=====================================================================*/

void pgwbxw_updtIssuedFlag ( int value, int *watch_offset, int *new_idx, 
								int *iret )
/************************************************************************
 * pgwbxw_updtIssuedFlag						*
 *									*
 * This routine is not documented.					*
 *									*
 * void pgwbxw_updtIssuedFlag (value, watch_offset, new_idx, iret)	*
 *									*
 * Input parameters:							*
 *	value		int	flag value				*
 *									*
 * Input/Output parameters:						*
 *	*watch_offset	int	file offset for watch element		*
 *									*
 * Output parameters:							*
 *	*new_idx	int	new element index			*
 *	*iret		int	return code   0 = normal		*
 *					     -1 = bad offset    	*
 *					     -2 = error in delete/save  *
 *					      1 = no change made	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	12/99	inital coding				*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
int		old_idx, np, location, layer, ier, el_layer, found;
int		update_crg;
float		llx, lly, urx, ury, inf_bbox[4];
float		o_llx, o_lly, o_urx, o_ury;
float		*dx, *dy;
VG_DBStruct	el;
filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;

    update_crg = 0;
 
    crg_getinx (*watch_offset, &old_idx, &ier);
    if (ier < 0) {
	*iret = -1;
	return;
    }

    cvg_rdrec(cvg_getworkfile(), *watch_offset, &el, &ier);
    if (ier < 0) {
	*iret = -1;
	return;
    }

    if (el.elem.wbx.info.w_issued == value) {
        crg_getinx( *watch_offset, new_idx, &ier);
	*iret = 1;
	return;
    }

/*
 *  Change the flag value by deleting the watch and resaving it.
 */
    pgactv_setActvElm ( &el, *watch_offset);
    pgactv_getDevPts (&np, &dx, &dy); 

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
    cvg_rdrec(cvg_getworkfile(), *watch_offset, &el, &ier);
    cvg_checkplace(&el, 1, *watch_offset, &found, inf_bbox, &ier);
    if (found > 0) {
/*
 * Update the refresh extent if the area impacted by placement is bigger
 */
        o_llx = inf_bbox[0];
        o_lly = inf_bbox[2];
        o_urx = inf_bbox[1];
        o_ury = inf_bbox[3];
        update_crg = 1;
    }

    cvg_delet(cvg_getworkfile(), *watch_offset, TRUE, &ier);
    if (ier < 0) {
 	*iret = -2;
	return;
    }
    crg_clear (old_idx, &ier);

/*
 *  modify and resave the watch
 */    
    el.elem.wbx.info.w_issued = value;

    pgvgf_saveNewElm (NULL, sys_D, &el, np, dx, dy, FALSE, &location, &ier);
    if (ier < 0) {
	*iret = -2;
	return;
    }
   
    cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
    layer = pglayer_getCurLayer( );
    crg_set (&el, location, layer, &ier);
    pgactv_setActvElm (&el, location); 

    crg_getinx( location, new_idx, &ier);
    crg_get (*new_idx, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
    if (!found) {
        o_llx = llx;
        o_lly = lly;
        o_urx = urx;
        o_ury = ury;
    }

/*
 * Mark elements in placement that are effected by
 * the change, and get the area of influence back
 */
    cvg_checkplace(&el, 0, location, &found, inf_bbox, &ier);
    if (found > 0) {

/*
 * Update the refresh extent if the area impacted by placement is bigger
 */
        o_llx = G_MIN(o_llx, inf_bbox[0]);
        o_lly = G_MIN(o_lly, inf_bbox[2]);
        o_urx = G_MAX(o_urx, inf_bbox[1]);
        o_ury = G_MAX(o_ury, inf_bbox[3]);
        update_crg = 1;
    }
    if ( o_llx < llx )
        llx = o_llx;
    if ( o_lly < lly )
        lly = o_lly;
    if ( o_urx > urx )
        urx = o_urx;
    if ( o_ury > ury )
        ury = o_ury;

    xpgpaste (llx, lly, urx, ury, &ier);

    cvg_rfrsh(NULL, llx, lly, urx, ury, &ier);

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
    if (update_crg) {
        crg_rebuild();
    }
    *watch_offset = location;
}

/*=====================================================================*/

void pgwbxw_crtMrkTyp ( Widget parent )
/************************************************************************
 * pgwbxw_crtMrkTyp							*
 *									*
 * This function creates the area to select a watch marker type. 	*
 *									*
 * void pgwbxw_crtMrkTyp ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * H. Zeng/XTRIA        01/03   copied from pglist_crtMrkTyp            *
 ***********************************************************************/
{
    Widget	type_form, pulldown;
    int		iret;
    long	ii, ignore;
    Pixel	fg, bg;
    Pixmap	mark_pxm[TOTAL_MARKER];
    XmString	xmstr;
    char	filename[256], warning[200], type_id[100];
    char	*type_list[] = { "marker1.xbm",  "marker2.xbm",  "marker3.xbm",
			 "marker4.xbm",  "marker5.xbm",   "marker6.xbm",
			 "marker7.xbm",  "marker8.xbm",   "marker9.xbm",
			 "marker10.xbm", "marker11.xbm",  "marker12.xbm",
			 "marker13.xbm", "marker14.xbm",  "marker15.xbm",
			 "marker16.xbm", "marker17.xbm",  "marker18.xbm",
			 "marker19.xbm", "marker20.xbm",  "marker21.xbm" };

/*---------------------------------------------------------------------*/

    _mrkTypBtn = (WidgetList) XtMalloc ( TOTAL_MARKER * sizeof(Widget) );

/*
 * create type selection option menu
 */
    type_form = XtVaCreateManagedWidget ( "type_formW",
		xmFormWidgetClass,	parent,
		NULL ); 

    pulldown = XmCreatePulldownMenu ( type_form, " ", NULL, 0);

    _mrkTypOptW = XmCreateOptionMenu( type_form, " ", NULL, 0);

    XtVaSetValues ( pulldown, 
		XmNorientation,		XmHORIZONTAL,
		XmNpacking,		XmPACK_COLUMN,
		XmNnumColumns,		7,
		NULL );
 
    XtVaGetValues ( type_form,
		XmNforeground,		&fg,
		XmNbackground,		&bg,
		NULL );

    for ( ii = 0; ii < TOTAL_MARKER; ii++ ) {
        sprintf ( type_id, "%ld", (ii+1) );
        cfl_inqr ( type_list[ii], ICON_DIR, &ignore, filename, &iret );

        mark_pxm[ii] = XmGetPixmap ( XtScreen(parent),
					filename, fg, bg );

        if ( mark_pxm[ii] == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
            sprintf( warning, "cannot load pixmap file %s",
					filename );
            NxmWarn_show ( parent, warning );
            _mrkTypBtn[ii] = XtVaCreateManagedWidget( type_id, 
		xmPushButtonWidgetClass,	pulldown,
		NULL );
        }
        else {
            _mrkTypBtn[ii] = XtVaCreateManagedWidget ( type_id, 
		xmPushButtonWidgetClass,	pulldown,
		XmNlabelType,			XmPIXMAP,
		XmNlabelPixmap,			mark_pxm[ii],
		NULL );
        }
        
        XtAddCallback ( _mrkTypBtn[ii], XmNactivateCallback, 
			(XtCallbackProc)pgwbxw_mrkTypCb, (XtPointer)ii ); 
    }

    xmstr = XmStringCreateLocalized("Type:   ");
    XtVaSetValues ( _mrkTypOptW, 
		XmNsubMenuId,		pulldown,
		XmNmenuHistory,		_mrkTypBtn[0], 
		XmNlabelString, 	xmstr, 
		NULL );
    XmStringFree(xmstr);

    XtManageChild ( _mrkTypOptW );
}

/*=====================================================================*/

void pgwbxw_crtSzWth ( Widget parent )
/************************************************************************
 * pgwbxw_crtSzWth							*
 *									*
 * This function creates watch marker size and width input panel.	*
 *									*
 * void pgwbxw_crtSzWth ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA        01/03   copied from pgwbxw_crtSzWth		*
 ***********************************************************************/
{
    Widget	size_form, width_form;
    XmString	xmstr;
/*---------------------------------------------------------------------*/
/*
 * create width and size input
 */
    size_form = XtVaCreateManagedWidget ( "size_formW",
		xmFormWidgetClass,	parent,
		NULL);

    _mrkSizSldW = (Widget)XmCreateScale ( size_form, "size", NULL, 0 );
    XtManageChild( _mrkSizSldW );
    xmstr = XmStringCreateLocalized( "Size" );
    XtVaSetValues( _mrkSizSldW,
		XmNorientation,             	XmHORIZONTAL,
		XmNminimum,                 	(int) (MIN_SIZE_SCALE * 10.0F),
		XmNmaximum,                 	(int) ((float)MAX_SIZE_SCALE * 10.0F),
		XmNprocessingDirection,     	XmMAX_ON_RIGHT,
		XmNvalue,                   	(int) (10.0F),
		XmNshowValue,               	False,
		XmNtitleString,             	xmstr,
		XmNtopAttachment,           	XmATTACH_FORM,
		XmNleftAttachment,          	XmATTACH_FORM,
		NULL );
    XmStringFree(xmstr);
 
    XtAddCallback ( _mrkSizSldW, XmNdragCallback, 
		    (XtCallbackProc)pgwbxw_sizSldCb, NULL);
    XtAddCallback ( _mrkSizSldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgwbxw_sizSldCb, NULL);


    _mrkSizTxtW = XtVaCreateManagedWidget ( "mrk_size",
		xmTextFieldWidgetClass,		size_form,
		XmNcolumns,			6,
		XmNvalue,			"1.0",
                XmNeditable,                    TRUE,
		XmNcursorPositionVisible,	TRUE,
		XmNleftAttachment,		XmATTACH_WIDGET,
                XmNleftWidget,                  _mrkSizSldW,
		NULL );

    XtAddCallback ( _mrkSizTxtW, XmNlosingFocusCallback, 
		    (XtCallbackProc)pgwbxw_sizTxtCb, NULL ); 

/*
 * create width input
 */   
    width_form = XtVaCreateManagedWidget ( "widthformW",
		xmFormWidgetClass, parent,
		NULL );

    _mrkWdthSldW = (Widget)XmCreateScale ( width_form, "width", NULL, 0);
    XtManageChild ( _mrkWdthSldW );
    xmstr = XmStringCreateLocalized ( "Width" );
    XtVaSetValues ( _mrkWdthSldW,
		XmNorientation,		XmHORIZONTAL,
		XmNminimum,		MIN_WIDTH_SCALE,
		XmNmaximum,		MAX_WIDTH_SCALE,
		XmNprocessingDirection,	XmMAX_ON_RIGHT,
		XmNvalue,		3,
		XmNscaleMultiple,	1,
		XmNshowValue,		False,
		XmNtitleString,		xmstr,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		NULL );
    XmStringFree(xmstr);
   
    XtAddCallback ( _mrkWdthSldW, XmNdragCallback, 
		    (XtCallbackProc)pgwbxw_wdthSldCb, NULL);
    XtAddCallback ( _mrkWdthSldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgwbxw_wdthSldCb, NULL);

    _mrkWdthTxtW = 
	XtVaCreateManagedWidget ( "mrk_width",
		xmTextFieldWidgetClass,		width_form,
		XmNcolumns,	                6,
		XmNvalue,			"3",
                XmNeditable,                    TRUE,
		XmNcursorPositionVisible,	TRUE,
		XmNleftAttachment,		XmATTACH_WIDGET,
                XmNleftWidget,                  _mrkWdthSldW,
		NULL );

    XtAddCallback ( _mrkWdthTxtW, XmNlosingFocusCallback, 
		    (XtCallbackProc)pgwbxw_wdthTxtCb, NULL ); 
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_mrkTypCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgwbxw_mrkTypCb							*
 *									*
 * Callback for the marker type selection.				*
 *									*
 * void pgwbxw_mrkTypCb ( w, which, call )				*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	which	   long		widget button				*
 *	call	   XtPointer	resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/03   initial coding				*
 ***********************************************************************/
{   
    _mrkType = (int)which + 1;
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_sizSldCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgwbxw_sizSldCb                                                      *
 *                                                                      *
 * Callback for pattern size scale widget.                              *
 *                                                                      *
 * void pgwbxw_sizSldCb ( w, clnt, cbs )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *   w			Widget			Widget ID               *
 *   clnt		XtPointer		not used                *
 *   *cbs		XmScaleCallbackStruct	callback struct         *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/03	initial coding				*
 ***********************************************************************/
{
    float	mrksiz;		/* marker size */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    mrksiz = (float)cbs->value / 10.0F;
    sprintf ( txtstr, "%.1f", mrksiz );
    XmTextFieldSetString ( _mrkSizTxtW, txtstr );
    _mrkSize = mrksiz;
   
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_sizTxtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwbxw_sizTxtCb                                                      *
 *                                                                      *
 * Callback for mrker size text widget.					*
 *                                                                      *
 * void pgwbxw_sizTxtCb( w, clnt, call )			        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w             	Widget         	  	Widget ID               *
 *   clnt		XtPointer         	not used                *
 *   call		XtPointer		callback struct         *
 *                                                                      *
 * Output parameters:							*
 *		    none						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/03   initial coding				*
 * H. Zeng/XTRIA	12/03	changed the range of marker size	*
 ***********************************************************************/
{
    char        *sizestr, newstr[8];
    int         lenstr, ier;
    float	mrksiz;
/*---------------------------------------------------------------------*/

    sizestr = XmTextFieldGetString (_mrkSizTxtW);
    sscanf (sizestr, "%f", &mrksiz);
    XtFree (sizestr);

    if (0.1F <= mrksiz && mrksiz <= 10.0F) {
	sprintf (newstr, "%-5.1f", mrksiz);
	_mrkSize = mrksiz;
    }
    else {
	sprintf (newstr, "%-5.1f", _mrkSize);
    }

    cst_rmbl ( newstr, newstr, &lenstr, &ier );
    XmTextFieldSetString (_mrkSizTxtW, newstr);
  
/*
 * Set the Size Slider value accordingly.
 */
    XmScaleSetValue ( _mrkSizSldW, (int)(_mrkSize * 10.0F) );

}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_wdthSldCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgwbxw_wdthSldCb                                                     *
 *                                                                      *
 * Callback for marker width scale widget.                              *
 *                                                                      *
 * void pgwbxw_wdthSldCb ( w, clnt, cbs )		        	*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              	Widget          	Widget ID               *
 *   clnt		XtPointer       	not used                *
 *   *cbs		XmScaleCallbackStruct	callback struct         *
 *                                                                      *
 * Output parameters:							*
 *		    none						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/03   initial coding				*
 ***********************************************************************/
{
    int 	mrkwid;		/* marker width */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    mrkwid = cbs->value;
    sprintf ( txtstr, "%i", mrkwid );
    XmTextFieldSetString ( _mrkWdthTxtW, txtstr );
    _mrkWdth = mrkwid;
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_wdthTxtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwbxw_wdthTxtCb                                                     *
 *                                                                      *
 * Callback for mrker width text widget.				*
 *                                                                      *
 * void pgwbxw_wdthTxtCb( w, clnt, call )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              	Widget            	Widget ID       	*
 *   clnt		XtPointer         	not used        	*
 *  call		XtPointer		callback struct 	*
 *                                                                      *
 * Output parameters:							*
 *		    none						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/03   initial coding				*
 * H. Zeng/XTRIA	12/03	changed the range of marker width	*
 ***********************************************************************/
{
    char        *wdthstr, newstr[8];
    int         mrkwid, lenstr, ier;
/*---------------------------------------------------------------------*/

    wdthstr = XmTextFieldGetString (_mrkWdthTxtW);
    sscanf (wdthstr, "%d", &mrkwid);
    XtFree (wdthstr);

    if (1 <= mrkwid && mrkwid <= 10) {
	sprintf (newstr, "%-4d", mrkwid);
	_mrkWdth = mrkwid;
    }
    else {
	sprintf (newstr, "%-4d", _mrkWdth);
    }

    cst_rmbl ( newstr, newstr, &lenstr, &ier );
    XmTextFieldSetString (_mrkWdthTxtW, newstr);
  
/*
 * Set the Size Slider value accordingly.
 */
    XmScaleSetValue ( _mrkWdthSldW, _mrkWdth );
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_fillBtnCb ( Widget wid, XtPointer clnt, 
				    XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * mapw_fillBtnCb							*
 *									*
 * Callback function for "Use Fill" button.				*
 *									*
 * void mapw_fillBtnCb (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget				widget ID		*
 *	clnt	XtPointer			button index		*
 *	*cbs	XmToggleButtonCallbackStruct	callback structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		10/04	initial coding				*
 * H. Zeng/SAIC		11/04	added call to Nxm_ClrW_popdown2		*
 ***********************************************************************/
{
    _cntyFill = (Boolean)cbs->set ?  1  :  0;

    if ( _cntyFill == 1 ) {

         XtSetSensitive ( _fillColrForm, True );
         XtSetSensitive ( _markerRc    , False);
    }
    else {

         XtSetSensitive ( _fillColrForm, False);
         XtSetSensitive ( _markerRc    , True );

	 NxmClrW_popdown2 ( _colorBtn2);
    }
}

/*=====================================================================*/

void pgwbxw_updtWcnFlag ( int watch_offset )
/************************************************************************
 *                                                                      *
 * pgwbxw_updtWcnFlag							*
 *                                                                      *
 * This function pops up dialog box asking the user if he wants to 	*
 * automatic update of active counties in the watch.  The box pops up   *
 * only if USE_WCN_WSM flag in prefs.tbl set to TRUE.			*
 *                                                                      *
 * void pgwbxw_updtWcnFlag (watch_offset)                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      watch_offset    int     file offset (WORK_FILE) for parent watch*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 11/04   initial coding                          *
 ***********************************************************************/
{
int      ier;
char     prefs_tag[12];
Boolean  usezn;
Widget   dialog, dialog_area;
XmString msg, yes, no;
/*---------------------------------------------------------------------*/
/*
 *  Store the watch offset for grouping with the new line
 */
    pgwbxw_setWtchOffset(watch_offset);
    _wtch_offset = watch_offset;

/*
 *  Get the value for USE_WCN_WSM flag from the prefs.tbl
 */
    strcpy (prefs_tag, "USE_WCN_WSM");
    ctb_pfbool (prefs_tag, &usezn, &ier);

/*
 *  Pop up dialog box if the flag is TRUE
 */
    if ( usezn == TRUE ) {
       _wtch_offset = watch_offset;

       dialog_area = (Widget)mcanvw_getDrawingW();
       dialog = XmCreateQuestionDialog (dialog_area, "county-auto-update", NULL, 0);
       yes = XmStringCreateLocalized ("Yes");
       no = XmStringCreateLocalized ("No");
       msg = XmStringCreateLocalized ("Do you want an automatic update to the active counties in the watch?");

       XtVaSetValues ( dialog,
                       XmNmessageString,       msg,
                       XmNokLabelString,       yes,
                       XmNcancelLabelString,   no,
                       XmNdialogStyle,         XmDIALOG_APPLICATION_MODAL,
                       XmNdefaultPosition,     FALSE,
                       XmNautoUnmanage,        False,
                       XmNx,                   450,
                       XmNy,                   300,
                       NULL);

       XtAddCallback ( dialog,
                       XmNokCallback,
                       (XtCallbackProc)pgwbxw_updtYesNoCb,
                       "YES");
       XtAddCallback ( dialog,
                       XmNcancelCallback,
                       (XtCallbackProc)pgwbxw_updtYesNoCb,
                       "NO");

       XmStringFree (yes);
       XmStringFree (no);
       XmStringFree (msg);

       XtUnmanageChild( XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON) );
       XtManageChild (dialog);

    }

/*
 *  Do not pop up dialog box if the flag is FALSE
 */
    else {
       pgwbxw_setUpWtchLn (_wtch_offset);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwbxw_updtYesNoCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwbxw_updtYesNoCb 							*
 *                                                                      *
 * Callback function for the control buttons.                           *
 *                                                                      *
 * void pgwbxw_updtYesNoCb (wid, clnt, call)                		*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid		Widget          Widget ID                       *
 *   clnt		XtPointer	client data			*
 *   call		XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 11/04   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    char *message = (char *)clnt;
    if ( strcmp (message, "YES") == 0) { /* button 'yes' */
        pglist_updtStatusCntys (_wtch_offset);
        pgwbxw_setUpWtchLn (_wtch_offset);
    }
    else {                               /* button 'no' */
        pgwbxw_setUpWtchLn (_wtch_offset);
    }
}

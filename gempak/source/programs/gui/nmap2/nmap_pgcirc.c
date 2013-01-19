#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "hints.h"

#define MAX_WIDTH_SCALE		10
#define MAX_LABEL_TYPES		14

static Widget	_circles_dlgW;
static Widget   _circ_width_sldW;
static Widget	_circ_width_txtW;

static Widget   _labelReminder_formW;
static Widget   _labelReminder_textW;

static Widget	_label_formW;
static Widget	_label_optW;
static Widget	_label_pbW[MAX_LABEL_TYPES];
static Widget	_label_toggW, _label_toggW2;
static Widget	_label_menuW, _label_menuW2;
static Widget	_label_submenuW;

static Widget	  _circleColrW;

static Widget	  _ctlBb;
static WidgetList _ctlBtns;

static char	_labelFlag, _labelColorFlag;
static Boolean	_ghostFlag = FALSE;

static int	_attrColr;	
static int	_attrWdth;

static float	_Lat1 = -9999.0F;
static float	_Lon1 = -9999.0F;
static float	_Lat2 = -9999.0F;
static float	_Lon2 = -9999.0F;

static char	_vgType;
static int	_subTyp;

static char	_labelName[10];

void		(*_updateGhostCirc) (VG_DBStruct *el);


/*
 * private callback functions
 */
void pgcirc_colorCb     ( Widget, XtPointer, XtPointer );
void pgcirc_labelPbCb   ( Widget, long, XtPointer );
void pgcirc_labelToggCb ( Widget, XtPointer, XtPointer );
void pgcirc_widthCb     ( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgcirc_widTxtCb    ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );

/*
 *  private functions
 */
void pgcirc_fillElement ( VG_DBStruct *el);
void pgcirc_setLabValue ( int which );
void pgcirc_setTable ( void );


/************************************************************************
 * nmap_pgcirc.c							*
 *									*
 * This module creates and displays the VG circle setting box. It also  *
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pgcirc_create()	create circle attribute editing window		*
 *  pgcirc_popup()	pop up circle attribute editing window		*
 *  pgcirc_popdown()	pop down circle attribute editing window	*
 *  pgcirc_saveAttr()	save the circle attributes			*
 *  pgcirc_setAttr()	set the circle attributes			*
 *  pgcirc_setLabFlag() set the label flag				*
 *  pgcirc_setGhostFlag() set the circle ghosting flag			*
 *  pgcirc_updateGstCirc() updates the circle for ghosting		*
 *  pgcirc_setLocation() sets the circle coordinate position		*
 *									*
 *  pgcirc_isUp()	query whether the window is up			*
 *  pgcirc_getAttr()	query the circle attributes			*
 *  pgcirc_getLabFlag() query the saved label flag			*
 *  pgcirc_getLabColorFlag() query the label color flag                 *
 *  pgcirc_getLabValue() query the saved label name text value		*
 *  pgcirc_getLocation() gets the circle coordinate position		*
 *  pgcirc_getGrptyp()  gets the circle coordinate position		*
 *									*
 *  pgcirc_widthCb()	callback for width scale			*
 *  pgcirc_widTxtCb()	callback for width text widget			*
 *  pgcirc_colorCb()	callback for color button			*
 *  pgcirc_labelPbCb()	Callback for circle label widget		*
 *  pgcirc_labelToggCb	Callback for circle label toggle button widget	*
 *									*
 *  pgcirc_setTable()	places element into set table			*
 *  pgcirc_setLabValue	put the selected label name into text editor	*
 *  pgcirc_fillElement	fills the VG_DBStruct				*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void pgcirc_create ( Widget parent )
/************************************************************************
 * pgcirc_create							*
 *									*
 * This function creates a Circles attribute selection box.		*
 *									*
 * void pgcirc_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_create		*
 * S. Law/GSC		11/99	changed label names			*
 * H. Zeng/EAI          02/00   did minor changes to the appearance     *
 * H. Zeng/EAI          06/00   added label color toggle                *
 * H. Zeng/EAI          07/00   added label reminder text widget        *
 * J. Wu/SAIC           05/02   verify input to circles' width		*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget	bb, rc2, width_form, label_form, colrlbl;
    Widget	label_menubar, labelReminder_label;
    Widget	pane;
    XmString	xmstr;
    Arg         args[10];
    long	ii, nn;
    char	*btnstr[] = {"Apply", "Cancel"};
    char	cc[10];
    char	*label_name[] = {"No Label", ".01", ".10", ".25", ".50", 
				  "1", "1.5", "2", "2.5", "3", "4", "5",
				  "6", "Other"} ;
/*---------------------------------------------------------------------*/

    /*
     *  Create the VG circle selection box.
     */
    _circles_dlgW = XmCreateFormDialog ( parent, "circles_edit",
				      NULL, 0 );
    XtVaSetValues(_circles_dlgW,
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	FALSE,
		  NULL);

    xmstr = XmStringCreateLocalized("Circle Attributes");
    XtVaSetValues( _circles_dlgW, XmNdialogTitle, xmstr, NULL);
    XmStringFree(xmstr);

    pane  = XtVaCreateManagedWidget ("pane",
				     xmPanedWindowWidgetClass,	_circles_dlgW,
				     XmNsashWidth,		1,
				     XmNsashHeight,		1,
				     NULL); 

    /*
     * create color input
     */

    bb = XtVaCreateWidget( "pg_circles_bb",
			  xmBulletinBoardWidgetClass,	pane,
			  NULL);
    colrlbl = XtVaCreateManagedWidget ("Color:",
		xmLabelGadgetClass,        	bb,
		XmNx,                      	10,
		XmNy,                      	15,
                NULL);
    XtManageChild (colrlbl);

    /*
     * create color widget
     */

    _circleColrW	= XtVaCreateManagedWidget(" ",
					  xmPushButtonWidgetClass,	bb,
					  XmNwidth,			25,
					  XmNheight,			20,
					  XmNx,				180,
					  XmNy,				15,
					  NULL);

    XtAddCallback(_circleColrW, XmNactivateCallback, (XtCallbackProc)pgcirc_colorCb, 
		  NULL);
    XtManageChild(bb);

    /*
     * create width input
     */

    rc2 = XtVaCreateWidget("Attribs",
			   xmRowColumnWidgetClass, 	pane,
			   XmNorientation, 		XmVERTICAL,
			   XmNradioAlwaysOne,		FALSE,
			   NULL);

    width_form = XtVaCreateManagedWidget("_circ_width_formW",
					 xmFormWidgetClass,
					 rc2, NULL);

    _circ_width_txtW = 
	XtVaCreateManagedWidget("circ_width",
				xmTextFieldWidgetClass,		width_form,
				XmNcolumns,	                4,
				XmNvalue,			"2",
				XmNcursorPositionVisible,	True,
				XmNrightAttachment,		XmATTACH_FORM,
				NULL);

    XtAddCallback(_circ_width_txtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback(_circ_width_txtW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgcirc_widTxtCb, NULL);

    _circ_width_sldW=(Widget)XmCreateScale(width_form, "width", NULL, 0);
    XtManageChild( _circ_width_sldW);
    xmstr = XmStringCreateLocalized("Width");
    XtVaSetValues( _circ_width_sldW,
		  XmNorientation,		XmHORIZONTAL,
		  XmNmaximum,			MAX_WIDTH_SCALE,
		  XmNminimum,			1,
		  XmNprocessingDirection,	XmMAX_ON_RIGHT,
		  XmNvalue,			1,
		  XmNscaleMultiple,		1,
		  XmNshowValue,			False,
		  XmNtitleString,		xmstr,
		  XmNtopAttachment,		XmATTACH_FORM,
		  XmNleftAttachment,		XmATTACH_FORM,
		  XmNrightAttachment,		XmATTACH_WIDGET,
		  XmNrightWidget,		_circ_width_txtW,
		  NULL);
    XmStringFree(xmstr);
    XtAddCallback(_circ_width_sldW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgcirc_widthCb, NULL);
    XtAddCallback(_circ_width_sldW, XmNdragCallback, 
		  (XtCallbackProc)pgcirc_widthCb, NULL);

    XtManageChild(rc2);


    /*
     * Create label reminder
     */
    _labelReminder_formW = 
             (Widget)XtVaCreateWidget ("_label_reminder_formW",
		     xmFormWidgetClass,		pane,
	 	     NULL);

    labelReminder_label  = 
             (Widget)XtVaCreateManagedWidget ("Label:  ",
		     xmLabelGadgetClass,     _labelReminder_formW,
                     XmNtopAttachment,       XmATTACH_FORM,
                     XmNtopOffset,           10,
		     XmNleftAttachment,	     XmATTACH_FORM,
                     XmNleftOffset,          3,
		     NULL);

    nn = 0;
    XtSetArg(args[nn], XmNrows,                         1); nn++;
    XtSetArg(args[nn], XmNcolumns,                     13); nn++;
    XtSetArg(args[nn], XmNcursorPositionVisible,    False); nn++;
    XtSetArg(args[nn], XmNresizeHeight,              True); nn++;
    XtSetArg(args[nn], XmNeditable,                 False); nn++;
    XtSetArg(args[nn], XmNeditMode,     XmMULTI_LINE_EDIT); nn++; 

    _labelReminder_textW =  
             (Widget)XmCreateScrolledText(_labelReminder_formW, 
                                       "Label", args, nn);

    XtVaSetValues( XtParent(_labelReminder_textW),
		XmNleftAttachment,	 XmATTACH_WIDGET,
		XmNleftWidget,           labelReminder_label,
		NULL);

    XtManageChild(_labelReminder_textW);


    /*
     * create label widget input
     */
    _label_formW = XtVaCreateWidget ("form", 
			 xmFormWidgetClass, 	pane,  
			 NULL);

    label_form = XtVaCreateManagedWidget("_label_formW",
				 xmFormWidgetClass,	_label_formW,
				 NULL);

    _label_menuW = XtVaCreateManagedWidget ("_label_menu",
	 xmRowColumnWidgetClass,	label_form,
         XmNtopAttachment,              XmATTACH_FORM, 
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW = XtVaCreateManagedWidget("  ",
	 xmToggleButtonGadgetClass,	_label_menuW,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgcirc_labelToggCb, (XtPointer)0 );

    XtVaCreateManagedWidget ("Label:",
	 xmLabelGadgetClass,		_label_menuW,
	 NULL); 

    _label_submenuW = XtVaCreateManagedWidget("_circ_lab_submenu",
	 xmRowColumnWidgetClass,	_label_menuW,
	 NULL);

    label_menubar = XmCreatePulldownMenu (_label_submenuW, 
					  "Label", NULL, 0);

    _label_optW = XmCreateOptionMenu (_label_submenuW, 
					  "label", NULL, 0);

    for (ii=0; ii < MAX_LABEL_TYPES; ii++) {
	sprintf (cc, "%s", label_name[ii]);
        xmstr = XmStringCreateLocalized (cc);
        _label_pbW[ii] = XtVaCreateManagedWidget(cc,
	     xmPushButtonWidgetClass,	    label_menubar,
	     NULL);
	XtVaSetValues(_label_pbW[ii],
	    XmNlabelString, 		xmstr,
	    XmNalignment,		XmALIGNMENT_CENTER,
	    NULL);
        XmStringFree (xmstr);
        XtAddCallback(_label_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgcirc_labelPbCb, (XtPointer) ii);
    }

    XtVaSetValues (_label_optW, 
	XmNsubMenuId,			label_menubar,
	XmNmenuHistory,			_label_pbW[0], 
	NULL);

    XtManageChild (_label_optW);

    /*
     * create use CIRCLE color toggle
     */
    _label_menuW2 = XtVaCreateManagedWidget ("_label_color_menu",
	 xmRowColumnWidgetClass,	label_form,
         XmNtopAttachment,              XmATTACH_WIDGET,
         XmNtopWidget,                  _label_menuW, 
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW2 = XtVaCreateManagedWidget("  ",
	 xmToggleButtonGadgetClass,	_label_menuW2,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW2, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgcirc_labelToggCb, (XtPointer)1 );

    XtVaCreateManagedWidget ("use CIRCLE color",
	 xmLabelGadgetClass,		_label_menuW2,
	 NULL); 

    XtManageChild (_label_formW);

    _ctlBb = (Widget)XtVaCreateManagedWidget("_circ_ctl_bbW",
	xmBulletinBoardWidgetClass,	pane,
        XmNheight,                      50,
        XmNwidth,                       200,
	NULL);

    _ctlBtns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));

    NxmCtlBtn_create (_ctlBb, 1, "ctlBtns", XtNumber(btnstr), btnstr,
							NULL, _ctlBtns);


    /* 
     * Initialize member variables 
     */
    _attrColr	= 0;
    _attrWdth	= 1;
    _labelFlag	= 0; 
    _labelColorFlag = 0;

    return;
}

/*=====================================================================*/

void pgcirc_popup ( int subtyp, int eltyp, int show_width, int show_ctl, 
				XtCallbackProc callback, char *label_ptr)
/************************************************************************
 * pgcirc_popup								*
 *									*
 * This function shows a VG circle drawing attribute box.		*
 *									*
 * pgcirc_popup( subtyp,eltyp,show_width,show_ctl,callback,label_ptr)	*
 *									*
 * Input parameters:							*
 *   subtyp	int		GEMPAK subtype code			*
 *   eltyp	int		element type 				*
 *   show_width	int		show width flag 			*
 *   show_ctl 	int		show control buttons flag		*
 *   callback	XtCallbackProc	callback function for Apply/Cancel btns	*
 *   label_ptr  char*           pointer to the possible label string    *
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_popup		*
 * H. Zeng/EAI          06/00   added label color toggle initialization *
 * H. Zeng/EAI          07/00   added a new para. label_ptr             *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * M. Li/SAIC		04/02	Added pglabel_setLabelPending		*
 * M. Li/SAIC		05/02	Removed the check for the active Group	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    VG_DBStruct		el;
    int			ier;
    long		ii;
    /*---------------------------------------------------------------------*/

    if (XtIsManaged (_circles_dlgW)) {
	XtUnmanageChild (_circles_dlgW);
    }

    XtSetSensitive(_circ_width_sldW, show_width);
    XtSetSensitive(_circ_width_txtW, show_width);

    pglabel_setLabelPending (False);

    if (show_ctl) {
	XtManageChild (_ctlBb);
	XtUnmanageChild (_label_formW);
        if (callback) {
	    for (ii=0; ii< 2; ii++) {
                XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
  	        XtAddCallback (_ctlBtns[ii], XmNactivateCallback, 
			       (XtCallbackProc)callback, (XtPointer)ii);
	    }
	}
    }
    else {
	XtUnmanageChild (_ctlBb);
   	XtManageChild (_label_formW);
    }

    /* Save parameters to local variables */

    _subTyp  = subtyp;
    _vgType  = (char)eltyp;

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_CIRCLE;

    ces_get(_subTyp, &el, &ier);

    pgcirc_setAttr (el.hdr.maj_col, el.elem.cir.info.width);


    /*
     * If there is a label string associated with the circle,
     * show it on the attribute window.
     */
    if( label_ptr != NULL ) {
       XtManageChild(_labelReminder_formW);
       XmTextSetString (_labelReminder_textW, label_ptr);
      
    }
    else {
       if( XtIsManaged(_labelReminder_formW) ) {
         XtUnmanageChild(_labelReminder_formW);
       }
    }

    XtManageChild ( _circles_dlgW );

    /*
     * set label 
     */
    XmToggleButtonGadgetSetState (_label_toggW2, _labelColorFlag, TRUE);

    XtSetSensitive(_label_submenuW, _labelFlag);
    XtSetSensitive(_label_menuW2,   _labelFlag);

}

/*=====================================================================*/

void pgcirc_popdown ( void )
/************************************************************************
 * pgcirc_popdown							*
 *									*
 * This function unmanages the circle dialog box			*
 *									*
 * void pgcirc_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *				NONE					*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_popdown		*
 * E. Safford/SAIC	12/01	move pgcirc_setTable inside if stmnt	*
 *				  add pgcirc_setGhostFlag  		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_circles_dlgW) ) {

        pgcirc_setTable ();
        NxmClrW_popdown();

    	XtUnmanageChild(_circles_dlgW);
    }

    pgcirc_setGhostFlag (FALSE, NULL);
}

/*=====================================================================*/

void pgcirc_saveAttr ( void )
/************************************************************************
 * pgcirc_saveAttr							*
 *									*
 * Stores the new Attributes set by call back functions			*
 *									*
 * void pgcirc_saveAttr ()						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99						*
 * E. Safford/SAIC	12/01	add pgutls_initHdr			*
 ***********************************************************************/
{
    VG_DBStruct		el;
    int			ier;
/*---------------------------------------------------------------------*/

    pgutls_initHdr( &(el.hdr) );

    el.hdr.vg_class = (char)CLASS_CIRCLE;
    el.hdr.vg_type  = _vgType;
    ces_get(_subTyp, &el, &ier);
    el.hdr.maj_col  = _attrColr;
    el.hdr.min_col  = _attrColr;
    el.elem.cir.info.width = _attrWdth;

    if (_Lat1 >=  -90.0F && _Lat1 <=  90.0F && 
        _Lon1 >= -180.0F && _Lon1 <= 180.0F ) {
        el.elem.cir.data.latlon[0] = _Lat1;
        el.elem.cir.data.latlon[2] = _Lon1;
    }

    if (_Lat2 >=  -90.0F && _Lat2 <=  90.0F && 
        _Lon2 >= -180.0F && _Lon2 <= 180.0F ) {
        el.elem.cir.data.latlon[1] = _Lat2;
        el.elem.cir.data.latlon[3] = _Lon2;
    }

    ces_set(_subTyp, &el, &ier);

}

/*=====================================================================*/

void pgcirc_setAttr ( int color, int width )
/************************************************************************
 * pgcirc_setAttr							*
 *									*
 * This function sets the values in the circle dialog box		*
 *									*
 * void pgcirc_setAttr (color, width)		                        *
 *									*
 * Input parameters:                                                    *
 *	color		int		color value			*
 *	width		int		width value			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_setAttr		*
 ***********************************************************************/
{
    char	str[5];
/*---------------------------------------------------------------------*/

    /*
     * color
     */
    _attrColr = color; 

    XtVaSetValues(_circleColrW,
	XmNbackground,		NxmColrP_getColorPixel(_attrColr),
	XmNtopShadowColor,	NxmColrP_getColorPixel(_attrColr),
	XmNbottomShadowColor,	NxmColrP_getColorPixel(_attrColr),
	NULL);

    /* 
     * width
     */
    if (XtIsSensitive (_circ_width_sldW)) { 
        if (0 < width && width <= MAX_WIDTH_SCALE) {
	    sprintf (str, "%i", width);
            XmTextFieldSetString(_circ_width_txtW, str);
            XmScaleSetValue(_circ_width_sldW, width);

	    _attrWdth = width;
	}
    } 


}
 
/*=====================================================================*/

void pgcirc_setLabValue ( int which )
/************************************************************************
 * pgcirc_setLabValue							*
 *									*
 * put the selected label name into the text string 			*
 *									*
 * void pgcirc_setLabValue (which)					*
 *									*
 * Input parameters:							*
 *	which		int	which label				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_setLabValue		*
 * T. Piper/SAIC	 2/02	Freed text				*
 ***********************************************************************/
{
    XmString	xmstr;
    char	*text;
/*---------------------------------------------------------------------*/
    XtVaGetValues(_label_pbW[which],
	    XmNlabelString, 		&xmstr,
	    NULL);    

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text);
    XmStringFree(xmstr);

    if (strcmp(text, "Other")){
        sprintf(_labelName, "%s", text);
    }
    else {
	sprintf(_labelName, "%s", "");
    }
    if (text) XtFree(text);
}

/*=====================================================================*/

Boolean pgcirc_isUp ( void )
/************************************************************************
 * pgcirc_isUp								*
 *									*
 * This function returns a boolean value specifying whether the circle	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgcirc_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pgcirc_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_isUp			*
 ***********************************************************************/
{

    return (XtIsManaged(_circles_dlgW) );

}

/*=====================================================================*/

void pgcirc_getAttr ( int *color, int *width )
/************************************************************************
 * pgcirc_getAttr							*
 *									*
 * This function gets the circle attributes.				*
 *									*
 * void pgcirc_getAttr (color, width)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*color		int		major color			*
 *	*width		int		width				*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_getAttr		*
 ***********************************************************************/
{
    *color	= _attrColr;
    *width	= _attrWdth;
}

/*=====================================================================*/

Boolean pgcirc_getLabFlag ( void )
/************************************************************************
 * pgcirc_getLabFlag							*
 *									*
 * This function returns a boolean value specifying whether the	        *
 * circle labelFlag is TRUE or FALSE.					*
 *									*
 * Boolean pgcirc_getLabFlag()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pgcirc_getLabFlag	Boolean		Use/do not use label	*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_getLabFlag		*
 ***********************************************************************/
{
    return _labelFlag;
}

/*=====================================================================*/

Boolean pgcirc_getLabColorFlag ( void )
/************************************************************************
 * pgcirc_getLabColorFlag						*
 *									*
 * This function returns a boolean value specifying whether the	circle  *
 * label color flag is TRUE or FALSE.					*
 *									*
 * Boolean pgcirc_getLabColorFlag()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pgcirc_getLabColorFlag	Boolean		Use/do not use label	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	   06/00       initial coding		                *
 ***********************************************************************/
{
    return _labelColorFlag;
}

/*=====================================================================*/

void pgcirc_getLabValue ( char *label )
/************************************************************************
 * pgcirc_getLabValue							*
 *									*
 * This function gets the label text value 				*
 *									*
 * void pgcirc_getLabValue ( label)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*label		char		* label value			*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_getLabValue		*
 ***********************************************************************/
{
    strcpy(label, _labelName);
}

/*=====================================================================*/
/* ARGSUSED */
void pgcirc_widthCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgcirc_widthCb                                                       *
 *                                                                      *
 * Callback for width scale widget.                                     *
 *                                                                      *
 * void pgcirc_widthCb( w, clnt, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget			Widget ID		*
 *	clnt	XtPointer		not used		*
 *	*cbs		XmScaleCallbackStruct	callback struct		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	 5/99	Copied from pgline_widthCb		*
 ***********************************************************************/
{
    int		linwid;		/* line width */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    linwid = cbs->value;
    XmScaleSetValue(w, linwid);
    sprintf(txtstr, "%i", linwid);
    XmTextFieldSetString(_circ_width_txtW, txtstr);

    if (cbs->reason == XmCR_VALUE_CHANGED) {
	_attrWdth = linwid;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgcirc_widTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgcirc_widTxtCb                                                      *
 *                                                                      *
 * Callback for circle line width text widget.				*
 *                                                                      *
 * void pgcirc_widTxtCb( w, clnt, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget				Widget ID	*
 *	clnt	XtPointer			not used	*
 *	*cbs		XmTextVerifyCallbackStruct	callback struct	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	 5/99	Copied from pgline_widTxtCb		*
 ***********************************************************************/
{
    char	*ss;
    int		slval, linwid;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if (!cbs->event) 
	return;

    /*
     * if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue(_circ_width_sldW, &slval);
    ss = XmTextFieldGetString(_circ_width_txtW);
    linwid = atoi(ss);
    XtFree(ss);

    if (1 <= linwid && linwid <= MAX_WIDTH_SCALE) {
        if (linwid != slval) {
            XmScaleSetValue(_circ_width_sldW, linwid);

	    _attrWdth = linwid;
        }
    }
}

/*=====================================================================*/

void pgcirc_colorCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgcirc_colorCb							*
 *									*
 * Callback for color button widget.  Replaces original call to		*
 * NxmClrW_popup to insert a call to NxmClrW_setOkF, before passing	*
 * the information on to NxmClrW_popup.					*
 *									*
 * void pgcirc_colorCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *   wid		Widget			Widget ID		*
 *   clnt	XtPointer       	color			*
 *   cbs		XtPointer	callback struct		*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_colorCb		*
 ***********************************************************************/
{
    clnt = (XtPointer) &_attrColr;
    NxmClrW_popup (wid, clnt, (XtPointer)cbs);
}

/*=====================================================================*/
/* ARGSUSED */
void pgcirc_labelPbCb ( Widget ww, long clnt, XtPointer cbs ) 
/************************************************************************
 * pgcirc_labelPbCb							*
 *									*
 * This function is the callback function of circle label widget.	*
 *									*
 * void pgcirc_labelPbCb ( ww, clnt, call )			*
 *									*
 * Input parameters:							*
 *	ww		Widget			widget ID		*
 *	clnt	long			client data		*
 *	cbs	XtPointer	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_labelPbCb		*
 ***********************************************************************/
{
    _labelFlag = (clnt>0) ? 1 : 0;

    XmToggleButtonGadgetSetState (_label_toggW, _labelFlag, TRUE);
    XtManageChild (_label_optW);

    pgcirc_setLabValue ((int)clnt);

    if (_labelFlag){
	 mbotw_mouseSet(LMHINT_NEXT, MMHINT_LABEL);
    }
    else {
	 mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgcirc_labelToggCb ( Widget w, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgcirc_labelToggCb                                                   *
 *                                                                      *
 * Callback for label toggle button widget.                             *
 *                                                                      *
 * void pgcirc_labelToggCb( w, clnt, cbs)                  *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   cbs	XtPointer	callback struct                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	 5/99	Copied from pgline_ToggCb		*
 * H. Zeng/EAI          06/00   added label color toggle callback       *
 * J. Wu/SAIC		08/01	cast _labelFlag if used as array index	*
 ***********************************************************************/
{
     int      which_toggle;
     Boolean  btnval;
/*---------------------------------------------------------------------*/

    /* check to see if the button is up or down and take appropriate
     * actions.
     */
    XtVaGetValues(w, XmNset, &btnval, NULL);
    which_toggle = (long)clnt;

    if(which_toggle == 0) {
      _labelFlag=(btnval) ? 3 : 0;

      XtSetSensitive(_label_submenuW, _labelFlag);
      XtSetSensitive(_label_menuW2,   _labelFlag);
      XtVaSetValues (_label_optW, 
	  XmNmenuHistory,	      _label_pbW[(int)_labelFlag], 
	  NULL);

      pgcirc_setLabValue (_labelFlag);
      if (_labelFlag){
	 mbotw_mouseSet(LMHINT_NEXT, MMHINT_LABEL);
      }
      else {
	 mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
      }

    }
    else {
      _labelColorFlag=(btnval) ? 1 : 0;

    }


}

/*=====================================================================*/

void pgcirc_setTable ( void )
/************************************************************************
 * pgcirc_setTable							*
 *									*
 * Places an element into the set table					*
 *									*
 * void pgcirc_setTable ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_setTable		*
 * E. Safford/SAIC	12/01	added pgutls_initHdr()			*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		loglev, ier, ier1;
    char	logstr[10], grp[4];
/*---------------------------------------------------------------------*/

    loglev = 2;
    strcpy(grp, "CES");

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_CIRCLE;
    ces_get(_subTyp, &el, &ier);

    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }

    el.hdr.maj_col = _attrColr;
    el.elem.cir.info.width  = _attrWdth;

    ces_set(_subTyp, &el, &ier);
    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }
}

/*=====================================================================*/

void pgcirc_setLabFlag ( Boolean lab_flag )
/************************************************************************
 * pgcirc_setLabFlag							*
 *									*
 * set the label flag 							*
 *									*
 * void pgcirc_setLabFlag (lab_flag)					*
 *									*
 * Input parameters:							*
 *	lab_flag	Boolean	lab_flag				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgline_setLabFlag		*
 * J. Wu/SAIC		08/01	cast _labelFlag if used as array index	*
 ***********************************************************************/
{
    _labelFlag = lab_flag;

    XtSetSensitive(_label_submenuW, _labelFlag);
    XtVaSetValues (_label_optW, 
	XmNmenuHistory,			_label_pbW[(int)_labelFlag], 
	NULL);

    pgcirc_setLabValue (_labelFlag);

}

/*=====================================================================*/

void pgcirc_getLocation ( float *xx1, float *yy1, float *xx2, float *yy2)
/************************************************************************
 * pgcirc_getLocation							*
 *                                                                      *
 * gets the circle coordinate position					*
 *                                                                      *
 * void pgcirc_getLocation(xx1, yy1, xx2, yy2)				*
 *                                                                      *
 * Output parameters:                                                   *
 *	*xx1		float		x coodinate of circle center	*
 *	*yy1		float		y coodinate of circle center	*
 *	*xx2		float		x coodinate of circle edge	*
 *	*yy2		float		y coodinate of circle edge	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	 5/99						*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 ***********************************************************************/
{
 float	rx[2], ry[2], lat_input[2], lon_input[2];
 int	np, ier;
/*---------------------------------------------------------------------*/
    np =2;
    lat_input[0] = _Lat1;
    lon_input[0] = _Lon1;
    lat_input[1] = _Lat2;
    lon_input[1] = _Lon2;

    gtrans (sys_M, sys_D, &np, lat_input, lon_input, rx, ry, 
            &ier, strlen(sys_M), strlen(sys_D) );

    *xx1 = rx[0];
    *yy1 = ry[0];
    *xx2 = rx[1];
    *yy2 = ry[1];
}
/*=====================================================================*/

char pgcirc_getGrptyp ( void )
/************************************************************************
 * pgcirc_getGrptyp							*
 *									*
 * This function returns the current grptyp from the attribute window.	*
 *									*
 * NOTE:  at the moment circles do not have a group menu.  If drawn     *
 * with a label, the circle and text are placed in group type 8, or     *
 * "LABEL".  Robust, the circle window is not.				*
 *									*
 * char pgcirc_getGrptyp ( )  	 					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 *	pgcirc_getGrptyp    char    the current value of the grptyp	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	04/01						*
 * M. Li/SAIC		05/02	Get the group type from Grouping	*
 ***********************************************************************/
{
char	grptyp;
/*---------------------------------------------------------------------*/

    if (pgpalw_isGrpActv()) {
	grptyp = pggrpw_getGrpType ();
    }
    else {
    	grptyp = ( pgcirc_getLabFlag() ) ? 8 : 0;
    }

    return ( grptyp );

}


/*=====================================================================*/

void pgcirc_setGhostFlag ( Boolean flag, void (*update)(VG_DBStruct *) )
/************************************************************************
 * pgcirc_setGhostFlag							*
 *									*
 * This function sets the flag for ghosting.				*
 *									*
 * void pgcirc_setGhostFlag (flag, update)				*
 *									*
 * Input Parameters:							*
 *	flag		Boolean		To set or not to set		*
 *	*update()	void 	ghost update function (NULL if !flag)	*
 *									*
 * Output Parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgtxt_setGhostFlag		*
 ***********************************************************************/
{
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (_ghostFlag) {
	el.hdr.vg_class = 0;
	_updateGhostCirc (&el);		/* using the function */
    }

    _ghostFlag = flag;

    if (_ghostFlag) {
	_updateGhostCirc = update;	/* setting the function */
    }
}

/*=====================================================================*/

void pgcirc_setLocation ( float xx1, float yy1, float xx2, float yy2 )
/************************************************************************
 * pgcirc_setLocation							*
 *									*
 * Sets the circle coordinate position					*
 *									*
 * void pgcirc_setLocation(xx1, yy1, xx2, yy2)				*
 *									*
 * Input parameters:							*
 *	xx1		float		x coodinate of circle center	*
 *	yy1		float		y coodinate of circle center	*
 *	xx2		float		x coodinate of circle edge	*
 *	yy2		float		y coodinate of circle edge	*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI	 5/99						*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 ***********************************************************************/
{
    float	rx[2], ry[2], lat_input[2], lon_input[2];
    int	np, ier;
/*---------------------------------------------------------------------*/
    np =2;
    rx[0] = xx1;
    ry[0] = yy1;
    rx[1] = xx2;
    ry[1] = yy2;

    gtrans (sys_D, sys_M, &np, rx, ry, lat_input, lon_input, 
            &ier, strlen(sys_D), strlen(sys_M) );

    _Lat1 = lat_input[0];
    _Lon1 = lon_input[0];
    _Lat2 = lat_input[1];
    _Lon2 = lon_input[1];
}

/*=====================================================================*/

void pgcirc_updateGstCirc ( void )
/************************************************************************
 * pgcirc_updateGstCirc							*
 *									*
 * This function updates the circle for ghosting.			*
 *									*
 * void pgcirc_updateGstCirc ()						*
 *									*
 * Input Parameters:							*
 * Output Parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgtxt_updateGstTxt		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 ***********************************************************************/
{
    VG_DBStruct el;
    int		np, ier;
    float	srx[2], sry[2];
/*---------------------------------------------------------------------*/

    np = 2;

    if (_ghostFlag) {
	pgcirc_fillElement (&el);
	pgcirc_getLocation(&srx[0], &sry[0], &srx[1], &sry[1]);
	gtrans (sys_D, sys_M, &np, srx, sry, 
		&el.elem.cir.data.latlon[0], &el.elem.cir.data.latlon[np], 
		    &ier, strlen(sys_D), strlen(sys_M));
	_updateGhostCirc (&el);
    }
}

/*=====================================================================*/

void pgcirc_fillElement ( VG_DBStruct *el)
/************************************************************************
 * pgcirc_fillElement							*
 *									*
 * This function fills the VG_DBStruct.					*
 *									*
 * void pgcirc_fillElement (el)						*
 *									*
 * Input Parameters:							*
 *		NONE							*
 *									*
 * Output Parameters:							*
 *	*el	VG_DBStruct	structure to fill			*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgtxt_fillElement		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	if (_vgType == CIRCLE_ELM) {
	    el->hdr.vg_class = CLASS_CIRCLE;
	    el->hdr.vg_type  = _vgType;
	    el->hdr.maj_col = _attrColr;
	    el->elem.cir.info.lintyp	= _subTyp; 
	    el->elem.cir.info.lthw	= 0;
	    el->elem.cir.info.width	= _attrWdth;
	    el->elem.cir.info.lwhw	= 0;
	}
}

/*=====================================================================*/

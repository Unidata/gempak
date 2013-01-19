#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"


#define SAMPLE_WIDTH	140		/* width of sample draw area	*/
#define SAMPLE_HEIGHT	30		/* height of sample draw area	*/
#define D_BT_WIDTH	80		/* width of drawn button	*/
#define STY_CHOICE	10		/* number of available style	*/
#define WID_CHOICE	4		/* number of available width	*/
#define STY_PAT 	10		/* number of style patterns	*/
#define DRAWING_MARGIN	10		/* margin of drawing area	*/
#define STY_TOTAL	(STY_PAT*3)	/* total num. of style patterns */

#define D_BT_HEIGHT  20

typedef struct {
int	pattern[8];
int	scal;
}	_styPat_t;			/* structure of style patterns	*/

static _styPat_t     _styPatStr[STY_TOTAL];    /* style patterns	*/


static _NXMattr  *_lineAttrCopy;      /* original copy of line attribute */
static _NXMattr  _lineAttrEdit;       /* line attribute to be edited	 */

static Widget (*_workareaCreate)(Widget);   /* workarea creating function */

static Widget  _attrW;			/* attribute widget ID		*/
static Widget  _attrWkW;		/* workarea widget ID		*/
static Widget  _workareaFrame;		/* workarea frame widget	*/
static Widget  _demoAreaW;		/* drawing area's widget IDs	*/
static Widget	_styButton[STY_CHOICE]; /* buttons for style		*/
static Widget	_widButton[WID_CHOICE]; /* buttons for width		*/

static	GC _attrGc = NULL ;	/* shared GC				*/

static int	   _styPatInx;		/* style pattern scale index	*/
static int	   _styFlag;		/* flag for style CB function	*/
static int	   _widFlag;		/* flag for width CB function	*/

static NxmColrP_t	*_NxmColrP;	   /* color pallete in NxmLineA */


/*
 *  Private functions
 */
void NxmLineA_colorEh ( Widget, long which, XEvent *event, Boolean *ctdr );
void NxmLineA_createStyle ( Widget parent );
void NxmLineA_createWidth ( Widget parent );
void NxmLineA_demoExposeCb ( Widget, XtPointer, XtPointer );
void NxmLineA_popDownCb	( Widget, long which, XtPointer );
void NxmLineA_scaleCb	( Widget, long which, XtPointer );
void NxmLineA_styleCb	( Widget, long which, XtPointer );
void NxmLineA_widthCb	( Widget, long which, XtPointer );
void NxmLineA_unSetAll ( void );
void _drawDemoLine ( void );
void _drawStyle ( Widget w, _styPat_t ptn_struct );
void _drawWidth ( Widget w, int line_width );
void _styPtnInit ( void );
static void (*_NxmLineAApplyFunc)(void) = NULL;	/* function to be
						  executed if Apply
						  button is clicked */


/************************************************************************
 * NxmLineA.c								*
 *									*
 * This module creates the width, style and color attribute editing	*
 * panel. Each available choice is displayed on a button. The final	*
 * attribute which is the combination of all three choice is displayed	*
 * in the demo area. Additional functionality can be added by using	*
 * a workarea function. 						*
 *									*
 * CONTENTS:								*
 *									*
 *	NxmLineA_create()	create the line attributes popup window.*
 *	NxmLineA_popUp()	pops up the attribute window.		*
 *	NxmLineA_popDownCb()	pops down the attribute window. 	*
 *	NxmLineA_createStyle()	create style selction module.		*
 *	NxmLineA_createWidth()	create width selction module.		*
 *	NxmLineA_styleCb()	callback of style selection.		*
 *	NxmLineA_widthCb()	callback of width selection.		*
 *	NxmLineA_colorEh()	callback of color selection.		*
 *	NxmLineA_scaleCb()	callback of style pattern scaling.	*
 *	NxmLineA_demoExposeCb() callback of demo line drawing area.	*
 *	NxmLineA_unSetAll()	unset buttons and flags.		*
 *	_drawStyle()		draw a patterned line.			*
 *	_drawWidth()		draw a width defined line.		*
 *	_drawDemoLine() 	draw a line with current attribute.	*
 *	_styPtnInit()		initialize the style patterns.		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmLineA_create ( Widget parent )
/************************************************************************
 * NxmLineA_create							*
 *									*
 * This function creates the attribute popup window.			*
 *									*
 * Widget NxmLineA_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	NxmLineA_create		Widget		widget ID		*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC	03/96						*
 *	C. Lin/EAI	06/96	use NxmControlBtn() add background	*
 *					to _demoAreaW			*
 *	S. Wang/GSC	08/96	add workarea frame			*
 *	S. Wang/GSC	04/97	clean up & move to nxm library		*
 *	G. Krueger/EAI	10/97	NxmFrameLabel->NxmLabel_createFrameLbl	*
 *	G. Krueger/EAI	10/97	NxmControlBtn->NxmCtlBtn_create 	*
 *	I. Durham/GSC	05/98	Changed underscore decl. to an include	*
 ***********************************************************************/
{
Widget	 pane, form;
Widget	 pane_fr;
Widget	 frame_sty, frame_wid, frame_col, radio_box;
XmString one, two, three;
char	 *buttonlist[] = { "Accept", "Cancel" };

/*---------------------------------------------------------------------*/

	/*
	 * initialize style patterns
	 */
	_styPtnInit();

	_styPatInx = 1;

	_attrW = XmCreateFormDialog(parent,
		"attrwPanel", NULL, 0);

	pane = XtVaCreateWidget("pane",
		xmPanedWindowWidgetClass, _attrW,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL);

	form = XtVaCreateManagedWidget("form",
		xmFormWidgetClass, pane,
		NULL);

	/*
	 * create width editing area
	 */
	frame_wid = XtVaCreateManagedWidget("frame_wid",
		 xmFrameWidgetClass,	 form,
		 XmNtopAttachment,	 XmATTACH_FORM,
		 XmNtopOffset,		 20,
		 XmNleftAttachment,	 XmATTACH_FORM,
		 XmNleftOffset, 	 10,
		 XmNmarginWidth,	 10,
		 XmNmarginHeight,	 10,
		 NULL);
	NxmLineA_createWidth(frame_wid);

	/*
	 * create  demo area
	 */
	_demoAreaW = XtVaCreateManagedWidget("NxmLineA_drawArea",
		xmDrawingAreaWidgetClass, form,
		XmNtopAttachment,	  XmATTACH_FORM,
		XmNtopOffset,		  20,
		XmNleftAttachment,	  XmATTACH_WIDGET,
		XmNleftWidget,		  frame_wid,
		XmNleftOffset,		  10,
		XmNwidth,		  SAMPLE_WIDTH,
		XmNheight,		  SAMPLE_HEIGHT,
		NULL);

	XtAddCallback(_demoAreaW, XmNexposeCallback,
		      (XtCallbackProc)NxmLineA_demoExposeCb, NULL);

	/*
	 * create style editing area
	 */
	frame_sty = XtVaCreateManagedWidget("frame_sty",
		xmFrameWidgetClass,	form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		frame_wid,
		XmNtopOffset,		20,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		10,
		XmNmarginWidth, 	10,
		XmNmarginHeight,	10,
		NULL);

	NxmLineA_createStyle(frame_sty);

	/*
	 * create width editing area
	 */
	frame_col = XtVaCreateManagedWidget("frame_col",
		xmFrameWidgetClass,	form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		_demoAreaW,
		XmNtopOffset,		20,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		frame_sty,
		XmNleftOffset,		10,
		XmNmarginWidth, 	10,
		XmNmarginHeight,	15,
		NULL);

	pane_fr = XtVaCreateManagedWidget( "pane_fr",
		xmPanedWindowWidgetClass, frame_col,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNx,			  10,
		NULL );

	NxmLabel_createFrameLbl("Color", pane_fr, frame_col );
	_NxmColrP = NxmColrP_create( pane_fr, 8, 1, 
					(XtEventHandler)NxmLineA_colorEh);


	/*
	 * create style pattern scale editing area
	 */
	one   = XmStringCreateLocalized("Compress");
	two   = XmStringCreateLocalized("Normal");
	three = XmStringCreateLocalized("Expand");

	radio_box = XmVaCreateSimpleRadioBox(form,
		"styleRadio", _styPatInx, 
		(XtCallbackProc)NxmLineA_scaleCb,
		XmVaRADIOBUTTON, one,	NULL, NULL, NULL,
		XmVaRADIOBUTTON, two,	NULL, NULL, NULL,
		XmVaRADIOBUTTON, three, NULL, NULL, NULL,
		XmNy,		 60,
		NULL);

	XtVaSetValues(radio_box,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		frame_col,
		XmNtopOffset,		20,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		10,
		XmNorientation, 	XmHORIZONTAL,
		NULL);

	XmStringFree(one);
	XmStringFree(two);
	XmStringFree(three);

	XtManageChild(radio_box);

	/*
	 * create work area frame widget
	 */

	_workareaFrame = XtVaCreateWidget("waPane",
		xmFrameWidgetClass, pane,
		NULL);

	NxmCtlBtn_create( pane, 1, "attw_cntrlBtn", XtNumber(buttonlist),
			  buttonlist, (XtCallbackProc)NxmLineA_popDownCb, NULL);

	XtManageChild(pane);

	XtVaSetValues(_demoAreaW, XmNbackground,
		NxmColrP_getColorPixel(0), NULL);

	XtRealizeWidget(_attrW);

	return(_attrW);

}


/*=====================================================================*/

void NxmLineA_popUp ( _NXMattr *attr_copy, char *title_name, 
		      void (*apply_func)(void), Widget (*workarea_create)(Widget), 
		      void (*workarea_init)(void) )
 /***********************************************************************
 * NxmLineA_popUp							*
 *									*
 * This function sets up the parameters for the line attributes 	*
 * window and pop it up. Workarea widget is created and initilized	*
 * when necessary.							*
 *									*
 * void NxmLineA_popUp( attr_copy, title_name, apply_func,		*
 *				workarea_create, workarea_init )	*
 *									*
 * Input parameters:							*
 *  attr_copy		_NXMattr*	original line attribute 	*
 *  title_name		char*		attribute window name		*
 *  apply_func		void		applying function		*
 *  workarea_create	(Widget *)	work area create function	*
 *  workarea_init	(void *)	work area init function 	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *  S. Wang/GSC       04/96						*
 *  C. Lin/EAI	      06/96	add checking to lineAttrEdit.style	*
 *				add function to be invoked when apply	*
 *  S. Wang/GSC       09/96	add workarea creating function		*
 *  S. Wang/GSC       04/97	clean up and move to nxm library	*
 ***********************************************************************/
{
int		i;
XmString	titlestr;

/*---------------------------------------------------------------------*/

	if (XtIsManaged(_attrW))
	       XtUnmanageChild(_attrW);

	/*
	 * assign title name and current values to the
	 * parameters in NxmLineA_create function
	 */
	_lineAttrCopy = attr_copy;
	_lineAttrEdit.color = _lineAttrCopy->color;
	_lineAttrEdit.width = _lineAttrCopy->width;
	_lineAttrEdit.style = _lineAttrCopy->style;

	_styFlag = _widFlag = 0;

	if ( _lineAttrEdit.style < STY_PAT )
		_lineAttrEdit.style += 2*STY_PAT;

	titlestr = XmStringCreateLocalized(title_name);

	XtVaSetValues(_attrW, XmNdialogTitle, titlestr, NULL);
	XmStringFree(titlestr);

	_styPatInx = _lineAttrEdit.style/STY_PAT;

	/*
	 * set the current color, width and style units
	 * in attribute window
	 */

	for (i=0;i<STY_CHOICE;i++)
	    if ( !(i - (_lineAttrCopy->style)%STY_PAT)	)
		break;

	if (i<STY_CHOICE)
	    XtVaSetValues( _styButton[i],
		XmNshadowType, XmSHADOW_IN,
		NULL);

	if ( (_lineAttrCopy->width)>0 )
	    XtVaSetValues( _widButton[(_lineAttrCopy->width)-1],
		XmNshadowType,	XmSHADOW_IN,
		NULL);

	NxmColrP_setColor( _NxmColrP, _lineAttrCopy->color );

	if ( apply_func )
		_NxmLineAApplyFunc = apply_func;

	_drawDemoLine();

	/*
	 * create workarea when necessary
	 */

	if ( _workareaCreate != workarea_create ) {
	    _workareaCreate = workarea_create;

	    if ( _workareaCreate != NULL ) {
		_attrWkW = _workareaCreate(_workareaFrame);
		if ( workarea_init )
			workarea_init();
		XtManageChild( _workareaFrame );
		XtManageChild( XtParent(_workareaFrame) );
	    }
	    else
		if ( _attrWkW )
		    XtUnmanageChild(_workareaFrame);
	}

	XtManageChild(_attrW);
}


/*=====================================================================*/

void NxmLineA_unSetAll ( void )
 /***********************************************************************
 * NxmLineA_unSetAll						 	*
 *									*
 * this function unset all the buttons and flags.			*
 *									*
 * void NxmLineA_unSetAll()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		08/96					*
 ***********************************************************************/
{
int	i;

/*---------------------------------------------------------------------*/

	for ( i=0; i<WID_CHOICE; i++ )
	    XtVaSetValues(_widButton[i],
		 XmNshadowType, XmSHADOW_ETCHED_IN,
		 NULL);

	for ( i=0; i<STY_CHOICE; i++ )
	    XtVaSetValues(_styButton[i],
		 XmNshadowType, XmSHADOW_ETCHED_IN,
		 NULL);

	NxmColrP_deselectAll(_NxmColrP);

}


/*=====================================================================*/

void NxmLineA_createStyle ( Widget parent )
 /***********************************************************************
 * NxmLineA_createStyle 						*
 *									*
 * this function creates the style selection area.			*
 *									*
 * void NxmLineA_createStyle(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 *	G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
long	ii;
Widget	pane, rowcol;

/*---------------------------------------------------------------------*/

	pane = XtVaCreateManagedWidget("pane",
		xmPanedWindowWidgetClass, parent,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL);

	NxmLabel_createFrameLbl("Style", pane, parent );

	rowcol = XtVaCreateManagedWidget("rowcol",
		xmRowColumnWidgetClass, pane,
		XmNpacking,		XmPACK_COLUMN,
		XmNnumColumns,		2,
		NULL);

	for(ii=0; ii<STY_CHOICE; ii++) {
	    _styButton[ii] = XtVaCreateManagedWidget( "line_style",
			xmDrawnButtonWidgetClass,  rowcol,
			XmNwidth,		   D_BT_WIDTH,
			XmNheight,		   D_BT_HEIGHT,
			NULL);

	    XtAddCallback( _styButton[ii], XmNexposeCallback,
			(XtCallbackProc)NxmLineA_styleCb, (XtPointer)ii );
	    XtAddCallback( _styButton[ii], XmNactivateCallback,
			(XtCallbackProc)NxmLineA_styleCb, (XtPointer)ii );
	}

}

/*=====================================================================*/

void NxmLineA_createWidth ( Widget parent )
/************************************************************************
 * NxmLineA_createWidth 						*
 *									*
 * This function creates the width selection module.			*
 *									*
 * void NxmLineA_createWidth(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 *	G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
long	ii;
Widget	pane, rowcol1;

/*---------------------------------------------------------------------*/

	pane = XtVaCreateManagedWidget("pane",
		xmPanedWindowWidgetClass, parent,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNx,			  10,
		NULL);

	NxmLabel_createFrameLbl("Width", pane, parent );

	rowcol1 = XtVaCreateManagedWidget("rowcol1",
		xmRowColumnWidgetClass, pane,
		XmNpacking,		XmPACK_COLUMN,
		XmNnumColumns,		2,
		NULL);

	for (ii=0; ii<WID_CHOICE; ii++) {
	    _widButton[ii] = XtVaCreateManagedWidget( "line_width",
			xmDrawnButtonWidgetClass,  rowcol1,
			XmNheight,		   D_BT_HEIGHT,
			XmNwidth,		   D_BT_WIDTH,
			NULL);

	    XtAddCallback( _widButton[ii], XmNexposeCallback,
			(XtCallbackProc)NxmLineA_widthCb, (XtPointer)ii );
	    XtAddCallback( _widButton[ii], XmNactivateCallback,
			(XtCallbackProc)NxmLineA_widthCb, (XtPointer)ii );
	}

}

/*=====================================================================*/

void NxmLineA_styleCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmLineA_styleCb							*
 *									*
 * Function NxmLineA_styleCb is the callback function for		*
 * line style selection.						*
 *									*
 * void NxmLineA_styleCb(w, which, cbs)				*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	which	   long		widget button				*
 *	cbs	   XtPointer	resource				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		04/96						*
 ***********************************************************************/
{
int				i;
static int			last_unit;
XmDrawnButtonCallbackStruct*	call_struct;

/*---------------------------------------------------------------------*/

	call_struct = (XmDrawnButtonCallbackStruct *)cbs;

	/*
	 * initialize style to current original attribute.
	 */

	if ( !_styFlag ) {
		for(i=0; i<STY_CHOICE ;i++)
		    if( (i - (_lineAttrCopy->style)%STY_PAT )==0)
			break;

		last_unit = i;
		_styFlag  = 1;
	}

	/*
	 * set up the current value of style when window
	 *  first exposed
	 */

	if( call_struct->reason == XmCR_EXPOSE ) { 
		_drawStyle(w, _styPatStr[which+STY_PAT]);
	}

	if ( call_struct->reason == XmCR_ACTIVATE ) {
		if (last_unit >= STY_CHOICE)
			last_unit = 0;

		_lineAttrEdit.style = (int)which + _styPatInx*STY_PAT;

		XtVaSetValues(_styButton[last_unit],
			XmNshadowType, XmSHADOW_ETCHED_IN,
			NULL);

		XtVaSetValues(_styButton[which],
			XmNshadowType, XmSHADOW_IN,
			 NULL);

		last_unit = (int)which;
			_drawDemoLine();
	}
}

/*=====================================================================*/

void NxmLineA_widthCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmLineA_widthCb							*
 *									*
 * Function NxmLineA_widthCb is the callback function for		*
 * line width selection.						*
 *									*
 * void NxmLineA_widthCb(w, which, cbs)					*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	which	   long		widget button				*
 *	cbs	   XtPointer	resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		04/96						*
 ***********************************************************************/
{
static int			last_unit;
XmDrawnButtonCallbackStruct*	call_struct;

/*---------------------------------------------------------------------*/

	call_struct = (XmDrawnButtonCallbackStruct*)cbs;

	if (!_widFlag) {
		last_unit = (_lineAttrCopy->width) - 1;
		_widFlag  = 1;
	}

/*
 * set up the current value of width when window
 * first exposed
 */

	if( call_struct->reason == XmCR_EXPOSE ) {
		_drawWidth( w, (which+1) );
	}

/*
 * update the selected unit
 */
	if (call_struct->reason == XmCR_ACTIVATE) {
		_lineAttrEdit.width = (int)which + 1;

		XtVaSetValues(_widButton[last_unit],
			XmNshadowType, XmSHADOW_ETCHED_IN,
			NULL);

		XtVaSetValues(_widButton[which],
			XmNshadowType, XmSHADOW_IN,
			NULL);

		last_unit = (int)which;

		_drawDemoLine();
	}
}

/*=====================================================================*/
/* ARGSUSED */
void NxmLineA_colorEh ( Widget w, long which, XEvent *event, Boolean *ctdr )
/************************************************************************
 * NxmLineA_colorEh							*
 *									*
 * Function NxmLineA_colorEh is the callback function for choosing	*
 * line color.								*
 *									*
 * void NxmLineA_colorEh(w, which, event, ctdr)				*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	which	   long		widget button				*
 *	*event	   XEvent	resource				*
 *	*ctdr	   Boolean	contiue to dispath			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		04/96						*
 ***********************************************************************/
{
	if (event->xbutton.button == Button1 ) {
		_lineAttrEdit.color = (int)which;
		_drawDemoLine();

		NxmColrP_setColor( _NxmColrP, (int)which );
	}
}

/*=====================================================================*/
/* ARGSUSED */
void NxmLineA_scaleCb ( Widget w, long which, XtPointer cbs )
 /***********************************************************************
 * NxmLineA_scaleCb							*
 *									*
 * NxmLineA_scaleCb() is the callback function for pattern scale	*
 * selection.								*
 *									*
 * NxmLineA_scaleCb(w, which, cbs)					*
 *									*
 * Input parameters:							*
 *	w	   Widget	   widget ID				*
 *	which	   long		   which widget 			*
 *	cbs	   XtPointer	   resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 ***********************************************************************/
{
XmToggleButtonCallbackStruct	*call_struct;

/*---------------------------------------------------------------------*/

	call_struct = (XmToggleButtonCallbackStruct*)cbs;

	if (call_struct->set == 0)
		return;

	_styPatInx = (int)which+1;

	_lineAttrEdit.style = _lineAttrEdit.style%STY_PAT +
				_styPatInx*STY_PAT;

	_drawDemoLine();

}

/*=====================================================================*/
/* ARGSUSED */
void NxmLineA_popDownCb ( Widget w, long which, XtPointer cbs )
 /***********************************************************************
 * NxmLineA_popDownCb							*
 *									*
 * This is the callback function for attribute window close up. 	*
 *									*
 * void NxmLineA_popDownCb (w, which, cbs)				*
 *									*
 * Input parameters:							*
 *	w	    Widget	 widget ID				*
 *	which	    long 	 resource				*
 *	cbs	    XtPointer	 resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 *	C. Lin/EAI		06/96	add _NxmLineAApplyFunc		*
 *	S. Wang/GSC		08/96	add workarea init function	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	XtUnmanageChild(_attrW);

	if ( which == 0 ) {

	    /*
	     * copy original variables
	     */

	    _lineAttrCopy->width = _lineAttrEdit.width;
	    _lineAttrCopy->color = _lineAttrEdit.color;
	    _lineAttrCopy->style = _lineAttrEdit.style;

	    /*
	     * call apply function
	     */

	    if ( _NxmLineAApplyFunc != NULL )
		_NxmLineAApplyFunc();
	}

	/* unset buttons and flags */

	NxmLineA_unSetAll();
}

/*=====================================================================*/
/* ARGSUSED */
void NxmLineA_demoExposeCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * NxmLineA_demoExposeCb						*
 *									*
 * This is the callback function for drawing the demo line when 	*
 * the window is exposed.						*
 *									*
 * void NxmLineA_demoExposeCb(w, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	clnt	   XtPointer	widget button				*
 *	cbs	   XtPointer	resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 ***********************************************************************/
{
    if ( XtIsRealized(w) )
	_drawDemoLine();
}

/*=====================================================================*/

void _drawDemoLine ( void )
 /***********************************************************************
 * _drawDemoLine							*
 *									*
 * This is the function for drawing the demo line.			*
 *									*
 * void _drawDemoLine() 						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC    04/96						*
 *	S. Wang/GSC    04/97	clean up and move to nxm library	*
 ***********************************************************************/
{
int		scale;
int		temp, p_sign;
int		i, x1, x2, y;
_styPat_t	ptn_str;
XGCValues	demo_val;
Drawable	demo_win;

/*---------------------------------------------------------------------*/

	/*
	 * set color, width and style to selected values
	 */

	ptn_str = _styPatStr[_lineAttrEdit.style-STY_PAT];

	demo_win = XtWindow(_demoAreaW);
	demo_val.line_width = _lineAttrEdit.width;
	demo_val.foreground = NxmColrP_getColorPixel(_lineAttrEdit.color);

	if ( _attrGc == NULL )
	    _attrGc = XCreateGC( NXMdisplay, demo_win,
			GCForeground|GCLineWidth, &demo_val );
	else
	    XChangeGC(NXMdisplay, _attrGc,
			GCForeground|GCLineWidth, &demo_val);

	XSetWindowBackground(NXMdisplay, demo_win, NxmColrP_getColorPixel(0) );

	x1 = x2 = STY_PAT;
	y = SAMPLE_HEIGHT/2;
	scale = ptn_str.scal;

	XClearWindow(NXMdisplay, demo_win);

	while( x1 < (SAMPLE_WIDTH-STY_PAT) ) {
	    for (i=0;i<8;i++) {
		temp = ptn_str.pattern[i];
		if (temp == 0)
		    break;

		p_sign = temp/abs(temp);

		switch (p_sign) {

		case 1:
			x2=x1+scale*temp;
			if ( x2>(SAMPLE_WIDTH-STY_PAT) )
			    x2 = SAMPLE_WIDTH-STY_PAT;

			if (i%2 == 0)
			    XDrawLine(NXMdisplay, demo_win,
				_attrGc,x1,y,x2,y);
			break;
		case -1:
			x2 = x1 + scale/2;
			if ( x2>(SAMPLE_WIDTH-STY_PAT) )
			    return;
			XDrawLine(NXMdisplay, demo_win,
				_attrGc,x1,y, x2,y);
			break;
		}
		x1 = x2;
	    }
	}
}

 /*====================================================================*/

void _drawStyle ( Widget w, _styPat_t ptn_struct )
 /***********************************************************************
 * _drawStyle								*
 *									*
 * Function _drawStyle draws a patterned line on a drawnbutton widget.	*
 *									*
 * void _drawStyle(w, ptn_struct )					*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	ptn_struct  _styPat_t	structure of a style pattern		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		04/96						*
 ***********************************************************************/
{
int		x1,x2, y, i, temp;
int		p_sign, scale;
XGCValues	style_val;
Drawable	style_screen;

/*---------------------------------------------------------------------*/

	style_screen = XtWindow(w);

	x1 = x2 = DRAWING_MARGIN;
	y = D_BT_HEIGHT/2;

	scale = ptn_struct.scal;
	if (ptn_struct.pattern[0] == 0) {
		return;
	}

	style_val.line_width = 1;
	style_val.foreground = NxmColrP_getColorPixel(0);

	if ( _attrGc == NULL )
	    _attrGc = XCreateGC( NXMdisplay, XtWindow(_demoAreaW),
				GCForeground|GCLineWidth, &style_val );
	else
	    XChangeGC( NXMdisplay, _attrGc,
				GCForeground|GCLineWidth, &style_val );

	XClearArea(NXMdisplay, style_screen,
		DRAWING_MARGIN/2, DRAWING_MARGIN/2,
		D_BT_WIDTH-DRAWING_MARGIN, DRAWING_MARGIN, FALSE);

	while(x1<D_BT_WIDTH - DRAWING_MARGIN) {
	    for (i=0;i<8;i++) {
		temp = ptn_struct.pattern[i];

		if (temp == 0)
		    break;

		p_sign = temp/abs(temp);

		switch (p_sign) {
		case 1:
		     x2=x1+scale*temp;

		    if ( x2>(D_BT_WIDTH-DRAWING_MARGIN) )
			x2 = D_BT_WIDTH-DRAWING_MARGIN;

		    if (i%2 == 0)
			XDrawLine(NXMdisplay, style_screen,
			    _attrGc,x1,y,x2,y);
		    break;

		case -1:
		    x2 = x1 + scale/2;
		    if ( x2>(D_BT_WIDTH-DRAWING_MARGIN) )
			return;

		    if (i%2 == 0)
			XDrawLine( NXMdisplay, style_screen,
			    _attrGc,x1,y,x2,y );
		    break;
		}
		x1 = x2;

	    }
	}
}

/*=====================================================================*/

void _drawWidth ( Widget w, int line_width )
 /***********************************************************************
 * _drawWidth								*
 *									*
 * Function _drawWidth draws a width defined line on the drawn button *
 * widget.								*
 *									*
 * void _drawWidth(w, line_width)					*
 *									*
 * Input parameters:							*
 *	w	     Widget	  widget ID				*
 *	line_width   int	  width of the line			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/97					*
 ***********************************************************************/
{
XGCValues			width_val;
Drawable			width_screen;

/*---------------------------------------------------------------------*/

	width_screen = XtWindow(w);

	width_val.line_width = line_width;
	width_val.foreground = NxmColrP_getColorPixel(0);

	if ( _attrGc == NULL )
	    _attrGc = XCreateGC(NXMdisplay, XtWindow(_demoAreaW),
			GCForeground|GCLineWidth, &width_val );
	else
	    XChangeGC(NXMdisplay, _attrGc,
			GCForeground|GCLineWidth, &width_val );

	XDrawLine( NXMdisplay, width_screen, _attrGc,
	    DRAWING_MARGIN, D_BT_HEIGHT/2,
	    D_BT_WIDTH - DRAWING_MARGIN, D_BT_HEIGHT/2 );

}


/*=====================================================================*/

void _styPtnInit ( void )
 /***********************************************************************
 * _styPtnInit								*
 *									*
 * This function initialize the variables representing style patterns.	*
 *									*
 * void _styPtnInit()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		04/96					*
 *	S. Wang/GSC		04/97	clean up and move to nxm lib	*
 ***********************************************************************/
{
int	   i, j,k;
static int val[STY_PAT][8] =
	{{-1,1,0,0,0,0,0,0},
	  {1,0,0,0,0,0,0,0},
	  {1,1,0,0,0,0,0,0},
	  {2,2,0,0,0,0,0,0},
	  {4,2,1,2,0,0,0,0},
	  {4,2,0,0,0,0,0,0},
	  {4,2,1,2,1,2,1,2},
	  {4,2,-1,2,0,0,0,0},
	  {4,2,-1,2,-1,2,-1,2},
	  {2,2,-1,2,0,0,0,0}};

/*---------------------------------------------------------------------*/

	for (j=0;j<3;j++) {
	    for (i=0;i<STY_PAT;i++) {
		_styPatStr[j*STY_PAT+i].scal = 2*(j+1);
		for (k=0;k<8;k++) {
		    _styPatStr[STY_PAT*j+i].pattern[k] = val[i][k];
		}
	    }
	}
}

/*=====================================================================*/

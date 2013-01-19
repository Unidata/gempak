#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


#define  TOTAL_MARKER	21
#define  ICON_DIR	"$NAWIPS/icons/nmap"

static Widget	   _markW;      /* marker panel widget	*/
static WidgetList  _stateBtn;   /* marker, text toggle buttons widget */
static Widget	   _sizeScale;	/* marker size scale widget */
static Widget	   _widthScale;	/* marker width scale widget */
static Widget	   _curType;	/* marker type cascade button */

static WidgetList  _txtSizeW;   /* text size buttons widget */
static Widget	   _sizeMenuW;   /* text size optino menu widget */

static NxmMarkA_t   *_mkAttrOrig;  /* marker editing data pointer */
static NxmMarkA_t   _mkAttrEdit;   /* local marker editing data */
static NxmMarkA_t   _mkAttrCopy;   /* marker saved data */
static NxmColrP_t   *_colrStr;	   /* color editing data structure  */
static WidgetList   _typeBtn;	   /* type selection option buttons */


/*
 *  Private functions
 */
void NxmMarkA_colorCb ( Widget, long, XtPointer );
void NxmMarkA_createType ( Widget );
void NxmMarkA_createSzWth ( Widget );
void NxmMarkA_popdown ( Widget, long, XtPointer );
void NxmMarkA_sizeCb ( Widget, XtPointer, XmScaleCallbackStruct* );
void NxmMarkA_stateCb ( Widget, long, XtPointer );
void NxmMarkA_txtsizeCb ( Widget, long, XtPointer );
void NxmMarkA_typeCb ( Widget, long, XtPointer );
void NxmMarkA_widthCb ( Widget, long, XmScaleCallbackStruct* );
static void (*_applyFunc)(void);    /* application applying function */


/************************************************************************
 * NxmMarkA.c								*
 *									*
 * This module makes the marker attribute editing panel 		*
 *									*
 * CONTENTS:								*
 *									*
 * NxmMarkA_create()	   create the marker attribute editing widget	*
 * NxmMarkA_createType()    create type selection area			*
 * NxmMarkA_createSzWth()   create size and width selection area	*
 *									*
 * NxmMarkA_popup()	   pops up the editing widget			*
 * NxmMarkA_popdown()	   pops down the editing widget 		*
 *									*
 * NxmMarkA_colorCb()	   color selection callback function		*
 * NxmMarkA_typeCb()	   type selection callback function		*
 * NxmMarkA_sizeCb()	   size selection callback function		*
 * NxmMarkA_widthCb()	   width selection callback function		*
 * NxmMarkA_stateCb()	   marker, text toggle callback function	*
 * NxmMarkA_txtsizeCb()   text size callback function			*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmMarkA_create ( Widget parent )
/************************************************************************
 * NxmMarkA_create							*
 *									*
 * This function create the marker attribute editing panel		*
 *									*
 * Widget NxmMarkA_create(parent) 					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	NxmMarkA_create	  Widget ID of the marker attr. editing widget	*
 *									*
 ** Log:								*
 *  S. Wang/GSC		09/97						*
 *  G. Krueger/EAI	10/97  	NxmFrameLabel->NxmLabel_createFrameLbl	*
 *  G. Krueger/EAI	10/97	NxmControlBtn->NxmCtlBtn_create		*
 *  E. Safford/GSC	06/00	Remove call to XtRealizeWidget(_markW)	*
 ***********************************************************************/
{
Widget	rc, pane;
Widget	pane_col, frame_col;
XmString  title;
char	 *buttonlist[] = {"OK", "Cancel"};

/*---------------------------------------------------------------------*/
/*
 * create marker attribute editing widget
 */
	_markW = XmCreateFormDialog(parent,
			"marker_attribute", NULL, 0);

	title = XmStringCreateLocalized("Marker Attribute");
	XtVaSetValues(_markW,
		XmNdialogTitle,		title, 
		XmNnoResize,    	True,
		NULL);
	XmStringFree(title);

	pane = XtVaCreateManagedWidget( "pane",
		xmPanedWindowWidgetClass, _markW,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL );

	rc = XtVaCreateWidget("rc1",
		xmRowColumnWidgetClass,  pane,
		XmNorientation, 	 XmHORIZONTAL,
		XmNspacing,		 10,
		NULL);

/*
 * create marker type selection area
 */
	NxmMarkA_createType(rc);

/*
 * create width and size selection area
 */
	NxmMarkA_createSzWth(rc);

/*
 * create marker color selection area
 */
	frame_col = XtVaCreateManagedWidget("frame_col",
		xmFrameWidgetClass,	 pane,
		XmNmarginWidth, 	 30,
		NULL );

	pane_col = XtVaCreateManagedWidget( "pane_col",
		xmPanedWindowWidgetClass, frame_col,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL );

	NxmLabel_createFrameLbl("color", pane_col, frame_col );

	_colrStr = NxmColrP_create(pane_col, 4, 1,
					(XtEventHandler)NxmMarkA_colorCb);

/*
 * create control buttons
 */	
	NxmCtlBtn_create( pane, 1, "cntrlBtn", XtNumber(buttonlist),
			  buttonlist, (XtCallbackProc)NxmMarkA_popdown, NULL);

	XtManageChild(rc);
	XtManageChild(pane);

	return(_markW);

}

/*=====================================================================*/

void NxmMarkA_createType ( Widget parent )
/************************************************************************
 * NxmMarkA_createType							*
 *									*
 * This function creates the marker type selection area 		*
 *									*
 * void NxmMarkA_createType(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * S. Wang/GSC		09/97						*
 * W. Li/EAI		12/97	Add "marker + text" radio box. 		*
 * M. Li/SAIC		12/01	Changed the path/file names of markers	*
 ***********************************************************************/
{
Widget		frame, pane;
Widget		pulldown, option_menu, radio_box;
long		ii, ignore, nn;
int		iret;
Pixel		fg, bg;
Pixmap		mark_pxm[TOTAL_MARKER];
Arg		args[5], args1[5];
XmString	str;
char		filename[256], warning[200], type_id[100];
char		*state_list[] = {"marker only", "marker + text", "text only"}; 
char		*type_list[] = { "marker1.xbm",  "marker2.xbm",  "marker3.xbm",
			 "marker4.xbm",  "marker5.xbm",   "marker6.xbm",
			 "marker7.xbm",  "marker8.xbm",   "marker9.xbm",
			 "marker10.xbm", "marker11.xbm",  "marker12.xbm",
			 "marker13.xbm", "marker14.xbm",  "marker15.xbm",
			 "marker16.xbm", "marker17.xbm",  "marker18.xbm",
			 "marker19.xbm", "marker20.xbm",  "marker21.xbm" };

/*---------------------------------------------------------------------*/

	_typeBtn = (WidgetList)XtMalloc(TOTAL_MARKER * sizeof(Widget));

/*
 * create type selection option menu
 */
	frame = XtVaCreateManagedWidget("frame_type",
		 xmFrameWidgetClass,	 parent,
		 NULL);

	pane = XtVaCreateManagedWidget("pane",
		xmPanedWindowWidgetClass, frame,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		XmNmarginHeight,	  10,
		XmNmarginWidth,	 	  10,
		XmNspacing,	 	  30,
		NULL);

	NxmLabel_createFrameLbl("type", pane, frame );

	nn =0;
	XtSetArg(args[nn],XmNorientation, XmHORIZONTAL); nn++;
	XtSetArg(args[nn],XmNpacking, XmPACK_COLUMN); nn++;
	XtSetArg(args[nn],XmNnumColumns, 7); nn++;
	
	pulldown = XmCreatePulldownMenu(pane, "opt_menu", args, nn);
	str = XmStringCreateLocalized("  ");

	nn = 0;
	XtSetArg(args1[nn], XmNsubMenuId, pulldown); nn++;
	XtSetArg(args1[nn], XmNlabelString, str); nn++;

	option_menu = XmCreateOptionMenu(pane, "  ", args1, nn);

	XmStringFree(str);

	XtVaGetValues (pane,
		XmNforeground,		&fg,
		XmNbackground,		&bg,
		NULL);

	for ( ii=0; ii<TOTAL_MARKER; ii++ ) {
	    sprintf( type_id, "%ld", (ii+1) );
	    cfl_inqr(type_list[ii], ICON_DIR, &ignore, filename, &iret );

	    mark_pxm[ii] = XmGetPixmap( XtScreen(parent),
					filename, fg, bg );

	    if ( mark_pxm[ii] == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
		sprintf( warning, "cannot load pixmap file %s",
					filename );
		NxmWarn_show(parent, warning);
		_typeBtn[ii] = XtVaCreateManagedWidget( type_id, 
		     xmPushButtonWidgetClass,	pulldown,
		     NULL);
	    }
	    else {
		_typeBtn[ii] = XtVaCreateManagedWidget( type_id, 
		    xmPushButtonWidgetClass,	pulldown,
		    XmNlabelType,		XmPIXMAP,
		    XmNlabelPixmap,		mark_pxm[ii],
		    NULL);
	    }

	    XtAddCallback(_typeBtn[ii], XmNactivateCallback, 
			  (XtCallbackProc)NxmMarkA_typeCb, (XtPointer)ii);

	}

	XtManageChild(option_menu);

/*
 * get cascade button id
 */
	_curType = XmOptionButtonGadget(option_menu);
	
/*
 * create marker/text toggle buttons		
 */

	radio_box = XmCreateRadioBox (pane, "radio_box",
				NULL, 0);
	nn = XtNumber(state_list);
	_stateBtn = (WidgetList) XtMalloc( nn * sizeof(Widget));

	for (ii=0; ii<nn; ii++){
	    _stateBtn[ii] = XtVaCreateManagedWidget (state_list[ii],
			 xmToggleButtonGadgetClass, radio_box, NULL, 0);
	    XtAddCallback(_stateBtn[ii], XmNvalueChangedCallback,
			    (XtCallbackProc)NxmMarkA_stateCb, 
			    (XtPointer)ii );
	}	

	XtManageChild(radio_box); 
}

/*=====================================================================*/

void NxmMarkA_createSzWth ( Widget parent )
/************************************************************************
 * NxmMarkA_createSzWth 						*
 *									*
 * This function creates the marker size and width selection area	*
 *									*
 * void NxmMarkA_createSzWth(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Wang/GSC		09/97						*
 * W. Li/EAI		12/97	Changed size range of marker and width.	*
 * W. Li/EAI		06/98  	Added text size 			*
 * C. Lin/EAI		08/98  	modified to use ctb_fszXXX 		*
 ***********************************************************************/
{
Widget	rc, frame1, frame2, frame3, menu_row_col;
Widget	pane1, pane2, pane3, menu_bar_size;
int	n, iret;
long	ii;
char    fsznam[20];
/*---------------------------------------------------------------------*/


	rc = XtVaCreateWidget("rc",
		 xmRowColumnWidgetClass,  parent,
		 XmNorientation,	  XmVERTICAL,
		 NULL);

	frame1 = XtVaCreateManagedWidget("frame1",
		 xmFrameWidgetClass,	 rc,
		 NULL);
	pane1 = XtVaCreateManagedWidget("pane1",
		xmPanedWindowWidgetClass, frame1,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL);

	NxmLabel_createFrameLbl("size", pane1, frame1 );
	_sizeScale = XtVaCreateManagedWidget("size",	
		xmScaleWidgetClass,	   pane1,
		XmNorientation, 	   XmHORIZONTAL,
		XmNshowValue,		   True,
		XmNminimum,		   5,
		XmNmaximum,		   30,
		XmNvalue,		   10,
		XmNscaleMultiple,	   1,
		XmNdecimalPoints,	   1,
		NULL);
	XtAddCallback(_sizeScale, XmNdragCallback,
		      (XtCallbackProc)NxmMarkA_sizeCb, NULL);
	XtAddCallback(_sizeScale, XmNvalueChangedCallback,
		      (XtCallbackProc)NxmMarkA_sizeCb, NULL);

	frame2 = XtVaCreateManagedWidget("frame2",
		 xmFrameWidgetClass,	 rc,
		 NULL);
	pane2 = XtVaCreateManagedWidget("pane2",
		xmPanedWindowWidgetClass, frame2,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL);

	NxmLabel_createFrameLbl("width", pane2, frame2 );
	_widthScale = XtVaCreateManagedWidget("width",	
		xmScaleWidgetClass,	   pane2,
		XmNorientation, 	   XmHORIZONTAL,
		XmNshowValue,		   True,
		XmNminimum,		   1,
		XmNmaximum,		   5,
		XmNvalue,		   1,
		XmNscaleMultiple,	   1,	
		NULL);
	XtAddCallback(_widthScale, XmNdragCallback,
		      (XtCallbackProc)NxmMarkA_widthCb, NULL);
	XtAddCallback(_widthScale, XmNvalueChangedCallback,
		      (XtCallbackProc)NxmMarkA_widthCb, NULL);

	frame3 = XtVaCreateManagedWidget("frame3",
		 xmFrameWidgetClass,	 rc,
		 NULL);
	pane3 = XtVaCreateManagedWidget("pane3",
		xmPanedWindowWidgetClass, frame3,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL);

	NxmLabel_createFrameLbl("text size", pane3, frame3 );

        menu_row_col = XtVaCreateManagedWidget ("menu_row_col",
  		xmRowColumnWidgetClass,	    pane3,  
		XmNpacking,		    XmPACK_COLUMN,
		XmNnumColumns,		    2,
		XmNorientation,		    XmHORIZONTAL,
		XmNisAligned,		    TRUE,
		XmNentryAlignment,	    XmALIGNMENT_END,
		NULL);
        menu_bar_size  = XmCreatePulldownMenu (menu_row_col, "Size", NULL, 0);
        _sizeMenuW   = XmCreateOptionMenu (menu_row_col, "sizeMenu", NULL, 0);

        ctb_fszqn(&n, &iret);
        _txtSizeW  = (WidgetList)XtMalloc((size_t)n * sizeof(Widget));

        for (ii=0; ii<n; ii++) {
	    ctb_fsznam(ii, fsznam, &iret);
            _txtSizeW[ii] = XtVaCreateManagedWidget(fsznam,
                xmPushButtonWidgetClass,    menu_bar_size,
		NULL);
            XtAddCallback(_txtSizeW[ii], XmNactivateCallback,
            	          (XtCallbackProc)NxmMarkA_txtsizeCb, 
			  (XtPointer)ii );
        }

        XtVaSetValues (_sizeMenuW, 
		XmNsubMenuId, 		    menu_bar_size,
		NULL);
        XtManageChild(_sizeMenuW);

	XtManageChild(rc);	
}

/*=====================================================================*/

void NxmMarkA_popup ( NxmMarkA_t *mk_info, void (*apply_func)(void) )
/************************************************************************
 * NxmMarkA_popup							*
 *									*
 * This function creates the editing panel popup function.		*
 *									*
 * void NxmMarkA_popup(mk_info, apply_func)				*
 *									*
 * Input parameters:							*
 *	*mk_info	NxmMarkA_t	marker editing data		*
 *	*apply_func()	void	application func. called when OK	*
 *				button is clicked			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * S. Wang/GSC		09/97						*
 * W. Li/EAI		12/97	Add initialization for marker + text.	*
 * W. Li/EAI		06/98	Add initialization for text size.	*
 * C. Lin/EAI		08/98	modify to use ctb_fszfnd.		*
 * M. Li/GSC		08/00	changed the position of XtSetSensitive	*
 * J. Wu/SAIC		08/03	correct the rounding of marker size	*
 * H. Zeng/SAIC		10/04	used new pointer _mkAttrOrig		*
 ***********************************************************************/
{
int	  i, isize, size_id, show_id, iret;
Boolean	  show_mark[] = {True, True, False };
Boolean	  show_txt [] = {False, True, True };
Pixmap	  cur_pxm;
XmString  str;
char	  temp[20];

/*---------------------------------------------------------------------*/

	if ( _markW == NULL )
	    return;

	if ( XtIsManaged(_markW))
	    XtUnmanageChild(_markW);

/*
 * assign marker and text editing pointer and also copy
 * the original data to a local copy.
 */
	_mkAttrOrig =  mk_info;
	_mkAttrEdit = *mk_info;

/*
 * copy original data 
 */
	_mkAttrCopy= _mkAttrEdit;

/*
 * initialization
 */
	show_id = _mkAttrCopy.state;

/*
 * show_id = 0, mark_only;
 * show-id = 1, mark + text;
 * show_id = 2, text_only;
 */
	for (i = 0; i < 3; i++) {
	    if (i == (_mkAttrCopy.state)) {
	        XmToggleButtonGadgetSetState( _stateBtn[i], TRUE, FALSE);
	    }
	    else {
		XmToggleButtonGadgetSetState( _stateBtn[i], FALSE, FALSE);
	    }	
	}

	NxmColrP_setColor(_colrStr, _mkAttrCopy.color);

	XtVaGetValues(_typeBtn[(_mkAttrCopy.type_id)-1], 
				XmNlabelPixmap, &cur_pxm, NULL);


	if ( cur_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
	    sprintf( temp,"%d", _mkAttrCopy.type_id );
	    str = XmStringCreateLocalized(temp);
	    XtVaSetValues( _curType, XmNlabelString, str, NULL );
	    XmStringFree(str);
	}
	else {
	    XtVaSetValues(_curType, 
			XmNlabelType,	XmPIXMAP,
			XmNlabelPixmap, cur_pxm, 
			NULL); 
	}

	isize = (int)(_mkAttrCopy.size*10.0F + 0.5);
	XmScaleSetValue( _sizeScale, isize);
	XmScaleSetValue( _widthScale, _mkAttrCopy.width);

	ctb_fszfnd(_mkAttrCopy.txt_size, &size_id, &iret);

        XtVaSetValues (_sizeMenuW, 
         	XmNmenuHistory, _txtSizeW[size_id], 
		NULL);

	_applyFunc = apply_func;

	XtManageChild(_markW);

        XtSetSensitive(_sizeScale, show_mark[show_id]);
        XtSetSensitive(_widthScale, show_mark[show_id]);
        XtSetSensitive(_sizeMenuW, show_txt[show_id]);

}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_popdown ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmMarkA_popdown							*
 *									*
 * This function creates the editing panel popup function		*
 *									*
 * void NxmMarkA_popdown(w, which, cbs)					*
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
 ** Log:								*
 *	S. Wang/GSC		09/97					*
 *	H. Zeng/SAIC		10/04	used new pointer _mkAttrOrig	*
 ***********************************************************************/
{
	XtUnmanageChild(_markW);

	if ( which == 0 ) {

	  *_mkAttrOrig = _mkAttrEdit;
	  if ( _applyFunc ) _applyFunc();
	}
	else {

	  *_mkAttrOrig = _mkAttrCopy;
	}
}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_colorCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmMarkA_colorCb							*
 *									*
 * This is the callback function for color selection			*
 *									*
 * void NxmMarkA_colorCb(w, which, cbs)				*
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
 *	S. Wang/GSC		09/97					*
 ***********************************************************************/
{
	_mkAttrEdit.color = (int)which;

	NxmColrP_setColor(_colrStr, (int)which);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_typeCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmMarkA_typeCb							*
 *									*
 * This is the callback function for type selection			*
 *									*
 * void NxmMarkA_typeCb(w, which, cbs) 					*
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
 *	S. Wang/GSC		09/97					*
 ***********************************************************************/
{
    _mkAttrEdit.type_id = (int)(which+1);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_sizeCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs ) 
/************************************************************************
 * NxmMarkA_sizeCb							*
 *									*
 * This is the callback function for size selection			*
 *									*
 * void NxmMarkA_sizeCb ( w, clnt, cbs ) 				*
 *									*
 * Input parameters:							*
 *	w	   Widget			widget ID		*
 *	clnt	   XtPointer			client data		*
 *	*cbs	   XmScaleCallbackStruct	resource		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/97						*
 ***********************************************************************/
{

    if (cbs != (XmScaleCallbackStruct *) NULL)
	_mkAttrEdit.size = (float)cbs->value/10.0F;

}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_widthCb ( Widget w, long which, XmScaleCallbackStruct *cbs )
/************************************************************************
 * NxmMarkA_widthCb							*
 *									*
 * This is the callback function for width selection			*
 *									*
 * void NxmMarkA_widthCb(w, which, cbs)					*
 *									*
 * Input parameters:							*
 *	w	   Widget			widget ID		*
 *	which	   long				widget button		*
 *	*cbs	   XmScaleCallbackStruct	resource		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *	S. Wang/GSC		09/97					*
 ***********************************************************************/
{
	if (cbs != (XmScaleCallbackStruct *) NULL)
		_mkAttrEdit.width = cbs->value;
}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_stateCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmMarkA_stateCb							*
 *									*
 * This is the callback function for state selection			*
 *									*
 * void NxmMarkA_stateCb(w, which, cbs)					*
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
 * S. Wang/GSC		09/97						*
 * W. Li/EAI		12/97	NxmLineA_onOffCb --> NxmMarkA_stateCb	*
 * W. Li/EAI		06/98	Set sensitive for marker and text size	*
 ***********************************************************************/
{
Boolean	  show_mark[] = {True, True, False };
Boolean	  show_txt [] = {False, True, True };
/*---------------------------------------------------------------------*/

	_mkAttrEdit.state = (int)which;

/*
 * which = 0, mark_only;
 * which = 1, mark + text;
 * which = 2, text_only;
 */
	XtSetSensitive(_sizeScale, show_mark[which]);
	XtSetSensitive(_widthScale, show_mark[which]);
	XtSetSensitive(_sizeMenuW, show_txt[which]);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmMarkA_txtsizeCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmMarkA_txtsizeCb							*
 *									*
 * This is the callback function for size selection			*
 *									*
 * void NxmMarkA_txtsizeCb(w, which, cbs) 				*
 *									*
 * Input parameters:							*
 *	w	  	Widget			widget ID		*
 *	which	   	long			widget button		*
 *	cbs	  	XtPointer		resource		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * W. Li/EAI		06/98						*
 * C. Lin/EAI		08/98 	rewrite to call ctb_fszval		*
 ***********************************************************************/
{
    int iret;
/*---------------------------------------------------------------------*/

    ctb_fszval((int)which, &(_mkAttrEdit.txt_size), &iret);
}

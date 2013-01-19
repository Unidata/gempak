#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgcmn.h"
#include "proto_nmaplib.h"

static Widget	   _scaleW;      /* scale attribute panel widget  */
static Widget	   _valueTxtW;   /* value text box      */
static Widget	   _latiTxtW;    /* latitude text box   */

static  struct  optMenuStrc     _posStrc;
static	struct	optMenuStrc	_unitStrc;
static	struct	optMenuStrc	_valueStrc;
static	struct	optMenuStrc	_latiStrc;
static  struct  optMenuStrc     _fontStrc;
static	struct	optMenuStrc	_sizeStrc;
static	struct	optMenuStrc	_styleStrc;

static  char    *_posStr[] =   {"Upper Left", "Upper Center", "Upper Right", 
			        "Lower Left", "Lower Center", "Lower Right" };
static  char	*_unitStr[] =  {"SM", "NM", "KM"};
static  char	*_valueStr[] = {"Auto", "Manual"};
static  char	*_latiStr[]  = {"Auto", "Manual"};
static  char    *_fontStr[] =  {"Courier", "Helvetica", "Times" };
static  char	*_sizeStr[] =  {"Tiny", "Small", "Medium", "Large", 
				"Huge", "Giant" };
static  char	*_styleStr[] = {"Regular", "Italic", "Bold", "B_Italic"};

static NxmScaleA_t   *_scAttrOrig;  /* scale attrib. pointer */
static NxmScaleA_t    _scAttrEdit;  /* scale editing data */
static NxmScaleA_t    _scAttrCopy;  /* scale saved data */
static NxmColrP_t    *_colrStr;	    /* color editing data structure  */


/*
 *  Private functions
 */
void NxmScaleA_colorCb ( Widget, long which, XtPointer );
void NxmScaleA_popdown ( Widget, long which, XtPointer );
void NxmScaleA_optPbCb ( Widget, long which, XtPointer );
void NxmScaleA_txtCb   ( Widget, long which, XtPointer );

static void (*_applyFunc)(void);    /* application applying function */


/************************************************************************
 * NxmScaleA.c								*
 *									*
 * This module makes the marker attribute editing panel 		*
 *									*
 * CONTENTS:								*
 *									*
 * NxmScaleA_create()	   create the marker attribute editing widget	*
 *									*
 * NxmScaleA_isUp()	   query if the attribute window is displayed	*
 * NxmScaleA_popup()	   pops up the editing widget			*
 * NxmScaleA_popdown()	   pops down the editing widget 		*
 *									*
 * NxmScaleA_updtLat()	   updates the lat value on the GUI		*
 * NxmScaleA_colorCb()	   color selection callback function		*
 * NxmScaleA_optPbCb()	   option button callback function		*
 ***********************************************************************/

/*=====================================================================*/

void   NxmScaleA_create ( Widget parent )
/************************************************************************
 * NxmScaleA_create							*
 *									*
 * This function create the scale attribute window			*
 *									*
 * Widget NxmScaleA_create(parent) 					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *  H. Zeng/SAIC	08/04	initial coding				*
 *  H. Zeng/SAIC	10/04	added lat menu				*
 ***********************************************************************/
{
Widget	  pane, pane_col, frame_col, form1, form2;
int	  mm;
XmString  title;
char	  *buttonlist[] = {"Accept", "Cancel"};

/*---------------------------------------------------------------------*/
/*
 * create marker attribute editing widget
 */
	_scaleW = XmCreateFormDialog(parent,
			"scale_attribute", NULL, 0);

	title = XmStringCreateLocalized("Scale Attribute");
	XtVaSetValues(_scaleW,
		XmNdialogTitle,		title, 
		XmNnoResize,    	True,
		NULL);
	XmStringFree(title);

	pane = XtVaCreateManagedWidget( "pane",
		xmPanedWindowWidgetClass, _scaleW,
		XmNsashWidth,		  1,
		XmNsashHeight,		  1,
		NULL );

	form1 = XtVaCreateWidget("form1",
		xmFormWidgetClass,	pane,
		NULL);

        mm = XtNumber (_posStr);
        _posStrc.current = 0;
        pgutls_createOptionMenu (form1, mm, (XtPointer)&_posStrc.current, 
			         "Position", (XtCallbackProc)NxmScaleA_optPbCb, &_posStrc.form, 
				 &_posStrc.label, &_posStrc.menu, _posStrc.pb, 
				 _posStr);

        XtVaSetValues (_posStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_posStrc.form, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        mm = XtNumber (_unitStr);
        _unitStrc.current = 0;
        pgutls_createOptionMenu (form1, mm, (XtPointer)&_unitStrc.current, 
			         "Unit", (XtCallbackProc)NxmScaleA_optPbCb, &_unitStrc.form, 
				 &_unitStrc.label, &_unitStrc.menu, _unitStrc.pb, 
				 _unitStr);

        XtVaSetValues (_unitStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_unitStrc.form, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNleftAttachment,   XmATTACH_WIDGET,
		       XmNleftWidget,	    _posStrc.form,
		       XmNleftOffset,	    20,
		       NULL);

        mm = XtNumber (_valueStr);
        _valueStrc.current = 0;
        pgutls_createOptionMenu (form1, mm, (XtPointer)&_valueStrc.current, 
			         "Value    ", (XtCallbackProc)NxmScaleA_optPbCb, 
				 &_valueStrc.form, &_valueStrc.label, 
			         &_valueStrc.menu, _valueStrc.pb, 
				 _valueStr);

        XtVaSetValues (_valueStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_valueStrc.form, 
		       XmNtopAttachment,    XmATTACH_WIDGET,
		       XmNtopWidget,	    _posStrc.form,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        _valueTxtW = XtVaCreateManagedWidget ("value_text",
		       xmTextFieldWidgetClass,   form1,
		       XmNcolumns,	         12,
		       XmNtopAttachment,         XmATTACH_OPPOSITE_WIDGET,
		       XmNtopWidget,	         _valueStrc.form,
		       XmNleftAttachment,        XmATTACH_WIDGET,
		       XmNleftWidget,		 _valueStrc.form,
		       XmNleftOffset,		 15,
		       NULL );

        XtAddCallback (_valueTxtW, XmNlosingFocusCallback, 
		       (XtCallbackProc)NxmScaleA_txtCb, (XtPointer)0 );

        mm = XtNumber (_latiStr);
        _latiStrc.current = 0;
        pgutls_createOptionMenu (form1, mm, (XtPointer)&_latiStrc.current, 
			         "Latitude", (XtCallbackProc)NxmScaleA_optPbCb, 
				 &_latiStrc.form, &_latiStrc.label, 
			         &_latiStrc.menu, _latiStrc.pb, 
				 _latiStr);

        XtVaSetValues (_latiStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_latiStrc.form, 
		       XmNtopAttachment,    XmATTACH_WIDGET,
		       XmNtopWidget,	    _valueStrc.form,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        _latiTxtW = XtVaCreateManagedWidget ("latitude_text",
		       xmTextFieldWidgetClass,   form1,
		       XmNcolumns,	         12,
		       XmNtopAttachment,         XmATTACH_OPPOSITE_WIDGET,
		       XmNtopWidget,	         _latiStrc.form,
		       XmNleftAttachment,        XmATTACH_WIDGET,
		       XmNleftWidget,		 _latiStrc.form,
		       XmNleftOffset,		 15,
		       NULL );

        XtAddCallback (_latiTxtW, XmNlosingFocusCallback, 
		       (XtCallbackProc)NxmScaleA_txtCb, (XtPointer)1 );

	XtManageChild (form1);

/*
 * create text attributes area
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

	NxmLabel_createFrameLbl("Text Attributes", pane_col, frame_col );

        form2 = XtVaCreateWidget("form2", 
			         xmFormWidgetClass, pane_col, NULL);

        mm = XtNumber (_fontStr);
        _fontStrc.current = 0;
        pgutls_createOptionMenu (form2, mm, (XtPointer)&_fontStrc.current, 
			         "Font", (XtCallbackProc)NxmScaleA_optPbCb, &_fontStrc.form, 
				 &_fontStrc.label, &_fontStrc.menu, 
				 _fontStrc.pb, _fontStr);

        XtVaSetValues (_fontStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_fontStrc.form, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        mm = XtNumber (_sizeStr);
        _sizeStrc.current = 0;
        pgutls_createOptionMenu (form2, mm, (XtPointer)&_sizeStrc.current, 
			         "Size", (XtCallbackProc)NxmScaleA_optPbCb, &_sizeStrc.form, 
				 &_sizeStrc.label, &_sizeStrc.menu, 
				 _sizeStrc.pb, _sizeStr);

        XtVaSetValues (_sizeStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_sizeStrc.form, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNleftAttachment,   XmATTACH_WIDGET,
		       XmNleftWidget,	    _fontStrc.form,
		       XmNleftOffset,	    20,
		       NULL);

        mm = XtNumber (_styleStr);
        _styleStrc.current = 0;
        pgutls_createOptionMenu (form2, mm, (XtPointer)&_styleStrc.current, 
			         "Style", (XtCallbackProc)NxmScaleA_optPbCb, &_styleStrc.form, 
				 &_styleStrc.label, &_styleStrc.menu, 
				 _styleStrc.pb, _styleStr);

        XtVaSetValues (_styleStrc.label, 
		       XmNtopAttachment,    XmATTACH_FORM,
		       XmNtopOffset,        10,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

        XtVaSetValues (_styleStrc.form, 
		       XmNtopAttachment,    XmATTACH_WIDGET,
		       XmNtopWidget,	    _fontStrc.form,
		       XmNleftAttachment,   XmATTACH_FORM,
		       NULL);

	XtManageChild (form2);

/*
 * create color selection area
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

	NxmLabel_createFrameLbl("Color", pane_col, frame_col );

	_colrStr = NxmColrP_create(pane_col, 4, 1, 
					(XtEventHandler)NxmScaleA_colorCb);

/*
 * create control buttons
 */	
	NxmCtlBtn_create( pane, 1, "cntrlBtn", XtNumber(buttonlist),
			  buttonlist, (XtCallbackProc)NxmScaleA_popdown, NULL);

	XtManageChild(pane);

}

/*=====================================================================*/

void NxmScaleA_popup ( NxmScaleA_t *sc_info, void (*apply_func)(void) )
/************************************************************************
 * NxmScaleA_popup							*
 *									*
 * This function popps up the Scale Attribute Window.			*
 *									*
 * void NxmScaleA_popup(sc_info, apply_func)				*
 *									*
 * Input parameters:							*
 *	*sc_info	NxmScaleA_t	scale editing data		*
 *	*apply_func()	void	application func. called when OK	*
 *				button is clicked			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * H. Zeng/SAIC		08/04	initial coding				*
 * H. Zeng/SAIC		10/04	added lat menu initialization		*
 ***********************************************************************/
{
char	lati_txt[16];
int	iret, one = 1, size_id;
float	cenlat, cenlon, pxl, pyb, pxr, pyt, pxo, pyo;

/*---------------------------------------------------------------------*/

	if ( _scaleW == NULL )
	    return;

	if ( XtIsManaged(_scaleW))
	    XtUnmanageChild(_scaleW);

/*
 * copy scale attributes and pointer to a local global.
 */
        _scAttrOrig = sc_info;

	_scAttrEdit = *sc_info;
        strcpy (_scAttrEdit.value_txt, sc_info->value_txt);

/*
 * copy original data 
 */
	_scAttrCopy= _scAttrEdit;
        strcpy (_scAttrCopy.value_txt, _scAttrEdit.value_txt);

/*
 * initialization
 */
	_posStrc.current = _scAttrCopy.pos;

        XtVaSetValues (_posStrc.menu, 
		   XmNmenuHistory,    _posStrc.pb[_posStrc.current], 
		   NULL);

	_unitStrc.current = _scAttrCopy.unit;

        XtVaSetValues (_unitStrc.menu, 
		   XmNmenuHistory,    _unitStrc.pb[_unitStrc.current], 
		   NULL);

	_valueStrc.current = _scAttrCopy.val_opt;

        XtVaSetValues (_valueStrc.menu, 
		   XmNmenuHistory,    _valueStrc.pb[_valueStrc.current], 
		   NULL);

        if ( _valueStrc.current == 0 ) {  /*  Auto mode for value  */

             XmTextFieldSetString(_valueTxtW, "\0");
	     XtVaSetValues(_valueTxtW,
                  XmNcursorPositionVisible, False,
                  NULL);
             XtSetSensitive (_valueTxtW, FALSE);
        }
        else {

             XmTextFieldSetString(_valueTxtW, _scAttrCopy.value_txt);
             XtVaSetValues(_valueTxtW,
                  XmNcursorPositionVisible, True,
                  NULL);
             XtSetSensitive (_valueTxtW, TRUE);
        }

	_latiStrc.current = _scAttrCopy.lat_opt;

        XtVaSetValues (_latiStrc.menu, 
		   XmNmenuHistory,    _latiStrc.pb[_latiStrc.current], 
		   NULL);

	if ( _latiStrc.current == 0 ) {  /* Auto mode for latitude  */
	    gqbnd(sys_P, &pxl, &pyb, &pxr, &pyt, &iret, strlen(sys_P) );
	    pxo = ( pxl + pxr ) / 2.0F;
	    pyo = ( pyb + pyt ) / 2.0F;
	    gtrans(sys_P, sys_M, &one, &pxo, &pyo, &cenlat, &cenlon, &iret,
						strlen(sys_P), strlen(sys_M ));
	sprintf (lati_txt, "%-9.2f", cenlat);
	    XmTextFieldSetString(_latiTxtW, lati_txt);
	    XtVaSetValues(_latiTxtW,
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
                  NULL);
        }
        else {
	    sprintf (lati_txt, "%-9.2f", _scAttrCopy.lat);
	    XmTextFieldSetString(_latiTxtW, lati_txt);
            XtVaSetValues(_latiTxtW,
                  XmNeditable,              True,
                  XmNcursorPositionVisible, True,
                  NULL);
        }

	_fontStrc.current = _scAttrCopy.font;

        XtVaSetValues (_fontStrc.menu, 
		   XmNmenuHistory,    _fontStrc.pb[_fontStrc.current], 
		   NULL);

	ctb_fszfnd(_scAttrCopy.size, &size_id, &iret);

	_sizeStrc.current = size_id;

        XtVaSetValues (_sizeStrc.menu, 
		   XmNmenuHistory,    _sizeStrc.pb[_sizeStrc.current], 
		   NULL);

	_styleStrc.current = _scAttrCopy.style;

        XtVaSetValues (_styleStrc.menu, 
		   XmNmenuHistory,    _styleStrc.pb[_styleStrc.current], 
		   NULL);

	NxmColrP_setColor(_colrStr, _scAttrCopy.color);

	_applyFunc = apply_func;

	XtManageChild(_scaleW);

}

/*=====================================================================*/
/* ARGSUSED */
void NxmScaleA_popdown ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmScaleA_popdown							*
 *									*
 * This function pops down the Scale Attribute Window.			*
 *									*
 * void NxmScaleA_popdown(w, which, cbs)				*
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
 * H. Zeng/SAIC		08/04	initial coding				*
 * H. Zeng/SAIC		10/04	added _scAttrOrig			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	XtUnmanageChild(_scaleW);

	if ( which == 0 ) {

	    *_scAttrOrig = _scAttrEdit;
            strcpy (_scAttrOrig->value_txt, _scAttrEdit.value_txt);

	    if ( _applyFunc ) _applyFunc();
	}
	else {

	    *_scAttrOrig = _scAttrCopy;
            strcpy (_scAttrOrig->value_txt, _scAttrCopy.value_txt);
	}
}

/*=====================================================================*/

Boolean NxmScaleA_isUp ( void )
/************************************************************************
 * NxmScaleA_isUp                                                       *
 *                                                                      *
 * This function queries whether the scale legend attribute window is	*
 *	displayed.							*
 *                                                                      *
 * Boolean NxmScaleA_isUp ()                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 * NxmScaleA_isUp          Boolean      True -- up,  False -- down      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	10/04	Created from pgvolw_isUp                *
 ***********************************************************************/
{
    return (XtIsManaged (_scaleW));
}

/*=====================================================================*/
/* ARGSUSED */
void NxmScaleA_colorCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmScaleA_colorCb							*
 *									*
 * This is the callback function for color selection			*
 *									*
 * void NxmScaleA_colorCb(w, which, cbs)				*
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
 *									*
 * H. Zeng/SAIC		08/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	_scAttrEdit.color = (int)which;

	NxmColrP_setColor(_colrStr, _scAttrEdit.color);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmScaleA_optPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * NxmScaleA_optPbCb							*
 *									*
 * Callback function for option menu push buttons.			*
 *									*
 * void NxmScaleA_optPbCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/04	initial coding				*
 * H. Zeng/SAIC		10/04	added latitude menu			*
 ***********************************************************************/
{
    int		   *optval, iret, one=1;
    char	   lati_txt[10];
    float          pxl, pyb, pxr, pyt, pxo, pyo, cenlat, cenlon;
    XtPointer	   userdata;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, XmNuserData, &userdata, NULL);
    optval = (int *)userdata;
    *optval = (int)which;

    if ( optval == &_posStrc.current ) {

	     _scAttrEdit.pos = (int)which;
    }
    else if ( optval == &_unitStrc.current ) {

	     _scAttrEdit.unit = (int)which;
    }
    else if ( optval == &_valueStrc.current ) {

	     _scAttrEdit.val_opt = (int)which;

/*   
 * Change the sensitivity of _valueTxtW according to the option.
 */
	     switch ( which ) {

	        case 0:  /*  Auto mode for values  */
                  XmTextFieldSetString(_valueTxtW, "\0");
                  XtVaSetValues(_valueTxtW,
                      XmNcursorPositionVisible, False,
                      NULL);
		  XtSetSensitive (_valueTxtW, FALSE);
                  break;

	        default:
                  if ( _scAttrEdit.value_txt[0] == '-' ) {
		      _scAttrEdit.value_txt[0] = '\0';
		  }		      
		  XmTextFieldSetString(_valueTxtW, _scAttrEdit.value_txt);
                  XtVaSetValues(_valueTxtW,
                      XmNcursorPositionVisible, True,
                      NULL);
		  XtSetSensitive (_valueTxtW, TRUE);
                  break;
             }
    }
    else if ( optval == &_latiStrc.current ) {

	     _scAttrEdit.lat_opt = (int)which;

/*   
 * Change the editability of _latiTxtW according to the option.
 */
	     switch ( which ) {

	        case 0:  /*  Auto mode for latitude  */

	       	    gqbnd(sys_P, &pxl, &pyb, &pxr, &pyt, &iret, strlen(sys_P) );
        	    pxo = ( pxl + pxr ) / 2.0F;
        	    pyo = ( pyb + pyt ) / 2.0F;
        	    gtrans(sys_P, sys_M, &one, &pxo, &pyo, &cenlat, &cenlon, 
				&iret, strlen(sys_P), strlen(sys_M ));
                    sprintf (lati_txt, "%-9.2f", cenlat );
                    XmTextFieldSetString(_latiTxtW, lati_txt);
		    XtVaSetValues(_latiTxtW,
                      XmNeditable,              False,
                      XmNcursorPositionVisible, False,
                      NULL);
                  break;

	        default:
                  if ( ERMISS(_scAttrEdit.lat) ) {

                     XmTextFieldSetString(_latiTxtW, "\0");
                  }
                  else {

                     sprintf (lati_txt, "%-9.2f", _scAttrEdit.lat);
                     XmTextFieldSetString(_latiTxtW, lati_txt);
                  }
	          XtVaSetValues(_latiTxtW,
                      XmNeditable,              True,
                      XmNcursorPositionVisible, True,
                      NULL);
                  break;
             }

    }
    else if ( optval == &_fontStrc.current ) {

	     _scAttrEdit.font = (int)which;
    }
    else if ( optval == &_sizeStrc.current ) {

	     ctb_fszval((int)which, &(_scAttrEdit.size), &iret);
    }
    else if ( optval == &_styleStrc.current ) {

	     _scAttrEdit.style = (int)which;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void NxmScaleA_txtCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * NxmScaleA_txtCb							*
 *									*
 * Callback function for the value and latitude text boxes.		*
 *									*
 * void NxmScaleA_txtCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which text box			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/04	initial coding				*
 * H. Zeng/SAIC		10/04	minor modification to lat text box	*
 ***********************************************************************/
{
    float	lat;
    int		lenstr, ier;
    char	*pstr, newstr[10];
/*---------------------------------------------------------------------*/
    switch ( which ) {

       case 0:  /*  Values  */
 
            pstr = XmTextGetString (_valueTxtW);

	    if ( pstr != NULL ) {

                 strncpy (_scAttrEdit.value_txt, pstr, 63);
	         _scAttrEdit.value_txt[63] = '\0';
	    }

            break;

       case 1:  /*  Latitude  */
 
            pstr = XmTextGetString (_latiTxtW);

            if ( sscanf (pstr, "%f", &lat) == 1 ) {

               if ( -85.0F <= lat && lat <= 85.0F ) {

	          sprintf (newstr, "%-9.2f", lat);
	          _scAttrEdit.lat = lat;
               }
               else {

	          sprintf (newstr, "%-9.2f", _scAttrEdit.lat);
               }
            }
            else {

	          sprintf (newstr, "%-9.2f", _scAttrEdit.lat);
            }

            cst_rmbl ( newstr, newstr, &lenstr, &ier );
            XmTextFieldSetString (_latiTxtW, newstr);

	    break;

    } /* the end of switch(... */

    if ( pstr != NULL )  XtFree (pstr);

}

/*=====================================================================*/

void NxmScaleA_updtLat ( void )
/************************************************************************
 * NxmScaleA_updtLat							*
 *									*
 * This function updates the latitude value on Scale Attribute Window.	*
 * when latitude is in AUTO mode.					*
 *									*
 * void NxmScaleA_updtLat ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * H. Zeng/SAIC		10/04	initial coding				*
 ***********************************************************************/
{
char	lati_txt[16];
int	ier, one = 1;
float	cenlat, cenlon, pxl, pyb, pxr, pyt, pxo, pyo;

/*---------------------------------------------------------------------*/

	if ( _latiStrc.current == 0 ) {  /* Auto mode for latitude  */

	    gqbnd(sys_P, &pxl, &pyb, &pxr, &pyt, &ier, strlen(sys_P) );
	    pxo = ( pxl + pxr ) / 2.0F;
	    pyo = ( pyb + pyt ) / 2.0F;
	    gtrans(sys_P, sys_M, &one, &pxo, &pyo, &cenlat, &cenlon, &ier,
						strlen(sys_P), strlen(sys_M ));
     
	    sprintf (lati_txt, "%-9.2f", cenlat);
	    XmTextFieldSetString(_latiTxtW, lati_txt);

        }
}

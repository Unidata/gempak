#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"

#define LOC_TBL      "locator.tbl"

struct optMenuStrc {
    int		*current, *save,  /* values for each position */
		*deflt, *newval;  /* values for each locator type */
    Widget	form;
    Widget	label;
    Widget	menu;
    WidgetList	pb;
};

static	struct	optMenuStrc	_posStrc;
static  char    **_posStr;
static  int     _posCurr, _posDeflt;
static  int     _posNum;

static  Boolean	*_onOffCurr, *_onOffSave;

static	struct	optMenuStrc	_locStrc;
static  char    **_locStr, **_locId;
static  int     *_locCode, _locNum;

static  Widget  _roundingTxtW, _roundingForm;
static  char    *_roundingStr[] = {"5", "10" };
static  int     *_roundingCurr,  *_roundingSave,   /* values for each position */
		*_roundingDeflt, *_roundingNewval; /* values for each locator type */

static	struct	optMenuStrc	_unitStrc;
static  char    *_unitStr[] = {"omit", "NM", "SM", "KM" };

static	struct	optMenuStrc	_dirStrc;
static  char    *_dirStr[] = {"omit", "16 point", "degrees" };

static	struct	optMenuStrc	_dspStrc;
static  char    *_dspStr[] = {"degrees", "decimal/minutes", 
                              "station ID", "station No.","station name" };

static Widget		_locfmtwWin, _onOffBtn;
static Widget		_locForm, _fmtForm;
static WidgetList       _fmtLabel;


/*
 *  private callback functions
 */
void locfmtw_ctlBtnCb ( Widget wid, long which, XtPointer call );
void locfmtw_dirOptCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void locfmtw_dspOptCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void locfmtw_roundingMenuCb ( Widget wid, long which, XtPointer call );
void locfmtw_roundingTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void locfmtw_unitOptCb ( Widget wid, XtPointer clnt, XtPointer cbs );
void locfmtw_onOffCb   ( Widget wid, XtPointer clnt, XtPointer cbs );
void locfmtw_posOptCb  ( Widget wid, XtPointer clnt, XtPointer cbs );

/************************************************************************
 * nmap_locfmtw.c							*
 *									*
 * This module defines a locator format edit popup window for nmap      *
 *									*
 * CONTENTS:								*
 *	locfmtw_create()	create the locator format edit window	*
 *	locfmtw_popup()		pop up the locator format edit window	*
 *	locfmtw_popdown()	popdown the locator format edit window	*
 *									*
 *	locfmtw_isUp()		query if the window is up 		*
 *      locfmtw_getPosNum()	get total number of locator pos.	*
 *	locfmtw_getPosOnoff()	get on/off info. for a loc. pos.	*
 *	locfmtw_getPosInfo()	get pos. info for a loc. pos.		*
 *	locfmtw_setCurrPos()	set the value of _posCurr		*
 *	locfmtw_getLocNum()	get total number of locator types	*
 *	locfmtw_getLocStr()	get pointer to a loc. type name str	*
 *	locfmtw_getLocId()	get pointer to a loc. type alias str	*
 *									*
 *      locfmtw_readLocTbl()    read locator format info. from table    *
 *                                                                      *
 *      locfmtw_locOptCb()      callback for locator option menu        *
 *      locfmtw_roundingMenuCb()callback for rounding option menu       *
 *      locfmtw_roundingTxtCb() callback for rounding text field        *
 *      locfmtw_unitOptCb()     callback for units option menu          *
 *      locfmtw_dirOptCb()      callback for direction option menu      *
 *      locfmtw_dspOptCb()      callback for display option menu        *
 *	locfmtw_ctlBtnCb()      callback for control buttons            *
 *	locfmtw_onOffCb()	callback for on/off button		*
 *	locfmtw_posOptCb()	callback for position option menu	*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void locfmtw_create ( Widget parent )
/************************************************************************
 * locfmtw_create							*
 *									*
 * This function creates the locator format edit popup window.		*
 *									*
 * void   locfmtw_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * M. Li/SAIC		12/01	arrow.xbm -> menu_arrow.xbm		*
 * M. Li/SAIC		07/03	made locator number unlimited		*
 * T. Piper/SAIC	10/05	declared ii long			*
 * H. Zeng/SAIC		12/06	added new GUI components		*
 ***********************************************************************/
{
    Widget	pane, form, button, formbtn;
    Widget      menub, menu, cascade;
    char	*btnstrs[] = {"OK", "Defaults", "Cancel"};
    int		nn, loff = 5, toff2 = 5, int_val, iret;
    XmString    xmstr;
    Pixel	fg, bg;
    long	ii, ignore;
    char	filename[256], tag_val[64];
    static Boolean    first = TRUE;
    static Pixmap     menu_pxm;
/*---------------------------------------------------------------------*/
    /*
     * create dialog shell
     */
    _locfmtwWin = XmCreateFormDialog ( parent, "locfmtw_popup",
				       NULL, 0);
    XtVaSetValues ( _locfmtwWin, 
		    XmNnoResize,        True, 
		    NULL);
    XtVaSetValues ( XtParent(_locfmtwWin),
		    XmNtitle, "LOCATOR SELECT and EDIT",
		    NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("locfmtw_pane",
			    xmPanedWindowWidgetClass, _locfmtwWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
			     
    /*
     *  create label widgets
     */
    nn = 6;
    _fmtLabel = (WidgetList)XtMalloc(nn*sizeof(Widget));

    /*
     * create position choosing area.
     */
    form = XtVaCreateWidget ( "form",
			       xmFormWidgetClass,  pane,
			       NULL );

    /*
     * POSITION
     */

    _fmtLabel[5] = XtVaCreateManagedWidget ("POSITION",
				     xmLabelWidgetClass,	form,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    /*
     * Create position menu
     */
    /*
     * get total number of locator positions from prefs.tbl.
     */
    _posNum = 1;
    ctb_pfstr ( "LOCATOR_POSITIONS", tag_val, &iret );
    if ( iret == 0 && sscanf(tag_val, "%d", &int_val) == 1) {

       _posNum = int_val;
       if ( _posNum > 10 )  _posNum = 10;
       if ( _posNum <  1 )  _posNum =  1;
    }

    _posDeflt = 0;
    _posCurr  = 0;

    _posStr = (char **) malloc ( _posNum * sizeof(char *) );	
    for ( ii = 0; ii < _posNum; ii++ ) {
       _posStr[ii] = (char *)XtMalloc(6);
       sprintf (_posStr[ii], "%d", (int)(ii+1));
    }

    _posStrc.pb = (WidgetList)XtMalloc(_posNum*sizeof(Widget));
    pgutls_createOptionMenu (form, _posNum, (XtPointer)&_posCurr, NULL, 
                             locfmtw_posOptCb, &_posStrc.form, &_posStrc.label,
			     &_posStrc.menu, _posStrc.pb, _posStr);
 
    XtVaSetValues (_posStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_fmtLabel[5],
                   XmNleftOffset,       loff,
		   NULL);

    /*
     * ON/OFF button
     */
    _onOffCurr  = (Boolean *) malloc (_posNum * sizeof (Boolean));
    _onOffSave  = (Boolean *) malloc (_posNum * sizeof (Boolean));
    for(ii = 0; ii< _posNum; ii++)  _onOffCurr[ii]  = (ii == 0);

    _onOffBtn = XtVaCreateManagedWidget("ON/OFF",
			    xmToggleButtonWidgetClass,	form,
			    XmNtraversalOn,		FALSE,
			    XmNtopAttachment,	        XmATTACH_FORM, 
			    XmNtopOffset,		toff2,
			    XmNleftAttachment,	        XmATTACH_WIDGET, 
			    XmNleftWidget,	        _posStrc.form,
			    XmNleftOffset,		loff+10,
			    NULL);

    XtAddCallback(_onOffBtn, XmNvalueChangedCallback, 
                  (XtCallbackProc)locfmtw_onOffCb, NULL);

    XtManageChild(form);


    /*
     * create locator type choosing area.
     */
    _locForm = XtVaCreateWidget ( "form1",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     * LOCATOR
     */

    _fmtLabel[0] = XtVaCreateManagedWidget ("LOCATOR",
				     xmLabelWidgetClass,	_locForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    /*
     * Create locator menu
     */
    nn = _locNum;
    _locStrc.pb = (WidgetList)XtMalloc(nn*sizeof(Widget));
    _locStrc.current = (int *) malloc (_posNum * sizeof (int));
    _locStrc.save    = (int *) malloc (_posNum * sizeof (int));
    for(ii = 0; ii< _posNum; ii++)  _locStrc.current[ii] = 0;

    pgutls_createOptionMenu (_locForm, nn, (XtPointer)&(_locStrc.current[_posCurr]), 
			     NULL, locfmtw_locOptCb, &_locStrc.form, &_locStrc.label,
			     &_locStrc.menu, _locStrc.pb, _locStr);
 
    XtVaSetValues (_locStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_fmtLabel[0],
                   XmNleftOffset,       loff+49,
		   NULL);

    XtManageChild(_locForm);

    /*
     * create format setting area.
     */
    _fmtForm = XtVaCreateWidget ( "form2",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     * ROUNDING 
     */
    xmstr = XmStringCreateLtoR("ROUNDING TO\nNEAREST", 
                               XmFONTLIST_DEFAULT_TAG );

    _fmtLabel[1] = XtVaCreateManagedWidget ("ROUNDING TO\nNEAREST",
				     xmLabelWidgetClass,	_fmtForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		0,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                                     XmNlabelString,            xmstr,
                         	     NULL);

    XmStringFree(xmstr);

    /*
     * create a form container for text menu
     */
    _roundingForm = XtVaCreateWidget("form",
			    xmFormWidgetClass,	_fmtForm,
                            XmNmarginWidth,     0,
			    NULL );

    /*
     * create text field 
     */
    _roundingCurr   = (int *) malloc (_posNum * sizeof (int));
    _roundingSave   = (int *) malloc (_posNum * sizeof (int));
    _roundingDeflt  = (int *) malloc (_locNum * sizeof (int));
    _roundingNewval = (int *) malloc (_locNum * sizeof (int));
    for(ii = 0; ii< _locNum; ii++) {
	_roundingDeflt[ii]  = _locCode[ii]/1000;
        _roundingNewval[ii] = _roundingDeflt[ii];
    } 
    for(ii = 0; ii< _posNum; ii++)  
        _roundingCurr[ii] = _roundingDeflt[_locStrc.current[ii]];

    _roundingTxtW = XtVaCreateManagedWidget ("text", 
			      xmTextFieldWidgetClass,   _roundingForm,
			      XmNcolumns,		3, 
			      XmNleftAttachment,	XmATTACH_FORM,
                              XmNleftOffset,            0,
			      NULL);

    XtAddCallback (_roundingTxtW, XmNlosingFocusCallback, 
		   (XtCallbackProc)locfmtw_roundingTxtCb, (XtPointer)NULL);

    /*
     * create menu
     */
    menub = XmCreateMenuBar (_roundingForm, "menubar", NULL, 0);

    XtVaSetValues (menub, 
		   XmNleftAttachment,		XmATTACH_WIDGET,
		   XmNleftWidget,		_roundingTxtW,
		   XmNleftOffset,		0,
		   XmNmarginHeight,		0,
		   XmNmarginWidth,		0,
		   XmNborderWidth,		0,
		   XmNwidth,			5,
		   XmNhighlightThickness,	1,
		   XmNshadowThickness,		1,
		   NULL);

    menu  = XmCreatePulldownMenu (menub, "menu", NULL, 0);

    cascade = XtVaCreateManagedWidget ("cascade", 
				       xmCascadeButtonWidgetClass, menub, 
				       XmNsubMenuId, menu, 
				       NULL);

    if (first) {
	first = FALSE;

	XtVaGetValues (_fmtForm,
		       XmNforeground,	&fg,
		       XmNbackground, 	&bg,
		       NULL);

	cfl_inqr ("menu_arrow.xbm", "$NAWIPS/icons/nmap", &ignore, filename, &iret);

	if (iret == 0) {
	    menu_pxm = XmGetPixmap (XtScreen (parent), filename, fg, bg);
	}
	else {
	    menu_pxm = XmUNSPECIFIED_PIXMAP;
	}
    }

    if (menu_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP) {
	xmstr = XmStringCreateLocalized ("\\/");

	XtVaSetValues (cascade,
		       XmNlabelString, xmstr,
		       NULL);

	XmStringFree (xmstr);
    }
    else {
	XtVaSetValues (cascade, 
		       XmNlabelType,		XmPIXMAP,
		       XmNlabelPixmap,		menu_pxm,
		       XmNmarginHeight,		0,
		       XmNmarginWidth,		0,
		       NULL);
    }

    nn = XtNumber(_roundingStr);

    for (ii = 0; ii < nn; ii++) {
	xmstr = XmStringCreateLocalized (_roundingStr[ii]);
	button = XtVaCreateManagedWidget ("button", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	xmstr,
					  NULL);
	XmStringFree (xmstr);
 
	XtAddCallback (button, XmNactivateCallback,
		       (XtCallbackProc)locfmtw_roundingMenuCb,
		       (XtPointer) ii);
	
    }

    XtManageChild(menub);
    XtVaSetValues (_roundingForm, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_fmtLabel[1],
                   XmNleftOffset,       loff+30,
		   NULL);
    XtManageChild(_roundingForm);

    /*
     * Create units menu
     */
    nn = XtNumber(_unitStr);
    _unitStrc.pb = (WidgetList)XtMalloc(nn*sizeof(Widget));
    _unitStrc.current = (int *) malloc (_posNum * sizeof (int));
    _unitStrc.save    = (int *) malloc (_posNum * sizeof (int));
    _unitStrc.deflt   = (int *) malloc (_locNum * sizeof (int));
    _unitStrc.newval  = (int *) malloc (_locNum * sizeof (int));
    for(ii = 0; ii< _locNum; ii++) {
	_unitStrc.deflt[ii]  = (_locCode[ii]%1000)/100;
        _unitStrc.newval[ii] = _unitStrc.deflt[ii];
    }
    for(ii = 0; ii< _posNum; ii++)
        _unitStrc.current[ii] = _unitStrc.deflt[_locStrc.current[ii]];

    pgutls_createOptionMenu (_fmtForm, nn, (XtPointer)&_unitStrc.current[_posCurr], 
			     NULL, locfmtw_unitOptCb, &_unitStrc.form, &_unitStrc.label,
			     &_unitStrc.menu, _unitStrc.pb, _unitStr);

    XtVaSetValues (_unitStrc.form, 
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        _roundingForm,
                   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		   XmNleftWidget,	_roundingForm,
                   XmNleftOffset,       -14,
		   NULL);

    /*
     * UNITS
     */

    _fmtLabel[2] = XtVaCreateManagedWidget ("DISTANCE UNITS",
				     xmLabelWidgetClass,	_fmtForm,
                                     XmNtopAttachment,		XmATTACH_OPPOSITE_WIDGET,
				     XmNtopWidget,		_unitStrc.form,
                                     XmNtopOffset,              toff2,
                                     XmNleftAttachment,         XmATTACH_OPPOSITE_WIDGET,
                                     XmNleftWidget,             _fmtLabel[1],
                         	     NULL);

    /*
     * Create direction menu
     */
    nn = XtNumber(_dirStr);
    _dirStrc.pb = (WidgetList)XtMalloc(nn*sizeof(Widget));
    _dirStrc.current = (int *) malloc (_posNum * sizeof (int));
    _dirStrc.save    = (int *) malloc (_posNum * sizeof (int));
    _dirStrc.deflt   = (int *) malloc (_locNum * sizeof (int));
    _dirStrc.newval  = (int *) malloc (_locNum * sizeof (int));
    for(ii = 0; ii< _locNum; ii++) {
	_dirStrc.deflt[ii]  = (_locCode[ii]%100)/10;
	_dirStrc.newval[ii] = _dirStrc.deflt[ii];
    }
    for(ii = 0; ii< _posNum; ii++) 
        _dirStrc.current[ii] = _dirStrc.deflt[_locStrc.current[ii]];

    pgutls_createOptionMenu (_fmtForm, nn, (XtPointer)&_dirStrc.current[_posCurr], 
			     NULL, locfmtw_dirOptCb, &_dirStrc.form, &_dirStrc.label,
			     &_dirStrc.menu, _dirStrc.pb, _dirStr);
 
    XtVaSetValues (_dirStrc.form, 
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        _unitStrc.form,
		   XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		   XmNleftWidget,	_unitStrc.form,
		   NULL);

    /*
     * DIRECTION
     */

    _fmtLabel[3] = XtVaCreateManagedWidget ("DIRECTION",
				     xmLabelWidgetClass,	_fmtForm,
                                     XmNtopAttachment,		XmATTACH_OPPOSITE_WIDGET,
				     XmNtopWidget,		_dirStrc.form,
                                     XmNtopOffset,              toff2,
                                     XmNleftAttachment,         XmATTACH_OPPOSITE_WIDGET,
                                     XmNleftWidget,             _fmtLabel[2],
                         	     NULL);

    /*
     * Create display menu
     */
    nn = XtNumber(_dspStr);
    _dspStrc.pb = (WidgetList)XtMalloc(nn*sizeof(Widget));
    _dspStrc.current = (int *) malloc (_posNum * sizeof (int));
    _dspStrc.save    = (int *) malloc (_posNum * sizeof (int));
    _dspStrc.deflt   = (int *) malloc (_locNum * sizeof (int));
    _dspStrc.newval  = (int *) malloc (_locNum * sizeof (int));
    for(ii = 0; ii< _locNum; ii++) {
	_dspStrc.deflt[ii]  = _locCode[ii]%10;
        _dspStrc.newval[ii] = _dspStrc.deflt[ii];
    }
    for(ii = 0; ii< _posNum; ii++) 
        _dspStrc.current[ii] = _dspStrc.deflt[_locStrc.current[ii]];

    pgutls_createOptionMenu (_fmtForm, nn, (XtPointer)&_dspStrc.current[_posCurr], 
			     NULL, locfmtw_dspOptCb, &_dspStrc.form, &_dspStrc.label,
			     &_dspStrc.menu, _dspStrc.pb, _dspStr);

    XtSetSensitive ( _dspStrc.pb[3],  FALSE );      
 
    XtVaSetValues (_dspStrc.form, 
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        _dirStrc.form,
		   XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET, 
		   XmNleftWidget,	_dirStrc.form,
		   NULL);

    /*
     * DISPLAY
     */

    _fmtLabel[4] = XtVaCreateManagedWidget ("DISPLAY",
				     xmLabelWidgetClass,	_fmtForm,
                                     XmNtopAttachment,		XmATTACH_OPPOSITE_WIDGET,
				     XmNtopWidget,		_dspStrc.form,
                                     XmNtopOffset,              toff2,
                                     XmNleftAttachment,         XmATTACH_OPPOSITE_WIDGET,
                                     XmNleftWidget,             _fmtLabel[3],
                         	     NULL);

    XtManageChild(_fmtForm);

    /*
     * create control buttons
     */
    nn = XtNumber ( btnstrs );
    formbtn = XtVaCreateWidget("formbtn",
                            xmFormWidgetClass, pane,
			    XmNfractionBase,   (nn * 100),
                            NULL                       ); 

    for ( ii = 0; ii < nn; ii++ )  {

	button = XtVaCreateManagedWidget ( btnstrs[ii], 
			xmPushButtonWidgetClass, formbtn,
			XmNheight,               25,
			XmNwidth,                100,
                        XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         (ii * 100),
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        ((ii + 1) * 100),
			NULL );

	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)locfmtw_ctlBtnCb, (XtPointer)ii );

    }


    XtManageChild(formbtn);
    XtManageChild(pane);

}

/*=====================================================================*/

void locfmtw_popup ( void )
/************************************************************************
 * locfmtw_popup				      			*
 *									*
 * This function pops up the locator format edit popup window.		*
 *									*
 * void locfmtw_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		12/06	changed argument list			* 
 ***********************************************************************/
{
int      ii;
/*---------------------------------------------------------------------*/

    /*
     * Save current values.
     */
    for(ii = 0; ii< _posNum; ii++) {
        _onOffSave[ii]     = _onOffCurr[ii];
        _locStrc.save[ii]  = _locStrc.current[ii];
        _roundingSave[ii]  = _roundingCurr[ii];
        _unitStrc.save[ii] = _unitStrc.current[ii];
        _dirStrc.save[ii]  = _dirStrc.current[ii];
        _dspStrc.save[ii]  = _dspStrc.current[ii];  
    }

    /*
     * reset the value of _posCurr to 0 and update the GUI.
     */
    _posCurr = 0;
    locfmtw_posOptCb (NULL, (XtPointer)(long)_posCurr, NULL);

    if( !(locfmtw_isUp()) ) {
        XtManageChild (_locfmtwWin);
    }
    
}

/*=====================================================================*/

Boolean locfmtw_isUp ( void ) 
/************************************************************************
 * locfmtw_isUp								*
 *									*
 * This function queries whether the locator format edit window is up.	*
 *									*
 * Boolean locfmtw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * locfmtw_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		01/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_locfmtwWin));
}

/*=====================================================================*/

void locfmtw_popdown ( void ) 
/************************************************************************
 * locfmtw_popdown							*
 *									*
 * This function pops down the locator format edit window.		*
 *									*
 * void locfmtw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		01/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_locfmtwWin)) {
	XtUnmanageChild (_locfmtwWin);

    }
    
}

/*=====================================================================*/

/* ARGSUSED */
void locfmtw_posOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_posOptCb							*
 *									*
 * This is the callback function for position option menu.		*
 *									*
 * void locfmtw_posOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	Current position		*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          12/06   initial coding                          *
 ***********************************************************************/
{
    char        textstr[5];
/*---------------------------------------------------------------------*/

    _posCurr = (long)clnt;

    /*
     * Set the current position
     */
    XtVaSetValues(_posStrc.menu,
                  XmNmenuHistory,    _posStrc.pb[_posCurr],
                  NULL  );


    /*
     * Set the format values.
     */
    if (_roundingCurr[_posCurr] == 0 ) {
        XmTextFieldSetString ( _roundingTxtW, "\0" );       
    }
    else {
        sprintf (textstr, "%-3d", _roundingCurr[_posCurr] );
        XmTextFieldSetString ( _roundingTxtW, textstr );
    }

    XtVaSetValues(_unitStrc.menu,
                  XmNmenuHistory,    
                  _unitStrc.pb[ _unitStrc.current[_posCurr] ],
                  NULL  );

    XtVaSetValues(_dirStrc.menu,
                  XmNmenuHistory,    
                  _dirStrc.pb[ _dirStrc.current[_posCurr] ],
                  NULL  );

    XtVaSetValues(_dspStrc.menu,
                  XmNmenuHistory,    
                  _dspStrc.pb[ _dspStrc.current[_posCurr] ],
                  NULL  );


    /*
     * Set the current locator type.
     */
    XtVaSetValues(_locStrc.menu,
                  XmNmenuHistory,    _locStrc.pb[_locStrc.current[_posCurr]],
                  NULL  );

    if ( _locStrc.current[_posCurr] == 0 ) {

       XmTextFieldSetString ( _roundingTxtW, "\0" );       
       XtSetSensitive ( _fmtLabel[1],  FALSE );      
       XtSetSensitive ( _roundingForm, FALSE );   
       XtSetSensitive ( _fmtLabel[2],  FALSE );
       XtSetSensitive ( _unitStrc.form,FALSE );   
       XtSetSensitive ( _fmtLabel[3],  FALSE );
       XtSetSensitive ( _dirStrc.form, FALSE );
    }
    else {

       XtSetSensitive ( _fmtLabel[1],  TRUE  );      
       XtSetSensitive ( _roundingForm, TRUE  ); 
       if (_roundingCurr[_posCurr] == 0 ) {
          XmTextFieldSetString ( _roundingTxtW, "\0" );       
       }
       else {
          sprintf (textstr, "%-3d", _roundingCurr[_posCurr] );
          XmTextFieldSetString ( _roundingTxtW, textstr );
       }   
       XtSetSensitive ( _fmtLabel[2],  TRUE  );
       XtSetSensitive ( _unitStrc.form,TRUE  );   
       XtSetSensitive ( _fmtLabel[3],  TRUE  );
       XtSetSensitive ( _dirStrc.form, TRUE  );  
    }

    /*
     * Make non-applicable choices insensitive for display
     */
    if( _locStrc.current[_posCurr] == 0) {

      XtSetSensitive ( _dspStrc.pb[0],  TRUE  ); 
      XtSetSensitive ( _dspStrc.pb[1],  TRUE  );
      XtSetSensitive ( _dspStrc.pb[2],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[4],  FALSE );     
    }
    else {

      XtSetSensitive ( _dspStrc.pb[0],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[1],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[2],  TRUE  );  
      XtSetSensitive ( _dspStrc.pb[4],  TRUE  );  
    }


    /*
     * Set ON/OFF button for current position.
     */
    XmToggleButtonSetState (_onOffBtn, _onOffCurr[_posCurr], TRUE);

}
/*=====================================================================*/

/* ARGSUSED */
void locfmtw_onOffCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_onOffCb                                                   	*
 *                                                                      *
 * Callback for on/off button widget.                                   *
 *                                                                      *
 * void locfmtw_onOffCb( wid, clnt, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid		Widget		Widget ID                       *
 *   clnt		XtPointer	not used                        *
 *   cbs		XtPointer	callback struct			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		12/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtVaGetValues(_onOffBtn, XmNset, &(_onOffCurr[_posCurr]), NULL);

    if ( _onOffCurr[_posCurr] ) {

       XtSetSensitive( _locForm, TRUE );
       XtSetSensitive( _fmtForm, TRUE );
    }
    else {

       XtSetSensitive( _locForm, FALSE );
       XtSetSensitive( _fmtForm, FALSE );
    }
   
}

/*=====================================================================*/

/* ARGSUSED */
void locfmtw_locOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_locOptCb							*
 *									*
 * This is the callback function for locator option menu.		*
 *									*
 * void locfmtw_locOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		12/06	modified for new GUI			*
 ***********************************************************************/
{
    int	  new_val;
    char  textstr[5];
/*---------------------------------------------------------------------*/

    /*
     * Check if there is a change to the locator type.
     */
    if (_locStrc.current[_posCurr] == (int)(long)clnt)  return;  


    /*
     * Set the current locator type.
     */
    _locStrc.current[_posCurr] = (long)clnt;
    XtVaSetValues(_locStrc.menu,
                  XmNmenuHistory, _locStrc.pb[_locStrc.current[_posCurr]],
                  NULL  );

    /*
     * Set the format values.
     */
    new_val = _roundingNewval[_locStrc.current[_posCurr]];
    if ( new_val == 0 ) {
        XmTextFieldSetString ( _roundingTxtW, "\0" );       
    }
    else {
        sprintf ( textstr, "%-3d", new_val );
        XmTextFieldSetString ( _roundingTxtW, textstr );
    }
    _roundingCurr[_posCurr] = new_val;

    new_val = _unitStrc.newval[_locStrc.current[_posCurr]];
    XtVaSetValues(_unitStrc.menu,
                  XmNmenuHistory,    
                  _unitStrc.pb[ new_val ],
                  NULL  );
    _unitStrc.current[_posCurr] = new_val;

    new_val = _dirStrc.newval[_locStrc.current[_posCurr]];
    XtVaSetValues(_dirStrc.menu,
                  XmNmenuHistory,    
                  _dirStrc.pb[ new_val ],
                  NULL  );
    _dirStrc.current[_posCurr] = new_val;

    new_val = _dspStrc.newval[_locStrc.current[_posCurr]];
    XtVaSetValues(_dspStrc.menu,
                  XmNmenuHistory,    
                  _dspStrc.pb[ new_val ],
                  NULL  );
    _dspStrc.current[_posCurr] = new_val;


    /*
     * Set the sensitivity for certain GUI component.
     */
    if ( _locStrc.current[_posCurr] == 0 ) {

       XmTextFieldSetString ( _roundingTxtW, "\0" );       
       XtSetSensitive ( _fmtLabel[1],  FALSE );      
       XtSetSensitive ( _roundingForm, FALSE );   
       XtSetSensitive ( _fmtLabel[2],  FALSE );
       XtSetSensitive ( _unitStrc.form,FALSE );   
       XtSetSensitive ( _fmtLabel[3],  FALSE );
       XtSetSensitive ( _dirStrc.form, FALSE );
    }
    else {

       XtSetSensitive ( _fmtLabel[1],  TRUE  );      
       XtSetSensitive ( _roundingForm, TRUE  ); 
       if (_roundingCurr[_posCurr] == 0 ) {
          XmTextFieldSetString ( _roundingTxtW, "\0" );       
       }
       else {
          sprintf (textstr, "%-3d", _roundingCurr[_posCurr] );
          XmTextFieldSetString ( _roundingTxtW, textstr );
       }   
       XtSetSensitive ( _fmtLabel[2],  TRUE  );
       XtSetSensitive ( _unitStrc.form,TRUE  );   
       XtSetSensitive ( _fmtLabel[3],  TRUE  );
       XtSetSensitive ( _dirStrc.form, TRUE  );  
    }


    /*
     * Make non-applicable choices insensitive for display
     */
    if( _locStrc.current[_posCurr] == 0) {

      XtSetSensitive ( _dspStrc.pb[0],  TRUE  ); 
      XtSetSensitive ( _dspStrc.pb[1],  TRUE  );
      XtSetSensitive ( _dspStrc.pb[2],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[4],  FALSE );     
    }
    else {

      XtSetSensitive ( _dspStrc.pb[0],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[1],  FALSE );  
      XtSetSensitive ( _dspStrc.pb[2],  TRUE  );  
      XtSetSensitive ( _dspStrc.pb[4],  TRUE  );  
    }

}
/*=====================================================================*/

/* ARGSUSED */
void locfmtw_roundingMenuCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * locfmtw_roundingMenuCb						*
 *									*
 * Callback function for rounding option menu.   			*
 *									*
 * void locfmtw_roundingMenuCb (wid, which, call)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		01/00	initial coding          		*
 * H. Zeng/SAIC		12/06	modified for new GUI component		*
 ***********************************************************************/
{
    int		new_val;
    char	*ptext;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, 
		   XmNlabelString,	&xmstr, 
		   NULL);

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);

    sscanf(ptext, "%d", &new_val);
    _roundingCurr[_posCurr] = new_val;
    _roundingNewval[_locStrc.current[_posCurr]] = new_val;

    XmTextFieldSetString ( _roundingTxtW, ptext );       

    XmStringFree (xmstr);
    XtFree (ptext);
   
}

/*=====================================================================*/

/* ARGSUSED */
void locfmtw_roundingTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_roundingTxtCb						*
 *									*
 * Callback function for the rounding text box.				*
 *									*
 * void locfmtw_roundingTxtCb (wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   intial coding                           *
 * H. Zeng/SAIC		01/07	modified for new GUI component		*
 ************************************************************************/
{
    int 	value;
    char	*ptext, textstr[5];
/*---------------------------------------------------------------------*/

    ptext = XmTextFieldGetString (_roundingTxtW);

    if( sscanf(ptext, "%d", &value) == 1 ) {
      if( value >= 1 ) {
        _roundingCurr[_posCurr] = value;
        _roundingNewval[_locStrc.current[_posCurr]] = value;
      }
      
    }

    if(_roundingCurr[_posCurr] >= 1) {
      sprintf (textstr, "%-3d", _roundingCurr[_posCurr] );
      XmTextFieldSetString ( _roundingTxtW, textstr );     
    }
  
    XtFree (ptext);

}

/*=====================================================================*/
/* ARGSUSED */
void locfmtw_unitOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_unitOptCb							*
 *									*
 * This is the callback function for unit option menu.		        *
 *									*
 * void locfmtw_unitOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		01/07	modified for new GUI component		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _unitStrc.current[_posCurr] = (long) clnt;
    _unitStrc.newval[_locStrc.current[_posCurr]] = (long) clnt;

}

/*=====================================================================*/
/* ARGSUSED */
void locfmtw_dirOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_dirOptCb							*
 *									*
 * This is the callback function for direction option menu.	        *
 *									*
 * void locfmtw_dirOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		01/07	modified for new GUI component		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _dirStrc.current[_posCurr] = (long)clnt;
    _dirStrc.newval[_locStrc.current[_posCurr]] = (long)clnt;

}

/*=====================================================================*/
/* ARGSUSED */
void locfmtw_dspOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * locfmtw_dspOptCb							*
 *									*
 * This is the callback function for display option menu.	        *
 *									*
 * void locfmtw_dspOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		01/07	modified for new GUI component		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _dspStrc.current[_posCurr] = (long)clnt;
    _dspStrc.newval[_locStrc.current[_posCurr]] = (long)clnt;

}

/*=====================================================================*/
/* ARGSUSED */
void locfmtw_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * locfmtw_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of locator format*
 * popup window.						        *
 *									*
 * void locfmtw_ctlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * H. Zeng/SAIC		01/07	modified for new GUI component		*
 ***********************************************************************/
{ 
int     ii;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* OK */
	locfmtw_popdown ();

	break;

      case 1:	/* Defaults */
        _roundingCurr[_posCurr] = _roundingDeflt[_locStrc.current[_posCurr]];
        _roundingNewval[_locStrc.current[_posCurr]] = _roundingCurr[_posCurr];

        _unitStrc.current[_posCurr] = _unitStrc.deflt[_locStrc.current[_posCurr]];
        _unitStrc.newval[_locStrc.current[_posCurr]] = _unitStrc.current[_posCurr];

        _dirStrc.current[_posCurr] = _dirStrc.deflt[_locStrc.current[_posCurr]];
        _dirStrc.newval[_locStrc.current[_posCurr]] = _dirStrc.current[_posCurr];

        _dspStrc.current[_posCurr] = _dspStrc.deflt[_locStrc.current[_posCurr]]; 
        _dspStrc.newval[_locStrc.current[_posCurr]] = _dspStrc.current[_posCurr];
      
        locfmtw_posOptCb (NULL, (XtPointer)(long)_posCurr, NULL);

	break;

      case 2:	/* Cancel */
        for(ii = 0; ii< _posNum; ii++) {

          _onOffCurr[ii]        = _onOffSave[ii];
          _locStrc.current[ii]  = _locStrc.save[ii];
          _roundingCurr[ii]     = _roundingSave[ii];
          _unitStrc.current[ii] = _unitStrc.save[ii];
          _dirStrc.current[ii]  = _dirStrc.save[ii];
          _dspStrc.current[ii]  = _dspStrc.save[ii];  
        }

	locfmtw_popdown();

	break;

    } /* the end of switch */

}

/*=====================================================================*/

int locfmtw_getPosNum ( void )
/************************************************************************
 * locfmtw_getPosNum							*
 *									*
 * This function gets total numbe of locator  positions.		*
 *									*
 * int  locfmtw_getPosNum()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * locfmtw_getPosNum	int     number of locator positions		*
 **									*
 * Log:									*
 * H. Zeng/SAIC         01/07   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return ( _posNum );

}

/*=====================================================================*/

Boolean locfmtw_getPosOnoff ( int index )
/************************************************************************
 * locfmtw_getPosOnoff							*
 *									*
 * This function gets on/off info a certain loc position index. 	*
 *									*
 * Boolean locfmtw_getPosOnoff ( index )				*
 *									*
 * Input parameters:							*
 *	index		int	index of locator position               *
 *									*
 * Return parameters:							*
 * locfmtw_getPosOnoff	Boolean TRUE  ----  position is on		*
 *				FALSE ----  position is off		*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( index < 0 || index >= _posNum )  return ( FALSE );
    
      return (_onOffCurr[index]);

}

/*=====================================================================*/

void locfmtw_getPosInfo ( int index, int* loc_idx, int* loc_fmt )
/************************************************************************
 * locfmtw_getPosInfo							*
 *									*
 * This function gets the locator type and locator format info for a	*
 * particular position index.						*
 *									*
 * void locfmtw_getPosInfo(index, loc_idx, loc_fmt)			*
 *									*
 * Input parameters:							*
 *	index		int	index of locator position               *
 *									*
 * Output parameters:							*
 *	loc_idx		int*	locator type idx for that position	*
 *	loc_fmt		int*	locator type format for that pos.	*
 *									*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( index < 0 || index >= _posNum ) {

	*loc_idx = *loc_fmt = -1;
	return;
      }
  
      *loc_idx = _locStrc.current[index];  
      *loc_fmt = _roundingCurr[index]*1000 +
                 _unitStrc.current[index]*100 +
                 _dirStrc.current[index]*10 +
                 _dspStrc.current[index];

}

/*=====================================================================*/

void locfmtw_setCurrPos ( int new_pos )
/************************************************************************
 * locfmtw_setCurrPos							*
 *									*
 * This function sets the value of _posCurr.			 	*
 *									*
 * void locfmtw_setCurrPos (new_pos)					*
 *									*
 * Input parameters:							*
 *	new_pos		int	new position index		        *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( new_pos < 0 || new_pos >= _posNum )  return;
    
      _posCurr = new_pos;

}

/*=====================================================================*/

int locfmtw_getLocNum ( void )
/************************************************************************
 * locfmtw_getLocNum							*
 *									*
 * This function gets total numbe of locator types.			*
 *									*
 * int  locfmtw_getLocNum()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * locfmtw_getLocNum	int     number of locator types			*
 **									*
 * Log:									*
 * H. Zeng/SAIC         01/07   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return ( _locNum );

}

/*=====================================================================*/

char* locfmtw_getLocStr ( int index )
/************************************************************************
 * locfmtw_getLocStr							*
 *									*
 * This function gets the pointer for a particular loc type name str.	*
 *									*
 * char*  locfmtw_getLocStr( index )					*
 *									*
 * Input parameters:							*
 *	index		int	index of locator type                   *
 *									*
 * Return parameters:							*
 * locfmtw_getLocStr	char*   pointer to loc type name str		*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( index < 0 || index >= _locNum )  return (NULL);
    
      return (_locStr[index]);

}

/*=====================================================================*/

char* locfmtw_getLocId ( int index )
/************************************************************************
 * locfmtw_getLocId							*
 *									*
 * This function gets the pointer for a particular loc type alias str.  *
 *									*
 * char*  locfmtw_getLocId ( index )					*
 *									*
 * Input parameters:							*
 *	index		int	index of locator type                   *
 *									*
 * Return parameters:							*
 * locfmtw_getLocId	char*   pointer to loc type alias str		*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( index < 0 || index >= _locNum )  return (NULL);
    
      return (_locId[index]);

}

/*=====================================================================*/

void locfmtw_readLocTbl ( int *iret )
/************************************************************************
 * locfmtw_readLocTbl							*
 *									*
 * This function reads the locator information table.		        *
 *									*
 * void locfmtw_readLocTbl( iret )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *                               -1 - Unable to open table      	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 * M. Li/SAIC		07/03	Allocate _locStr and _locCode		*
 ***********************************************************************/
{
    int		jj, bufsiz, format, ier;
    char	buffer[80], alias[15], name[15];
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Open the locator table. If not found, return an error.
     */
    fp = cfl_tbop(LOC_TBL, "nmap", &ier);

    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }
    else {
	bufsiz = sizeof(buffer);
	cfl_tbnr ( fp, &(_locNum), &ier );

	_locId  = (char **) malloc ( _locNum * sizeof(char *) );
	_locStr = (char **) malloc ( _locNum * sizeof(char *) );
	_locCode = (int *)  malloc (_locNum * sizeof (int));
	
	for ( jj = 0; jj < _locNum; jj++ ){
	     cfl_trln ( fp, bufsiz, buffer, &ier ); 
	     sscanf ( buffer,"%s %s %d",name, alias, &format);
	     _locCode[jj] = format;
             _locStr[jj] = (char *)XtMalloc(sizeof(name)+1);
             _locId[jj]  = (char *)XtMalloc(sizeof(alias)+1);
	     strcpy (_locStr[jj], name);
	     strcpy (_locId[jj],  alias);

	}
    }
    cfl_clos ( fp, &ier );

}
/*=====================================================================*/

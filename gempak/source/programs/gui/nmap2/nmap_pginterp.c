#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h" 
#include "hints.h"
#include "Nxm.h"
#include "proto_xw.h"


#define	NONE_SELECTED 	( 0 )	/* No current element selected value */
#define MAX_SELECT	( 2 )	/* Max. number of elems in a single interp */
#define MAX_INTERVAL	( 500 )	/* Max. interval in unit of hour */
#define MAX_NEWELM	( 500 )	/* Max. number of new elements in a single interp */


enum  Process_id { INTERP_START = 0,	/* interpolation starting   */
		   FIRST_SELECTED,	/* first element selected   */
		   FIRST_CONFIRMED,     /* first element confirmed  */
		   SECOND_SELECTED,	/* second element selected  */
		   SECOND_CONFIRMED	/* second element confirmed */
		 };
static enum Process_id	_inProcessId;

static Boolean		_interpClosedFig;
static char		_interpAreaTyp[32];
static char		_interpTag[32];
static char 		_interpVgType;
static char 		_interpVgClass;
static int		_numElSelected;
static int		_selectedEls[MAX_SELECT];	/* elem file locations */

static Widget		_interpForm;
static Widget		_startText, _endText, _intervalText;
static Widget		_layerLbl, _layerMenu, _interpModeBtns[2];
static WidgetList	_layerBtns, _cntlBtns;

static char		_startStr[10], _endStr[10];
static int		_currInterval=1;
static int		_numOfNewElm;
static int		_defInterpMode, _interpMode;
static int		_resultLayer;	

/*
 *  private callback functions
 */
static void pginterp_arrowCb    ( Widget, long, XtPointer );
static void pginterp_startTxtCb ( Widget, XtPointer, XtPointer );
static void pginterp_endTxtCb   ( Widget, XtPointer, XtPointer );
static void pginterp_intervalCb ( Widget, XtPointer, XtPointer );
static void pginterp_layerOptCb ( Widget, long, XtPointer );
static void pginterp_layerMenuCb( Widget, XtPointer, XtPointer );
static void pginterp_layerPbCb  ( Widget, long, XtPointer );
static void pginterp_ctlBtnCb   ( Widget, long,  XtPointer );


/*
 *  private functions
 */
static void pginterp_handleStateSelectEh ( Widget	wid, 
				    XtPointer		clnt,
			 	    XEvent		*event,
				    Boolean		*ctdr );
static void pginterp_handleStateConfirmEh ( Widget	wid, 
				    XtPointer		clnt,
			 	    XEvent		*event,
				    Boolean		*ctdr ); 
static void pginterp_handleStateInterpEh ( Widget	wid, 
				    XtPointer		clnt,
			 	    XEvent		*event,
				    Boolean		*ctdr );
static void pginterp_selectInterpEl ( XEvent	*event,
				      int	*iret );
static void pginterp_interp ( 	    int		*iret );

static void pginterp_prepElm (      int		location,
			            int		*np,
				    float	xp[], 
			            float	yp[],
				    VG_DBStruct	*el,
				    int		*iret );
static int pginterp_getInterpStep(  int		startLevel,
				    int		endLevel,
				    int		numNewElms);
static Boolean pginterp_isFigureClosed ( VG_DBStruct *el );
void        pginterp_updateLayerMenu (          void	 );
void	    pginterp_updateLayerName (		void	 );

/************************************************************************
 * nmap_pginterp.c                                                      *
 *                                                                      *
 * This module controls the interpolation function.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   pginterp_selectElmEh	Route all mouse clicks to state handler *
 *   pginterp_startInterp	Initialize for interpolation         	*
 *									*
 * private callback functions:						*
 *   pginterp_startTxtCb()	callback for end time text		*
 *   pginterp_endTxtCb()	callback for start time text		*
 *   pginterp_intervalCb()	callback for interval text              *
 *   pginterp_arrowCb()		callback for interval arrow buttons	*
 *   pginterp_layerOptCb()	callback for radio buttons		*
 *   pginterp_layerMenuCb()	callback for layer menu cascase btn	*
 *   pginterp_layerPbCb()	callback for layer menu push btns	*
 *   pginterp_ctlBtnCb()	callback for control buttons		*
 *									*
 * private functions:							*
 *   pginterp_handleStateSelectEh Handle events when in SELECT state	*
 *   pginterp_handleStateConfirmEh Handle events when in CONFRIM state	*
 *   pginterp_handleStateInterpEh Handle events when in INTERP state	*
 *   pginterp_selectInterpEl	Select a vg element for interpolation	*
 *   pginterp_interp        	Perform the interpolation               *
 *   pginterp_prepElm		Load element for interpolation         	*
 *   pginterp_getInterpStep	Calculate interp for flight level	*
 *   pginterp_isFigureClosed	examine if a VG el is a closed figure	*
 *   pginterp_updateLayerMenu	updates the layer menu			*
 *   pginterp_updateLayerName   adjust layer menu when switching layer	*
 *   pginterp_toBeContinued	checks if to continue cur. interp.	*
 *   pginterp_resumeOper	resume cur. oper. when layer is changed *
 ***********************************************************************/

/*=====================================================================*/

void pginterp_create ( Widget parent )
/************************************************************************
 * pginterp_create							*
 *									*
 * This function creates an INTERPOLATION window.			*
 *									*
 * void pginterp_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 * B. Yin/SAIC		11/04	removed variables set but not used	*
 * T. Piper/SAIC	09/05	made ii and nn long			*
 * H. Zeng/SAIC		09/06	added new radio btn group		*
 ***********************************************************************/
{
    long	ii = 0, nn = 0;
    int		toff = 40;
    
    char	*ctlstrs[] = {"Interpolate", "Close"};
    
    Widget	arrow_up, arrow_down, attrform, pd_menu;
    Widget	form[3], pane, layer_form, radio_rc;
    
    XmString	xmstr;
    
/*---------------------------------------------------------------------*/
        
    /*
     *  Create main form dialog window.
     */
    _interpForm = XmCreateFormDialog ( parent, "pginterp_popup", NULL, 0 );
    
    xmstr = XmStringCreateLocalized("Interpolation");
    XtVaSetValues ( _interpForm,
		XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		NULL );
    XmStringFree(xmstr);


    /*
     *  Create pane area and forms.
     */
    pane = XtVaCreateWidget ( "interp_pane",
		xmPanedWindowWidgetClass, 	_interpForm,
		XmNsashWidth,			1,
		XmNsashHeight,	 		1,
		XmNleftAttachment,  		XmATTACH_FORM,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL );

    attrform = XtVaCreateManagedWidget ( "interp_attrform",
		xmFormWidgetClass,		pane,
		NULL );


    for ( ii = 0; ii < 3; ii++ ) {
        form[ii] = XtVaCreateManagedWidget ( "form",
		xmFormWidgetClass,		attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5 + ii*toff,
		NULL );    
    }
    
    
    /*
     *  Starting time
     */ 
             XtVaCreateManagedWidget ( "Starting Time:",
		xmLabelWidgetClass,		form[0],
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			5,
		NULL ); 
     _startText = XtVaCreateManagedWidget ( "start_text",
		xmTextWidgetClass,		form[0],
       		XmNcolumns,			5,
  		XmNmaxLength,			10, 
        	XmNvalue,			"",
        	XmNeditable,			True,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			120,
        	NULL );

    XtAddCallback ( _startText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );
    XtAddCallback ( _startText, XmNlosingFocusCallback, 
		    (XtCallbackProc)pginterp_startTxtCb, (XtPointer) NULL );


    /*
     *  Ending time
     */ 
             XtVaCreateManagedWidget ( "Ending Time:",
		xmLabelWidgetClass,		form[1],
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			5,
		NULL ); 

    _endText = XtVaCreateManagedWidget ( "end_text",
		xmTextWidgetClass,		form[1],
		XmNcolumns,			5,
		XmNmaxLength,			10,
		XmNvalue,			"",
	       	XmNeditable,			True,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			120,
		NULL );

    
    XtAddCallback ( _endText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );
    XtAddCallback ( _endText, XmNlosingFocusCallback, 
		    (XtCallbackProc)pginterp_endTxtCb, (XtPointer) NULL );


    /*
     *  Interval (hours)
     */ 
             XtVaCreateManagedWidget ( "Interval (hrs):",
		xmLabelWidgetClass,		form[2],
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			5,
		NULL ); 

    _intervalText = XtVaCreateManagedWidget ( "sequence_text",
		xmTextWidgetClass,		form[2],
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			7,
		XmNcolumns,			5,
		XmNmaxLength,			6,
		XmNeditable,			True,
		XmNvalue,			"1",
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			120,
		NULL );

    XtAddCallback ( _intervalText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );
    XtAddCallback ( _intervalText, XmNlosingFocusCallback, 
		    (XtCallbackProc)pginterp_intervalCb, (XtPointer) NULL );


    arrow_up = XtVaCreateManagedWidget ( "interp_arrowup",
		xmArrowButtonWidgetClass,	form[2],
		XmNarrowDirection,		XmARROW_UP,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			0,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_intervalText,
		NULL );

    XtAddCallback ( arrow_up, XmNactivateCallback, 
		   (XtCallbackProc)pginterp_arrowCb, (XtPointer) 1 );


    arrow_down = XtVaCreateManagedWidget ( "interp_arrowdown",
		xmArrowButtonWidgetClass,	form[2],
		XmNarrowDirection,		XmARROW_DOWN,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			arrow_up,
		XmNtopOffset,			0,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_intervalText,
		NULL );

    XtAddCallback ( arrow_down, XmNactivateCallback, 
		   (XtCallbackProc)pginterp_arrowCb, (XtPointer) -1 );

    /*
     * Create radio button group and result layer option menu.
     */
    layer_form = XtVaCreateWidget ( "layer_form",
		 xmFormWidgetClass,		attrform,
		 XmNtopAttachment,		XmATTACH_WIDGET,
		 XmNtopWidget,			form[2],
		 XmNleftAttachment,		XmATTACH_FORM,
		 NULL );    

    radio_rc   = XtVaCreateManagedWidget ("radio_rowcol",
			      xmRowColumnWidgetClass,	layer_form,
			      XmNradioBehavior,	        TRUE,
			      XmNorientation,	        XmVERTICAL,
			      XmNpacking,		XmPACK_TIGHT,
			      XmNtopAttachment,		XmATTACH_FORM,
			      XmNtopOffset,		10,
			      XmNleftAttachment,        XmATTACH_FORM,
			      XmNleftOffset,		10,
			      NULL                                     ); 

    _interpModeBtns[0]  = XtVaCreateManagedWidget ("This Layer Only",
			      xmToggleButtonWidgetClass, radio_rc,
			      XmNhighlightThickness,     0,
			      NULL                                     );
    
    XtAddCallback (_interpModeBtns[0], XmNarmCallback, 
                   (XtCallbackProc)pginterp_layerOptCb, (XtPointer) 0  ); 

    _interpModeBtns[1]  = XtVaCreateManagedWidget ("Multiple  Layers",
			      xmToggleButtonWidgetClass, radio_rc,
			      XmNhighlightThickness,     0,
			      NULL                                     );
    
    XtAddCallback (_interpModeBtns[1], XmNarmCallback, 
                   (XtCallbackProc)pginterp_layerOptCb, (XtPointer) 1  );

    _layerLbl = XtVaCreateManagedWidget ( "Result Layer",
		xmLabelWidgetClass,		layer_form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			radio_rc,
		XmNrightAttachment,		XmATTACH_OPPOSITE_WIDGET,
		XmNrightWidget,			radio_rc,
		XmNrightOffset,			3,
		NULL ); 

    pd_menu     = XmCreatePulldownMenu (layer_form, "interp_pdmenu", NULL, 0);
    _layerMenu  = XmCreateOptionMenu   (layer_form, "interp_menu", NULL, 0);
    _layerBtns  = (WidgetList)XtMalloc(MAX_LAYERS*sizeof(Widget));

    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {

	  _layerBtns[ii] = XtVaCreateManagedWidget ("layer_1",
			     xmPushButtonWidgetClass, pd_menu,
			     NULL );
	  XtAddCallback (_layerBtns[ii], XmNactivateCallback, 
	     (XtCallbackProc)pginterp_layerPbCb, (XtPointer) ii);
    }

    xmstr = XmStringCreateLocalized ("");
    XtVaSetValues (_layerMenu, 
		   XmNlabelString,	xmstr,	
		   XmNsubMenuId,	pd_menu,
		   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	_layerLbl,	  
		   XmNrightAttachment,	XmATTACH_OPPOSITE_WIDGET,
		   XmNrightWidget,	_layerLbl,
		   NULL);
    XmStringFree (xmstr);

    XtAddCallback (_layerMenu, XmNentryCallback, 
	           (XtCallbackProc)pginterp_layerMenuCb, NULL );

    XtManageChild (_layerMenu);
    XtManageChild ( layer_form );        
  
    /*
     *  Control buttons
     */
    nn = XtNumber ( ctlstrs );
    _cntlBtns = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 0, "pginterp_ctlBtn", nn,
		     ctlstrs, (XtCallbackProc)pginterp_ctlBtnCb, _cntlBtns);

    XtManageChild(pane);

}

/*=====================================================================*/

void pginterp_popup ( void )
/************************************************************************
 * pginterp_popup							*
 *									*
 * This function manages the interpolation window.			*
 *									*
 * void pginterp_popup ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( !XtIsManaged ( _interpForm ) ) {        
        XtManageChild ( _interpForm );        
    }

}

/*=====================================================================*/

void pginterp_popdown ( void )
/************************************************************************
 * pginterp_popdown							*
 *									*
 * This function unmanages the interpolation window.			*
 *									*
 * void pginterp_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _interpForm ) ) {
	XtUnmanageChild ( _interpForm );
    }
}

/*=====================================================================*/

Boolean pginterp_isUp ( void )
/************************************************************************
 * pginterp_isUp							*
 *									*
 * Query whether the interpolation dialog is managed or not.		*
 *									*
 * Boolean pginterp_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 *									*
 * Return parameters:							*
 *	pginterp_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_interpForm) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pginterp_startTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pginterp_startTxtCb							*
 *									*
 * Callback function for the start time text.				*
 *									*
 * static void pginterp_startTxtCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
    char    *ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext ) {
        strcpy ( _startStr, ptext );
	XtFree ( ptext );
    }
        
}

/*=====================================================================*/
/* ARGSUSED */
static void pginterp_endTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pginterp_endTxtCb							*
 *									*
 * Callback function for the start time text.				*
 *									*
 * static void pginterp_endTxtCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
    char    *ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext ) {
        strcpy ( _endStr, ptext ); 
	XtFree ( ptext );
    }
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pginterp_arrowCb ( Widget wid, long incr, XtPointer cbs )
/************************************************************************
 * pginterp_arrowCb							*
 *									*
 * Callback function for the interval arrow buttons.			*
 *									*
 * static void pginterp_arrowCb ( wid, incr, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   incr		long		interval increment		*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
    int		lenstr, ier;
    char	interval[10];
/*---------------------------------------------------------------------*/
    
    _currInterval += (int)incr;

    if ( _currInterval < 1 ) _currInterval = 1;
    if ( _currInterval > MAX_INTERVAL ) _currInterval = MAX_INTERVAL;

    sprintf ( interval, "%-3d", _currInterval );
    cst_rmbl ( interval, interval, &lenstr, &ier );

    XtVaSetValues ( _intervalText, XmNvalue, interval, NULL );

}

/*=====================================================================*/
/* ARGSUSED */
static void pginterp_intervalCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pginterp_intervalCb							*
 *									*
 * Callback function for the intreval text.				*
 *									*
 * static void pginterp_intervalCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 ***********************************************************************/
{
    int		ier	= 0;
    int 	num	= 0;
    int		lenstr	= 0;

    char	interval[10];
    char	*str	= NULL;
/*---------------------------------------------------------------------*/

    str = XmTextGetString ( _intervalText );

    sscanf ( str, "%d", &num );

    XtFree ( str );

    if ( num < 1 ) num = 1;
    if ( num > MAX_INTERVAL ) num = MAX_INTERVAL;

    _currInterval = num;

    sprintf ( interval, "%-3d", _currInterval );
    cst_rmbl ( interval, interval, &lenstr, &ier );

    XtVaSetValues ( _intervalText, XmNvalue, interval, NULL );

}

/*=====================================================================*/

/* ARGSUSED */
static void pginterp_layerOptCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pginterp_layerOptCb							*
 *									*
 * Callback function for the radio buttons.				*
 *									*
 * static void pginterp_layerOptCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		09/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * Determine the mode of interpolation. Possible modes are:
     *		0: This Layer Only
     *		1: Multiple Layers
     */ 
    _interpMode = (int)which;

    /*
     * Making GUI changes for current interpolation mode.
     */
    if ( _interpMode == 0 ) {

      /*
       * For "This Layer Only".
       */
      XtSetSensitive ( _layerLbl,   FALSE );
      XtSetSensitive ( _layerMenu,  FALSE );     

    }
    else {

      /*
       * For "Multiple Layers".
       */
      XtSetSensitive ( _layerLbl,   TRUE );
      XtSetSensitive ( _layerMenu,  TRUE );

      pginterp_updateLayerMenu();

    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pginterp_layerMenuCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pginterp_layerMenuCb							*
 *									*
 * Callback function for the layer menu cascade button.		        *
 *									*
 * static void pginterp_layerMenuCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	not used			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pginterp_updateLayerMenu();

}

/*=====================================================================*/

/* ARGSUSED */
static void pginterp_layerPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pginterp_layerPbCb							*
 *									*
 * Callback function for the layer menu push button.		        *
 *									*
 * static void pginterp_layerPbCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
    XtPointer   userdata;
    long	layer_idx;
/*---------------------------------------------------------------------*/

    XtVaGetValues (_layerBtns[which],  
		   XmNuserData,	&userdata, 
		   NULL);
    layer_idx = (long)userdata;
    _resultLayer = (int)layer_idx;

}

/*=====================================================================*/

/* ARGSUSED */
static void pginterp_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pginterp_cntlBtnCb							*
 *									*
 * Callback function for the interpolation button.			*
 *									*
 * static void pginterp_ctlBtnCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding 				*
 * H. Zeng/SAIC		10/06	added "Close" button			*
 ***********************************************************************/
{
    int		errorCode = 0;    
    int		ier	  = 0;    
    int		timeSpan  = 0;  
    int		start_time, end_time;  
    char	errStr[256];    
    Widget	mainw;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:		/* Interpolation */	
	  	  
	  /*
	   *  The user should have already selected two elements to
	   *  activate the "Interpolation" button. Anyway, we still check
	   *  it for safety. 
	   */
	  if ( _numElSelected == MAX_SELECT ) {
	      
	      /*
	       *  Verify the inputs for starting/ending time, and interval.
	       */
	      mainw = mcanvw_getDrawingW(); 
	      
	      if ( strlen(_startStr) == (size_t)0 ) {
	          NxmWarn_show ( mainw, "Please provide a valid start time!" );
                  return;
	      }
	      	      
	      if ( strlen(_endStr) == (size_t)0 ) {
	          NxmWarn_show ( mainw, "Please provide a valid end time!" );
                  return;
	      }	      
	      
              /*
	       *  Retrieve the start and end time
	       */
	      start_time = atoi ( _startStr );
              end_time = atoi ( _endStr );
	      
	      timeSpan = G_ABS(end_time - start_time);
	      if ( (_currInterval > timeSpan / 2) ) {
	          strcpy ( errStr, "No valid interpolation available.  Check if\n" );
	          strcat ( errStr, "interval <= |end time - start time|/2" );
		  NxmWarn_show ( mainw, errStr );
                  return;
	      }	      
	      

	      /*
	       *  Calculate the number of new elements (interpolations).
	       */
              _numOfNewElm = timeSpan / _currInterval;	      
	      
	      if ( ( ( timeSpan - _numOfNewElm * _currInterval ) == 0 ) ) {
	          _numOfNewElm--;	          	      
	      }
	      	      	      	      
	      if ( _numOfNewElm > MAX_NEWELM ) {
	          _numOfNewElm = MAX_NEWELM;
		  strcpy ( errStr, "Too many new elements.  Set to 500." );
		  NxmWarn_show ( mainw, errStr );
	      }	      
	      	      	      
	      
	      /*
	       *  Perform the interpolation.
	       */
              pginterp_interp ( &ier );

	      if ( ier != 0 ) {
	          /*
	           * Error encountered when trying to interpolate.
	           */
	          errorCode = ier;
		  er_wmsg ( "PGEN", &errorCode, NULL, &ier, 4, 0 );
	          NxmErr_update ();
              }
	      	          	      

	      /*
	       *  Start again.
	       */
              _inProcessId = INTERP_START;
	      _numElSelected = 0;
              pghdlb_deselectAll();
	      XtSetSensitive ( _cntlBtns[0], False );
	      mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );
	  
	  }
	  
	break;

      case 1:		/* Close */

        pghdlb_deselectAll();
	_numElSelected = 0;
	_inProcessId   = INTERP_START;
        pginterp_popdown ();
        pgpalw_setCurBtns ( FUNC_SELECT, -1, -1 );
        pgpalw_setupOper ();

	break;

    } /* the end of switch ( which... */

}

/*=====================================================================*/

/* ARGSUSED */
void pginterp_selectElmEh ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr ) 
/************************************************************************
 * pginterp_selectElmEh                                                 *
 *                                                                      *
 * This function selects elements for an interpolation action.        	*
 *                                                			*
 * void pginterp_selectElmEh ( wid, clnt, event, ctdr  )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          	                        *
 *      clnt		XtPointer                               	*
 *      *event          XEvent          	                 	*
 *                                                                      *
 * Output parameters:                                             	*
 * 	*iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	copy from pgsmear_selectElmCb		*
 * H. Zeng/SAIC		10/06	modified for multi_layer interpolation	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

   switch ( _inProcessId ) {

      case ( INTERP_START ):
      case ( FIRST_CONFIRMED ):
         pginterp_handleStateSelectEh ( wid, clnt, event, ctdr ); 
         break;

      case ( FIRST_SELECTED ):
      case ( SECOND_SELECTED):
         pginterp_handleStateConfirmEh ( wid, clnt, event, ctdr ); 
         break;

      case ( SECOND_CONFIRMED ):
         pginterp_handleStateInterpEh ( wid, clnt, event, ctdr ); 
         break;

      default:
         break;
   }
}

/*=====================================================================*/

void pginterp_startInterp ( void )
/************************************************************************
 * pginterp_startInterp                                                 *
 *                                                                      *
 * This function does all the setup necessary to begin an interpolation.* 
 *									*
 * void pginterp_startInterp ( )         				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *    		`	None		            		 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	copy from pgsmear_startSmear		*
 * E. Safford/SAIC	08/04	add _interpAreaTyp			*
 * E. Safford/SAIC	08/04	chg _interpAreaTyp to string		*
 * J. Wu/SAIC		12/04	add _interpClosedFig			*
 * B. Yin/SAIC		11/05	remove subtype				*
 * H. Zeng/SAIC		09/06	added GUI initialization		*
 ***********************************************************************/
{
    int	   int_val, ii, ier;
    char   tag_val[64];
    static Boolean  first_time = TRUE;
/*---------------------------------------------------------------------*/
   
    _inProcessId       = INTERP_START;
    _interpVgClass     = 0; 
    _interpVgType      = 0;
    _interpAreaTyp[0]  = '\0';
    _interpTag[0]      = '\0';
    _interpClosedFig    = False;
    
    for ( ii = 0; ii < MAX_SELECT; ii++ ) {
        _selectedEls[ii] = 0;
    }
   
    _numElSelected = 0;

    /*
     * Initialization of GUI components.
     */
    /*
     * get default interpolation mode from prefs.tbl.
     */
    if ( first_time ) {

      _defInterpMode = 0;
      ctb_pfstr ( "DEF_INTERP_MODE", tag_val, &ier );
      if ( ier == 0 && sscanf(tag_val, "%d", &int_val) == 1) {

	 if ( int_val != 0 )  _defInterpMode = 1;
      }
      first_time = FALSE;
    }

    /*
     * Determine the starting interpolation mode.
     */
    if ( pglayer_getNumLayers() <= 1 )  _interpMode = 0;
    else  _interpMode = _defInterpMode; 
    
    for ( ii = 0; ii < 2; ii++ ) {

      XtVaSetValues ( _interpModeBtns[ii], 
		      XmNset, FALSE, NULL );
    }
    XtVaSetValues ( _interpModeBtns[_interpMode], 
		    XmNset, TRUE, NULL );
    pginterp_layerOptCb ( NULL, _interpMode, NULL );


    XtSetSensitive ( _cntlBtns[0], False );
   
    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );

}

/*=====================================================================*/
  
/* ARGSUSED */
static void pginterp_handleStateSelectEh ( Widget wid, XtPointer clnt,
			 	    XEvent *event, Boolean *ctdr ) 
/************************************************************************
 * pginterp_handleStateSelectEh                                        	*
 *                                                                      *
 * Handle all mouse click events in SELECT state. This is not a direct	*
 * callback function.							*
 *                                                                      *
 * static void pginterp_handleStateSelectEh ( wid, clnt, event, ctdr )*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid                     Widget                                	*
 *      clnt			XtPointer                               *
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      NONE                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	Initial coding				*
 * H. Zeng/SAIC		10/06	modified for multi_layer interpolation	*
 ***********************************************************************/
{
    int	iret = 0;
/*---------------------------------------------------------------------*/

    /*
     *  Select Mode - Selectable elements at this point include only
     *                closed lines, airmets, sigmets, or GFAs.
     *
     *  At Start:  Element selected with standard handlebars: 0.
     *		   Element confirmed with red squares: 0 or 1. 
     *
     *  Mouse hint:  
     *      "INTERP <L> Select <M> EXIT"     - no confirmed element. 
     *      "INTERP <L> Select <M> DELESECT" - one confirmed element.
     *
     *  Actions:
     *       1)  The user clicks L on/near one of the correct class of
     *           elements.  Mark the element as selected using standard
     *           handlebars.  If there is already one confirmed element,
     *           then Only elements that match the same class as the 
     *           confirmed one can be selected. Put standard handlebars
     *           on it.  Go to CONFIRM state. 
     *
     *       2)  The user clicks M. There are three possibilities:
     *           One element selected - deselected it & keep SELECT;
     *           One confirmed & none selected - deselected & keep SELECT;  
     *           None confirmed/selected - exit INTERP function.
     *
     */
    
    if ( event->xbutton.button == Button1 ) {

        if ( _numElSelected < MAX_SELECT ) {
            pginterp_selectInterpEl ( event, &iret );
      
            if ( iret == 0 ) {

	      /*
	       * _inProcessId goes to the next state.
	       */
              _inProcessId = _inProcessId + 1;	
              mbotw_mouseSet ( LMHINT_CONFIRM, MMHINT_DESELECT );
            }
        }
    }
    else if ( event->xbutton.button == Button2 ) {

        if ( _numElSelected == 0 ) {		/* exit	*/

	    _inProcessId = INTERP_START;
            pginterp_popdown ();
            pgpalw_setCurBtns ( FUNC_SELECT, -1, -1 );
            pgpalw_setupOper ();
        }
	else if ( _numElSelected == 1 ) {	/* deselect */	
	    _inProcessId = INTERP_START;
	    _numElSelected = 0;
            pghdlb_deselectAll();
	    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );
	}
    }   
}

/*=====================================================================*/
  
/* ARGSUSED */
static void pginterp_handleStateConfirmEh ( Widget wid, XtPointer clnt,
			 	    XEvent *event, Boolean *ctdr ) 
/************************************************************************
 * pginterp_handleStateConfirmEh                                       	*
 *                                                                      *
 * Handle all mouse click events in CONFIRM state. This is not a direct	*
 * callback function.							*
 *                                                                      *
 * static void pginterp_stateConfirmEh ( wid, clnt, event, ctdr )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid                     Widget                                  *
 *      clnt			XtPointer                               *
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      NONE                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	initial coding		 		*
 * H. Zeng/SAIC		10/06	modified for multi_layer interpolation	*
 ***********************************************************************/
{
    int	elementLoc = 0;
    int elementNum = 0;
    int	ier        = 0;
/*---------------------------------------------------------------------*/

    /*
     *  Confirm State
     *
     *  At Start:    Element selected with standard handlebars: 1.
     *		     Element confirmed with red squares: 0 or 1. 
     *
     *  Mouse hint:  "INTERP <L> Confirm <M> Deselect"
     *
     *  Actions:
     *     1)  The user clicks L (Confirm Selection).  Change handlebars 
     *         on element from standard (white circles) to red squares.  
     *	       
     *         Now, if there are a total of 2 elements have been selected 
     *         and confirmed, go to CONFRIM state for interpolation;
     *         otherwise, go to SELECT state for next selection.
     *
     *     2)  The user clicks M (Deselect).  Remove the handlebars from
     *         the element.  Go to SELECT state for next selection.
     */

    elementLoc = pgactv_getElmLoc( );

    if ( event->xbutton.button == Button1 ) {		/* confirmed */

        /*
         *  Add this element file location to the _selectEls array
         */
        if ( _numElSelected < MAX_SELECT ) {
            _selectedEls[_numElSelected] = elementLoc;
	    _numElSelected++;
            _inProcessId = _inProcessId + 1;
        }

        pgactv_clearActv();
        pghdlb_displayAllSel();
            
    }
    else if ( event->xbutton.button == Button2 ) {	/* de-select */

        crg_getinx ( elementLoc, &elementNum, &ier );
   
        pghdlb_deselectEl ( elementNum, True );
        pgactv_clearActv ();  
	_inProcessId = _inProcessId - 1;   
    }


    if ( _numElSelected == MAX_SELECT ) {
        XtSetSensitive ( _cntlBtns[0], True );        
	mbotw_mouseSet ( LMHINT_NOACTION, MMHINT_TOSELECTOPER );    
    }
    else {   
        if ( pghdlb_elemSelected() > 0 ) {
            mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DESELECT );	
        }
	else {
            mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );		
	}
    }    
}

/*=====================================================================*/
  
/* ARGSUSED */
static void pginterp_handleStateInterpEh ( Widget wid, XtPointer clnt,
			 	   XEvent *event, Boolean *ctdr ) 
/************************************************************************
 * pginterp_handleStateInterpEh                                		*
 *                                                                      *
 * Handle all mouse click events in SECOND_CONFIRMED (Ready to		*
 * Interpolate) state. This is not a direct callback function.		*
 *                                                                      *
 * static void pginterp_handleStateInterpEh ( wid, clnt, event, ctdr )*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid			Widget                                  *
 *      clnt			XtPointer                               *
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	initial coding				*
 * H. Zeng/SAIC		10/06	modified for multi_layer interpolation	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     *   SECOND_CONFIRMED (Ready to Interpolate) State
     *
     *   At Start:    two elements have been selected and confirmed.
     *
     *   Mouse hint:  "INTERP <L> NO ACTION    <M> RESET"
     *
     *   Actions:
     *      1)  The user clicks L.  No action.  Click the "Interploation"
     *          button to perform the actual interploation.
     *
     *      2)  The user clicks M.  Exit without performing interpolation.
     *		Deselect all selected elements.  Restart INTERP.
     *
     */

    if ( event->xbutton.button == Button2 ) {   /* restart interp */
        _inProcessId = INTERP_START;
	_numElSelected = 0;
        pghdlb_deselectAll();
	XtSetSensitive ( _cntlBtns[0], False );
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );
    }

}

/*=====================================================================*/

static void pginterp_selectInterpEl( XEvent *event, int *iret )
/************************************************************************
 * pginterp_selectInterpEl                                              *
 *                                                                      *
 * Given a mouse click event, determine if it is near an element that   *
 * can be used in an interpolation action.				*
 *                                                                      *
 * void pginterp_selectInterpEl ( event, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret			int	return code			*
 *					  0 = element selected		*
 *					 -1 = no element selected	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	copy from pgsmear_selectSmearEl		*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * E. Safford/SAIC	08/04	restrict GFA selection to same areaType *
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_getFld()	*
 * J. Wu/SAIC		12/04	allow non-closed lines & fronts		*
 * B. Yin/SAIC		11/05	remove the subtype check for GFAs	*
 ***********************************************************************/
{
    int		curLayer = 0;
    int		class    = NONE_SELECTED;
    int		elNum    = 0;
    int		ier      = 0;
    int		ipos     = 0;
    int		nearest  = 0;
    int		numSelected  = 0; 
    int		offsetToElem = 0;

    int		xoff, yoff;
    
    float	xx, yy;

    char	selectFlag;
    char	str[10], value[32];
    
    VG_DBStruct el;

/*---------------------------------------------------------------------*/

    *iret = -1;

    numSelected = pghdlb_elemSelected();
    curLayer    = pglayer_getCurLayer();
   
    xgtoff( &xoff, &yoff, &ier );
    xx = (float) ( event->xbutton.x + xoff );
    yy = (float) ( event->xbutton.y + yoff );


    if ( numSelected == 0 ) {		

        /*
         * No elements are selected.  Get the vg_class from the palette and 
         * make sure it's an allowable class for interpolation.  Put the class 
         * into the el for masking the cvg_scan.
         */

        class   = pgpalw_getCurClassId();
        el.hdr.vg_class = class;	

        if ( (class != CLASS_LINES) && (class != CLASS_SIGMETS) && 
             (class != CLASS_ANY) && (class != CLASS_MET) &&
	     (class != CLASS_FRONTS) ) {
            return;
        }
    }
    else {					

        /* 
         * This is not the first selection.  Use the same vg_class and type 
         * as the first selected element.
         */

        el.hdr.vg_class = class = _interpVgClass; 
        el.hdr.vg_type  = _interpVgType;        	
    }

     
    cvg_scan( NULL, curLayer, (char) class, xx, yy, 0, 
      		&el, &offsetToElem, &nearest, &ier );

    if ( ier >= 0 ) {

	/*
         *  The second element must have the same open/closed condition as
	 *  the first selected element.  Otherwise, do not select it.
         */
        if ( numSelected == 0 ) {
	    _interpClosedFig = pginterp_isFigureClosed( &el );
	}
	else {
	    if ( pginterp_isFigureClosed( &el ) != _interpClosedFig ) {
                return;
	    } 
        }


        /*
         *  If we're in CLASS_ANY, then we might have gotten the wrong type
         *  of element returned.  In that case toss out anything that isn't
         *  a line or sigmet or gfa or fronts.
         */
        if ( (el.hdr.vg_class != CLASS_LINES) && 
      	     (el.hdr.vg_class != CLASS_SIGMETS) &&
	     (el.hdr.vg_class != CLASS_MET) &&
	     (el.hdr.vg_class != CLASS_FRONTS) ) {
            return; 
        }


        /*
         *  Of the Sigmets class, ESOL and Isolated are not "interpolatable".
         *  Check for those and exit if that's what this element is.
         */      
        if ( (el.hdr.vg_class == CLASS_SIGMETS) ) {
            if( el.elem.sig.info.subtype != SIGTYP_AREA ) {
                return;
            }
        }

        
	/*
         *  Of the MET class, only GFA elements with the same subtype 
	 *  are "interpolatable".  Check for those and exit if that's
	 *  that's what this element is.
         */      
        if ( (el.hdr.vg_class == CLASS_MET) ) {
            
	    if ( (el.hdr.vg_type != GFA_ELM) ) { 

               /*
                * Free TCA break point memory
                */
               if ( el.hdr.vg_type == TCA_ELM ) {
                  cvg_freeBkpts ( &el );
               }

               return;
            }
	    
	    if ( (numSelected == 1) && (el.hdr.vg_type == GFA_ELM) ) {
			          	
	        cvg_getFld ( &el, TAG_GFA_AREATYPE, value, &ier );
		if ( strcmp( _interpAreaTyp, value ) != 0 ) { 
		    return;
		}		

	        cvg_getFld ( &el, TAG_GFA_TAG, value, &ier );
		if ( strcmp( _interpTag, value ) != 0 ) { 
		    return;
		}		
            }
	}


        /*
         *  Make sure selected el is not already selected.
         */
        crg_getinx( offsetToElem, &elNum, &ier );  
        crg_gsel( elNum, &selectFlag, &ier );
        if ( selectFlag ) {
            return;
        }

        pgactv_setActvElm( &el, offsetToElem );
        pghdlb_select( &el, offsetToElem );
        numSelected = pghdlb_elemSelected();

        if ( numSelected  == 1 ) { 		/* on first selection   */
            _interpVgType  = el.hdr.vg_type;	/* set the VgType & Class */
            _interpVgClass = el.hdr.vg_class;
            	    
	    if ( el.hdr.vg_type == GFA_ELM ) {  /* set the subtype for GFA */

	        cvg_getFld ( &el, TAG_GFA_AREATYPE, value, &ier );
		strcpy( _interpAreaTyp, value );

	        cvg_getFld ( &el, TAG_GFA_TAG, value, &ier );
		strcpy( _interpTag, value );
            }
        }


        /*
         *  Set starting/ending time for GFA elements, if the forecast hour 
	 *  is discrete.
         */
        if ( _interpVgType == GFA_ELM ) {
	    cvg_getFld ( &el, TAG_GFA_FCSTHR, str, &ier );
            cst_srch ( 0, strlen(str), str, "-", &ipos, &ier );

            if ( ier != 0 ) {
                if ( numSelected == 1 ) {
	            cvg_getFld ( &el, TAG_GFA_FCSTHR, _startStr, &ier );
		    XtVaSetValues ( _startText, XmNvalue, _startStr, NULL );
                }
		else if ( numSelected == 2 ) {
	            cvg_getFld ( &el, TAG_GFA_FCSTHR, _endStr, &ier );
		    XtVaSetValues ( _endText, XmNvalue, _endStr, NULL );   
		}
	    }	
        }
              
        *iret = 0;
    }
}

/*=====================================================================*/

static void pginterp_interp( int *iret )
/************************************************************************
 * pginterp_interp	                                                *
 *                                                                      *
 * Perform the interpolation operation.                                 *
 *                                                                      *
 * void pginterp_interp ( iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	*iret			int	return code			*
 *					  0 = normal			*
 *					 -1 = unable to read element	* 
 *					 -4 = error on interpolation	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	initial coding				*
 * E. Safford/SAIC	08/04	add linear interp of flight levels	*
 * E. Safford/SAIC	08/04	update for change of top/bottom to char *
 * J. Wu/SAIC		09/04	remove check for GFA_GFA		*
 * E. Safford/SAIC	09/04	round the top/bottom FL to nearest 1000s*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_get/setFld()	*
 * B. Yin/SAIC		11/04	Removed gfa description field		*
 * E. Safford/SAIC	11/04	update for new gui & GFA table		*
 * E. Safford/SAIC	12/04	add useTop/useBottom checks		*
 * H. Zeng/SAIC		12/04	used cgr_ordrccw()			*
 * J. Wu/SAIC		12/04	interpolate non-closed lines & fronts	*
 * E. Safford/SAIC	01/05	add worst case sigmet reference string  *
 * B. Yin/SAIC		06/05	add fzl case for icing			*
 * H. Zeng/SAIC		09/05	rm call to pggfaw_combineSigStrs	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * J. Wu/SAIC		10/05	replace pggfaw_cmpSeverity with ctb_gfa**
 * H. Zeng/SAIC		10/06	changed _startTime to start_time	*
 * H. Zeng/SAIC		10/06	added check the val. of _interpMode	* 
 * J. Wu/SAIC		05/08	let text box arrow point to the center	*
 ***********************************************************************/
{
    int		curLayer	= 0;
    int		ier		= 0;
    int		newElmLocation	= 0;
    int		newPts		= 0;
    int    	numMapPts	= 0;
    int    	maxNewPts	= MAXPTS;
    int		ntmp		= 1;
    int		ii, pts0, pts1;
    int		startTop = 0, endTop = 0, startBottom = 0, endBottom = 0;
    signed int	stepTop = 0, stepBottom = 0;
    int		elTop   = 0, elBottom   = 0;
    int		start_time, end_time;

    char	timeStr[10], value[32], hazard[32];
    char	freq0[32], freq1[32], sev0[32], sev1[32];
    char	worstFreq[32], worstSev[32];
    char	bottomStr1[32], bottomStr0[32];
    char	fixedBottom[ 4 ] = "";

    float	pct;
    float	x0[MAXPTS], y0[MAXPTS];
    float	x1[MAXPTS], y1[MAXPTS];
    float	*xMap1 = NULL, *yMap1 = NULL;
    float	*xMap2 = NULL, *yMap2 = NULL;
    float	xNew[MAXPTS], yNew[MAXPTS];
    float	lat0[1], lon0[1], lat1[1], lon1[1], xlat[1], ylon[1];
    float	xct0, yct0, igr, xct1, yct1, lat2[1], lon2[1];
   
    Boolean	useTop = False, useBottom = False, useFixedBottom = False; 
    VG_DBStruct	newEl, elm0, elm1;
/*---------------------------------------------------------------------*/

    *iret = 0;
    curLayer = pglayer_getCurLayer(); 

    worstFreq[0] = '\0';
    worstSev[0]  = '\0';

    /*
     *  Sanity check; we should have 2 elements selected and confirmed
     *  by this point and should have at least one valid interpolation
     *  available based on the starting, ending time, and interval.
     */
    if ( _numElSelected != 2 || _numOfNewElm < 1 ) {
       *iret = -4;
       return;
    }


    /*
     *  Read the first selected element and use it as a template for 
     *  the resulting interpolation element.  Everything about the new 
     *  element is the same as the first element except the points.
     *  
     */
    cvg_rdrec( cvg_getworkfile(), _selectedEls[0], &newEl, &ier ); 
    if ( ier != 0 ) {
       *iret = -1;
       return;
    }


    /*
     *  Load the two selected elements for interpolation.
     */
    pginterp_prepElm ( _selectedEls[0], &pts0, x0, y0, &elm0, &ier );
    if ( ier != 0 ) {
       *iret = -1;
       return;
    }
    
    pginterp_prepElm ( _selectedEls[1], &pts1, x1, y1, &elm1, &ier );    
    if ( ier != 0 ) {
       *iret = -1;
       return;
    }


    /*
     *  Start interpolation to create new elements.
     *
     *  For GFA elements, interpolate the flight levels and set 
     *  the text location as well. 
     */
    pgundo_newStep ();        
    cgr_init ( &ier );
    

    /*
     *  Calculate the center for GFA elements. 
     */
    if ( elm0.hdr.vg_type == GFA_ELM ) {
        cgr_centroid ( x0, y0, &pts0, &xct0, &yct0, &igr, &ier );
        
        cvg_getFld ( &elm0, TAG_GFA_LAT, value, &ier );
	xlat[0] = atof ( value );
        cvg_getFld ( &elm0, TAG_GFA_LON, value, &ier );
	ylon[0] = atof ( value );

	gtrans ( sys_M, sys_D, &ntmp, xlat, ylon, lat0, lon0, 
    		 &ier, strlen(sys_M), strlen(sys_D) );    

 
        cvg_getFld ( &elm1, TAG_GFA_LAT, value, &ier );
	xlat[0] = atof ( value );
        cvg_getFld ( &elm1, TAG_GFA_LON, value, &ier );
	ylon[0] = atof ( value );

	gtrans ( sys_M, sys_D, &ntmp, xlat, ylon, lat2, lon2, 
    		 &ier, strlen(sys_M), strlen(sys_D) );    

	/*
	 *  ier will be negative if there is no TOP value
	 */
        cvg_getFld ( &elm0, TAG_GFA_TOP, value, &ier );
	if( ier >= 0 ) {
            useTop     = True;
	    startTop   = cvg_getFlghtLvl( value );
            cvg_getFld ( &elm1, TAG_GFA_TOP, value, &ier );
            endTop     = cvg_getFlghtLvl( value );
            stepTop    = pginterp_getInterpStep( startTop, endTop, _numOfNewElm );
        }

	/*
	 *  ier will be negative if there is no BOTTOM value
	 */
        cvg_getFld ( &elm0, TAG_GFA_BOTTOM, bottomStr0, &ier );
	if( ier >= 0 ) {
	    useBottom   = True;
            startBottom = cvg_getFlghtLvl( bottomStr0 );
            cvg_getFld ( &elm1, TAG_GFA_BOTTOM, bottomStr1, &ier );
            endBottom   = cvg_getFlghtLvl( bottomStr1 );

	    if ( ( strcasecmp( bottomStr0, "fzl" ) == 0 ) ||
   	         ( strcasecmp( bottomStr1, "fzl" ) == 0 ) ) {
	
	       useBottom = False;
	       useFixedBottom = True;
	       strcpy ( fixedBottom, "FZL" );

	    }
	    else {
               stepBottom  = pginterp_getInterpStep( startBottom, 
					     endBottom, _numOfNewElm );
	    }
        }


	/*
	 *  Determine the worst-case Frequency and/or Severity 
	 *  for GFA elements (if either/both are applicable to this element).
	 */

        cvg_getFld ( &elm0, TAG_GFA_AREATYPE, value, &ier );         
        strcpy( hazard, value ); 

        cvg_getFld ( &elm0, "<Frequency>", freq0, &ier );
        if( strlen( freq0 ) > (size_t)0 ) {
            cvg_getFld ( &elm1, "<Frequency>", freq1, &ier );
	    ctb_gfaCmpSeverity( hazard, "Frequency", freq0, freq1, worstFreq, &ier );
        } 

        cvg_getFld ( &elm0, "<Severity>", sev0, &ier );
        if( strlen( sev0 ) > (size_t)0 ) {
            cvg_getFld ( &elm1, "<Severity>", sev1, &ier );
	    ctb_gfaCmpSeverity( hazard, "Severity", sev0, sev1, worstSev, &ier );
        } 

    }


    start_time = atoi ( _startStr );
    end_time = atoi ( _endStr ); 

    for ( ii = 0; ii < _numOfNewElm; ii++ ) {
        
        /*
         *  Calculate the percetage for interpolation. 
         */
	pct = (_currInterval * (ii+1)) / (float)G_ABS(end_time - start_time);
	
        
	/*
         *  Interpolation. 
         */
        if ( _interpClosedFig ) {  /* closed figures */
	    cgr_polyinterp ( &pts0, x0, y0, &pts1, x1, y1, &pct, 
      		    &numMapPts, xMap1, yMap1, xMap2, yMap2, 
		    &maxNewPts, &newPts, xNew, yNew, &ier );                     
	    
	    if ( ier == 0 ) {
	        cgr_ordrccw (  newPts, xNew, yNew, &ier );
	    }
        }
	else {	/* open figures */
	    cgr_lineinterp ( &pts0, x0, y0, &pts1, x1, y1, &pct, 
      		    &maxNewPts, &newPts, xNew, yNew, &ier );             	
	}
	
	if ( ier != 0 ) {
            *iret = -4;
            return;
        }
	        	        
        
	/*
         *  Interpolate the text location for GFA element and assign
	 *  forecast hours. 
         */
        if ( _interpVgType == GFA_ELM ) {
            cgr_centroid ( xNew, yNew, &newPts, &xct1, &yct1, &igr, &ier );
            
	    gtrans ( sys_D, sys_M, &ntmp, &xct1, &yct1, xlat, ylon, 
		 &ier, strlen(sys_D), strlen(sys_M) );
	    
	    sprintf ( value, "%7.2f", xlat[0] );
            cvg_setFld ( &newEl, TAG_GFA_ARROW_LAT, value, &ier );
            sprintf ( value, "%7.2f", ylon[0] );
            cvg_setFld ( &newEl, TAG_GFA_ARROW_LON, value, &ier );
	
	    lat1[0] = lat0[0] + (lat2[0] - lat0[0]) * pct;
	    lon1[0] = lon0[0] + (lon2[0] - lon0[0]) * pct;

	    gtrans ( sys_D, sys_M, &ntmp, lat1, lon1, xlat, ylon, 
		 &ier, strlen(sys_D), strlen(sys_M) );

	    sprintf ( value, "%7.2f", xlat[0] );
	    cvg_setFld ( &newEl, TAG_GFA_LAT, value, &ier );

	    sprintf ( value, "%7.2f", ylon[0] );
	    cvg_setFld ( &newEl, TAG_GFA_LON, value, &ier );

	    if ( start_time <= end_time ) {
	        sprintf ( timeStr, "%i",  start_time + _currInterval * (ii+1) );
	    }
	    else {
	        sprintf ( timeStr, "%i",  start_time - _currInterval * (ii+1) );
	    }
	
	    /*
	     *  Calculate the new element's top and bottom flight levels.
	     *  Any value less than 0 is a missing value -- a null string
	     *  goes in the flight level.  A bottom level of 0 gets a 
	     *  string of "SFC".
	     *
	     *  Round the elTop and elBottom flight levels to the nearest 1000s
	     */
	    if( useTop ) {
	        elTop    = startTop    + ( (ii+1) * stepTop );
	        elTop    = (G_NINT((elTop+5)/10) * 10);
                if( elTop >= 0 ) {
	            sprintf( value, "%d", elTop );
                }
	        else {
	            strcpy( value, "" );
                }
	    
	        cvg_setFld ( &newEl, TAG_GFA_TOP, value, &ier );
            }

	    if ( useBottom || useFixedBottom ) {
	      if( useBottom ) {
   	        elBottom = startBottom + ( (ii+1) * stepBottom );
	        elBottom = (G_NINT((elBottom+5)/10) * 10);

                if( elBottom > 0 ) {
	            sprintf( value, "%d", elBottom );
                }
	        else if( elBottom == 0 ) {
	            strcpy( value, "SFC" );
                }
	        else {
	            strcpy( value, "" );
                }
              }
	      else if ( useFixedBottom ) {
		strcpy ( value, fixedBottom );
	      }
	    
	      cvg_setFld ( &newEl, TAG_GFA_BOTTOM, value, &ier );
	    }
	    cvg_setFld ( &newEl, TAG_GFA_FCSTHR, timeStr, &ier );


	    /*
	     *  use the worst-case description for GFA elements.
	     */
	    if( strlen( worstFreq ) > (size_t)0 ) {
  	        cvg_setFld ( &newEl, "<Frequency>", worstFreq, &ier );
	    }
	    if( strlen( worstSev ) > (size_t)0 ) {
  	        cvg_setFld ( &newEl, "<Severity>", worstSev, &ier );
	    }

	}
	

        /*
         *  Save the new element, set the range record, and save the 
         *  location for the undo step.
         */
	pgvgf_saveNewElm ( NULL, sys_D, &newEl, newPts, 
      		           xNew, yNew, TRUE, &newElmLocation, &ier );

        if ( _interpMode == 0 ) {

	   /*
            * For "This Layer Only", set the new element as in the
	    * current layer.
	    */
	   crg_set ( &newEl, newElmLocation, curLayer, &ier );    
        }
	else {

	   /*
	    * For "Multiple Layers", set the new element as in the
	    * layer of _resultLayer.
	    */
	   crg_set ( &newEl, newElmLocation, _resultLayer, &ier );    
	   pglayer_setChngMade ( _resultLayer, TRUE ); 
	} 
  
	pgundo_storeThisLoc ( newElmLocation, UNDO_ADD, &ier );

    }
    
    
    /*
     *  Complete the interpolation step.
     */
    cgr_done ( &ier );        
    pgundo_endStep ();


    /*
     *  Clean up.
     */
    cvg_freeElPtr ( &elm0 );
    cvg_freeElPtr ( &elm1 );
    cvg_freeElPtr ( &newEl );

}

/*=====================================================================*/

static void pginterp_prepElm( int location, int *np, float xp[], 
			       float yp[], VG_DBStruct *el, int *iret )
/************************************************************************
 * pginterp_multiInterp                                                 *
 *                                                                      *
 * Prepare an element for interpolation.				*
 *									*
 * static void pginterp_multiInterp ( location, np, xp, yp, el, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	location	int	Element location			*
 *					  				*
 * Output parameters:                                                   *
 *	*np		int		number of points in the element	*
 *	*xp		float		array of x coordinates		*
 *	*yp		float 		array of y coordinates		*
 *	*el		VG_DBStruct	VG element			*
 *	*iret		int		return code			*
 *					  0 = normal			*
 *					 -1 = fail to load element	* 
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/04	initial coding				*
 * J. Wu/SAIC		12/04	reorder only if el is a closed figure	*
 ***********************************************************************/
{
    int	  	ii, pts, ier;
    float	*xpts = NULL, *ypts = NULL;
/*---------------------------------------------------------------------*/

    *np = 0;
    *iret = 0;

    /* 
     *  Read the element.
     */
    cvg_rdrec ( cvg_getworkfile(), location, el, &ier ); 
    if ( ier < 0 ) {
        *iret = -1;
        return;
    }

    pts = cvg_gtnumpts( el, &ier );
    if ( ier < 0 ) {
        *iret = -1;
        return;
    }

    xpts = ( float * )malloc( pts * sizeof(float) );
    ypts = ( float * )malloc( pts * sizeof(float) );



    /* 
     *  Convert to device coordinates.
     */
    cvg_todev ( el, &pts, xpts, ypts, &ier );  
    if ( ier < 0 ) {
         *iret = -1;
         if ( xpts )  free( xpts );
         if ( ypts )  free( ypts );
         return;
    }


    /*
     *  Reorder the coordinate arrays if el is a closed figure.
     */
    if ( _interpClosedFig ) {
	cgr_ordrccw ( pts, xpts, ypts, &ier );
        
	if ( ier < 0 ) {
	    *iret = -1;
             return;
	}
    }


    /*
     *  Copy to output array.
     */
    *np = pts;
    
    for ( ii = 0; ii < pts; ii++ ) {
        xp [ii] = xpts [ii];
        yp [ii] = ypts [ii];
    }
            
    if ( xpts ) free( xpts );       
    if ( ypts ) free( ypts );

}

/*=====================================================================*/

static signed int pginterp_getInterpStep(  int startLevel,
			 	    int	endLevel, int numNewElms)
/************************************************************************
 * pginterp_getInterpStep                                               *
 *                                                                      *
 * Get the step value for the linear interpolation of flight levels.	*
 * The "step value" means the change between new elements, so the first *
 * new element would use startLevel + 1 * step, the second would use    *
 * startLevl + 2*step, etc.						*
 *									*
 * static signed int pginterp_getInterpStep ( startLevel, 		*
 *					endLevel, numNewElms)		*
 *                                                                      *
 * Input parameters:                                                    *
 *	startLevel	int	Initial flight level			*
 *	endLevel  	int	Final flight level			*
 *	numNewElms	int	Number of new interpolated elements	*
 *					  				*
 * Output parameters:                                                   *
 *			None                                            *
 * Return parameters:                                                   *
 *                      signed int	the step value (+ or -)		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/04	initial coding				*
 ***********************************************************************/
{
   signed int step = 0; 
/*---------------------------------------------------------------------*/
   if ( numNewElms > 0 ) {

      step = -1 * ( ( startLevel - endLevel ) / (numNewElms + 1) );

   } 

   return( step );
}

/*=====================================================================*/

static Boolean pginterp_isFigureClosed ( VG_DBStruct *el )
/************************************************************************
 * pginterp_isFigureClosed                                       	*
 *                                                                      *
 * Check if the input element is a closed figure. GFA elements & closed	*
 * are treated as closed figures and others are treated as non-closed.	*
 *									*
 * static Boolean pginterp_isFigureClosed ( el )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Pointer to the el to be examined*
 *					  				*
 * Output parameters:                                                   *
 *			None                                            *
 *					  				*
 * Return parameters:                                                   *
 *                      Boolean		True - el is a closed figure	*
 *                      		False - el is an open figure	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		12/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( el->hdr.vg_type == GFA_ELM ||
         ( el->hdr.vg_class == CLASS_LINES && 
	   (Boolean)el->hdr.closed ) ) {
        return ( True );
    }
    else {
        return ( False );    
    }

}

/*=====================================================================*/

void pginterp_updateLayerMenu ( void )
/************************************************************************
 * pginterp_updateLayerMenu                                       	*
 *                                                                      *
 * This function updates the pulldown menu associated with "Multiple    *
 * Layers" choice.							*
 *									*
 * void pginterp_updateLayerMenu ( void )				*
 *                                                                      *
 * Input parameters:							*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			None                                            *
 *					  				*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
      int  cur_layer;
      long ii, num_layer;
      XmString	xmstr;
/*---------------------------------------------------------------------*/

      /*
       * Update the pulldown menu.
       */
      num_layer = pglayer_getNumLayers();    
      cur_layer = pglayer_getCurLayer(); 

      for ( ii = 0; ii < num_layer; ii++ ) {

        xmstr = XmStringCreateLocalized ( pglayer_getName(ii) );
        XtVaSetValues (_layerBtns[ii], 
		       XmNlabelString,	xmstr,
		       XmNuserData,	(XtPointer)ii,
		       NULL);
        XmStringFree (xmstr);
        XtManageChild ( _layerBtns[ii] );
      }

      for ( ii = num_layer; ii < MAX_LAYERS; ii++ ) {

	XtUnmanageChild ( _layerBtns[ii] );
      }


      XtVaSetValues (_layerMenu, 
		     XmNmenuHistory,	_layerBtns[cur_layer],
		     NULL);
      _resultLayer = cur_layer;

}

/*=====================================================================*/

void pginterp_updateLayerName ( void )
/************************************************************************
 * pginterp_updateLayerName                                       	*
 *                                                                      *
 * This function adjusts the pulldown menu associated with "Multiple    *
 * Layers" choice when the user switchs layer in the middle of		*
 * interplation operation. This function is only called by pginterp_	*
 * resumeOper().							*
 *									*
 * void pginterp_updateLayerName ( void )				*
 *                                                                      *
 * Input parameters:							*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			None                                            *
 *					  				*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
      int  num_layer, ii;
      XmString	xmstr;
/*---------------------------------------------------------------------*/

      num_layer = pglayer_getNumLayers();

      /*
       * Update the names of the pulldown menu options.
       */
      for ( ii = 0; ii < num_layer; ii++ ) {

        xmstr = XmStringCreateLocalized ( pglayer_getName(ii) );
        XtVaSetValues (_layerBtns[ii], 
		       XmNlabelString,	xmstr,
		       NULL);
        XmStringFree (xmstr);
      }

}

/*=====================================================================*/

Boolean pginterp_toBeContinued ( void )
/************************************************************************
 * pginterp_toBeContinued                                       	*
 *                                                                      *
 * Decides whether the current interpolation process is to be continuced*
 * The function is called when the program switchs to another layer and *
 * wants to know whether to continue the current interpolation process  *
 * or not.								*
 *									*
 * Boolean pginterp_toBeContinued ( void )				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			None                                            *
 *					  				*
 * Return parameters:                                                   *
 *                      Boolean		TRUE  - YES			*
 *                      		FALSE - NO			*
 **                                                                     *
 * Log:                                                                 *
 * H. ZENG/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

      if ( _interpMode == 1 )  return TRUE;

      return FALSE;

}

/*=====================================================================*/

void pginterp_resumeOper ( void )
/************************************************************************
 * pginterp_resumeOper		                                      	*
 *                                                                      *
 * This function opens up interpolation edit widnow and resume the      *
 * operation when a cross_layer interpolation is in process.		*
 *									*
 * void pginterp_resumeOper ( void )					*
 *                                                                      *
 * Input parameters:							*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			None                                            *
 *					  				*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		10/06	initial coding				*
 ***********************************************************************/
{
      int	ii;
/*---------------------------------------------------------------------*/

      /*
       * Set current operation to be FUNC_INTERP and set the
       * associated event handlers.
       */
      pgpalw_setCurOper ( pgpalw_getOperWid (FUNC_INTERP) );
      pginterp_popup();
      mcanvw_setPressFunc( (XtEventHandler)&pginterp_selectElmEh, CURS_DEFAULT);

      /*
       * Make sure _inProcessId is one of INTERP_START, FIRST_CONFIRMED 
       * or SECOND_CONFIRMED.
       */
      if ( _inProcessId == FIRST_SELECTED ) {

        pgactv_clearActv ();  
        _inProcessId -= 1;
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );
      }
      else if ( _inProcessId == SECOND_SELECTED ) {

        pgactv_clearActv ();  
        _inProcessId -= 1;
        mbotw_mouseSet(LMHINT_SELECT, MMHINT_DESELECT);
      }

      /*
       * Update the names of layer menu every time the layer is switched.
       */
      pginterp_updateLayerName ();

      /*
       * Since the layer is changed, we need to put red-square marks 
       * on all selected elements once again.
       */
      for ( ii = 0; ii <  _numElSelected; ii++ ) {

        pghdlb_setSelect ( _selectedEls[ii] );
      }

      pghdlb_displayAllSel ();

}

/*=====================================================================*/

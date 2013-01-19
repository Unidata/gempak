#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"

#define ALL_ON		" All On "
#define ALL_OFF		" All Off "


enum { EDIT_NAME, ON_OFF, EXPAND, EXIT };

typedef struct {
    Widget      form;
    Widget      namepb;
    Widget      checkb;
    Widget      editpb;
} Layer_gui_t;


static Layer_gui_t	_layerW[MAX_LAYERS];

static Widget		_pglayrwWin;
static Widget		layer_pane;
static Widget		_layerRC;


static Pixel		_fg, _bg;
static Boolean		_allOn = FALSE;		/* track ALL_ON & ALL_OFF */
static Widget		_allOnOff;
static Widget		_ctlform;
static Widget		_editName;
static Widget		_expand;

/*
 *  private callback functions
 */
static void pglayrw_ctlBtnCb ( Widget, long, XtPointer );
static void pglayrw_checkbCb ( Widget, long, XtPointer );
static void pglayrw_editCb   ( Widget, long, XtPointer );

/*
 *  private functions
 */
static void pglayrw_turnLayrBtnOn ( int layer );
static void pglayrw_turnLayrBtnOff( int layer );
static void pglayrw_addLayerCallbacks( void );
static void pglayrw_removeLayerCallbacks( void );
static void pglayrw_initAllBtn( void );
static void pglayrw_allOnOff ( void );
static void pglayrw_expand ( Widget wdgt );

/************************************************************************
 * nmap_pglayrw.c							*
 *									*
 * This module defines a layer control popup window for VGF "Layering"  *
 * capability.                                                          *
 *									*
 * CONTENTS:								*
 *	pglayrw_create()	create the layer control window	        *
 *	pglayrw_popup()		pop up the layer control window	        *
 *	pglayrw_popdown()	pop down the layer control window	*
 *	pglayrw_isUp()		query if the window is up 		*
 * 	pglayrw_exit()		exit layering				*
 *      pglayrw_setActvLayer()	set the active layer			*
 *									*
 *	pglayrw_ctlBtnCb()	callback for control buttons 		*
 *	pglayrw_allOnOffBtn()	function for 'All On' and 'All Off' btns*
 *      pglayrw_nameCb()        callback for layer name push buttons    *
 *      pglayrw_checkbCb()      callback for check buttons              *
 *      pglayrw_editCb()        callback for layer edit push buttons    *
 *      pglayrw_refresh()       refresh and update display     		*
 *      pglayrw_turnLayrBtnOn   switch a layer display btn to On        *
 *      pglayrw_turnLayrBtnOff  switch a layer display btn to Off       *
 *      pglayrw_addLayer        add a new layer				*
 *      pglayrw_manageLayers    mang.|unmang layers per in_use flags    *
 *      pglayrw_updtSettings    reload attributes and redisplay layer   *
 *      pglayrw_addLayerCallbacks	add all layer callbacks		*
 *      pglayrw_removeLayerCallbacks	remove all layer callbacks	*
 *      pglayrw_setLayer	set current layer using hotkeys		*
 *      pglayrw_initAllBtn      initialize the All On/Off button        *
 *      pglayrw_getLayerName	get current layer's name		*
 *      pglayrw_matchLayer	match a given layer 			*
 *	pglayrw_expand 		expand/collapse the layer window	*
 ***********************************************************************/

/*=====================================================================*/

Widget pglayrw_create ( Widget parent )
/************************************************************************
 * pglayrw_create							*
 *									*
 * This function creates the layer control window.		        *
 *									*
 * Widget pglayrw_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pglayrw_create	Widget  ID of the layer control window	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	     	02/02	initial coding                          *
 * H. Zeng/EAI       	02/02   modified gui components                 *
 * E. Safford/SAIC	03/02	Reduce overall size of window		*
 * J. Wu/SAIC		03/02	add pglayrxt module for exit layering	*
 * E. Safford/SAIC	03/02	rm XtSetSensitive for checkb[]		*
 * B. Yin/SAIC		06/04	Added 'all on' and 'all off' buttons	*
 * E. Safford/SAIC	08/04	combine All On & All Off buttons	*
 * E. Safford/SAIC	08/04	add call to pglayrw_initAllBtn		*
 * J. Wu/SAIC		12/04	adjust width of the layer name button	*
 * B. Yin/SAIC		09/05	added an arrow button to expand/collapse*
 * T. Piper/SAIC	10/05	declared ii long			*
 * T. Piper/SAIC	02/06	Removed XmCreate to fix AWIPS bug	*
 * T. Piper/SAIC	07/06	Restored XmCreate			*
 ***********************************************************************/
{
    Widget	exitBtn;
    Boolean     on_off;
    Pixel	bg;
    XmString	xmstr;
    long	ii;
    char        name[20], attr[10];
/*---------------------------------------------------------------------*/
/*
 * create the layer dialog shell
 */
    _pglayrwWin = XmCreateFormDialog(parent, "pglayrw_popup",
				     NULL, 0);
    XtVaSetValues(_pglayrwWin,
		XmNnoResize,        		True,
		XmNdefaultPosition, 		False,
		NULL);

    XtVaSetValues(XtParent(_pglayrwWin),
			XmNtitle,		"Layer",
			NULL);

/*
 * create the layer 'paned' manager widget
 */

    layer_pane = XtVaCreateWidget("pglayrw_pane",
		xmPanedWindowWidgetClass,	_pglayrwWin,
			XmNallowResize,			True,
			XmNmarginHeight,		0,
			XmNmarginWidth,			0,
			XmNsashHeight,			1,
			XmNsashWidth,			1,
			XmNspacing,			1,
			NULL);


/*
 * create the layer 'row/columned' CONTROL area
 */
    _layerRC = XtVaCreateWidget ("",
		xmRowColumnWidgetClass,         layer_pane,
		XmNorientation,			XmVERTICAL,
		XmNpacking,			XmPACK_COLUMN,
		XmNspacing,			0,
		XmNmarginWidth,			0,
		XmNmarginHeight,		0,
		NULL);

/*
 * create the buttons for each layer
 */
    for (ii = 0; ii < MAX_LAYERS; ii++) {

	_layerW[ii].form = XtVaCreateWidget("form",
		xmFormWidgetClass,		_layerRC,
		NULL);

        sprintf( name, "%s", pglayer_getName(ii) );
	_layerW[ii].namepb = 
		XtVaCreateManagedWidget(name,
		     xmPushButtonWidgetClass,	_layerW[ii].form,
		     XmNshadowThickness,	1,
		     XmNleftAttachment,		XmATTACH_FORM,
		     XmNtopAttachment,		XmATTACH_FORM,
		     XmNtraversalOn,		FALSE,
		     XmNrecomputeSize,		FALSE,
		     XmNwidth,			70,
		     NULL);

	XtAddCallback(_layerW[ii].namepb, XmNactivateCallback,
		      (XtCallbackProc)pglayrw_nameCb, (XtPointer)ii);


	on_off = (Boolean)pglayer_getDsplOn(ii);
	_layerW[ii].checkb = 
		XtVaCreateManagedWidget(" ",
		     xmToggleButtonWidgetClass, _layerW[ii].form,
		     XmNleftAttachment,         XmATTACH_WIDGET,
		     XmNleftWidget,		_layerW[ii].namepb,
		     XmNleftOffset,		0, 
		     XmNtopAttachment,		XmATTACH_FORM,
		     XmNtraversalOn,		FALSE,
		     XmNindicatorType,		XmN_OF_MANY,
		     XmNset,			on_off,
		     XmNwidth,			22,
		     XmNheight,			25,
		     NULL);

	XtAddCallback(_layerW[ii].checkb, XmNvalueChangedCallback,
		      (XtCallbackProc)pglayrw_checkbCb, (XtPointer)ii);


        if ( pglayer_getDsplClr(ii) ) {
             sprintf(attr, " A/");
        }
        else {
             sprintf(attr, " M/");
        }

        if ( pglayer_getFill(ii) ) {
             strcat(attr, "F ");
        }
        else {
             strcat(attr, "N ");
        }
        xmstr = XmStringCreateLocalized(attr);
        bg = (Pixel)NxmColrP_getColorPixel
                    ( pglayer_getMonoClr(ii) );

	_layerW[ii].editpb =
		XtVaCreateWidget(" M/F ",
		     xmPushButtonWidgetClass,   _layerW[ii].form,
                     XmNbackground,             bg,
                     XmNlabelString,            xmstr,
		     XmNshadowThickness,        1,
                     XmNtopAttachment,          XmATTACH_FORM,
		     XmNleftAttachment,		XmATTACH_WIDGET,
		     XmNleftWidget,		_layerW[ii].checkb,
                     XmNtraversalOn,            FALSE,
                     XmNrecomputeSize,          FALSE,
                     XmNleftOffset,           	0, 
                     XmNmarginWidth,            6,
		     NULL);
        XmStringFree(xmstr);

	XtAddCallback(_layerW[ii].editpb, XmNactivateCallback,
		(XtCallbackProc)pglayrw_editCb, (XtPointer)ii);


	XtManageChild(_layerW[ii].form);

    }

    XtVaGetValues (_layerW[0].namepb,
		XmNforeground,			&_fg,
		XmNbackground,			&_bg,
		NULL);		

    pglayer_getCurLayer();
    XtManageChild(_layerRC); /* Make the control area visible */


/*
 * create the layer 'formed' action area
 */
    _ctlform =  XtVaCreateWidget("pglayrw_ctlform",
		xmFormWidgetClass,      layer_pane, NULL );

/*
 * create 'all on' and 'all off' buttons
 */
    _allOnOff = XtVaCreateManagedWidget ( ALL_ON,
			xmPushButtonWidgetClass,	_ctlform,
			XmNtopAttachment,		XmATTACH_FORM,
			XmNleftAttachment,		XmATTACH_FORM,
			XmNleftOffset,                  0,
			XmNheight,                      28,
			XmNwidth,                       60,
			XmNrecomputeSize,		False,
			NULL );

    XtAddCallback( _allOnOff, XmNactivateCallback,
			(XtCallbackProc)pglayrw_ctlBtnCb,
			(XtPointer)ON_OFF );

/*
 * create 'Edit Name' button
 */
    _editName = XtVaCreateWidget ( "Edit Name",
			xmPushButtonWidgetClass,        _ctlform,
			XmNtopAttachment,            	XmATTACH_FORM,
			XmNleftAttachment,              XmATTACH_FORM,
			XmNleftOffset,                  60,
			XmNheight,                      28,
			XmNrecomputeSize,		False,
			NULL );

    XtAddCallback( _editName, XmNactivateCallback,
		(XtCallbackProc)pglayrw_ctlBtnCb, (XtPointer)EDIT_NAME );

/*
 * create 'Exit' button
 */
    exitBtn = XtVaCreateManagedWidget ( "Exit",
			xmPushButtonWidgetClass,        _ctlform,
			XmNtopAttachment,            	XmATTACH_FORM,
			XmNleftAttachment,              XmATTACH_FORM,
			XmNtopOffset,			28,
			XmNleftOffset,                  0,
			XmNheight,                      28,
			XmNwidth,                       60,
			XmNrecomputeSize,		False,
			NULL );

    XtAddCallback( exitBtn, XmNactivateCallback,
		(XtCallbackProc)pglayrw_ctlBtnCb, (XtPointer)EXIT );

/*
 *  create the expand/collapse arrow button
 */
    _expand = XtVaCreateManagedWidget ( "expand",
			xmArrowButtonWidgetClass,       _ctlform,
			XmNarrowDirection,              XmARROW_RIGHT,
			XmNtopAttachment,            	XmATTACH_FORM,
			XmNleftAttachment,              XmATTACH_FORM,
			XmNtopOffset,			29,
			XmNleftOffset,                  60,
			XmNheight,                      26,
			XmNwidth,                       30,
			XmNrecomputeSize,		False,
			NULL );

    XtAddCallback( _expand, XmNactivateCallback,
		(XtCallbackProc)pglayrw_ctlBtnCb, (XtPointer)EXPAND );

    XtManageChild(_ctlform); /* Make action area visible */
    XtManageChild(layer_pane);

/*
 * create associated "Layer Display" and "Layer Name" windows
 */
    pglayred_create ( parent );
    pglayrnm_create ( parent );
    pglayrxt_create ( parent );


    pglayrw_initAllBtn();
    return(_pglayrwWin);

}

/*=====================================================================*/

void pglayrw_popup ( void )
/************************************************************************
 * pglayrw_popup							*
 *									*
 * This function pops up the layer control window.		        *
 *									*
 * void pglayrw_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 * E. Safford/SAIC	03/02	reset layer name using _updtSettings	*
 * E. Safford/SAIC	03/02	rm XtSetSensitive for checkb[]        	*
 * J. Wu/SAIC		12/02	popup layer name window without check	*
 * E. Safford/SAIC	11/03	use pglayr_switchOn not _switchColor    *
 * E. Safford/SAIC	08/04	combine All On and All Off buttons	*
 * E. Safford/SAIC	08/04	move All On/Off logic to _initAllBtn()	*
 * B. Yin/SAIC		09/05	collapse the layer window if necessary	*
 ***********************************************************************/
{
    long    		cur_layer;
    unsigned char	direction;
/*---------------------------------------------------------------------*/
/*
 *  Collapse the window if necessary
 */
    XtVaGetValues ( _expand, XmNarrowDirection, &direction, NULL );

    if ( direction == XmARROW_LEFT ) pglayrw_expand ( _expand );

    cur_layer = pglayer_getCurLayer();

/*
 *  Set up the current layer and GUI controls
 */
    pglayrw_updtSettings ( cur_layer );
    pglayrw_turnLayrBtnOn( cur_layer ); 

    pglayrw_manageLayers();
    
/*
 * Popup the Layer Name Window.
 */
    pglayrnm_popup (cur_layer);

}

/*=====================================================================*/

void pglayrw_popdown ( void ) 
/************************************************************************
 * pglayrw_popdown							*
 *									*
 * This function pops down the layer control window.			*
 *									*
 * void pglayrw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pglayrwWin)) {
	XtUnmanageChild (_pglayrwWin);
    }
}

/*=====================================================================*/

Boolean pglayrw_isUp ( void ) 
/************************************************************************
 * pglayrw_isUp								*
 *									*
 * This function queries whether the layer control window is up or the	*
 * layering process is activated without popping up layering widow.	*
 *									*
 * Boolean pglayrw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:
 * pglayrw_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 * J. Wu/SAIC		12/02	call to pgpalw_isLayerActv()		*
 ***********************************************************************/
{
    return ( XtIsManaged (_pglayrwWin) || pgpalw_isLayerActv() );
}

/*=====================================================================*/

void pglayrw_exit ( void ) 
/************************************************************************
 * pglayrw_exit								*
 *									*
 * This function shuts down layring and resets all the layer flags.	*
 *									*
 * void pglayrw_exit ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			None						* 
 **									*
 * Log:									*
 * E. Safford/SAIC	03/02	initial coding				*
 * E. Safordd/SAIC	03/02	use pglayer_init for layer values reset	*
 * E. Safordd/SAIC	03/02	remove _bg and _fg color assigment     	*
 * J. Wu/SAIC		03/02	check if elements are moved to layer 1	*
 * E. Safford/SAIC	03/02	rm XtSetSensitive for checkb[], fix fg  *
 *					and bg color reset		*
 * J. Wu/SAIC		03/02	redraw all elements upon exit		*
 * H. Zeng/EAI          03/02   added call to pgpalw_setBtnSntv()       *
 * J. Wu/SAIC		03/02	rm file name at the bottom of main win.	*
 * E. Safford/SAIC	03/02	add pgpalw_setupOper()			*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	08/04	add call to pglayrw_initAllBtn		*
 ***********************************************************************/
{
    int		ier, ii;
    Boolean	changed, moved;
/*---------------------------------------------------------------------*/
/* 
 *  Reset the colors for all layer buttons so a restart will begin
 *  with the correct bg and fg colors.
 */
    for (ii=0; ii<MAX_LAYERS; ii++) {
        XtVaSetValues (_layerW[ii].namepb,
           	XmNforeground,           _fg,
           	XmNbackground,           _bg,
           	NULL);
    }

    pglayrw_popdown ();
    pglayred_popdown ();
    pglayrnm_popdown ();
    pgpalw_setBtnSntv (FUNC_LAYER, TRUE);    
    pgpalw_inactvLayer ();

/*
 *  Reset the current layer to 0 and move all elements to 
 *  layer 0.  Also clear file name and save flag.
 */
    changed = pglayer_isChngMade ( 0 );
    crg_mvallayer ( 0, &moved, &ier );
    pgfilw_clearSaveFlag ();
    pgfilw_clearFileName ();
    pgfilw_showFileName ();

/*
 * Upon exit, re-initialize the layer settings and displays 
 * to their at-start conditions. 
 */
    pglayer_init();
    pglayrw_initAllBtn();


    if ( changed || moved ) pglayer_setChngMade ( 0, TRUE );

/*
 *  Redraw all elements, including those elements on the
 *  display-off layers 
 */         
    cvg_redraw ( cvg_getworkfile(), &ier );
    pgpalw_setupOper();
}

/*=====================================================================*/

void pglayrw_setActvLayer (  long newLayer, int *iret ) 
/************************************************************************
 * pglayrw_setActvLayer							*
 *									*
 * This function updates the display to show (highlight) the active     *
 * layer.  Note that this only updates the layer display.  The actual   *
 * mechanism that sets the current layer is pglayer_setCurLayer().	*
 * This function gets the current layer from the nmap_pglayer.c module. *
 *									*
 * void pglayrw_setActvLayer ()						*
 *									*
 * Input parameters:							*
 *			none						*
 * Output parameters:							*
 *	iret		*int	 0 = normal				*
 *				 1 = no change (newLayer == cur_layer) 	*
 *                             	-1 = newLayer is out of range		*
 * Return parameters:							*
 *			None						* 
 **									*
 * Log:									*
 * E. Safford/SAIC	11/02	initial coding, moved code here from	*
 *				  pglayrw_nameCb()			*
 * J. Wu/SAIC		08/04	retain GFA window if it was in ADD mode	*
 * J. Wu/SAIC		09/04	Restart GFA to finish previous drawing	*
 * H. Zeng/SAIC		10/06	allowed for cross_layer interpolation	*
 * J. Wu/SAIC		09/07	do not initialize Undo/Redo for FROM	*
 *                              if no new Undo/Redo step is added	*
 * B. Yin/SAIC		10/07	bring up the GFA' window		*
 ***********************************************************************/
{
    int		cur_layer, cur_obj, fromStep;
    Boolean	keepGfaw = False;
    Boolean	keepGfawp = False;
    char	curLayerName[MXFLSZ];
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 * Check the current layer.  If newLayer is the same, take no action.
 */
    cur_layer = pglayer_getCurLayer();
    if ( newLayer == cur_layer ) {
        *iret = 1;
        return;
    }
  
/*
 *  If newLayer is negative or greater than the number of 
 *  allowable layers, then toss this out as an error.
 */
    if ( newLayer < 0 || newLayer >= MAX_LAYERS ) {
        *iret = -1;
        return;
    }
  
/*
 *  Check if gfaw is up. If yes, we may need to restart it later.
 */  
    keepGfaw = ( pggfaw_isUp() && pggfaw_isAddMode() );
    keepGfawp = ( pggfawp_isUp() && pggfawp_isAddMode() );
    cur_obj = pgpalw_getCurObjId ();
    
/*
 *  Proceed with layer change.
 *
 *  Terminate any pending operations before switching layer and
 *  reset to "SELECT" state. 
 */    
    pggrpch_popdown();
    if ( pggrpw_isUp() )  pgpalw_inactvGrp ();   
    
    pgevt_unsetOper ( TRUE );            

/*
 *  Switch to the specified layer.
 */
    pglayrnm_popdown ();
    pglayred_popdown ();
    
    pglayrw_turnLayrBtnOff( cur_layer );

    pglayer_setCurLayer ( newLayer  );
    pglayrw_turnLayrBtnOn( newLayer );


    pglayrw_updtSettings ( newLayer );
    pglayrw_refresh (); /* redraw and update the display */

/*
 *  Reset UNDO/REDO, all changes made on the previous layer
 *  will be irreverseable now.
 *  Note: for GFA "FROM", simply switching layer should not
 *        reset the Undo/Redo. But any actions involving a
 *        a new Undo/Redo step will make the previous 
 *        FROM action irreverseable.
 */
    fromStep = pgfrom_getUndoStep( );
    if ( fromStep == IMISSD || fromStep != pgundo_getCurStep() ) {
        pgundo_initUndo ();
    }

/*
 *  Show the current file name at the bottom of the main window.
 */
    pgfilw_showFileName ();

/*
 *  Restart GFA window if it was in "ADD" mode.
 */
    if ( keepGfaw ) {	        
	pgpalw_setCurBtns ( FUNC_SELECT, CLASS_MET, cur_obj );
        
        pggfaw_popup ( NULL, NULL );        
	pgnew_setArmDynamic();
	
/*
 *  Match the current layer name with the GFA area type.
 */
        pglayrw_getLayerName ( curLayerName );
        pggfaw_setType ( curLayerName );    
    }

    if ( keepGfawp ) {	        
	pgpalw_setCurBtns ( FUNC_SELECT, CLASS_MET, cur_obj );
        
        pggfawp_popup ( NULL, NULL );        
	pgnew_setArmDynamic();
	
        pglayrw_getLayerName ( curLayerName );
        pggfawp_setType ( curLayerName );    
    }

/*
 * The following codes allow for cross-layer interpolation.
 */
    if ( pgpalw_getPrevOperWid() == pgpalw_getOperWid (FUNC_INTERP) &&
	 pginterp_toBeContinued()  ) {

         pginterp_resumeOper ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pglayrw_ctlBtnCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * pglayrw_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of layer control *
 * window.						                *
 *									*
 * void pglayrw_ctlBtnCb (wdgt, which, call)				*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/02   initial coding                          *
 * E. Safford/SAIC	02/02	reset current layer and elems on exit	*
 * J. Wu/SAIC      	02/02   clear file name & save flag upon exit	*
 * H. Zeng/EAI          02/02   added pglayrnm_popup()                  *
 * J. Wu/SAIC      	03/02   redraw and reset changes_made flag	*
 * E. Safford/SAIC	03/02	use pglayrw_exit to exit layering	*
 * J. Wu/SAIC		03/02	add pglayrxt module to exit layering	*
 * J. Wu/SAIC		03/02	move cvg_redraw to pglayrw_exit()	*
 * E. Safford/SAIC	03/02	inactivate group window on exit		*
 * H. Zeng/EAI          03/02   modified to use pggrpch_popdown()       *
 * B. Yin/SAIC		09/05	handle all bottom panel buttons 	*
 ***********************************************************************/
{
    int      cur_layer;
/*---------------------------------------------------------------------*/

    switch(which) {

      case ON_OFF:	/* All on/off	*/

	pglayrw_allOnOff( );
	break;
	
      case EXPAND:	/* Expand/Collapse */

	pglayrw_expand ( wdgt );
	break;

      case EDIT_NAME:	/* Edit Name */

        cur_layer = pglayer_getCurLayer ();
        pglayrnm_popup ( cur_layer );
	break;

      case EXIT:	/* Exit Layering */
       
        pghdlb_deselectAll();
        pgpalw_inactvGrp();
	pgpalw_classPopdown();
	pggrpch_popdown();
        pglayrxt_popup (); 
	break;

      default:
	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/

static void pglayrw_allOnOff ( void )
/************************************************************************
 * pglayrw_allOnOffBtn							*
 *									*
 * Function for 'All On' and 'All Off' buttons at the bottom   		*
 * of layer control window.				                *
 *									*
 * void pglayrw_allOnOffBtnCb ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/04	created					*
 * E. Safford/SAIC	08/04	combine All On & All Off buttons	*
 * B. Yin/SAIC		09/05	changed calling sequence		*
 *				added the callback for layer check box	*
 ***********************************************************************/
{
    int 	counter	  = 0;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if( _allOn ) {
	xmstr = XmStringCreateLtoR( ALL_OFF, XmFONTLIST_DEFAULT_TAG );
        pglayrw_removeLayerCallbacks();

        for ( counter = 0; counter < MAX_LAYERS; counter++ ) {
	    pglayer_setDsplOn ( counter, True );
            XtVaSetValues ( _layerW[ counter ].checkb, 
    			    XmNset, 		True, 
			    NULL);
        }
        pglayrw_refresh();
        pglayrw_addLayerCallbacks();

    }
    else {
	xmstr = XmStringCreateLtoR( ALL_ON, XmFONTLIST_DEFAULT_TAG );
        pglayrw_removeLayerCallbacks();

        for ( counter = 0; counter < MAX_LAYERS; counter++ ) {
	    pglayer_setDsplOn ( counter, False );
            XtVaSetValues ( _layerW[ counter ].checkb, 
    			    XmNset, 		False, 
			    NULL);
        }
        pglayrw_refresh();
        pglayrw_addLayerCallbacks();

    }

    XtVaSetValues( _allOnOff, XmNlabelString, xmstr, NULL );
    XmStringFree( xmstr );
    _allOn = !_allOn;
}

/*=====================================================================*/
/* ARGSUSED */
void pglayrw_nameCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * pglayrw_nameCb							*
 *                                                                      *
 * Callback function for layer control window Column 1 push buttons.    *
 *                                                                      *
 * void pglayrw_nameCb(wdgt, which, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt          Widget     widget ID                                  *
 *  which         long       button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI     	02/02	initial coding                          *
 * J. Wu/SAIC      	02/02   reset UNDO/REDO                         *
 * J. Wu/SAIC      	02/02   update layering win. & refresh display  *
 * H. Zeng/EAI     	02/02   added call to pglayrnm_popdown()        *
 * J. Wu/SAIC      	03/02   end pending oper. before switching layer*
 * E. Safford/SAIC	03/02	add pglayred_popdown()			*
 * E. Safford/SAIC	03/02	rm the attritbute reset for cur layer	*
 * J. Wu/SAIC      	03/02   show current file name at bottom of win.*
 * H. Zeng/EAI          03/02   modified to use pggrpch_popdown()       *
 * E. Safford/SAIC	11/03	mv functionality to pglayrw_setActvLayer*
 ***********************************************************************/
{
    int iret;
/*---------------------------------------------------------------------*/

    pglayrw_setActvLayer( (int)which, &iret );
}

/*=====================================================================*/
/* ARGSUSED */
static void pglayrw_checkbCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * pglayrw_checkbCb                                                     *
 *                                                                      *
 * Callback function for layer control check buttons.     		*
 *                                                                      *
 * void pglayrw_checkbCb(wdgt, which, call)                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt          Widget     widget ID                                  *
 *  which         long        button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI     	02/02   initial coding                          *
 ***********************************************************************/
{
XmToggleButtonCallbackStruct 
                  *cbs = (XmToggleButtonCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    pglayer_setDsplOn((int)which, cbs->set); 
    pglayrw_refresh (); /* redraw and update the display */     
}

/*=====================================================================*/
/* ARGSUSED */
static void pglayrw_editCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * pglayrw_editCb                                                       *
 *                                                                      *
 * Callback function for layer control window Column 3 push buttons.    *
 *                                                                      *
 * void pglayrw_editCb(wdgt, which, call)                               *
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt          Widget     widget ID                                  *
 *  which         long       button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI     02/02        initial coding                          *
 ***********************************************************************/
{
    pglayred_popup ( which );
}

/*=====================================================================*/

void pglayrw_refresh ( void )
/************************************************************************
 * pglayrw_refresh    							*
 *									*
 * This function refeshes the drawn pgen elements.  The current pixmap	*
 * is cleared and restored from the master copy, then cvg_redraw is 	*
 * called to redraw pgen elements.  The xw driver will then redraw the	*
 * pgen elements for all the remaining pixmaps in the loop.		*
 *									*
 * void pglayrw_refresh ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 **									*
 * Log:									*
 * J. Wu/SAIC	   	02/02   copied from pgpalw_refresh()		*
 * T. Piper/SAIC	0603	added XtDisplay	and XtWindow		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    Widget	canvas;
    Dimension	width, height;
    int		ier;
/*---------------------------------------------------------------------*/
/* 
 * clear current display 
 */
    canvas = (Widget)mcanvw_getDrawingW();
    XtVaGetValues( canvas,
                   XmNwidth,  		&width,
                   XmNheight, 		&height,
                   NULL );

    XClearArea( XtDisplay(canvas), XtWindow(canvas), 
               0, 0, width, height, False );

/*
 * copy from master pixmap to displayable pixmaps in gemwindow.
 */
    xpgrestlp();

/* 
 *  redraw VG elements.
 */
    cvg_redraw( cvg_getworkfile(), &ier );

/*
 * tell xw driver to refresh the vector graphics.
 */
    xpgrfrsh();
    crg_rebuild(); 
}

/*=====================================================================*/

static void pglayrw_turnLayrBtnOn ( int layer )
/************************************************************************
 * pglayrw_turnLayrBtnOn						*
 *									*
 * This function sets the background and foreground color of current    *
 * PGEN layer name pushbutton to display the button as switched on.     *
 *									*
 * void pglayrw_turnLayrBtnOn( layer )					*
 *									*
 * Input parameters:							*
 *	layer		int	the current layer	                *
 *									*
 * Output parameters:							*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * E. Safford/SAIC        02/02	initial coding				*
 ***********************************************************************/
{
    XtVaSetValues (_layerW[layer].namepb,
           XmNforeground, 	   _bg,
           XmNbackground, 	   _fg,
           NULL);
}

/*=====================================================================*/

static void pglayrw_turnLayrBtnOff ( int layer )
/************************************************************************
 * pglayrw_turnLayrBtnOff						*
 *									*
 * This function sets the background and foreground color of current    *
 * PGEN layer name pushbutton to display the button as switched off.    *
 *									*
 * void pglayrw_turnLayrBtnOff( layer )					*
 *									*
 * Input parameters:							*
 *	layer		int	the current layer	                *
 *									*
 * Output parameters:							*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * E. Safford/SAIC        11/03	initial coding				*
 ***********************************************************************/
{
    XtVaSetValues (_layerW[layer].namepb,
           XmNforeground, 	   _fg,
           XmNbackground, 	   _bg,
           NULL);
}

/*=====================================================================*/

void pglayrw_addLayer ( void )
/************************************************************************
 * pglayrw_addLayer							*
 *									*
 * This function adds a layer responding to "Add Layer" button click.   *
 *									*
 * void pglayrw_addLayer()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 * H. Zeng/EAI          03/02   added call to pgpalw_setBtnSntv()       *
 * E. Safford/SAIC	11/03	call pglayrw_setActvLayer, not _nameCb  *
 ***********************************************************************/
{
    int		layer_num, cur_layer, iret;
    long	ii;
/*---------------------------------------------------------------------*/
 
    layer_num = pglayer_getNumLayers();
    if ( layer_num >= MAX_LAYERS ) {
	return;
    }
    else if ( layer_num == MAX_LAYERS-1 ) {

/*
 * If about to reach MAX_LAYERS, set "ADD LAYER" button insensitive.
 */
	pgpalw_setBtnSntv (FUNC_LAYER, FALSE);    
    }

    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {
	if ( !pglayer_getInUse (ii) ) {
	    pglayer_setInUse (ii, TRUE);

/*
 * Make the newly added layer the active layer.
 */
	    pglayrw_setActvLayer( ii, &iret );
	    break;
        }
    }
    pglayrw_manageLayers ();

/*
 * Popup the Layer Name Window.
 */
    cur_layer = pglayer_getCurLayer();
    pglayrnm_popup (cur_layer);
}

/*=====================================================================*/

void pglayrw_manageLayers ( void )
/************************************************************************
 * pglayrw_manageLayers							*
 *									*
 * This function manages|unmanages layers according to the in_use flags *
 *									*
 * void pglayrw_manageLayers()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 * S. Jacobs/NCEP	09/02	Added unmanage/manage of _pglayrwWin	*
 * T. Piper/SAIC	02/06	Modified to use XtManageChildREN	*
 * T. Piper/SAIC	02/06	Re-done for AWIPS issue w/KDE bluecurve	*
 * T. Piper/SAIC	07/06	Removed AWIPS KDE bluecurve stuff	*
 * H. Zeng/SAIC		07/06   Set height for outer shell widget	*
 ***********************************************************************/
{
    int   ii;
    WidgetList layerlist;
    Cardinal num_children;
    Dimension height;
/*---------------------------------------------------------------------*/
    num_children = 0;
    layerlist = (WidgetList) XtMalloc(MAX_LAYERS * sizeof(Widget));
    XtUnmanageChild(_pglayrwWin);
    XtUnmanageChild(_layerRC );
    for (ii = 0; ii < MAX_LAYERS; ii++) {

/*
 * Check in_use flag for each layer.
 */
	XtUnmanageChild(_layerW[ii].form);
	if ( pglayer_getInUse(ii) ) {
	    layerlist[num_children] = _layerW[ii].form;
	    num_children++;
	}
    }
    XtManageChildren(layerlist, num_children);
    XtFree((XtPointer)layerlist);
    XtManageChild(_layerRC );
    XtManageChild(_pglayrwWin);

    XtVaGetValues(_pglayrwWin, XmNheight, &height, NULL);
    XtVaSetValues(XtParent(_pglayrwWin), XmNheight, height, NULL);
    XmUpdateDisplay(XtParent(_pglayrwWin));
}

/*=====================================================================*/

void pglayrw_updtSettings ( long layer )
/************************************************************************
 * pglayrw_updtSettings							*
 *									*
 * This function updates the display of current PGEN layer control      *
 * widget according to its attributes. 		                        *
 *									*
 * void pglayrw_updtSettings ( layer )					*
 *									*
 * Input parameters:							*
 *	layer		int	the current layer	                *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * H. Zeng/EAI 	        02/02	initial coding				*
 * E. Safford/SAIC	03/02	add update to the checkb value		* 
 ***********************************************************************/
{
    char        name[20], attr[10];
    XmString	xmstr;
    Pixel	bg;
    Boolean	check_val;
/*---------------------------------------------------------------------*/  
/*
 * Update the label for name pushbutton.
 */
    sprintf( name, "%s", pglayer_getName(layer) );
    xmstr = XmStringCreateLocalized(name);
    XtVaSetValues(_layerW[layer].namepb,
                  XmNlabelString,        xmstr,
		  NULL);
    XmStringFree(xmstr);

/*
 * Update the display for edit pushbutton.
 */
    if ( pglayer_getDsplClr(layer) ) {
         sprintf(attr, " A/");
    }
    else {
         sprintf(attr, " M/");
    }

    if ( pglayer_getFill(layer) ) {
         strcat(attr, "F ");
    }
    else {
         strcat(attr, "N ");
    }
    xmstr = XmStringCreateLocalized(attr);
    bg = (Pixel)NxmColrP_getColorPixel
                ( pglayer_getMonoClr(layer) ); 
   
    XtVaSetValues(_layerW[layer].editpb,
                  XmNbackground,         bg, 
                  XmNlabelString,        xmstr,
		  NULL);

    XmStringFree(xmstr);

/*
 *  Set the check button value.  Remove the callback before setting
 *  the value to avoid the element redisplay that the value changed
 *  callback orders.
 */
    check_val = pglayer_getDsplOn (layer);

    XtRemoveAllCallbacks (_layerW[layer].checkb, XmNvalueChangedCallback);
    XtVaSetValues ( _layerW[layer].checkb, 
    			XmNset, 		check_val, 
			NULL);

    XtAddCallback(_layerW[layer].checkb, XmNvalueChangedCallback,
		      (XtCallbackProc)pglayrw_checkbCb, (XtPointer)layer);
}

/*=====================================================================*/

static void pglayrw_removeLayerCallbacks( void )
/************************************************************************
 * pglayrw_removeLayerCallbacks						*
 *									*
 * This function removes all callbacks from the layer check boxes       *
 *									*
 * void pglayrw_removeLayerCallbacks( void ) 				*
 *									*
 * Input parameters:							*
 *	none								*
 * Output parameters:							*
 *	none								*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/04	Created					* 
 ***********************************************************************/
{ 
    int ii = 0;
/*---------------------------------------------------------------------*/
    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {
        XtRemoveAllCallbacks (_layerW[ ii ].checkb, XmNvalueChangedCallback);
    }
}

/*=====================================================================*/

static void pglayrw_addLayerCallbacks( void )
/************************************************************************
 * pglayrw_addLayerCallbacks						*
 *									*
 * This function adds all callbacks from the layer check boxes		*
 *									*
 * void pglayrw_addLayerCallbacks( void ) 				*
 *									*
 * Input parameters:							*
 *	none								*
 * Output parameters:							*
 *	none								*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/04	Created					* 
 ***********************************************************************/
{ 
    long ii = 0;
/*---------------------------------------------------------------------*/
    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {
        XtAddCallback( _layerW[ ii ].checkb, XmNvalueChangedCallback,
		      (XtCallbackProc)pglayrw_checkbCb, (XtPointer)ii);
    }
}
    
/*=====================================================================*/
/* ARGSUSED */
void pglayrw_setLayer( Widget wdgt, XEvent *evt, String *parms, Cardinal *num )
/************************************************************************
 * pglayrw_setLayer							*
 *									*
 * This function is to set the current layer using hot keys.		*
 *									*
 * void pglayrw_setLayer( wid, evt, parms, num ) 			*
 *									*
 * Input parameters:							*
 *  wdgt          Widget     widget ID                                  *
 *  *evt	  XEvent     event that invoked the action              *
 *  *parms        String     argument strings    			*
 *  *num          Cardinal   number of arguments    			*
 *                                                                      *
 * Output parameters:							*
 *	none								*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/04	Created					* 
 * B. Yin/SAIC		08/04	Changed keycode to keysym 		* 
 * B. Yin/SAIC		09/05	Changed the single letter variables 	*
 ***********************************************************************/
{ 
   int  	iret = 0;
   char 	buf[ 16 ];
   KeySym 	keysym;
   XComposeStatus compose;
/*---------------------------------------------------------------------*/  

   if ( pglayrw_isUp() ) {

       XLookupString ( &(evt->xkey), buf, sizeof ( buf ), &keysym, &compose );

       switch ( keysym ) {

            case XK_1:                          /* layer 1 */
		     if ( pglayer_getInUse ( 0 ) ) {
                        pglayrw_setActvLayer( 0, &iret );
		     }
                     break;
            case XK_2:                          /* layer 2 */
		     if ( pglayer_getInUse ( 1 ) ) {
                        pglayrw_setActvLayer( 1, &iret );
		     }
                     break;
            case XK_3:                          /* layer 3 */
		     if ( pglayer_getInUse ( 2 ) ) {
                        pglayrw_setActvLayer( 2, &iret );
		     }
                     break;
            case XK_4:                          /* layer 4 */
		     if ( pglayer_getInUse ( 3 ) ) {
                        pglayrw_setActvLayer( 3, &iret );
		     }
                     break;
            case XK_5:                          /* layer 5 */
		     if ( pglayer_getInUse ( 4 ) ) {
                        pglayrw_setActvLayer( 4, &iret );
		     }
                     break;
            case XK_6:                          /* layer 6 */
		     if ( pglayer_getInUse ( 5 ) ) {
                        pglayrw_setActvLayer( 5, &iret );
		     }
                     break;
            case XK_7:                          /* layer 7 */
		     if ( pglayer_getInUse ( 6 ) ) {
                        pglayrw_setActvLayer( 6, &iret );
		     }
                     break;
            case XK_8:                          /* layer 8 */
		     if ( pglayer_getInUse ( 7 ) ) {
                        pglayrw_setActvLayer( 7, &iret );
		     }
                     break;
            case XK_9:                          /* layer 9 */
		     if ( pglayer_getInUse ( 8 ) ) {
                        pglayrw_setActvLayer( 8, &iret );
		     }
                     break;
            case XK_0:                          /* layer 10 */
		     if ( pglayer_getInUse ( 9 ) ) {
                        pglayrw_setActvLayer( 9, &iret );
		     }
                     break;
            case XK_exclam:                     /* layer 11 */
		     if ( pglayer_getInUse ( 10 ) ) {
                        pglayrw_setActvLayer( 10, &iret );
		     }
                     break;
            case XK_at:                         /* layer 12 */
		     if ( pglayer_getInUse ( 11 ) ) {
                        pglayrw_setActvLayer( 11, &iret );
		     }
                     break;
            case XK_numbersign:                 /* layer 13 */
		     if ( pglayer_getInUse ( 12 ) ) {
                        pglayrw_setActvLayer( 12, &iret );
		     }
                     break;
            case XK_dollar:                     /* layer 14 */
		     if ( pglayer_getInUse ( 13 ) ) {
                        pglayrw_setActvLayer( 13, &iret );
		     }
                     break;
            case XK_percent:                    /* layer 15 */
		     if ( pglayer_getInUse ( 14 ) ) {
                        pglayrw_setActvLayer( 14, &iret );
		     }
                     break;
            case XK_asciicircum:                /* layer 16 */
		     if ( pglayer_getInUse ( 15 ) ) {
                        pglayrw_setActvLayer( 15, &iret );
		     }
                     break;
            case XK_ampersand:                  /* layer 17 */
		     if ( pglayer_getInUse ( 16 ) ) {
                        pglayrw_setActvLayer( 16, &iret );
		     }
                     break;
            case XK_asterisk:                   /* layer 18 */
		     if ( pglayer_getInUse ( 17 ) ) {
                        pglayrw_setActvLayer( 17, &iret );
		     }
                     break;
            case XK_parenleft:                  /* layer 19 */
		     if ( pglayer_getInUse ( 18 ) ) {
                        pglayrw_setActvLayer( 18, &iret );
		     }
                     break;
            case XK_parenright:                 /* layer 20 */
		     if ( pglayer_getInUse ( 19 ) ) {
                        pglayrw_setActvLayer( 19, &iret );
		     }
                     break;
            default:
                     break;
       }
   }
}

/*=====================================================================*/

static void pglayrw_initAllBtn( void )       
/************************************************************************
 * pglayrw_initAllBtn      						*
 *									*
 * This function starts the All On/Off button to "All Off".		*
 *									*
 * void pglayrw_initAllBtn( void )	 				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 **									*
 * Log:									*
 * E. Safford/SAIC	08/04	Initial coding				* 
 * B. Yin/SAIC		09/05	Changed _allOnOff to a single Widget	*
 ***********************************************************************/
{ 
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    _allOn = FALSE;
    xmstr = XmStringCreateLtoR( ALL_OFF, XmFONTLIST_DEFAULT_TAG );

    XtVaSetValues( _allOnOff, XmNlabelString, xmstr, NULL );
    XmStringFree( xmstr );

}

/*=====================================================================*/

void pglayrw_getLayerName ( char *layerName )
/************************************************************************
 * pglayrw_getLayerName							*
 *									*
 * This function gets the active layer's name.				*
 *									*
 * void pglayrw_getLayerName ( layerName ) 				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*layerName	char	Current layer's name			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				* 
 ***********************************************************************/
{     
    if ( pglayrw_isUp() ) {
        strcpy ( layerName, pglayer_getName ( pglayer_getCurLayer() ) );
    }
    else {
        layerName[0] = '\0';
    }
}
    
/*=====================================================================*/

void pglayrw_matchLayer ( const char *newLayer )
/************************************************************************
 * pglayrw_matchLayer							*
 *									*
 * This function matches the new layer to all available layers.	If a	*
 * match is found, the macthed layer will be set as the active layer.	*
 *									*
 * void pglayrw_matchLayer ( newLayer ) 				*
 *									*
 * Input parameters:							*
 *	*newLayer	const char	Layer to be matched		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				* 
 ***********************************************************************/
{     
    int		ii, layer_num, ier;
    char	*nm_ptr; 
/*---------------------------------------------------------------------*/
 
    layer_num = pglayer_getNumLayers();
    
    for ( ii = 0; ii < layer_num; ii++ ) {
	
	nm_ptr = pglayer_getName ( ii );
        
	if ( strcasecmp ( nm_ptr, newLayer ) == 0 ) {
            pglayrw_setActvLayer ( ii, &ier ); 
	    break;
	}	
    }    
}
    
/*=====================================================================*/

static void pglayrw_expand ( Widget wdgt )
/************************************************************************
 * pglayrw_expand							*
 *									*
 * This routine expands or collapses the layer control window.		*
 *									*
 * void pglayrw_expand ( wdgt ) 					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		Widget ID of the expand arrow button	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		09/05	Created					* 
 * T. Piper/SAIC	02/06	Modified to call XtManageChildREN	*
 * T. Piper/SAIC	02/06	Re-done for AWIPS issue w/KDE bluecurve	*
 * T. Piper/SAIC	07/06	Removed AWIPS KDE bluecurve stuff	*
 * H. Zeng/SAIC		07/06   added manage/unmanage for bluecurve	*
 ***********************************************************************/
{
    int 		ii;
    unsigned char	direction;
    WidgetList		layerlist;
    Cardinal		num_children;
    static Dimension    smallWidth = 0;
/*---------------------------------------------------------------------*/
/*
 *  Unmanage the outer Dialog Shell widget.
 */
    XtUnmanageChild(XtParent(_pglayrwWin));

/*
 *  Get the arrow direction
 */
    XtVaGetValues ( wdgt, XmNarrowDirection, &direction, NULL );

    num_children = 0;
    if ( direction == XmARROW_RIGHT ) {		/*  expand  */

        XtVaSetValues ( wdgt, XmNarrowDirection, XmARROW_LEFT, NULL );

/*
 *  Get the width before expanding.  It will be used when collapsing.
 */
	if ( smallWidth == 0 ) {
    	   XtVaGetValues( _layerRC, XmNwidth, &smallWidth, NULL );
	}

/*
 *  Bring up the 'Edit Name' button and the color edit buttons.
 */
	XtUnmanageChild(_ctlform);
	XtUnmanageChild(_layerRC);

	layerlist = (WidgetList) XtMalloc((MAX_LAYERS*2) * sizeof(Widget));
	for ( ii = 0; ii < MAX_LAYERS; ii++ ) {

	    XtUnmanageChild(_layerW[ii].form); 
	    XtManageChild(_layerW[ii].editpb);

	    if ( pglayer_getInUse(ii) ) {
		layerlist[num_children] = _layerW[ii].form;
		num_children++;
            }
	}
	XtManageChildren(layerlist, num_children);
	XtManageChild(_editName );

    }
    else {		/*  collapse  */

/*
 *  Set arrow direction.
 */
        XtVaSetValues ( wdgt, XmNarrowDirection, XmARROW_RIGHT, NULL );

/*
 *  Unmanage the 'Edit Name' button and the color buttons.
 */
	XtUnmanageChild(_editName );
	XtUnmanageChild(_ctlform );
	XtUnmanageChild(_layerRC );
	layerlist = (WidgetList) XtMalloc(MAX_LAYERS * sizeof(Widget));

        for ( ii = 0; ii < MAX_LAYERS; ii++ ) {

	    XtUnmanageChild(_layerW[ii].form ); 
            XtUnmanageChild(_layerW[ii].editpb );
	    if ( pglayer_getInUse(ii) ) {
		layerlist[num_children] = _layerW[ii].form;
		num_children++;
	    }
        }

	XtManageChildren(layerlist, num_children);     
        XtVaSetValues( _layerRC, XmNwidth, smallWidth, NULL );
        XtVaSetValues( _ctlform, XmNwidth, smallWidth, NULL );
    }
    XtFree((XtPointer)layerlist);
    XtManageChild(_layerRC );
    XtManageChild(_ctlform );

/*
 *  Manage the outer Dialog Shell widget.
 */
    XtManageChild(XtParent(_pglayrwWin));
}

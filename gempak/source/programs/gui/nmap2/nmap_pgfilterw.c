#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h" 
#include "hints.h"
#include "Nxm.h"
#include "proto_xw.h"


#define	ACTV_FILTER	"NONE"	/* Filter window up but no filters checked */
#define	CANCEL_FILTER	"ALL"	/* Filter window down */

#define MAX_FILTYPES	10	/* Max. number of filter types */

#define	TIME_FILTER	0	/* Time filter */
#define ALL_ON		"All On"
#define ALL_OFF		"All Off"

static	int		_nFilTime[MAX_FILTYPES];
static	char		_filTime[MAX_FILTYPES][MAX_FILTER_NUM][DSPLY_FILTER_SZ];
static  Boolean 	_filterStatus[MAX_FILTER_NUM];

static Widget		_filterForm;
static WidgetList 	_filterChkBtn;
static WidgetList 	_cntlBtn;

static char		_curFilter[512];

static Boolean		_allOn = True;	/* Toggle between ALL_ON & ALL_OFF */

static Boolean		_offByHotkey = False;	/* Button turned off by hotkey */

static Boolean		_fromGfa = False;

/*
 *  private callback functions
 */
static void pgfilterw_ctlBtnCb ( Widget, long, XtPointer );
static void pgfilterw_toggleCb ( Widget, long, XtPointer );

/*
 *  private functions
 */
static void pgfilterw_updateFilter ( void );
static void pgfilterw_refresh ( void );
static void pgfilterw_getLastTime ( char *selectedTime );


/************************************************************************
 * nmap_pgfilterw.c                                        		*
 *                                                                      *
 * This module controls the display filter functions.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   pgfilterw_create()		creates filter control window		*
 *   pgfilterw_popup()		pops up	filter control window		*
 *   pgfilterw_popdown()	pops down filter control window		*
 *   pgfilterw_isUp()		Queries if filter control window is up	*
 *   pgfilterw_stepOneUp()	Moves one up from the current filter	*
 *   pgfilterw_stepOneDown()	Moves one down from the current filter	*
 *									*
 * private callback functions:						*
 *   pgfilterw_toggleCb()	callback for filter toggle buttons	*
 *   pgfilterw_ctlBtnCb()	callback for filter control buttons	*
 *									*
 * private functions:							*
 *   pgfilterw_updateFilter()	updates current filter and redisplay	*
 *   pgfilterw_refresh()	updates display				*
 *   pgfilterw_turnOnTime       turns on a given filter time		*
 *   pgfilterw_getLastTime     	gets the name of the last selected time	*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgfilterw_create ( Widget parent )
/************************************************************************
 * pgfilterw_create							*
 *									*
 * This function creates the display filter control window.		*
 *									*
 * void pgfilterw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * E. Safford/SAIC	08/04	make filter times horizontal layout &  	*
 *				  use ALL_ON/ALL_OFF definitions	*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * J. Wu/SAIC		06/06	load table from CVG lib & adjust layout *
 * M. Li/SAIC		02/07	Change callback type for filter hour	*
 ***********************************************************************/
{
    int		ier;    
    long	ii, nn;
    char	*ctlstrs[] = { ALL_ON, "Close" };    
    Widget	pane, optform, cntl_form;
    XmString	xmstr;    
/*---------------------------------------------------------------------*/
/*
 *  Load the filter table.
 */
    cvg_rdfilter ( &ier );
    
/*
 *  Initialize filter selections in CVG library
 */
    strcpy ( _curFilter, CANCEL_FILTER ); 
    cvg_setfilter ( _curFilter, &ier );
    
/*
 *  Retrieve all valid filter entries 
 */
    cvg_gettblfilter ( &_nFilTime[TIME_FILTER],
                       _filTime[TIME_FILTER], &ier );

    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
        _filterStatus[ii] = False;
    }
                  
/*
 *  Create main dialog window.
 */
    _filterForm = XmCreateFormDialog ( parent, "pgfilterw_popup", NULL, 0 );
    
    xmstr = XmStringCreateLocalized("Filter Control");
    XtVaSetValues ( _filterForm,
		XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		NULL );
    XmStringFree(xmstr);

/*
 *  Create pane area.
 */
    pane = XtVaCreateManagedWidget ( "filterw_pane",
		xmPanedWindowWidgetClass, 	_filterForm,
		XmNsashWidth,			1,
		XmNsashHeight,	 		1,
		XmNleftAttachment,  		XmATTACH_FORM,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL );

/*
 *  Create check buttons
 */
    optform = XtVaCreateManagedWidget ( "filterw_optform",
		xmRowColumnWidgetClass,		pane,
		XmNpacking,			XmPACK_TIGHT,
		XmNorientation,			XmHORIZONTAL,
		NULL );
    
    nn = _nFilTime[0];    
    _filterChkBtn = (WidgetList) XtMalloc( (size_t)nn * sizeof(Widget) );

    for ( ii = 0; ii < nn; ii++ ) {		
	_filterChkBtn[ii] = XtVaCreateManagedWidget ( _filTime[0][ii], 
		xmToggleButtonWidgetClass,	optform,
		NULL );  	
                
	XtAddCallback ( _filterChkBtn[ii], XmNvalueChangedCallback,
			(XtCallbackProc)pgfilterw_toggleCb,
			(XtPointer) ii );        
    }
           
/*
 *  Create control buttons
 */
    nn = XtNumber ( ctlstrs );
    _cntlBtn = (WidgetList) XtMalloc( (size_t)nn * sizeof(Widget) );
    cntl_form = XtVaCreateManagedWidget ( "filter_cntl_form",
		xmFormWidgetClass,		pane,
		XmNfractionBase,		nn * 100,
		NULL );

    for ( ii = 0; ii < nn; ii++ ) {
	_cntlBtn[ii] = XtVaCreateManagedWidget ( ctlstrs[ii], 
		xmPushButtonWidgetClass,	cntl_form, 
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		((ii * 100) + 10 ),
		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		(((ii + 1) * 100) - 10),

		NULL );

	XtAddCallback ( _cntlBtn[ii], XmNactivateCallback,
		(XtCallbackProc)pgfilterw_ctlBtnCb, (XtPointer) ii );
    }
    
/*
 *  Make the "ALL ON/ALL OFF" button insensetive if no valid filters
 *  found from the filter table.
 */
    if ( _nFilTime[TIME_FILTER] <= 0 ) {
        XtSetSensitive ( _cntlBtn[0], False );        
    }
}

/*=====================================================================*/

void pgfilterw_popup ( void )
/************************************************************************
 * pgfilterw_popup							*
 *									*
 * This function manages the display filter window.			*
 *									*
 * void pgfilterw_popup ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * E. Safford/SAIC	08/04   use ALL_ON/ALL_OFF definitions		*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if ( !XtIsManaged ( _filterForm ) ) {        
        XtManageChild ( _filterForm );        
    }
    
/*
 *  At the beginning, set to ALL_OFF status, none of the elements
 *  with time stamp should be displayed.
 */
    _allOn = False;
    xmstr = XmStringCreateLtoR ( ALL_ON,
                                 XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _cntlBtn[0], XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );
    
    pgfilterw_updateFilter ();

}

/*=====================================================================*/

void pgfilterw_popdown ( void )
/************************************************************************
 * pgfilterw_popdown							*
 *									*
 * This function unmanages the display filter window.			*
 *									*
 * void pgfilterw_popdown ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _filterForm ) ) {
	XtUnmanageChild ( _filterForm );
    }
    
/*
 *  Deactivate filter, so all elements could be displayed.
 */    
    _allOn = False;
    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
         _filterStatus[ii] = False;
         XmToggleButtonSetState ( _filterChkBtn[ii], False, False );	
    }
    pgfilterw_updateFilter ();           
}

/*=====================================================================*/

Boolean pgfilterw_isUp ( void )
/************************************************************************
 * pgfilterw_isUp							*
 *									*
 * Query whether the filter control dialog is managed or not.		*
 *									*
 * Boolean pgfilterw_isUp ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 *									*
 * Return parameters:							*
 *	pgfilterw_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_filterForm) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilterw_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgfilterw_cntlBtnCb							*
 *									*
 * Callback function for the control buttons in filter window.		*
 *									*
 * static void pgfilterw_ctlBtnCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding 				*
 * E. Safford/SAIC	08/04   use ALL_ON/ALL_OFF definitions		*
 ***********************************************************************/
{
    int		ii;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    switch ( which ) {
        case 0:  /* All On/Off - toggle on/off all filters in window */
	    
	    if ( _allOn ) {
	        _allOn = False;		
                xmstr = XmStringCreateLtoR ( ALL_ON, XmFONTLIST_DEFAULT_TAG );
	    
	    }
	    else {
	        _allOn = True;
                xmstr = XmStringCreateLtoR ( ALL_OFF, XmFONTLIST_DEFAULT_TAG );
	    }           
	    
            XtVaSetValues ( _cntlBtn[0], XmNlabelString, xmstr, NULL );
	    XmStringFree ( xmstr );		
		
	    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
                _filterStatus[ii] = _allOn;
                XmToggleButtonSetState ( _filterChkBtn[ii], _allOn, False );	
	    }
	
	    pgfilterw_updateFilter ();    
	    break;

        case 1:	/* Close - close window and display all elements */
	    pgfilterw_popdown ();
	    break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilterw_toggleCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgfilterw_toggleCb                                        		*
 *                                                                      *
 * Callback function for filter check buttons.     			*
 *                                                                      *
 * static void pgfilterw_toggleCb ( wid, which, call )                	*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid		Widget		widget ID			*
 *	which		long        	button index			*
 *	call		XtPointer	not used			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           07/04   initial coding                  	*
 ***********************************************************************/
{
    if ( _filterStatus[which] ) {
         _filterStatus[which] = False;
    }
    else {
         _filterStatus[which] = True;       
    }
    pgfilterw_updateFilter ();    
}

/*=====================================================================*/

static void pgfilterw_updateFilter ( void )
/************************************************************************
 * pgfilterw_updateFilter						*
 *									*
 * Updates the current filter string and then refreshes the display. 	*
 *									*
 * static void pgfilterw_updateFilter ( void )				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * J. Wu/SAIC		08/04	link filter change with GFA window	*
 * J. Wu/SAIC		09/04	Restart GFA for any filter changes	*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_getFld()	*
 * E. Safford/SAIC	07/05	rm sequence # from cvg_scangfa() call	*
 * M. Li/SAIC		03/07	Not reset fcst hour if change from GFA	*
 * E. Safford/SAIC	05/07	fix gfa' connection			*
 * E. Safford/SAIC	06/07	fix bug in preFilterOn determination	*
 * B. Yin/SAIC		12/07	set GFA hr to last selected filter time	*
 * J. Wu/SGT		06/14	Unset operation for a new selection 	*
 ***********************************************************************/
{
    int 	ii, filternum, ier, cur_obj, cur_loc, newel_loc, num;
    int 	subtyp, areatyp, len;
    char	lastTime[8], value[32], tag[32], timeCmp[10];
    Boolean	gfaIsUp = False, gfaInAdd = False, preFilterOn = False;
    Boolean	gfaPrimeIsUp = False;
    VG_DBStruct	cur_el, newel;
/*---------------------------------------------------------------------*/
       
    cur_el.elem.gfa.info.nblocks = 0;

    _curFilter[0] = '\0';
    filternum = 0;
    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
        if ( _filterStatus[ii] ) {           
	    filternum++;
	    strcat ( _curFilter, _filTime[TIME_FILTER][ii] );	
	    if ( ii < (_nFilTime[TIME_FILTER] - 1) ) {
	        strcat ( _curFilter, ";" );
	    }
	}
    }
    
/*
 *  If no filter is provided but the filter window is up - none
 *  of the elements with time stamp will be displayed. If the
 *  filter window is down, all elements should be displayed.
 */    
    if ( filternum == 0 ) {
        if ( pgfilterw_isUp() ) {
	    strcpy ( _curFilter, ACTV_FILTER ); 
	}
	else {
	    strcpy ( _curFilter, CANCEL_FILTER );		
	}
    }

/*
 *   Check if gfaw is up and its mode. 
 */  
    gfaIsUp = pggfaw_isUp(); 
    gfaPrimeIsUp = pggfawp_isUp();

    if( gfaIsUp ) {
        gfaInAdd =  pggfaw_isAddMode();
    }
    else if( gfaPrimeIsUp ) {
        gfaInAdd =  pggfawp_isAddMode();
    }
    cur_obj = pgpalw_getCurObjId ();

/*
 *  If gfaw is up, retrieve the current GFA attributes and check
 *  if the previous forecast hour is still "ON" in filter window.
 *
 *  If it is still on, then take no action.  If it is not, and there
 *  is another time selected, set the GFA/GFA' window forecast hour to
 *  the first selected time.  
 *
 *  Note that the GFA/GFA' will ignore the Airmet and Outlook filter 
 *  settings.
 */  
    cur_loc = -1;
    preFilterOn = False;   
    
    if ( gfaIsUp || gfaPrimeIsUp ) {
	
	if ( gfaInAdd ) {
	    if( gfaIsUp ) {
                pggfaw_getAttr ( &cur_el );
                ier = 0;
	    }
	    else {
                pggfawp_getAttr ( &cur_el );
                ier = 0;
	    }
	}
	else {
	    cur_loc = pgactv_getElmLoc ();
            cvg_rdrec ( cvg_getworkfile(), cur_loc, &cur_el, &ier );
	}

	cvg_getFld ( &cur_el, TAG_GFA_FCSTHR, value, &ier );

        if ( filternum > 0 && ier == 0 ) {

            for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {

	        if (  _filterStatus[ii] ) { 

		    strcpy( timeCmp, _filTime[TIME_FILTER][ii] );
		    len = strlen( timeCmp );

		    if( timeCmp[ len-1 ] == '+' ) {
			timeCmp[ len-1 ] = '\0';
                    }

		    if ( strcmp ( value, timeCmp ) == 0 )  {        
	                preFilterOn = True;   
	                break;
		    }    
	        }	    
            }	
        }		
    }
    
/*
 *  Update filter setting in CVG library.
 */    
    cvg_setfilter ( _curFilter, &ier ); 
        
/*
 *  Refresh the display.
 */ 
    pgfilterw_refresh ();
        

/*
 *  Adjust GFA window, if necessary.
 */    
    if ( gfaIsUp || gfaPrimeIsUp ) {	        	
		
/*  
 * If previously in "Add" mode, keep drawing.
 */
	if ( gfaInAdd ) {

/*  
 *  Set the GFA hour to the last selected filter time.
 *  
 */
	   pgfilterw_getLastTime ( lastTime );
           if ( strlen ( lastTime ) > (size_t)0 ) {
	       if (!_fromGfa) {
		  if( gfaIsUp ) {
		    pggfaw_setHour ( lastTime );
		  }
		  else {
		    pggfawp_setHour ( lastTime );
		  }
	       }
           }
	}
	else {	   /* Previously in "Edit" mode */	    
	    
/*
 *  The filter matching the active element's forecast hour
 *  is still ON, keep editing it.
 */
	    if ( preFilterOn ) {
                cvg_freeElPtr( &cur_el );
		pghdlb_select ( &cur_el, cur_loc );
	    }
	    else {  
	        
/*
 *  The filter matching the active element's forecast
 *  hour is OFF, terminate all pending actions. 
 *  However, if it is turned off by pressing hotkeys,
 *  and a GFA element matching the same subtype,
 *  and area type number could be found, 
 *  set to edit that element.
 */		
		if ( _offByHotkey  ) {
	            cvg_getFld ( &cur_el, TAG_GFA_SUBTYPE, value, &ier );
	            subtyp = atoi ( value );
	            cvg_getFld ( &cur_el, TAG_GFA_AREATYPE, value, &ier );
	            areatyp = atoi ( value );
  	            cvg_getFld ( &cur_el, TAG_GFA_TAG, tag, &ier );
  		    
		    cvg_scangfa ( NULL, pglayer_getCurLayer(),
			  subtyp, areatyp, tag, &newel, &newel_loc, &ier );

		    if ( ier == 0 ) {
                        crg_getinx ( cur_loc, &num, &ier);
	                pghdlb_deselectEl ( num, TRUE );
	                
			pgactv_setActvElm ( &newel, newel_loc );
		        pghdlb_select ( &newel, newel_loc );
	                          
			if( gfaIsUp ) {              
			    pggfaw_setAttr ( &newel );
			}
			else {
			    pggfawp_setAttr ( &newel );
			}
                        
		    }
		    else {
                        pgevt_unsetOper ( TRUE );    
		    }
		}
		else {
                    pgevt_unsetOper ( TRUE ); 		
		}	        
                
		_offByHotkey = False;
	    }
        } /* End "if" in edit mode */
    } 
    // Need to reset the operation to allow a new selection.
    else {
        pgevt_unsetOper ( FALSE );
    } 
  
    cvg_freeElPtr( &cur_el );
    _fromGfa = False;
}

/*=====================================================================*/

void pgfilterw_stepOneUp ( void )
/************************************************************************
 * pgfilterw_stepOneUp							*
 *									*
 * Moves up one filter from the highest one currently selected.		*
 *									*
 * void pgfilterw_stepOneUp ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * J. Wu/SAIC		08/04	set _offByHotkey to True		*
 * J. Wu/SAIC		12/04	disable if the filter window is not up	*
 ***********************************************************************/
{
    int 	ii, highest;
/*---------------------------------------------------------------------*/
    
    if ( !pgfilterw_isUp() ) return;
    
    highest = 0;
    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
	if ( _filterStatus[ii] && ii >= highest ) {
	    highest = ii + 1; 
	}

        _filterStatus[ii] = False;
        XmToggleButtonSetState ( _filterChkBtn[ii], False, False );	
    
    }
        
    if ( highest >= (_nFilTime[TIME_FILTER]) ) {
        highest = _nFilTime[TIME_FILTER] - 1;
    }        
    
    _filterStatus[highest] = True;
    XmToggleButtonSetState ( _filterChkBtn[highest], True, False );	
    
    _offByHotkey = True;
    
    pgfilterw_updateFilter ( );
}

/*=====================================================================*/

void pgfilterw_stepOneDown ( void )
/************************************************************************
 * pgfilterw_stepOneDown						*
 *									*
 * Moves down one filter from the lowest one currently selected.	*
 *									*
 * void pgfilterw_stepOnDown ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * J. Wu/SAIC		08/04	set _offByHotkey to True		*
 * J. Wu/SAIC		12/04	disable if the filter window is not up	*
 ***********************************************************************/
{
    int 	ii, lowest;
/*---------------------------------------------------------------------*/

    if ( !pgfilterw_isUp() ) return;
    
    lowest = _nFilTime[TIME_FILTER] - 1;
    for ( ii = (_nFilTime[TIME_FILTER]-1); ii >= 0; ii-- ) {
	if ( _filterStatus[ii] && ii <= lowest ) {
	    lowest = ii - 1; 
	}

        _filterStatus[ii] = False;
        XmToggleButtonSetState ( _filterChkBtn[ii], False, False );	
    
    }
    
    if ( lowest <= 0 ) {
        lowest = 0;
    }        
    
    _filterStatus[lowest] = True;
    XmToggleButtonSetState ( _filterChkBtn[lowest], True, False );	

    _offByHotkey = True;

    pgfilterw_updateFilter ();
}

/*=====================================================================*/

static void pgfilterw_refresh ( void )
/************************************************************************
 * pgpalw_refresh    							*
 *									*
 * This function refeshes the drawn pgen elements and signals for a     *
 * reload of all frames in the loop.  The current pixmap is cleared and *
 * restored from the master copy, then cvg_redraw is called to redraw 	*
 * pgen elements.  The xw driver will then reload the pgen elements for *
 * all the remaining pixmaps in the loop.  This reload will take place  *
 * the next time they are displayed.					*
 *									*
 * static void pgfilterw_refresh ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 **									*
 * Log:									*
 * J. Wu/SAIC	        07/04   copy from pgpalw_refresh()		*
 * S. Danz/AWC	        07/06   cvg_redraw before crg_rebuild so	*
 *                              autoplacement is ready			*
 * J. Wu/SAIC	        02/08   refresh under user's projection		*
 ***********************************************************************/
{
    Widget	canvas;
    Cardinal	width, height;
    int		ier;
    Boolean	compWinIsUp = False;
/*---------------------------------------------------------------------*/
   
/* 
 * Check if the comptational window is up or not.  The refresh should be
 * done on the user's projection 
 */
   if ( ncw_isup() ) {
       compWinIsUp = True; 
       ncw_unset();
   } 
   

/* 
 * Clear current display 
 */
    canvas = (Widget)mcanvw_getDrawingW();
    XtVaGetValues ( canvas,
		XmNwidth,	&width,
		XmNheight,	&height,
		NULL );

    XClearArea ( XtDisplay(canvas), XtWindow(canvas), 
    			0, 0, width, height, False );

/*
 * For each frame, copy from master pixmap to displayable 
 * pixmaps in gemwindow.
 */
    xpgrestlp ();

/*
 * Tell xw driver to refresh the vector graphics
 * for all pixmaps in the loop.
 */
    xpgrfrsh ();

/* 
 * Load and plot the vg elements in current frame.
 */
    cvg_redraw( cvg_getworkfile(), &ier );

/*
 * Now that we have everything loaded, rebuild the range
 * records
 */
    crg_rebuild ();
    geplot ( &ier );


/* 
 * Reset the comptational window if necessary.
 */
    if ( compWinIsUp ) {
        ncw_set ();
        ncw_sproj ( "PREFS" );
    } 

}

/*=====================================================================*/

void pgfilterw_turnOnTime ( const char *newTime )
/************************************************************************
 * pgfilterw_turnOnTime							*
 *									*
 * Turns on the given new time, if it is in the menu.			*
 *									*
 * void pgfilterw_turnOnTime ( newTime )				*
 *									*
 * Input parameters:							*
 *	*newTime	char	New time to be turned on		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 * M. Li/SAIC           03/07   add cvg_matchfilter                 	*
 * M. Li/SAIC		03/07	Turn on _fromGfa - change from GFA	*
 ***********************************************************************/
{
    int 	ii, ier;
    filter_t	timeMatched, timecmp;
    Boolean	matchAny = True, filter_match;
/*---------------------------------------------------------------------*/

    strcpy ( timecmp, newTime );
    cvg_matchfilter ( timecmp, matchAny, &filter_match, timeMatched, &ier );
    if ( filter_match ) {
	strcpy ( timecmp, timeMatched );
    }
    
    for ( ii = 0; ii < _nFilTime[TIME_FILTER]; ii++ ) {
	if ( !(_filterStatus[ii] ) && 
	    strcasecmp ( timecmp, _filTime[TIME_FILTER][ii] ) == 0 ) {
            	     
	    _filterStatus[ii] = True;
            XmToggleButtonSetState ( _filterChkBtn[ii], True, False );
                        
	    _fromGfa = True;
	    pgfilterw_updateFilter ();	    
	    
	    break;    
	}
    }        
}

/*=====================================================================*/

static void pgfilterw_getLastTime ( char *selectedTime )
/************************************************************************
 * pgfilterw_getLastTime						*
 *									*
 * Gets the name of the last selected filter time.			*
 *									*
 * void pgfilterw_getLastTime ( selectedTime )				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*selectedTime	char	Name of the first selected filter time	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 * B. Yin/SAIC		12/07	change first time to last time		*
 ***********************************************************************/
{
    int 	ii;
/*---------------------------------------------------------------------*/
    
    selectedTime[0] = '\0';
        
    for ( ii =  _nFilTime[TIME_FILTER] - 1; ii >= 0; ii-- ) {
        if ( _filterStatus[ii] ) {
	    strcpy ( selectedTime, _filTime[TIME_FILTER][ii] );
            break;
	}
    }   
}

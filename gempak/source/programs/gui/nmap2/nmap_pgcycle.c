#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"

#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define NUM_DAYS        ( 31 )
#define ROUTINE		( 0 )
#define UPDATE		( 1 )

static char 		_curDay[ 8 ];
static char 		_curCycle[ 8 ];

static Widget		_cycleForm;
static Widget		_dayMenu;
static Widget		_dayPdMenu;
static WidgetList       _dayButtons;

static Widget		_cycleMenu;
static Widget		_cyclePdMenu;
static WidgetList  	_cycleButtons;

static Widget		_form1;
static WidgetList       _ctrlButtons;
static Widget		_issueRadioBox;

static int		_nCycles;
static int		_useCycle = -1;
static int              _issuance = 0;

/*
 *  Private Functions
 */
static void pgcycle_putCycleInTitle ( void ); 
static void pgcycle_createPanel1 ( void );
static void pgcycle_createPanel2 ( void );
static void pgcycle_ctlBtnCb ( Widget, long, XtPointer );

/************************************************************************
 * nmap_pgcycle.c                                                       *
 *                                                                      *
 * This module contains the forecast Cycle and Day selection GUI and    *
 * display setting functions.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *      pgcycle_create()	create the day/cycle selection GUI	*	
 *      pgcycle_popup() 	manage the day/cycle GUI		*
 *	pgcycle_popdown()	unmanage the day/cycle GUI		*
 *	pgcycle_showCycleTime	display day/cycle time in nmap2 title	*
 *      pgcycle_removeCycleTime remove the day/cycle from nmap2 title   *
 *	pgcycle_useCycle()	True if ok to use cycle in prefs.tbl    *
 *	pgcycle_getCycle()	return the day and cycle                *
 *	pgcycle_getIssue()	return the issuance               	*
 *									*
 *	pgcycle_putCycleInTitle put day/cycle time in nmap2 title	*
 *	pgcycle_createPanel1	create the day and cycle selection 	*
 *	pgcycle_createPanel2	create the control buttons		*
 *	pgcycle_ctlBtnCb	callback for Set and Cancel buttons	*
 *      pgcycle_updateCycleTags Update NMAP display/VGF work file when  *
 *                              cycle time has changed                  *
 *                                                                      *
 ***********************************************************************/


/*=====================================================================*/

void pgcycle_create ( Widget parent )
/************************************************************************
 * pgcycle_create                                                      	*
 *                                                                      *
 * This function creates the day/cycle selection GUI and establishes    *
 * default forecast day/cycle time.					*
 *									*
 * void pgcycle_create ( void )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:  		                                        *
 *      		None						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                      	*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    _cycleForm = XmCreateFormDialog ( parent, "cycle_form", NULL, 0 );
    xmstr = XmStringCreateLocalized ( "Cycle Selection" );

    XtVaSetValues ( _cycleForm,
                    XmNnoResize,                    TRUE,
                    XmNautoUnmanage,                FALSE,
                    XmNdialogTitle,                 xmstr,
                    XmNx,                           70,
                    XmNy,                           100,
                    NULL );

    XmStringFree ( xmstr );

    pgcycle_createPanel1();
    pgcycle_createPanel2();   

}


/*=====================================================================*/

void pgcycle_popup ( void )
/************************************************************************
 * pgcycle_popup                                                        *
 *                                                                      *
 * This function manages the day/cycle GUI.  				*
 *                                                                      *
 * void pgcycle_popup( void )                                          	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    int		ii, num_buttons;
    char	*tmpStr;

    XmString    labelStr;
    WidgetList	btns;
/*---------------------------------------------------------------------*/

    if ( !XtIsManaged ( _cycleForm ) ) {

	/* 
	 *  Set the day menu to last saved setting.
	 */
	for( ii=0; ii<NUM_DAYS; ii++ ) {
            
            XtVaGetValues ( _dayButtons[ ii ], 
	    		XmNlabelString, 	&labelStr,
			NULL );
  	    XmStringGetLtoR ( labelStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	    if( strcmp( _curDay, tmpStr ) == 0 ) {
		XtVaSetValues( _dayMenu, XmNmenuHistory, _dayButtons[ii], NULL );
	    }

	    XtFree ( tmpStr );
	    XmStringFree( labelStr ); 
	}


	/* 
	 *  Set the cycle menu to last saved setting.
	 */
	for( ii=0; ii<_nCycles; ii++ ) {
            
            XtVaGetValues ( _cycleButtons[ ii ], 
	    		XmNlabelString, 	&labelStr,
			NULL );
  	    XmStringGetLtoR ( labelStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	    if( strcmp( _curCycle, tmpStr ) == 0 ) {
		XtVaSetValues( _cycleMenu, XmNmenuHistory, 
						_cycleButtons[ii], NULL );
	    }

	    XtFree ( tmpStr );
	    XmStringFree( labelStr ); 
	}


	/* 
	 *  Set the issuance menu to last saved setting
	 */
        XtVaGetValues( _issueRadioBox, XmNnumChildren, &num_buttons, 
			XmNchildren, &btns, NULL ); 
 	XmToggleButtonGadgetSetState( btns[ _issuance ], TRUE, TRUE );	


	XtManageChild( _cycleForm );

    }

}

/*=====================================================================*/

void pgcycle_popdown ( void )
/************************************************************************
 * pgcycle_popdown                                                      *
 *                                                                      *
 * This function unmanages the day/cycle GUI.  				*
 *                                                                      *
 * void pgcycle_popdown( void )                                       	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{

    if ( XtIsManaged ( _cycleForm ) ) {
	XtUnmanageChild( _cycleForm );
    }

}

/*=====================================================================*/

void pgcycle_showCycleTime ( int *iret )
/************************************************************************
 * pgcycle_showCycleTime                                                *
 *                                                                      *
 * This function displays the day/cycle time in the nmap window title.	*
 *                                                                      *
 * void pgcycle_showCycleTime( void )                                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 * Output parameters:                                                   *
 *	*iret	int	return code 	 0 = normal			*
 *					-1 = default day/cycle not set  *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    *iret = 0;

    if( ( strlen( _curDay ) > 0 )  && ( strlen( _curCycle ) > 0 ) ) {
        pgcycle_putCycleInTitle();
    }
    else {
	*iret = -1;
    }
}

/*=====================================================================*/

void pgcycle_removeCycleTime ( void )
/************************************************************************
 * pgcycle_removeCycleTime                                              *
 *                                                                      *
 * This function removes the day/cycle time from the nmap window title.	*
 *                                                                      *
 * void pgcycle_removeCycleTime( void )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    mainw_removeCycle(); 
} 

/*=====================================================================*/

Boolean pgcycle_useCycle ( void )
/************************************************************************
 * pgcycle_useCycle                                                     *
 *                                                                      *
 * This function returns True if the USE_DAY_CYCLE value in the         *
 * prefs.tbl is set to True. 						*
 *									*
 * Boolean pgcycle_useCycle( void )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    char	cval[ 32 ];
    int		ier;

    Boolean	useCycle = False;
/*---------------------------------------------------------------------*/

    if( _useCycle < 0 ) {

	_useCycle = 0;

        ctb_pfstr( "USE_DAY_CYCLE", cval, &ier);
        if( ier >= 0 && strcasecmp( cval, "TRUE" ) == 0 ) {
            _useCycle = 1;
        }    
    }

    if( _useCycle == 1 ) {
	useCycle = True;
    } 

    return( useCycle );

} 

/*=====================================================================*/

void pgcycle_getCycle ( char *day, char *cycle, int *iret )
/************************************************************************
 * pgcycle_getCycle                                                     *
 *                                                                      *
 * This function returns the set day and cycle.                         *
 *									*
 * void pgcycle_getCycle( day, cycle, iret )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * 	day		*char	set day					*
 *	cycle		*char	set cycle				*
 *	iret		*int	return code  0 = normal			*
 *					    -1 = day or cycle not set	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{

    *iret = 0;

    if( ( strlen( _curDay ) <= 0 ) || ( strlen( _curCycle ) <= 0 ) ) {
	*iret = -1;
	return;
    }

    strcpy( day, _curDay );
    strcpy( cycle, _curCycle );     
}

/*=====================================================================*/

static void pgcycle_putCycleInTitle ( void ) 
/************************************************************************
 * pgcycle_putCycleInTitle                                              *
 *                                                                      *
 * This function updates the day/cycle and orders the new settings 	*
 * displayed in the nmap2 window.					*
 *                                                                      *
 * static void pgcycle_putCycleInTitle( )                             	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		None							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    char	cycleStr[ 64 ];
    char	issueStr[ 12 ] = {"Routine"};
/*---------------------------------------------------------------------*/

    if( _issuance == UPDATE ) {
        strcpy( issueStr, "Update" );    
    }

    sprintf( cycleStr, "Day/Cycle:  %s/%sZ  %s", _curDay, _curCycle, issueStr );

    mainw_setTitleName( NULL, cycleStr );
}

/*=====================================================================*/

static void pgcycle_createPanel1 ( void )                             
/************************************************************************
 * pgcycle_createPanel1                                                 *
 *                                                                      *
 * This function creates the day and cycle selection menus and sets the *
 * initial (default) values for the _curDay and _curCycle.     		*
 *                                                                      *
 * void pgcycle_createPanel1( void )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			none						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 * S. Guan/NCEP		07/20	Added tzone as an input of TI_DST       *  
 ***********************************************************************/
{
    int         tmType, ier, tmArray[ 5 ], isDst, strLen;
    int         nearest, minDiff, intCycle, minCycle, minCycleIndex;
    long        ii;
    char        **cycles;
    char        dayStr[ 3 ], dattim[ 20 ], tzone[2];

    time_t      tt;
    struct tm   *tStruct;

    XmString    labelStr, nrml, other;
    Widget	issueForm;
/*---------------------------------------------------------------------*/

    _form1 = XtVaCreateManagedWidget( "form1", 
     		xmFormWidgetClass, 		_cycleForm, 
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    /*
     *  Create 'Day' pulldown menu
     */
    _dayMenu   = XmCreateOptionMenu   ( _form1, "day", NULL, 0);
    _dayPdMenu = XmCreatePulldownMenu ( _form1, "daypd", NULL, 0);
    _dayButtons = (WidgetList)XtMalloc ( NUM_DAYS * sizeof( Widget ) );

    for ( ii = 0; ii < NUM_DAYS; ii++ ) {

        sprintf ( dayStr, "%02d", (int)ii + 1 );
	_dayButtons[ ii ] = XtVaCreateManagedWidget ( dayStr,
	        xmPushButtonWidgetClass,    	_dayPdMenu,
                NULL );
    }



    labelStr = XmStringCreateLocalized ( "Day:" );
    tt = time ( NULL );
    tStruct = localtime ( &tt );

    XtVaSetValues ( _dayMenu,
                XmNsubMenuId,           _dayPdMenu,
                XmNmenuHistory,         _dayButtons[ tStruct->tm_mday - 1 ],
                XmNlabelString,         labelStr,
                XmNspacing,             10,
                XmNx,                   0,
                XmNy,                   30,
                NULL );

    XtManageChild ( _dayMenu );

    XmStringFree ( labelStr );

    /*
     *  Set the default day
     */
    sprintf( _curDay, "%d", tStruct->tm_mday );


    /*
     *  Read the cycle times from the airmet table
     */
    tmType = 1;
    css_gtim ( &tmType, dattim, &ier );
    ti_ctoi ( dattim, tmArray, &ier, strlen ( dattim ) );
    /* Central Time zone assumed for this code specific to AWC. */
    tzone[0] = 'C';
    tzone[1] = '\0';
    ti_dst ( tmArray, tzone, &isDst, &ier );
    /*
     *   Use local time
     */
    tmType = 0;
    css_gtim ( &tmType, dattim, &ier );
    ti_ctoi ( dattim, tmArray, &ier, strlen ( dattim ) );
    ctb_airmetGetCycleTms( (isDst != 0), &_nCycles, &cycles, &ier );

    if ( ier < 0 ) {

        _nCycles = 1;
        strLen = 3;
        G_MALLOC ( cycles, char *, _nCycles, "airmet cycle menu" );
        G_MALLOC ( cycles[ 0 ], char, strLen, "airmet cycle menu" );
        strcpy ( cycles[ 0 ], "03" );

    }

    /*
     *  Find the nearest next cycle time
     */
    nearest = -1;
    minDiff = 24;
    minCycle = atoi ( cycles[ 0 ] );
    minCycleIndex = 0;

    for ( ii = 0; ii < _nCycles; ii++ ) {

        intCycle = atoi ( cycles[ ii ] );

	if ( intCycle < minCycle ) {
	    minCycle = intCycle;
	    minCycleIndex = ii;
	}

	if ( ( tmArray[ 3 ] ) < intCycle &&
	     ( intCycle - tmArray[ 3 ] ) <= minDiff ) {
	    minDiff = intCycle - tmArray[ 3 ];
	    nearest = ii;
	}
    }

    if ( nearest == -1 ) { 
	nearest = minCycleIndex;
    }

    /*
     *  Set the default _cycle
     */
    strcpy( _curCycle, cycles[ nearest ] );

    /*
     *  Create Cycle pulldown menu
     */
    _cycleMenu   = XmCreateOptionMenu   ( _form1, "cycle", NULL, 0);
    _cyclePdMenu = XmCreatePulldownMenu ( _form1, "cyclepd", NULL, 0);
    _cycleButtons = (WidgetList)XtMalloc ( _nCycles * sizeof( Widget ) );

    for ( ii = 0; ii < _nCycles; ii++ ) {

        _cycleButtons[ ii ] = XtVaCreateManagedWidget ( cycles[ ii ],
		xmPushButtonWidgetClass,    	_cyclePdMenu,
	   	NULL );
	G_FREE ( cycles[ ii ], char );

    }

    G_FREE ( cycles, char* );
  

    labelStr = XmStringCreateLocalized ( "Cycle:" );
    XtVaSetValues ( _cycleMenu,
                XmNsubMenuId,           	_cyclePdMenu,
                XmNmenuHistory,         	_cycleButtons[ nearest ],
                XmNlabelString,         	labelStr,
                XmNspacing,             	10,
  	        XmNx,                   	150,
	        XmNy,                   	30, 
	        NULL );

    XtManageChild ( _cycleMenu );


    issueForm = XtVaCreateManagedWidget ( "issueForm", 
    		xmFormWidgetClass,		_form1,
    		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_cycleMenu,
                NULL );


    nrml = XmStringCreateLtoR ( "Routine", XmFONTLIST_DEFAULT_TAG );
    other = XmStringCreateLtoR ( "Update", XmFONTLIST_DEFAULT_TAG );

    _issueRadioBox = XmVaCreateSimpleRadioBox ( issueForm, "issuance", 0,
			 NULL,
		         XmVaRADIOBUTTON, nrml, NULL, NULL,NULL,
		         XmVaRADIOBUTTON, other, NULL, NULL,NULL,
		         XmNorientation, XmVERTICAL,
		         XmNx,   25,
		         XmNy,   15,
		         NULL );
   
    XmStringFree( nrml );
    XmStringFree( other ); 
    XtManageChild( _issueRadioBox );


    XmStringFree ( labelStr );

}

/*=====================================================================*/

static void pgcycle_createPanel2 ( void )                             
/************************************************************************
 * pgcycle_createPanel2                                                 *
 *                                                                      *
 * This function creates the control buttons for the cycle GUI.         *
 *                                                                      *
 * void pgcycle_createPanel2( void )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			none						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding                     	*
 ***********************************************************************/
{
    long        ii, nn;
    char        *ctlstrs[] = { "Set", "Cancel" };

    Widget      ctrlForm, frame;

/*---------------------------------------------------------------------*/

    /*
     *  Control buttons
     */
    nn = XtNumber ( ctlstrs );

    frame = XtVaCreateManagedWidget( "frame", 
    		xmFrameWidgetClass,		_cycleForm,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_form1,
		XmNtopOffset,			10,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    ctrlForm = XtVaCreateManagedWidget ( "airmet_cntl_form",
		xmFormWidgetClass,		frame,
                XmNfractionBase,                nn * 50,
                NULL );

    _ctrlButtons = (WidgetList)XtMalloc ( (size_t) nn * sizeof(Widget) );

    for ( ii = 0; ii < nn; ii++ ) {

        _ctrlButtons[ ii ] = XtVaCreateManagedWidget ( ctlstrs[ii],
  	        xmPushButtonWidgetClass,        ctrlForm, 
	        XmNleftAttachment,              XmATTACH_POSITION,
	        XmNleftPosition,                ((ii * 50) + 10 ),
	        XmNrightAttachment,             XmATTACH_POSITION,
	        XmNrightPosition,               (((ii + 1) * 50) - 10),
	        NULL );

        XtAddCallback ( _ctrlButtons[ ii ], XmNactivateCallback,
                (XtCallbackProc)pgcycle_ctlBtnCb, (XtPointer) ii );

    }



}

/*=====================================================================*/

/* ARGSUSED */
static void pgcycle_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgcycle_ctlBtnCb                                                     *
 *                                                                      *
 * Callback function for the control buttons.                           *
 *                                                                      *
 * static void pgairmet_ctlBtnCb ( wid, which, call )                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      which           long            which button                    *
 *      call            XtPointer       callback struct                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      07/07   initial coding                          *
 * L. Hinson/AWC        06/08   Add call to pgcycle_updateCycleTags to  *
 *                              update the UTC times, if displayed      *
 ***********************************************************************/
{
    char        *tmpStr, day[ 3 ], cycle[ 3 ];
    int		num_buttons;

    Boolean	btn0 = False;
    XmString    xmStr;
    Widget      wPushButton;
    WidgetList	btns;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:           /* Set new day/cycle and close */

	  /* 
	   *  Get cycle time
           */
	  XtVaGetValues ( _cyclePdMenu, XmNmenuHistory, &wPushButton, NULL );
	  XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	  XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	  strncpy ( cycle, tmpStr, 2 );
	  cycle[ 2 ] = '\0';

	  XtFree ( tmpStr );
	  XmStringFree( xmStr );

	  /*
	   * Get Issuance Day
	   */
	  XtVaGetValues (_dayMenu, XmNmenuHistory, &wPushButton, NULL);
	  XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	  XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
	  strncpy (day, tmpStr,2);
	  day[2] = '\0';
	  XtFree ( tmpStr );
	  XmStringFree( xmStr );

	  strcpy( _curCycle, cycle );
	  strcpy( _curDay, day );

	  /*
	   *  Get Issuance Type
	   */
          XtVaGetValues( _issueRadioBox, XmNnumChildren, &num_buttons, 
			XmNchildren, &btns, NULL ); 
 	  btn0 = XmToggleButtonGadgetGetState( btns[ 0 ] );
          if( btn0 ) {
	      _issuance = 0;
	  }
	  else {
	      _issuance = 1;
	  }

          pgcycle_putCycleInTitle();
          
          /* Update Cycle Tags/Refresh Screen*/
          pgcycle_updateCycleTags();

          pgcycle_popdown ();
          break;

      case 1:           /* Cancel */

          pgcycle_popdown ();
          break;


      default:

	   break;
    }

}

void pgcycle_updateCycleTags( void )
/***********************************************************************
 pgcycle_updateCycleTags
 
 This routine updates the NMAP display and associated VGF work file to
 update the Text Labels when the Cycle Time is Changed.
 This routine is called by the pgcycle_ctlBtnCb() function
 
 Input parameters:
 Output parameters:
 Return parameters:
 NONE
**
Log:
L. Hinson/AWC           06/08 Created
                        05/09 Fix Cycle Change Issue:: drop call to 
                              pgutls_redraw. Add pglayer_setChngMade
                        09/09 Fix Refresh Issue on Cycle Change.
                              Add back call to pgutls_redraw.
                        09/09 Add call to pggfawp_resetFcstHrOptMenu
                              to update the Forecast Hour Pulldown Menu.
***********************************************************************/
{
  int			offsets[ MAX_EDITABLE_ELEMS ], noffsets = 0;
  int curLayer, i, npts;
  VG_DBStruct el;
  int loc = 0, ier = 0;
  float	llx, lly, urx, ury;
  
  curLayer = pglayer_getCurLayer();
  crg_goffsets( CLASS_MET, GFA_ELM, curLayer, offsets, &noffsets );
  for (i = 0; i < noffsets; i++) {
    pgutls_prepNew ( offsets[i], &el, &llx, &lly, &urx, &ury, &ier );
    cvg_setFld ( &el, TAG_GFA_CYCLE, _curCycle, &ier);
    npts = el.elem.gfa.info.npts;
    pgvgf_saveNewElm ( NULL, sys_M, &el, npts, 
	                       &el.elem.gfa.latlon[0],
			       &el.elem.gfa.latlon[npts], TRUE, &loc, &ier ); 
/*
 *  free the blocks (pgutls_prepNew calls cvg_rdrec).
 */                               
    cvg_freeElPtr ( &el );
    pgvgf_dsp ( loc, &ier );
    pgutls_redraw (loc, &el, &ier);
/*
 *  free the blocks (pgutls_redraw calls cvg_rdrec)..
 */
    cvg_freeElPtr ( &el );                             
  }
  pgpalw_refresh();        	    
  pglayer_setChngMade(curLayer, TRUE);
  /* Now Update the GFA Create/Edit Menu Forecast Hours to reflect the
     current Cycle */
  pggfawp_resetFcstHrOptMenu(_curCycle);
}


/*=====================================================================*/
int pgcycle_getIssue ( void )
/************************************************************************
 * pgcycle_getIssue	                                        	*
 *                                                                      *
 * This function returns the set issuance.                         	*
 *									*
 * int pgcycle_getIssue ( void )                          		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	pgcycle_getIssue	int	set issuance			*
 *                                      0 - ROUTINE			*
 *                                      1 - UPDATE			*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/07	initial coding                     	*
 ***********************************************************************/
{
    return ( _issuance );   
}

/*=====================================================================*/

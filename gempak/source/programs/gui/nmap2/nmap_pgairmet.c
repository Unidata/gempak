#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "Nxm.h"

#define XSLT_DIR        "$GEMTBL/xslt/"

#define NUM_REGIONS	3
#define NUM_AREAS	2
#define NUM_TYPES	3
#define NUM_DAYS	31
#define COL_WRAP	65
#define STR_TYPE	"{type}"
#define STR_AREA	"{FA_Area}"
#define STR_CYCLE       "{CC}"
#define FILE_TEMP	"AIRMET_{type}_{FA_Area}_{CC}.txt"

typedef struct {
	char		fileName[ 128 ];
	unsigned char	*saveStr;
} SaveStr_t;

static	Widget	_airmetForm;
static	Widget	_editPane;
static	Widget	_msgTxt;
static	Widget	_dayMenu;
static	Widget	_dayPdMenu;
static	Widget	_cycleMenu;
static	Widget	_cyclePdMenu;

static	WidgetList	_areaChkBox[ 3 ];
static	WidgetList	_typeChkBox;
static	WidgetList	_statusChkBox;
static  WidgetList	_ctrlButtons;

static	int		_numFiles = 0;
static	SaveStr_t	_save[ NUM_REGIONS * NUM_AREAS * NUM_TYPES ];
static 	Boolean		_errTbl = FALSE;
static	int		_issuance = 0;

/*
 * private functions 
 */
static void pgairmet_createPanel1 ( void );
static void pgairmet_createPanel2 ( void );
static void pgairmet_createPanel3 ( void );

static void pgairmet_setTxt ( void );
static void pgairmet_format ( char *vgFile, int nAreas, char areas[][ 8 ], 
			      int nTypes, char *types[3], char *day, char *cycle, 
			      int flag, char **txt );
static void pgairmet_createAreaChkBox ( Widget parent, Widget *rowCol, 
					WidgetList *chkBox, char **labelStr, 
					int posX, int posY );
static char* pgairmet_replaceStr ( char *string, char *subStr, char *rplsStr );
static char* pgairmet_mkFileName ( char *nameTmp, char *type, char *area,
				   char *cycle, char *fileName );

static void pgairmet_statusChkBoxCb	( Widget, XtPointer, XtPointer );
static void pgairmet_ctlBtnCb		( Widget, long, XtPointer );
static void pgairmet_areaChkBoxCb	( Widget, long, XtPointer );
static void pgairmet_settingChangedCb	( Widget, XtPointer, XtPointer );
static void pgairmet_issueRadioBtnCb	( Widget, long, XtPointer );

/************************************************************************
 * nmap_pgairmet.c							*
 *									*
 * This module defines everything for Airmet formatter dialog.		*
 *									*
 * CONTENTS:								*
 *									*
 * public functions:							*
 *	pgairmet_create		-	create airmet formatter dialog	*
 *	pgairmet_isUp		-	check if the dialog is up	*
 *	pgairmet_popup 		-	popup airmet formatter dialog	*
 *	pgairmet_popdown 	-	popdown airmet formatter dialog	*
 *									*
 * private functions:							*	
 *	pgairmet_createAreaChkBox  -	create the area check boxs	*
 *	pgairmet_createPanel1 	-	create the top panel 		*
 *	pgairmet_createPanel2 	-	create the middle panel		*
 *	pgairmet_createPanel3	-	create the third panel		*
 *	pgairmet_format 	-	create text from gfa elements	* 
 *	pgairmet_mkFileName	-	make airmet bulletin file names	*
 *	pgairmet_replaceStr	-	replace string in template	*
 *	pgairmet_setTxt		-	create and set the text message	*
 *	pgairmet_areaChkBoxCb	-	callback for area check boxs	*
 *	pgairmet_ctlBtnCb 	   -	callback for control buttons	*
 *	pgairmet_statusChkBoxCb	   -	callback for status check box	*
 *	pgairmet_settingChangedCb  -	callback for any setting changes*
 *									*
 **									*
 * Log:									*
 * B .Yin/SAIC		11/04	Created					*
 ***********************************************************************/

void pgairmet_create ( Widget parent )
/************************************************************************
 * pgairmet_create							*
 *									*
 * This function creates the airmet formatter dialog.			*
 *									*
 * void pgairmet_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			None						*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/04	Created					*
 * E. Safford/SAIC	04/05	Increase dialog width			*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    /*
     *  Create main form dialog 
     */
    _airmetForm = XmCreateFormDialog ( parent, "airmet_format", NULL, 0 );

    xmstr = XmStringCreateLocalized ( "AIRMET Format" );

    XtVaSetValues ( _airmetForm,
		XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		XmNx,				70,
		XmNy,				100,
		NULL );

    XmStringFree ( xmstr );

    /*
     *  Create pane area and child forms.
     */
    _editPane = XtVaCreateManagedWidget ( "airmet_pane",
			xmPanedWindowWidgetClass, 	_airmetForm,
                        XmNmarginHeight,                10,
                        XmNmarginWidth,                 10,
                        XmNspacing,                     20,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
			NULL );

    pgairmet_createPanel1 ( );
    pgairmet_createPanel2 ( );
    pgairmet_createPanel3 ( );
    
}

/*=====================================================================*/

void pgairmet_popup ( void )
/************************************************************************
 * pgairmet_popup							*
 *									*
 * This function pops up the airmet formatter dialog.			*
 *									*
 * void pgairmet_popup ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/04	Created					*
 * B. Yin/SAIC		07/05	Check the error flag and display error	*
 * B. Yin/SAIC		10/05	Combine 'Format' and 'Save' buttons	*
 ***********************************************************************/
{
    int		ier, ier1;
/*---------------------------------------------------------------------*/
 
    pgairmet_popdown ();
        
    if ( !XtIsManaged ( _airmetForm ) ) {

       XtManageChild ( _airmetForm );

       XtVaSetValues ( _msgTxt, XmNvalue, "", NULL );
       XtVaSetValues ( _ctrlButtons[ 0 ],  XmNsensitive, TRUE, NULL );

    }
            
    if ( _errTbl ) {

       ier = -1;
       er_wmsg ( "CTB", &ier, "airmetcycle.tbl", &ier1, 3, 
       		 strlen ( "airmetcycle.tbl" ) );
       NxmErr_update();

    }

}

/*=====================================================================*/

void pgairmet_popdown ( void )
/************************************************************************
 * pggfaw_popdown							*
 *									*
 * This function unmanages the airmet formatter dialog.			*
 *									*
 * void pggfaw_popdown (  )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/04   Created                                 *
 * H. Zeng/SAIC		10/05	added call to pgpalw_refresh()		*
 ************************************************************************/
{

   if ( XtIsManaged ( _airmetForm ) ) {

	XtUnmanageChild ( _airmetForm );
        pgpalw_refresh ();

    }
    
}

/*=====================================================================*/

Boolean pgairmet_isUp ( void )
/************************************************************************
 * pgairmet_isUp                                                        *
 *                                                                      *
 * This function returns the current status of the airmet formatter     *
 * window.                                                              *
 *                                                                      *
 * Boolean pgairmet_isUp ( )                                            *
 *                                                                      *
 * Input Parameters:                                                    *
 * Output Parameters:                                                   *
 *              None                                                    *
 *                                                                      *
 * Return Parameters:                                                   *
 *              Boolean         True if airmet formatter is active      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created                                 *
 ***********************************************************************/
{

    return ( XtIsManaged ( _airmetForm ) );

}

/*=====================================================================*/

static void pgairmet_createPanel1 ( void )
/************************************************************************
 * pgairmet_createPanel1                                                *
 *                                                                      *
 * This routine creates the top panel of the airmet formatter dialog.   *
 *                                                                      *
 * void pgairmet_createPanel1 ( )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	11/04   Created                                 *
 * E. Safford/SAIC	05/05	free dayButtons, cycleButtons		*
 * B. Yin/SAIC		07/05	read the cycle times from the airmet tbl*
 * L. Hinson/AWC        09/05   Format the day to a 2-digit figure for  *
 *                                days 1-9                              *
 * T. Piper/SAIC	10/05	change ii to long			*
 * B. Yin/SAIC		10/05	make initial cycle based on current time*
 * S. Guan/NCEP		07/20	Added tzone as an input of TI_DST	*  
 ***********************************************************************/
{
    int 	tmType, ier, tmArray[ 5 ], isDst, nCycles, strLen;
    int		nearest, minDiff, intCycle, minCycle, minCycleIndex;
    long	ii;
    char	statusStr[ 2 ][ 5 ] ={ "NRML", "TEST" };
    char	**cycles;
    char        dayStr[ 3 ], dattim[ 20 ], tzone[2];

    time_t	tt;
    struct tm	*tStruct;

    XmString	labelStr;
    Widget      form1, statusRowCol;
    WidgetList	dayButtons, cycleButtons;
/*---------------------------------------------------------------------*/

    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, _editPane, NULL );

    /*
     *  Create 'Day' pulldown menu
     */
    _dayMenu   = XmCreateOptionMenu   ( form1, "day", NULL, 0);
    _dayPdMenu = XmCreatePulldownMenu ( form1, "daypd", NULL, 0);
    dayButtons = (WidgetList)XtMalloc ( NUM_DAYS * sizeof( Widget ) );

    for ( ii = 0; ii < NUM_DAYS; ii++ ) {

	sprintf ( dayStr, "%02d", (int)ii + 1 );
        dayButtons[ ii ] = XtVaCreateManagedWidget ( dayStr,
				xmPushButtonWidgetClass,    _dayPdMenu,
				NULL );

    }

    XtAddCallback( _dayPdMenu, XmNentryCallback,
                   (XtCallbackProc)pgairmet_settingChangedCb, NULL );

    labelStr = XmStringCreateLocalized ( "Day:" );

    tt = time ( NULL );
    tStruct = localtime ( &tt );

    XtVaSetValues ( _dayMenu,
                XmNsubMenuId,           _dayPdMenu,
                XmNmenuHistory,         dayButtons[ tStruct->tm_mday - 1 ],
                XmNlabelString,         labelStr,
		XmNspacing,		10,
                XmNx,                   0,
                XmNy,                   0,
                NULL );

    XtManageChild ( _dayMenu );

    XmStringFree ( labelStr );

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
    ctb_airmetGetCycleTms( (isDst != 0), &nCycles, &cycles, &ier );

    if ( ier < 0 ) {
       
       _errTbl = TRUE;

       nCycles = 1;
       strLen = 3;
       G_MALLOC ( cycles, char *, nCycles, "airmet cycle menu" );
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

    for ( ii = 0; ii < nCycles; ii++ ) {

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

    if ( nearest == -1 ) nearest = minCycleIndex;

    /*
     *  Create Cycle pulldown menu
     */
    _cycleMenu   = XmCreateOptionMenu   ( form1, "cycle", NULL, 0);
    _cyclePdMenu = XmCreatePulldownMenu ( form1, "cyclepd", NULL, 0);
    cycleButtons = (WidgetList)XtMalloc ( nCycles * sizeof( Widget ) );

    for ( ii = 0; ii < nCycles; ii++ ) {

        cycleButtons[ ii ] = XtVaCreateManagedWidget ( cycles[ ii ],
				xmPushButtonWidgetClass,    _cyclePdMenu,
				NULL );
	G_FREE ( cycles[ ii ], char );

    }

    G_FREE ( cycles, char* );

    XtAddCallback( _cyclePdMenu, XmNentryCallback,
                   (XtCallbackProc)pgairmet_settingChangedCb, NULL );

    labelStr = XmStringCreateLocalized ( "Cycle:" );
    XtVaSetValues ( _cycleMenu,
                XmNsubMenuId,           _cyclePdMenu,
                XmNmenuHistory,         cycleButtons[ nearest ],
                XmNlabelString,         labelStr,
		XmNspacing,		10,
                XmNx,                   150,
                XmNy,                   0,
                NULL );

    XtManageChild ( _cycleMenu );

    XmStringFree ( labelStr );

    /*
     *  Create status check box
     */
    statusRowCol = XtVaCreateWidget ( "status",
			     xmRowColumnWidgetClass,	form1,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_COLUMN,
			     XmNnumColumns,		1,
			     XmNradioBehavior,		False,
			     XmNx,			310,
			     XmNy, 			0,
			     NULL );

    _statusChkBox = (WidgetList)XtMalloc ( (size_t) 2 * sizeof(Widget) );

    for (ii = 0; ii < 2; ii++) {

	_statusChkBox[ ii ] = XtVaCreateManagedWidget ( statusStr[ ii ],
				 xmToggleButtonWidgetClass, statusRowCol,
				 NULL );

	XtAddCallback ( _statusChkBox[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmet_statusChkBoxCb,
		        (XtPointer)ii );
	XtAddCallback ( _statusChkBox[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmet_settingChangedCb,
		        (XtPointer)NULL );

	if ( ii == 0 ) { 

	    XmToggleButtonSetState ( _statusChkBox[ ii ], True, True );

	}

    }

    XtManageChild ( statusRowCol );

    XtFree( (XtPointer) dayButtons );
    XtFree( (XtPointer) cycleButtons );
}

/*=====================================================================*/

static void pgairmet_createPanel2 ( void )
/************************************************************************
 * pgairmet_createPanel2                                                *
 *                                                                      *
 * This routine creates the middle panel of the airmet formatter dialog.*
 *                                                                      *
 * void pgairmet_createPanel2 ( )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	11/04   Created                                 *
 * B. Yin/SAIC  	03/07   Added the issuance radio buttons        *
 * B. Yin/SAIC          04/07   Changed issuance radio button labels    *
 ***********************************************************************/
{
    int		ii;
    char	*westStr[]    = { "WEST", "SLC", "SFO" };
    char	*centralStr[] = { "CENTRAL", "CHI", "DFW" };
    char	*eastStr[]    = { "EAST", "BOS", "MIA" };
    char	*typeStr[]    = { "SIERRA", "TANGO", "ZULU" };
    
    XmString	nrml, other;
    Widget	form2, form3, westRowCol, centralRowCol, eastRowCol;
    Widget	typeRowCol, pane, areaForm, issueForm, issueRadioBtn;
/*---------------------------------------------------------------------*/

    form2 = XtVaCreateManagedWidget ( "form2", xmFormWidgetClass, 
    				      _editPane, NULL );

    pane = XtVaCreateManagedWidget ( "2ndpane",
			xmPanedWindowWidgetClass, 	form2,
                        XmNmarginHeight,                10,
                        XmNmarginWidth,                 10,
                        XmNspacing,                     20,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
                        XmNorientation,       XmHORIZONTAL,
			NULL );

    areaForm = XtVaCreateManagedWidget ( "areaForm", xmFormWidgetClass, 
    				      pane, NULL );

    /*
     *  Create FA area check boxs
     */
    pgairmet_createAreaChkBox ( areaForm, &westRowCol, &_areaChkBox[ 0 ],
				westStr, 40, 0 );
    pgairmet_createAreaChkBox ( areaForm, &centralRowCol, &_areaChkBox[ 1 ],
				centralStr, 190, 0 );
    pgairmet_createAreaChkBox ( areaForm, &eastRowCol, &_areaChkBox[ 2 ],
				eastStr, 340, 0 );
				
    issueForm = XtVaCreateManagedWidget ( "issueForm", xmFormWidgetClass, 
    				      pane, NULL );

    nrml = XmStringCreateLtoR ( "Routine", XmFONTLIST_DEFAULT_TAG );
    other = XmStringCreateLtoR ( "Update", XmFONTLIST_DEFAULT_TAG );

    issueRadioBtn = XmVaCreateSimpleRadioBox ( issueForm, "issuance", 0, 
    				(XtCallbackProc)pgairmet_issueRadioBtnCb,
    				XmVaRADIOBUTTON, nrml, NULL, NULL,NULL,
    				XmVaRADIOBUTTON, other, NULL, NULL,NULL,
				XmNorientation, XmVERTICAL,
				XmNx,	25,
				XmNy,	15,
				NULL );
    					
    XmStringFree( nrml );
    XmStringFree( other );
    XtManageChild( issueRadioBtn );
    
    form3 = XtVaCreateManagedWidget ( "form3", xmFormWidgetClass, 
    				      _editPane, NULL );

    typeRowCol = XtVaCreateManagedWidget ( "type_check_box",
			     xmRowColumnWidgetClass,	form3,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_NONE,
			     XmNnumColumns,		1,
			     XmNradioBehavior,		False,
			     XmNx,			0,
			     XmNy, 			0,
			     NULL );

    _typeChkBox = (WidgetList)XtMalloc ( (size_t) NUM_TYPES * sizeof(Widget) );

    for ( ii = 0; ii < NUM_TYPES; ii++ ) {

	_typeChkBox[ ii ] = XtVaCreateManagedWidget ( typeStr[ ii ],
				 xmToggleButtonWidgetClass, 	typeRowCol,
			         XmNx,				ii*150 + 52,
			         XmNy, 				0,
				 NULL );

	XtAddCallback ( _typeChkBox[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmet_settingChangedCb,
		        (XtPointer)NULL );
    }

}

/*=====================================================================*/

static void pgairmet_createPanel3 ( void )
/************************************************************************
 * pgairmet_createPanel3                                                *
 *                                                                      *
 * This routine creates the bottom panel of the airmet formatter dialog.*
 *                                                                      *
 * void pgairmet_createPanel3 ( )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	11/04   Created                                 *
 * E. Safford/SAIC	04/05   Increase width of _msgTxt		*
 * T. Piper/SAIC	10/05	Change ii and nn to long		*
 * B. Yin/SAIC		10/05	Combine 'Format' and 'Save' buttons	*
 * E. Safford/SAIC	03/06	make _mgsTxt courier font		*
 ***********************************************************************/
{
    long		ii, nn;
    char	*ctlstrs[] = { "Format/Save", "Cancel" };
       
    Arg         args[ 10 ];
    Widget	ctrlForm, txtForm;

    char        	fontname[] = 
    			  "-adobe-courier-*-r-normal-*-*-140-*-*-m-*-*-*";
    XmFontListEntry 	flentry;
    XmFontList  	fontlist;
    Display		*dsp;
/*---------------------------------------------------------------------*/

    /*
     * Text window
     */
    txtForm = XtVaCreateManagedWidget ( "airmet_txt_form",
			xmFormWidgetClass,	_editPane,
			NULL );

    dsp = XtDisplay( txtForm );
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

    nn = 0;
    XtSetArg(args[nn], XmNscrollHorizontal,          False 	 ); nn++;
    XtSetArg(args[nn], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); nn++;
    XtSetArg(args[nn], XmNcursorPositionVisible,     False       ); nn++;
    XtSetArg(args[nn], XmNeditable,                  False       ); nn++;
    XtSetArg(args[nn], XmNcolumns,                   70          ); nn++;
    XtSetArg(args[nn], XmNrows,                      15          ); nn++;
    XtSetArg(args[nn], XmNwordWrap,                  True        ); nn++;
    XtSetArg(args[nn], XmNeditMode,                  XmMULTI_LINE_EDIT ); nn++;
    XtSetArg(args[nn], XmNfontList, 		     fontlist    ); nn++;

    _msgTxt = XmCreateScrolledText ( txtForm, "txt_msg", args, nn );
    XmFontListFree( fontlist ); 

    XtManageChild ( _msgTxt );

    /*
     *  Control buttons
     */
    nn = XtNumber ( ctlstrs );
    ctrlForm = XtVaCreateManagedWidget ( "airmet_cntl_form",
		xmFormWidgetClass,		_editPane,
                XmNfractionBase,                nn * 50,
		NULL );

    _ctrlButtons = (WidgetList)XtMalloc ( (size_t) nn * sizeof(Widget) );

    for ( ii = 0; ii < nn; ii++ ) {

	_ctrlButtons[ ii ] = XtVaCreateManagedWidget ( ctlstrs[ii], 
		xmPushButtonWidgetClass,	ctrlForm, 
             	XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                ((ii * 50) + 10 ),
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,		(((ii + 1) * 50) - 10),
		NULL );

	XtAddCallback ( _ctrlButtons[ ii ], XmNactivateCallback,
		(XtCallbackProc)pgairmet_ctlBtnCb, (XtPointer) ii );

    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgairmet_statusChkBoxCb ( Widget wid, XtPointer clnt,
							XtPointer call )
/************************************************************************
 * pgairmet_statusChkBoxCb                                              *
 *                                                                      *
 * Callback function for status check box. 		   		*
 *                                                                      *
 * void pgairmet_statusChkBoxCb ( wid, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     	Widget          widget ID                       *
 *      clnt   		XtPointer	client data			*
 *      call    	XtPointer       callback data                   *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC        11/04   Created			                *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/

    for (ii = 0; ii < 2; ii++ ) {

        XmToggleButtonSetState ( _statusChkBox[ ii ], FALSE, FALSE );

    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgairmet_areaChkBoxCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgairmet_areaChkBoxCb                                                *
 *                                                                      *
 * Callback function for area check boxs. 		                *
 *                                                                      *
 * void pgairmet_areaChkBoxCb ( wid, which, call )                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     	Widget          widget ID                       *
 *      which   	long		which button                    *
 *      call    	XtPointer       callback structure              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           11/04   Created		                *
 ***********************************************************************/
{
    int ii;

    XmToggleButtonCallbackStruct 	*cbData;
    WidgetList				*chkBox;
/*---------------------------------------------------------------------*/

    cbData = (XmToggleButtonCallbackStruct *) call;
 
/*  
 *  Find out which region is clicked
 */
    for ( ii = 0; ii < NUM_AREAS + 1; ii++ ) {

        if ( wid == _areaChkBox[ ii ][ which ] ) {

           chkBox = &_areaChkBox[ ii ];

	}
    }
		   
    if ( which == 0 ) {
       
       XmToggleButtonSetState( (*chkBox)[ 1 ], !cbData->set, !cbData->set );
       XmToggleButtonSetState( (*chkBox)[ 2 ], !cbData->set, !cbData->set );

    }
    else if ( which == 1 ) {
      
       if ( !cbData->set && XmToggleButtonGetState ( (*chkBox)[ 2 ] ) ) {
	  
	  XmToggleButtonSetState( (*chkBox)[ 0 ], True, True );

       }
       else {

	  XmToggleButtonSetState( (*chkBox)[ 0 ], False, False );

       }
    }
    else if ( which == 2 ) {
      
       if ( !cbData->set && XmToggleButtonGetState ( (*chkBox)[ 1 ] ) ) {
	  
	  XmToggleButtonSetState( (*chkBox)[ 0 ], True, True );

       }
       else {

	  XmToggleButtonSetState( (*chkBox)[ 0 ], False, False );

       }
   }
}

/*=====================================================================*/

static void pgairmet_createAreaChkBox ( Widget parent, Widget *rowCol, 
					WidgetList *chkBox, char **labelStr, 
					int posX, int posY )
/************************************************************************
 * pgairmet_createAreaChkBox                                            *
 *                                                                      *
 * This routine creates the area check boxs of the airmet dialog.       *
 *                                                                      *
 * void pgairmet_createAreaChkBox ( parent, rowCol, chkBox, 		*
 * 				    labelStr, posX, posY )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	parent		Widget		parent Widget			*
 *	*rowCol		Widget		RowCol Widget to hold the boxs	*
 *	**labelStr	char		array of label strings		*
 *	posX		int		x position			*
 *	posY		int		y position			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*chkBox		WidgetList	area check boxs			*
 *                                                                      *
 * Return parameters:                                                   *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	11/04   Created                                 *
 * T. Piper/SAIC	10/05	changed ii to long			*
 ***********************************************************************/
{
    long ii;
/*---------------------------------------------------------------------*/

    *rowCol = XtVaCreateManagedWidget ( "area_check_box",
			     xmRowColumnWidgetClass,	parent,
			     XmNorientation,		XmVERTICAL,
			     XmNpacking,		XmPACK_COLUMN,
			     XmNnumColumns,		1,
			     XmNradioBehavior,		False,
			     XmNx,			posX,
			     XmNy, 			posY,
			     NULL );

    *chkBox = (WidgetList)XtMalloc( (size_t)( NUM_AREAS + 1 ) * sizeof(Widget) );

    for ( ii = 0; ii < NUM_AREAS + 1; ii++ ) {

	(*chkBox)[ ii ] = XtVaCreateManagedWidget( labelStr[ ii ],
				 xmToggleButtonWidgetClass, *rowCol,
				 NULL );

	XtAddCallback ( (*chkBox)[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmet_areaChkBoxCb,
		        (XtPointer)ii );
	XtAddCallback ( (*chkBox)[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmet_settingChangedCb,
		        (XtPointer)NULL );

    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgairmet_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgairmet_ctlBtnCb                                                    *
 *                                                                      *
 * Callback function for the control buttons.                           *
 *                                                                      *
 * static void pgairmet_ctlBtnCb ( wid, which, call )                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      which           long		which button                    *
 *      call            XtPointer       callback struct                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created				        *
 * B. Yin/SAIC		10/05	Combine 'Format' and 'Save' buttons	*
 * H. Zeng/SAIC		10/05	added call to pgpalw_refresh()		*
 ***********************************************************************/
{
    int 	ii;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:           /* Format and Save */

/*
 * Refresh the screen before calling pgairmet_setTxt(),
 * and then call pgairmet_setTxt().
 */
        pgpalw_refresh ();
	pgairmet_setTxt();

        for ( ii = 0; ii < _numFiles; ii++ ) {

	    if ( !(fp = fopen ( _save[ ii ].fileName, "w" )) ) continue;

	    fprintf ( fp, "%s", _save[ ii ].saveStr );
	    G_FREE ( _save[ ii ].saveStr, unsigned char );
	    fclose ( fp );

	}

	_numFiles = 0;

        XtVaSetValues ( _ctrlButtons[ 0 ],  XmNsensitive, FALSE, NULL );

	break;

      case 1:           /* Cancel */

        for ( ii = 0; ii < _numFiles; ii++ ) {
	    G_FREE ( _save[ ii ].saveStr, unsigned char );
	}

	_numFiles = 0;

	pgairmet_popdown ();

	break;

      default:

	break;
   }
}

/*=====================================================================*/

static void pgairmet_setTxt ( void )
/************************************************************************
 * pgairmet_setTxt                                                      *
 *                                                                      *
 * This routine sets the text message in the scrolled text window.      *
 *                                                                      *
 * static void pgairmet_setTxt ( ) 			                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created				        *
 * J. Wu/SAIC           07/05   change declaration for "types"	        *
 * L. Hinson            09/05   Propogate day thru call to              *
 *                              pgairmet_format                         *
 ***********************************************************************/
{
    int 	ii, jj, flag, nAreas, nTypes;
    char	areas[ NUM_REGIONS * NUM_AREAS ][ 8 ], *types[ NUM_TYPES ];
    char	*tmpStr, day[ 3 ], cycle[ 3 ];

    char	*txt = NULL;

    XmString	xmStr;
    Widget	wPushButton;
/*---------------------------------------------------------------------*/

/*
 *  Get FA areas
 */
    nAreas = 0;

    for ( ii = 0; ii < NUM_REGIONS; ii++ ) {
	for ( jj = 1; jj < NUM_AREAS + 1; jj++ ) {

	    if ( XmToggleButtonGetState( _areaChkBox[ ii ][ jj ] ) ) {

    		XtVaGetValues ( _areaChkBox[ ii ][ jj ], XmNlabelString, &xmStr, NULL );
    		XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

		strcpy ( areas[ nAreas++ ], tmpStr );

		XtFree ( tmpStr );
		XmStringFree ( xmStr );

	    }
	}
    }

/* 
 *  Get airmet type (Serria/Tango/Zulu)
 */
    nTypes = 0;

    for ( ii = 0; ii < NUM_TYPES; ii++ ) {

	if ( XmToggleButtonGetState( _typeChkBox[ ii ] ) ) {

	    XtVaGetValues ( _typeChkBox[ ii ], XmNlabelString, &xmStr, NULL );
	    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	    G_MALLOC ( types[ nTypes ], char, (int)strlen(tmpStr)+1, "" );
	    strcpy ( types[ nTypes++ ], tmpStr );

	    XtFree ( tmpStr );
	    XmStringFree ( xmStr );

	}
    }

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

/* 
 *  Get normal/test flag
 */
    flag = 0;
    if ( XmToggleButtonGetState ( _statusChkBox[ 0 ] ) ) {

       flag = 1;

    }

/*
 *  Format
 */
    pgairmet_format ( cvg_getworkfile(), nAreas, areas, nTypes,
  		      types, day, cycle, flag, &txt );
    
    if ( txt ) {

	XtVaSetValues ( _msgTxt, XmNvalue, txt, NULL );
	G_FREE ( txt, char );

    }
    else {

	XtVaSetValues ( _msgTxt, XmNvalue, "", NULL );

    }
}

/*=====================================================================*/

static void pgairmet_format ( char *vgFile, int nAreas, char areas[][8], 
			      int nTypes, char *types[3], char *day, 
                              char *cycle, int flag, char **txt )
/************************************************************************
 * pgairmet_format                                                      *
 *                                                                      *
 * This routine creates the text message from GFAs in the VG file. 	*
 *                                                                      *
 * static void pgairmet_format ( vgFile, nAreas, areas, nTypes, 	*
 *				 types, day, cycle, flag, txt )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgFile		char	VG file name				*
 *	nAreas		int	number of FA areas			*
 *	areas[][8]	char	array of area strings			*
 *	nTypes		int	number of hazard types Sierra/Tango/...	*
 *	types[][8]	char	array of hazard type strings		*
 *      *day            char    day string                              *
 *	*cycle		char	cycle string				*
 *	flag		int	normal or test (0/1)			*
 *                                                                      *
 * Output parameters:                                                   *
 *	**txt		char	text message string			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created				        *
 * E. Safford/SAIC	03/05	allow a 0 smear element report		*
 * J. Wu/SAIC		07/05	overhaul GFA bound clipping algorithm	*
 * E. Safford/SAIC	07/05	do 2 stage format, add transform err ck *
 * L. Hinson/AWC        09/05   Added day argument and propogate thru   *
 *                              call to af_create                       *
 * L. Hinson/AWC        09/05   Add cycle arg. to pgairmet_mkFileName   *
 * H. Zeng/SAIC		09/05	added call to af_displayPts		*
 * H. Zeng/SAIC		10/05	added display_flg			*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	add indentation when wrapping FZLVL text*
 * J. Wu/SAIC		04/06	fix indentation for non-FZLVL text	*
 * D.W.Plummer/NCEP	11/06	Chg cst_wrap call to use mult seps	*
 * D.W.Plummer/NCEP     03/07   Add call to af_qualityControl           *
 * B. Yin/SAIC		03/07	Chg af_create calling sequence		*
 * B. Yin/SAIC          04/07   Use " |-" as separator for FZLVLs.      *
 * E. Safford/SAIC	05/07	fix memory leak                         *
 * J. Wu/SAIC		10/07	skip F/BB/A elements			*
 ***********************************************************************/
{
    int         ii, jj, kk, ier, nextEl, curPos, nIn, line, tmplen;
    int		errCode, ignore, display_flg, cat;
    char	*fmtStr[NUM_REGIONS*NUM_AREAS][NUM_TYPES];
    char        typeLC[ 32 ], xslFile[ 64 ], fileName[ 128 ];
    char	blank[2]={' '}, tag_val[4096];
    char	newLineStr[7]="      ";  /* 6 spaces */

    char		*tmpPtr, *tmpStr, *fzlPtr, *fzlStr;
    unsigned char	*outStr, *interumStr, *xmlStr;
    Boolean		xsltError = False;

    VG_DBStruct 	*elIn, el;

    char        info[64], value[32];
/*---------------------------------------------------------------------*/
/*
 *  Get the airmet file name template
 */
    ctb_gfagtemp ( fileName, &ier );
    if ( ier != 0 ) {
       strcpy ( fileName, FILE_TEMP );
    }

/*
 *  Get the total number of GFA elements in the vg file
 *  Allocate memory for them - do not count F/BB/As, though.
 */
    nIn     = 0;
    curPos  = 0;
    nextEl  = 0;
    ier     = 0;

    while ( nextEl < MAX_EDITABLE_ELEMS )  {

       cvg_rdrec ( vgFile, curPos, &el, &ier );

       if ( ier != 0 ) break;
     
       if ( el.hdr.recsz > 0 )  {
    
          curPos += el.hdr.recsz;

          if ( (int)el.hdr.vg_type == GFA_ELM && !el.hdr.delete )  {
              
	      cvg_getFld ( &el, TAG_GFA_SUBTYPE, value, &ier );
              cat = atoi( value ) - atoi( value )/10 * 10;

              if ( cat == GFA_SYSTEM_SMEAR || cat == GFA_SYSTEM_OUTLOOK ||
                   cat == GFA_USER_SMEAR   || cat == GFA_USER_OUTLOOK ||
		   cat == GFA_SNAPSHOT ) {
	          
		  nIn++;
              }
	  }
       }

       cvg_freeElPtr ( &el );
       nextEl++;

    }                                       /* element loop  */

    G_MALLOC ( elIn, VG_DBStruct, nIn, "pgairmet_format" );

    /*
     *  Read GFA elements - skip F/BB/As
     */
    curPos  = 0;
    nextEl  = 0;
    ier     = 0;
    ii	    = 0;

    while ( ( nextEl < MAX_EDITABLE_ELEMS ) && ( ii < nIn ) )  {

       cvg_rdrec ( vgFile, curPos, &elIn[ ii ], &ier );

       if ( ier != 0 ) break;

       if ( elIn[ ii ].hdr.recsz > 0 )  {

          curPos += elIn[ ii ].hdr.recsz;

          if ( (int)elIn[ ii ].hdr.vg_type == GFA_ELM &&
               !elIn[ ii ].hdr.delete )  {
	      
	      cvg_getFld ( &elIn[ ii ], TAG_GFA_SUBTYPE, value, &ier );
              cat = atoi( value ) - atoi( value )/10 * 10;

              if ( cat == GFA_SYSTEM_SMEAR || cat == GFA_SYSTEM_OUTLOOK ||
                   cat == GFA_USER_SMEAR   || cat == GFA_USER_OUTLOOK ||
		   cat == GFA_SNAPSHOT ) {
                  
		  ii++;
              }
	      else {
                  cvg_freeElPtr ( &elIn[ii] );	      
	      }
          }
	  else {
              cvg_freeElPtr ( &elIn[ii] );
          }
	      
       }

       nextEl++;

    }                                       /* element loop  */

    /*
     *  Generate the information string for requested areas/types
     */

    /*
     *  First do a QC check for GFAs w/ fewer than 3 points.
     */
    af_qualityControl ( nIn, elIn, sizeof(info), info, &ier );
                                                                                              
    if ( ier != 0 )  {
                                                                                              
        NxmWarn_show ( _airmetForm, info );
                                                                                              
    }
    else  {

      /*
       * First set computational projection.
       */
      ncw_set ();
      ncw_sproj ( "PREFS" );

      /*
       * Create the XML string.
       */
      af_create ( nAreas, areas, nTypes, types, day, cycle, 
	          nIn, elIn, _issuance, fmtStr, &ier );

      /*
       * Unset the computational projection back to the NMAP main canvas.
       */
      ncw_unset ();
    
      /*
       *  Loop over FA areas and airmet types to generate the bulletin
       */
      for ( ii = 0; ii < nAreas; ii++ ) {
         
        for ( jj = 0; jj < nTypes; jj++ ) {
	   
	    
            /*
             *  Generate the bulletin
             */
            xslFile[ 0 ] = '\0';
            cst_uclc ( types[ jj ], typeLC, &ier ),
            sprintf ( xslFile, "airmet_%s.xsl", typeLC ); 

	    ier = -1;

	    /*
             * Check the flag to decide whether to display the 
             * AIRMET FROM line on the screen.
             */
            ctb_pfstr ( "GFA_AF_DISPLAY", tag_val, &ier );
            if ( ier == 0 ) {
              if ( strcasecmp(tag_val,"FALSE") == 0 ) display_flg = 0;
              else display_flg = 1;
            }
            else {
              display_flg = 1;
            }

            if ( display_flg != 0 )  af_displayPts ( fmtStr[ii][jj] );

	    /*
	     *  first transform xml -> xml
	     */
            xml_transform( fmtStr[ii][jj], strlen ( fmtStr[ii][jj] ), xslFile,
                           &interumStr, &ier );

            if( ier >= 0 ) {
	       strcpy( xslFile, "indent.xsl" );

	       /*
	        *  second transform, xml -> text
		*/
               xml_transform( ( char * )interumStr, strlen( (char *)interumStr),
	       				xslFile, &xmlStr, &ier );
	       if( ier < 0 ) {
	          xsltError = True;
	       }
	    }
	    else {
	       xsltError = True;
	    }


            if ( ier == 0 ) {

	       G_FREE( interumStr, unsigned char );
               
               /*
	         *  Wrap the text into multiple lines.  
		 *  For "FZLVL", also add indentation after the first line.
		 */
	       tmplen = (int) strlen( (char *)xmlStr );
	       
	       G_MALLOC ( outStr, unsigned char, tmplen * 2, "pgairmet_format" );
	       G_MALLOC ( tmpStr, char, tmplen * 2, "pgairmet_format" );
	       G_MALLOC ( fzlStr, char, tmplen * 2, "pgairmet_format" );
	       outStr[ 0 ] = '\0';
	       tmpStr[ 0 ] = '\0';
	       fzlStr[ 0 ] = '\0';
	       
	       line = COL_WRAP;	       	       
	       fzlPtr = strstr ( (char *)xmlStr, "FRZLVL..." );
	       if ( fzlPtr != (char *)NULL ) {
		   strcpy ( tmpStr, (char *)xmlStr );
		   tmpPtr = strstr ( tmpStr, "FRZLVL..." );
		   *tmpPtr = '\0';
		   cst_wrap ( tmpStr, blank, &line, "\n", newLineStr,
	                      (char *)outStr, &ier );		   
		   strcpy ( (char *)outStr, tmpStr ); 
		   
		   cst_wrap ( fzlPtr, " |-", &line, "\n", newLineStr,
	                      fzlStr, &ier );
		   strcat ( (char *)outStr, fzlStr );
	       }
	       else {
	           cst_wrap ( (char *)xmlStr, " |-", &line, "\n", (char *)NULL,
	                      (char *)outStr, &ier );	       
	       }
	       
	       G_FREE ( xmlStr, unsigned char );
	       G_FREE ( tmpStr, char );
	       G_FREE ( fzlStr, char );
	       
	       if ( outStr ) {
		  
		  pgairmet_mkFileName ( fileName, types[ jj ], areas[ ii ], cycle,
		  	_save[ _numFiles ].fileName );

		  _save[ _numFiles ].saveStr = outStr;
		  _numFiles++;

	       }

              
	       if ( *txt == NULL ) {
		  G_MALLOC ( *txt, char, (int)( strlen( (char *)outStr ) + 2 ), 
		             "pgairmet_format" );

		  (*txt)[ 0 ] = '\0';
	       }
	       else {
		  G_REALLOC ( *txt, char, (int)(strlen ( *txt ) + 
		              strlen ( (char *)outStr ) + 2 ),
			      "pgairmet_format" );
	       }
               
	       strcat ( *txt, (char *)outStr );
	       strcat ( *txt, "\n" );
                              
	    }
	    
	    G_FREE ( fmtStr[ii][jj], char );

	    if( xsltError ) {
	       errCode = -5;
	       er_wmsg( "pgen", &errCode, NULL, &ignore, 4, 0 );
	       NxmErr_update();
	    }

        }
      }

      for ( kk = 0; kk < nIn; kk++ ) {
          cvg_freeElPtr ( &elIn[kk] );
      }

      G_FREE ( elIn, VG_DBStruct );

      /*
       *  Flush buffer
       */
      geplot ( &ier );

    }

}

/*=====================================================================*/

static void pgairmet_settingChangedCb ( Widget wdgt, XtPointer clnt,
							XtPointer cds )
/************************************************************************
 * pgairmet_settingChangedCb                                            *
 *                                                                      *
 * Callback function for changes of any setting. This routines enables  *
 * the 'Format' button and disables the 'Save' button.			*
 *                                                                      *
 * static void pgairmet_settingChangedCb (Widget, XtPointer, XtPointer) *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created				        *
 * B. Yin/SAIC		10/05	Combine 'Format' and 'Save' buttons	*
 ***********************************************************************/
{

    XtVaSetValues ( _ctrlButtons[ 0 ],  XmNsensitive, TRUE, NULL );

}

/*=====================================================================*/

static char* pgairmet_replaceStr ( char *string, char *subStr, char *rplsStr )
/************************************************************************
 * pgairmet_replaceStr                                                  *
 *                                                                      *
 * This routine replaces all sub strings in a string with the replace   *
 * string.                                                              *
 *                                                                      *
 * static void pgairmet_replaceStr ( string, subStr, rplsStr )          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *string         char    string with sub strings to be replaced  *
 *      *subStr         char    sub string to be replaced               *
 *      *rplsStr        char    string used to replace the sub strings  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *string         char    result string                           *
 *                                                                      *
 * Return parameters:                                                   *
 *      *string         char    result string                           *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created                                 *
 ***********************************************************************/
{
    int         subLen, rplsLen;
    char	*pos;
/*---------------------------------------------------------------------*/

    subLen  = strlen ( subStr );
    rplsLen = strlen ( rplsStr );

    if ( string && strlen ( string ) != (size_t)0 && subLen != 0 ) {

        while ( ( pos = strstr ( string, subStr ) ) ) {

            memmove ( pos + rplsLen, pos + subLen, 
	    		strlen ( pos + subLen ) + 1 );

            strncpy ( pos, rplsStr, rplsLen );

        }
    }

    return string;

}

/*=====================================================================*/

static char* pgairmet_mkFileName ( char *nameTmp, char *type, char *area,
				   char *cycle, char *fileName )
/************************************************************************
 * pgairmet_mkFileName                                                  *
 *                                                                      *
 * This routine makes the file name of the airmet bulletin.		*
 *                                                                      *
 * static void pgairmet_mkFileName ( nameTmp, type, area, fileName )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *nameTmp        char    file name template			*
 *      *type           char    hazard type		                *
 *      *area           char    FA area string 				*
 *      *cycle          char    airmet cycle time                       *
 * Output parameters:                                                   *
 *      *fileName       char    result file name	                *
 *                                                                      *
 * Return parameters:                                                   *
 *      *fileName       char    result file name                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created                                 *
 * L. Hinson/AWC        09/05   Modified Filename structure to incorp.  *
 *                                cycle instead of date                 *
 ***********************************************************************/
{
    char	cycl[ 3 ];

/*---------------------------------------------------------------------*/

    sprintf ( cycl,  "%s",   cycle);
    strcpy ( fileName, nameTmp );

    pgairmet_replaceStr ( fileName, STR_TYPE,  type );
    pgairmet_replaceStr ( fileName, STR_AREA,  area );
    pgairmet_replaceStr ( fileName, STR_CYCLE,  cycl );

    return fileName;

}

/*=====================================================================*/

/* ARGSUSED */
static void pgairmet_issueRadioBtnCb ( Widget wid, long which,
						XtPointer call )
/************************************************************************
 * pgairmet_issueRadioBtnCb                                             *
 *                                                                      *
 * Callback function for the issuance radio buttons.			*
 *                                                                      *
 * static void pgairmet_issueRadioBtnCb ( ) 		                *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      which           long		which button                    *
 *      call            XtPointer       callback struct                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/07   Created				        *
 ***********************************************************************/
{

    _issuance = (int)which;

    pgairmet_settingChangedCb ( wid, NULL, NULL );

}

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

static	WidgetList	_areaChkBox[ 3 ];
static	WidgetList	_typeChkBox;
static	WidgetList	_statusChkBox;
static  WidgetList	_ctrlButtons;

static	int		_numFiles = 0;
static	SaveStr_t	_save[ NUM_REGIONS * NUM_AREAS * NUM_TYPES ];
static 	Boolean		_errTbl = FALSE;


/*
 * private functions 
 */
static void pgairmetp_createPanel1 ( void );
static void pgairmetp_createPanel2 ( void );
static void pgairmetp_createPanel3 ( void );

static void pgairmetp_setTxt ( void );
static void pgairmetp_format ( char *vgFile, int nAreas, char areas[][ 8 ], 
			      int nTypes, char *types[3], char *day, char *cycle, 
			      int flag, char **txt );
static void pgairmetp_createAreaChkBox ( Widget parent, Widget *rowCol, 
					WidgetList *chkBox, char **labelStr, 
					int posX, int posY );
static char* pgairmetp_replaceStr ( char *string, char *subStr, char *rplsStr );
static char* pgairmetp_mkFileName ( char *nameTmp, char *type, char *area,
				   char *cycle, char *fileName );

static void pgairmetp_statusChkBoxCb	( Widget, long, XtPointer );
static void pgairmetp_ctlBtnCb		( Widget, long, XtPointer );
static void pgairmetp_areaChkBoxCb	( Widget, long, XtPointer );
static void pgairmetp_settingChangedCb	( void );

/************************************************************************
 * nmap_pgairmetp.c							*
 *									*
 * This module defines everything for Airmet formatter dialog.		*
 *									*
 * CONTENTS:								*
 *									*
 * public functions:							*
 *	pgairmetp_create		-	create airmet formatter dialog	*
 *	pgairmetp_isUp		-	check if the dialog is up	*
 *	pgairmetp_popup 		-	popup airmet formatter dialog	*
 *	pgairmetp_popdown 	-	popdown airmet formatter dialog	*
 *									*
 * private functions:							*	
 *	pgairmetp_createAreaChkBox  -	create the area check boxs	*
 *	pgairmetp_createPanel1 	-	create the top panel 		*
 *	pgairmetp_createPanel2 	-	create the middle panel		*
 *	pgairmetp_createPanel3	-	create the third panel		*
 *	pgairmetp_format 	-	create text from gfa elements	* 
 *	pgairmetp_mkFileName	-	make airmet bulletin file names	*
 *	pgairmetp_replaceStr	-	replace string in template	*
 *	pgairmetp_setTxt		-	create and set the text message	*
 *	pgairmetp_areaChkBoxCb	-	callback for area check boxs	*
 *	pgairmetp_ctlBtnCb 	   -	callback for control buttons	*
 *	pgairmetp_statusChkBoxCb	   -	callback for status check box	*
 *	pgairmetp_settingChangedCb  -	callback for any setting changes*
 *									*
 ***********************************************************************/

void pgairmetp_create ( Widget parent )
/************************************************************************
 * pgairmetp_create							*
 *									*
 * This function creates the airmet formatter dialog.			*
 *									*
 * void pgairmetp_create ( parent )					*
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

    xmstr = XmStringCreateLocalized ( "AIRMET Prime Format" );

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

    pgairmetp_createPanel1 ( );
    pgairmetp_createPanel2 ( );
    pgairmetp_createPanel3 ( );
    
}

/*=====================================================================*/

void pgairmetp_popup ( void )
/************************************************************************
 * pgairmetp_popup							*
 *									*
 * This function pops up the airmet formatter dialog.			*
 *									*
 * void pgairmetp_popup ( )						*
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
 
    pgairmetp_popdown ();
        
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

void pgairmetp_popdown ( void )
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

Boolean pgairmetp_isUp ( void )
/************************************************************************
 * pgairmetp_isUp                                                        *
 *                                                                      *
 * This function returns the current status of the airmet formatter     *
 * window.                                                              *
 *                                                                      *
 * Boolean pgairmetp_isUp ( )                                            *
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

static void pgairmetp_createPanel1 ( void )
/************************************************************************
 * pgairmetp_createPanel1                                                *
 *                                                                      *
 * This routine creates the top panel of the airmet formatter dialog.   *
 *                                                                      *
 * void pgairmetp_createPanel1 ( )                                       *
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
 * E. Safford/SAIC	07/07	mv to pgairmetp, rm day and cycle 	*
 ***********************************************************************/
{
    long	ii;
    char	statusStr[ 2 ][ 5 ] ={ "NRML", "TEST" };

    Widget      form1, statusRowCol;
/*---------------------------------------------------------------------*/

    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, _editPane, NULL );

    /*
     *  Create status check box
     */
    statusRowCol = XtVaCreateWidget ( "status",
			     xmRowColumnWidgetClass,	form1,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_COLUMN,
			     XmNnumColumns,		1,
			     XmNradioBehavior,		False,
			     XmNx,			240,
			     XmNy, 			0,
			     NULL );

    _statusChkBox = (WidgetList)XtMalloc ( (size_t) 2 * sizeof(Widget) );

    for (ii = 0; ii < 2; ii++) {

	_statusChkBox[ ii ] = XtVaCreateManagedWidget ( statusStr[ ii ],
				 xmToggleButtonWidgetClass, statusRowCol,
				 NULL );

	XtAddCallback ( _statusChkBox[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmetp_statusChkBoxCb,
		        (XtPointer)ii );
	XtAddCallback ( _statusChkBox[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmetp_settingChangedCb,
		        (XtPointer)NULL );

	if ( ii == 0 ) { 

	    XmToggleButtonSetState ( _statusChkBox[ ii ], True, True );

	}

    }

    XtManageChild ( statusRowCol );

}

/*=====================================================================*/

static void pgairmetp_createPanel2 ( void )
/************************************************************************
 * pgairmetp_createPanel2                                                *
 *                                                                      *
 * This routine creates the middle panel of the airmet formatter dialog.*
 *                                                                      *
 * void pgairmetp_createPanel2 ( )		                        *
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
 * E. Safford/SAIC	05/07	rm issuance radio btns -- they will     *
 *				  move to the Format GUI in 8.80.4	*
 ***********************************************************************/
{
    int		ii;
    char	*westStr[]    = { "WEST", "SLC", "SFO" };
    char	*centralStr[] = { "CENTRAL", "CHI", "DFW" };
    char	*eastStr[]    = { "EAST", "BOS", "MIA" };
    char	*typeStr[]    = { "SIERRA", "TANGO", "ZULU" };
   
    Widget	form2, form3, westRowCol, centralRowCol, eastRowCol;
    Widget	typeRowCol, pane, areaForm;
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
    pgairmetp_createAreaChkBox ( areaForm, &westRowCol, &_areaChkBox[ 0 ],
				westStr, 110, 0 );
    pgairmetp_createAreaChkBox ( areaForm, &centralRowCol, &_areaChkBox[ 1 ],
				centralStr, 260, 0 );
    pgairmetp_createAreaChkBox ( areaForm, &eastRowCol, &_areaChkBox[ 2 ],
				eastStr, 410, 0 );


    form3 = XtVaCreateManagedWidget ( "form3", xmFormWidgetClass, 
    				      _editPane, NULL );

    typeRowCol = XtVaCreateManagedWidget ( "type_check_box",
			     xmRowColumnWidgetClass,	form3,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_NONE,
			     XmNnumColumns,		1,
			     XmNradioBehavior,		False,
			     XmNx,			70,
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
		        (XtCallbackProc)pgairmetp_settingChangedCb,
		        (XtPointer)NULL );
    }

}

/*=====================================================================*/

static void pgairmetp_createPanel3 ( void )
/************************************************************************
 * pgairmetp_createPanel3                                                *
 *                                                                      *
 * This routine creates the bottom panel of the airmet formatter dialog.*
 *                                                                      *
 * void pgairmetp_createPanel3 ( )                                       *
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
 * E. Safford/SAIC	07/07	change "Format/Save" to "Generate/Save" *
 ***********************************************************************/
{
    long		ii, nn;
    char	*ctlstrs[] = { "Generate/Save", "Cancel" };
       
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
		(XtCallbackProc)pgairmetp_ctlBtnCb, (XtPointer) ii );

    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgairmetp_statusChkBoxCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgairmetp_statusChkBoxCb                                              *
 *                                                                      *
 * Callback function for status check box. 		   		*
 *                                                                      *
 * void pgairmetp_statusChkBoxCb ( wid, which, call )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     	Widget          widget ID                       *
 *      which   	long		which button                    *
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
static void pgairmetp_areaChkBoxCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgairmetp_areaChkBoxCb                                                *
 *                                                                      *
 * Callback function for area check boxs. 		                *
 *                                                                      *
 * void pgairmetp_areaChkBoxCb ( wid, which, call )                      *
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

static void pgairmetp_createAreaChkBox ( Widget parent, Widget *rowCol, 
					WidgetList *chkBox, char **labelStr, 
					int posX, int posY )
/************************************************************************
 * pgairmetp_createAreaChkBox                                            *
 *                                                                      *
 * This routine creates the area check boxs of the airmet dialog.       *
 *                                                                      *
 * void pgairmetp_createAreaChkBox ( parent, rowCol, chkBox, 		*
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
		        (XtCallbackProc)pgairmetp_areaChkBoxCb,
		        (XtPointer)ii );
	XtAddCallback ( (*chkBox)[ ii ], XmNarmCallback,
		        (XtCallbackProc)pgairmetp_settingChangedCb,
		        (XtPointer)NULL );

    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgairmetp_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgairmetp_ctlBtnCb                                                    *
 *                                                                      *
 * Callback function for the control buttons.                           *
 *                                                                      *
 * static void pgairmetp_ctlBtnCb ( wid, which, call )                   *
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
 * B. Yin/SAIC		04/08	set default CLASS to MET 		*
 ***********************************************************************/
{
    int 	ii;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:           /* Format and Save */

        /*
         * Refresh the screen before calling pgairmetp_setTxt(),
         * and then call pgairmetp_setTxt().
	 */
        pgpalw_refresh ();
	pgairmetp_setTxt();

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

	pgairmetp_popdown ();

	break;

      default:

	break;
   }

   pgpalw_setClassMet();

}

/*=====================================================================*/

static void pgairmetp_setTxt ( void )
/************************************************************************
 * pgairmetp_setTxt                                                      *
 *                                                                      *
 * This routine sets the text message in the scrolled text window.      *
 *                                                                      *
 * static void pgairmetp_setTxt ( ) 			                *
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
 *                              pgairmetp_format                        *
 * E. Safford/SAIC	07/07	use pgcycle_getCycle for day/cycle	*
 ***********************************************************************/
{
    int 	ii, jj, flag, nAreas, nTypes, ier;
    char	areas[ NUM_REGIONS * NUM_AREAS ][ 8 ], *types[ NUM_TYPES ];
    char	*tmpStr, day[ 12 ] = {"01"}, cycle[ 12 ] = {"03"};

    char	*txt = NULL;

    XmString	xmStr;
/*---------------------------------------------------------------------*/

    pgcycle_getCycle( day, cycle, &ier );

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
     *  Get normal/test flag
     */
    flag = 0;
    if ( XmToggleButtonGetState ( _statusChkBox[ 0 ] ) ) {

       flag = 1;

    }

    /*
     *  Format
     */
    pgairmetp_format ( cvg_getworkfile(), nAreas, areas, nTypes,
  		      types, day, cycle, flag, &txt );
    
    if ( txt ) {

	XtVaSetValues ( _msgTxt, XmNvalue, txt, NULL );
	G_FREE ( txt, char );

    }
    else {

	XtVaSetValues ( _msgTxt, XmNvalue, "", NULL );

    }

    
    for ( ii = 0; ii < nTypes; ii++ ) {
        G_FREE( types[ii], char );
    }

}

/*=====================================================================*/

static void pgairmetp_format ( char *vgFile, int nAreas, char areas[][ 8 ], 
			      int nTypes, char *types[3], char *day, 
                              char *cycle, int flag, char **txt )
/************************************************************************
 * pgairmetp_format                                                      *
 *                                                                      *
 * This routine creates the text message from GFAs in the VG file. 	*
 *                                                                      *
 * static void pgairmetp_format ( vgFile, nAreas, areas, nTypes, 	*
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
 * L. Hinson/AWC        09/05   Add cycle arg. to pgairmetp_mkFileName   *
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
 * B. Yin/SAIC		12/07	Send an error message if there are 	*
 *				different FZLVL ranges.			*
 ***********************************************************************/
{
    int         ii, jj, kk, ier, nextEl, curPos, nIn, line, tmplen;
    int		errCode, ignore, display_flg;

    char	*fmtStr[NUM_REGIONS*NUM_AREAS][NUM_TYPES];
    char        typeLC[ 32 ], xslFile[ 64 ], fileName[ 128 ];
    char	blank[2]={' '}, tag_val[4096];
    char	newLineStr[7]="      ";  /* 6 spaces */
    
    char		*tmpPtr, *tmpStr, *fzlPtr, *fzlStr;
    char		issueTm[ 5 ] = { "0145" };
    unsigned char	*outStr, *interumStr, *xmlStr;

    Boolean		xsltError = False;
    VG_DBStruct 	*elIn, el;

    char        info[64];
/*---------------------------------------------------------------------*/

    for( ii=0; ii<NUM_AREAS; ii++ ) {
        for( jj=0; jj<NUM_TYPES; jj++ ) {
	    fmtStr[ii][jj] = NULL;
	}
    }


    /*
     *  Get the airmet file name template
     */
    ctb_gfagtemp ( fileName, &ier );
    if ( ier != 0 ) {
       strcpy ( fileName, FILE_TEMP );
    }

    /*
     *  Get the total number of GFA elements in the vg file
     *  Allocate memory for them
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
	      nIn++;
          }
       }

       cvg_freeElPtr ( &el );
       nextEl++;

    }                                       /* element loop  */

    G_MALLOC ( elIn, VG_DBStruct, nIn, "pgairmetp_format" );

    /*
     *  Read GFA elements
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

              ii++;
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

      af_getAirmetXml( vgFile, nAreas, areas, nTypes, types, day, cycle,
      			issueTm, fmtStr, &ier );

      /*
       *  Send an error message if there are different FZLVL ranges.
       */
      if ( ier == 2 ) {

      	       errCode = 13;
	       er_wmsg( "pgen", &errCode, NULL, &ignore, 4, 0 );
	       NxmErr_update();

      }

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
	       
	       G_MALLOC ( outStr, unsigned char, tmplen * 2, "pgairmetp_format" );
	       G_MALLOC ( tmpStr, char, tmplen * 2, "pgairmetp_format" );
	       G_MALLOC ( fzlStr, char, tmplen * 2, "pgairmetp_format" );
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
		  
		  pgairmetp_mkFileName ( fileName, types[ jj ], areas[ ii ], cycle,
		  	_save[ _numFiles ].fileName );

		  _save[ _numFiles ].saveStr = outStr;
		  _numFiles++;

	       }

              
	       if ( *txt == NULL ) {
		  G_MALLOC ( *txt, char, (int)( strlen( (char *)outStr ) + 2 ), 
		             "pgairmetp_format" );

		  (*txt)[ 0 ] = '\0';
	       }
	       else {
		  G_REALLOC ( *txt, char, (int)(strlen ( *txt ) + 
		              strlen ( (char *)outStr ) + 2 ),
			      "pgairmetp_format" );
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

static void pgairmetp_settingChangedCb ( void )
/************************************************************************
 * pgairmetp_settingChangedCb                                            *
 *                                                                      *
 * Callback function for changes of any setting. This routines enables  *
 * the 'Format' button and disables the 'Save' button.			*
 *                                                                      *
 * static void pgairmetp_settingChangedCb ( ) 		                *
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

static char* pgairmetp_replaceStr ( char *string, char *subStr, char *rplsStr )
/************************************************************************
 * pgairmetp_replaceStr                                                  *
 *                                                                      *
 * This routine replaces all sub strings in a string with the replace   *
 * string.                                                              *
 *                                                                      *
 * static void pgairmetp_replaceStr ( string, subStr, rplsStr )          *
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

static char* pgairmetp_mkFileName ( char *nameTmp, char *type, char *area,
				   char *cycle, char *fileName )
/************************************************************************
 * pgairmetp_mkFileName                                                  *
 *                                                                      *
 * This routine makes the file name of the airmet bulletin.		*
 *                                                                      *
 * static void pgairmetp_mkFileName ( nameTmp, type, area, fileName )    *
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

    pgairmetp_replaceStr ( fileName, STR_TYPE,  type );
    pgairmetp_replaceStr ( fileName, STR_AREA,  area );
    pgairmetp_replaceStr ( fileName, STR_CYCLE,  cycl );

    return fileName;

}

/*=====================================================================*/


#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "proto_xw.h"

#define TCAINFO_TBL 	"tcainfo.tbl"
#define BKP_TBL 	"TCA_BKPTS"
#define BKP_TBL_OFF 	"TCA_BKPTS_OFF"
#define BKP_TBL_SUP 	"TCA_BKPTS_SUP"
#define BKP_TBL_ISL 	"TCA_BKPTS_ISL"
#define BKP_TBL_WAT 	"TCA_BKPTS_WAT"
#define FIXED_FONT	"9x15bold"
#define TXT_NAME	"KNHCTCV"
#define TXT_NAMEHI	"PHFOTCV"
#define TEMP_TEXT	"temp_txt.txt"
#define PHFO_HDR1       "WTPA8"
#define PHFO_HDR2       "TCVCP"

#define MSG_SIZE	10000

#define MENU_STR_LENGTH 32
#define NUM_OPTION_MENU	9
#define VISIBLE_ITEMS	6

#define WHITE		31
#define SKYBLUE		26
#define CYAN		6

#define CIRCLE		2
#define DOT		17

#define BKPT_INC 	5
#define HDLBSIZE 	7

#define NUM_BPS		30	/* number of bps when searching for 2nd bp */

#define CUBA		3	/* first digit of bp pri in Cuba    */
#define FL_KEYS		9	/* first digit of bp pri on FL keys */
#define RICO		7	/* first digit of bp pri on Puerto Rico */
#define HISPAN		4	/* first digit of bp pri on Hispaniola */

/************************************************************************
 * pgtca.c								*
 *									*
 * This file contains the subroutines necessary to build and operate	*
 * the tca popup to edit tca attributes. 				*
 *									*
 * CONTENTS:								*
 *   global functions:							*
 *	pgtca_create		- creates the tca attribute window	*
 *      pgtca_isNewSeg 		- new segment or not			*
 *      pgtca_isUp 		- return tca dialog status		*
 *	pgtca_popdown		- unmanages the tca attribute window	*
 *	pgtca_popup		- manages the tca attribute window	*
 *	pgtca_stepList 		- select segment by arrow key (up/down)	*
 *									*
 *   private functions:							*
 *	pgtca_addItem		- add a segment into the list		*
 *	pgtca_advisoryArrowCb 	- callback for advisory# arrow buttons	*
 *      pgtca_allocEntryStr     - allocate memory for menu entries      *
 *      pgtca_bpTypeCb	 	- break point type menu callback	*
 *	pgtca_buttonCb 		- callback for the all other buttons	* 
 *      pgtca_clickEh 		- click event handler			*
 *      pgtca_closeCb 		- callback to close TCA dialog		*
 *      pgtca_completeTimeCb	- callback to complete the time string  *
 *	pgtca_createPanel1	- create the top panel of the dialog 	*
 *	pgtca_createPanel2	- create the middle panel of the dialog	*
 *	pgtca_createPanel3	- create the bottom panel of the dialog *
 *      pgtca_createPopSv 	- create the VG file save dialog	*
 *      pgtca_createTxt  	- save the TCA elements into a vg file  *
 * 	pgtca_createOptMenu	- create option menu			*
 *      pgtca_delHandleBar 	- delete a handle bar			*
 *	pgtca_delItem		- delete a segment from the list	*
 *      pgtca_delSegment	- delete a segment from TCA element	*
 *      pgtca_enableApplyCb	- callback to enable create txt button  *
 *      pgtca_geoCb	 	- geography menu callback		*
 *      pgtca_getBasinId	- get the basinID			* 
 *      pgtca_getLatLon 	- get break point name/lat/lon		*
 *   	pgtca_getMenuEntries 	- read one menu entries from the table	*
 *      pgtca_getMenuInfo 	- read menu entry strings from the table*
 *      pgtca_getOptMenuValue 	- get menu entry value			*
 *   	pgtca_getPri		- get priority number of a break point	* 
 *      pgtca_getSegmentAttr 	- get segment attributes from panel 2	*
 *      pgtca_getTcaAttr 	- get tca attributes from panel 1	*
 *      pgtca_itemSelectedCb	- callback for item selected in list	*
 *      pgtca_makeTextMsg 	- create TCA text message		*
 *	pgtca_makeTcaVgFile	- create VGF name and save TCA ele.	*
 *	pgtca_saveVgFile	- save TCA element into a vg file	*
 *	pgtca_confirmCb		- OK callback for confirmation window	*
 *      pgtca_confirmCancelCb   - OK callback for cancel all confirmation
 *      pgtca_pointerEh 	- event handler for mouse drag		*
 *      pgtca_popdownDragBox 	- pop down break point label drag box 	*
 *      pgtca_putHandleBar 	- draw a handle bar			*
 *      pgtca_setSegmentAttr 	- set segment attributes in panel 2	*
 *      pgtca_setTcaAttr 	- set TCA attributs in panel 1		*
 *	pgtca_stormArrowCb 	- callback for storm# arrow buttons	*
 *      pgtca_svCtlBtnCb 	- callback for file save buttons	*
 *      pgtca_updateSegment 	- save a updated segment to element	*
 *      pgtca_updateTca 	- save a TCA element			*
 *	pgtca_checkInfo		- check some textwidget box values	*
 *      pgtca_addSegment	- load a segment into _listBP		*
 *      pgtca_vrfyAdvNumCb	- Verify input as valid advisory number *
 **									*
 * Log:									*
 * B. Yin/SAIC   	02/04	Created                               	*
 * B. Yin/SAIC   	04/04	Added functionality to edit a TCA 	*
 * B. Yin/SAIC   	05/04	Added under scores to global variables 	*
 * B. Yin/SAIC   	05/04	Changed yellow circle to sky blue. 	*
 *                              Moved delete button to the middle panel.*
 * B. Yin/SAIC   	05/04	Corrected typo.			 	*
 * B. Yin/SAIC   	07/04	Added two callbacks for geo and bp type *
 * B. Yin/SAIC   	08/04	Modified code for water and islands	*
 * B. Yin/SAIC   	08/04	Added code to generate text message	*
 * B. Yin/SAIC   	12/04	Added time zone menu			*
 * H. Zeng/SAIC		01/05	Added two golbal variables		*
 * B. Yin/SAIC   	03/05	Added arrow key function pgtca_stepList	*
 * B. Yin/SAIC		04/05	Added issue status into TCA elememnts	*
 * B. Yin/SAIC		09/05	Fixed the Florida Keys problem		*
 * S. Gilbert/NCEP	10/05	Added "Cancel All" button		*
 * S. Gilbert/NCEP	02/06	Modified to handle intermediate advisory*
 *                              numbers                                 *
 * S. Gilbert/NCEP	03/06	Added Boolean arg to pgtca_makeTcaVgFile*
 * X. Guo/CWS           04/11   Added functionality to edit a TCA text  *
 *                              file name for PHFO                      *
  ***********************************************************************/

/*
 * Global variables and function prototypes
 */
    static Widget	_tcaAttribW;
    static Widget	_editPane;
    static Widget	_txtStormNum;
    static Widget	_txtStormName;
    static Widget	_txtTime;
    static Widget	_txtAdvisoryNum;
    static Widget 	_optMenu[ NUM_OPTION_MENU ];
    static Widget 	_pulldownMenu[ NUM_OPTION_MENU ];
    static WidgetList 	_menuButtons[ NUM_OPTION_MENU ];
    static Widget	_listBP;
    static Widget	_txtBP1;
    static Widget	_txtBP2;
    static Widget       _bkpLabel = NULL;
    static Widget       _tcaSvWin = NULL;
    static Widget       _txtFileName;
    static Widget       _txtMsg;
    static Widget	_createText;
    static Widget       _cancelAll;
    static WidgetList	_buttonBtm;
    static Widget	_buttonApply;
    static Widget	_buttonNewSeg;
    static Widget	_buttonDel;
    static Widget       _rowcolName;
    static Boolean	_newTca 	= TRUE;
    static Boolean	_newSegment 	= FALSE;
    static Boolean	_segSelected 	= FALSE;
    static Boolean	_bpSelected 	= FALSE;
    static Boolean	_bp1HandleBar 	= FALSE;
    static Boolean      _isPHFO_Office  = FALSE;
    static int		_bpPri		= -1;		/* first digit of bp Pri */
    static int 		_curSegment;		     	/* current segment index */
    static char 	***_menuEntryStr;
    static int  	_entryNum[ NUM_OPTION_MENU ];
    static int		_bpType  = 0;
    static int		_geoType = 0;
    static Breakpt_T	*_bkpts  = NULL;		/* break points in cur seg */
    static int		_capacity;			/* capacity of _bkpts	   */
    static int		_numBkpts;			/* number of break points  */
    static char		_vgFileName[MXFLSZ];
    static char         _tcvWorkDir[FILE_FULLSZ];
    static char         _curTextFile[20];
    static char         _selectedTextFile[20];


/*
 * Private functions 
 */
    static void pgtca_createPanel1 ( void );
    static void pgtca_createPanel2 ( void );
    static void pgtca_createPanel3 ( void );
    static void pgtca_createOptMenu ( Widget, char *, int, int, int );
    static int  pgtca_allocEntryStr ( int , char*** ); 
    static void pgtca_addItem ( void );
    static void pgtca_delItem ( void );
    static void pgtca_updateTca ( void );
    static void pgtca_delSegment ( int );
    static void pgtca_updateSegment ( void );
    static void pgtca_stormArrowCb ( Widget, long, XtPointer );
    static void pgtca_advisoryArrowCb ( Widget, long, XtPointer );
    static void pgtca_buttonCb ( Widget, long, XtPointer ); 
    static void	pgtca_itemSelectedCb (void);
    static void pgtca_pointerEh ( Widget, long, XEvent*, Boolean* );
    static void pgtca_clickEh ( Widget, long, XEvent*, Boolean*);
    static void pgtca_svCtlBtnCb ( Widget, long, XtPointer );
    static void pgtca_enableApplyCb ( Widget, XtPointer, XtPointer );
    static void pgtca_enableCancelCb ( Widget, XtPointer, XtPointer );
    static void pgtca_createPopSv ( Widget );
    static void pgtca_createTxt ( void ); 
    static void pgtca_setTcaAttr ( TcaInfo * );
    static void pgtca_setSegmentAttr ( TcaWw_T * );
    static void pgtca_getTcaAttr ( TcaInfo * );
    static void pgtca_getSegmentAttr ( TcaWw_T * );
    static int  pgtca_getMenuInfo ( void );
    static int  pgtca_getMenuEntries ( FILE*, int, char*, char*** );
    static int  pgtca_getOptMenuValue ( int );
    static void pgtca_popdownDragBox ( void );
    static void pgtca_putHandleBar ( int, int, float, float );
    static void pgtca_delHandleBar ( float, float, Boolean );
    static void pgtca_getLatLon ( Widget, char *, float *, float * );
    static void pgtca_geoCb ( Widget, XtPointer, XtPointer );
    static void pgtca_bpTypeCb ( Widget, XtPointer, XtPointer );
    static void pgtca_multiPtSel ( int, const float [], const float [] );
    static void pgtca_singlePtSel ( float, float );
    static void pgtca_endMultiPtSel ( void ); 
    static void pgtca_makeTextMsg ( char * ); 
    static void pgtca_makeTcaVgFile ( Boolean );
    static void pgtca_saveVgFile (  char* fileName, int* iret ); 
    static void pgtca_confirmCb  (  Widget, XtPointer, XtPointer );
    static void pgtca_confirmCancelCb  (  Widget, XtPointer, XtPointer );
    static void pgtca_getBasinId ( char * ); 
    static void pgtca_closeCb ( Widget, XtPointer, XtPointer );
    static void pgtca_completeTimeCb ( Widget, XtPointer, XtPointer );
    static Boolean pgtca_checkInfo ( void );
    static void pgtca_addSegment( int replaceSeg, int geogType, char *severity, 
 		char *advisoryType, int numPts, char **pointName, int *iret );
    static int 	pgtca_getPri ( char * bpName );
    static void pgtca_vrfyAdvNumCb ( Widget, XtPointer, XtPointer );
    static void pgtca_findPHFOTextFile ( char * filename, int *location, int *iret );
    static void pgtca_selTextFileCb ( Widget wid, XtPointer clnt, XtPointer call );
    static void pgtca_writeTextMsg ( int *iret );

    enum { STATUS, STORM_TYPE, BASIN, SEVERITY, AD_TYPE, BP_TYPE, GEO, 
    	   TIME_ZONE,PHFO };
    enum { APPLY, DEL_SEG, NEW_SEG, SAVE_TCA, CREATE_TXT, CANCEL_ALL, CLOSE };
    enum { OFFICIAL, ALL_BP };

/*=====================================================================*/

void pgtca_create ( Widget parentW )
/************************************************************************
 * pgtca_create								*
 *									*
 * This function creates the TCA attribute dialog box 			*
 *									*
 * void pgtca_create ( parentW )					*
 *									*
 * Input parameters:							*
 *	parentW		Widget	Parent widget				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC   	02/04	Created         			*
 * B. Yin/SAIC   	04/04	Added code to read menu entry strings	*
 * B. Yin/SAIC   	08/04	Added callback for closing TCA dialog	*
 * m.gamazaychikov/SAIC 04/08   Add code to read work_dir from prefs.tbl*
 ***********************************************************************/
{
    XmString    title_string;
    Atom 	close_dialog;
    int		length, ier;
/*---------------------------------------------------------------------*/ 

   /*
    * Retrieve the working directory for TCV files.
    */
    ctb_rdprf ( "prefs.tbl", "config", "TCV_WORK_DIR", _tcvWorkDir, &ier );
    if ( ier != 0 ) strcpy ( _tcvWorkDir, "./");
    cst_rmbl ( _tcvWorkDir, _tcvWorkDir, &length, &ier );
    if ( _tcvWorkDir[length-1] != '/' ) strcat ( _tcvWorkDir, "/" );

    _tcaAttribW = XmCreateFormDialog ( parentW, "tca_edit", NULL, 0 );

    title_string = XmStringCreateLocalized ( "TCA Attributes" );

    XtVaSetValues( _tcaAttribW,
                   XmNnoResize,          True,  
                   XmNdefaultPosition,   False,
                   XmNdialogTitle,       title_string,
                   XmNautoUnmanage,      False,
                   NULL );
    XmStringFree ( title_string );

    _editPane = XtVaCreateManagedWidget ( "pane",
			xmPanedWindowWidgetClass,	_tcaAttribW,
			XmNmarginHeight,		15,
			XmNmarginWidth,			10,
			XmNspacing,			30,
			XmNsashWidth,			1,
			XmNsashHeight,			1,
			NULL );   

    close_dialog = XmInternAtom ( XtDisplay ( _tcaAttribW ), 
				  "WM_DELETE_WINDOW", False );

    XmAddWMProtocolCallback ( XtParent ( _tcaAttribW ), close_dialog,
                              (XtCallbackProc) pgtca_closeCb, NULL);

    if ( pgtca_getMenuInfo() != 0 ) return;

    pgtca_createPanel1( );
    pgtca_createPanel2( );
    pgtca_createPanel3( );
}
/*=====================================================================*/


static void pgtca_createPanel1 ( void )
/************************************************************************
 * pgtca_createPanel1							*
 *									*
 * This function creates the top panel of the TCA dialog box	 	*
 *									*
 * void pgtca_createPanel1 ( )						*
 *									*
 * Input parameters:							*
 *	                                                                *
 *	None                      	  				*
 **									*
 * Log:									*
 * B. Yin/SAIC	02/04	Created         				*
 * B. Yin/SAIC  04/04	Added callback to enable CreateTxt button	*
 * B. Yin/SAIC  09/04	Reduced the maximum storm number to two digits	*
 * B. Yin/SAIC  12/04	Added time check and complete callbacks		*
 * B. Yin/SAIC 	12/04	Added time zone menu				*
 * B. Yin/SAIC 	04/05	Removed the year field, changed layout		*
 * T. Piper/SAIC	09/05	changed counter to long for 64-bit	*
 * S. Gilbert/NCEP	10/05	added a callback to _txtAdvisoryNum	*
 * S. Gilbert/NCEP	01/06	increased num of chars _txtAdvisoryNum	*
 * S. Gilbert/NCEP	03/06	Changed callback list for               *
 *                              pgtca_enableCancelCb                    *
 ***********************************************************************/
{
    long 	counter; 
    int		col1_x    = 5,  col2_x    = 260, rowHeight = 35;
    int		col1Width = 100, col2Width = 100;

    Widget      form1;
    Widget 	rowcolStormNum;
    Widget 	rowcolStormBox;
    Widget 	rowcolName;
    Widget 	rowcolTime;
    Widget 	rowcolAdvisory;
    Widget 	rowcolAdvisoryBox;
    Widget	stormArrow[ 2 ];
    Widget	advisoryArrow[ 2 ];
/*---------------------------------------------------------------------*/ 

    form1 = XtVaCreateManagedWidget ("form1",
			xmFormWidgetClass, _editPane, NULL );	

    /*
     *  Create 'Issuing Status' option menu
     */
    pgtca_createOptMenu( form1, "Issuing Status:", STATUS, col1_x, 0 );
    XtAddCallback( _pulldownMenu[STATUS], XmNentryCallback, 
                   (XtCallbackProc)pgtca_enableApplyCb, NULL );

    /*
     *  Create 'Storm Type' option menu
     */
    pgtca_createOptMenu( form1, "Storm Type:", STORM_TYPE, col2_x, 0 );
    XtAddCallback( _pulldownMenu[STORM_TYPE], XmNentryCallback, 
                   (XtCallbackProc)pgtca_enableApplyCb, NULL );

    XtVaSetValues( _menuButtons[ STORM_TYPE ][ 0 ],
                   XmNwidth, 200,
                   NULL );

    /*
     * Create 'Basin' option menu
     */
    pgtca_createOptMenu( form1, "Basin:", BASIN, col1_x, 2 * rowHeight );
    XtAddCallback( _pulldownMenu[BASIN], XmNentryCallback, 
                   (XtCallbackProc)pgtca_enableApplyCb, NULL );

    XtVaSetValues( _menuButtons[ BASIN ][ 0 ],
                   XmNwidth, 80,
                   NULL );

    /*
     * Create storm number text box
     */
    rowcolStormNum = XtVaCreateManagedWidget ( "stormNumRowCol", 
  			xmRowColumnWidgetClass,	    	form1, 
			XmNpacking,		    	XmPACK_TIGHT,
			XmNorientation,		    	XmHORIZONTAL,
			XmNx,				col1_x,
			XmNy,				3 * rowHeight,
			NULL );
    XtVaCreateManagedWidget ( "Storm#:",
		    	xmLabelGadgetClass,	rowcolStormNum,
			XmNwidth,		col1Width,
			XmNrecomputeSize,	False,
		    	NULL );
    rowcolStormBox = XtVaCreateManagedWidget ( "stormBoxRowCol",
                        xmRowColumnWidgetClass,         rowcolStormNum,
                	XmNpacking,                     XmPACK_TIGHT,
                	XmNspacing,                     0,
			XmNmarginHeight,		0,
			XmNmarginWidth,			0,
                        XmNorientation,                 XmHORIZONTAL,
                        NULL );
    _txtStormNum = XtVaCreateManagedWidget ( "stormNumTxt",
		    	xmTextWidgetClass,		rowcolStormBox,
   			XmNrows,                        1,
   			XmNcolumns,                     5,
   			XmNcursorPositionVisible,     	True,
   			XmNresizeHeight,              	True,
   			XmNeditable,                  	True,
   			XmNeditMode,     		XmSINGLE_LINE_EDIT, 
   			XmNmaxLength,		 	2,
			XmNvalue,			"1",
   			NULL );

    XtAddCallback( _txtStormNum, XmNmodifyVerifyCallback, 
                   (XtCallbackProc)pgtca_enableApplyCb, NULL );
    XtAddCallback( _txtStormNum, XmNmodifyVerifyCallback, 
		   pgutls_vrfyPosIntCb, NULL );

    stormArrow[ 0 ] = XtVaCreateManagedWidget ( "stormArrow0",
                	xmArrowButtonWidgetClass, 	rowcolStormBox,    
                	XmNarrowDirection,        	XmARROW_UP,
			XmNheight,			16,
                	NULL );
    stormArrow[ 1 ] = XtVaCreateManagedWidget ( "stormArrow1",
                	xmArrowButtonWidgetClass, 	rowcolStormBox,     
                	XmNarrowDirection,       	XmARROW_DOWN,
			XmNheight,			16,
                	NULL );

    for ( counter = 0; counter < 2; counter++) {
        XtAddCallback( stormArrow[ counter ],	XmNactivateCallback,
                       (XtCallbackProc)pgtca_stormArrowCb, (XtPointer)counter ); 
    }

    /*
     * Create storm name text box
     */ 
    rowcolName = XtVaCreateManagedWidget ("nameRowCol",
                xmRowColumnWidgetClass,         form1,
                XmNpacking,                     XmPACK_TIGHT,
                XmNorientation,                 XmHORIZONTAL,
   		XmNx,				col2_x,
   		XmNy,				2 * rowHeight,
                NULL );
    XtVaCreateManagedWidget( "Name:",
                    	xmLabelWidgetClass, 		rowcolName,
			XmNrecomputeSize,		False,
			XmNwidth,			col2Width,
                    	NULL );
    _txtStormName = XtVaCreateManagedWidget( "StormNameTxt",
			xmTextWidgetClass,		rowcolName,
   			XmNrows,			1,
   			XmNcolumns,			12,
   			XmNcursorPositionVisible,	True,
   			XmNresizeHeight,		True,
   			XmNeditable,			True,
   			XmNeditMode,			XmSINGLE_LINE_EDIT,
   			XmNmaxLength,			32,
   			NULL );
    XtAddCallback( _txtStormName, XmNmodifyVerifyCallback, 
		   (XtCallbackProc)pgtca_enableApplyCb, NULL );

    /*
     * Create valid time text box
     */ 
    rowcolTime = XtVaCreateManagedWidget ("timeRowCol",
                xmRowColumnWidgetClass,         form1,
                XmNpacking,                     XmPACK_TIGHT,
                XmNorientation,                 XmHORIZONTAL,
   		XmNx,				col2_x,
   		XmNy,				3 * rowHeight + 2,
                NULL );
    XtVaCreateManagedWidget( "Valid Time:",
			xmLabelGadgetClass, 		rowcolTime,
			XmNrecomputeSize,		False,
			XmNwidth,			col2Width,
                    	NULL );
    _txtTime = XtVaCreateManagedWidget( "timeTxt",
			xmTextWidgetClass,		rowcolTime,
   			XmNrows,			1,
   			XmNcolumns,			12,
   			XmNcursorPositionVisible,	True,
   			XmNresizeHeight,		True,
   			XmNeditable,			True,
   			XmNeditMode,			XmSINGLE_LINE_EDIT,
   			XmNmaxLength,			32,
   			NULL );
    XtAddCallback( _txtTime, XmNmodifyVerifyCallback, 
	 	   (XtCallbackProc)pgtca_enableApplyCb, NULL );
    XtAddCallback( _txtTime, XmNmodifyVerifyCallback, 
	 	   (XtCallbackProc)pgutls_vrfyTimeCb, NULL );
    XtAddCallback( _txtTime, XmNlosingFocusCallback, 
	 	   (XtCallbackProc)pgtca_completeTimeCb, NULL );

    /*
     *  Create 'Time Zone' option menu
     */
    pgtca_createOptMenu ( form1, "Time Zone:", TIME_ZONE, col2_x,
    			  4 * rowHeight + 5 );

    XtVaSetValues ( XmOptionLabelGadget( _optMenu[ TIME_ZONE ] ), 
                XmNwidth,               col2Width,
                XmNrecomputeSize,       False,
                NULL );

    XtVaSetValues( _menuButtons[ TIME_ZONE ][ 0 ],
                   XmNwidth, 	45,
                   NULL );


    XtAddCallback( _pulldownMenu[TIME_ZONE], XmNentryCallback, 
                   (XtCallbackProc)pgtca_enableApplyCb, NULL );
    
    /*
     * Create advisory number text box
     */ 
    rowcolAdvisory = XtVaCreateManagedWidget ("advisoryRowCol",
                xmRowColumnWidgetClass,         form1,
                XmNpacking,                     XmPACK_TIGHT,
                XmNorientation,                 XmHORIZONTAL,
		XmNx,				col1_x,
   		XmNy,				4 * rowHeight + 2,
                NULL );
    XtVaCreateManagedWidget( "Advisory#: ",
			xmLabelGadgetClass, 		rowcolAdvisory,
			XmNrecomputeSize,		False,
			XmNwidth,			col1Width,
                    	NULL );
    rowcolAdvisoryBox = XtVaCreateManagedWidget ("advisoryBoxRowCol",
                        xmRowColumnWidgetClass,         rowcolAdvisory,
                        XmNpacking,                     XmPACK_TIGHT,
                        XmNspacing,                     0,
			XmNmarginHeight,		0,
			XmNmarginWidth,			0,
                        XmNorientation,                 XmHORIZONTAL,
                        NULL );
    _txtAdvisoryNum = XtVaCreateManagedWidget( "advisoryNumTxt",
                xmTextWidgetClass,  		rowcolAdvisoryBox,
   		XmNrows,                        1,
   		XmNcolumns,                     5,
   		XmNcursorPositionVisible,     	True,
   		XmNresizeHeight,              	True,
   		XmNeditable,                  	True,
   		XmNeditMode,     		XmSINGLE_LINE_EDIT,
		XmNvalue,			"1",
   		XmNmaxLength,                 	4,
   		NULL );

    XtAddCallback( _txtAdvisoryNum, XmNmodifyVerifyCallback, pgtca_vrfyAdvNumCb, NULL );
    XtAddCallback( _txtAdvisoryNum, XmNmodifyVerifyCallback, 
		   (XtCallbackProc)pgtca_enableApplyCb, NULL );
    XtAddCallback( _txtAdvisoryNum, XmNvalueChangedCallback,
                   (XtCallbackProc)pgtca_enableCancelCb, NULL );

    advisoryArrow[ 0 ] = XtVaCreateManagedWidget("advisoryArrow0",
                	xmArrowButtonWidgetClass, 	rowcolAdvisoryBox,
                	XmNarrowDirection,        	XmARROW_UP,
			XmNborderWidth,                 0,
			XmNheight,			16,
                	NULL );
    advisoryArrow[ 1 ] = XtVaCreateManagedWidget("advisoryArrow1",
                	xmArrowButtonWidgetClass, 	rowcolAdvisoryBox,
                	XmNarrowDirection,       	XmARROW_DOWN,
			XmNborderWidth,                 0,
			XmNheight,			16,
                	NULL );

    for ( counter = 0; counter < 2; counter++) {
        XtAddCallback ( advisoryArrow[ counter ],	XmNactivateCallback,
                	(XtCallbackProc)pgtca_advisoryArrowCb, (XtPointer) counter ); 
    }
}

/*=====================================================================*/

static void pgtca_createPanel2 ( void )
/************************************************************************
 * pgtca_createPanel2							*
 *									*
 * This function creates the middle panel of the TCA dialog box	 	*
 *									*
 * void pgtca_createPanel2 ( )						*
 *									*
 * Input parameters:							*
 *									*
 *	None                        					*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC   	02/04	Created         			*
 * B. Yin/SAIC   	04/04	Added 'New Segment' button		*
 * B. Yin/SAIC   	05/04	Added the delete button			*
 *                              Changed break point text to uneditable	*
 * B. Yin/SAIC   	07/04	Added the Geo & Break Point callbacks	*
 * B. Yin/SAIC   	12/04	Changed alignments for some fields	*
 ***********************************************************************/
{
    int		col1 = 5, col2 = 185, col3 = 355, oneLineH = 60;
    int 	counter;
    Widget      form2;
    Widget 	rowcolBP1;
    Widget 	rowcolBP2;
/*---------------------------------------------------------------------*/ 

    form2    =  XtVaCreateManagedWidget ("form2",
		xmFormWidgetClass,          	_editPane,
	  	NULL);	

    /*
     *  Severity option menu
     */
    pgtca_createOptMenu( form2, "Severity:", SEVERITY, col1, 0 );

    /*
     *  Advisory Type option menu
     */
    pgtca_createOptMenu( form2, "Advisory Type:", AD_TYPE, col2, 0 );

    XtVaSetValues( _menuButtons[ AD_TYPE ][ 0 ],
                   XmNwidth, 100,
                   NULL );

    /*
     *  Break Point Type  option menu
     */
    pgtca_createOptMenu( form2, "Breakpoint Type:", BP_TYPE, col3, 0 );

    XtVaSetValues( _pulldownMenu[ BP_TYPE ], XmNentryCallback, NULL, NULL );

    for ( counter = 0; counter < _entryNum[ BP_TYPE ]; counter++ ) {
        XtAddCallback( _menuButtons[ BP_TYPE ][ counter ], XmNactivateCallback, 
                       (XtCallbackProc)pgtca_bpTypeCb, NULL );
    }

    /*
     *  Special Geography option menu
     */
    pgtca_createOptMenu( form2, "Special Geography:", GEO, col3, oneLineH );

    XtVaSetValues( _pulldownMenu[ GEO ], XmNentryCallback, NULL, NULL );

    for ( counter = 0; counter < _entryNum[ GEO ]; counter++ ) {
        XtAddCallback( _menuButtons[ GEO ][ counter ], XmNactivateCallback, 
                       (XtCallbackProc)pgtca_geoCb, NULL );
    }

    /*
     * Break Point 1 text box
     */ 
    rowcolBP1 = XtVaCreateManagedWidget ("bk1RowCol",
			xmRowColumnWidgetClass,         form2,
                	XmNpacking,                     XmPACK_TIGHT,
   			XmNx,				col1,
   			XmNy,				oneLineH,
                	NULL );
    XtVaCreateManagedWidget( "Break Point 1: ",
                    	xmLabelGadgetClass, 		rowcolBP1,
                    	NULL );
    _txtBP1 = XtVaCreateManagedWidget( "bp1",
                    	xmTextWidgetClass,  		rowcolBP1,
   			XmNrows,                        1,
   			XmNcolumns,                     13,
   			XmNcursorPositionVisible,     	True,
   			XmNresizeHeight,              	True,
   			XmNeditable,                  	False,
   			XmNeditMode,     		XmSINGLE_LINE_EDIT,
   			XmNmaxLength,                 	64,
   			NULL );

    /*
     * Break Point 2 text box
     */ 
    rowcolBP2 = XtVaCreateManagedWidget ("bk2RowCol",
                	xmRowColumnWidgetClass,         form2,
                	XmNpacking,                     XmPACK_TIGHT,
   			XmNx,				col2,
   			XmNy,				oneLineH,
                	NULL );
    XtVaCreateManagedWidget( "Break Point 2: ",
                    	xmLabelGadgetClass, 		rowcolBP2,
                    	NULL );
    _txtBP2 = XtVaCreateManagedWidget( "bp2",
                    	xmTextWidgetClass,  		rowcolBP2,
   			XmNrows,                        1,
   			XmNcolumns,                     12,
   			XmNcursorPositionVisible,     	True,
   			XmNresizeHeight,              	True,
   			XmNeditable,                  	False,
   			XmNeditMode,     		XmSINGLE_LINE_EDIT,
   			XmNmaxLength,                 	64,
   			NULL ); 

   /*
    * Create 'Apply', 'Delete',  and 'New Segment' buttons
    */
   _buttonApply  = XtVaCreateManagedWidget( "Apply",
			xmPushButtonWidgetClass,	form2,
			XmNsensitive, 			FALSE,
	 		XmNx,				col1 + 40,
			XmNy,				2 * oneLineH + 20,
			XmNheight,			30,
			XmNwidth,			120,
			NULL );
   XtAddCallback( _buttonApply, XmNactivateCallback,
		  (XtCallbackProc)pgtca_buttonCb, (XtPointer) APPLY );

   _buttonDel  = XtVaCreateManagedWidget( "Delete Segment",
			xmPushButtonWidgetClass,	form2,
			XmNsensitive, 			FALSE,
	 		XmNx,				col2,
			XmNy,				2 * oneLineH + 20,
			XmNheight,			30,
			XmNwidth,			120,
			NULL );
   XtAddCallback( _buttonDel, XmNactivateCallback,
		  (XtCallbackProc)pgtca_buttonCb, (XtPointer) DEL_SEG );

   _buttonNewSeg  = XtVaCreateManagedWidget( "New Segment",
			xmPushButtonWidgetClass,	form2,
	 		XmNx,				col3 - 30,
			XmNy,				2 * oneLineH + 20,
			XmNheight,			30,
			XmNwidth,			120,
			NULL );
   XtAddCallback( _buttonNewSeg, XmNactivateCallback,
		  (XtCallbackProc)pgtca_buttonCb, (XtPointer) NEW_SEG );
}
/*=====================================================================*/

static void pgtca_createPanel3 ( void )
/************************************************************************
 * pgtca_createPanel3							*
 *									*
 * This function creates the bottom panel of the TCA dialog box	 	*
 *									*
 * void pgtca_createiPanel3 ( )						*
 *									*
 * Input parameters:							*
 *	                                                                *
 *	None                   		    				*
 **									*
 * Log:									*
 * B. Yin/SAIC   	02/04	Created         			*
 * B. Yin/SAIC   	04/04	Removed 'Clear' button, added callbacks	*
 * B. Yin/SAIC   	05/04	Removed 'delete' button			*
 * H. Zeng/SAIC		01/05	added "Save TCA" button			*
 * T. Piper/SAIC	09/05	changed counter to long for 64-bit	*
 * S. Gilbert/NCEP	10/05	added "Cancel All" button		*
 ***********************************************************************/
{
    int		btnNum  = 4, col1 = 5, nn = 0;
    long	counter = 0;
    char	*btnStr[] = { "Save TCA", "Create Text", "Cancel All", "Close" };

    Arg		args[ 10 ];
    XmFontList	fontlist;
    XmFontListEntry entry;

    Widget 	form3;
/*---------------------------------------------------------------------*/ 

    form3    =  XtVaCreateManagedWidget ("form3",
		xmFormWidgetClass,          	_editPane,
		XmNheight,			180,
	  	NULL);	

    /*
     * Break Points list box
     */
    entry = XmFontListEntryLoad ( XtDisplay( _tcaAttribW ), FIXED_FONT, XmFONT_IS_FONT, "tag1");
    fontlist = XmFontListAppendEntry(NULL, entry);
    XmFontListEntryFree(&entry);

    nn = 0;
    XtSetArg(args[nn], XmNlistSizePolicy,	  XmCONSTANT ); nn++;
    XtSetArg(args[nn], XmNvisibleItemCount,    VISIBLE_ITEMS ); nn++;
    XtSetArg(args[nn], XmNx,                            col1 ); nn++; 
    XtSetArg(args[nn], XmNy,                               0 ); nn++; 
    XtSetArg(args[nn], XmNwidth,                         480 ); nn++; 
    XtSetArg(args[nn], XmNfontList,                 fontlist ); nn++; 

    _listBP   =  XmCreateScrolledList( form3, "list", args, nn );
    XmFontListFree( fontlist );   
    XtAddCallback( _listBP, XmNbrowseSelectionCallback,
		       (XtCallbackProc)pgtca_itemSelectedCb, NULL );

    XtManageChild( _listBP );

    _buttonBtm = ( WidgetList ) XtMalloc( btnNum * sizeof ( Widget ) );
 
    for ( counter = 0; counter < btnNum; counter++ ) {
        _buttonBtm[ counter ] = XtVaCreateManagedWidget ( btnStr[ counter ],
		xmPushButtonWidgetClass,	form3,
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNleftAttachment,            	XmATTACH_FORM,
		XmNleftOffset,			5 + counter * 140,
		XmNheight,			30,
		XmNwidth,			120,
		NULL );
	XtAddCallback( _buttonBtm[ counter ], XmNactivateCallback,
		       (XtCallbackProc)pgtca_buttonCb, (XtPointer)(counter+3) );
   }    

   _createText = _buttonBtm[1];
   _cancelAll = _buttonBtm[2];
   XtVaSetValues ( _createText, XmNsensitive, FALSE, NULL );
   XtVaSetValues ( _cancelAll, XmNsensitive, FALSE, NULL );
}
/*=====================================================================*/


void pgtca_popup ( VG_DBStruct *el )
/************************************************************************
 * pgtca_popup                                                          *
 *                                                                      *
 * This function initiates the text edit popup.				*
 *                                                                      *
 * void  pgtca_popup ( el )                    	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *   	el		VG_DBStruct	Selected TCA element		*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		02/04	Created     				*
 * B. Yin/SAIC		04/04	Modified code to set tca attr from el	*
 * B. Yin/SAIC		05/04	Changed delete button widget name	*
 * B. Yin/SAIC		08/04	Enable geo menu	and allocate _bkpts	*
 * B. Yin/SAIC		08/04	Popdown TCA only when it is up		*
 * H. Zeng/SAIC		01/05	set _vgFileName to null			*
 ***********************************************************************/
{
    if ( pgtca_isUp() )
       pgtca_popdown();

    XtManageChild( _tcaAttribW );

    mcanvw_disarmPress();
    mcanvw_setPressFunc( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);

    _segSelected = FALSE;
    if ( el != NULL ) {
	_newTca = FALSE;
	pgtca_setTcaAttr( &(el->elem.tca.info) );

        XtVaSetValues ( _buttonBtm[0], XmNsensitive, TRUE,   NULL );
        XtVaSetValues ( _createText,   XmNsensitive, FALSE,  NULL );

    }
    else {
	_newTca = TRUE;
	pgtca_setTcaAttr( NULL );

        XtVaSetValues ( _buttonBtm[0], XmNsensitive, FALSE, NULL );
        XtVaSetValues ( _createText,   XmNsensitive, FALSE, NULL );
    }

    _vgFileName[0] = '\0';

    XtVaSetValues ( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		    XmNsensitive,		True,
	            NULL );
    XtVaSetValues ( _buttonApply,  XmNsensitive, FALSE, NULL );
    XtVaSetValues ( _buttonDel,  XmNsensitive, FALSE, NULL );

    if ( !(_bkpts = malloc ( BKPT_INC * sizeof ( Breakpt_T )))) return;
    _capacity = BKPT_INC;
}
/*=====================================================================*/


void pgtca_popdown ( void )
/************************************************************************
 * pgtca_popdown                                                        *
 *                                                                      *
 * This function popdown the TCA edit popup.				*
 *                                                                      *
 * void  pgtca_popdown ( )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *   None                                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          02/04   Created                                 *
 * B. Yin/SAIC          04/04   Set global variable values when popdown *
 * B. Yin/SAIC          08/04   Free the memory of _bkpts		*
 * B. Yin/SAIC          08/04   Added code to clean up	 		*
 * H. Zeng/SAIC		02/05	removed TEMP_VGF			*
 ***********************************************************************/
{
    int 	ier;
    Widget	canvas;
    int		width, height;
/*---------------------------------------------------------------------*/

    if ( _bkpts ) free ( _bkpts );

    _bkpts 		= NULL;
    _newSegment 	= FALSE;
    _segSelected 	= FALSE;
    _bp1HandleBar 	= FALSE;

    mcanvw_disarmDynamic ();
    mcanvw_disarmDrag ();
    mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );

    pgtca_popdownDragBox ();

    /* 
     * clean up the drawing area
     */
    canvas = (Widget) mcanvw_getDrawingW();
    XtVaGetValues( canvas,
                   XmNwidth,  		&width,
                   XmNheight, 		&height,
                   NULL );

    XClearArea ( XtDisplay(canvas), XtWindow(canvas), 0, 0, 
		 width, height, False);

    xpgrestlp();

    /* 
     *  plot the vg elements 
     */
    cvg_redraw( NULL, &ier );
    geplot(&ier);

    XtUnmanageChild ( _tcaAttribW );
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_stormArrowCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtca_stormArrowCb							*
 *									*
 * This function is the callback for the storm number arrow buttons.   	*
 *									*
 * void pgtca_stormArrowCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 *									*
 *	NONE								*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		08/04	Fixed an invalid memory bug		*
 * B. Yin/SAIC		09/04	Reduced the maximum storm number to 99	*
 ***********************************************************************/
{
    char	*stormNumStr, newStr [ 5 ];
    int 	stormNum;
/*---------------------------------------------------------------------*/
         
    XtVaGetValues( _txtStormNum, XmNvalue, &stormNumStr, NULL );
    stormNum = atoi( stormNumStr );

    switch( which ) {
        case 0:                         /* arrow up */
	    if ( stormNum < 99 ) {
	       sprintf( newStr, "%d", ++stormNum );
	       XtVaSetValues( _txtStormNum, XmNvalue, newStr, NULL );
	    }
	    break;

	case 1:                         /* arrow down */
	    if ( stormNum > 1 ) {
	       sprintf( newStr, "%d", --stormNum );
	       XtVaSetValues( _txtStormNum, XmNvalue, newStr, NULL );
	    }
	    break;
    }
   
    XtFree( stormNumStr );
}
/*=====================================================================*/


/* ARGSUSED */
static void pgtca_advisoryArrowCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtca_advisoryArrowCb						*
 *									*
 * This function is the callback for the advisory number arrow buttons. *
 *									*
 * void pgtca_advisoryArrowCb( wid, which, call) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 *									*
 *	NONE								*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		08/04	Fixed an invalid memory bug		*
 * S. Gilbert/NCEP	01/06	Modified to handle intermediate adv nos.*
 ***********************************************************************/
{
    char 	*advisoryNumStr, newStr [ 6 ];
    int 	advisoryNum, ier, iflag;
/*---------------------------------------------------------------------*/
         
    XtVaGetValues( _txtAdvisoryNum, XmNvalue, &advisoryNumStr, NULL );
    gh_advn ( advisoryNumStr, &advisoryNum, &iflag, &ier );
    if ( ier != 0 ) {
        XtFree( advisoryNumStr );
        return;
    }

    switch( which ) {
        case 0:                         /* arrow up */
	    if ( advisoryNum < 32767 ) {
	       sprintf( newStr, "%d", ++advisoryNum );
	       XtVaSetValues( _txtAdvisoryNum, XmNvalue, newStr, NULL );
	    }
	    break;

	case 1:                         /* arrow down */
	    if ( advisoryNum > 1 ) {
               if ( iflag != 0 )
	           sprintf( newStr, "%d", advisoryNum );
               else
	           sprintf( newStr, "%d", --advisoryNum );
	       XtVaSetValues( _txtAdvisoryNum, XmNvalue, newStr, NULL );
	    }
	    break;
    }
   
    XtFree( advisoryNumStr );
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_buttonCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * pgtca_buttonCb							*
 *									*
 * This function is the callback for the control buttons.       	*
 *									*
 * void pgtca_buttonCb ( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 *									*
 *	NONE								*
 **									*
 * Log:									*
 * B. Yin/SAIC   	03/04	Created					*
 * B. Yin/SAIC   	04/04	Added button functionality		*
 * B. Yin/SAIC   	05/04	Changed delete button widget name	*
 * B. Yin/SAIC   	07/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * H. Zeng/SAIC		01/05	added a new case SAVE_TCA		*
 * S. Gilbert/NCEP	10/05	added a new case CANCEL_ALL		*
 * S. Gilbert/NCEP	03/06	Added Boolean arg to pgtca_makeTcaVgFile*
 * M. Li/SAIC		05/06	Added pgtca_delItem			*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int         iret, num, nps, el_loc, el_layer, ii, jj; 
    float 	*xs, *ys, llx, lly, urx, ury;
    char        cancelmesg[]="Cancel all current watches and warnings\n  And create TCV message ?       ";
    filter_t	filter;

    VG_DBStruct	el; 

    switch ( which ) {

	case APPLY:

             el_loc = pgactv_getElmLoc ();
             cvg_rdrec ( cvg_getworkfile(), el_loc, &el, &iret );
             cvg_freeBkpts( &el );

     	     pgactv_getDevPts ( &nps, &xs, &ys );
             pgutls_prepNew ( el_loc, &el, &llx, &lly, &urx, &ury, &iret );
             pgtca_getTcaAttr ( &(el.elem.tca.info) );
             pgvgf_saveNewElm ( NULL, sys_D, &el, nps, xs, ys, TRUE, &el_loc, &iret );

             el_layer = pglayer_getCurLayer( );
             crg_set( &el, el_loc, el_layer, &iret);
	     pgactv_setActvElm(&el, el_loc);

	     if ( _segSelected ) {
	        pgtca_updateSegment ();
		if ( _numBkpts == 0 ) pgtca_delItem();
	     }
	     else {
                /* 
	         * Redraw elements
	         */
                crg_getinx( el_loc, &num, &iret );
                crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret );
                cvg_rfrsh( NULL, llx, lly, urx, ury, &iret );
                /*
                 * Redraw handle bars
                 */
                for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++ ) {
                    for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
                        pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		                       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		                       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	            }
                }
             }

             geplot( &iret );
             cvg_freeBkpts( &el );
 	
	     break;

	case NEW_SEG:
	     _newSegment  = TRUE;
	     _segSelected = FALSE;
	     _numBkpts    = 0;

	     if ( _bp1HandleBar ) {
    		pgtca_delHandleBar ( _bkpts[0].lat, _bkpts[0].lon, True );
		_bp1HandleBar = FALSE;
	     }

	     XmListDeselectAllItems ( _listBP );
	     XtVaSetValues ( _buttonApply, XmNsensitive, FALSE, NULL );
	     XtVaSetValues ( _buttonDel  , XmNsensitive, FALSE, NULL );

      	     XtVaSetValues ( XmOptionButtonGadget( _optMenu[ GEO ] ), 
				XmNsensitive,		False,
				NULL );

	     XtVaSetValues ( _txtBP1, XmNvalue, "", NULL);
	     XtVaSetValues ( _txtBP2, XmNvalue, "", NULL);

	     if ( _geoType != NO_TYPE ) {
                mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);
		pgasel_start ( &pgtca_singlePtSel,
               		       &pgtca_multiPtSel,
                       	       &pgtca_endMultiPtSel );
		break;
	     }

	     mcanvw_disarmDrag ();
	     mcanvw_disarmPress ();
             mcanvw_setPressFunc ( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);
             mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);

	     break;

	case SAVE_TCA:

	    pgtca_makeTcaVgFile ( True );
	    break;

	case CREATE_TXT:

	    pgtca_createTxt ();
	    break;

	case DEL_SEG:

	    pgtca_delItem ();
	    break;

        case CANCEL_ALL:
                                                                                
            if ( ! pgtca_checkInfo () ) {
               NxmConfirm_show ( _tcaAttribW, cancelmesg, pgtca_confirmCancelCb,
                         NULL, NULL, &iret );
            }
            break;

	case CLOSE:
	    /*
	     * remove break point drag box
	     */
            pgtca_popdownDragBox ();

	    mcanvw_disarmPress ();
            mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );

	    if( !_newTca ){
                  el_loc = pgactv_getElmLoc ();
	          crg_getinx ( el_loc, &num, &iret );
                  pghdlb_deselectEl ( num, TRUE );
	    }

	    if ( _bp1HandleBar ) {
    	       pgtca_delHandleBar ( _bkpts[0].lat, _bkpts[0].lon, True );
	       _bp1HandleBar = FALSE;
	    }
            pgtca_popdown ();

	    break;

	default:
	   break;
    }
}

/*====================================================================*/

static void pgtca_createOptMenu( Widget parent, char *labelStr, int menuID, 
				 int posX, int posY ) 
/************************************************************************
 * pgtca_createOptMenu							*
 *									*
 * This function is to create all option menus in the TCA box. 		*
 *									*
 * int pgtca_createOptMenu( parent, labelStr, menuID,			*
 *			    posX, PosY )	 			*
 *									*
 * Input parameters:							*
 *  	parent		Widget		parent widget			*
 *  	*laeblStr	char		labels				*
 *  	menuID		int		which option menu		*
 *  	posX		int		x position of the menu		*
 *  	posY		int		y position of the menu		*
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		04/04	Changed menu entry strings to global	*
 * B. Yin/SAIC		12/04	Added time zone menu			*
 * B. Yin/SAIC		04/05	Modified width of basin menu		*
 * X. Guo/CWS           04/11   Add PHFO menu                           *
 ***********************************************************************/
{
    int 		counter = 0;
    unsigned char 	direction  = 0;
    XmString    	text_string;
/*--------------------------------------------------------------------------*/

    _optMenu[ menuID ]  	= XmCreateOptionMenu  ( parent, "mn", NULL, 0);
    _pulldownMenu[ menuID ]     = XmCreatePulldownMenu( parent, "mnpd", NULL, 0);

    XtVaSetValues ( XmOptionButtonGadget( _optMenu[ menuID ] ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                NULL );

    if ( menuID == BASIN ) {
       XtVaSetValues ( XmOptionLabelGadget( _optMenu[ menuID ] ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                XmNwidth,               100,
                XmNrecomputeSize,       False,
                NULL );
       direction = XmHORIZONTAL;
    }
    else if ( menuID == TIME_ZONE ) {
       XtVaSetValues ( XmOptionLabelGadget( _optMenu[ menuID ] ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                XmNwidth,               80,
                XmNrecomputeSize,       False,
                NULL );
       direction = XmHORIZONTAL;
    }
    else if ( menuID == PHFO ) {
       XtVaSetValues ( XmOptionLabelGadget( _optMenu[ menuID ] ),
                XmNalignment,           XmALIGNMENT_BEGINNING,
                XmNwidth,               120,
                XmNrecomputeSize,       False,
                NULL );
       direction = XmHORIZONTAL;
    }
    else {
       XtVaSetValues ( XmOptionLabelGadget( _optMenu[ menuID ] ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                NULL );
       direction = XmVERTICAL;
    }

    _menuButtons[ menuID ] = (WidgetList) XtMalloc( _entryNum[ menuID ] * sizeof( Widget ) );

    for ( counter = 0; counter < _entryNum[menuID]; counter++ ) {
        _menuButtons[ menuID ][ counter ] =
              XtVaCreateManagedWidget( _menuEntryStr[menuID][ counter ],
                                       xmPushButtonWidgetClass,    _pulldownMenu[ menuID ],
                                       NULL);
    }

    text_string = XmStringCreateLocalized ( labelStr );
    XtVaSetValues (_optMenu[ menuID ],
                XmNsubMenuId,           _pulldownMenu[ menuID ],
                XmNmenuHistory,         _menuButtons[ menuID ][ 0 ],
                XmNlabelString,         text_string,
                XmNorientation,         direction,
                XmNx,                   posX,
                XmNy,                   posY,
                NULL);

    XtManageChild( _optMenu[ menuID ]);

    XmStringFree( text_string );
}
/*====================================================================*/


static void pgtca_addItem( void )
/************************************************************************
 * pgtca_addItem							*
 *									*
 * This function is to add an item into the list	 		*
 *									*
 * int pgtca_addItem( )							*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		04/04	Changed advisory type and severity	*
 *				Disabled 'New Seg' when seg > MAX_TCAWW	*
 * B. Yin/SAIC		05/04	Fixed a bug when reaching MAX_TCAWW	*
 * B. Yin/SAIC		08/04	Modified code to handle water and island*
 * E. Safford/SAIC	04/05	mv seg list dsply to pgtca_addSegment	*
 * E. Safford/SAIC	05/05	free tmpStr				*
 ***********************************************************************/
{
    int		counter = 0, ii, ier;
    char	*tmpStr, **breakPts;
    char	severity[ 128 ]="", advisoryType[ 128 ]="";
    char	newStr[ 128 ]="";
    XmString 	xmStr;
    Widget	wID;
/*-------------------------------------------------------------------*/

    /*
     * get string values from menu 'Advisory Type' and 'Severity'
     */
    for ( counter = SEVERITY; counter <= AD_TYPE ; counter++ ) {

        XtVaGetValues( _pulldownMenu[counter],
			XmNmenuHistory,			&wID,
			NULL );

     	XtVaGetValues(  wID,
			XmNlabelString,			&xmStr,
			NULL );

	XmStringGetLtoR(xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr);
        if ( counter == SEVERITY ) {
           strcpy( severity, tmpStr );
  	   sprintf( newStr, "%s%-15.15s ", newStr, tmpStr ); 
        }
	else {
           strcpy( advisoryType, tmpStr );
  	   sprintf( newStr, "%s%-9.9s ", newStr, tmpStr ); 
        }
	XtFree( tmpStr );
        
    XmStringFree(xmStr);
    }


    /*
     *  Allocate space for the breakPts array and copy the
     *  breakpoint names into it.
     */
    breakPts = ( char ** ) malloc( _numBkpts * sizeof( char *) );
  
    for( ii=0; ii < _numBkpts; ii++ ) {
       breakPts[ ii ] = (char *) malloc( (strlen( _bkpts[ii].breakPtName) + 1 ) 
       							* sizeof( char ) ); 
       strcpy( breakPts[ ii ], _bkpts[ii].breakPtName );
    }
 
    /* 
     *  Display the segment in the segment list.
     */ 
    pgtca_addSegment( -1, _geoType, severity, advisoryType, _numBkpts,
    			breakPts, &ier );

    /*
     *  Free breakPts array
     */
    for( ii=0; ii < _numBkpts; ii++ ) {
        if( breakPts[ ii ] ) free( breakPts[ ii ] );
    }
    if( breakPts ) free( breakPts );
  
}

/*====================================================================*/


static void pgtca_delItem( void )
/************************************************************************
 * pgtca_delItem							*
 *									*
 * This function removes an item from the list and deletes the 		*
 * segment from the TCA element	 					*
 *									*
 * int pgtca_delItem( )							*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * B. Yin/SAIC		02/04	Created					*
 * B. Yin/SAIC		04/04	Added code to modify tca and redraw	*
 *				handle bars				*
 * B. Yin/SAIC		05/04	Added code to activate New Seg button	*
 * B. Yin/SAIC		05/04	Set _segSelelected to false after del	*
 * B. Yin/SAIC		08/04	Made the geo menu active 		*
 * H. Zeng/SAIC		02/05	set the sensitivity of _createText	*
 * S. Gilbert/NCEP	10/05	set the sensitivity of _cancelAll	*
 ***********************************************************************/
{
    int		*pos = NULL;
    int 	count = 0;
    int		itemCount = 0;
/*---------------------------------------------------------------------*/

    if ( XmListGetSelectedPos( _listBP, &pos, &count ) ) {

       XmListDeletePos( _listBP, pos[ 0 ] );
       pgtca_delSegment( pos[ 0 ] - 1 );

       XtVaGetValues( _listBP, XmNitemCount, &itemCount, NULL );
       XtVaSetValues( _buttonNewSeg, XmNsensitive, TRUE, NULL );

       _segSelected = FALSE;

       if ( itemCount > 0 ){
          XtVaSetValues( _buttonBtm[ 0 ], XmNsensitive, TRUE, NULL );
          XtVaSetValues ( _createText,  XmNsensitive, FALSE,  NULL );
       }
       /*
        * No segment left in the list
        */
       else {
          XtVaSetValues( _buttonBtm[ 0 ], XmNsensitive, FALSE, NULL );
          XtVaSetValues( _createText,   XmNsensitive, FALSE, NULL );
          XtVaSetValues( _cancelAll,   XmNsensitive, FALSE, NULL );
       }

       XtVaSetValues( _buttonApply, XmNsensitive, FALSE, NULL );
       XtVaSetValues( _buttonDel,   XmNsensitive, FALSE, NULL );
       XtVaSetValues( XmOptionButtonGadget( _optMenu[ GEO ] ), 
	  	      XmNsensitive,		True,
	              NULL );

       pgtca_setSegmentAttr( NULL );
    }

    if ( pos != NULL )
       free( pos );
}
/*=====================================================================*/


Boolean pgtca_isUp ( void )
/************************************************************************
 * pgtca_isUp                                                           *
 *                                                                      *
 * This function returns the current status of the TCA attribute        *
 * window.                                                              *
 *                                                                      *
 * Boolean pgtca_isUp ( )                                               *
 *                                                                      *
 * Input Parameters:                                                    *
 * Output Parameters:                                                   *
 * 		None	                                                *
 *                                                                      *
 * Return Parameters:                                                   *
 *              Boolean         True if attribute window is active      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		03/04	Created					*
 ***********************************************************************/
{
    return ( XtIsManaged( _tcaAttribW ) );
}
/*=====================================================================*/


static int pgtca_allocEntryStr( int entryNum, char*** entryStr ) 
/************************************************************************
 * pgtca_allocEntryStr							*
 *									*
 * This function is to allocate memory for menu entry strings		*
 *									*
 * int pgtca_allocEntryStr( entryNum, entryStr ) 			*
 *									*
 * Input parameters:							*
 *  	entryNum	int		number of entries		*
 *									*
 * Output parameters:							*
 *  	***entryStr	char		menu entry string array		*
 *									*
 * Return parameters:							*
 *			int		 0:	normal return		*
 *					-1:	not enough memory 	*
 **									*
 * Log:									*
 * B. Yin/SAIC		03/04	Created					*
 ***********************************************************************/
{
   int counter;
/*---------------------------------------------------------------------*/

   if ( ( *entryStr = ( char** ) calloc( entryNum, sizeof( char * ) )) 
        == NULL ) return -1;

   for ( counter = 0; counter < entryNum; counter++ ) {
       if ( ( (*entryStr)[ counter ] = ( char* ) calloc( MENU_STR_LENGTH, 
             sizeof( char ) )) == NULL ) 
          return -1;
   }

   return 0;
}
/*=====================================================================*/


/* ARGSUSED */
static void pgtca_pointerEh ( Widget wid, long which, XEvent *event, 
                              Boolean *ctdr )
/************************************************************************
 * pgtca_pointerEh                                                      *
 *                                                                      *
 * This is the event handler to display the nearest breakpoint to       *
 * the cursor location.                                                 *
 *                                                                      *
 * void pgtca_pointerEh ( wid, which, event, ctdr )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget  the widget calling this function                *
 *      which   long     press, drag, or drop                            *
 *      *event  XEvent  the event callback structure                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ctdr   Boolean continue to dispatch return flag                *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		03/04	Modified from E.Safford's prototype     *
 * B. Yin/SAIC		05/04	Modified code to handle blank stn names *
 * B. Yin/SAIC		05/04	Modified code to handle blank stn ids   *
 *                             	Changed 5 closest stations to 1 station *
 * B. Yin/SAIC		07/04	Modified code to use various bp types   *
 * B. Yin/SAIC          07/04   Changed the length of the char array stn*
 * B. Yin/SAIC          07/04   Changed the length of the table name	*
 * B. Yin/SAIC		09/05	Display 2nd bp only in area of 1st bp	*
 ***********************************************************************/
{
   XmString             xmstr;

   int                  xOff, yOff, iret, npt, nclose, ii;
   int			elv[ NUM_BPS ], pri[ NUM_BPS ], stnnm[ NUM_BPS ];
   
   float		xx, yy, lat, lon;
   float		alat[ NUM_BPS ], alon [ NUM_BPS ];
   
   char        		stn[ 36 ], nstn[ 165 ], nid[ 165 ], tbl[ 33 ]; 
   char			desc[ NUM_BPS ][ 32 ], state[ NUM_BPS ][ 3 ];
   char			country[ NUM_BPS ][ 3 ], stnid[ NUM_BPS ][ 9 ];
   char			c10[ NUM_BPS ][ 20 ];

   Boolean		inCuba, inRico, inHispan, inFLkeys;
   Boolean		outCuba, outRico, outHispan, outFLkeys;
/*---------------------------------------------------------------------*/

   xgtoff ( &xOff, &yOff, &iret );

   switch (which) {

      case 0:				/* press */
         break;

      case 1:				/* drag */

         if ( _bkpLabel == NULL ) {
            _bkpLabel = XtVaCreateWidget( "bkp_label", xmLabelWidgetClass,
                                           mcanvw_getDrawingW(), NULL );
         }
	 else {
	    XtManageChild( _bkpLabel );
         }

	 xx = (float) ( event->xmotion.x + xOff );
	 yy = (float) ( event->xmotion.y + yOff );

	 npt = 1;
	 gtrans ( sys_D, sys_M, &npt, &xx, &yy, &lat, &lon, 
	          &iret, strlen(sys_D), strlen(sys_M) );

	/*
 	 * Find out the proper table to use
 	 */
  	strcpy ( tbl, BKP_TBL );

	switch ( _geoType ) {

           case NO_TYPE:

	      if ( _bpType == OFFICIAL ) {
		 strcpy ( tbl, BKP_TBL_OFF );
              }
              else if ( _bpType == ALL_BP ) {
                 strcpy ( tbl, BKP_TBL_SUP );
              }
	      break;

	   case ISLANDS:
	   
	      strcpy ( tbl, BKP_TBL_ISL );
	      break;

	   case WATER:
              strcpy ( tbl, BKP_TBL_WAT );
              break;

	   default:
	      break;   
        }

        /*
         * Get the closest station id and name of the break point
         */
	 nclose = 1;
	 npt = 0;
         clo_tclosest( tbl, lat, lon, nclose, &iret );
         clo_tgnm( tbl, nclose, nclose * sizeof( stn ), &npt, nstn, &iret );
         clo_tgid( tbl, nclose, nclose * sizeof( stn ), &npt, nid, &iret );

	 /*
	  *  Make sure the 2nd bp is in the same area of the 1st bp
	  */
	 if ( ( _geoType == NO_TYPE ) && ( _bp1HandleBar || _bpSelected ) ) { 

	    nclose = NUM_BPS;
    	    npt    = 0;

	    /*
	     *  Get closest break points and their attributes
	     */
    	    clo_tclosest ( tbl, lat, lon, nclose, &iret );
	    clo_stngall ( tbl, nclose, &npt, alat, alon, desc, 
		          state, stnid, stnnm, country, elv, pri, c10, &iret ); 		

	    /*
	     *  Find the closest bp in the same area of the 1st bp
	     */
  	    nid[ 0 ] = '\0';
	    nstn[ 0 ] = '\0';

	    for ( ii = 0; ii < npt; ii++ ) {
			
		inFLkeys  = ( _bpPri == FL_KEYS ) && ( pri[ ii ] / 10 == FL_KEYS );
		outFLkeys = ( _bpPri != FL_KEYS ) && ( pri[ ii ] / 10 != FL_KEYS );
		inCuba    = ( _bpPri == CUBA ) && ( pri[ ii ] / 10 == CUBA );
		outCuba   = ( _bpPri != CUBA ) && ( pri[ ii ] / 10 != CUBA );
		inRico    = ( _bpPri == RICO ) && ( pri[ ii ] / 10 == RICO );
		outRico   = ( _bpPri != RICO ) && ( pri[ ii ] / 10 != RICO );
		inHispan  = ( _bpPri == HISPAN ) && ( pri[ ii ] / 10 == HISPAN );
		outHispan = ( _bpPri != HISPAN ) && ( pri[ ii ] / 10 != HISPAN );

	        if ( inCuba || inRico || inHispan || inFLkeys ||
		     ( outCuba && outRico && outHispan && outFLkeys ) ) {

	 	   strcpy ( nid, stnid[ ii ] );
		   strcpy ( nstn, desc[ ii ] );
		   break;

		}
	    }
	 }

        /*
         * Get the first station name and id that are not blank
         */
	 if ( strlen( nid ) <= (size_t)1 || strlen ( nstn ) <= (size_t)1 || npt == 0 ) break; 

         xmstr = XmStringCreateLtoR( nstn, XmFONTLIST_DEFAULT_TAG );
         XtVaSetValues( _bkpLabel,
			XmNlabelString,                 xmstr,
                        XmNx,                           event->xbutton.x,
                        XmNy,                           event->xbutton.y + 25,
                                                        NULL);
         XmStringFree (xmstr);
         break;

      case 2:				/* drop */
         break;
   }
}
/*======================================================================*/


/* ARGSUSED */
static void pgtca_clickEh ( Widget wid, long which, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgtca_clickEh                                                        *
 *                                                                      *
 * This is the event handler for button press				*
 *                                                                      *
 * pgtca_clickEh ( wid, which, event, ctdr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget  the widget calling this function                *
 *      which   long     press, drag, or drop                            *
 *      *event  XEvent  the event callback structure                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ctdr   Boolean continue to dispatch return flag                *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created					*
 * B. Yin/SAIC		05/04	Modified code to handle blank stn name	*
 *                              Changed delete button widget name     	*
 *                              Changed yellow circle to sky blue     	*
 * B. Yin/SAIC		05/04	Enlarged segment range			*
 * B. Yin/SAIC          07/04   Added a call to xgtoff() for roaming    *
 * B. Yin/SAIC		08/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * B. Yin/SAIC          09/04   Redraw element after removing handlebars*
 * B. Yin/SAIC          12/04   Redraw element only once 		*
 * E. Safford/SAIC	03/05	MB2 when drawing a new segment treated  *
 * 				as a deselect/reset, not a popdown	*
 * B. Yin/SAIC		04/05	Reset _numrBbkpts when removing 1st bkpt*
 * E. Safford/SAC	04/05	use CYAN instead of RED for point edit  *
 * B. Yin/SAIC		09/05	save the priority number of the 1st bp	*
 ***********************************************************************/
{
   int 			np, iret, ii, jj, num, counter, el_loc, xOff, yOff;
   float 		x1, y1,	xlon, xlat;
   float		maxlon, maxlat, minlon, minlat;
   static Boolean  	bp1Selected = FALSE;
   static Boolean  	bp2Selected = FALSE;
   VG_DBStruct		el; 
/*----------------------------------------------------------------------*/

   /*
    * Left mouse click
    */
   if (event->type == ButtonPress && event->xbutton.button == Button1) {

      if ( _newSegment ) {

	 if ( ! _bp1HandleBar ) {

            pgtca_getLatLon ( _bkpLabel, _bkpts[0].breakPtName, 
			      &(_bkpts[0].lat), &(_bkpts[0].lon) );


	    if ( strlen( _bkpts[0].breakPtName ) != (size_t)0 ) {

               XtVaSetValues ( _txtBP1, XmNvalue, _bkpts[0].breakPtName, NULL);
  	       pgtca_putHandleBar ( WHITE, DOT, _bkpts[0].lat,_bkpts[0].lon );
	       _bp1HandleBar = TRUE;
	       _bpPri = pgtca_getPri ( _bkpts[0].breakPtName );
	       _numBkpts++;

            }

	}
	else {

            pgtca_getLatLon ( _bkpLabel, _bkpts[1].breakPtName, 
			      &(_bkpts[1].lat), &(_bkpts[1].lon) );

	    if ( strlen ( _bkpts[1].breakPtName ) != (size_t)0 ) {
               XtVaSetValues ( _txtBP2, XmNvalue,_bkpts[1].breakPtName, NULL );

	       /*
	        * Remove the first break point handle bar 
	        */	
    	       pgtca_delHandleBar( _bkpts[0].lat, _bkpts[0].lon, True );
	       _numBkpts++;

               pgtca_addItem ();
               pgtca_updateTca ();
  
	       /*
	        * Remove break point drag box
	        */
               pgtca_popdownDragBox( );

	       _bp1HandleBar = FALSE;
               _newSegment   = FALSE;
	       _bpPri 	     = -1;
	    }
  	  }
      }					/* end new segment */

      else if ( ! _newTca ) {

	  if ( ! _segSelected ) {

            xgtoff ( &xOff, &yOff, &iret );

            x1 = event->xbutton.x + xOff;
            y1 = event->xbutton.y + yOff;

            np = 1;	
	    gtrans ( sys_D, sys_M, &np, &x1, &y1, &xlat, &xlon, 
	             &iret, strlen(sys_D), strlen(sys_M) );

            el_loc = pgactv_getElmLoc();
            cvg_rdrec( cvg_getworkfile(), el_loc, &el, &iret );

	    /*
	     * Find nearest segment
	     */
	    for ( counter = 0; counter < el.elem.tca.info.wwNum; counter++ ) {

		maxlat = minlat = el.elem.tca.info.tcaww[ counter ].breakPnt[ 0 ].lat;
		maxlon = minlon = el.elem.tca.info.tcaww[ counter ].breakPnt[ 0 ].lon;

		for ( ii = 1; ii < el.elem.tca.info.tcaww[counter].numBreakPts; ii++ ) {
		    if ( el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat > maxlat )
		       maxlat = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat;
		    if ( el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat < minlat )
		       minlat = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat;
		    if ( el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon > maxlon )
		       maxlon = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon;
		    if ( el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon < minlon )
		       minlon = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon;
		}

		if ( ( xlat > minlat - .5 ) && ( xlat < maxlat + .5 )
		   &&( xlon > minlon - .5 ) && ( xlon < maxlon + .5 ) ) {

                   XmListSelectPos( _listBP, counter + 1, False );
		
		   _numBkpts = el.elem.tca.info.tcaww[ counter ].numBreakPts;

		   pgtca_setSegmentAttr( &(el.elem.tca.info.tcaww[ counter ] ) );

		   if ( !( _bkpts = realloc( _bkpts, 
				    MAX( _numBkpts, BKPT_INC ) * sizeof( Breakpt_T ) ) ))
		      return;

		   _capacity = MAX( _numBkpts, BKPT_INC );

		   for ( ii = 0; ii < _numBkpts; ii++ ) {
		       strcpy ( _bkpts[ ii ].breakPtName, 
				el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName );
		       _bkpts[ ii ].lat = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat;
		       _bkpts[ ii ].lon = el.elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon;
    	               pgtca_delHandleBar ( _bkpts[ ii ].lat, _bkpts[ ii ].lon, ii == _numBkpts - 1 );
		   }

		   _curSegment = counter + 1;

		   /*
                    * Put handle bars 
 		    */

                   for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++) {
	               if ( ii == _curSegment -1 ){
		          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
  	                      pgtca_putHandleBar ( WHITE, DOT, 
				       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
				       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
			  }
		       }
		       else {
		          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
                              pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
				       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
				       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
			  }
		       }
                   }


	           _segSelected = TRUE;
                   _bpSelected  = FALSE;
                   bp1Selected	= FALSE;
                   bp2Selected	= FALSE;

	           XtVaSetValues( _buttonApply,  XmNsensitive, TRUE, NULL );
	           XtVaSetValues( _buttonDel,  XmNsensitive, TRUE, NULL );
   		   XtVaSetValues( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		  		  XmNsensitive,		False,
		  		  NULL );

	     	   if ( _geoType != NO_TYPE ) {
		      mcanvw_disarmPress ();
		      mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);
		      pgasel_start ( &pgtca_singlePtSel,
               		             &pgtca_multiPtSel,
                       	             &pgtca_endMultiPtSel );
	     	   }

	           break;
	       }    			/* end if block */
	    }				/* end for loop */
            cvg_freeBkpts( &el );
	  }		/* end tca selected, segment not selected block */

	  else {			/* segment selected */

	     if ( _geoType != NO_TYPE ) {
	     }
	     else {
	       if ( ! _bpSelected ) {	/* break point not selected */

                  xgtoff ( &xOff, &yOff, &iret );

                  x1 = event->xbutton.x + xOff;
                  y1 = event->xbutton.y + yOff;

                  np = 1;	
	          gtrans (sys_D, sys_M, &np, &x1, &y1, &xlat, &xlon, 
	                      &iret, strlen(sys_D), strlen(sys_M) );
		
	          _bpPri = pgtca_getPri ( _bkpts[ 0 ].breakPtName );

		  /*
		   * Select break point
		   */
		  if ( ( xlat > ( _bkpts[0].lat - .5 ) ) && ( xlat < ( _bkpts[0].lat + .5 ) ) && 
		       ( xlon > ( _bkpts[0].lon - .5 ) ) && ( xlon < ( _bkpts[0].lon + .5 ) ) ) {
  	               pgtca_putHandleBar ( CYAN, DOT, _bkpts[0].lat, _bkpts[0].lon );
		       _bpSelected = TRUE;	
		       bp1Selected = TRUE;
		       bp2Selected = FALSE;
                       mcanvw_setDragFunc( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT );
	          }
		  else if ( ( xlat > ( _bkpts[1].lat - .5 ) ) && ( xlat < ( _bkpts[1].lat + .5 ) ) &&
                            ( xlon > ( _bkpts[1].lon - .5 ) ) && ( xlon < ( _bkpts[1].lon + .5 ) ) ) {
  	               pgtca_putHandleBar ( CYAN, DOT, _bkpts[1].lat, _bkpts[1].lon );
		       _bpSelected = TRUE;		
		       bp1Selected = FALSE;
		       bp2Selected = TRUE;
                       mcanvw_setDragFunc( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);
		  }
	       }
	       else {				/* break point selected */
		  if ( bp1Selected ) {

                     pgtca_getLatLon ( _bkpLabel, _bkpts[ 0 ].breakPtName, 
				       &_bkpts[ 0 ].lat, &_bkpts[ 0 ].lon );

                     XtVaSetValues( _txtBP1, XmNvalue, _bkpts[ 0 ].breakPtName, NULL);

	             _bpSelected = FALSE;		
	             bp1Selected = FALSE;
		  }
		  else if( bp2Selected ) {

                     pgtca_getLatLon ( _bkpLabel, _bkpts[ 1 ].breakPtName, 
				       &_bkpts[ 1 ].lat, &_bkpts[ 1 ].lon );

                     XtVaSetValues( _txtBP2, XmNvalue, _bkpts[ 1 ].breakPtName, NULL);

	             _bpSelected = FALSE;		
	             bp2Selected = FALSE;
		  }

	          /* refresh list*/
	          pgtca_updateSegment();
	      }
	    }
	  }				/* end segment selected */

      }					/* end TCA selected */
   }					/* end button 1 */

   /*
    * Middle mouse click
    */
   if (event->type == ButtonPress && event->xbutton.button == Button2 ) {

      if ( _newSegment ) {			/* create new segment */

         if ( _bp1HandleBar ) {		/* first break point has been choosen */
	    XtVaSetValues ( _txtBP1, XmNvalue, "", NULL);
    	    pgtca_delHandleBar ( _bkpts[0].lat, _bkpts[0].lon, True );
	    _bp1HandleBar = FALSE;
	    _bpPri 	  = -1;
	    _numBkpts = 0;
         }
	 else {

	    /*
	     * remove break point drag box
	     */
            pgtca_popdownDragBox ();

	    mcanvw_disarmPress ();
            mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );

	    if( ! _newTca ){
              el_loc = pgactv_getElmLoc ();
	      crg_getinx ( el_loc, &num, &iret );
              pghdlb_deselectEl ( num, TRUE );
	    }

	    /*
	     *  Turn on the geography menu
	     */
            XtVaSetValues ( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		    XmNsensitive,		True,
	            NULL );
	 }
      }

      else {					/* not a new segment */

	   if ( _segSelected ) {

              el_loc = pgactv_getElmLoc ();
              cvg_rdrec ( cvg_getworkfile (), el_loc, &el, &iret );

	      if ( _bpSelected ) {		/* break point selected */

	          _bpSelected = FALSE;
	          _bpPri      = -1;

		  if ( bp1Selected ){

		     bp1Selected = FALSE;

	             /*
	              * remove break point drag box
	              */
                     pgtca_popdownDragBox( );

    	             pgtca_delHandleBar( _bkpts[0].lat, _bkpts[0].lon, True );

  	             pgtca_putHandleBar ( WHITE, DOT, _bkpts[0].lat, _bkpts[0].lon );
  	             pgtca_putHandleBar ( WHITE, DOT, _bkpts[1].lat, _bkpts[1].lon );
		  }

		  if ( bp2Selected ){

		     bp2Selected = FALSE;

	             /*
	              * remove break point drag box
	              */
                     pgtca_popdownDragBox( );

    	             pgtca_delHandleBar( _bkpts[1].lat, _bkpts[1].lon,True );

  	             pgtca_putHandleBar ( WHITE, DOT, _bkpts[0].lat, _bkpts[0].lon );
  	             pgtca_putHandleBar ( WHITE, DOT, _bkpts[1].lat, _bkpts[1].lon );
		  }

              }
	      else {				/* break point not selected */

	           _segSelected = FALSE;
		   XtVaSetValues ( _buttonApply,  XmNsensitive, FALSE, NULL );
		   XtVaSetValues ( _buttonDel  ,  XmNsensitive, FALSE, NULL );
   		   XtVaSetValues( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		  		  XmNsensitive,		True,
		  		  NULL );

		   XmListDeselectAllItems ( _listBP );
                   pgtca_setSegmentAttr( NULL );

                   for ( jj = 0; jj < _numBkpts; jj++ ) {
    	               pgtca_delHandleBar ( _bkpts[ jj ].lat, _bkpts[ jj ].lon, 
		       			    jj == _numBkpts - 1 );
		   }

                   for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++ ) {
                       for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
  	                   pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
				    el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat,
				    el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
		       }
                   }
	      }
              cvg_freeBkpts( &el );
	   }
	   else {
	        /*
	         * remove break point drag box
	         */
                pgtca_popdownDragBox( );

	        mcanvw_disarmPress();
                mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	        if( !_newTca ){
                  el_loc = pgactv_getElmLoc();
	          crg_getinx ( el_loc, &num, &iret );
                  pghdlb_deselectEl ( num, TRUE );
	        }

	        pgtca_popdown();
	   }
      }
   }

   geplot( &iret );
}

/*====================================================================*/

static void pgtca_updateTca ( void )
/************************************************************************
 * pgtca_updateTca	                                                *
 *                                                                      *
 * This rountine saves the tca element					*
 *                                                                      *
 * pgtca_updateTca ( )				                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		08/04	Added code to free the breakPts memory 	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * H. Zeng/SAIC		02/05	set the sensitivity of _createText	*
 * S. Gilbert/NCEP      12/05   Added default settings for text_ tcainfo*
 *                              members                                 *
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    float 	*xs, *ys, llx, lly, urx, ury;
    int 	nps, iret, el_loc; 
    VG_DBStruct	el; 
/*----------------------------------------------------------------------*/

    if ( _newTca ) {

       pgutls_initHdr( &(el.hdr) );
       el.hdr.vg_type = TCA_ELM;
       el.hdr.vg_class = CLASS_MET;
       ces_get ( -99, &el, &iret );
       el.elem.tca.info.text_lat = 25.8;
       el.elem.tca.info.text_lon = -80.4;
       el.elem.tca.info.text_font = 1;
       el.elem.tca.info.text_size = 1.;
       el.elem.tca.info.text_width = 3;
       pgundo_newStep();
       _newTca = FALSE;
    }
    else {

       el_loc = pgactv_getElmLoc();
       cvg_rdrec( cvg_getworkfile(), el_loc, &el, &iret );
       cvg_freeBkpts( &el );

       pgutls_prepNew( el_loc, &el, &llx, &lly, &urx, &ury, &iret );
       pgundo_newStep();
       pgundo_storeThisLoc( el_loc, UNDO_DEL, &iret );
    }

    pgactv_getDevPts ( &nps, &xs, &ys );
    pgtca_getTcaAttr ( &(el.elem.tca.info) );
    pgtca_getSegmentAttr ( &(el.elem.tca.info.tcaww[ el.elem.tca.info.wwNum - 1 ] ));

    pgvgf_saveNewElm ( NULL, sys_D, &el, nps, xs, ys, TRUE, &el_loc, &iret );
    pgundo_storeThisLoc ( el_loc, UNDO_ADD, &iret );
    pgundo_endStep ();
    cvg_freeBkpts( &el );

    pgutls_redraw ( el_loc, &el, &iret );
    cvg_freeBkpts( &el );

    XtVaSetValues ( _buttonBtm[0], XmNsensitive, TRUE, NULL );
    XtVaSetValues ( _createText,   XmNsensitive, FALSE, NULL );
}

/*====================================================================*/

static void pgtca_getTcaAttr ( TcaInfo *tca )
/************************************************************************
 * pgtca_getTcaAttr	                                                *
 *                                                                      *
 * This rountine gets TCA attributes from panel 1			*
 *                                                                      *
 * pgtca_getTcaAttr ( tca )			                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      None								*
 *                                                                      *
 * Output parameters:                                                   *
 *      tca		Tca		tca attributes			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		12/04	Fixed compile warning on IRIX6		*
 * B. Yin/SAIC 		12/04	Added time zone 			*
 * B. Yin/SAIC		04/05   Removed year field, added issue status	*
 * B. Yin/SAIC		04/05   Using menu strings to set issue status 	*
 * S. Gilbert/NCEP	01/06	advisoryNum changed from int to char*   *
 ***********************************************************************/
{
   char		*tmpStr;
   int 		tmp, statusPos;

   Widget	wID;
   XmString	xmStr;
/*----------------------------------------------------------------------*/

   XtVaGetValues( _txtStormNum, XmNvalue, &tmpStr, NULL );
   tca->stormNum = atoi( tmpStr );
   XtFree( tmpStr);

   XtVaGetValues( _txtStormName, XmNvalue, &tmpStr, NULL );
   strcpy( tca->stormName, tmpStr );
   XtFree( tmpStr);

   XtVaGetValues( _txtTime, XmNvalue, &tmpStr, NULL );
   strcpy( tca->validTime, tmpStr );
   XtFree( tmpStr);

   XtVaGetValues( _txtAdvisoryNum, XmNvalue, &tmpStr, NULL );
   strcpy ( tca->advisoryNum, tmpStr );
   XtFree( tmpStr);

   XtVaGetValues( _listBP, XmNitemCount, &tmp, NULL );
   tca->wwNum = tmp;

   /*
    * Get basin from the pulldown menu
    */
   tmp = pgtca_getOptMenuValue( BASIN );
   tca->basin = (enum basin_t)tmp;

   /*
    * Get storm type from the pulldown menu
    */
   tmp = pgtca_getOptMenuValue( STORM_TYPE );
   tca->stormType = (enum storm_type_t)tmp;

   /*
    *  Get issue status
    */
   statusPos = pgtca_getOptMenuValue( STATUS );

   if ( strcasecmp ( _menuEntryStr[ STATUS ][ statusPos ], "experimental" ) == 0 )
		tca->issueStatus = TCA_EXPERIMENTAL;
   else if ( strcasecmp ( _menuEntryStr[ STATUS ][ statusPos ], "test" ) == 0 )
      tca->issueStatus = TCA_TEST;
   else if ( strcasecmp ( _menuEntryStr[ STATUS ][ statusPos ], "operational" ) == 0 )
      tca->issueStatus = TCA_OPERATIONAL;
   else if ( strcasecmp ( _menuEntryStr[ STATUS ][ statusPos ], "experimental operational" ) == 0 )
		tca->issueStatus = TCA_EXPERIMENTAL_OPNL;
   else 
		tca->issueStatus = TCA_OPERATIONAL;

   /*
    *  Get Time Zone
    */
   XtVaGetValues ( _pulldownMenu[ TIME_ZONE ], XmNmenuHistory, &wID, NULL );
   XtVaGetValues ( wID, XmNlabelString,	&xmStr, NULL );

   XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

   strncpy ( tca->timezone, tmpStr, 3 );

   tca->timezone[ 3 ] = '\0';

   XmStringFree ( xmStr );
   XtFree( tmpStr);
}
/*====================================================================*/


static void pgtca_getSegmentAttr ( TcaWw_T * tcaWW )
/************************************************************************
 * pgtca_getSegmentAttr                                                 *
 *                                                                      *
 * This rountine gets the segment attributes from panel 2		*
 *                                                                      *
 * void pgtca_getSegmentAttr ( tcaWW )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      None								*
 *                                                                      *
 * Output parameters:                                                   *
 *      tcaWW		TcaWw_t		segment attributes              *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		07/04	Added code to handle water and islands 	*
 * B. Yin/SAIC		12/04	Fixed compile warning on IRIX6		*
 ***********************************************************************/
{
    int counter, tmp;
/*---------------------------------------------------------------------*/

    tmp = pgtca_getOptMenuValue( SEVERITY );
    tcaWW->severity 	= (enum tca_sev_t)tmp;
    tmp = pgtca_getOptMenuValue( AD_TYPE );
    tcaWW->advisoryType = (enum tca_adv_t)tmp;
    tmp = pgtca_getOptMenuValue( GEO );
    tcaWW->specialGeog 	= (enum tca_sp_geog_t)tmp;
    tcaWW->numBreakPts  = _numBkpts;

    if ( _newSegment ) {
       if ( !( tcaWW->breakPnt = malloc ( _numBkpts * sizeof ( Breakpt_T ))))
           return;
    }
    else {
       if ( !( tcaWW->breakPnt = realloc ( tcaWW->breakPnt, 
					_numBkpts * sizeof ( Breakpt_T ))))
	   return;
       _capacity = _numBkpts;
    }

    for ( counter = 0; counter < _numBkpts; counter++ ) {
	strcpy( tcaWW->breakPnt[ counter ].breakPtName, _bkpts[ counter ].breakPtName );
	tcaWW->breakPnt[ counter ].lat = _bkpts[ counter ].lat;
	tcaWW->breakPnt[ counter ].lon = _bkpts[ counter ].lon;
    }
}
/*====================================================================*/


static void pgtca_delSegment ( int index ) 
/************************************************************************
 * pgtca_delSegment                                                     *
 *                                                                      *
 * This rountine deletes the selected segment				*
 *                                                                      *
 * void pgtca_delSegment ( index )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      index   int     index of the selected segment                   *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Changed the memory move loop.	   	*
 * B. Yin/SAIC		05/04	Added code to redraw handle bars.	*
 * B. Yin/SAIC		08/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * S. Gilbert/NCEP	12/05	Changed case when deleting last element *
 *                              to save lat/lon info into new TCA elmnt.*
 * M. Li/SAIC		06/06   added check for break point number	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    float 	*xs, *ys, llx, lly, urx, ury;
    int 	nps, iret, counter, el_loc, ii, jj; 
    VG_DBStruct	el; 
/*----------------------------------------------------------------------*/

    /*
     * remove break point drag box
     */
    pgtca_popdownDragBox ();
    mcanvw_disarmDynamic ();
    mcanvw_setPressFunc ( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);

    _segSelected = FALSE;

    el_loc = pgactv_getElmLoc ();
    cvg_rdrec ( cvg_getworkfile (), el_loc, &el, &iret );
    cvg_freeBkpts( &el );

    pgactv_getDevPts ( &nps, &xs, &ys );
    pgutls_prepNew ( el_loc, &el, &llx, &lly, &urx, &ury, &iret );
    pgundo_newStep ();
    pgundo_storeThisLoc ( el_loc, UNDO_DEL, &iret );

    if ( el.elem.tca.info.wwNum == 1 ) {
       /*
        * Save last breakpoint lat/lon to use for 0 WW text box display
        */
       jj = el.elem.tca.info.tcaww[ 0 ].numBreakPts - 1;
       if ( jj >= 0 ) {
       	   el.elem.tca.info.text_lat = el.elem.tca.info.tcaww[0].breakPnt[jj].lat;
           el.elem.tca.info.text_lon = el.elem.tca.info.tcaww[0].breakPnt[jj].lon + 1.0;
       }
       cvg_freeBkpts( &el );
       pgutls_initHdr( &(el.hdr) );
       el.hdr.vg_type  = TCA_ELM;
       el.hdr.vg_class = CLASS_MET;
       el.elem.tca.info.wwNum = 0;
       cvg_rfrsh ( NULL, llx, lly, urx, ury, &iret );
    }
    else {
       pgutls_initHdr( &(el.hdr) );
       el.hdr.vg_type  = TCA_ELM;
       el.hdr.vg_class = CLASS_MET;
       ces_get ( -99, &el, &iret );

       for ( counter = 0; counter < el.elem.tca.info.tcaww[ index ].numBreakPts; counter++ ) {
           pgtca_delHandleBar ( el.elem.tca.info.tcaww[ index ].breakPnt[ counter ].lat, 
				el.elem.tca.info.tcaww[ index ].breakPnt[ counter ].lon,
				_geoType == NO_TYPE );
       }

       free ( el.elem.tca.info.tcaww[ index ].breakPnt );

       for ( counter = index; counter < el.elem.tca.info.wwNum; counter++ ) {
	   memcpy ( &( el.elem.tca.info.tcaww[ counter ] ),
	            &( el.elem.tca.info.tcaww[ counter  + 1 ]),
	            sizeof( TcaWw_T ) );
       }
       el.elem.tca.info.wwNum--;
    }

       pgvgf_saveNewElm ( NULL, sys_D, &el, nps, xs, ys, TRUE, &el_loc, &iret );
       pgundo_storeThisLoc ( el_loc, UNDO_ADD, &iret );
       pgundo_endStep ();
       cvg_freeBkpts( &el );
       pgutls_redraw ( el_loc, &el, &iret );

       for ( ii=0; ii < el.elem.tca.info.wwNum; ii++ ) {
	   for ( counter = 0; counter < el.elem.tca.info.tcaww[ ii ].numBreakPts; counter++ ) {
               pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		                    el.elem.tca.info.tcaww[ ii ].breakPnt[ counter ].lat,
		                    el.elem.tca.info.tcaww[ ii ].breakPnt[ counter ].lon );
 	   }
       }
       cvg_freeBkpts( &el );
       geplot( &iret );
}
/*====================================================================*/


static void pgtca_setTcaAttr( TcaInfo *tca )
/************************************************************************
 * pgtca_setTcaAttr	                                                *
 *                                                                      *
 * This rountine sets the TCA attributes 				*
 *                                                                      *
 * pgtca_setTcaAttr ( tca )			                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      tca	Tca		tca attributes				*
 *                                                                      *
 * Output parameters:                                                   *
 *      None								*
 *                                                                      *
 * Return parameters:                                                   *
 *	None			                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Changed 'delete' button widget name   	*
 * B. Yin/SAIC   	08/04	Added code to handle water and islands	*
 * B. Yin/SAIC		12/04	Added time zone				*
 * B. Yin/SAIC		04/05	Removed year field, added issue status	*
 * E. Safford/SAIC	04/05	mv seg list dsply to pgtca_addSegment	*
 * B. Yin/SAIC		04/05	Set status accordign to menu strings 	*
 * S. Gilbert/NCEP	01/06	advisoryNum changed from int to char*   *
 ***********************************************************************/
{
   enum   	{ EXP, TEST, OPNL, EXP_OPNL };
   int 		counter, ii, numBkPts, ier, issueStatusIndex[ 4 ];
   char		tmpStr[ 128 ], **breakPts;
   char		severity[ 128 ]="", advisoryType[ 128 ] = "";

/*--------------------------------------------------------------------*/

   if ( tca != NULL ) {
      sprintf( tmpStr, "%d", tca->stormNum );
      XtVaSetValues( _txtStormNum, XmNvalue, tmpStr, NULL );

      XtVaSetValues( _txtStormName, XmNvalue, tca->stormName, NULL );
      XtVaSetValues( _txtTime, XmNvalue, tca->validTime, NULL );
      XtVaSetValues( _txtAdvisoryNum, XmNvalue, tca->advisoryNum, NULL );

      XmListDeleteAllItems( _listBP );

      for ( counter=0; counter < tca->wwNum; counter++ ) {
	  numBkPts = tca->tcaww[ counter ].numBreakPts;
	  strcpy( severity, 
	  	_menuEntryStr[ SEVERITY ][ tca->tcaww[ counter ].severity ] );
	  strcpy( advisoryType, 
	  	_menuEntryStr[ AD_TYPE ][ tca->tcaww[ counter ].advisoryType ] );

          /*
           *  Allocate space for the breakPts array and copy the
           *  breakpoint names into it.
           */
          breakPts = ( char ** ) malloc( numBkPts * sizeof( char *) );
  
          for( ii=0; ii < numBkPts; ii++ ) {
             breakPts[ ii ] = (char *) malloc( 
		(strlen( tca->tcaww[ counter ].breakPnt[ii].breakPtName) + 1 )
       							* sizeof( char ) ); 
             strcpy( breakPts[ ii ], 
	     		tca->tcaww[ counter ].breakPnt[ii].breakPtName );
          }
 
          /* 
           *  Display the segment in the segment list.
           */ 
          pgtca_addSegment( -1, tca->tcaww[ counter ].specialGeog, severity,
	  			advisoryType, numBkPts, breakPts, &ier );
          
          /*
           *  Free breakPts array
           */
          for( ii=0; ii < numBkPts; ii++ ) {
              if( breakPts[ ii ] ) free( breakPts[ ii ] );
          }
          if( breakPts ) free( breakPts );
 
      }
	

      XtVaSetValues( _optMenu[ BASIN ],
		     XmNmenuHistory, _menuButtons[ BASIN ][ tca->basin ],
		     NULL );

      XtVaSetValues( _optMenu[ STORM_TYPE ],
		     XmNmenuHistory, _menuButtons[ STORM_TYPE ][ tca->stormType ],
		     NULL );

      /*
       *  Make a lookup table for issue status menu buttons
       */
      for ( counter = 0; counter < 4; counter++ ) {
          issueStatusIndex[ counter ] = 0;
      }

      for ( counter = 0; counter < _entryNum[ STATUS ]; counter++ ) {
	  if ( strcasecmp ( _menuEntryStr[ STATUS ][ counter ], "experimental" ) == 0 ) 
	     issueStatusIndex[ EXP ] = counter;
	  else if ( strcasecmp ( _menuEntryStr[ STATUS ][ counter ], "test" ) == 0 ) 
	     issueStatusIndex[ TEST ] = counter;
	  else if ( strcasecmp ( _menuEntryStr[ STATUS ][ counter ], "operational" ) == 0 ) 
	     issueStatusIndex[ OPNL ] = counter;
	  else if ( strcasecmp ( _menuEntryStr[ STATUS ][ counter ], "experimental operational" ) == 0 ) 
	     issueStatusIndex[ EXP_OPNL ] = counter;
      }

      /* 
       *  Set issuing status
       */
      switch ( tca->issueStatus ) {
	case TCA_OPERATIONAL:
      		XtVaSetValues( _optMenu[ STATUS ],
		     XmNmenuHistory, _menuButtons[ STATUS ][ issueStatusIndex[ OPNL ] ],
		     NULL );
		break;
	case TCA_TEST:
		XtVaSetValues( _optMenu[ STATUS ],
		     XmNmenuHistory, _menuButtons[ STATUS ][ issueStatusIndex[ TEST ] ],
     		     NULL );
     		break;
	case TCA_EXPERIMENTAL:
		XtVaSetValues( _optMenu[ STATUS ],
		     XmNmenuHistory, _menuButtons[ STATUS ][ issueStatusIndex[ EXP ] ],
     		     NULL );
     		break;
	case TCA_EXPERIMENTAL_OPNL:
		XtVaSetValues( _optMenu[ STATUS ],
		     XmNmenuHistory, _menuButtons[ STATUS ][ issueStatusIndex[ EXP_OPNL ] ],
     		     NULL );
     		break;
	default:
		XtVaSetValues( _optMenu[ STATUS ],
		     XmNmenuHistory, _menuButtons[ STATUS ][ issueStatusIndex[ OPNL ] ],
     		     NULL );
     		break;
      }

      /* 
       *  Set time zone
       */
      if ( !tca->timezone || strlen ( tca->timezone ) == (size_t)0 ) {

	 XtVaSetValues ( _optMenu[ TIME_ZONE ],
	     		 XmNmenuHistory, _menuButtons[ TIME_ZONE ][ 0 ],
			 NULL );

      }
      else {

	 for ( counter = 0; counter < _entryNum[ TIME_ZONE ]; counter++ ) {

	     if ( strcasecmp ( tca->timezone, _menuEntryStr[ TIME_ZONE ][ counter ] ) 
	          == 0 ) break;
	     
	 }

	 if ( counter >= _entryNum[ TIME_ZONE ] ) counter = 0;

	 XtVaSetValues ( _optMenu[ TIME_ZONE ],
	   	         XmNmenuHistory, _menuButtons[ TIME_ZONE ][ counter ],
			 NULL );
      }
					     
      pgtca_setSegmentAttr( NULL );
   }
   else
   {
      XtVaSetValues( _txtStormNum, XmNvalue, "1", NULL );

      XtVaSetValues( _txtStormName, XmNvalue, "", NULL );

      XtVaSetValues( _txtTime, XmNvalue, "", NULL );

      XtVaSetValues( _txtAdvisoryNum, XmNvalue, "1", NULL );

      XtVaSetValues( _optMenu[ BASIN ],
                     XmNmenuHistory, _menuButtons[BASIN][0],
                     NULL );

      XtVaSetValues( _optMenu[ STORM_TYPE ],
                     XmNmenuHistory, _menuButtons[STORM_TYPE][0],
                     NULL );

      XmListDeleteAllItems( _listBP );
      pgtca_setSegmentAttr( NULL );

      XtVaSetValues( _buttonApply,  XmNsensitive, FALSE, NULL );
      XtVaSetValues( _buttonDel,  XmNsensitive, FALSE, NULL );

   }
}
/*====================================================================*/


static void pgtca_setSegmentAttr ( TcaWw_T * tcaWW )
/************************************************************************
 * pgtca_setSegmentAttr                                                 *
 *                                                                      *
 * This rountine sets the segment attributes for second panel		*
 *                                                                      *
 * void pgtca_setSegmentAttr ( tcaWW )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      None								*
 *                                                                      *
 * Output parameters:                                                   *
 *      tcaWW	TcaWw_t		segment attributes                      *
 *                                                                      *
 * Return parameters:                                                   *
 *      None 			                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		07/04	Inactivate/Activate breakpoint type menu*
 *                              Initialize _bpType and _geoType		*
 * B. Yin/SAIC   	08/04	Added code to handle water and islands	*
 ***********************************************************************/
{
   if ( tcaWW != NULL ){
      XtVaSetValues( _optMenu[ SEVERITY ],
	 	     XmNmenuHistory, _menuButtons[SEVERITY][tcaWW->severity ],
			NULL );

      XtVaSetValues( _optMenu[ AD_TYPE ],
		     XmNmenuHistory, _menuButtons[AD_TYPE][ tcaWW->advisoryType ],
		     NULL );

      XtVaSetValues( _optMenu[ BP_TYPE ],
		     XmNmenuHistory, _menuButtons[BP_TYPE][ 0 ],
		     NULL );
      _bpType = OFFICIAL;

      XtVaSetValues( _optMenu[ GEO ],
		     XmNmenuHistory, _menuButtons[GEO][ tcaWW->specialGeog ],
		     NULL );
      if ( tcaWW->specialGeog != NO_TYPE ) {
         XtVaSetValues( XmOptionButtonGadget( _optMenu[ BP_TYPE ] ), 
			XmNsensitive,		False,
			NULL );
         XtVaSetValues( _txtBP1, XmNvalue, "", NULL );
         XtVaSetValues( _txtBP2, XmNvalue, "", NULL );
      }
      else {
         XtVaSetValues ( XmOptionButtonGadget( _optMenu[ BP_TYPE ] ), 
			 XmNsensitive,		True,
			 NULL );
         XtVaSetValues( _txtBP1, XmNvalue, tcaWW->breakPnt[0].breakPtName, NULL );
         XtVaSetValues( _txtBP2, XmNvalue, tcaWW->breakPnt[1].breakPtName, NULL );
      }
      _geoType = tcaWW->specialGeog;
   }
   else {
      XtVaSetValues( _optMenu[ SEVERITY ],
                     XmNmenuHistory, _menuButtons[ SEVERITY ][ 0 ],
                     NULL );

      XtVaSetValues( _optMenu[ AD_TYPE ],
                     XmNmenuHistory, _menuButtons[ AD_TYPE ][ 0 ],
                     NULL );

      XtVaSetValues( _optMenu[ BP_TYPE ],
		     XmNmenuHistory, _menuButtons[BP_TYPE][ 0 ],
		     NULL );
      XtVaSetValues( XmOptionButtonGadget( _optMenu[ BP_TYPE ] ), 
		     XmNsensitive,		True,
		     NULL );
      _bpType = OFFICIAL;

      XtVaSetValues( _optMenu[ GEO ],
                     XmNmenuHistory,_menuButtons[ GEO ][ 0 ] ,
                     NULL );
      _geoType = NO_TYPE;

      XtVaSetValues( _txtBP1, XmNvalue, "", NULL );

      XtVaSetValues( _txtBP2, XmNvalue, "", NULL );
   }
}
/*====================================================================*/


/* ARGSUSED */
static void pgtca_itemSelectedCb ( void )
/************************************************************************
 * pgtca_itemSelectedCb                                                 *
 *                                                                      *
 * Callback when item selected in the list				*
 *                                                                      *
 * void pgtca_itemSelectedCb ( )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Modified code to remove handle bars.   	*
 * B. Yin/SAIC		05/04	Changed 'delete' button widget name   	*
 *                              Changed yellow circle to sky blue   	*
 * B. Yin/SAIC		05/04	Added deselect function		   	*
 * B. Yin/SAIC		07/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * B. Yin/SAIC          08/04   Redraw element after removing handlebars*
 * B. Yin/SAIC          01/05   Redraw element only once		*
 ***********************************************************************/
{
   int          ii, jj, count, iret, el_loc;
   int		*pos = NULL;
   VG_DBStruct	el; 
/*--------------------------------------------------------------------*/

   _newSegment = FALSE;
   _bpSelected = FALSE;

  /*
   * remove break point drag box and the handle bar if exist
   */
   pgtca_popdownDragBox ( );

   if ( _bp1HandleBar ) {
      pgtca_delHandleBar ( _bkpts[0].lat, _bkpts[0].lon, True );
      _bp1HandleBar = FALSE;
   }

   count = 0;
   XmListGetSelectedPos ( _listBP, &pos, &count );

   /*
    * deselect the item if it's selected
    */
   if ( _segSelected && ( _curSegment == pos[ 0 ] ) ) {
      _segSelected = FALSE;
      XmListDeselectPos( _listBP, pos[ 0 ] );
      XtVaSetValues( _buttonApply,  XmNsensitive, FALSE, NULL );
      XtVaSetValues( _buttonDel,  XmNsensitive, FALSE, NULL );
      XtVaSetValues( XmOptionButtonGadget( _optMenu[ GEO ] ), 
	   	     XmNsensitive,		True,
		     NULL );

      el_loc = pgactv_getElmLoc ();
      cvg_rdrec( cvg_getworkfile(), el_loc, &el, &iret );

      for ( ii = 0; ii < el.elem.tca.info.tcaww[ _curSegment - 1 ].numBreakPts; ii++ ) {
             pgtca_delHandleBar ( el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lat, 
		    	          el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lon,
				  ii == el.elem.tca.info.tcaww[ _curSegment - 1 ].numBreakPts - 1 );
      }

      for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++ ) {
	  for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) { 
              pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		   el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat,
		   el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	  }
      }
      geplot( &iret );
      pgtca_setSegmentAttr( NULL );

      mcanvw_disarmDynamic ();
      mcanvw_disarmDrag ();
      pgtca_popdownDragBox ();
      mcanvw_setPressFunc ( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);

      cvg_freeBkpts( &el );
      return;
   }

   el_loc = pgactv_getElmLoc ();
   cvg_rdrec( cvg_getworkfile(), el_loc, &el, &iret );

   /*
    * Remove handle bars of the previous selected segment
    */
   if ( _segSelected ) {
      for ( ii = 0; ii < el.elem.tca.info.tcaww[ _curSegment - 1 ].numBreakPts; ii++ ) {
             pgtca_delHandleBar ( el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lat, 
		    	          el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lon,
				  ii == el.elem.tca.info.tcaww[ _curSegment - 1 ].numBreakPts - 1 );
      }
   }

   /*
    * Load the selected segment attributes to the second panel
    */
   _curSegment = pos[ 0 ];
   pgtca_setSegmentAttr( &(el.elem.tca.info.tcaww[pos[0]-1]) );

   /*
    * Redraw handle bars
    */
   _numBkpts = el.elem.tca.info.tcaww[ _curSegment - 1 ].numBreakPts;

   while ( _numBkpts > _capacity ) {
         _capacity += BKPT_INC;
   }

   if ( !( _bkpts = realloc( _bkpts, _capacity * sizeof( Breakpt_T ) ) ))
      return;

   for ( ii = 0; ii < _numBkpts; ii++ ) {
       strcpy ( _bkpts[ ii ].breakPtName, 
		el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].breakPtName );
       _bkpts[ ii ].lat = el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lat;
       _bkpts[ ii ].lon = el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lon;
             pgtca_delHandleBar ( el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lat, 
		    	          el.elem.tca.info.tcaww[ _curSegment - 1 ].breakPnt[ ii ].lon,
				  ii == _numBkpts - 1 );
   }

   for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++) {
       if ( ii == _curSegment -1 ){
          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
              pgtca_putHandleBar ( WHITE, DOT, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	  }
       }
       else {
          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
              pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	  }
       }
   }
   geplot( &iret );
     
   if ( el.elem.tca.info.tcaww[ _curSegment - 1 ].specialGeog != NO_TYPE ) {
      mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);
      pgasel_start ( &pgtca_singlePtSel,
      	             &pgtca_multiPtSel,
                     &pgtca_endMultiPtSel );
      
   } 
   else {
      mcanvw_disarmDynamic ();
      mcanvw_disarmDrag ();
      pgtca_popdownDragBox ();
      mcanvw_setPressFunc ( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);
   }

   _segSelected = TRUE;
   XtVaSetValues( _buttonApply,  XmNsensitive, TRUE, NULL );
   XtVaSetValues( _buttonDel,  XmNsensitive, TRUE, NULL );
   XtVaSetValues( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		  XmNsensitive,		False,
		  NULL );

   if ( pos != NULL ) {
      free ( pos);
   }

   cvg_freeBkpts( &el );
}
/*=====================================================================*/


static void pgtca_updateSegment ( void )
/************************************************************************
 * pgtca_updateSegment                                                  *
 *                                                                      *
 * This rountine updates selected segment attributes from panel 2	*
 *                                                                      *
 * void pgtca_updateSegment ( )			                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Changed yellow circle to sky blue   	*
 * B. Yin/SAIC		08/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * H. Zeng/SAIC		02/05	set the sensitivity of _createText	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
   float 	*xs, *ys, llx, lly, urx, ury;
   int	 	nps, iret, ii, jj, counter, el_loc, num, el_layer;
   int		ier;
   char		*tmpStr;
   char		severity[ 128 ] ="", advisoryType[ 128 ]="";
   char		**breakPts;

   VG_DBStruct	el; 
   XmString 	xmStr;
   Widget	wID;
   filter_t	filter;
/*---------------------------------------------------------------------*/

    /* 
     *	Get severity and advisory type
     */
    for ( counter = SEVERITY; counter <= AD_TYPE ; counter++ ) {

        XtVaGetValues ( _pulldownMenu[counter],
		       XmNmenuHistory,		&wID,
		       NULL );

        XtVaGetValues ( wID,
		       XmNlabelString,		&xmStr,
		       NULL );

        XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

        if ( counter == SEVERITY )
  	   strcpy( severity, tmpStr );
        else
	   strcpy( advisoryType, tmpStr );

        XtFree( tmpStr );
    }

    /*
     *  Allocate space for the breakPts array and copy the
     *  breakpoint names into it.
     */
    breakPts = ( char ** ) malloc( _numBkpts * sizeof( char *) );
  
    for( ii=0; ii < _numBkpts; ii++ ) {
       breakPts[ ii ] = (char *) malloc( (strlen( _bkpts[ii].breakPtName) + 1 ) 
       							* sizeof( char ) ); 
       strcpy( breakPts[ ii ], _bkpts[ii].breakPtName );
    }
    
    /* 
     *  Display the segment in the segment list.
     */ 
    pgtca_addSegment( _curSegment, _geoType, severity, advisoryType, _numBkpts,
    			breakPts, &ier );

    /*
     *  Free breakPts array
     */
    for( ii=0; ii < _numBkpts; ii++ ) {
        if( breakPts[ ii ] ) free( breakPts[ ii ] );
    }
    if( breakPts ) free( breakPts );
  

   /*
    * remove break point drag box
    */
   pgtca_popdownDragBox( );

   /* 
    * Write the new segment to the tca element 
    */
   el_loc = pgactv_getElmLoc ();
   cvg_rdrec ( cvg_getworkfile(), el_loc, &el, &iret );
   cvg_freeBkpts( &el );

   pgutls_prepNew ( el_loc, &el, &llx, &lly, &urx, &ury, &iret );
   pgundo_newStep ();
   pgundo_storeThisLoc ( el_loc, UNDO_DEL, &iret );

   pgactv_getDevPts ( &nps, &xs, &ys );
   pgtca_getSegmentAttr ( &(el.elem.tca.info.tcaww[ _curSegment - 1 ] ) );
   pgvgf_saveNewElm ( NULL, sys_D, &el, nps, xs, ys, TRUE, &el_loc, &iret );
   pgundo_storeThisLoc ( el_loc, UNDO_ADD, &iret );
   pgundo_endStep ();

   el_layer = pglayer_getCurLayer( );
   crg_set( &el, el_loc, el_layer, &iret);
   pgactv_setActvElm( &el, el_loc );
   crg_getinx( el_loc, &num, &iret );
   crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret );

   cvg_rfrsh( NULL, llx, lly, urx, ury, &iret );

   /*
    * Redraw handle bars
    */
   for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++) {
       if ( ii == _curSegment -1 ){
          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
              pgtca_putHandleBar ( WHITE, DOT, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	  }
       }
       else {
          for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
              pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	  }
       }
   }
   geplot( &iret );

   XtVaSetValues( _buttonBtm[0], XmNsensitive, TRUE, NULL );
   XtVaSetValues( _createText,   XmNsensitive, FALSE, NULL );
   XmStringFree ( xmStr );
   cvg_freeBkpts( &el );

}

/*=====================================================================*/

/* ARGSUSED */
static void pgtca_enableApplyCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_enableApplyCb							*
 *                                                                      *
 * This is the callback routine to enable the Create Text button	*
 *                                                                      *
 * void pgtca_enableApplyCb ( wid, clnt, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  	wid		Widget		widget ID			*
 *  	clnt		XtPointer	client data (not used)		*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * H. Zeng/SAIC		01/05	added setting of _createText btn	*
 ***********************************************************************/
{
   int itemCount = 0;
/*----------------------------------------------------------------------*/

   XtVaGetValues ( _listBP, XmNitemCount, &itemCount, NULL );
   if ( itemCount != 0 ) {
      XtVaSetValues ( _buttonBtm[0], XmNsensitive, TRUE, NULL );
      XtVaSetValues ( _createText,   XmNsensitive, FALSE, NULL );
      XtVaSetValues ( _buttonApply , XmNsensitive, TRUE, NULL );
   }
}
/*====================================================================*/

static void pgtca_enableCancelCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_enableCancelCb							*
 *                                                                      *
 * This is the callback routine to enable the Cancel All button	        *
 *                                                                      *
 * void pgtca_enableCancelCb ( )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  	wid		Widget		widget ID			*
 *  	clnt		XtPointer	client data (not used)		*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP		04/04	Created			   	*
 * S. Gilbert/NCEP		01/06	Modified to recogize char       *
 *                                      advisory format.                *
 ***********************************************************************/
{
   int         advisoryNum = 0, iflag, ier;
   char        *tmpStr;
   int         itemCount;
/*---------------------------------------------------------------------*/
                                                                                
   /*
    * Get num of breakpoints in list
    */
   XtVaGetValues( _listBP, XmNitemCount, &itemCount, NULL );

   /*
    * Get advisory num
    */
   XtVaGetValues( _txtAdvisoryNum, XmNvalue, &tmpStr, NULL );
   gh_advn ( tmpStr, &advisoryNum, &iflag, &ier );
   XtFree( tmpStr );

   if ( ier != 0 ) {
      XtVaSetValues ( _cancelAll,   XmNsensitive, FALSE, NULL );
      return;
   }

   if ( advisoryNum > 1 && itemCount > 0 ) {
      XtVaSetValues ( _cancelAll,   XmNsensitive, TRUE, NULL );
   }
   else {
      XtVaSetValues ( _cancelAll,   XmNsensitive, FALSE, NULL );
   }
}
/*====================================================================*/

static void pgtca_createTxt ( void ) 
/************************************************************************
 * pgtca_createTxt         		                                *
 *                                                                      *
 * This routine brings up the save vgf confirmation dialog		*
 *                                                                      *
 * void pgtca_createTxt ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Added code to check storm name and time *
 * B. Yin/SAIC		05/04	Check storm name/time seperately	*
 * B. Yin/SAIC		08/04	Added code to generate text message	*
 * H. Zeng/SAIC		02/05	moved warning window popup into a func. *
 * E. Safford/SAIC	02/05	save off txt message if it exists	*
 * S. Gilbert/NCEP	10/07	Changed txtFile name to depend on basin *
 * m.gamazaychikov/SAIC	12/07	Add code to generate name for CP basin  *
 * m.gamazaychikov/SAIC	04/08	Add code to append work_dir to file name*
 * m.gamazaychikov/SAIC	04/08	Fix problem with name of text file name	*
 * X. Guo/CWS           04/11   Add code to handle text dialog window   *
 *                              switch between PHFO and others          *
 ***********************************************************************/
{
   int 		stormNum, ier, ii, jj,location=-1,iret;
   char 	txtFile[FILE_FULLSZ],outText[MSG_SIZE],*tmpStr,basin[3], 
                rspoff[5], txtFile2[11];
   XmString     xmStr;
/*--------------------------------------------------------------------*/

   /*
    * First check if there are values in some text widget boxes.
    * If there is a warning window popping up, stop here.
    */
   if ( pgtca_checkInfo () ) return;
   
  /*
   * Retrieve the responsible office.
   */
   pgtca_getBasinId ( basin );
   ctb_rdprf ( "prefs.tbl", "config", "TCV_RSP_OFFICE", rspoff, &ier );
   if ( ( ier == 0 ) && ( strncmp( rspoff, "PHFO", 4 ) == 0 ) && 
        (strncmp( basin, "ep", 2 ) == 0 || strncmp( basin, "cp", 2 ) == 0) ) {
       if ( ! _isPHFO_Office && ( _tcaSvWin != NULL )) {
           XtUnmanageChild ( _rowcolName );
           XtManageChild ( _optMenu[PHFO] );
       }
       _isPHFO_Office = TRUE;
   }
   else {
       if (  _isPHFO_Office && ( _tcaSvWin != NULL )) {
           XtUnmanageChild (  _optMenu[PHFO] );
           XtManageChild (_rowcolName);
       }
       _isPHFO_Office = FALSE;
   }

   if ( _tcaSvWin == NULL )
      pgtca_createPopSv ( _tcaAttribW );
   else
      XtManageChild ( _tcaSvWin );

   XtVaGetValues( _txtStormNum, XmNvalue, &tmpStr, NULL );

   stormNum = atoi( tmpStr ) % 5;
   stormNum = ( stormNum == 0 ) ? 5 : stormNum;

  /*
   * Construct text file name.
   */
   if ( ( ier != 0 ) ||
         ( ( strncmp( rspoff, "KNHC", 4 ) != 0 ) && 
           ( strncmp( rspoff, "PHFO", 4 ) != 0 )  ) ) {
       /*
        * Old naming convention - if 'TCV_RSP_OFFICE' tag is missing
        * or set to improper value
        */ 
       sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
       if ( strncmp( basin, "ep", 2 ) == 0 )
         sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "EP", stormNum );
       if ( strncmp( basin, "cp", 2 ) == 0 )
         sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAMEHI, "CP", stormNum );
   }
   else {
       /*
        * New naming convention - if 'TCV_RSP_OFFICE' tag is present
        */
      if ( strncmp( rspoff, "PHFO", 4 ) == 0 ) {
         /* PHFO only uses CP for "ep" and "cp" */          
         if ( strncmp( basin, "ep", 2 ) == 0 ||
              strncmp( basin, "cp", 2 ) == 0 ) {
             sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAMEHI, "CP", stormNum );
             sprintf ( _curTextFile, "%s%2s%d", TXT_NAMEHI, "CP", stormNum );
             strcpy ( _selectedTextFile , _curTextFile );
             pgtca_findPHFOTextFile ( _curTextFile, &location, &iret );
             if ( iret < 0 ) {
                location = -1;
             }
          }
          else if ( strncmp( basin, "al", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
      }
      else if ( strncmp( rspoff, "KNHC", 4 ) == 0 ) {
          if ( strncmp( basin, "ep", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "EP", stormNum );
          if ( strncmp( basin, "cp", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "CP", stormNum );
          if ( strncmp( basin, "al", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
      }
   }
   if( access( txtFile, F_OK ) == 0 ) {
       rename( txtFile, TEMP_TEXT );
   }

   jj = 0;
   for (ii = (int)strlen(_tcvWorkDir); ii < (int)strlen(txtFile); ii++) {
        txtFile2[jj] = txtFile[ii];
        jj++;
   }
   txtFile2[jj] = '\0';

   xmStr = XmStringCreateLtoR ( txtFile2, XmSTRING_DEFAULT_CHARSET );
   if ( (strncmp( rspoff, "PHFO", 4 ) == 0) && ( location >=0 )) {
       XtVaSetValues( _optMenu[ PHFO ],
                     XmNmenuHistory, _menuButtons[ PHFO ][ location ],
                     NULL );

   }
   else {
       XtVaSetValues ( _txtFileName,
                   XmNlabelString,        xmStr,
                   NULL );
   }

   pgtca_makeTextMsg ( outText );
   XtVaSetValues ( _txtMsg,
                   XmNvalue,        outText,
                   NULL );

   XtFree( tmpStr );
   XmStringFree( xmStr );

}
/*=====================================================================*/
static void pgtca_findPHFOTextFile ( char * filename, int *location, int *iret )
/************************************************************************
 * pgtca_findPHFOTextFile                                               *
 *                                                                      *
 * This function will find PHFO text file location.                     *
 *                                                                      *
 * void pgtca_findPHFOTextFile ( filename,location, iret )              *
 *                                                                      *
 * Input parameters:                                                    *
 *      filename           char *      text file name                   *
 *                                                                      *
 * Output parameters:                                                   *
 *      location           int *       text file location               *
 *      *iret           int             Return value                    *
 *                                       0: normal return               *
 *                                      -1: no data                     *
 * Return parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS          04/11    Created                                 *
 ***********************************************************************/
{
      int counter;
      *iret = 0;

      for ( counter = 0; counter < _entryNum[ PHFO ]; counter++ ) {
          if ( strcasecmp ( _menuEntryStr[ PHFO ][ counter ], filename ) == 0 ) {
             *location = counter;
             return;
          }
      }
     *iret = -1;
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_selTextFileCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_selTextFileCb                                                  *
 *                                                                      *
 * This is the callback routine to process selected text file for PHFO  *
 *                                                                      *
 * void pgtca_selTextFileCb ( wid, clnt, call )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      clnt            XtPointer       client data (not used)          *
 *      call            XtPointer       callback struct                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS          04/11    Created                                 *
 ***********************************************************************/
{
    XmString    xmStr;
    Widget      wID;
    char         *tmpStr,*txtOutStr,*p;
    char        hdln[10];
    int         location,iret;
/*-------------------------------------------------------------------*/

    XtVaGetValues( _pulldownMenu[PHFO],
                    XmNmenuHistory,                 &wID,
                    NULL );

    XtVaGetValues(  wID,
                    XmNlabelString,                 &xmStr,
                    NULL );

   XmStringGetLtoR(xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr);

   if ( tmpStr == NULL ) {
       XmStringFree( xmStr ); 
       return;
   }

   if ( strcasecmp ( _selectedTextFile, tmpStr ) == 0 ) {
        XtFree( tmpStr);
         XmStringFree( xmStr );
        return;
   }
   strcpy ( _selectedTextFile, tmpStr );
   pgtca_findPHFOTextFile ( tmpStr, &location, &iret );
   XtFree( tmpStr);
    XmStringFree( xmStr );
   if ( iret < 0 ) return;
   
   XtVaGetValues ( _txtMsg, XmNvalue, &txtOutStr, NULL );
  if ( txtOutStr != NULL ) {
     sprintf (hdln,"%s%d", PHFO_HDR1, location+1);
     if ( (p = strstr(txtOutStr,PHFO_HDR1 )) != NULL ) {
         memcpy ( p, hdln, strlen (hdln));
     }
     sprintf (hdln,"%s%d", PHFO_HDR2, location+1);
     if ( (p = strstr(txtOutStr,PHFO_HDR2 )) != NULL ) {
         memcpy ( p, hdln, strlen (hdln));
     }
     XtVaSetValues ( _txtMsg, XmNvalue, txtOutStr, NULL );
  }
  XtFree( txtOutStr );
}

/*====================================================================*/

/* ARGSUSED */
static void pgtca_svCtlBtnCb ( Widget wid, long  which, XtPointer call )
/************************************************************************
 * pgtca_svCtlBtnCb                                                     *
 *                                                                      *
 * Callback function for control buttons at the bottom of vgf file save *
 * popup window.                                                        *
 *                                                                      *
 * void pgtca_svCtlBtnCb ( wid, which, call )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      which           long            which button                    *
 *      call            XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Modified to get record size dynamically *
 * B. Yin/SAIC		05/04	Draw handle bars after saving vg file	*
 * B. Yin/SAIC		07/04	Added code to handle water and islands	*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * B. Yin/SAIC          08/04   Modified code to create text message	*
 * H. Zeng/SAIC		01/05	removed restoring VGF file part		*
 * E. Safford/SAIC	02/05	rm/restore prev text file & make create *
 *				 txt button insensitive after save 	*
 * X. Guo/CWS           04/11   Add code to save/cancel PHFO text file  *
 ***********************************************************************/
{
   char         *tmpStr;
   char         txtFile[FILE_FULLSZ];
   XmString     xmStr;
   int iret;
/*---------------------------------------------------------------------*/

   switch ( which ) {

     case 0:     /* Save */

         if( access( TEMP_TEXT, F_OK ) == 0 ) {
	     remove( TEMP_TEXT );
	 }
         XtVaSetValues ( _createText,  XmNsensitive, FALSE,  NULL );
         if ( _isPHFO_Office ) {
             if ( strcmp ( _curTextFile, _selectedTextFile ) != 0 ) {
                 sprintf( txtFile, "%s%s", _tcvWorkDir,_curTextFile );
                 pgtca_writeTextMsg ( &iret );
                 if ( iret == 0 ) {
                     remove ( txtFile );
                 }
                 else {
                     sprintf( txtFile, "%s%s", _tcvWorkDir,_selectedTextFile );
                     if ( access( txtFile, F_OK ) == 0 ) {
                        remove( txtFile );
                     }
                 }
             }
         }
          break;

     case 1:	/* Cancel */
 
          /*
           * Remove saved TCA text message file.
           */
         if ( _isPHFO_Office ) {
             sprintf( txtFile, "%s%s", _tcvWorkDir,_curTextFile );
             remove ( txtFile );
             if( access( TEMP_TEXT, F_OK ) == 0 )
                 rename( TEMP_TEXT, txtFile );
         }
         else {
             XtVaGetValues ( _txtFileName,
                         XmNlabelString,        &xmStr,
                         NULL );
             XmStringGetLtoR ( xmStr, XmSTRING_DEFAULT_CHARSET, &tmpStr );
             remove ( tmpStr );
             if( access( TEMP_TEXT, F_OK ) == 0 )
                 rename( TEMP_TEXT, tmpStr );

             XtFree( tmpStr );
             XmStringFree( xmStr );
         }

          break;
    }

    XtUnmanageChild ( _tcaSvWin );

}
/*=====================================================================*/

static void pgtca_makeTcaVgFile ( Boolean vgverify ) 
/************************************************************************
 * pgtca_makeTcaVgFile         		                                *
 *                                                                      *
 * This routine creates a proper vg file name and saves the underlining *
 * TCA element into this vg file.					*
 *                                                                      *
 * void pgtca_makeTcaVgFile ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      vgverify	Boolean		If true, prompt user if OK to   *
 *                                      overwrite existing vgfile.      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		01/05	initial coding				*
 * B. Yin/SAIC		04/05   removed year field, get year from time	*
 * S. Gilbert/NCEP	01/06   Handle intermediate advisories          *
 * S. Gilbert/NCEP	03/06	Added Boolean arg vgverify              *
 * m.gamazaychikov/SAIC	04/08	Add code to append work_dir to file name*
 ***********************************************************************/
{
    int 	stormNum, advisoryNum, iflag, ier, iret;
    long	flen;
    char        *tmpStr, mesg[512], file_nm[64], basin[ 3 ];
/*---------------------------------------------------------------------*/

    /*
     * First check if there are values in some text widget boxes.
     * If there is a warning window popping up, stop here.
     */
    if ( pgtca_checkInfo () ) return;

 
    /* 
     * create vgf file name.
     */
    pgtca_getBasinId ( basin );

    XtVaGetValues( _txtStormNum, XmNvalue, &tmpStr, NULL );
    stormNum = atoi( tmpStr );
    XtFree( tmpStr );

    XtVaGetValues( _txtAdvisoryNum, XmNvalue, &tmpStr, NULL );
    gh_advn ( tmpStr, &advisoryNum, &iflag, &ier );

    XtFree( tmpStr );

    XtVaGetValues( _txtTime, XmNvalue, &tmpStr, NULL );

    if ( iflag == 0 )
        sprintf( _vgFileName, "%stca_%2s%02d20%2.2s_%03d.vgf", 
	          _tcvWorkDir, basin, stormNum, tmpStr, advisoryNum );
    else if ( iflag == 1 )
        sprintf( _vgFileName, "%stca_%2s%02d20%2.2s_%03da.vgf", 
	          _tcvWorkDir, basin, stormNum, tmpStr, advisoryNum );
    else if ( iflag == 2 )
        sprintf( _vgFileName, "%stca_%2s%02d20%2.2s_%03db.vgf", 
	          _tcvWorkDir, basin, stormNum, tmpStr, advisoryNum );

    XtFree( tmpStr );

    /*
     * if the vg file exists, pop up a confirming window.
     */
    cfl_inqr ( _vgFileName, NULL, &flen, file_nm, &iret ); 

    if ( iret == 0  && vgverify ) {

       sprintf ( mesg, "VG file %s already exists. Overwrite?",
		       _vgFileName );
       NxmConfirm_show ( _tcaAttribW, mesg, pgtca_confirmCb, NULL, 
                         NULL, &ier );

       return;
         }


    /*
     * Save TCA element into vg file.
     */
    pgtca_saveVgFile ( _vgFileName, &iret );

}
 
/*====================================================================*/

static void pgtca_saveVgFile ( char* fileName, int* iret ) 
/************************************************************************
 * pgtca_saveVgFile         		                                *
 *                                                                      *
 * This routine saves the corrently selected TCA element into vg file.	*
 *                                                                      *
 * void pgtca_saveVgFile ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	fileName	char*	vg file name				*
 * Output parameters:                                                   *
 *	iret		int*	return code				*
 *					=0	NORMAL			*
 *				       !=0	ERROR			*
 * Return parameters:                                                   *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		01/05	initial coding				*
 * S. Danz/AWC		07/06	Update to new cvg_writef() parameter    *
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int 	ier;
    int 	nps, recsize, el_loc, ii, jj, num, el_layer;
    float 	*xs, *ys, llx, lly,urx, ury;
    char        *ptca;
    filter_t	filter;
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Save TCA element into vg file.
     */
    el_loc = pgactv_getElmLoc ();
    cvg_rdrec ( cvg_getworkfile(), el_loc, &el, &ier );
    cvg_freeBkpts( &el );

    pgactv_getDevPts ( &nps, &xs, &ys );
    pgutls_prepNew ( el_loc, &el, &llx, &lly, &urx, &ury, &ier );

    pgtca_getTcaAttr ( &(el.elem.tca.info) );
    recsize = (int) ( sizeof(VG_HdrStruct) + 
	      cvg_loadTcaFileInfo(el.elem.tca.info, &ptca, &ier) );
    free( ptca );
    el.hdr.recsz = recsize;
	
    cvg_crvgf ( fileName, &ier );
    cvg_writef ( &el, -1, recsize, fileName, FALSE, &el_loc, &ier );

    pgvgf_saveNewElm ( NULL, sys_D, &el, nps, xs, ys, TRUE, &el_loc, &ier );

    /* 
     * Redraw elements
     */
    pgactv_setActvElm ( &el, el_loc );
    crg_set ( &el, el_loc, 0, &ier );
    crg_getinx( el_loc, &num, &ier );
    crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier );

    cvg_rfrsh( NULL, llx, lly, urx, ury, &ier );

    /*
     * Redraw handle bars
     */
    if ( _segSelected ) {
       for ( ii = 0; ii < _numBkpts; ii++ ) {
           pgtca_delHandleBar ( _bkpts[ ii ].lat, _bkpts[ ii ].lon, ii == _numBkpts - 1 );
       }
    }

    for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++) {
        if ( _segSelected && ( ii == _curSegment -1 ) ){
           for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
               pgtca_putHandleBar ( WHITE, DOT, 
                     el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		     el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
           }
        }
        else {
           for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
               pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
	             el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		     el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	   }
        }
    }
    geplot( &ier );

    cvg_freeBkpts( &el );

    /*
     * Set the sensitivity of bottom buttons.
     */
    XtVaSetValues ( _buttonBtm[0], XmNsensitive, FALSE, NULL );
    XtVaSetValues ( _createText,   XmNsensitive, TRUE,  NULL );

}

/*====================================================================*/

/* ARGSUSED */
static void pgtca_confirmCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_confirmCb							*
 *									*
 * Callback function for confirm popup when vgf file  already exists.	*
 *									*
 * void pgtca_confirmCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/05	initial coding				*
 ************************************************************************/
{
    int iret;
/*---------------------------------------------------------------------*/
/*
 * Save TCA element into vg file.
 */
    pgtca_saveVgFile ( _vgFileName, &iret );
  
}

/*=====================================================================*/

static void pgtca_confirmCancelCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_confirmCancelCb                                                *
 *                                                                      *
 * Callback function for confirm popup when requesting "Cancel All"     *
 * watches and warnings TCV message                                     *
 *                                                                      *
 * void pgtca_confirmCancelCb (wid, clnt, call)                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      clnt    XtPointer       not used                                *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP              10/05   initial coding                  *
 * S. Gilbert/NCEP	03/06	Added Boolean arg to pgtca_makeTcaVgFile*
 ************************************************************************/
{
    int         ii;
    int         itemCount;
/*---------------------------------------------------------------------*/
                                                                                
    /*
     * Delete each watch/warning
     */
    XtVaGetValues( _listBP, XmNitemCount, &itemCount, NULL );
    for ( ii=0; ii< itemCount;ii++) {
       XmListSelectPos( _listBP, 0, False );
       pgtca_delItem ();
    }
                                                                                
    /*
     *  Save into vg file.
     */
    pgtca_makeTcaVgFile ( False );
                                                                                
    /*
     * Create "Cancel All" TCV message
     */
    pgtca_createTxt ();
    XtVaSetValues( _buttonBtm[0],   XmNsensitive, FALSE, NULL );
    XtVaSetValues( _createText,   XmNsensitive, FALSE, NULL );
    XtVaSetValues( _cancelAll,   XmNsensitive, FALSE, NULL );
                                                                                
}

/*=====================================================================*/

static void pgtca_createPopSv ( Widget parent )
/************************************************************************
 * pgtca_createPopSv                                                    *
 *                                                                      *
 * This routine creates the vgf file save popup window.                 *
 *                                                                      *
 * void pgtca_createPopSv ( parent )             	                *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent             Widget          parent widget ID             *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/04	Created				   	*
 * B. Yin/SAIC		05/04	Reduced the width of the file name field*
 * B. Yin/SAIC		08/04	Added the TCA text message dialog	*
 * B. Yin/SAIC		12/04	Fixed compile warning on IRIX6		*
 * X. Guo/CWS           04/11   Add PHFO text file name dialog box      *
 ***********************************************************************/
{
   int    	btn, nn;
   int         col1_x    = 9, rowHeight = 35;
   char   	*btnstrs[] = { "Save", "Cancel" };
   char		title[] = "TCA Text Message";
   Widget 	pane;
   Arg		args[ 10 ];
   XmFontList	fontlist;
   XmFontListEntry entry;
/*---------------------------------------------------------------------*/

   /*
    * create dialog form
    */
    _tcaSvWin = XmCreateFormDialog ( parent, "pgtca_popupSave",
                                     NULL, 0);
    XtVaSetValues ( _tcaSvWin,
		    XmNdefaultPosition, False,
		    XmNx,		200,
		    XmNy,		200,
                    XmNnoResize,        True,
		    XmNdialogStyle, 	XmDIALOG_PRIMARY_APPLICATION_MODAL,
                    NULL );

    XtVaSetValues ( XtParent ( _tcaSvWin ),
		    XmNmwmFunctions,	4,
                    XmNtitle, 		title,
                    NULL );

   /*
    * create a parent pane widget
    */
    pane = XtVaCreateManagedWidget ( "pgtca_svpane",
                        xmPanedWindowWidgetClass, _tcaSvWin,
                        XmNsashWidth,             1,
                        XmNsashHeight,            1,
                        NULL );

   /*
    * create text widget
    */

    entry = XmFontListEntryLoad ( XtDisplay( _tcaAttribW ), FIXED_FONT, XmFONT_IS_FONT, "tag1");
    fontlist = XmFontListAppendEntry(NULL, entry);
    XmFontListEntryFree(&entry);

    nn = 0;
    XtSetArg(args[nn], XmNscrollingPolicy,           XmAUTOMATIC ); nn++;
    XtSetArg(args[nn], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); nn++;
    XtSetArg(args[nn], XmNcursorPositionVisible,     False       ); nn++; 
    XtSetArg(args[nn], XmNeditable,                  False       ); nn++; 
    XtSetArg(args[nn], XmNcolumns,                   90          ); nn++; 
    XtSetArg(args[nn], XmNrows,                      25          ); nn++; 
    XtSetArg(args[nn], XmNfontList,                  fontlist    ); nn++; 
    XtSetArg(args[nn], XmNeditMode,                  XmMULTI_LINE_EDIT ); nn++; 

   _txtMsg = XmCreateScrolledText( pane, "pgtca_txt", args, nn );
    XmFontListFree( fontlist );   

   /*
    * file name area
    */
    pgtca_createOptMenu( pane, "Text File Name:  ", PHFO, col1_x, 26 * rowHeight );
    XtAddCallback( _pulldownMenu[PHFO], XmNentryCallback,
                       (XtCallbackProc)pgtca_selTextFileCb, NULL );

    XtVaSetValues( _menuButtons[ PHFO ][ 0 ],
                       XmNwidth, 120,
                       NULL );
    _rowcolName = XtVaCreateManagedWidget ("txtnameRowCol",
                    xmRowColumnWidgetClass,         pane,
                    XmNorientation,                 XmHORIZONTAL,
                    NULL );
    XtVaCreateManagedWidget( "Text File Name:   ",
                    	    xmLabelWidgetClass, 		_rowcolName,
                    	    NULL );
   if ( _isPHFO_Office ) {
       _txtFileName = _menuButtons[ PHFO ][ 0 ];
       XtUnmanageChild ( _rowcolName );     
   }
   else {
        _txtFileName = XtVaCreateManagedWidget( "txtName",
                            xmLabelWidgetClass,         _rowcolName,
                            NULL );
       XtUnmanageChild ( _optMenu[PHFO] );
   }
   /*
    * create control buttons
    */
   btn = XtNumber ( btnstrs );
   NxmCtlBtn_create ( pane, 1, "pgtca_svctlBtn", btn,
                      btnstrs, (XtCallbackProc)pgtca_svCtlBtnCb, NULL);

   XtManageChild ( _txtMsg );
   XtManageChild ( _tcaSvWin );

}
/*=====================================================================*/


static int pgtca_getMenuInfo ( void )
/************************************************************************
 * pgtca_getMenuInfo							*
 *									*
 * This function is to get menu entries from the tca info table		*
 *									*
 * int pgtca_getMenuInfo ( ) 						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *  	None								*
 *									*
 * Return parameters:							*
 *			int		number of entries or		*
 *					 0:	normal return		*
 *					-1:	not enough memory 	*
 **									*
 * Log:									*
 * B. Yin/SAIC		03/04	Created					*
 * B. Yin/SAIC		05/04	Modified code to allocate _menuEntryStr.*
 *                              Changed the order of the basin menu.	*
 * B. Yin/SAIC   	12/04	Added time zone menu			*
 * B. Yin/SAIC   	 1/05	Added PST/PDT into time zone menu	*
 * B. Yin/SAIC		 4/05	Changed the last entry of issue status	*
 * B. Yin/SAIC		 4/05	Changed the order of issue status	*
 * X. Guo/CWS           04/11   Add PHFO menu                           *
 * S. Jacobs/NCEP	 3/13	Added Post-Tropical Cyclone		*
 * M. Onderlinde/NHC     9/16   Added Potential Tropical Cyclone        *
 ***********************************************************************/
{
    FILE 	*tblFile = NULL;
    int  	iret, row; 
/*---------------------------------------------------------------------*/

    /*
     * Allocate memory for the menu entry strings
     */
    if ( ( _menuEntryStr = (char ***) malloc ( NUM_OPTION_MENU*sizeof( char ** ) ) ) 
			== NULL ) return -1;

    /*
     * Open the tca info table file
     */
    if ( ( tblFile = cfl_tbop ( TCAINFO_TBL, "pgen", &iret ) ) != NULL ) {
       cfl_tbnr ( tblFile, &row, &iret );
    }

    /*
     * Read each menu from the table 
     * Set default values if no table or no data 
     */
    if ( tblFile == NULL || row == 0 || 
         ( _entryNum[ STATUS ] = pgtca_getMenuEntries ( tblFile, row, 
                   "IssuingStatus", &_menuEntryStr[ STATUS ] ) ) <= 0 ) {

       	_entryNum[ STATUS ] = 4;
	if ( pgtca_allocEntryStr( _entryNum[ STATUS ], 
			&_menuEntryStr[ STATUS ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ STATUS ][ 0 ], "Experimental" );
        strcpy( _menuEntryStr[ STATUS ][ 1 ], "Test" );
        strcpy( _menuEntryStr[ STATUS ][ 2 ], "Operational" );
        strcpy( _menuEntryStr[ STATUS ][ 3 ], "Experimental Operational" );
    }


    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ STORM_TYPE ] = pgtca_getMenuEntries ( tblFile, row, 
                "StormType", &_menuEntryStr[ STORM_TYPE ] )) <= 0 ) {

       	_entryNum[ STORM_TYPE ] = 7;

	if ( pgtca_allocEntryStr( _entryNum[ STORM_TYPE ], 
			&_menuEntryStr[ STORM_TYPE ]) != 0) return -1;

        strcpy( _menuEntryStr[ STORM_TYPE ][ 0 ], "Hurricane" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 1 ], "Tropical Storm" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 2 ], "Tropical Depression" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 3 ], "Subtropical Storm" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 4 ], "Subtropical Depression" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 5 ], "Post-Tropical Cyclone" );
        strcpy( _menuEntryStr[ STORM_TYPE ][ 6 ], "Potential-Tropical Cyclone" );
    }


    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ BASIN ] = pgtca_getMenuEntries ( tblFile, row, 
                "Basin", &_menuEntryStr[ BASIN ] ) ) <= 0 ) {

       	_entryNum[ BASIN ] = 4;
	if ( pgtca_allocEntryStr( _entryNum[ BASIN ], 
			&_menuEntryStr[ BASIN ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ BASIN ][ 0 ], "Atlantic" );
        strcpy( _menuEntryStr[ BASIN ][ 1 ], "E. Pacific" );
        strcpy( _menuEntryStr[ BASIN ][ 2 ], "C. Pacific" );
        strcpy( _menuEntryStr[ BASIN ][ 3 ], "W. Pacific" );
    }

    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ SEVERITY ] = pgtca_getMenuEntries ( tblFile, row, 
                   "Severity", &_menuEntryStr[ SEVERITY ] ) ) <= 0 ) {

       	_entryNum[ SEVERITY ] = 2;

	if ( pgtca_allocEntryStr( _entryNum[ SEVERITY ], 
			&_menuEntryStr[ SEVERITY ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ SEVERITY ][ 0 ], "Tropical Storm" );
        strcpy( _menuEntryStr[ SEVERITY ][ 1 ], "Hurricane" );
    }

    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ AD_TYPE ] = pgtca_getMenuEntries ( tblFile, row, 
                   "AdvisoryType", &_menuEntryStr[ AD_TYPE ] ) ) <= 0 ) {

       	_entryNum[ AD_TYPE ] = 2;

	if ( pgtca_allocEntryStr( _entryNum[ AD_TYPE ], 
			&_menuEntryStr[ AD_TYPE ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ AD_TYPE ][ 0 ], "Watch" );
        strcpy( _menuEntryStr[ AD_TYPE ][ 1 ], "Warning" );
    }

    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ BP_TYPE ] = pgtca_getMenuEntries ( tblFile, row, 
                   "BreakpointType", &_menuEntryStr[ BP_TYPE ] ) ) <= 0 ) {

       	_entryNum[ BP_TYPE ] = 2;
	if ( pgtca_allocEntryStr( _entryNum[ BP_TYPE ], 
			&_menuEntryStr[ BP_TYPE ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ BP_TYPE ][ 0 ], "Official" );
        strcpy( _menuEntryStr[ BP_TYPE ][ 1 ], "All" );
    }

    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ GEO ] = pgtca_getMenuEntries ( tblFile, row, 
                   "SpecialGeography", &_menuEntryStr[ GEO ] ) ) <= 0 ) {

       	_entryNum[ GEO ] = 3;

	if ( pgtca_allocEntryStr( _entryNum[ GEO ],  
			&_menuEntryStr[ GEO ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ GEO ][ 0 ], "None" );
        strcpy( _menuEntryStr[ GEO ][ 1 ], "Islands" );
        strcpy( _menuEntryStr[ GEO ][ 2 ], "Water" );
    }

    if ( tblFile == NULL || row == 0 || 
	 ( _entryNum[ TIME_ZONE ] = pgtca_getMenuEntries ( tblFile, row, 
                   "TimeZone", &_menuEntryStr[ TIME_ZONE ] ) ) <= 0 ) {

       	_entryNum[ TIME_ZONE ] = 7;

	if ( pgtca_allocEntryStr( _entryNum[ TIME_ZONE ],  
			&_menuEntryStr[ TIME_ZONE ] ) != 0 ) return -1;

        strcpy( _menuEntryStr[ TIME_ZONE ][ 0 ], "AST" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 1 ], "EST" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 2 ], "EDT" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 3 ], "CST" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 4 ], "CDT" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 5 ], "PST" );
        strcpy( _menuEntryStr[ TIME_ZONE ][ 6 ], "PDT" );
    }

    /*For PHFO office text file*/
    _entryNum[ PHFO ] = 5;
    if ( pgtca_allocEntryStr( _entryNum[ PHFO ],
         &_menuEntryStr[ PHFO ] ) != 0 ) return -1;

    strcpy( _menuEntryStr[ PHFO ][ 0 ], "PHFOTCVCP1" );
    strcpy( _menuEntryStr[ PHFO ][ 1 ], "PHFOTCVCP2" );
    strcpy( _menuEntryStr[ PHFO ][ 2 ], "PHFOTCVCP3" );
    strcpy( _menuEntryStr[ PHFO ][ 3 ], "PHFOTCVCP4" );
    strcpy( _menuEntryStr[ PHFO ][ 4 ], "PHFOTCVCP5" );
    cfl_clos( tblFile, &iret );

    return 0;
}
/*=====================================================================*/


static int pgtca_getMenuEntries ( FILE *tblFile, int row, 
                                  char *menuName, char ***menuEntries ) 
/************************************************************************
 * pgtca_getMenuEntries							*
 *									*
 * This function reads entries of a menu from the tca info table	*
 *									*
 * int pgtca_getMenuEntries ( tblFile, row, menuName, menuEntries ) 	*
 *									*
 * Input parameters:							*
 *  	*tblFile 	FILE		pointer to the table file	*
 *  	row		int		number of rows in the table file*
 *  	*menuName	char		menu name			*
 *									*
 * Output parameters:							*
 *  	***menuEntries	char		menu entries array		*
 *									*
 * Return parameters:							*
 *			int		number of entries or		*
 *					 0:	no table file		*
 *					-1:	not enough memory 	*
 **									*
 * Log:									*
 * B. Yin/SAIC		03/04	Created					*
 * B. Yin/SAIC		04/04	Deleted code to open table		*
 ***********************************************************************/
{
    int 	entryNum = 0, iret = 0;
    int		bufsize = 0;
    int		counter, ii;
    char	oneRow[ 512 ];
    char	*tmpStr = NULL, *col1 = NULL, *col2 = NULL;

    struct entryList {
		char			*entry;
		struct entryList 	*next;
    } *listHead, *curPos, *newItem, *tmpItem;
/*---------------------------------------------------------------------*/

    listHead = curPos = newItem = tmpItem = NULL;

    cfl_seek( tblFile, 0, SEEK_SET, &iret );

    /*
     * Read menu entries from the table file
     */
    bufsize = sizeof( oneRow );
    for ( counter = 1; counter <= row; counter++ ) {
  	cfl_trln( tblFile, bufsize, oneRow, &iret );

        /*
         *  skip leading blank space 
         */
        ii = 0;
        while( ( oneRow[ ii ] == '\t' || oneRow[ ii ] == ' ' ) &&  
               ii < bufsize )
             ii++;
        
        /*
         *  get first column
         */
        col1 = &oneRow[ ii ];
        while( ( oneRow[ ii ] != '\t' && oneRow[ ii ] != ' ' ) &&
               ii < bufsize )
             ii++;
        oneRow[ ii ] = '\0';

        if ( strcmp( col1, menuName ) == 0 )  {
           ii++;

           /*
            * skip blank space and get the rest of the line
            */
           while( ( oneRow[ ii ] == '\t' || oneRow[ ii ] == ' ' )
                &&( ii < bufsize ) )
                ii++;
           col2 = &oneRow[ ii ];

           tmpStr = strtok( col2, ";" ); 

           while ( tmpStr != NULL ) {
	      if( ( newItem = (struct entryList *) malloc( sizeof( struct entryList ))) 
			== NULL ) return -1;
	      if( ( newItem->entry = ( char* ) calloc( MENU_STR_LENGTH, sizeof(char)))  
			== NULL ) return -1;
	      newItem->next = NULL;

	      strcpy( newItem->entry, tmpStr );
	      entryNum++;

	      if ( listHead == NULL )
		 listHead = curPos = newItem;
	      else {
		 curPos->next = newItem;
		 curPos = newItem;
	      }
              tmpStr = strtok( NULL, ";" );
           }
        }
    }

    /*
     * Put the menu entry strings into the output string array
     * Free the linked list
     */
    if ( entryNum > 0 ) {
       if ( ( *menuEntries = ( char** ) calloc( entryNum, sizeof( char * ) )) 
                           == NULL ) return -1;

       curPos = listHead;
       for ( counter = 0; counter < entryNum; counter++ ) {
           if ( ( (*menuEntries)[ counter ] = 
			( char* ) calloc( MENU_STR_LENGTH, sizeof( char ) )) 
					== NULL ) return -1;
           strcpy( (*menuEntries)[ counter ], curPos->entry );

           tmpItem = curPos;
           curPos  = curPos->next;
           free( tmpItem->entry );
           free( tmpItem );
       }
    }
 
    return entryNum;
}
/*=====================================================================*/


static int pgtca_getOptMenuValue ( int menuID )
/************************************************************************
 * pgtca_getOptMenuValue						*
 *									*
 * This function returns the integer value of an option menu entry	*
 *									*
 * static int pgtca_getOptMenuValue ( menuID )			 	*
 *									*
 * Input parameters:							*
 *  	menuID		int		integer menu ID			*
 *									*
 * Output parameters:							*
 * 	NONE								*
 *									*
 * Return parameters:							*
 *			static int	value of the menu entry		*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/04	Created					*
 ***********************************************************************/
{
   Widget 	wID;
   XmString 	xmStr;
   char		*tmpStr;
   int 		counter, value = 99;
/*---------------------------------------------------------------------*/

   /*
    * Get value from the pulldown menu
    */
   XtVaGetValues ( _pulldownMenu[ menuID ], XmNmenuHistory, &wID, NULL );
   XtVaGetValues ( wID, XmNlabelString,	&xmStr, NULL );

   XmStringGetLtoR(xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr);

   if ( tmpStr != NULL ) {
      for ( counter = 0; counter < _entryNum[ menuID ]; counter++ ) {
          if ( strcmp( tmpStr, _menuEntryStr[ menuID ][ counter ] ) == 0 ) {
             value = counter; 
          }
      }
      XtFree( tmpStr);
   }

   XmStringFree( xmStr );

   return value;
}
/*=====================================================================*/


static void pgtca_popdownDragBox ( void )
/************************************************************************
 * pgtca_pgtca_popdownDragBox						*
 *									*
 * This function pops down the break point label drag box		*
 *									*
 * int pgtca_pgtca_popdownDragBox ( )				 	*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 * 	NONE								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/04	Created					*
 * B. Yin/SAIC		07/04	Disarm drag box in all cases		*
 ***********************************************************************/
{
   if ( ( _bkpLabel != NULL ) && ( XtIsManaged ( _bkpLabel ) ) ) {
	XtUnmanageChild ( _bkpLabel );
   }
   mcanvw_disarmDrag ();
}

/*=====================================================================*/

static void pgtca_delHandleBar ( float lat, float lon, Boolean redraw )
/************************************************************************
 * pgtca_delHandleBar							*
 *									*
 * This function removes the handle bar at (lat, lon)			*
 *									*
 * int pgtca_delHandleBar ( lat, lon, redraw )			 	*
 *									*
 * Input parameters:							*
 *  	lat		float		latitude of the handle bar	*
 *  	lat		float		longitude of the handle bar	*
 *  	redraw		Boolean		whether to redraw element or not*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 * 	NONE								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/04	Created					*
 * B. Yin/SAIC		07/04	Added the redraw parameter		*
 ***********************************************************************/
{
   float	x1, y1;
   int 		iret, np;
/*---------------------------------------------------------------------*/

   np = 1;
   gtrans ( sys_M, sys_D, &np, &lat, &lon, &x1, &y1,
            &iret, strlen(sys_M), strlen(sys_D) );

   if ( redraw ) {
      pgutls_refresh ( x1 - HDLBSIZE, y1 - HDLBSIZE, 
		       x1 + HDLBSIZE, y1 + HDLBSIZE, &iret );
   }  
   else {
      xpgpaste ( x1 - HDLBSIZE, y1 - HDLBSIZE,
                 x1 + HDLBSIZE, y1 + HDLBSIZE, &iret );
   }
}
/*=====================================================================*/


static void pgtca_putHandleBar ( int icolor, int itype, float lat, 
                                 float lon )
/************************************************************************
 * pgtca_putHandleBar							*
 *									*
 * This function puts a handle bar at (lat, lon)			*
 *									*
 * int pgtca_putHandleBar ( icolor, itype, lat, lon )		 	*
 *									*
 * Input parameters:							*
 *  	icolor		int		color of the handle bar  	*
 *  	itype		int		type of the handle bar		*
 *  	lat		float		latitude of the handle bar	*
 *  	lon		float		longitude of the handle bar	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 * 	NONE								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/04	Created					*
 ***********************************************************************/
{
   int 		ihw, id, iret, np;
   float 	size;
/*---------------------------------------------------------------------*/

   ihw  = 1;
   size = 1.0F;
   id   = 2;

   gscolr ( &icolor, &iret );
   gsmrkr ( &itype, &ihw, &size, &id, &iret );

   np = 1;
   gmark ( sys_M, &np, &lat, &lon, &iret, strlen(sys_M) );
}
/*=====================================================================*/


static void pgtca_getLatLon ( Widget wg, char *bkpName , float *lat, 
                              float *lon )
/************************************************************************
 * pgtca_getLatLon							*
 *									*
 * This function gets break point name, lat and lon from a widget	*
 *									*
 * void pgtca_getLatLon ( wg, bkpName, lat, lon )		 	*
 *									*
 * Input parameters:							*
 *  	wg		Widget		break point widget		*
 *									*
 * Output parameters:							*
 *  	*bkpName	char		name of the break point		*
 *  	*lat		float		latitude of the break point	*
 *  	*lon		float		longitude of the break point	*
 * Return parameters:							*
 * 	NONE								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/04	Created					*
 * E. Safford/SAIC	01/05	increase size of info[]			*
 * B. Yin/SAIC		09/05	check if _bkpLabel is managed		*
 ***********************************************************************/
{
   int  		maxlen, nret, iret;
   char 		info[ 256 ];
   char*		tmpStr = NULL;
   XmString        	xmStr  = NULL;
/*---------------------------------------------------------------------*/

   if ( wg == _bkpLabel ) {

      if ( !XtIsManaged ( _bkpLabel ) ) return;

      XtVaGetValues ( wg, XmNlabelString, &xmStr, NULL );
      XmStringGetLtoR ( xmStr, XmSTRING_DEFAULT_CHARSET, &tmpStr );
   }
   else if ( wg == _txtBP1 || wg == _txtBP2 ) {
      XtVaGetValues ( wg, XmNvalue, &tmpStr, NULL );
   }
   else {
      return;
   }

   strcpy ( bkpName, tmpStr );
   maxlen = sizeof ( info );
   clo_findmatch ( BKP_TBL, tmpStr, "", 1, 1, maxlen, &nret,
                             info, &iret );
   cst_gtag ( "LAT", info, "99999", tmpStr, &iret );
   cst_crnm ( tmpStr, lat, &iret );
   cst_gtag ( "LON", info, "99999", tmpStr, &iret );
   cst_crnm ( tmpStr, lon, &iret );

   if ( xmStr != NULL )
      XmStringFree ( xmStr );
   if ( tmpStr != NULL )
      XtFree ( tmpStr );
}
/*=====================================================================*/


Boolean pgtca_isNewSeg ( void )
/************************************************************************
 * pgtca_isNewSeg                                                       *
 *                                                                      *
 * This rountine returns whether a new segment is being created 	*
 *                                                                      *
 * pgtca_isNewSeg ( )                                        		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *		TRUE	if working on a new segment			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          04/04   Created                                 *
 ***********************************************************************/
{
   return _newSegment;
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_geoCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_geoCb                                                          *
 *                                                                      *
 * Callback function for the Special Geograpgy menu			*
 *                                                                      *
 * void pgtca_geoCb ( wid, clnt, call )	                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      clnt		XtPointer	ient data (not used)            *
 *      call            XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		07/04	Created				   	*
 ***********************************************************************/
{
   char		*tmpStr;
   XmString 	xmStr;
   Widget	wID;
/*---------------------------------------------------------------------*/

   XtVaGetValues( _pulldownMenu[GEO],
		  XmNmenuHistory,			&wID,
		  NULL );

   XtVaGetValues(  wID,
		   XmNlabelString,			&xmStr,
		   NULL );

   XmStringGetLtoR( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

   if ( strcmp( tmpStr, "None" ) != 0 ) {
      XtVaSetValues ( XmOptionButtonGadget( _optMenu[ BP_TYPE ] ), 
			XmNsensitive,		False,
			NULL );
      if ( strcmp( tmpStr, "Islands" ) == 0 ) {
         _geoType = ISLANDS;
      }
      else if ( strcmp( tmpStr, "Water" ) == 0 ) {
         _geoType = WATER;
      }
   }
   else {
      XtVaSetValues ( XmOptionButtonGadget( _optMenu[ BP_TYPE ] ), 
			XmNsensitive,		True,
			NULL );
      _geoType = NO_TYPE;
   }
        
   XmStringFree( xmStr );
   XtFree( tmpStr );
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_bpTypeCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_bpTypeCb                                                       *
 *                                                                      *
 * Callback function for the Break Point Type menu			*
 *                                                                      *
 * void pgtca_bpTypeCb ( wid, clnt, call )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          widget ID                       *
 *      clnt		XtPointer	client data (not used)		*
 *      call            XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		07/04	Created				   	*
 ***********************************************************************/
{
   char		*tmpStr;
   XmString 	xmStr;
   Widget	wID;
/*---------------------------------------------------------------------*/

   XtVaGetValues( _pulldownMenu[BP_TYPE],
		  XmNmenuHistory,			&wID,
		  NULL );

   XtVaGetValues(  wID,
		   XmNlabelString,			&xmStr,
		   NULL );

   XmStringGetLtoR( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

   if ( strcmp( tmpStr, "Official" ) == 0 ) {
      _bpType = OFFICIAL;
   }
   else if ( strcmp( tmpStr, "All" ) == 0 ){
      _bpType = ALL_BP;
   }
        
   XmStringFree( xmStr );
   XtFree( tmpStr );
}
/*=====================================================================*/

static void pgtca_multiPtSel ( int np, const float xx[], const float yy[] )
/************************************************************************
 * pgtca_multiPtSel                                                     *
 *									*
 * This function processes the area selection of break points.		*
 *									*
 * static void pgtca_multiPtSel ( np, xx[], yy[] )			*
 *									*
 * Input parameters:							*
 *	np		np	number of points			*
 *	xx		float	x coordinates of points			*
 *	yy		float	y coordinates of points			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		07/04	Created				   	*
 ***********************************************************************/
{
   int 		ii, jj, numBkpts, npt, ier;
   char 	tbl[ 34 ], stnStr[ MAXPTS * 34 ], stn[ 34 ];
   float 	lat[ MAXPTS ],lon[ MAXPTS ];
   float 	xxv[ MAXPTS ],yyv[ MAXPTS ];
   Boolean 	inList = False;
/*---------------------------------------------------------------------*/

   mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);

   for ( ii = 0; ii < np -1; ii++ ) {
       xxv[ ii ] = xx[ ii ];
       yyv[ ii ] = yy[ ii ];
   }

   switch ( _geoType ) {

       case ISLANDS:
	   
	    strcpy ( tbl, BKP_TBL_ISL );
	    break;

       case WATER:

            strcpy ( tbl, BKP_TBL_WAT );
            break;

       default:

	    break;   

   }

   /*
    * Find all break points in the area
    */
   clo_tinpoly( tbl, sys_D, np-1, xxv, yyv, &ier );
   clo_tgltln( tbl, sizeof(lat)/sizeof(float), &npt, lat, lon, &ier );
   clo_tgnm( tbl, sizeof(lat)/sizeof(float), sizeof(stnStr), &npt, stnStr, &ier );

   if ( (npt + _numBkpts) > _capacity ) {
      _bkpts = realloc ( _bkpts, ( _numBkpts + MAX( npt, BKPT_INC) )* sizeof( Breakpt_T ));
   }

   _capacity =  _numBkpts + MAX( npt, BKPT_INC);

   numBkpts = _numBkpts;

   if ( npt != 0 ) {
      strcpy ( stn, strtok ( stnStr, ";" ) );
   }

   for ( ii = 0; ii < npt; ii++ ) {

        /*
	 * Check if the break point is already in the segment
         */
        for ( jj = 0; jj < numBkpts; jj++ ) {
            inList = ! strcmp ( stn, _bkpts[ jj ].breakPtName );
	    if ( inList ) break;
        }

	/*
	 * Add the break point into the segment
	 */
        if ( ! inList ) {

           _bkpts[ _numBkpts ].lat = lat [ ii ];
           _bkpts[ _numBkpts ].lon = lon [ ii ];
	   strcpy ( _bkpts[ _numBkpts ].breakPtName, stn );

           pgtca_putHandleBar ( WHITE, DOT, _bkpts[ _numBkpts ].lat, _bkpts[ _numBkpts ].lon );

           _numBkpts++;
        }

        if ( ii != npt - 1 ) {
	   strcpy( stn, strtok( NULL, ";" ) );
	}
   }

   geplot( &ier );
}
/*=====================================================================*/

/* ARGSUSED */
static void pgtca_singlePtSel ( float xx, float yy )
/************************************************************************
 * pgtca_singlePtSel                                                    *
 *									*
 * This function processes the single selection of break points.	*
 *									*
 * static void pgtca_singlePtSel ( xx, yy )				*
 *									*
 * Input parameters:							*
 *	xx		float	x coordinate of point			*
 *	yy		float	y coordinate of point			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		07/04	Created				   	*
 ***********************************************************************/
{
    int 	counter, iret;
    Boolean 	inList = False;
/*---------------------------------------------------------------------*/

    mcanvw_setDragFunc ( (XtEventHandler)&pgtca_pointerEh, CURS_DEFAULT);

    if ( _numBkpts >= _capacity ) {
       if ( !( _bkpts = realloc( _bkpts, 
		    ( _capacity + BKPT_INC ) * sizeof( Breakpt_T ) )))
	    return;
       _capacity += BKPT_INC;
    }

    pgtca_getLatLon ( _bkpLabel, _bkpts[ _numBkpts ].breakPtName, 
		      &( _bkpts[ _numBkpts ].lat ), &( _bkpts[ _numBkpts ].lon ) );

    if ( strlen( _bkpts[ _numBkpts ].breakPtName ) != (size_t)0 ) {

       for ( counter = 0; counter < _numBkpts; counter++ ) {
	   inList = !(strcmp( _bkpts[ _numBkpts ].breakPtName, _bkpts[ counter ].breakPtName ));
	   if ( inList ) break;
       }

       if ( ! inList ) {
          XtVaSetValues ( _txtBP1, XmNvalue, _bkpts[ _numBkpts ].breakPtName, NULL );
          pgtca_putHandleBar ( WHITE, DOT, _bkpts[ _numBkpts ].lat, _bkpts[ _numBkpts ].lon );
          geplot( &iret );
          _numBkpts++;
       }
       else {
          XtVaSetValues ( _txtBP1, XmNvalue, "", NULL );
          pgtca_delHandleBar (  _bkpts[ _numBkpts ].lat, _bkpts[ _numBkpts ].lon, False );
	  memcpy ( &_bkpts[ counter ], &_bkpts[ --_numBkpts ], sizeof ( Breakpt_T ) );
       }
    }
}

/*=====================================================================*/

static void pgtca_endMultiPtSel ( void ) 
/************************************************************************
 * pgtca_endMultiPtSel                                                  *
 *									*
 * This function handles the termination of multi-select.		*
 *									*
 * static void pgtca_endMultiPtSel ( )					*
 *									*
 * Input parameters:							*
 *                      NONE                                            *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		07/04	Created				   	*
 * B. Yin/SAIC		09/04	Added code to redraw handle bars	*
 * B. Yin/SAIC		01/05	Redraw element only once		*
 ***********************************************************************/
{
    int 	ii, jj, iret, el_loc;
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/

    if ( _newSegment ) {

       /*
        * New TCA
        */
       if ( _numBkpts != 0 ) {
          /*
           * Remove handle bars
           */
          for ( ii = 0; ii < _numBkpts; ii++ ) {
              pgtca_delHandleBar ( _bkpts[ ii ].lat, _bkpts[ ii ].lon, False );
          }

          pgtca_addItem ();
          pgtca_updateTca ();
          _newSegment = False;

       }
    }
    else {

       /*
        * Update segment of an existing TCA
        */
       if ( _numBkpts != 0 ) {
	  pgtca_updateSegment();

	  /*
	   * Deselect the segment
	   */
          _segSelected = FALSE;

          XmListDeselectPos( _listBP, _curSegment );
          XtVaSetValues( _buttonApply,  XmNsensitive, FALSE, NULL );
          XtVaSetValues( _buttonDel,  XmNsensitive, FALSE, NULL );

          pgtca_setSegmentAttr( NULL );

	  /*
	   * Redraw handle bars
	   */
          for ( ii = 0; ii < _numBkpts; ii++ ) {
              pgtca_delHandleBar ( _bkpts[ ii ].lat, _bkpts[ ii ].lon, ii == _numBkpts - 1 );
	  }

          el_loc = pgactv_getElmLoc ();
          cvg_rdrec ( cvg_getworkfile(), el_loc, &el, &iret );

          for ( ii = 0; ii < el.elem.tca.info.wwNum; ii++) {
              for ( jj = 0; jj < el.elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
                  pgtca_putHandleBar ( SKYBLUE, CIRCLE, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat, 
		       el.elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon );
	      }
          }

          cvg_freeBkpts( &el );
          geplot( &iret );
       }
       else {
 
          pgtca_delItem ();

       }
    }

    XtVaSetValues ( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		    XmNsensitive,		True,
	            NULL );

    mcanvw_disarmDynamic ();
    mcanvw_disarmDrag ();

    pgtca_popdownDragBox ();

    mcanvw_setPressFunc ( (XtEventHandler)&pgtca_clickEh, CURS_DEFAULT);
}

/*=====================================================================*/

static void pgtca_makeTextMsg ( char * outText ) 
/************************************************************************
 * pgtca_makeTextMsg                                                    *
 *									*
 * This function creates the TCA text message.				*
 *									*
 * static void pgtca_makeTextMsg ( outText )				*
 *									*
 * Input parameters:							*
 *	*outText	char	pointer to the message string           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		08/04	Created				   	*
 * B. Yin/SAIC		01/05	Redraw element only once		*
 * H. Zeng/SAIC		01/05	removed saving VGF part			*
 * M. Li/SAIC		03/05	Modified gh_tctx			*
 * B. Yin/SAIC		04/05	Removed year field, get year from time	*
 * S. Gilbert/NCEP	01/06   Handle intermediate advisories          *
 * S. Gilbert/NCEP	10/07	Changed txtFile name to depend on basin *
 * m.gamazaychikov/SAIC	12/07	Add code to generate name for CP basin  *
 * m.gamazaychikov/SAIC	04/08	Add code to append work_dir to file name*
 * m.gamazaychikov/SAIC	04/08	Fix problem with name of text file name	*
 * X. Guo/CWS           04/11   PHFO uses "CP" for "cp" and "ep"        *
 ***********************************************************************/
{
    int 	stormNum, advisoryNum, iflag, nextEl, pos, more, ier, found;
    char        *tmpStr, buf[ 256 ];
    char 	prevVGF[ 50 ], txtFile[FILE_FULLSZ],basin[3],rspoff[5];
    FILE	*advsFile, *workFile;
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/

    /* 
     * get vgf file name for prevVGF.
     */
    pgtca_getBasinId ( basin );

    XtVaGetValues( _txtStormNum, XmNvalue, &tmpStr, NULL );
    stormNum = atoi( tmpStr );
    XtFree( tmpStr );

    XtVaGetValues( _txtAdvisoryNum, XmNvalue, &tmpStr, NULL );
    gh_advn ( tmpStr, &advisoryNum, &iflag, &ier );
    XtFree( tmpStr );

    XtVaGetValues( _txtTime, XmNvalue, &tmpStr, NULL );

    found = 0;
    switch ( iflag ) {

	case 0:       /*   Regular advisory  */
            sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03db.vgf", 
	         basin, stormNum, tmpStr, advisoryNum - 1 );
           if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            if ( found == 0 ) {
                sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03da.vgf", 
	             basin, stormNum, tmpStr, advisoryNum - 1 );
                if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            }
            if ( found == 0 ) {
                sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03d.vgf", 
	             basin, stormNum, tmpStr, advisoryNum - 1 );
                if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            }
	    break;

	case 1:       /*   1st intermediate advisory  */
            sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03d.vgf", 
	         basin, stormNum, tmpStr, advisoryNum );
            if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            break;

	case 2:       /*   2nd intermediate advisory  */
            sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03da.vgf", 
	         basin, stormNum, tmpStr, advisoryNum );
            if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            if ( found == 0 ) {
                sprintf( prevVGF, "tca_%2s%02d20%2.2s_%03d.vgf", 
	             basin, stormNum, tmpStr, advisoryNum );
                if ( access ( prevVGF, F_OK ) == 0 ) found = 1;
            }
	    break;

	default:
            found = 0;
	    break;
    }

    XtFree( tmpStr );

    /*
     * save TCA text message.
     */
    if ( found  == 1 ) {
       gh_tctx ( _vgFileName, prevVGF, " ", " ", " " );
        }
    else {
       gh_tctx ( _vgFileName, "", " ", " ", " " ); 
    }

    /* 
     * Rebuild range records, which are cleared in gh_bkrv
     */
    nextEl = 0;
    pos = 0;
    more = G_TRUE;
    workFile = cfl_ropn ( cvg_getworkfile(), "", &ier );

    while ( nextEl < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
        cvg_rdrecnoc ( cvg_getworkfile(), workFile, pos, &el, &ier );
        if ( ier < 0 )  {
            more = G_FALSE;
        }
        else  {
	    if ( el.hdr.delete == 0 ) {
               crg_set ( &el, pos, pglayer_getCurLayer(), &ier );
	    }

            pos += el.hdr.recsz;

	    /*
             * Free TCA break point memory
             */
            if ( el.hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( &el );
            }

            nextEl++;
        }
    }

    cfl_clos ( workFile, &ier );

    /*
     * Read text message from the file
     */
    outText [ 0 ] = '\0';

    stormNum %= 5;
    stormNum = ( stormNum == 0 ) ? 5 : stormNum;
  
    sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
    if ( strncmp( basin, "ep", 2 ) == 0 )
       sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "EP", stormNum );
    if ( strncmp( basin, "cp", 2 ) == 0 )
       sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAMEHI, "CP", stormNum );

    /*
     * Retrieve the responsible office.
     */
    ctb_rdprf ( "prefs.tbl", "config", "TCV_RSP_OFFICE", rspoff, &ier );
    /*
     * Construct text file name.
     */
    if ( ( ier != 0 ) ||
         ( ( strncmp( rspoff, "KNHC", 4 ) != 0 ) && 
           ( strncmp( rspoff, "PHFO", 4 ) != 0 )  ) ) {
       /*
        * Old naming convention - if 'TCV_RSP_OFFICE' tag is missing
        * or set to improper value
        */ 
       sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
       if ( strncmp( basin, "ep", 2 ) == 0 )
         sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "EP", stormNum );
       if ( strncmp( basin, "cp", 2 ) == 0 )
         sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAMEHI, "CP", stormNum );
    }
    else {
       /*
        * New naming convention - if 'TCV_RSP_OFFICE' tag is present
        */ 
      if ( strncmp( rspoff, "PHFO", 4 ) == 0 ) {
          /* PHFO only uses CP for "ep" and "cp" */
          if ( strncmp( basin, "ep", 2 ) == 0 ||
               strncmp( basin, "cp", 2 ) == 0 ) {
              sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAMEHI, "CP", stormNum );
          } else if ( strncmp( basin, "al", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
      }
      else if ( strncmp( rspoff, "KNHC", 4 ) == 0 ) {
          if ( strncmp( basin, "ep", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "EP", stormNum );
          if ( strncmp( basin, "cp", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "CP", stormNum );
          if ( strncmp( basin, "al", 2 ) == 0 )
            sprintf( txtFile, "%s%s%2s%d", _tcvWorkDir, TXT_NAME, "AT", stormNum );
      }
    }
    if ( ( advsFile = fopen ( txtFile, "r" ) ) != NULL ) {
       while ( fgets ( buf,  sizeof ( buf ), advsFile ) ) {
	     strcat ( outText, buf );
       }
       fclose ( advsFile );
    }

}
/*=====================================================================*/

static void pgtca_writeTextMsg ( int *iret )
/************************************************************************
 * pgtca_writeTextMsg                                                   *
 *                                                                      *
 * This function writes the TCA text message into new file.             *
 *                                                                      *
 * static void pgtca_writeTextMsg ( iret )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 * Output parameters:                                                   *
 *      *iret            int         Return code                        *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS          04/11    Created                                 *
 ***********************************************************************/
{
     char         *txtStr, txtFile[FILE_FULLSZ];
     FILE        *advsFile;
     
     *iret = 0;

     sprintf( txtFile, "%s%s", _tcvWorkDir,_selectedTextFile );     
     if ( ( advsFile = fopen ( txtFile, "w" ) ) != NULL ) {
         XtVaGetValues ( _txtMsg, XmNvalue, &txtStr, NULL );
         if ( txtStr != NULL ) {
             fwrite (txtStr, 1, strlen (txtStr), advsFile );
             XtFree( txtStr );
         }
         else *iret = -1;
         fclose (advsFile);
     }
     else  *iret = -1;
}
/*=====================================================================*/

static void pgtca_getBasinId ( char * basinId ) 
/************************************************************************
 * pgtca_getBasinId                                                     *
 *									*
 * This function gets the basin ID.					*
 *									*
 * static void pgtca_getBasinId ( basinId )				*
 *									*
 * Input parameters:							*
 *                      NONE                                            *
 * Output parameters:                                                   *
 *                                                                      *
 *	*basinId	char	 basin ID			        *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		08/04	Created				   	*
 ***********************************************************************/
{
    char         *tmpStr;

    XmString     xmStr;
    Widget	 wID;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( _pulldownMenu[ BASIN ], XmNmenuHistory, &wID, NULL );
    XtVaGetValues ( wID, XmNlabelString,	&xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

    if ( tmpStr == NULL ) {
       strncpy( basinId, "bb", 2 );
    }
    else if ( ( strcmp( tmpStr, "Atlantic" ) == 0 ) ) {
       strncpy( basinId, "al", 2 );
    }
    else if ( ( strcmp( tmpStr, "E. Pacific" ) == 0 ) ) {
       strncpy( basinId, "ep", 2 );
    }
    else if ( ( strcmp( tmpStr, "C. Pacific" ) == 0 ) ) {
       strncpy( basinId, "cp", 2 );
    }
    else if ( ( strcmp( tmpStr, "W. Pacific" ) == 0 ) ) {
       strncpy( basinId, "wp", 2 );
    }
    else {
       strncpy( basinId, tmpStr, 2 );
    }
    basinId[ 2 ] = '\0';

    XtFree( tmpStr );
    XmStringFree( xmStr );
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtca_closeCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtca_closeCb							*
 *									*
 * This is the callback function to close TCA edit dialog 		*
 *									*
 * static void pgtca_closeCb ( wid, clnt, call )			*
 *									*
 * Input parameters:                                                    *
 *      wid             Widget          not used                        *
 *      clnt		XtPointer	not used                        *
 *      call            XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 8/04	Created				   	*
 ***********************************************************************/
{
    pgtca_popdown();
}

/*=====================================================================*/

/* ARGSUSED */
static void pgtca_completeTimeCb ( Widget wid, XtPointer clnt, 
				   XtPointer call )
/************************************************************************
 * pgtca_completeTimeCb							*
 *									*
 * This is the callback function to complete the time string 		*
 *									*
 * static void pgtca_completeTimeCb ( wid, clnt, call )			*
 *									*
 * Input parameters:                                                    *
 *      wid             Widget          not used                        *
 *      clnt		XtPointer       not used                        *
 *      call            XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		12/04	Created				   	*
 * B. Yin/SAIC		12/04	Check if hour is 24 or min is 60   	*
 * E. Safford/SAIC	05/05	check and free tmpStr on early exit	*
 ***********************************************************************/
{
   int	timeType, year, mmdd, hhmm, ier, hh, mm;
   char	curTime[ 16 ], timeStr[ 16 ], padStr[ 5 ], *tmpStr = NULL, *pStr;
/*---------------------------------------------------------------------*/

   /*
    *  Get UTC time
    */
   timeType = 1;
   css_gtim ( &timeType, curTime, &ier );

   XtVaGetValues ( _txtTime, XmNvalue, &tmpStr, NULL );

   if ( ( tmpStr == NULL ) || ( strlen ( tmpStr ) == (size_t)0 ) ) {

      XtVaSetValues ( _txtTime, XmNvalue, curTime, NULL );

      if( tmpStr ) {
	  XtFree( tmpStr );
      }
      return;

   }
   
   if ( ( pStr = strchr ( tmpStr, '/' ) ) ) { 		/* '/' found */

      ti_c2i ( tmpStr, &year, &mmdd, &hhmm, &ier, strlen ( tmpStr ) );

      hh = hhmm / 100;
      mm = hhmm % 100;

      /*
       *  Check year, month and day
       */
      if ( ( ier == 0 ) && ( hh != 24 ) && ( mm != 60 ) ) {
	 
	 tmpStr[ 11 ] = '\0';
	 XtVaSetValues ( _txtTime, XmNvalue, tmpStr, NULL );
	   
      }
      else if ( ier == -1 || ier == -7 || ier == -8 || ier == -9 ) {

         timeStr[ 0 ] = '\0';
	 strncat ( timeStr, curTime, 6 );
	 strncat ( timeStr, pStr, 5 );

	 ti_c2i ( timeStr, &year, &mmdd, &hhmm, &ier, strlen ( timeStr ) );

         hh = hhmm / 100;
         mm = hhmm % 100;

	 if ( ( ier == 0 ) && ( hh != 24 ) && ( mm != 60 ) ) {

	    XtVaSetValues ( _txtTime, XmNvalue, timeStr, NULL ); 

	 }
	 else {

	    XtVaSetValues ( _txtTime, XmNvalue, curTime, NULL );

	 }
      }
      else {
	 
         XtVaSetValues ( _txtTime, XmNvalue, curTime, NULL );
	 	   
      }

   }

   else {			/* 	no '/' 		*/
    
      timeStr[ 0 ] = '\0';

      strncat ( timeStr, curTime, 6 );
      strcat ( timeStr, "/" );

      cst_padString ( tmpStr, '0', 0, 4, padStr );
      strncat ( timeStr, padStr, 4 );

      ti_c2i ( timeStr, &year, &mmdd, &hhmm, &ier, strlen ( timeStr ) );

      hh = hhmm / 100;
      mm = hhmm % 100;

      if ( ( ier == 0 ) && ( hh != 24 ) && ( mm != 60 ) ) {

	 XtVaSetValues ( _txtTime, XmNvalue, timeStr, NULL );
  
      }
      else {

	 XtVaSetValues ( _txtTime, XmNvalue, curTime, NULL );
	   
      }

   }

   XtFree( tmpStr );

}

/*=====================================================================*/

static Boolean pgtca_checkInfo ( void )
/************************************************************************
 * pgtca_checkInfo	                                                *
 *                                                                      *
 * This rountine checks if there are values in Storm Name and Valid Time*
 * text box and determines whether to pop up warning window or not.     *
 *                                                                      *
 * pgtca_checkInfo ( )				                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 * Return parameters:                                                   *
 *	pgtca_checkInfo	Boolean	warning window has popped up or not     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		02/05	initial coding				*
 ***********************************************************************/
{
   char 	*str1, *str2;
   char		mesg1[] = "Please provide the storm name.";
   char		mesg2[] = "Please provide the valid time.";
   XmString     xmwarning;
   Widget 	warning, child;
/*--------------------------------------------------------------------*/

   XtVaGetValues ( _txtStormName, XmNvalue, &str1, NULL );
   XtVaGetValues ( _txtTime, XmNvalue, &str2, NULL );
  
   if ( strlen ( str1 ) != (size_t)0 && strlen ( str2 ) != (size_t)0 ) {
   
        XtFree( str1 );
        XtFree( str2 );       
        return ( FALSE );
   }


   /*
    * Pop up the warning window below.
    */
   warning = XmCreateWarningDialog( _tcaAttribW, "Warning", NULL, 0);

   if ( strlen ( str1 ) == (size_t)0 ) {
        xmwarning = XmStringCreateLtoR ( mesg1, XmSTRING_DEFAULT_CHARSET );

        XtVaSetValues ( warning,
		XmNdialogType, 		XmDIALOG_WARNING,
		XmNmessageAlignment, 	XmALIGNMENT_CENTER,
                XmNnoResize,          	True,  
		XmNmessageString, 	xmwarning,
		XmNdialogStyle, 	XmDIALOG_FULL_APPLICATION_MODAL,
		NULL );
        XmStringFree (xmwarning);
   }
   else if ( strlen ( str2 ) == (size_t)0 ) {
        xmwarning = XmStringCreateLtoR ( mesg2, XmSTRING_DEFAULT_CHARSET );

        XtVaSetValues ( warning,
		XmNdialogType, 		XmDIALOG_WARNING,
		XmNmessageAlignment, 	XmALIGNMENT_CENTER,
                XmNnoResize,          	True,  
		XmNmessageString, 	xmwarning,
		XmNdialogStyle, 	XmDIALOG_FULL_APPLICATION_MODAL,
		NULL );
        XmStringFree (xmwarning);
   }

   child = XmMessageBoxGetChild( warning, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild ( child );

   child = XmMessageBoxGetChild( warning, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild ( child );

   XtManageChild( warning );

   XtFree( str1 );
   XtFree( str2 );
   return( TRUE );
   
}

/*====================================================================*/

void pgtca_stepList ( Arrow_Dir_t direction, int *iret )
/************************************************************************
 * pgtca_stepList	                                                *
 *                                                                      *
 * This routine selects TCA segment by pressing arrow keys (up/down). It*
 * is called by hotkeyArrowUp() and hotkeyArrowDown() which are actions *
 * defined in the resource file.					*
 *                                                                      *
 * pgtca_stepList ( direction, iret )  			                *
 *                                                                      *
 * Input parameters:                                                    *
 *      direction	Arrow_Dir_t     arrow direction (up/down)	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        	return code                     *
 *                                      0: normal                       *
 *                                      1: no TCA segment               *
 *                                      2: at the begining/end of list  *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		3/05	Created					*
 ***********************************************************************/
{
   int  listCount = 0, itemsSelected = 0, segIndex;
   int  *pos = NULL;
/*---------------------------------------------------------------------*/
                                                                                
   *iret = 0;
                                                                                
   XtVaGetValues ( _listBP, XmNitemCount, &listCount, NULL );
                                                                                
   /*
    *  Return if there is no TCA segment
    */
   if ( listCount <= 0 ) {

      *iret = 1;
      return;

   }
                                                                                
   XmListGetSelectedPos ( _listBP, &pos, &itemsSelected );
                                                                                
   if ( itemsSelected >= 1 ) {
                                                                                
      if ( direction == DOWN_LIST ) {
 
         if ( pos[ 0 ] < listCount ) {
        
            segIndex = pos[ 0 ] + 1;
        
         }
         else {
        
            segIndex = _curSegment;
            *iret = 2;
        
         }
        
      }
 
      else if ( direction == UP_LIST ) {
 
         if ( pos[ 0 ] > 1 ) {
        
            segIndex = pos[ 0 ] - 1;
        
         }
         else {
        
            segIndex = _curSegment;
            *iret = 2;
        
         }
 
      }
 
   }
   else {
 
      if ( direction == DOWN_LIST ) {
 
         segIndex = listCount;
 
      }
 
      else if ( direction == UP_LIST ) {
 
         segIndex = 1;
        
      }
 
   }
 
   if ( !_segSelected || ( segIndex != _curSegment ) ) {
 
      XmListSelectPos ( _listBP, segIndex, TRUE );
      _segSelected = TRUE;
 
   }

   if ( pos != NULL ) {

      free ( pos);

   }
 
}


/*====================================================================*/

static void pgtca_addSegment( int replaceSeg, int geogType, char *severity, 
 		char *advisoryType, int numPts, char **pointName, int *iret )
/************************************************************************
 * pgtca_addSegment	                                                *
 *                                                                      *
 * This routine adds a segment into the _listBP (scrolle list of        *
 * segments).                                                           *
 *                                                                      *
 * pgtca_addSegment ( replaceSeg, geogType, severity, advisoryType,     *
 *				numPts, pointName, iret ) 	        *
 *                                                                      *
 * Input parameters:                                                    *
 *	replaceSeg	int	segment number to be replaced or -1 if  *
 *				  this is a new segment			*
 *      geogType	int	geography type (none, islands, water)   *
 *      *severity	char	TS or Hurricane				*
 *      *advisoryType	char	Watch or warning			*
 *      numPts		int	number of breakpoints			*
 *      **pointName	char	array of breakpoint names		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int     return code                     	*
 *                                  0: normal                       	*
 *                                 -1: numPts 0 or less            	*
 *                                 -2: empty pointName array       	*
 * Return parameters:                                                   *
 *			NONE						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/05	initial coding				*
 * E. Safford/SAIC	05/05	reselect list pos for replacement seg   * 
 ***********************************************************************/
{
    int		pos = 1, ii;
    char 	newStr[ 1024 ] ="";
    XmString 	newItem;
/*---------------------------------------------------------------------*/

    if( numPts <= 0 )  {
        *iret = -1;
	return; 
    }
    if( pointName == NULL ) {
        *iret = -2;
	return; 
    }

    *iret = 0;

    /*
     * add Severity and Advisory Type to newStr.
     */
    sprintf( newStr, "%s%-15.15s ", newStr, severity );
    sprintf( newStr, "%s%-9.9s ", newStr, advisoryType );
        

    switch ( geogType ) {

	case NO_TYPE:
    	    sprintf( newStr, "%s%-12.12s ", newStr, pointName[0] );
    	    sprintf( newStr, "%s%-12.12s ", newStr, pointName[1] );
	    break;

	case WATER:
	case ISLANDS:
	    for( ii=0; ii < numPts; ii++ ) {	
	        if( ii > 0 ) {
	            sprintf( newStr, "%s, %s", newStr, pointName[ii] );
		}
		else {
		    sprintf( newStr, "%s%s", newStr, pointName[ii] );
		}
            }
	    break;

	default:
	    break;
    }

    newItem = ( XmString ) XmStringCreateLtoR( newStr, "chset1" );
    
    XtVaGetValues( _listBP, XmNitemCount, &pos, NULL );

    /*
     *  New segment
     */
    if( replaceSeg < 0 ) {

        XmListAddItem( _listBP, newItem, pos + 1 );

        _curSegment = pos + 1;	
        if ( pos + 1 > VISIBLE_ITEMS )
           XmListSetPos( _listBP, pos - VISIBLE_ITEMS + 2 );

        XtVaSetValues ( XmOptionButtonGadget( _optMenu[ GEO ] ), 
		    XmNsensitive,		True,
	            NULL );

        if ( pos + 1 == MAX_TCAWW )
	    XtVaSetValues( _buttonNewSeg, XmNsensitive, FALSE, NULL );

    }
    else {		/* replacement segment */
        XmListReplaceItemsPos ( _listBP, &newItem, 1, replaceSeg ); 
	XmListSelectPos( _listBP, replaceSeg, False );

        if ( replaceSeg > VISIBLE_ITEMS )
            XmListSetPos ( _listBP, replaceSeg - VISIBLE_ITEMS + 1 );

    }

    XmStringFree ( newItem );

}

/*=====================================================================*/

static int pgtca_getPri ( char * bpName )
/************************************************************************
 * pgtca_getPri                                                         *
 *                                                                      *
 * This routine gets the first digit of the priority number of a break  *
 * point. Default is -1.						*
 *                                                                      *
 * static int pgtca_getPri ( bpName )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *bpName	char	name string of a break point                    *
 *                                                                      *
 * Return parameters:                                                   *
 *	static int	first digit of priority number of the input bp  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		09/05	Created					*
 ***********************************************************************/
{
   int  		maxlen, nret, iret;
   float		pri;
   char 		info[ 256 ], tmpStr[ 32 ];
/*---------------------------------------------------------------------*/

   maxlen = sizeof ( info );

   clo_findmatch ( BKP_TBL, bpName, "", 1, 1, maxlen, &nret,
   		   info, &iret );
   cst_gtag ( "PRI", info, "-1", tmpStr, &iret );
   cst_crnm ( tmpStr, &pri, &iret );

   return ( (int)pri / 10 );
}

/*=====================================================================*/

static void pgtca_vrfyAdvNumCb ( Widget text_w, XtPointer clnt,
                                                XtPointer call )
/************************************************************************
 * pgtca_vrfyAdvNumCb                                                   *
 *                                                                      *
 * This callback function is used to verify that a string,              *
 * entered as the contents of a text widget, is in a valid advisory     *
 * number format.  Any other characters will be                         *
 * rejected and trigger a sound beep.					* 
 *                                                                      *
 * void pgtca_vrfyAdvNumCb( text_w, clnt, call )            *
 *                                                                      *
 * Input parameters:                                                    *
 *      text_w          Widget    text widget                           *
 *      clnt     XtPointer Widget's event data (not used here)   *
 *      call       XtPointer callback structure                    *
 *                                                                      *
 * Output parameters:                                                   *
 *                      None.                                           *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP      01/06                                           *
 ***********************************************************************/
{
    int         iaflag, iadnum, ier;
    char        *wtext, ctest[5];

    XmTextVerifyCallbackStruct *cbs =
                                (XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/
                                                                                
    strcpy ( ctest, "\0" );
    if (cbs->text->ptr == NULL) {
        return;
    }

    /*
     *  Get the current contents of the text widget
     */
    XtVaGetValues (text_w, XmNvalue, &wtext, NULL);

    if ( strlen(cbs->text->ptr) == (size_t)0 ) {
        /*
         *  delete character(s)
         */
        if ( cbs->startPos > 0 )
             cst_ncpy( ctest, wtext, (int)cbs->startPos, &ier);
        strcat ( ctest, wtext+(cbs->endPos) );
    }
    else if ( cbs->startPos == cbs->endPos ) {
        /*
         *  insert character
         */
        strcpy ( ctest, wtext );
        cst_cins ( ctest+(cbs->startPos), cbs->text->ptr[0], &ier );
    }
    else {
        /*
         *  replace character(s)
         */
        if ( cbs->startPos > 0 )
             cst_ncpy( ctest, wtext, (int)cbs->startPos, &ier);
        strcat ( ctest, cbs->text->ptr );
        if ( (int)cbs->endPos <= (int)(strlen(wtext)-1) )
             strcat ( ctest, wtext+(cbs->endPos) );
    }

    if ( (int)strlen(ctest) == 0 ) return;

    /*
     *  Check if new text is valid advisory number format.
     */
    gh_advn ( ctest, &iadnum, &iaflag, &ier);

    /*
     *  The doit flag of the callback structure is used to signal
     *  to X to not allow the typed character to be entered into the
     *  text widget.
     */
    if ( ier != 0 ) 
        cbs->doit = FALSE;
    else
        cbs->doit = TRUE;
                                                                                
    XtFree(wtext);

}
                                                                                
/*=====================================================================*/


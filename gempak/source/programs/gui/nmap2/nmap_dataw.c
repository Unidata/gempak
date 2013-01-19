#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "Nxm.h"
#include "nmsdef.h"
#include "nmapprm.h"


#define ICON_WIDTH  32  /* action icon width, height */
#define ICON_HEIGHT 32  

#define ICON_DIR    "$NAWIPS/icons/nmap"
#define ICON_FGNAME "white"
#define ICON_BGNAME "blue"

#define ROAM_TBL	        "nmap_roam.tbl"
#define MAX_ROAMVAL	        22

#define NONE_SELECTED           "None"
#define NO_SOURCE		-1
#define SRC_BTN			0
#define DOM_BTN			1

#define AUTO_UPDT_OFF		0
#define AUTO_UPDT_ON 		1

#define DEFAULT_LEVEL	        SIZE_OF_SCREEN	/* default roam */

#define SRC_ON          	"Turn Source On" 
#define SRC_OFF         	"Turn Source Off"

#define ONE_X_ONE		0
#define ONE_X_TWO		1
#define TWO_X_ONE		2
#define TWO_X_TWO		3
#define FST_LOOP		4
#define LST_LOOP		5
#define ONE_LOOP		6

#define	LOAD			0
#define HELP			1
#define CANCEL			2

#define BINHOURLIMIT	        48
#define BINMINLIMIT	        59

static Widget		_dataW; 
static Widget		_numFrameW, _numFscaleW,   _maxFrameW;
static Widget		_skipRcW,   _skipArrow[2], _skipW;
static Widget		_numFrmsRcW;
static Widget		_loopTmModeRcW, _loopTmModeRb;

static Widget   	_srcOptn;
static WidgetList	_srcBtn;
static Widget		_srcState;

static Widget   	_domOptn;
static WidgetList	_domBtn;
static Widget		_modSrcBtn;

static Widget		_editBtn;

static Widget		_srcBinW;
static Widget		_binhrBfTxt, _binhrAfTxt;
static Widget		_binmnBfTxt, _binmnAfTxt;
static Widget		_binSrcBtn;
static Widget		_binSrcToggBtn;
static Widget		_mstRctToggBtn;

static Widget		_rangeBtn;
static Widget		_rangeSelW;
static Widget		_intvDayTxt, _intvHrTxt, _intvMinTxt; 
static Widget		_rangeDayTxt, _rangeHrTxt, _rangeMinTxt;
static Widget		_refTimeLbl, _refTimeTxt, _refTimeCal;

static Widget		_loopOptn, _autoOptn, _roamOptn;
static WidgetList	_loopBtn,  _autoBtn, _roamBtn;
static Widget		_currTm;

/*
static int		_pnlVal;
static Widget		_pnlOptn;
static WidgetList	_pnlBtn;
static int		_panelStruct;
*/

static int		_srcItems[MAX_LOOP], _srcItemsCopy[MAX_LOOP];

static int		_pnlDsply, _pnlDsplyCopy;

static int		_numFrmSel, _numFrmAvail;

static int		_numRoam;
static int    		_imgSzBtn;

static dsrc_t 		_dsrc[MAX_LOOP][MAX_FRMSRC];
static dsrc_t 		_dsrcCopy[MAX_LOOP][MAX_FRMSRC];

static dominfo_t	_domInfo[MAX_LOOP];
static dominfo_t	_domInfoCopy[MAX_LOOP];

static Boolean		_noDom = FALSE;
static Boolean		_tmlnActv;

static Boolean		_loadInProgress = FALSE;

static int		_currCatg;

static Boolean		_singleTm = FALSE;

static long		_roamValues[MAX_ROAMVAL]; /* roam factors in table*/

static int		_spfInterval = -1;  /* range value from SP file */
static int		_spfRange = -1;     /* interval value from SP file */
static int		_spfDelta = -1;	    /* delta ref. time from SP file */
static int		_spfIonoff  = -99;  /* bin hrs - on/off flag */ 
static int		_spfMstrctf = -99;  /* most recent only - on/off flag */
static int		_spfBfhr  = -99;    /* bin hrs- before current time */ 
static int		_spfBfmn  = -99;    /* bin mns- before current time */ 
static int		_spfAfhr  = -99;    /* bin hrs- after current time */ 
static int		_spfAfmn  = -99;    /* bin mns- after current time */ 

static int		_rangeMin = -1;     /* total range minutes on GUI */
static int		_intvMin  = -1;     /* total intv  minutes on GUI */

static char		_currTimeStr[DTTMSZ];
static dttmi_t		_refTimeStrc;
static int		_timeSetFlag = 0;   /* 0  --- no change
					     * 1  --- change to use ref time
					     * 2  --- change to use curr time
					     */

static Boolean		_saveUseRefTm[MAX_LOOP];
static dttmi_t		_saveRefTm[MAX_LOOP];

static Boolean		_usingSPF = FALSE;
static Boolean		_restoreUsingSPF = FALSE;

/*
 *  Private callback functions
 */
void		dataw_autoUpdtCb	( Widget, long, XtPointer );
static void	dataw_binbxToggCb	( Widget, XtPointer, XtPointer );
static void	dataw_mstRctToggCb	( Widget, XtPointer, XtPointer );
static void	dataw_binCtlBtnCb	( Widget, long, XtPointer ); 
static void	dataw_binSrcCb		( Widget, long, XtPointer );
void		dataw_bottomCtlCb	( Widget, long, XtPointer );
void		dataw_calPushbCb	( Widget, XtPointer, XtPointer );
void		dataw_clearCb		( Widget, long, XtPointer );
void		dataw_currTimeCb	( Widget, long, XtPointer );
void		dataw_editCb		( Widget, long, XtPointer );
void		dataw_intvTxtCb		( Widget, XtPointer, XtPointer );
void		dataw_loopCb		( Widget, long, XtPointer );
void		dataw_mapCb		( Widget, XtPointer,  XtPointer );
void		dataw_rangeCb		( Widget, long, XtPointer ); 
void		dataw_rangeCtlBtnCb	( Widget, long, XtPointer ); 
void		dataw_rangeTxtCb	( Widget, XtPointer, XtPointer );
void		dataw_refTimeTxtCb	( Widget, XtPointer, XtPointer );
void		dataw_roamCb		( Widget, long, XtPointer );
void		dataw_selTimesCb	( Widget, long, XtPointer );
void		dataw_skipArrowCb	( Widget, long, XtPointer );
void		dataw_sourceCb		( Widget, long, XtPointer );
void		dataw_sourceCtlCb	( Widget, long, XtPointer );
void		dataw_timeModeCb	( Widget, long, XtPointer );
void		dataw_topCtlCb		( Widget, long, XtPointer );

/*
 *  private functions 
 */
void dataw_addSource ( int menu, char *label, Boolean mk_curr, int *iret );
void dataw_addToMenu ( int menu, int idx, char *label, Boolean mk_curr);
void dataw_cancel ( void );
void dataw_clearAttr ( dsrc_t *src );
void dataw_clearDsrc ( dsrc_t *src );
void dataw_createDomBox( Widget parent );
void dataw_createPanel ( Widget parent );
void dataw_createRefTm ( Widget parent, Widget ref_wid );
void dataw_createSource( Widget parent  );
void dataw_createRange ( Widget parent );
void dataw_modSource ( int target_menu, int idx, char *label, int *iret);
void dataw_loadDomMenu ( void );
void dataw_loadDomTimes ( void );
void dataw_getDHM ( int total_min, int *day, int *hr, int *min );
void dataw_getDefltIntvRng ( int *intv_min, int *range_min );
void dataw_getSelIdx ( int popup, int *loop, int *idx );
int  dataw_getPopupIdx ( int popup, int idx );
void dataw_initDsrc ( void );
Boolean dataw_isImgSrcOn ( int loop );
void dataw_removeSrc ( int loop, int frmsrc );
void dataw_resetAutoUpdate ( int lp );
void dataw_resetDom ( void );
void dataw_resetRoam ( int lp );
void dataw_resetTmMode ( int lp );
void dataw_resetTmln ( void );
void dataw_setAttr ( dsrc_t *src );
void dataw_setDsplyMenu ( int btn );
void dataw_setEdit ( void );
void dataw_setFrameScale ( int sel, int max );
void dataw_setLoopMenu ( int lp );
void dataw_setRefTm ( dttmi_t *dttm );
void dataw_setSkipFactor ( int skip, Boolean adjust );
void dataw_setTmlnSnstv ( Boolean state );
void dataw_setTmModeSnstv ( Boolean state );
void dataw_setAutoMenu ( int state );
void dataw_updSkipFactor ( int skip );
void dataw_updtLoopOptn ( void );
void dataw_updtPnlOptn ( void );
void dataw_updtSources ( int prev );
static void dataw_updtImgRoam ( void );
static void dataw_resetBtn ( void );
static void dataw_setAutoUpdt ( int state );
static void dataw_loadSrcAttr ( dsrc_t *datasrc, int lp, int srcnum );
static Boolean dataw_checkRoamValue ( int value );
static void dataw_setTitles ( void );
static void dataw_getDefltBinInfo ( int *hrbf, int *mnbf, int *hraf,
				    int *mnaf, int *ionoff, int *mstrct ); 
static void dataw_createSrcBin ( Widget parent );
static void dataw_setBinSrc ( void );

/************************************************************************
 * nmap_dataw.c                                                         *
 *                                                                      *
 * This module creates the frame set definition  popup window and       *
 * defines the callback functions for nmap. 				*
 *                                                                      *
 * CONTENTS:                                                            *
 *   dataw_create()   	  create the frame set definition popup window. *
 *   dataw_popup()    	  pop up the frame set definition popup window. *
 *   dataw_popdown()  	  pop down the frame set definition popup window*
 *   dataw_toggle()   	  if window is down, pop up, else pop down	*
 *   dataw_setDataSrc()   store the information about a data source     *
 *   dataw_loadTimes()    load the available times for a data source    *
 *   dataw_updSelTimes()  update the selected times in the time line    *
 *   dataw_clearLoop()    clear the data source info & draw base map	*
 *   dataw_loadData()     load all selected data			*
 *									*
 *   dataw_isUp()         querry the status of dataw window		*
 *   dataw_getDataSrc()   get the index for a specific data source      *
 *   dataw_enableLoop ()  make loop menu sensistive			*
 *   dataw_disableLoop()  make loop menu insensistive			*
 *   dataw_getNumSrcs()   get the number of sources in a loop	        *
 *   dataw_getNumLoops()  get the number of loaded loops		*
 *   dataw_getDomSrc()    get the dominant source for a loop		*
 *   dataw_getPanelLoc()  get the location of a panel within the pixmap *
 *   dataw_isImgDom()     true if image source is dominant		*
 *   dataw_isImgInLoop()  check for image data				*
 *   dataw_isImgSrcOn()	  check that image data is active (turned on)	*
 *   dataw_isLoopActv()   check if a specific loop has data		*
 *   dataw_isRadSelect()  check for radar data				*
 *   dataw_isSatSelect()  check for satelite data			*
 *   dataw_getImgInfo()   get file information on a loaded image	*
 *   dataw_getIRInfo()    get specific frame information on an IR image *
 *   dataw_getStnmName()  gets the station model name			*
 *   dataw_getSkip()      get the skip factor for a loop                *
 *   dataw_getDataW()     get a pointer to the _dataW Widget            *
 *   dataw_useRefTm()     true if set time has been selected            *
 *   dataw_getRefTm()     get a pointer to the reference time           *
 *   dataw_getFrameTime() get GEMPAK dttm of the frame  		*
 *									*
 *   dataw_loopCb() 	  callback for loop menu			*
 *   dataw_mapCb()  	  callback for map button			*
 *   dataw_sourceCb()     callback for source menu			*
 *   dataw_sourceCtlCb()  callback for add and modify buttons		*
 *   dataw_editCb()       callback for the edit parameter button	*
 *   dataw_topCtlCb()	  callback for top dataw buttons		*
 *   dataw_bottomCtlCb()  callback for bottom dataw buttons		*
 *   dataw_skipArrowCb()  callback for skip arrow buttons		*
 *   dataw_clearCb()      callback for the clear button			*
 *   dataw_selTimesCb()   callback for the selected times button	*
 *   dataw_roamCb()       callback for the "Roam:" option menu          *
 *   dataw_currTimeCb()	  callback for the current time buttons       	*
 *   dataw_timeModeCb()	  callback for the single time loop buttons	*
 *   dataw_rangeCb()	  callback for range/interval selection button	*
 *   dataw_rangeCtlBtnCb()callback for buttons in range/intv input win.	*
 *   dataw_intvTxtCb()    callback for Interval text boxes		*
 *   dataw_rangeTxtCb()   callback for Range txt boxes			*
 *   dataw_refTimeTxtCb() callback for ref time text box		*
 *   dataw_calPushbCb()	  callback for calendar				*
 *   dataw_binSrcCb()     callback for the "Bin Source" button		*
 *   dataw_binCtlBtnCb()  callback for buttons in source bin input win.	*
 *   dataw_binBxToggCb()  callback for source binning ON/OFF check box 	*
 *   dataw_mstRctToggCb() callback for only most recent check box 	*
 *									*
 *   dataw_createPanel()  create the panel selection 			*
 *   dataw_createSource() create the source selction			*
 *   dataw_createDomBox() create the dom box portion of dataw		*
 *   dataw_createRefTm()  create the reference time controls   		*
 *   dataw_createRange()  create the timeline range/interval input win.	*
 *   dataw_createSrcBin() create the source bin input window		*
 *									*
 *   dataw_addSource()    add an item to the source/dom menu		*
 *   dataw_modSource()    modify an existing data source		*
 *   dataw_removeSrc()    remove a data source				*
 *   dataw_updtSources()  update the source menues			*
 *   dataw_addToMenu()    add an item to the source/dom menu 		*
 *   dataw_loadDomMenu()  load the dominant source menu			*
 *   dataw_setEdit()	  deals with the edit button and popup		*
 *									*
 *   dataw_getSelIdx()    get the selected source's index number	*
 *   dataw_getPopupIdx()  get the index number for a current source item*
 *									*
 *   dataw_initDsrc()     initialize the data source structure		*
 *   dataw_clearAttr()	  clear attributes for a specific source	*
 *   dataw_clearDsrc()    clear a specific data source			*
 *   dataw_setAttr()      set the attributes for a data source		*
 *									*
 *   dataw_setSkipFactor()set the skip factor				*
 *   dataw_updSkipFactor()update the skip factor			*
 *									*
 *   dataw_resetDom()     reset the dom menu and timeline		*
 *   dataw_resetRoam()    reset the "Roam:" option menu                 *
 *   dataw_resetTmMode()  reset the single time flag for loops		*
 *   dataw_resetTmln()    reset the timeline based on range/intv inputs	*
 *   dataw_updateTmln()   update the timeline				*
 *   dataw_setFrameScale()set the number of frames selected		*
 *   dataw_setTmlnSnstv() set the timeline sensitive/insensitive	*
 *   dataw_setTmModeSnstv()set the single time sensitive/insensitive	*
 *   dataw_getTimeStr()   gets the appropiate time string		*
 *   dataw_getDHM()	  gets the day, hr, min value from total min	*
 *   dataw_getDefltIntvRng() get default interval/range info		*
 *   dataw_loadDomTimes() loads the dominant times of the current loop	*
 *									*
 *   dataw_updtPnlOptn()  update the panel selection			*
 *   dataw_updtLoopOptn() update the selected loop			*
 *									*
 *   dataw_setLoopMenu()  set up the loop menu				*
 *   dataw_setRefTm()	  set the reference time for the current loop	*
 *   dataw_setAutoMenu()  sets auto-update menu for the current loop	*
 *   dataw_cancel()       cancel active operations and exit window      *
 *   dataw_updtImgRoam()  update the size of image option in roam menu  *
 *   dataw_resetBtn()	  reset modify and edit buttons if state changes*
 *   dataw_setLoop()	  switches from current loop to the given loop	*
 *   dataw_loadsp()	  loads data settings from a given SPF file	*
 *   dataw_loadSrcAttr()  loads attribute settings for a data source	*
 *   dataw_setLpBtns()	  sets the loop button state to current loop	*
 *   dataw_noActSrc()	  finds if any active sources exist		*
 *   dataw_clearDataSel() eliminates data selections temperarily	*
 *   dataw_getRoamVal()   gets the current selection of roam factor	*
 *   dataw_setAutoUpdt()  sets the value of auto update state		*
 *   dataw_resetAutoUpdate() resets auto update menu & state		*
 *   dataw_checkRoamValue()  check if a roam factor is valid		*
 *   dataw_cmdLineLoadSPF()  load spf from command line option		*
 *   dataw_setTitles()    sets NMAP2 icon title & NMAP2 window title	*
 *   dataw_getDefltBinInfo() gets default binning info from datatype.tbl*
 *   dataw_setBinSrc()	  deals with the "Bin Source" button sensetivity*
 ***********************************************************************/

/*=====================================================================*/

void dataw_create ( Widget parent ) 
/************************************************************************
 * dataw_create                                              		*
 *                                                                      *
 * This function creates the frame set definition popup window.    	*
 *                                                                      *
 * void dataw_create(parent)     	              			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      frame set definition popup widget ID            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/96  						*
 * C. Lin/EAI		04/97 	create data category buttons 		*
 * C. Lin/EAI		06/97 	change category frame left offset 	*
 * C. Lin/EAI		07/97 	add creating VGF edit popup window 	*
 * G. Krueger/EAI	10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * G. Krueger/EAI	11/97  	Renamed NxmHelp functions               *
 * C. Lin/EAI		12/97 	initialize _datawRADsource 		*
 * C. Lin/EAI		06/98 	initialize frame time info 		*
 * S. Law/GSC		12/98	cleanup and initializing _dataList	*
 * S. Jacobs/NCEP	 5/99	Attach map button to row-col widget	*
 * E. Safford/GSC	08/99	overhaul for new nmap         		*
 * E. Safford/GSC	01/00	add loop changed initialization		*
 *				 temporarily remove Save & Load Settings*
 * S. Jacobs/NCEP	 3/00	Removed call to vgfw_create		*
 * J. Wu/GSC		 6/01	activate "Restore/Save" SPF buttons	*
 * J. Wu/SAIC		 8/01	adjust spacing of "Restore/Save" buttons*
 * T. Piper/SAIC	12/01	freed btm, top				*
 * J. Wu/SAIC		 7/03	create timeline range/intv input win.	*
 * J. Wu/SAIC		 9/04	create time bin input win.		*
 * J. Wu/SAIC		12/04	re-activate time binning		*
 ***********************************************************************/
{
    Widget	pane;
    WidgetList  top, btm;
    char	*top_btns[] =
      		   {"Restore Data Settings", "Save Data Settings"}; 
    char	*bottom_btns[]={"LOAD", "Help", "Cancel"};
    int		lp;
/*---------------------------------------------------------------------*/
/*
 *  Create dialog shell
 */
    _dataW = XmCreateFormDialog(parent, "dataw_popup", NULL, 0);
    XtVaSetValues(_dataW, XmNnoResize, True, NULL);
    XtVaSetValues(XtParent(_dataW),
		XmNtitle, "Data Selection Window",
		NULL);
    
/*
 *  Create a pane widget
 */
    pane = XtVaCreateWidget ("dataw_pane",
		xmPanedWindowWidgetClass, _dataW,
		XmNsashWidth,  			1,
		XmNsashHeight, 			1,
		NULL);

/*
 *  Create "Restore/Save Settings" buttons at the top 
 */
    top = (WidgetList)XtMalloc(XtNumber(top_btns)* sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "top_ctlBtns", XtNumber(top_btns),
    		top_btns, (XtCallbackProc)dataw_topCtlCb, top);
    
    XtVaSetValues ( top[0],
    		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		40,
    		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	2, 
    		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		7,
    		XmNbottomAttachment,	XmATTACH_FORM,
		XmNbottomOffset,	7,
		NULL);

    XtVaSetValues ( top[1],
    		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	5,
    		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		40,
    		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		7,
    		XmNbottomAttachment,	XmATTACH_FORM,
		XmNbottomOffset,	7,
		NULL);
    XtFree((XtPointer)top);
/*
 *  Create SPF file selection popup window
 */
    spfw_create(pane);
    
/*
 *  Create panel/source selection GUI
 */
    dataw_createPanel (pane);
    dataw_createSource(pane);

/*
 *  Initialize the loop changed flags to FALSE
 */
    for (lp=0; lp < MAX_LOOP; lp++) {
        loop_setDataChngd (lp, FALSE);
    }

/*
 *  Create "Load/Help/Cancel" buttons at the bottom 
 */
    btm = (WidgetList)XtMalloc(XtNumber(bottom_btns)* sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "bottom_ctlBtns", XtNumber(bottom_btns),
    		bottom_btns, (XtCallbackProc)dataw_bottomCtlCb, btm);
    XtSetSensitive (btm[1], FALSE);
    XtFree((XtPointer)btm);

/*
 *  Create data type selection popup window
 */
    dslw_create(pane);

    XtManageChild (pane);

/*
 *  Create the attribute edit windows
 */
    stnmw_create(parent);

/*
 *  Create the timeline range/interval input windows
 */
    dataw_createRange(parent);

/*
 *  Create the source bin input windows.
 */
    dataw_createSrcBin (parent);
   
}

/*=====================================================================*/

void dataw_popup ( void )
/************************************************************************
 * dataw_popup 								*
 *                                                                      *
 * This function pops up the frame set definition window.    		*
 *                                                                      *
 * void dataw_popup()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/96						*
 * C. Lin/EAI		01/97	add MODEL data type			*
 * C. Lin/EAI		04/97	changes added for data category concept	*
 * C. Lin/EAI		05/97	add case SHIP				*
 * C. Lin/EAI		06/97	add case VGF				*
 * C. Lin/EAI		07/97	allow editing VGF data			*
 * C. Lin/EAI		07/97	use new mapw module			*
 * G. Krueger/EAI	10/97	NxmSetLabel->NxmLabel_setStr		*
 * C. Lin/EAI		12/97	add RADAR source			*
 * C. Lin/EAI		02/98	add case FFG				*
 * C. Lin/EAI		04/98	modified to use data source name, modes	*
 * C. Lin/EAI		05/98	add special checks for model data	*
 * C. Lin/EAI		08/98	modify to use the new time line module	*
 * E. Safford/GSC	10/98	add pgpalw_classPopdown			*
 * S. Law/GSC		12/98	cleanup					*
 * S. Law/GSC		12/98	changed parameters for dsrc_getNframe	*
 *				and added call to dataw_resetFrame	*
 * E. Safford/GSC	01/99	add auto update    			*
 * E. Safford/GSC	02/99	redraw timeline if _autoUpdate       	*
 * S. Jacobs/NCEP	 5/99	Added WTCH_WARN data type		*
 * E. Safford/GSC	10/99	removed references to _curLp		*
 * H. Zeng/EAI          11/99   added dataw_resetRoam()                 *
 * E. Safford/GSC	04/00	remove _initialLoad			*
 * E. Safford/GSC	05/00	save starting garea and projection	*
 * H. Zeng/EAI          05/00   added pgpalw_isUp() check               *
 * E. Safford/GSC	05/00	added deselectAll                   	*
 * M. Li/GSC		07/00	Added Saving for data structure		*
 * E. Safford/GSC	08/00	added _startZoom assignment         	*
 * M. Li/GSC            11/00   added mapw_ctlBtnUpd                    *
 * T. Lee/GSC		02/01	added single time for loops		*
 * E. Safford/GSC	03/01	added roamw_disableMenu			*
 * T. Lee/GSC		03/01	changed call seq. for roam and time mode*
 * E. Safford/GSC	04/01	added dataw_updtImgRoam			*
 * M. Li/GSC            04/01   added nmp_svmap                         *
 * M. Li/GSC		05/01	replaced nmp_svmap with nmp_save	*
 * M. Li/GSC		06/01	add dataw_resetBtn			*
 * J. Wu/SAIC		08/01	add call to loop_getRoamVal		*
 * E. Safford/SAIC	08/01	add call to dataw_loadRoamVal()		*
 * E. Safford/SAIC	10/01	treat WM close as a cancel		*
 * M. Li/SAIC		01/02	Added loop_saveRoamVal, remove		*
 *				      dataw_loadRoamVal			*
 * E. Safford/SAIC	01/02	add pgpalw_inactvGrp()			*
 * E. Safford/SAIC	06/02	rm _currTm variable			*
 * T. Lee/SAIC		09/03	temporarily unmanage _rangeBtn		*
 * T. Lee/SAIC		09/03	Managed _rangeBtn			*
 * T. Lee/SAIC		12/03	Removed range/interval button		*
 * T. Lee/SAIC		01/04	Added reference time			*
 ***********************************************************************/
{
int	ii, jj, cur_lp, ier;
Widget	shell;
Atom	WM_DELETE_WINDOW;
/*---------------------------------------------------------------------*/

    if (pgpalw_isUp()) {
	pghdlb_deselectAll();
	pgpalw_inactvGrp();
	pgpalw_classPopdown();
    }

    if ( !dataw_isUp() ) {
        XtManageChild(_dataW);

/* 
 *  Treat a window manager close event (WM_DELETE_WINDOW)
 *  as a cancel.
 */
	shell = XtParent (_dataW);
	WM_DELETE_WINDOW = XmInternAtom (XtDisplay(_dataW), 
				"WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback (shell, WM_DELETE_WINDOW, 
		(XtCallbackProc)dataw_bottomCtlCb, (XtPointer)CANCEL);


	if (mapw_isUp()) {
            mapw_ctlBtnUpd();
	}

/*
 *  Save data driver structure 
 */
	ngd_save (&ier);
	nim_save (&ier);
	nms_save (&ier);
	nsf_save (&ier);
	nsn_save (&ier);
	nmp_save (&ier);
	loop_saveRoamVal();
	
/*
 *  Disable things that will cause conflicts
 */
	cur_lp = loop_getCurLoop();
  	auto_stopAutoUpdt();
	mbtnw_zoomSensitive(FALSE);
	mbtnw_loopSetSensitive(FALSE);
	loopw_sensitive(FALSE);	
	roamw_disableMenu();

	_singleTm = loop_getTmMode (cur_lp);
	if (_singleTm) {
	    dataw_setTmlnSnstv (FALSE);
	}
	else if (dataw_getNumSrcs(cur_lp) > 0) {
	    dataw_setTmlnSnstv (TRUE);
	}

/*
 *  Copy current data
 */
	_pnlDsplyCopy = _pnlDsply;

  	dataw_setLoopMenu(cur_lp); 

	for (ii=0; ii < MAX_LOOP; ii++) {
	    for (jj=0; jj<MAX_FRMSRC; jj++) {
		_dsrcCopy[ii][jj] = _dsrc[ii][jj];		
	    }
	    _srcItemsCopy[ii] = _srcItems[ii];
	}

	for (ii=0; ii < MAX_LOOP; ii++) {
	    _domInfoCopy[ii] = _domInfo[ii];
	}

	loop_saveTmMode();

    	dataw_updtSources(-1);
	dataw_resetBtn();
    
	dataw_resetAutoUpdate(cur_lp);
        dataw_resetTmMode(cur_lp);

        dataw_resetRoam(cur_lp);
        dataw_updtImgRoam();

    }
}

/*=====================================================================*/

void dataw_popdown ( void )
/************************************************************************
 * dataw_popdown							*
 *									*
 * This function pops down the frame set definition window.		*
 *									*
 * void dataw_popdown()                   				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/96						*
 * S. Law/GSC		12/98	added call to _isUp and _storeFrame	*
 * M. Li/GSC            11/00   added mapw_ctlBtnUpd                    *
 * E. Safford/GSC	03/01	added roamw_enableMenu			*
 * T. Piper/SAIC	04/03	replaced roamw_enableMenu w/roamw_setup	*
 * J. Wu/SAIC		07/03	add unmanagement for _rangeSelW		*
 ***********************************************************************/
{
int             cur_lp;
/*---------------------------------------------------------------------*/

    if ( dataw_isUp() ) { 
	XtUnmanageChild (_rangeSelW);
	XtUnmanageChild (_dataW);

	if (mapw_isUp()) {
            mapw_ctlBtnUpd();
	}

/*
 *  Enable menu items 
 */
        mbtnw_zoomSensitive(TRUE);
        mbtnw_loopSetSensitive(TRUE);
        loopw_sensitive(TRUE);	
	cur_lp = loop_getCurLoop();
	roamw_setup(cur_lp, TRUE);
   
/*
 *  Restart auto update
 */ 
	if (!_loadInProgress) {
            auto_startAutoUpdt();
	}
    }
}

/*=====================================================================*/

void dataw_toggle ( void )
/************************************************************************
 * dataw_toggle 							*
 *									*
 * This function toggles the window down or up, to the state it      	*
 * currently isn't.							*
 *									*
 * void dataw_toggle()                   				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * S. Law/GSC		12/99	added seek popdown			*
 * E. Safford/GSC	08/00	route toggle down thru dataw_cancel 	*
 ***********************************************************************/
{
    if (dataw_isUp ()) {
	dataw_cancel();
    }
    else {
	seekw_popdown ();
	dataw_popup ();
    }
}

/*=====================================================================*/

void dataw_setDataSrc ( int src_type, int dcatg, char *path )
/************************************************************************
 * dataw_setDataSrc							*
 *									*
 * This function is called from nmap_dslw.c (data selection window)     *
 * when the Accept btn is selected.  It sets up the data source 	*
 * information and menus for the new or modified data source.		*
 *									*
 * void dataw_setDataSrc( src_type, dcatg, path )        		*
 *									*
 * Input parameters:							*
 * 	src_type	int	NEW_SOURCE or MOD_SOURCE		*
 *      dcatg           int     category of data                        *
 *      path            char*   path to data                            *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/99	initial coding				*
 * S. Jacobs/NCEP	10/99	Changed ss_gtim to css_gtim		*
 * E. Safford/GSC	10/99	add resetAutoUpdate        		*
 * E. Safford/GSC	11/99	fix bug in selected times 		*
 * S. Law/GSC		11/99	added call to dataw_setEdit		*
 * E. Safford/GSC	11/99	turn on modify btn if 0 times in source *
 * S. Law/GSC		11/99	changed defines to match new table	*
 * E. Safford/GSC	01/00	use loop_setDataChngd			*
 * E. Safford/GSC	01/00	fix time order problem w/ MSC data	*
 * E. Safford/GSC	01/00	add check for null path            	*
 * E. Safford/GSC       02/00   re-initialize times & sel flgs on mod   *
 * E. Safford/GSC	03/00	split off get time functionality	*
 * A. Hardy/GSC         04/00   added stnmw_popdown to cat_msc		*
 * E. Safford/GSC	05/00	added call to dsp_setProj for images	*
 * E. Safford/GSC	05/00	added call to loop_restoreLut for images*
 * E. Safford/GSC	05/00	param change to dsp_setProj             *
 * E. Safford/GSC	05/00	limit calls to dsp_setProj for img data *
 * E. Safford/GSC	05/00   re-fix limiting calls to dsp_setProj	*
 * S. Law/GSC		06/00	changed to call dataw_getTimeStr	*
 * H. Zeng/EAI          06/00   changed passed-in parameters            *
 * A. Hardy/GSC         08/00   changed dttms_t -> dattm_t		*
 * S. Jacobs/NCEP	 1/01	Added setting attridx = -1 for new src	*
 * E. Safford/GSC	04/01	added dataw_updtImgRoam			*
 * M. Li/GSC            03/01   set proj using NMP library              *
 * S. Jacobs/NCEP	 5/01	Changed call to dataw_getImgInfo	*
 * S. Jacobs/NCEP	 5/01	Added call to mbotw_getFadeColor	*
 * M. Li/GSC		06/01	set src_on to TRUE			*
 * E. Safford/GSC	06/01	update params for dataw_getTimeStr()	*
 * E. Safford/SAIC	08/01	remove path = NULL assignment		*
 * S. Jacobs/NCEP	10/01	Added call to dataw_setImageNav		*
 * J. Wu/SAIC		07/03	retrieve default range/intv for source	*
 * J. Wu/SAIC		07/03	restore range/intv from SP file		*
 * J. Wu/SAIC		07/03	guard agaist invalid intv/range in table*
 * J. Wu/SAIC		07/03	allow interval=range for SPF restoring	*
 * H. Zeng/XTRIA	12/03   minor modification to intv val. setting *
 * T. Lee/SAIC		04/04	time line unchanged when update bottom	*
 * 				layer of data source			*
 * T. Lee/SAIC		05/04	added delta reference time		*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * J. Wu/SAIC		10/04	add bin hours to data src. attribute	*
 * m.gamazaychikov/SAIC	12/04	add dionoff flag to ctb_dtget CS	*
 * J. Wu/SAIC		12/04	add on/off flag to data src. attribute	*
 * A. Hardy/NCEP	02/05   restore bin spf info. to data struct 	*
 * m.gamazaychikov/SAIC 01/06   Changed dtemp string length to MXTMPL   *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * T. Piper/SAIC	08/06	moved strcpy of xpath back to original	*
 *					location in 'else' case		*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 * F. J. Yen/NCEP	04/08	Added bin minutes & most recent flag; if*
 *				SPF hours is 0,restore it instead of dft*
 ***********************************************************************/
{
    int		src, lp, ier, view, cur_lp, imgtyp, skip, nsel;
    int		dcatgry, dsubcat, dnframe, drange, dintrvl, dionoff;
    int		dhrsbfr, dhraftr, dtmch;
    int		dmnsbfr, dmnaftr, dmstrctf;
    char	imgfile[256], src_state[256];
    char	nupath[256], xpath[256];
    char	*pstr, tmp[12], src_str[LLPATH];
    char	dalias[13], dpath[26], dtemp[MXTMPL];    
    dattm_t	time;
    dsrc_t	*active_src;
    Boolean	had_img, now_img, no_change;
/*---------------------------------------------------------------------*/

    nsel = skip = 0;
    no_change = FALSE;
    cur_lp  = loop_getCurLoop();
    had_img = dataw_isImgInLoop(cur_lp);

    loop_setDataChngd (cur_lp, TRUE); 

    if (src_type == NEW_SOURCE) {
    
	src = _srcItems[cur_lp];
        _dsrc[cur_lp][src].catg = dcatg;
        strcpy(_dsrc[cur_lp][src].path, path);
	active_src = &_dsrc[cur_lp][src];

	if (strlen(active_src->path) < (size_t)1) {
	    return;
	}

	nupath[0] = '\0';
	xpath[0]  = '\0';

	active_src->attridx = -1;
	
        dataw_addSource(SRC_BTN, active_src->path, TRUE, &ier);

	active_src->src_on = TRUE;
    	NxmLabel_getStr(_srcState, src_state);

    	if ( strcmp(src_state, SRC_ON) == 0) {
            NxmLabel_setStr(_srcState, SRC_OFF);
    	}
    }
    else {

	dataw_getSelIdx(SRC_BTN, &lp, &src);
	strcpy ( xpath, _dsrc[lp][src].path );
	_dsrc[lp][src].catg = dcatg;
	strcpy(_dsrc[lp][src].path, path);
	active_src = &_dsrc[lp][src];

	if (strlen(active_src->path) < (size_t)1) {
	    return;
	}
	strcpy ( nupath, path );

/*
 *  Get abridged path.
 */
	if ( dcatg == CAT_IMG || dcatg == CAT_SFC ||
	     dcatg == CAT_SFF || dcatg == CAT_SND ||
	     dcatg == CAT_SNF ) {
	    no_change = TRUE;
	    pstr = strrchr ( xpath, '/' );
	    if ( pstr != NULL ) {
		*pstr = '\0';
	    }
	    pstr = strrchr ( nupath, '/' );
	    if ( pstr != NULL ) {
		*pstr = '\0';
	    }
	}
        else if ( dcatg == CAT_GRD || dcatg == CAT_ENS ) {
	    no_change = TRUE;
	    pstr = strrchr ( xpath, '/' );
	    if ( pstr != NULL ) {
		*pstr = '\0';
	    }
	    pstr = strrchr ( xpath, '/' );
	    if ( pstr != NULL ) {
		*pstr = '\0';
	    }
	    pstr = strrchr ( nupath, '/' );
	    if ( pstr != NULL ) {
		*pstr = '\0';
	    }
	    pstr = strrchr ( nupath, '/' );
	    if ( pstr != NULL ) {
	        *pstr = '\0';
	    }
	}

	if (active_src->catg != _currCatg) {
	    active_src->attridx = -1;
	}
	dataw_modSource(SRC_BTN, src, active_src->path, &ier); 
    }

/*
 *  Get default time interval/range and time binning hours for this source
 */
    if ( active_src->catg == CAT_VGF) {
	strcpy ( tmp, "VGF" );
    }
    else if ( active_src->catg == CAT_ENS ) {
	dslw_getFrstMod ( active_src->path, tmp );
    }
    else {
        strcpy ( src_str, active_src->path );
        pstr = strtok ( src_str, "/" );
        pstr = strtok ( NULL, "/" );
	strcpy ( tmp, pstr );
    }

    cst_lcuc ( tmp, dalias, &ier );
    ctb_dtget ( dalias, dpath, dtemp, &(dcatgry), &(dsubcat),
			&(dnframe), &(drange), &(dintrvl), 
			&(dionoff), &(dhrsbfr), &(dmnsbfr),
			&(dhraftr), &(dmnaftr), &(dmstrctf),
			&(dtmch), &ier );
                
    if ( drange <= 0 ) {   /* no negative value for range */
        if ( dcatg == CAT_MSC || dcatg == CAT_IMG || 
                 dcatg == CAT_SFC || dcatg == CAT_SND )	{
	    drange = 360;
	}
    }

    if ( dintrvl > drange ) {
        dintrvl = drange;   /* intv. always smaller than range */ 
    }
    
/*
 *  Do not update range/intevals/binning hours if only update the
 *  bottom layers of the data source.
 */
    if ( _domInfo[cur_lp].src != NULL ) {
	if ( ( active_src == _domInfo[cur_lp].src ) &&
	     ( strcmp ( xpath, nupath ) == 0 ) && 
	     no_change ) {
	}
	else {
	    active_src->range = drange;    
	    active_src->interval = dintrvl;    
            active_src->bfhr = dhrsbfr;    
            active_src->bfmn = dmnsbfr;    
            active_src->afhr = dhraftr;    
            active_src->afmn = dmnaftr;    
            active_src->ionoff = dionoff;    
            active_src->mstrctf = dmstrctf;    
            active_src->domtmmtch = dtmch;    
	}
    }
    else {
	active_src->range = drange;    
	active_src->interval = dintrvl;    
        active_src->bfhr = dhrsbfr;    
        active_src->bfmn = dmnsbfr;    
        active_src->afhr = dhraftr;    
        active_src->afmn = dmnaftr;    
        active_src->ionoff = dionoff;    
        active_src->mstrctf = dmstrctf;    
        active_src->domtmmtch = dtmch;    
    }
    
/*
 *  Restore interval/range valid values from SP file & reset them
 *  to -1 afterward. 
 */
    if ( _spfInterval <= _spfRange ) {	
        if ( _spfInterval > 0 ) {
            active_src->interval = _spfInterval;
        }
        
	if ( _spfRange > 0 ) {
            active_src->range = _spfRange;
	}

	active_src->delta_rt = _spfDelta;
    } 

    _spfInterval = -1;
    _spfRange = -1;
    _spfDelta = -1;

/*
 *  Restore bin valid values from SP file & reset them to -99 afterward. 
 */
    if (  _spfIonoff > 0 ) {	
        if ( _spfBfhr >= 0 ) {
            active_src->bfhr= _spfBfhr;
        }
        if ( _spfBfmn >= 0 ) {
            active_src->bfmn= _spfBfmn;
        }
	if ( _spfAfhr >= 0 ) {
            active_src->afhr = _spfAfhr;
	}
	if ( _spfAfmn >= 0 ) {
            active_src->afmn = _spfAfmn;
	}

	active_src->ionoff = _spfIonoff;
	active_src->mstrctf = _spfMstrctf;
    } 
    _spfIonoff = -99;
    _spfMstrctf= -99;
    _spfBfhr   = -99; 
    _spfBfmn   = -99; 
    _spfAfhr   = -99; 
    _spfAfmn   = -99; 

/*
 *  Add this source to menu and update timeline.
 */
    dataw_setAttr(active_src);
    now_img = dataw_isImgInLoop(cur_lp);
    if ( now_img ) {
	now_img = dataw_isImgSrcOn(cur_lp);
    }
    if ( now_img ) {

/*
 *  If this source is the image then set the projection & lut
 */
	if (active_src->catg == CAT_IMG ) {
	    dataw_setImageNav ( );
	}
    }
    else if ( had_img && !now_img ) {

/*
 *  There was an image -- but it was just replaced.
 *  Wipe the image file info in the nmp lib, and any zoom area.
 */
        strcpy (imgfile, " ");
	imgtyp = NO_IMG;

        nmp_simf ( cur_lp, imgfile, imgtyp, &ier );
        nmp_sproj( cur_lp, &ier ); 

	zoomw_clearZoom( cur_lp );
    }

    dataw_resetBtn();
    dataw_setEdit ( );
    dataw_setBinSrc ( );

    view = loop_getCurLoop();
    if (!_noDom && _domInfo[view].src == NULL) {
        _domInfo[view].src = active_src; 
    }

    dataw_getTimeStr (cur_lp, FALSE, time);

/*
 *  Time line is unchanged if only updating the bottom layers of
 *  the data source.
 */
    if ( ( active_src == _domInfo[cur_lp].src ) &&
	 ( strcmp ( xpath, nupath ) == 0 ) &&
	 no_change ) {
    }
    else if ( src_type == MOD_SOURCE && 
	    ( active_src == _domInfo[cur_lp].src ) ) {
	_domInfo[cur_lp].use_refTm = FALSE;
	_domInfo[cur_lp].src->delta_rt= -1;
	dataw_loadTimes(active_src, time, nsel, skip);
    }
    else {
	if ( active_src == _domInfo[cur_lp].src ) {
	    _domInfo[cur_lp].src->delta_rt= active_src->delta_rt;
	}
	dataw_loadTimes(active_src, time, nsel, skip);
    }
    dataw_loadDomMenu();

    if (src_type == NEW_SOURCE) {
	XtSetSensitive(_srcState, TRUE);
    }

    dataw_resetAutoUpdate(cur_lp);
    dataw_updtImgRoam();

}

/*=====================================================================*/

void dataw_loadTimes ( dsrc_t *source, char *time, int nsel, int skip )
/************************************************************************
 * dataw_loadTimes 							*
 *									*
 * This function gets the available times for the data source.          *
 *									*
 * void dataw_loadTimes( source, time, nsel, skip )			*
 *									*
 * Input parameters:							*
 * 	*source		dsrc_t	pointer to the data source struct	*
 *	*time		char	gempak time for data frame selection	*
 *	nsel		int	number of selected frames		*
 *				   if 0 the default will be used	*
 *	skip		int	skip factor for loop			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	03/00	initial (pulled from dataw_setDataSrc)  *
 * E. Safford/GSC	04/00	fix select prob w/ -1 nframes tbl val   *
 * S. Jacobs/NCEP	05/00	Changed to use time range from table	*
 * S. Law/GSC		06/00	moved getting times to tmln_getNewTimes	*
 * S. Jacobs/NCEP	 7/00	Check sub-cat number instead of category*
 * S. Law/GSC		06/00	initialize skip factor			*
 * R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
 * T. Lee/GSC		02/01	added single time for loops		*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 * F. J. Yen/NCEP	04/08   Add bin mins &most rct flag to ctb_dtget*
*************************************************************************/
{
    int		ii, nframe, times, frm_ctr, skip_ctr, cur_lp;
    int		isbcat, d3, d4, d5, d6, d7, d7m, d8, d8m, dmrf, dionoff,
		dtmch, ier;
    char	asrc[256], *alias, d1[256], d2[256], tmpstr[256];
    dttms_t	tmarry[MXNMFL];
    Boolean	tm_flag;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  Wipe any existing times & selection flags for source
 */
    for (ii=0; ii < MAX_FRAME; ii++) {
        source->frm[ii].ftime[0] = '\0';
	source->frm[ii].selected = FALSE;
    }

/*
 *  Get the available times for the source
 */
    nframe = tmln_getNewTimes (source, time, &times, tmarry);

/*
 *  Copy all available times into the source->frm struct
 */
    for (ii = 0; ii < times; ii++) {
	strcpy (source->frm[ii].ftime, tmarry[ii]);
    }

    source->skip = skip;

    if (times > 0) {
	if (nsel > 0 && nsel < MAX_FRAME) {
	    nframe = nsel;
	}

/*
 *  Traverse the times from the high end until requested       
 *  number of times have been selected.
 */
	if (skip < 0) skip = 0;
	skip_ctr = skip;
	frm_ctr = (nframe >= 0) ? nframe : times;

/*
 *  Get the data alias name to find the sub-cat number. 
 *  This is used to determine the direction of the time line.
 */
	if  ( source->catg == CAT_VGF )  {
	    strcpy ( tmpstr, "VGF" );
	}
	else if ( source->catg == CAT_ENS ) {
            dslw_getFrstMod ( source->path, tmpstr );
    	}
	else {
	    strcpy (asrc, source->path);
	    alias  = strtok(asrc, "/");
	    alias  = strtok(NULL, "/");
	    strcpy ( tmpstr, alias );
	}

	ctb_dtget ( tmpstr, d1, d2, &d3, &isbcat, &d4, &d5, &d6, 
		    &dionoff, &d7, &d7m, &d8, &d8m, &dmrf, &dtmch, &ier );

	tm_flag = loop_getTmMode (cur_lp);	
	if  ( isbcat == SCAT_FCT ||
	      isbcat == SCAT_SFF ||
	      isbcat == SCAT_SNF )  {

/*
 *  For a forecast grid on the single time loop, only single time
 *  is selected.
 */
	    for (ii = 0; ii < times && frm_ctr > 0; ii++) {
		if (skip_ctr == skip) {
		    if (tm_flag) {
			source->frm[ii].selected = TRUE;
 			break;
 		    }
 		    else {
			source->frm[ii].selected = TRUE;
			frm_ctr--;	
			skip_ctr = 0;
		    }
		}
		else {
		    skip_ctr++;
		}
	    }
	}
	else {
	    for (ii = times - 1; ii >= 0 && frm_ctr > 0; ii--) {
		if (skip_ctr == skip) {
		    source->frm[ii].selected = TRUE;
		    frm_ctr--;	
		    skip_ctr = 0;
		}
		else {
		    skip_ctr++;
		}
	    }
	}
    }
}

/*=====================================================================*/

void dataw_updSelTimes ( void )
/************************************************************************
 * dataw_updSelTimes							*
 *									*
 * This function is used to set the selected number of frames		*
 *									*
 * void dataw_updSelTimes ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	08/99	initial coding				*
 * E. Safford/GSC	11/99	add _loopChanged			*
 * E. Safford/GSC	03/00	update number of selected frms in dsrc	*
 * S. Law/GSC		06/00	removed frame parameter			*
 * E. Safford/SAIC	10/01	fix bug in final ftime[0] assignment	*
 ***********************************************************************/
{
    int		sel, times, ii, nsel;
    int		lp;
    dttms_t	tarry[MAX_FRAME], selarry[MAX_FRAME];
    dsrc_t	*dsrc;
/*---------------------------------------------------------------------*/

    lp   = loop_getCurLoop();
    dsrc = _domInfo[lp].src;

    loop_setDataChngd (lp, TRUE);


    tmln_getSelected(&sel, selarry);
    tmln_getTimeInfo(&times, tarry);


    for (ii=0, nsel=0; ii<times; ii++) {
	strcpy(dsrc->frm[ii].ftime, tarry[ii]);

	if (strcmp(tarry[ii], selarry[nsel])==0) {
	    dsrc->frm[ii].selected = TRUE;
	    nsel++;
	}
	else {
	    dsrc->frm[ii].selected = FALSE;
	}
    }

    if (ii < MAX_FRAME) {
        dsrc->frm[ii].ftime[0] = '\0';
    }
    dsrc->num_sel = nsel;

/*
 *  Set the number of frames scale 
 */
    dataw_setFrameScale (nsel, times);

}

/*=====================================================================*/

void dataw_clearLoop ( int lp ) 
/************************************************************************
 * dataw_clearLoop                                                      *
 *                                                                      *
 * This function eliminates any data that has been selected in the      *
 * current loop, and, if data has been loaded, the pixmaps are removed  *
 * and the loop is reset to just a base map.                            *
 *                                                                      *
 * void dataw_clearLoop ( lp )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int		loop number				*
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           07/97                                           *
 * C. Lin/EAI           12/97   add RADAR                               *
 * C. Lin/EAI           06/98   add clearing frame time                 *
 * S. Law/GSC           12/98   cleanup variables                       *
 * S. Law/GSC           12/98   combined with dataw_clearPushCb and     *
 *                              added clear_map parameter               *
 * E. Safford/GSC	09/99	updated for nmap2			*
 * S. Law/GSC		12/99	finished updating for nmap2		*
 * E. Safford/GSC	01/00	add tmln_clearTimeInfo and clean up	*
 * S. Law/GSC		01/00	fixed pgen refresh so it doesn't crash	*
 * E. Safford/GSC	04/00	dsp_loadData -> dsp_loadAllLoops     	*
 * E. Safford/GSC	04/00	rename, add lp param, clear only 1 loop *
 * E. Safford/GSC	04/01	add dataw_updtImgRoam			*
 * M. Li/GSC            03/01   added nmp_simf                          *
 * H. Zeng/EAI          08/01   changed nmp_simf to nmp_sdefmap         *
 * E. Safford/SAIC	10/01	rm nmp_* calls to avoid reset of map    *
 * T. Piper/SAIC	06/03	Added dataw_clearAttr 			*
 * E. Safford/SAIC	12/03	set the domInfo[lp].src = NULL		*
 ***********************************************************************/
{
    int		frmsrc, ii;
/*---------------------------------------------------------------------*/

    loop_setDataChngd (lp, TRUE);

/*
 *  Clear attributes for data source
 *  Remove the data source information.
 */
    for (frmsrc=0; frmsrc<(MAX_FRMSRC); frmsrc++) {
 	dataw_clearAttr (&_dsrc[lp][frmsrc]);
	dataw_clearDsrc (&_dsrc[lp][frmsrc]);
    }
    
    _srcItems[lp]    = 0;
    _domInfo[lp].src = NULL;  

    dataw_updtSources(-1);

/*
 *  Wipe the Copies of the data settings too.  
 */
    for (ii=0; ii<MAX_FRMSRC; ii++) {
	_dsrcCopy[lp][ii] = _dsrc[lp][ii];		
    }

    _srcItemsCopy[lp] = _srcItems[lp];
    _domInfoCopy[lp] = _domInfo[lp];

    dataw_updtImgRoam();

}

/*=====================================================================*/

void dataw_loadData ( void )
/************************************************************************
 * dataw_loadData                                                       *
 *                                                                      *
 * This function loads the selected data.				*
 *                                                                      *
 * void dataw_loadData ()	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	updated for nmap2			*
 * E. Safford/GSC	11/99	add full or partial load options	*
 * E. Safford/GSC	12/99	remove loop_resize call         	*
 * E. Safford/GSC	04/00	remove _initialLoad, use dsp_reloadLoop	*
 * E. Safford/GSC	06/00	add mapw_popdown call                   *
 * S. Law/GSC		06/00	changed to use loop_changeLoop		*
 * E. Safford/GSC	06/00   add mbtnw_setMbtns & xmloop_switchLoop  *
 * E. Safford/GSC	07/00   add loop_changeLoop()                   *
 * H. Zeng/EAI          04/01   reordered loop_setDataChngd() calls     *
 * E. Safford/GSC	05/01   add nmp_rstrproj()                      *
 * E. Safford/GSC	06/01   add mbotw_restoreFade()                 *
 * E. Safford/GSC	06/01   add loopw_resetHide()                   *
 * J. Wu/SAIC		08/01   add call to loop_setRoamVal		*
 * E. Safford/SAIC      08/01   chng ier check from == 0 to >= 0        *
 * E. Safford/SAIC	08/01	add call to dataw_loadRoamVal()		*
 * J. Wu/SAIC		08/01   add call to auto_startAutoApdt		*
 * M. Li/SAIC           01/02   remove _roamVal                         *
 * T. Lee/SAIC		01/04	update base_tm to dominant source       *
 * E. Safford/SAIC	02/04	add params to mbot_restoreFade		*
 * H. Zeng/SAIC		09/04	made call to dataw_makeIconName()	*
 * J. Wu/SAIC		03/05	replace dataw_makeIconName() with	*
 *					dataw_setTitles()		*
 ***********************************************************************/
{
int		lp, ier, cur_lp, roamVal;
dttms_t		lastim;
/*---------------------------------------------------------------------*/

    _loadInProgress = TRUE;

/*
 *  If this load has been ordered by the map window then the data 
 *  window isn't up.  So in order to get the actual roam values now
 *  in use we need to load them (they are otherwise loaded by 
 *  dataw_popup).
 */

    mapw_popdown();
    dataw_popdown(); 

    cur_lp = loop_getCurLoop();

    dsp_setBusy(TRUE);

/*
 *  Load the current loop first
 */
    if (loop_getDataChngd(cur_lp)) {
	loopw_resetHide();
	roamVal = loop_getRoamVal(cur_lp);
        loop_setRoamVal( cur_lp, roamVal );
	dsp_reloadLoop (cur_lp, &ier);
	tmln_getLastTm ( cur_lp, lastim ); 
	strcpy ( _domInfo[cur_lp].base_tm, lastim );
        if( ier >= 0) {

/* 
 *  Reset data changed flag
 */
	    loop_setDataChngd (cur_lp, FALSE);
        }
    }

/*
 *  Load all remaining loops.
 */
    for (lp=0; lp < MAX_LOOP; lp++) {
        if (lp == cur_lp) {
	    continue;
	}
  	else if ( loop_getDataChngd(lp) ) { 
	    nmp_rstrproj  (lp, &ier);
	    roamVal = loop_getRoamVal(lp);
            loop_setRoamVal( lp, roamVal );
	    dsp_reloadLoop(lp, &ier);
	    tmln_getLastTm ( lp, lastim ); 
	    strcpy ( _domInfo[lp].base_tm, lastim );
            if( ier >= 0) {
	        loop_setDataChngd (lp, FALSE);
	    }
        }
    }

    mbotw_restoreFade ( cur_lp, True );

    dsp_setBusy(FALSE);

    loop_changeLoop(cur_lp);

    _loadInProgress = FALSE;

    auto_startAutoUpdt();

/*
 * Determine the icon name and window title.
 */
    dataw_setTitles();
    _restoreUsingSPF = _usingSPF;

}

/*=====================================================================*/

void dataw_setNewRefTm ( dttmi_t *dttm )
/************************************************************************
 * dataw_setNewRefTm							*
 *									*
 * This function sets a new reference time to range/interval GUI.	*
 *									*
 * void dataw_setNewRefTm ( dttm )					*
 *									*
 * Input parameters:                                                    *
 *	*dttm		dttmi_t pointer to date/time struct		*
 * Output parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	03/00	initial coding				*
 * S. Law/GSC		06/00	changed to call dataw_loadDomTimes	*
 * E. Safford/GSC	12/00	add set data changed flag		*
 * T. Lee/SAIC		05/04	re-engineered to range/interval		*
 * T. Lee/SAIC		06/04	removed reference time set		*
 ***********************************************************************/
{
int     cur_lp;
/*---------------------------------------------------------------------*/
    cur_lp = loop_getCurLoop();
    dataw_setRefTm (dttm);

    sprintf  (  _currTimeStr, "%02d%02d%02d/%02d%02d",
		_domInfo[cur_lp].ref_tm.year%100,
		_domInfo[cur_lp].ref_tm.mon+1,
		_domInfo[cur_lp].ref_tm.day,
		_domInfo[cur_lp].ref_tm.hour,
		_domInfo[cur_lp].ref_tm.min );

    XmTextSetString (_refTimeTxt, _currTimeStr);
    dataw_refTimeTxtCb ( NULL, 0, NULL );
    loop_setDataChngd (cur_lp, TRUE);
}

/*=====================================================================*/

Boolean dataw_isUp ( void )
/************************************************************************
 * dataw_isUp	 							*
 *									*
 * This function returns the current state of the data window.      	*
 *									*
 * Boolean dataw_isUp()                   				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * dataw_isUp		Boolean		TRUE if dataw window is up	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged(_dataW) ); 
}

/*=====================================================================*/

dsrc_t *dataw_getDomSrc ( int lp ) 
/************************************************************************
 * dataw_getDomSrc                                                      *
 *                                                                      *
 * This function returns a pointer to a dominant source given the       *
 * dominant source index (which is range 0..MAX_LOOP-1).                *
 *                                                                      *
 * dsrc_t dataw_getDomSrc ( lp )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *   lp		int	loop number                            		*
 *									*
 * Output parameters:                                                   *
 * *dataw_getDomSrc    	dsrc_t	pointer to dominant source or null if 	*
 *						source doesn't exist.   *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/99	initial coding                          *
 * E. Safford/GSC	05/00	fix error in lp value limit		*
 ***********************************************************************/
{
    if (lp > MAX_LOOP-1 || lp < 0) {
	return (NULL);
    }
    else {
        return (_domInfo[lp].src);
    }
}

/*=====================================================================*/

int dataw_getNumLoops ( void )
/************************************************************************
 * dataw_getNumLoops                                                    *
 *                                                                      *
 * Returns the number of loops that are possible.  This number is based *
 * upon the panel configuration.  1x1 has 4 possible loops, 2x1 and 1x2 *
 * have 2 possible loops, and 2x2 has only 1.				*
 *                                                                      *
 * int dataw_getNumLoops ( )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	dataw_getNumLoops	int		1, 2, or 4		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/99	initial coding                          *
 ***********************************************************************/
{
int		loops;
/*---------------------------------------------------------------------*/

    loops = 4;
    return (loops);
}

/*=====================================================================*/

void dataw_getPanelLoc ( int panel, char location[] )
/************************************************************************
 * dataw_getPanelLoc                                                    *
 *                                                                      *
 * Given the panel number, this routine returns the GEMPAK string       *
 * describing the panel's location.  See panel.hl2 for a complete list  *
 * of location values.							*
 *                                                                      *
 * void dataw_getPanelLoc ( panel, location )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   panel	int		panel number 0 to MAX_PANELS		*
 *									*
 * Output parameters:                                                   *
 *   location[] char		string describing the panel's location  *
 *				within the window.			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/99	initial coding                          *
 ***********************************************************************/
{
    strcpy (location, "ALL");
}

/*=====================================================================*/

Boolean	dataw_isImgDom ( int loop )
/************************************************************************
 * dataw_isImgDom                                                       *
 *                                                                      *
 * This function checks whether SAT or RAD data is dominant for the     *
 * specified loop.                    					*
 *                                                                      *
 * Boolean dataw_isImgDom ( loop )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop		int	specified loop to check for Image data  *
 *									*
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 * Return parameters:							*
 * dataw_isImgDom	Boolean	True if sat or rad data is domiant src 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC 	05/00	initial coding                  	*
 ***********************************************************************/
{
Boolean	img_dom;
dsrc_t	*dom;
/*---------------------------------------------------------------------*/

    img_dom = FALSE;
    dom = _domInfo[loop].src;

    if (dom != NULL) {
        if (dom->catg == CAT_IMG) {
	    img_dom = TRUE;
	}
    }
    return ( img_dom );
}

/*=====================================================================*/

Boolean	dataw_isImgInLoop ( int loop )
/************************************************************************
 * dataw_isImgInLoop                                                    *
 *                                                                      *
 * This function checks whether SAT or RAD data is loaded in the  	*
 * specified loop.                    					*
 *                                                                      *
 * Boolean dataw_isImgInLoop ( loop )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop		int	specified loop to check for Image data  *
 * Output parameters:                                                   *
 *	dataw_isImgInLoop	Boolean	True if sat or rad data loaded 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC 	10/99	initial coding                  	*
 * T. Piper/SAIC	11/03	modified for performance enhancement	*
 ***********************************************************************/
{
int     ii;     
Boolean img_in; 
/*---------------------------------------------------------------------*/

    img_in = FALSE;

    for (ii=0; ii<_srcItems[loop]; ii++) { 
	if (_dsrc[loop][ii].catg == CAT_IMG) {
	    if ((strstr (_dsrc[loop][ii].path, "RAD") != NULL ) ||
	        (strstr (_dsrc[loop][ii].path, "SAT") != NULL ) ) {
		img_in = TRUE; 
	    }
	}
    }
    return (img_in);
}

/*=====================================================================*/

Boolean dataw_isImgSrcOn ( int loop )
/************************************************************************
 * dataw_isImgSrcOn                                                     *
 *                                                                      *
 * This function checks whether SAT or RAD data is loaded in the  	*
 * specified loop and is active (turned on).                          	*
 *                                                                      *
 * Boolean dataw_isImgSrcOn ( loop )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      loop            int     specified loop to check for Image data  *
 * Output parameters:                                                   *
 *      dataw_isImgSrcOn       Boolean True if sat or rad data on	*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	11/03	Modified from dataw_isImgInLoop		*
 ***********************************************************************/
{
int     ii;
Boolean src_on;
/*---------------------------------------------------------------------*/

    src_on = FALSE;

    for (ii=0; ii<_srcItems[loop]; ii++) {
        if (_dsrc[loop][ii].src_on) {
            if (_dsrc[loop][ii].catg == CAT_IMG) {
                if ((strstr (_dsrc[loop][ii].path, "RAD") != NULL ) ||
		    (strstr (_dsrc[loop][ii].path, "SAT") != NULL ) ) {
                    src_on = TRUE;
                }
            }
        }
    }
    return (src_on);
}

/*=====================================================================*/

Boolean	dataw_isLoopActv ( int loop )
/************************************************************************
 * dataw_isLoopActv                                                     *
 *                                                                      *
 * Returns True or false depending on whether there is data loaded for  *
 * the specified loop number.                  				*
 *                                                                      *
 * Boolean dataw_isLoopActv ( loop )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	loop		int	specified loop to test (value 0 - 3)	*
 *									*
 * Output parameters:                                                   *
 * dataw_isLoopActv	Boolean	True if loop is available and has data	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/99	initial coding                          *
 * S. Jacobs/NCEP	11/99	Fixed index for _srcItems array		*
 ***********************************************************************/
{
    Boolean	test;
/*---------------------------------------------------------------------*/

    test =  (Boolean)((_srcItems[loop] > 0) ? TRUE : FALSE);
    return (test);
}

/*=====================================================================*/

Boolean	dataw_isRadSelect ( int loop, int *iindex )
/************************************************************************
 * dataw_isRadSelect                                                    *
 *                                                                      *
 * This function checks whether any RAD is selected.                    *
 *                                                                      *
 * Boolean dataw_isRadSelect ( loop, iindex )                           *
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop		int	loop to check for loaded satellite src  *
 * Output parameters:                                                   *
 *	*iindex		int	Data source number that contains image	*
 *	dataw_isRadSelect	Boolean      0 - false, 1 - true        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	05/96                                           *
 * E. Safford/GSC	09/99	mod for nmap2                           *
 * S. Jacobs/NCEP	10/99	Added iindex to calling sequence	*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * T. Piper/SAIC	11/03	changed sat_found to rad_found		*
 ***********************************************************************/
{
int	ii;
Boolean	rad_found;
/*---------------------------------------------------------------------*/

    rad_found = FALSE;
    *iindex   = -1;

    for (ii=0; ii<_srcItems[loop]; ii++) {
        if (_dsrc[loop][ii].catg == CAT_IMG) {
            if (strstr (_dsrc[loop][ii].path, "RAD") != NULL) {
	        rad_found = TRUE;
	        *iindex = _dsrc[loop][ii].attridx;
	    }
        } 
    }
    return (rad_found);
}

/*=====================================================================*/

Boolean	dataw_isSatSelect ( int loop, int *iindex )
/************************************************************************
 * dataw_isSatSelect                                                    *
 *                                                                      *
 * This function checks whether the SAT is selected.                    *
 *                                                                      *
 * Boolean dataw_isSatSelect ( loop, iindex )                           *
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop		int	loop to check for loaded satellite src  *
 * Output parameters:                                                   *
 *	*iindex		int	Data source number that contains image	*
 *	dataw_isSatSelect	Boolean      0 - false, 1 - true        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	05/96                                           *
 * E. Safford/GSC	09/99	mod for nmap2                           *
 * S. Jacobs/NCEP	10/99	Added iindex to calling sequence	*
 * S. Law/GSC		11/99	changed defines to match new table	*
 ***********************************************************************/
{
int	ii;
Boolean	sat_found;
/*---------------------------------------------------------------------*/

    sat_found = FALSE;
    *iindex   = -1;

    for (ii=0; ii<_srcItems[loop]; ii++) {
	if (_dsrc[loop][ii].catg == CAT_IMG) {
	    if (strstr (_dsrc[loop][ii].path, "SAT") != NULL) {
	        sat_found = TRUE;
	        *iindex = _dsrc[loop][ii].attridx;
	    }
        }
    } 
    return (sat_found);
}

/*=====================================================================*/

void dataw_getImgInfo ( int lp, char *fname )
/************************************************************************
 * dataw_getImgInfo                                                     *
 *                                                                      *
 * This function gets the image data infomation.                        *
 *                                                                      *
 * void dataw_getImgInfo(lp, fname)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *									*
 * Output parameters:                                                   *
 *  *fname  	char    a satellite data file name                      *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     	04/96                                           *
 * E. Wehner/EAi  	 1/97   Added parameter to CFL_RDIR call        *
 * C. Lin/EAi     	12/97   rename, add free dnamelist, add typstr  *
 * E. Safford/GSC	10/99	update for use with nmap2		*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * S. Jacobs/NCEP	 5/01	Changed to use nim_gfil to get file name*
 * E. Safford/GSC	06/01	update params for dataw_getTimeStr	*
 * T. Lee/SAIC		 9/03	added time interval to nim_gfil		*
 * T. Lee/SAIC		 7/04	get TIME_MATCH option from pref. table	*
 * m.gamazaychikov/SAIC	04/06	change way time matching scheme is set	*
 ***********************************************************************/
{
int    	ii, ier, match;
dttms_t	time;
dsrc_t	*dsrc, *dom;
/*---------------------------------------------------------------------*/

    fname[0] = '\0';

    for (ii=0; ii < _srcItems[lp]; ii++) {

	dsrc = &(_dsrc[lp][ii]);
        dom  = dataw_getDomSrc(lp);

        if (dom != NULL) {
            match = dom->domtmmtch;
	}
	else {
            match = dsrc->domtmmtch;
	}

	if  ( dsrc->catg == CAT_IMG )  {
	    dataw_getTimeStr ( lp, FALSE, time );
  	    nim_gfil ( dsrc->attridx, time, time, dsrc->range,
	    	       -1, match, 0, fname, &ier );
	}
    }
}

/*=====================================================================*/

void dataw_getIRInfo ( char *dttm, char *path, int *iret )
/************************************************************************
 * dataw_getIRInfo    	                                                *
 *                                                                      *
 * This function returns information on the currently displayed IR      *
 * image.								*
 *                                                                      *
 * void dataw_getIRInfo (dttm, path, iret)	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *		NONE							*
 * 									*
 * Output parameters:                                                   *
 *  *dttm	char	date/time string (DATTM_SZ to avoid problems)	*
 *  *path	char    path and file name of the image file		*
 *  *iret	int	return code 					*
 *				 0 = normal 				*
 *				-1 = no image file			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * S. Jacobs/NCEP	10/99	Changed ss_gtim to css_gtim		*
 * E. Safford/GSC       10/99   removed references to _curLp            *
 * E. Safford/GSC	11/99	dsp_getFrameTime -> loop_getFrameTm	*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * T. Lee/GSC		 2/00	retrieved image type from common block	*
 * S. Law/GSC		06/00	changed to call dataw_getTimeStr	*
 * E. Safford/GSC	06/01	update params for dataw_getTimeStr	*
 * T. Piper/SAIC	07/03	removed unused xqcpxm call		*
 * T. Lee/SAIC		08/03	add time interval to nim_gfil		*
 * T. Lee/SAIC		 7/04	get TIME_MATCH option from pref. table	*
 * M. Li/SAIC		 3/05	Added nmp_rstrproj			*
 * m.gamazaychikov/SAIC	04/06	change way time matching scheme is set	*
 ***********************************************************************/
{
    int		ii, idx, ier, cur_lp, frm;
    char	chntyp[8];
    dttms_t	dattm, time;
    Boolean	ir_found;
    dsrc_t	*dsrc;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

    ir_found = FALSE;
    *iret = -1;

    frm = loop_getCurFrame();
    if (frm < 0) {
	return;
    }

    loop_getFrameTm (cur_lp, frm, dattm);

    for (ii=0; ii<_srcItems[cur_lp]; ii++) {
	if (_dsrc[cur_lp][ii].catg == CAT_IMG) {

            /*
             * get image type information
             */
            nmp_rstrproj (cur_lp, &ier);
	    im_qchn ( chntyp, &ier, sizeof(chntyp) );

	    if (strncmp ( chntyp, "IR", 2 ) == 0) {
		ir_found = TRUE;
		idx = _dsrc[cur_lp][ii].attridx;
		break;
  	    }
	}
    }

    if (ir_found) {
	dataw_getTimeStr ( cur_lp, FALSE, time );
	dsrc = _domInfo[cur_lp].src;

	nim_gfil (idx, dattm, time, dsrc->range, -1,
		  dsrc->domtmmtch, 0, path, &ier);

	if (ier == 0) {
	    *iret = 0;
	}
	strcpy (dttm, dattm);
    }
}

/*=====================================================================*/

void dataw_getStnmName ( char path[], Boolean extra_flag, char *typstr, 
							char *alias )
/************************************************************************
 * dataw_getStnmName							*
 *									*
 * This function gets the station model type and alias name based on	*
 * the input 'path' string.						*
 *									*
 * void dataw_getStnmName (path, extra_flag, typstr, alias)		*
 *									*
 * Input parameters:							*
 *	path[]		char						*
 *	extra_flag	Boolean	flag for extra layer of data spec	*
 *									*
 * Output parameters:							*
 *	*typstr		char	station model type string		*
 *	*alias		char	station model alias string		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		03/98						*
 * S. Jacobs/NCEP	11/98	Added extra_flag for MOS data types	*
 * S. Law/GSC		11/99	updated for nmap2			*
 ***********************************************************************/
{
    char	tmpstr[80], *ptr;
/*---------------------------------------------------------------------*/

    strcpy (tmpstr, path);

    ptr = strrchr (tmpstr, '/');
    strcpy (alias, ptr+1);

    if (extra_flag) {
	*ptr = '\0';
	ptr = strrchr (tmpstr, '/');
    }

    *ptr = '\0';
    ptr = strrchr (tmpstr, '/');
    strcpy (typstr, ptr+1);

    if (strcmp(alias, "standard") == 0) {
	strcpy (alias, typstr);
    }
}

/*=====================================================================*/

int dataw_getSkip ( int lp )
/************************************************************************
 * dataw_getSkip                                                        *
 *                                                                      *
 * This function returns the skip factor for a specified loop.          *
 *                                                                      *
 * int dataw_getSkip ( lp )                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *   lp		int		loop number                   		*
 *									*
 * Output parameters:                                                   *
 * dataw_getSkip	int	skip factor				*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 * E. Safford/GSC	05/00	lp == MAX_LOOP condition		*
 ***********************************************************************/
{
    if ( 0 > lp || lp > MAX_LOOP - 1 ) {
	return ((int) NULL);
    }
    else {
        return (_domInfo[lp].src->skip);
    }
}

/*=====================================================================*/

void dataw_getDataW ( Widget *wid )    
/************************************************************************
 * dataw_getDataW                                                       *
 *                                                                      *
 * This function returns the data selection widget ID.			*
 *                                                                      *
 * void dataw_getDataW ( wid )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *		NONE							*
 * Output parameters:                                                   *
 *  *wid	Widget	current value of _dataW 			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/99	initial coding                          *
 ***********************************************************************/
{
    *wid = _dataW;
}

/*=====================================================================*/

Boolean	dataw_useRefTm ( int lp )
/************************************************************************
 * dataw_useRefTm                                                       *
 *                                                                      *
 * This function returns the use_refTm flag for the given loop.  TRUE   *
 * indicates a set time is to be used as the reference time in loading	*
 * data.								*
 *                                                                      *
 * Boolean dataw_useRefTm ( lp )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop number				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 * Return value:							*
 *  dataw_useRefTm	Boolean	True if set time is to be used		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	12/00	initial coding                		*
 ***********************************************************************/
{
    return (_domInfo[lp].use_refTm);
}

/*=====================================================================*/

dttmi_t *dataw_getRefTm ( int lp )    
/************************************************************************
 * dataw_getRefTm                                                       *
 *                                                                      *
 * This function returns a pointer to the reference time for the        *
 * current loop.							*
 *                                                                      *
 * dttmi_t *dataw_getRefTm ( lp )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop number				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 * Return value:							*
 *  *dataw_getRefTm	dttmi_t	pointer to reference time for cur loop  *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/00	initial coding                          *
 * E. Safford/GSC	12/00	add lp param, replacing cur_lp		*
 ***********************************************************************/
{
    return (&_domInfo[lp].ref_tm);
}

/*=====================================================================*/

void dataw_getFrameTime ( int iframe, char *dttm ) 
/************************************************************************
 * dataw_getFrameTime							*
 *									*
 * This function returns GEMPAK date/time string of the specified	*
 * frame.								*
 *									*
 * void *dataw_getFrameTime (iframe, dttm)				*
 *									*
 * Input parameters:							*
 *	iframe		int	the frame index				*
 *									*
 * Output parameters:							*
 *    *dttm	char	GEMPAK date/time string				*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/00	initial coding				*
 ***********************************************************************/
{
    loop_getFrameTm (loop_getCurLoop (), iframe, dttm);
}

/*=====================================================================*/

int dataw_getNumSrcs ( int loop )
/************************************************************************
 * dataw_getNumSrcs                                                     *
 *                                                                      *
 * This function returns the number of selected sources for the         *
 * indicated loop.            						*
 *                                                                      *
 * int dataw_getNumSrcs(loop)	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *	loop		int	loop number				*
 * Output parameters:                                                   *
 * dataw_getNumSrcs	int	number of sources in the panel		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
************************************************************************/
{
    if (loop < 0 || loop > MAX_LOOP -1)  {
        return (-1);
    }
    else {
        return (_srcItems[loop]);
    }
}

/*=====================================================================*/

dsrc_t *dataw_getDataSrc ( int loop, int frmsrc )
/************************************************************************
 * dataw_getDataSrc                                                     *
 *                                                                      *
 * This function returns a pointer to the data source, given the loop 	*
 * frame source, and frame number.					*
 *                                                                      *
 * dsrc_t dataw_getDataSrc ( loop, frmsrc )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop	int		loop number				*
 *	frmsrc	int		frame source or data source index	*
 *									*
 * Output parameters:                                                   *
 * *dataw_getDataSrc	dsrc_t	data source structure or NULL if one or *
 *				 more of the input params is too large  *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/99	initial coding                          *
************************************************************************/
{
    dsrc_t	*dsrc;
/*---------------------------------------------------------------------*/

    if (loop >= MAX_LOOP || frmsrc >= MAX_FRMSRC) {
        dsrc = NULL;
    }
    else {
        dsrc = &(_dsrc[loop][frmsrc]);
    }

    return (dsrc);
}

/*=====================================================================*/

void dataw_enableLoop ( void )
/************************************************************************
 * dataw_enableLoop                                                  	*
 *                                                                      *
 * The routine makes the loop pulldown sensitive.  This is designed   	*
 * to be used when the mapw window is poped down, and the loop menu     *
 * selection is allowable again.                          		*
 *                                                                      *
 * void dataw_enableLoop ( void )  	                            	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/01   Initial coding                          *
 ***********************************************************************/
{
    XtSetSensitive(_loopOptn, TRUE);
}

/*=====================================================================*/

void dataw_disableLoop ( void )
/************************************************************************
 * dataw_disableLoop                                                  	*
 *                                                                      *
 * The routine make the loop pulldown insensitive.  This is designed   	*
 * to be used when the mapw window is up to prevent the user from       *
 * changing loops and causing problems to the mapw window.		*
 *                                                                      *
 * void dataw_disableLoop ( void )                              	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/01   Initial coding                          *
 ***********************************************************************/
{
    XtSetSensitive(_loopOptn, FALSE);
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_loopCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_loopCb	 							*
 *									*
 * This function is the callback for the loop selection buttons.	*
 *									*
 * void dataw_loopCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * E. Safford/GSC	10/99	set autoupdate value			*
 * H. Zeng/EAI          11/99   set roam value                          *
 * E. Safford/GSC	04/00	synch with loop in view 		*
 * S. Law/GSC		06/00	changed to use loop_changeLoop		*
 * T. Lee/GSC		02/01	set single time loop			*
 * E. Safford/GSC	04/01	add dataw_updtImgRoam			*
 * M. Li/GSC		06/01	add dataw_resetBtn 			*
 * J. Wu/GSC		07/01	call dataw_setLoop() 			*
 ***********************************************************************/
{
/*
 *  Switch to the given loop.
 */
    dataw_setLoop ( (int)which );
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_mapCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * dataw_mapCb	 							*
 *									*
 * This function is the callback for the map button.                   	*
 *									*
 * void dataw_mapCb ( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		XtPointer	which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	05/00	initial coding				*
 ***********************************************************************/
{
    mapw_popup();
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_sourceCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_sourceCb							*
 *									*
 * This function is the callback for the source selection and dominant	*
 * source popup menus.							*
 *									*
 * void dataw_sourceCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which menu			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * S. Law/GSC		11/99	add call to dataw_setEdit		*
 * E. Safford/GSC	01/00	clean up       				*
 * E. Safford/GSC	05/00	fix load bug w/ dom source selection	*
 * M. Li/GSC		06/01	add dataw_resetBtn			*
 * J. Wu/SAIC		12/04	add dataw_setBinSrc			*
 ***********************************************************************/
{
    if (which == DOM_BTN) {

	if (wid == _domBtn[0]) {
	    _noDom = TRUE;
	}
	else {
	    _noDom = FALSE;
	}

	dataw_resetDom();
        loop_setDataChngd (loop_getCurLoop(), TRUE);
	dataw_resetAutoUpdate(loop_getCurLoop());
    }
    else {
	dataw_setEdit ( );
	dataw_setBinSrc ( );
	dataw_resetBtn ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_sourceCtlCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_sourceCtlCb							*
 *									*
 * This function is the callback for the Add New, Modify Source btns 	*
 * and source status button.						*
 *									*
 * void dataw_sourceCtlCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * H. Zeng/EAI          06/00   changed dslw_popup() parameters         *
 * M. Li/GSC		06/01 	added _srcState setting			*
 * S. Jacobs/NCEP	 6/03	Changed dslw_popup y-location to 0	*
 * H. Zeng/SAIC		07/07	changed dslw_popup() parameters		*
 ***********************************************************************/
{
int		src, lp, cur_lp, ier;
char		src_label[256];
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

    if (which == NEW_SOURCE) {
        src = _srcItems[cur_lp];

        dataw_clearDsrc ( &(_dsrc[cur_lp][src]) );
        dslw_popup (_dsrc[cur_lp][src].catg, _dsrc[cur_lp][src].path );  
    }
    else if (which == SRC_STATE) {
	dataw_getSelIdx(SRC_BTN, &lp, &src);

        if (_dsrc[lp][src].src_on) {
           _dsrc[lp][src].src_on = FALSE;

	   strcpy(src_label, "(Off)   ");
	   strcat(src_label, _dsrc[lp][src].path);
        }
        else {
           _dsrc[lp][src].src_on = TRUE;
	   strcpy(src_label, _dsrc[lp][src].path);
        }

	dataw_modSource(SRC_BTN, src, src_label, &ier);
   	dataw_loadDomMenu();
	loop_setDataChngd(lp, TRUE);
	dataw_resetBtn();

    }
    else {
	dataw_getSelIdx( SRC_BTN, &lp, &src );

	if (src < 0) {
	    src = 0;	
	}

	_currCatg = _dsrc[lp][src].catg;

        dslw_popup (_dsrc[lp][src].catg, _dsrc[lp][src].path ); 
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_editCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_editCb								*
 *									*
 * This function is the callback for the edit button in the data list.	*
 *									*
 * void dataw_editCb (wid, which, call)					*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		not used			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 4/96						*
 * C. Lin/EAI		 5/97	add case SHIP				*
 * C. Lin/EAI		 7/97	add VGF edit popup			*
 * C. Lin/EAI		 7/97	add VGF color into calling of vgfw_popup*
 * C. Lin/EAI		10/97	add VGF fill into calling of vgfw_popup	*
 * C. Lin/EAI		03/98	add station model initialization	*
 * C. Lin/EAI		04/98	use display mode			*
 * S. Jacobs/NCEP	11/98	Added MOS display type			*
 * S. Jacobs/NCEP	11/98	Changed call to dataw_getStnmName	*
 * S. Jacobs/NCEP	 5/99	Added WTCH_WARN data type		*
 * S. Law/GSC		11/99	adapted for nmap2			*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * S. Jacobs/NCEP	 3/00	Changed CAT_VGF/CAT_MSC to use msc_popup*
 * S. Jacobs/NCEP	12/02	Added check for CAT_SNF			*
 * A. Hardy/NCEP	 9/03	Changed 'wid' -> '_dataW' in msc_popup	*
 * S. Chiswell/Unidata   3/05   Added CAT_IMG for image parameter edit  *
 ************************************************************************/
{
    char	typestr[20], alias[20];
    int		loop, src;
    dsrc_t	*active_src;
    Boolean	fcst_flag;
/*---------------------------------------------------------------------*/

    dataw_getSelIdx (SRC_BTN, &loop, &src);
    active_src = &_dsrc[loop][src];

    switch (active_src->catg) {

      case CAT_SFC:
      case CAT_SFF:
      case CAT_SND:
      case CAT_SNF:
	fcst_flag = (Boolean)( (active_src->catg == CAT_SFF) ||
		      (active_src->catg == CAT_SNF) );
	dataw_getStnmName (active_src->path, fcst_flag, typestr, alias);
	stnmw_popup (typestr, alias, active_src->attridx);
	break;

      case CAT_VGF:
      case CAT_MSC:
	msc_popup ( _dataW, active_src->attridx );
	break;

     case CAT_IMG:
        image_props_popup ( _dataW, active_src->attridx );
        break;

    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_topCtlCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_topCtlCb							*
 *									*
 * This function is the callback for the upper control buttons of the 	*
 * Data Selction Window:  Restore Data Settings and Save Data Settings.	*
 *									*
 * void dataw_topCtlCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * J. Wu/GSC		06/01	add actual functionality		*
 * J. Wu/GSC		07/01	check if any active sources exist	*
 ***********************************************************************/
{

    switch (which) {
	case 0:         /* Restore Data Settings */
            spfw_popup( RESTORE_SPF );
	    break;
	
	case 1:         /* Save Data Settings */
            if  ( dataw_noActSrc() ) {
	        NxmWarn_show ( _dataW, "No active sources, nothing to save!" );
            }
	    else {
	        spfw_popup( SAVE_SPF );
	    }

	    break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_bottomCtlCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_bottomCtlCb							*
 *									*
 * This function is the callback for the lower control buttons of the	*
 * Data Selection Window:  LOAD, HELP, and CANCEL.       		*
 *									*
 * void dataw_bottomCtlCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * E. Safford/GSC	10/99	remove reference to _curLp		*
 * E. Safford/GSC	11/99	remove reset on roam values in load	*
 * H. Zeng/EAI          11/99   use loop_setRoamVal()                   *
 * E. Safford/GSC	05/00	restore GAREA/PROJ on cancel		*
 * E. Safford/GSC	05/00	treat a load w/ no data as a cancel	*
 * E. Safford/GSC	05/00	param change to dsp_setProj		*
 * M. Li/GSC		07/00	added restore functions			*
 * E. Safford/GSC	08/00	move CANCEL operations to dataw_cancel()*
 * E. Safford/GSC	04/01	use loop_getDataChngd !dataw_getNumSrcs *
 * E. Safford/SAIC	10/01	treat WM Close as a cancel 		*
 * E. Safford/SAIC	11/01	ignore callbacks if spfw is still up	*
 ***********************************************************************/
{
int	ii;
Boolean	load_needed;
/*---------------------------------------------------------------------*/
/*
 *  A close from the window manager produces 2 calls to this routine.
 *  Toss out any beyond the first instance.
 */
    if (!dataw_isUp()) {
        return;
    }

/*
 *  If the user has reached under the spf load window to hit one
 *  of dataw's control buttons, then toss out the callback.
 */
    if ( spfw_isUp() ) {
	return;
    }

    switch (which) {

	case LOAD:
	    load_needed = FALSE;

	    for (ii=0; ii < MAX_LOOP; ii++) {

		if ( loop_getDataChngd(ii) > 0 ) {
		    load_needed = TRUE;
		    break;
		}
	    }

	    if (load_needed) {
		dataw_loadData();
	    }
	    else {
		dataw_cancel();
 	    }
	    break;

	case HELP:
	    break;

	case CANCEL:
	    dataw_cancel();
	    break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_skipArrowCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_skipArrowCb							*
 *									*
 * This function is the callback for the skip factor arrow buttons.   	*
 *									*
 * void dataw_skipArrowCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * J. Wu/SAIC		09/01	check if there are selected times	*
 ***********************************************************************/
{
    int		skip, view, selected;
    dsrc_t	*dsrc;
    dttms_t	sel_arry[MAX_PIX];
/*---------------------------------------------------------------------*/
/*  
 *  Exit if no time is selected.
 */
    tmln_getSelected ( &selected, sel_arry );
    if ( selected <= 0 )  return;   

    tmln_getSkipFactor(&skip); 
    
    switch( which ) {
        case 0:                         /* arrow up */
	    skip++;
	    break;

	case 1:                         /* arrow down */
	    if ( skip > 0 )
	    skip--;
	    break;
    }

    view = loop_getCurLoop();
    dsrc = _domInfo[view].src;

    if (dsrc != NULL) {
        dataw_setSkipFactor (skip, True); 
        dsrc->skip = skip;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_clearCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_clearCb							*
 *									*
 * This function is the callback for the clear button.                	*
 *									*
 * void dataw_clearCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/99	initial coding				*
 * E. Safford/GSC	10/99	add call to resetAutoUpdate		*
 * H. Zeng/EAI          11/99   add call to resetRoam()                 *
 * E. Safford/GSC	01/00	add call to loop_setDataChngd		*
 * T. Lee/GSC		03/01	changed call seq. of loop_setRoamVal	*
 * E. Safford/GSC	04/01	added dataw_updtImgRoam			*
 * E. Safford/GSC	04/01	make roam reset only happen when set to *
 *				  size of image and clearing the image	*
 * S. Jacobs/NCEP	 5/01	Fixed check for removal of image data	*
 * M. Li/GSC		06/01	add dataw_resetBtn			*
 ***********************************************************************/
{
int		lp, idx, cur_roam, ier;
nmpstr_t 	mapnms[MAX_MAP], map, proj, garea[2];
/*---------------------------------------------------------------------*/

    dataw_getSelIdx (SRC_BTN, &lp, &idx);

    nmp_gmapattr(lp, map, proj, garea, &ier);

    if  ( _dsrc[lp][idx].catg == CAT_IMG )  {
        nmp_simf(lp, " ", NO_IMG, &ier);
        nmp_gmapnms(mapnms, &ier);
        nmp_setmap(mapnms[0], lp, False, &ier);
    }

    if (idx >= 0) {
	dataw_removeSrc(lp, idx);
    }

    loop_setDataChngd (lp, TRUE);
    dataw_updtSources(-1);
    dataw_resetAutoUpdate(lp);

/* 
 *  Keep the current roam setting unless roam was set to size of
 *  image and we just cleared the image.  In this case, reset roam.
 */
    cur_roam = loop_getRoamVal(lp);
  
    if ( !dataw_isImgInLoop(lp) && cur_roam == SIZE_OF_IMAGE ) {
        loop_setRoamVal(lp, DEFAULT_LEVEL);
        dataw_resetRoam(lp);
    }

    dataw_updtImgRoam();

    dataw_getSelIdx (SRC_BTN, &lp, &idx);
    if (idx >= 0) {
	dataw_resetBtn();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_selTimesCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_selTimesCb							*
 *									*
 * This function is the callback for the selected times slider bar.	*
 *									*
 * void dataw_selTimesCb (wid, which, call) 				*
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
 * C. Lin/EAI		04/96						*
 * C. Lin/EAI		04/97	take out _frameInfo.nframe assignment	*
 * C. Lin/EAI		08/98	modify for new time line scheme		*
 * S. Law/GSC		12/98	moved from _totalScaleCb to _totalTimeCb*
 * E. Safford/GSC	08/99	renamed from dataw_totalTimeCb		*
 * S. Law/GSC		06/00	removed parameter from dataw_updSelTimes*
 ***********************************************************************/
{
    XmScaleCallbackStruct *cbp;
/*---------------------------------------------------------------------*/

    cbp = (XmScaleCallbackStruct *) call;

    if (cbp->value > 0) {
        tmln_setTotalFrame (cbp->value);
        dataw_updSelTimes ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_autoUpdtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_autoUpdtCb							*
 *									*
 * This function is the callback for the auto update menu.            	*
 *									*
 * void dataw_autoUpdtCb( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	10/99	initial coding                		*
 * J. Wu/SAIC		08/01	call dataw_setAutoUpdt()  		*
 ***********************************************************************/
{
    dataw_setAutoUpdt ( (int)which );    
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_roamCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_roamCb								*
 *									*
 * Callback function for roam value option menu	                        *
 *									*
 * void dataw_roamCb (wid, which, call)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID			        *
 *	which	long		button number				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI      	11/99  	initial coding				*
 * E. Safford/GSC	11/99	set _loopChanged      			*
 * T. Lee/GSC		03/01	changed call seq. of loop_setRoamVal	*
 * J. Wu/SAIC		08/01	reomove call to loop_setRoamVal 	*
 * M. Li/SAIC		01/02	remove _roamVal				*
 ***********************************************************************/
{
int	loop;
/*---------------------------------------------------------------------*/

    loop = loop_getCurLoop();

    loop_setRoamVal(loop, (int)which);
    loop_setDataChngd (loop, TRUE); 
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_currTimeCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * dataw_currTimeCb							*
 *									*
 * Callback function for the current time buttons.			*
 *									*
 * void dataw_currTimeCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID			        *
 *	which	long		button number				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI           04/96                                           *
 * C. Lin/EAI           04/97   set nframe info for domt type (accept)  *
 *                              set _databarSelect back when cancel     *
 * S. Law/GSC           12/98   moved from _rtimeRadioCb to _refTimeCb  *
 * E. Safford/GSC       03/99   rebuild timeline as needed              *
 * S. Law/GSC		06/00	changed to call dataw_loadDomTimes	*
 * E. Safford/GSC	05/01	add setting _autoOptn in/sensitive	*
 * S. Jacobs/NCEP	10/01	Added call to dataw_setImageNav		*
 * T. Lee/SAIC		04/04	renamed from dataw_refTimeCb		*
 * T. Lee/SAIC		05/04	set delta reference time		*
 ***********************************************************************/
{
    int		skip, lp;
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();
    loop_setDataChngd (lp, TRUE);

    _domInfo[lp].use_refTm = FALSE;
    _domInfo[lp].src->delta_rt= -1;

/*
 *  Get the current skip factor
 */
    XtSetSensitive (_autoOptn, (int)TRUE );
    dataw_loadDomTimes ();

    tmln_getSkipFactor(&skip);
    if (skip) {
        dataw_setSkipFactor (skip, TRUE);
    }

/*
 *  If there is an image present, set the navigation.
 */
    dataw_setImageNav ( );

}

/*=====================================================================*/

void dataw_createPanel ( Widget parent )
/************************************************************************
 * dataw_createPanel							*
 *									*
 * This function creates the panel selection GUI.          		*
 *									*
 * void dataw_createPanel(parent)             				*
 *									*
 * Input parameters:							*
 *  parent	Widget		parent widget for panel GUI		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * H. Zeng/EAI          11/99   added roam value option menu            *
 * E. Safford/GSC	05/00	add map button				*
 * H. Zeng/EAI          07/00   added static string icon_tip            *
 * E. Safford/GSC	10/00	fix bug in callback value on roam menu	*
 * J. Wu/SAIC		08/01	save off the largest roam value		*
 * J. Wu/SAIC		04/02	save off valid roam factors in table	*
 * R. Tian/SAIC  	01/03   add True flag to NxmBxmBtn_create(Multi)*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
Widget		panel_form, loop_pulldn, auto_pulldn, roam_pulldn, map_btn;
char		label[25], record[256], grp[10];
static char     icon_tip[15];
char		*on_off[] = {"Off", "On"};
char		iconfile[256];
int		ncount, loglev, state, iret; 
int             curr_val, ier;
long		ii, ignore;
XmString	xmstr;
Boolean         not_done;

FILE*           fp ; 
/*---------------------------------------------------------------------*/

    panel_form = XtVaCreateWidget("panel form", 
		xmFormWidgetClass,		parent,
		XmNfractionBase,		20,
		NULL);    	
   			
    _pnlDsply = ONE_X_ONE;

/*
 *  Loop selection items 
 */
    loop_pulldn   = XmCreatePulldownMenu (panel_form, "loop_pulldn", NULL, 0);
    _loopOptn     = XmCreateOptionMenu (panel_form, "_loopOptn", NULL, 0);
    _loopBtn      = (WidgetList)XtMalloc(MAX_LOOP*sizeof(Widget));

    for (ii=0; ii < MAX_LOOP; ii++) {
	sprintf (label, "%ld", ii+1);
	_loopBtn[ii] = XtVaCreateManagedWidget(label,
		xmPushButtonWidgetClass,	loop_pulldn,
		NULL);
  	XtAddCallback(_loopBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_loopCb, (XtPointer)ii);
    }

    XtVaSetValues (_loopOptn,
    		XmNsubMenuId,			loop_pulldn,
    		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		2,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		NULL);
    NxmLabel_setStr(_loopOptn, "Loop:  ");
    XtManageChild(_loopOptn);

    loop_setCurLoop (0);

/*
 *  Auto update selection items 
 */
    auto_pulldn   = XmCreatePulldownMenu (panel_form, "auto_pulldn", NULL, 0);
    _autoOptn     = XmCreateOptionMenu (panel_form, "_autoOptn", NULL, 0);
    _autoBtn      = (WidgetList)XtMalloc(XtNumber(on_off)*sizeof(Widget));

    for (ii=0; ii < (int)XtNumber(on_off); ii++) {
	xmstr = XmStringCreateLocalized (on_off[ii]);
	_autoBtn[ii] = XtVaCreateManagedWidget(on_off[ii],
		xmPushButtonWidgetClass,	auto_pulldn,
		XmNlabelString,			xmstr,
		NULL);
	XmStringFree(xmstr);

  	XtAddCallback(_autoBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_autoUpdtCb, (XtPointer)ii);
    }

    XtVaSetValues (_autoOptn,
    		XmNsubMenuId,			auto_pulldn,
    		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		12,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		XmNmenuHistory,			_autoBtn[1],
		NULL);
    NxmLabel_setStr(_autoOptn, "Auto Update:  ");

/*
 *  Roaming selection items
 */
    not_done = TRUE;

/* 
 *  Skip over "Fit to Window" and "Size of Image" 
 */
    ncount = 2;		
    
/*
 *  Attempt to open the file
 */
    if ((fp = cfl_tbop (ROAM_TBL, "nmap", &ier)) == NULL) {
	loglev = 2;
        iret = -1;
	strcpy(grp, "NMAP_ROAM");
	er_lmsg (&loglev, grp, &iret, ROAM_TBL, &ier, strlen (grp),
		 strlen (ROAM_TBL));

	not_done = FALSE;
    }

/* 
 *  Intialize the roam factors.
 */
    for ( ii = 0; ii < MAX_ROAMVAL; ii++ ) {
         _roamValues[ii] = 0; 
    }

/* 
 *  For every line in the table, read in the record,
 *  parse out the fields, and compare to input type.
 */
    while (not_done) {
	cfl_trln (fp, sizeof(record), record, &state );

	switch (state) {
	  case 4:	/* EOF */
	    not_done = FALSE;
	    break;

	  case -3:	/* read failure */
	  case -6:	/* no file has been opened */
	  default:
	    not_done = FALSE;
	    break;

	  case 1:	/* line is too long */
	  case -24:	/* line is really too long */
/* do nothing */
	    break;

	  case 0:	/* good record */
	    sscanf (record, "%d", &curr_val);

	    if (0 < curr_val && curr_val < 21) {
		_roamValues[ncount++] = curr_val;

		if (ncount == (MAX_ROAMVAL - 1)) not_done = FALSE;
	    
	    }
	    break;
	}
    }

    if (state != -6) {	/* no file has been opened */
	cfl_clos(fp, &ier);
    }

    roam_pulldn   = XmCreatePulldownMenu (panel_form, "roam_pulldn", NULL, 0);
    _roamOptn     = XmCreateOptionMenu (panel_form, "_roamOptn", NULL, 0);
    _roamBtn      = (WidgetList)XtMalloc((size_t)ncount*sizeof(Widget));

    _numRoam	  = ncount;

    ii = 0;
    strcpy (label, "Fit to Screen");
    _roamValues[ii] = 0;
    _roamBtn[ii] = XtVaCreateManagedWidget(label,
		   xmPushButtonWidgetClass,	roam_pulldn,
  		   XmNuserData,			ii, 
		   NULL);

    XtAddCallback(_roamBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_roamCb, (XtPointer)ii);

    ii = 1;
    _imgSzBtn = ii;
    strcpy (label, "Size of Image");
    _roamValues[ii] = 1;
    _roamBtn[ii] = XtVaCreateManagedWidget(label,
		   xmPushButtonWidgetClass,	roam_pulldn,
  		   XmNuserData,			ii, 
		   NULL);

    XtAddCallback(_roamBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_roamCb, (XtPointer)ii);

    for (ii=2; ii < ncount; ii++) {
	
	sprintf (label, "%ld x Screen Size", _roamValues[ii]);
	_roamBtn[ii] = XtVaCreateManagedWidget(label,
		xmPushButtonWidgetClass,	roam_pulldn,
  		XmNuserData,			_roamValues[ii], 
		NULL);

  	XtAddCallback(_roamBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_roamCb, 	(XtPointer)_roamValues[ii]);
    }

    XtVaSetValues (_roamOptn,
    		XmNsubMenuId,			roam_pulldn,
    		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		6,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		NULL);
    NxmLabel_setStr(_roamOptn, "Roam:  ");

    XtManageChild(_roamOptn);

/*
 *  Map push button
 */
    cfl_inqr("map.xbm", ICON_DIR, &ignore, iconfile, &ier);
    strcpy(icon_tip, "map window");
    map_btn = (Widget) NxmBxmBtn_create( panel_form, "map", NULL, 
    		ICON_WIDTH, ICON_HEIGHT, ICON_FGNAME , ICON_BGNAME, 
		NULL, iconfile, icon_tip, True, dataw_mapCb, (XtPointer)NULL );

    XtVaSetValues(map_btn,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      19,
                  XmNtopAttachment,      XmATTACH_FORM,
                  NULL);

/*
 *  Set the "Size of Image" button insensitive.
 */   
    XtSetSensitive(_roamBtn[1], False);

    XtManageChild(panel_form);

}

/*=====================================================================*/

void dataw_createSource ( Widget parent )
/************************************************************************
 * dataw_createSource							*
 *									*
 * This function creates the panel source selection GUI.       		*
 *									*
 * void dataw_createSource(parent)         				*
 *									*
 * Input parameters:							*
 *  parent	Widget		parent widget for panel GUI		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * S. Law/GSC		11/99	added edit button callback		*
 * E. Safford/GSC	05/01	make the src menu of fixed size		*
 * M. Li/GSC		06/01	add _srcState button & clean up		*
 * J. Wu/SAIC		09/04	add "Bin Source" button			*
 * A. Hardy/NCEP	11/04	comment out "Bin Source" button		*
 * J. Wu/SAIC		12/04	activate "Bin Source" check box/button	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	src_form, dom_form, clear_btn;
    Widget	new_srcbtn, src_pulldn;
    long		ii;
/*---------------------------------------------------------------------*/

    src_form = XtVaCreateWidget("source form", 
		xmFormWidgetClass,		parent,
		XmNfractionBase,		20,
		NULL);    	

/*
 *  Source Pull down
 */
    src_pulldn   = XmCreatePulldownMenu (src_form, "src_pulldn", NULL, 0);
    _srcOptn     = XmCreateOptionMenu (src_form, "sel_optn_menu", NULL, 0);
    _srcBtn      = (WidgetList)XtMalloc(MAX_FRMSRC*sizeof(Widget)); 

    for (ii=0; ii<MAX_LOOP; ii++) {
	_srcItems[ii] = 0;
    }

    for (ii=0; ii < MAX_FRMSRC; ii++) {
    	_srcBtn[ii] = XtVaCreateWidget(NONE_SELECTED,
		xmPushButtonWidgetClass,	src_pulldn,
		XmNalignment,			XmALIGNMENT_BEGINNING,
		XmNrecomputeSize,		FALSE,
		XmNwidth,			533,
		NULL);

        XtAddCallback(_srcBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_sourceCb,	(XtPointer)SRC_BTN);

        if (ii==0) {
	    XtManageChild(_srcBtn[ii]);
	}
    }
  
    XtVaSetValues (_srcOptn,
    		XmNsubMenuId,			src_pulldn,
    		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		1,
  		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5, 
		XmNmarginHeight,		5,
		XmNmarginWidth,			5,
		NULL);
    NxmLabel_setStr(_srcOptn, "Data Source:    ");

/*
 *  Add New Source and Modify Source buttons
 */
    new_srcbtn = XtVaCreateManagedWidget("dataw_new_srcbtn",
    	 	xmPushButtonWidgetClass,	src_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		1,
  		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		5, 
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_srcOptn,
		XmNtopOffset,			10, 
		XmNheight,			30,
		NULL);	 
    NxmLabel_setStr(new_srcbtn, "New Source");
    XtAddCallback(new_srcbtn, 			XmNactivateCallback,
    		(XtCallbackProc)dataw_sourceCtlCb, (XtPointer)NEW_SOURCE);


    _srcState = XtVaCreateManagedWidget("turn_source_off",
                xmPushButtonWidgetClass,        src_form,
                XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                8,
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,               12,
                XmNtopAttachment,               XmATTACH_WIDGET,
                XmNtopWidget,                   _srcOptn,
                XmNtopOffset,                   10,
                XmNheight,                      30,
                NULL);
    NxmLabel_setStr(_srcState, SRC_OFF);
    XtAddCallback(_srcState,                   XmNactivateCallback,
                (XtCallbackProc)dataw_sourceCtlCb, (XtPointer)SRC_STATE);


    _modSrcBtn = XtVaCreateManagedWidget("dataw_modSrcBtn",
    	 	xmPushButtonWidgetClass,	src_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		15,
  		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		19, 
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_srcOptn,
		XmNtopOffset,			10, 
		XmNheight,			30,
		NULL);	 
    NxmLabel_setStr(_modSrcBtn, "Modify Source");
    XtAddCallback(_modSrcBtn, 			XmNactivateCallback,
    		(XtCallbackProc)dataw_sourceCtlCb, (XtPointer)MOD_SOURCE);
	
/*
 *  Clear Button
 */
    clear_btn = XtVaCreateManagedWidget("Clear",
    	 	xmPushButtonWidgetClass,	src_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		1,
		XmNrightAttachment,             XmATTACH_POSITION,
		XmNrightPosition,               5,
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			new_srcbtn,
		XmNtopOffset,			20, 
		XmNheight,			22,
		NULL);	 
    NxmLabel_setStr(clear_btn, "Clear Source");
    XtAddCallback(clear_btn,			XmNactivateCallback,
		(XtCallbackProc)dataw_clearCb,	(XtPointer)0);

/*
 *  Bin Button and check box
 */
    _binSrcToggBtn = XtVaCreateManagedWidget (" ",
                xmToggleButtonWidgetClass, 	src_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		8,
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_srcState,
		XmNtopOffset,			22, 
                XmNtraversalOn,            	False,
		XmNset,                    	False,
                NULL );
		
    XtAddCallback ( _binSrcToggBtn, XmNvalueChangedCallback,
		    (XtCallbackProc)dataw_binbxToggCb, (XtPointer)ii);


    _binSrcBtn = XtVaCreateManagedWidget("Bin",
    	 	xmPushButtonWidgetClass,	src_form,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_binSrcToggBtn,
		XmNleftOffset,			0,
		XmNrightAttachment,             XmATTACH_POSITION,
		XmNrightPosition,               12,
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_srcState,
		XmNtopOffset,			20, 
		XmNheight,			22,
		NULL);	 
    NxmLabel_setStr(_binSrcBtn, "Bin Source");
    XtAddCallback(_binSrcBtn,			XmNactivateCallback,
		(XtCallbackProc)dataw_binSrcCb,	(XtPointer)NULL);

/*
 *  Edit Button
 */
    _editBtn = XtVaCreateManagedWidget("Edit",
	        xmPushButtonWidgetClass,        src_form,
		XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                15,
		XmNrightAttachment,             XmATTACH_POSITION,
		XmNrightPosition,               19,
  		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			new_srcbtn,
                XmNtopOffset,                   20,
                XmNheight,                      22,
                NULL);
    NxmLabel_setStr(_editBtn, "Edit Source");
    XtAddCallback(_editBtn, XmNactivateCallback,
		  (XtCallbackProc) dataw_editCb, (XtPointer) NULL);
    XtSetSensitive (_editBtn, FALSE);


    dom_form = XtVaCreateWidget("dom form", 
		xmFormWidgetClass,		src_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		1,
		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		19,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_editBtn,
		XmNtopOffset,			20,
		XmNborderWidth,			2,
		XmNfractionBase,		20,
		NULL);    	

    dataw_createDomBox(dom_form);
    XtManageChild (dom_form);

    XtManageChild(_srcOptn); 
    XtManageChild(src_form);
}

/*=====================================================================*/

void dataw_createDomBox ( Widget parent )
/************************************************************************
 * dataw_createDomBox							*
 *									*
 * This function creates the dominant type box contents.  		*
 *									*
 * void dataw_createDomBox (parent)         				*
 *									*
 * Input parameters:							*
 *  parent	Widget		parent widget               		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * E. Safford/GSC	03/00	add use_refTm initialization		*
 * T. Lee/GSC		02/01	added single time loop			*
 * E. Safford/GSC	05/01	make the dom menu of fixed size		*
 * H. Zeng/EAI          10/01   changed to use MAX_FRAME                *
 * J. Wu/SAIC		07/03	add "Range/Int" button			*
 * T. Lee/SAIC		01/04	initialized base time			*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
Widget		numf_frame, scale_rc, skip_rc, skip_frame;
Widget		time_bb;
Widget		dom_pulldn;
Widget		rc;

long		ii;
XmString	xmstr;

char		str[6];
/*---------------------------------------------------------------------*/
/*
 *  Dominant Source Pull down
 */
    dom_pulldn   = XmCreatePulldownMenu (parent, "dom_pulldn", NULL, 0);
    _domOptn     = XmCreateOptionMenu (parent, "dom_optn_menu", NULL, 0);
    _domBtn      = (WidgetList)XtMalloc((MAX_FRMSRC*MAX_LOOP)*sizeof(Widget)); 

    for (ii=0; ii< MAX_LOOP; ii++) {
	_domInfo[ii].src        = NULL;
	_domInfo[ii].use_refTm  = FALSE;
	_domInfo[ii].base_tm[0] = '\0';
    }


    for (ii=0; ii < MAX_FRMSRC*MAX_LOOP; ii++) {

    	_domBtn[ii] = XtVaCreateWidget(NONE_SELECTED,
		xmPushButtonWidgetClass,	dom_pulldn,
		XmNalignment,			XmALIGNMENT_BEGINNING,
		XmNrecomputeSize,		FALSE,
		XmNwidth,			500,
		NULL);

        XtAddCallback(_domBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)dataw_sourceCb,	(XtPointer)DOM_BTN);

        if (ii==0) {
	    XtManageChild(_domBtn[ii]);
	}
    }

    XtVaSetValues (_domOptn,
      		XmNsubMenuId,			dom_pulldn, 
		XmNx,				5,
		XmNy,				15,
		NULL);
    NxmLabel_setStr(_domOptn, "Dominant Source:");

/*
 *  Create a label showing total # of frames
 */
    _numFrmsRcW = XtVaCreateWidget("dataw_totalfRc",
                xmRowColumnWidgetClass,      	parent,
                XmNleftAttachment,       	XmATTACH_POSITION,
                XmNleftPosition,       		1,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_domOptn,
		XmNtopOffset,			10,
                XmNorientation,              	XmHORIZONTAL,
                XmNpacking,                  	XmPACK_TIGHT,
                NULL);

    XtVaCreateManagedWidget("Frames",
                xmLabelWidgetClass, 		_numFrmsRcW,
                NULL);
     
    numf_frame = XtVaCreateManagedWidget("numf_frame",
                xmFrameWidgetClass, 		_numFrmsRcW,
                XmNshadowType,      		XmSHADOW_IN,
                XmNmarginHeight,    		1,
                NULL);

    _numFrameW = XtVaCreateManagedWidget("dataw_totalLb",
                xmLabelWidgetClass,  		numf_frame,
                XmNwidth,            		30,
                XmNheight,           		20,
                XmNmarginHeight,     		1,
                XmNrecomputeSize,    		False,
                NULL);

    _numFrmSel = 1;
    sprintf(str, "%2d", _numFrmSel);
    NxmLabel_setStr(_numFrameW, "1");

    XtManageChild(_numFrmsRcW);

/*
 *  Create a scale widget 
 */
    scale_rc = XtVaCreateWidget("dataw_scale_rc",
                xmRowColumnWidgetClass,       	parent,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_domOptn,
		XmNtopOffset,			10,
                XmNleftAttachment,            	XmATTACH_WIDGET,
                XmNleftWidget,               	_numFrmsRcW,
                XmNleftOffset,                	5,
                XmNorientation,               	XmHORIZONTAL,
                XmNpacking,                   	XmPACK_TIGHT,
                NULL);

    xmstr = XmStringCreateLocalized("1");
    XtVaCreateManagedWidget("dataw_totalScaleLb1",
                xmLabelWidgetClass, 		scale_rc,
                XmNlabelString,     		xmstr,
                NULL);
    XmStringFree(xmstr);

    _numFscaleW = XtVaCreateManagedWidget("dataw_numFscaleW",
                xmScaleWidgetClass,     	scale_rc,
                XmNorientation,         	XmHORIZONTAL,
                XmNprocessingDirection, 	XmMAX_ON_RIGHT,
                XmNshowValue,           	False,
                XmNminimum,             	1,
                XmNmaximum,             	MAX_FRAME,
                XmNscaleMultiple,       	1,
                XmNvalue,               	_numFrmSel,
                XmNscaleHeight,         	23,
                XmNscaleWidth,          	100,
                NULL);
    
    XtAddCallback(_numFscaleW, 			XmNdragCallback,
                (XtCallbackProc)dataw_selTimesCb,	(XtPointer)NULL);
    XtAddCallback(_numFscaleW, 			XmNvalueChangedCallback,
                (XtCallbackProc)dataw_selTimesCb, 	(XtPointer)NULL);
  
    _numFrmAvail = MAX_FRAME;
    sprintf(str, "%d", MAX_FRAME);
    xmstr = XmStringCreateLocalized(str);
    _maxFrameW = XtVaCreateManagedWidget("dataw_maxFrameW",
                xmLabelWidgetClass,    		scale_rc,
                XmNlabelString,        		xmstr,
                NULL);

    XmStringFree(xmstr);

    XtManageChild(scale_rc);

/*
 *  Create a time_line skip factor arrow buttons
 */
    _skipRcW = XtVaCreateWidget("dataw_skipRcW",
		xmRowColumnWidgetClass, 	parent,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_domOptn,
		XmNtopOffset,			10,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			scale_rc,
		XmNleftOffset,			12,
		XmNorientation,         	XmHORIZONTAL,
		XmNmarginHeight,        	0,
		NULL);

    XtVaCreateManagedWidget("Skip",
                xmLabelWidgetClass, 		_skipRcW,
                NULL);

    skip_rc = XtVaCreateWidget("dataw_skip_rc",
                xmRowColumnWidgetClass,      	_skipRcW,
                XmNorientation,              	XmHORIZONTAL,
                XmNmarginHeight,             	0,
                NULL);

    _skipArrow[0] = XtVaCreateManagedWidget("dataw_skipArrow0",
                xmArrowButtonWidgetClass, 	skip_rc,
                XmNarrowDirection,        	XmARROW_UP,
                NULL);

    _skipArrow[1] = XtVaCreateManagedWidget("dataw_skipArrow1",
                xmArrowButtonWidgetClass, 	skip_rc,
                XmNarrowDirection,       	XmARROW_DOWN,
                NULL);
    for (ii=0; ii<2; ii++) {
        XtAddCallback(_skipArrow[ii], 		XmNactivateCallback,
                (XtCallbackProc)dataw_skipArrowCb, (XtPointer)ii); 
    }

    XtManageChild(skip_rc);
  
    skip_frame = XtVaCreateManagedWidget("dataw_skip_frame",
                xmFrameWidgetClass, 		skip_rc,
                XmNshadowType,      		XmSHADOW_IN,
                XmNmarginHeight,    		1,
                NULL);
    _skipW = XtVaCreateManagedWidget("xxxx",
                xmLabelWidgetClass,      	skip_frame,
                XmNmarginHeight,         	1,
                XmNrecomputeSize,        	False,
                NULL);

    XtManageChild(_skipRcW);

    dataw_setSkipFactor (0, True);
 
/*
 *  Create single time widget
 */ 
    _loopTmModeRcW = XtVaCreateWidget("dataw_loopTmModeRcW",
		xmRowColumnWidgetClass, 	parent,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_domOptn,
		XmNtopOffset,			10,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_skipRcW,
		XmNleftOffset,			10,
		XmNorientation,         	XmHORIZONTAL,
		XmNmarginHeight,        	0,
		NULL);


    rc = XtVaCreateManagedWidget ( "singletm_rc",
                               xmRowColumnWidgetClass, _loopTmModeRcW,
                               XmNradioBehavior,       FALSE,
                               XmNorientation,         XmHORIZONTAL,
                               XmNpacking,             XmPACK_TIGHT,
                               NULL );

    _loopTmModeRb = XtVaCreateManagedWidget ( "Single Time",
                            xmToggleButtonWidgetClass, rc,
                            XmNhighlightThickness,     0,
                            XmNset,                    FALSE,
                            NULL );

    XtAddCallback ( _loopTmModeRb, XmNvalueChangedCallback,
                    (XtCallbackProc)dataw_timeModeCb, NULL);

    XtManageChild(_loopTmModeRcW);

/*
 *  Create timeline Range/Interval selection button
 */
    _rangeBtn = XtVaCreateManagedWidget("dataw_rangeBtn",
    	 	xmPushButtonWidgetClass,	parent,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_domOptn,
		XmNtopOffset,			10,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_loopTmModeRcW,
		XmNleftOffset,			10,
		XmNorientation,         	XmHORIZONTAL,
		NULL);
    NxmLabel_setStr(_rangeBtn, "Range/Int");
    XtAddCallback(_rangeBtn,			XmNactivateCallback,
		(XtCallbackProc)dataw_rangeCb,	(XtPointer)NULL);
    
/*
 *  Create Time Line area
 */
    time_bb = XtVaCreateWidget("dataw_time_bb",
    		xmBulletinBoardWidgetClass,	parent,
		XmNy,				35,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			skip_rc,
		XmNtopOffset,			10,
		NULL);
    tmln_createCanvas(time_bb);
    XtManageChild(time_bb);	

/*
 *  Create reference time controls.    
 */
    dataw_createRefTm( parent, time_bb );

    XtManageChild(_domOptn);
}

/*=====================================================================*/

void dataw_createRefTm ( Widget parent, Widget ref_wid )
/************************************************************************
 * dataw_createRefTm                                                    *
 *                                                                      *
 * This function creates the reference time radio box area.             *
 *                                                                      *
 * void dataw_createRefTm(parent, ref_wid)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                          	*
 *  ref_wid      Widget  reference location widget (top attachment)	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/96                                           *
 * E. Safford/GSC	03/99	make toggle buttons global		*
 * H. Zeng/EAI          01/00   added _refTimeFlag initialization       *
 * T. Lee/SAIC		04/04	removed "set time" radio button		*
 ***********************************************************************/
{
    _currTm = XtVaCreateWidget("Current Time",
		xmPushButtonWidgetClass, 	parent,
                XmNorientation,         	XmHORIZONTAL,
                XmNradioBehavior,       	FALSE,
                XmNmarginHeight,        	0,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			10,
		XmNtopWidget,			ref_wid,
		XmNtopOffset,			10,
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNbottomOffset,		10,
                NULL);

    XtAddCallback(_currTm, XmNactivateCallback,	       
		  (XtCallbackProc)dataw_currTimeCb, NULL);  
    XtManageChild(_currTm);
    XtSetSensitive(_currTm, FALSE); 
}

/*=====================================================================*/

void dataw_addSource ( int menu, char *label, Boolean mk_curr, int *iret )
/************************************************************************
 * dataw_addSource							*
 *									*
 * This function adds a source entry to a source pulldown menu.		*
 *									*
 * void dataw_addSource(menu, label, mk_curr, iret)			*
 *									*
 * Input parameters:							*
 *  menu       	int		menu to add to (DOM_BTN or SRC_BTN)    	*
 *  *label	char		string for new pulldown entry		*
 *  mk_curr	Boolean		make this new source the curr selection *
 *									*
 * Output parameters:							*
 *  *iret	int		0 if normal, -1 if unable to add source *
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * E. Safford/GSC	11/99	remove reference to _curLp		*
 ***********************************************************************/
{
int		ii, cur_lp;
Boolean		default_dom;
/*---------------------------------------------------------------------*/

    *iret = 0;
    default_dom = FALSE;
    cur_lp = loop_getCurLoop();

    if (menu == SRC_BTN) {

	if (_srcItems[cur_lp] >= MAX_FRMSRC-1) {
	    *iret = -1;
	}
	else {
	    if (strcmp (label, NONE_SELECTED) == 0) {
		ii  = 0;
		_srcItems[cur_lp] = 0;
	    }
	    else {
	        ii  = _srcItems[cur_lp];
                _srcItems[cur_lp]++;
	    }
	}
    }

    if (*iret == 0) {
	dataw_addToMenu(menu, ii, label, (Boolean)(mk_curr || default_dom));
    }
}

/*=====================================================================*/

void dataw_modSource ( int target_menu, int idx, char *label, int *iret )
/************************************************************************
 * dataw_modSource							*
 *									*
 * This function modifies a source entry in a source pulldown menu.	*
 *									*
 * void dataw_modSource(target_menu, idx, label, iret)			*
 *									*
 * Input parameters:							*
 *  target_menu	int		menu to add to (DOM_BTN or SRC_BTN)    	*
 *  idx		int		the index of menu list to be modified 	*
 *  *label	char		string for modified pulldown entry	*
 *									*
 * Output parameters:							*
 *  *iret	int		0 if normal, -1 if unable to add source *
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 ***********************************************************************/
{
Widget		*wid;
/*---------------------------------------------------------------------*/

    if (target_menu == SRC_BTN) {
	wid     = &_srcBtn[idx];
    }
    else {
	wid     = &_domBtn[idx];
    }

    NxmLabel_setStr(*wid, label);

    *iret = 0;
}

/*=====================================================================*/

void dataw_removeSrc ( int loop, int frmsrc )
/************************************************************************
 * dataw_removeSrc							*
 *									*
 * This function removes a source entry from the source pulldown and	*
 * updates the _dsrc to remove the cleared source information.		*
 *									*
 * void dataw_removeSrc(loop, frmsrc)	              	 		*
 *									*
 * Input parameters:							*
 *  loop	int	loop index to the data source                   *
 *  frmsrc	int	frame source index				*
 *									*
 * Output parameters:							*
 *		NONE							*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * E. Safford/GSC	01/00   crash bug -- use MAX_LOOP, not ONE_LOOP *
 * J. Wu/GSC		07/01	crash bug -- only rm src within "loop" 	*
 * J. Wu/SAIC		09/01	correct dom. src. index after removing.	*
 * T. Piper/SAIC	06/03	added dataw_clearAttr			*
 ***********************************************************************/
{
int		ii, last;
int		src_idx;
char		src_label[256];
/*---------------------------------------------------------------------*/
/*
 *  Clear attributes for this source
 */
    dataw_clearAttr( &_dsrc[loop][frmsrc]);

    last = _srcItems[loop] - 1;
    src_idx = dataw_getPopupIdx (SRC_BTN, frmsrc);
    NxmLabel_getStr (_srcBtn[src_idx], src_label); 
   
/*
 *  If the dominant source of this "loop" matches this data source, 
 *  set it to NULL -- that dominant source has been cleared.
 */ 
    if (_domInfo[loop].src != NULL) {
        if (strcmp (_domInfo[loop].src->path, src_label) == 0) {
	    _domInfo[loop].src = NULL;
	}
    }

/*
 *  Decrement the source Items counter
 */
    if (_srcItems[loop] > 0) {
        _srcItems[loop]--;
    }

/*
 *  Remove the deleted source from the data source array
 */
    for (ii=frmsrc; ii < last; ii++) {

/*
 *  Check for dominant pointer and reassign as needed
 */
	if ( _domInfo[loop].src == &(_dsrc[loop][ii+1]) ) {
	    _domInfo[loop].src = &(_dsrc[loop][ii]);
        }

        _dsrc[loop][ii] = _dsrc[loop][ii+1];	
    }

/*
 *  Wipe the last _dsrc[loop][last] structures
 */
    dataw_clearDsrc (&_dsrc[loop][last]);
 
}

/*=====================================================================*/

void dataw_updtSources ( int prev )
/************************************************************************
 * dataw_updtSources							*
 *									*
 * This function builds/rebuilds the source menu.                     	*
 *									*
 * void dataw_updtSources ( prev )		       	 		*
 *									*
 * Input parameters:							*
 *	prev		int	previous loop number			*
 *									*
 * Output parameters:							*
 *		NONE							*
 **									*
 * Log:									*
 * E. Safford/GSC	08/99	initial coding				*
 * E. Safford/GSC	10/99	remove references to _curLp		*
 * E. Safford/GSC	04/00	clean up                   		*
 * M. Li/GSC		06/01	set _srcState				*
 * H. Zeng/EAI          03/02   made "Off" data sources noncurrent      *
 * J. Wu/SAIC		09/04	add _binSrcbtn				*
 * A. Hardy/NCEP        11/04   comment out time bin button sensitivity *
 * J. Wu/SAIC		12/04	set bin/edit button sensitivity		*
 ***********************************************************************/
{
int	ii, src, cur_lp;
char	src_label[256];
dsrc_t	*active_src;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  If the loop has not changed, then we don't need to rebuild
 *  the source menu.  Exit the routine with no changes.
 */
    if (prev == cur_lp) {
        return;
    }

/* 
 *  Start by unmanaging the old source menu items
 */
    ii = 1;
    while ( XtIsManaged(_srcBtn[ii]) ) {
        XtUnmanageChild(_srcBtn[ii]);
	ii++;
    }

/*  
 *  Load the new sources or NONE
 */
    src = _srcItems[cur_lp];

    if ( src == 0 ) {

/*
 *  Add "NONE" to the source menu
 */
	dataw_addToMenu( SRC_BTN, src, NONE_SELECTED, TRUE);
	XtSetSensitive(_modSrcBtn, FALSE);
	 	
 	NxmLabel_setStr(_srcState, SRC_OFF);	
	XtSetSensitive(_srcState, FALSE);
    }
    else {

/*
 *  Add the available item(s) to the source menu
 */
	for (ii=0; ii < src; ii++) {
    	    active_src = &_dsrc[cur_lp][ii];

	    if (!active_src->src_on) {
	        strcpy(src_label, "(Off)   ");
                strcat(src_label, active_src->path);
	        dataw_addToMenu(SRC_BTN, ii, src_label, FALSE);
	    }
	    else {
	        dataw_addToMenu(SRC_BTN, ii, active_src->path, TRUE);
	    }
	}

	XtSetSensitive(_modSrcBtn, TRUE);
	
	XtSetSensitive(_srcState, TRUE);   
    }

/*
 *  Set status for "Bin Source" and "Edit Source" buttons
 */
    dataw_setEdit ( );
    dataw_setBinSrc ( );

/*
 *  Now load the dominant menu
 */
    dataw_loadDomMenu(); 

}

/*=====================================================================*/

void dataw_addToMenu ( int menu, int idx, char *label, Boolean mk_curr )
/************************************************************************
 * dataw_addToMenu							*
 *									*
 * This function adds the label to the menu item list and manages the   *
 * menu button widget. 							*
 *									*
 * void dataw_addToMenu(menu, idx, label, mk_curr)			*
 *									*
 * Input parameters:							*
 *  menu       	int		menu to add to (DOM_BTN or SRC_BTN)    	*
 *  idx		int		index to desired menu item		*
 *  *label	char		string for new pulldown entry		*
 *  mk_curr	Boolean		make this new source the curr selection *
 *									*
 * Output parameters:							*
 *		NONE							*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * H. Zeng/EAI          06/00   removed managing&unmanaging option menu *
 ***********************************************************************/
{
Widget		*wid, *pull_dn;
/*---------------------------------------------------------------------*/

    if (menu == SRC_BTN) {
	pull_dn = &_srcOptn;
	wid     = &_srcBtn[idx];
    }
    else {
        pull_dn = &_domOptn;
	wid     = &_domBtn[idx];
    }

    NxmLabel_setStr(*wid, label);
    XtManageChild(*wid);

    if ( mk_curr) {
        XtVaSetValues(*pull_dn,
		XmNmenuHistory, *wid,
		NULL);
    }
}

/*=====================================================================*/

void dataw_loadDomMenu ( void )
/************************************************************************
 * dataw_loadDomMenu							*
 *									*
 * This function reloads the appropriate dom menu for the current panel *
 * and number of loops.							*
 *									*
 * void dataw_loadDomMenu ()                    			*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *		NONE							*
 **									*
 * Log:									*
 * E. Safford/GSC	08/99	initial coding				*
 * E. Safford/GSC	10/99	clean up, remove references to _curLp	*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * E. Safford/GSC	01/00	allow CAT_MSC data to plot a time line	*
 * M. Li/GSC		06/01	check & remove "off" source		*
 ***********************************************************************/
{
int	ii, pnl, fst_pnl, lst_pnl, idx, src, cur_lp;
dsrc_t	*active_src, *dom_src;
Boolean	ck_actv, no_srcs;
/*---------------------------------------------------------------------*/

    no_srcs  = TRUE;

/*
 *  Unmanage all menu items except for the first entry ("NONE")
 */
    ii = 1;
    while ( XtIsManaged(_domBtn[ii]) ) {
        XtUnmanageChild(_domBtn[ii]);
   	ii++;
    }

/*
 *  The number of loops indicates what panels are to be included in
 *  the dominant source menu.
 *  
 *  Until the panels are returned, the lst_pnl and fst_pnl loop 
 *  controls should be set to the current loop.
 */
    cur_lp  = loop_getCurLoop();
    lst_pnl = fst_pnl = cur_lp;
    
    dom_src = _domInfo[cur_lp].src; 
    if (dom_src != NULL && !dom_src->src_on) {
	dom_src = NULL;
    }

    if (dom_src == NULL) {
        XtVaSetValues(_domOptn,
		XmNmenuHistory, 		_domBtn[0],  /* NONE */
		NULL);
	ck_actv = FALSE;
    }
    else {
	ck_actv = TRUE;
    }

    idx=0;
    for (pnl=fst_pnl; pnl <= lst_pnl; pnl++) {
	src = _srcItems[pnl];

        for (ii=0; ii < src; ii++) {
	    active_src = &_dsrc[pnl][ii];

/*
 *  Check for the "off" data source
 */
	    if ( active_src->src_on ) {
	        if ( (dom_src == NULL && no_srcs) || 
	    	     (ck_actv && strcmp(dom_src->path, active_src->path) == 0) ) { 
                    dataw_addToMenu(DOM_BTN, idx, active_src->path, TRUE);
                    _domInfo[cur_lp].src = active_src; 
	        }
	        else {
                    dataw_addToMenu(DOM_BTN, idx, active_src->path, FALSE);
	        }
	        no_srcs = FALSE;

	        idx++;
	    }
        }
    }


    if (no_srcs) {
	dataw_addToMenu( DOM_BTN, 0, NONE_SELECTED, TRUE);
	_domInfo[cur_lp].src = NULL;
    }

    dataw_resetDom(); 
}

/*=====================================================================*/

void dataw_setEdit ( void )
/************************************************************************
 * dataw_setEdit							*
 *									*
 * This function is sets the edit button's sensitivity and updates or 	*
 * pops down the edit popup as appropiate.				*
 *									*
 * void dataw_setEdit ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * S. Law/GSC		11/99	changed defines to match new table	*
 * S. Jacobs/NCEP	 3/00	Added CAT_VGF/CAT_MSC to is_editable chk*
 * A. Hardy/GSC		 8/00   Force all edit windows to popdown       *
 * M. Li/GSC		06/01	add a check for the source status	*
 * J. Wu/SAIC		01/05	check if there is active source		*
 * S. Chiswell/Unidata   3/05   Added CAT_IMG                           *
 ***********************************************************************/
{
    int         loop, src;
    Boolean	is_editable = False;
    dsrc_t	*active_src;
    int		image_set_props(Widget, int);
/*---------------------------------------------------------------------*/

    dataw_getSelIdx (SRC_BTN, &loop, &src);
    
    if ( src >= 0 ) {          
    active_src = &_dsrc[loop][src];

        is_editable = (Boolean) (active_src->src_on && 
	          (active_src->catg == CAT_SFC || 
		   active_src->catg == CAT_SFF || 
		   active_src->catg == CAT_SND || 
		   active_src->catg == CAT_SNF ||
		   active_src->catg == CAT_VGF ||
		   active_src->catg == CAT_MSC ) );

         if ( ( active_src->src_on ) && ( active_src->catg == CAT_IMG ) )
            is_editable = (Boolean) image_set_props ( _dataW, active_src->attridx );
    }

    if ( is_editable ) {
	XtSetSensitive (_editBtn, TRUE);
    }
    else {
	XtSetSensitive (_editBtn, FALSE);
    }

/*
 *  Pop down all edit windows.
 */
    stnmw_popdown ();
    stnmw_popdownModel ();
    msc_popdown ( );

    image_props_popdown ( );
}

/*=====================================================================*/

void dataw_getSelIdx ( int popup, int *loop, int *idx )
/************************************************************************
 * dataw_getSelIdx							*
 *									*
 * This function returns the data source index number that corresponds  *
 * to the currently selected data source from either the source or the  *
 * dominant popup menues.						*
 *									*
 * void dataw_getSelIdx ( popup, loop, idx )           	 		*
 *									*
 * Input parameters:							*
 *	popup	int		Source or Dom menu button		*
 *									*
 * Output parameters:							*
 *  	*loop	int		loop containing the selected index      *
 *  	*idx  	int		index of data source, or -1 if not found*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/99	initial coding				*
 * E. Safford/GSC	06/00	restrict functioning to current loop	*
 ***********************************************************************/
{
int		ii;
Widget		wid;
XmString	xm_str;
char		*str;
/*---------------------------------------------------------------------*/

    *idx = -1;
    *loop = loop_getCurLoop();

    if (popup == SRC_BTN) {
/*
 *  The source menu is constructed in the same order as the
 *  _dsrc[lp][] is loaded, so finding the number of the current
 *  _srcBtn will give the source index.
 */ 
        XtVaGetValues(_srcOptn,
    		XmNmenuHistory,			&wid,
		NULL);

        for (ii=0; ii<_srcItems[*loop]; ii++) {
	    if (wid == _srcBtn[ii]) {
	        *idx = ii; 
	        break;
	    }
        }
    }
    else {

/*
 *  Find the source index for the dominant source by 
 *  pulling the dominant path info and matching it in 
 *  the loop's sources.
 */ 
        XtVaGetValues(_domOptn,
    		XmNmenuHistory,			&wid,
		NULL);

     	XtVaGetValues(wid,
		XmNlabelString,			&xm_str,
		NULL);

	XmStringGetLtoR(xm_str, XmFONTLIST_DEFAULT_TAG, &str);
	XmStringFree(xm_str);

	for (ii=0; ii<_srcItems[*loop]; ii++) {

	    if (strcmp (str, _dsrc[*loop][ii].path) == 0) {
	        *idx = ii;
		break;
	    }
	}
	XtFree(str);
    }
}

/*=====================================================================*/

int dataw_getPopupIdx ( int popup, int idx )
/************************************************************************
 * dataw_getPopupIdx							*
 *									*
 * This function returns the index to either the source or dominant     *
 * popup menu that corresponds to data source index number.		*
 *									*
 * int dataw_getPopupIdx ( popup, idx )                	 		*
 *									*
 * Input parameters:							*
 *	popup	int		Source or Dom menu button		*
 *	idx	int		data source index number  		*
 *									*
 * Output parameters:							*
 * dataw_getPopupIdx	int	index number of the desired popup menu  *
 *				or -1 on error.    			*
 **									*
 * Log:									*
 * E. Safford/GSC	07/99	initial coding				*
 ***********************************************************************/
{
int	rtn;
/*---------------------------------------------------------------------*/

    rtn = -1;

    if (popup == SRC_BTN) {
	rtn = idx;
    }
    else {
	rtn = idx + 1;
    }

    return (rtn);
}

/*=====================================================================*/

void dataw_initDsrc ( void )
/************************************************************************
 * dataw_initDsrc							*
 *									*
 * This function initializes the _dsrc structure and supporting objs.	*
 *									*
 * void dataw_initDsrc()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 ***********************************************************************/
{
int	loop, frmsrc, ii;
/*---------------------------------------------------------------------*/

    for (loop=0; loop< MAX_LOOP; loop++) {

        for (frmsrc=0; frmsrc<(MAX_FRMSRC); frmsrc++) {
	    dataw_clearDsrc (&_dsrc[loop][frmsrc]);
	}
	_srcItems[loop] = 0;

    }

    for (ii=0; ii < MAX_LOOP; ii++) {
        _domInfo[ii].src = NULL;
    }
}

/*=====================================================================*/

void dataw_clearAttr (  dsrc_t *src )
/************************************************************************
 * dataw_clearAttr							*
 *									*
 * This function clears the attributes of a specific data source	*
 *									*
 * void dataw_clearAttr (src)						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      *src    dsrc_t          data source structure                   *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	06/03	initial coding				*
 * M. Li/SAIC		02/08	added case CAT_ENS			*
 ***********************************************************************/
{
    int ier;
    char cnull[1];
/*---------------------------------------------------------------------*/

    cnull[0] = '\0';

    switch ( src->catg ) {
        case CAT_IMG:
            nim_satt(src->attridx, cnull, cnull, cnull, &(src->attridx), &ier);
            break;

        case CAT_SFC:
        case CAT_SFF:
	    nsf_satt(src->attridx, cnull, SCAT_NIL, cnull, cnull, cnull, cnull,
						cnull, &(src->attridx), &ier); 
	    break;

	case CAT_SND:
        case CAT_SNF:
	    nsn_satt(src->attridx, cnull, SCAT_NIL, cnull, cnull, cnull, cnull,
						cnull, cnull, cnull,
						&(src->attridx), &ier);
            break;

        case CAT_GRD:
	case CAT_ENS:
            ngd_satt(src->attridx, cnull, SCAT_NIL, cnull, cnull,
						&(src->attridx), &ier);
	    break;

        case CAT_VGF:
        case CAT_MSC:
	    nms_satt(src->attridx, cnull, SCAT_NIL, cnull, 0, (NMS_types*)NULL, 
						0, (NMS_flags*)NULL, &(src->attridx), &ier);
            break;
    }
}

/*=====================================================================*/

void dataw_clearDsrc ( dsrc_t *src )
/************************************************************************
 * dataw_clearDsrc							*
 *									*
 * This function clears the elements of a dsrc_t structure.     	*
 *									*
 * void dataw_clearDsrc (src)						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	*src	dsrc_t		data source structure			*
 *									*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 ***********************************************************************/
{
    int	ii;
/*---------------------------------------------------------------------*/

    src->catg     = NO_SOURCE;
    src->path[0]  = '\0';
    src->attridx  = NO_SOURCE; 
    src->num_sel  = 0;
    src->skip     = 0;

    for (ii=0; ii<MAX_FRAME; ii++) {
        src->frm[ii].ftime[0]  = '\0';
        src->frm[ii].selected = FALSE; 
    }
}

/*=====================================================================*/

void dataw_setAttr ( dsrc_t *src )
/************************************************************************
 * dataw_setAttr							*
 *									*
 * This function sets the attributes for a source in the correct data	*
 * driver.								*
 *									*
 * void dataw_setAttr (src)						*
 *									*
 * Input parameters:							*
 *	*src	dsrc_t		data source structure			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	06/99	initial coding				*
 * M. Li/SAIC		02/08	added grd_setEnsAttr			*
 ***********************************************************************/
{
    switch ( src->catg ) {
        case CAT_IMG:
 	    image_setAttr(src);
	    break;

	case CAT_SFC:
	case CAT_SFF:
	case CAT_SND:
	case CAT_SNF:
  	    obs_setAttr(src); 
	    break;

	case CAT_GRD:
	    grd_setAttr(src);
	    break;

	case CAT_ENS:
	    grd_setEnsAttr(src);
            break;

	case CAT_VGF:
        case CAT_MSC:
	    stnmw_popdown ( );
	    msc_setAttr(src);
	    break;
    }
}

/*=====================================================================*/

void dataw_setSkipFactor ( int skip, Boolean adjust )
/************************************************************************
 * dataw_setSkipFactor                                                  *
 *                                                                      *
 * This function sets the time_line skip factor.                        *
 *                                                                      *
 * void dataw_setSkipFactor (skip, adjust)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      skip            int     time line skip factor.                  *
 *      adjust          Boolean whether to adjust current selection     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           08/98                                           *
 * S. Law/GSC           12/98   adjusted parameters for this and tmln   *
 ***********************************************************************/
{
    dataw_updSkipFactor (skip);
    tmln_setSkipFactor (skip, adjust); 
}

/*=====================================================================*/

void dataw_updSkipFactor ( int skip )
/************************************************************************
 * dataw_updSkipFactor                                                  *
 *                                                                      *
 * This function updates the time_line skip factor label.               *
 *                                                                      *
 * void dataw_updSkipFactor(skip)                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  skip       int      time line skip factor.                          *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * G. Krueger/EAI  10/97        NxmSetLabel->NxmLabel_setStr            *
 * C. Lin/EAI      08/98        rename & modify to show skip factor     *
***********************************************************************/
{
    char     str[4];
/*---------------------------------------------------------------------*/
/*
 *  Update skip factor
 */
    sprintf(str, "%d", skip);
    NxmLabel_setStr(_skipW,  str);
}

/*=====================================================================*/

void dataw_resetDom ( void )
/************************************************************************
 * dataw_resetDom                                                       *
 *                                                                      *
 * This function sets the reference time and the corresponding          *
 * composite time widget if there has been a change in the dom          *
 * menu or selection.                                                   *
 *                                                                      *
 * void dataw_resetDom( )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/99	initial coding                          *
 * E. Safford/GSC	03/00	set reference time sensitivity          *
 * E. Safford/GSC	04/00	clean up (need more!!!)                 *
 * E. Safford/GSC	01/01	redefine ref time sensitivity categories*
 * J. Wu/SAIC		07/03	set range/intv selection sensitivity	*
 * J. Wu/SAIC		08/03	made range/intv insensitive for VGF	*
 * T. Lee/SAIC		08/03	made range/intv sensitive except VGF	*
 * T. Lee/SAIC		12/03	made ref. time insensitive for forecast	*
 * T. Lee/SAIC		01/04	added  ref. time			*
 * T. Lee/SAIC		04/04	removed set time			*
 ***********************************************************************/
{ 
int		idx, lp, cur_lp;
Boolean		allow_reftm;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  Set the dominant source pointer to the new dom.  This is 
 *  the current menu selection in the dominant source menu.
 */
    dataw_getSelIdx (DOM_BTN, &lp, &idx);

    if (idx < 0) {
	_domInfo[cur_lp].src = NULL;
      	XtSetSensitive (_currTm, FALSE);  
    }
    else {

/*
 *  Set the _domInfo[cur_lp].src pointer to the new selection in
 *  the dominant menu.  Use this source's num selected & skip values
 */
        _domInfo[cur_lp].src = &(_dsrc[lp][idx]);

/*
 *  Set the reference time RowColumn widget
 */
	allow_reftm = (Boolean)(_domInfo[cur_lp].src->catg != CAT_NIL);

      	XtSetSensitive (_currTm,      (int)allow_reftm); 
	XtSetSensitive (_refTimeLbl,  (int)allow_reftm);
  	XtSetSensitive (_refTimeTxt,  (int)allow_reftm);
	    
    }

    if ( _domInfo[cur_lp].use_refTm && allow_reftm ) {
	dataw_setRefTm(&_domInfo[cur_lp].ref_tm);
    }
    
    dataw_updateTmln(); 

/*  
 *  Turn off range/intv selection for VGF.
 */
    if ( _domInfo[cur_lp].src != NULL ) {
        if ( _domInfo[cur_lp].src->catg == CAT_VGF ) {
	    XtSetSensitive ( _rangeBtn, FALSE );
        }
        else {
	    XtSetSensitive ( _rangeBtn, TRUE );    
        }    
    }
    else {
	    XtSetSensitive ( _rangeBtn, FALSE );
    }            
}

/*=====================================================================*/

void dataw_updateTmln ( void )
/************************************************************************
 * dataw_updateTmln                                                     *
 *                                                                      *
 * This function updates the time line display.                         *
 *                                                                      *
 * void dataw_updateTmln( )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/99	initial coding                          *
 * E. Safford/GSC	10/99	remove references to _curLp             *
 * E. Safford/GSC	11/99   avoid drawing timeline with 0 ntimes    *
 * S. Law/GSC		11/99	changed defines to match new table	*
 * E. Safford/GSC	01/00	cleaned up CAT_MSC and CAT_VGF handling *
 * E. Safford/GSC	03/00	add tmln clearTimeInfo and _drawClear   *
 * E. Safford/GSC	04/00	clean up                                *
 * E. Safford/GSC	04/00	fix direction problem on CAT_SFF data	*
 * S. Law/GSC		06/00	reworked to use tmln_getNewTimes	*
 * S. Law/GSC		06/00	unworked for CAT_MSC			*
 * S. Law/GSC		06/00	changed to call dataw_getTimeStr	*
 * S. Jacobs/NCEP	 7/00	Check sub-cat number instead of category*
 * E. Safford/GSC	08/00	fix initialization error on dsrc times  *
 * E. Safford/GSC	11/00	fix reset problem with CAT_VGF          *
 * T. Lee/GSC		02/01	added single time loop			*
 * T. Lee/GSC		03/01	changed call seq. of loop_setTmMode	*
 * E. Safford/GSC	06/01	update params for dataw_getTimeStr	*
 * E. Safford/SAIC	12/01	incr msel for vgf source		*
 * J. Wu/SAIC		05/02	assign the new times to data source	*
 * E. Safford/SAIC	06/02	mv single time logic to dataw_timeMdCb  *
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * M. Li/SAIC		02/08	add CAT_ENS				*
 * F. J. Yen/NCEP	04/08	added d7m, d8m, & dmrf to ctb_dtget CS	*
 ***********************************************************************/
{
    int		ntimes, ii, jj, nsel, msel, cur_lp, ier;
    int		isbcat, d3, d4, d5, d6, d7, d7m, d8, d8m, dmrf, dtmch,
		dionoff;
    char	source[256], *alias, d1[256], d2[256], tmpstr[256];
    dttms_t	tmarry[MAX_FRAME], selarry[MAX_FRAME], time;
    Boolean	select[MAX_FRAME], dirflg;
    dsrc_t	*dsrc;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();
    dsrc = _domInfo[cur_lp].src;

    if (dsrc != NULL) {

/*
 *  Check time line buttons:
 *
 *  For grid data, if single time flag is ON, set time
 *  line buttons (total frames and skip factor) insensitive.
 *  If OFF, set time line buttons sensitive.
 *
 */
	if ( dsrc->catg == CAT_GRD || dsrc->catg == CAT_ENS ) {
	    dataw_setTmModeSnstv (TRUE);
	    if (_singleTm) {
		dataw_setTmlnSnstv(FALSE);
	    }
	    else {
		if (!_tmlnActv) {
		    dataw_setTmlnSnstv(TRUE);
		}
            }
	}
	else {
/*
 *  For non-grid data, set single time selection button
 *  OFF and insensitive.  Set time line buttons sensitive.
 */
	    if ( _singleTm ) {
		loop_setTmMode(cur_lp, FALSE);
		dataw_resetTmMode(cur_lp);
	    }
	    dataw_setTmModeSnstv (FALSE);
            if (!_tmlnActv) dataw_setTmlnSnstv(TRUE);
	}

/*
 *  Get the data alias name to find the sub-cat number. 
 *  This is used to determine the direction of the time line.
 */
	if  ( dsrc->catg == CAT_VGF )  {
	    strcpy ( tmpstr, "VGF" );
	}
	else if ( dsrc->catg == CAT_ENS ) {
            dslw_getFrstMod ( dsrc->path, tmpstr );
        }
	else {
	    strcpy (source, dsrc->path);
	    alias  = strtok(source, "/");
	    alias  = strtok(NULL, "/");
	    strcpy ( tmpstr, alias );
	}

	ctb_dtget ( tmpstr, d1, d2, &d3, &isbcat, &d4, &d5, &d6, 
		    &dionoff, &d7, &d7m, &d8, &d8m, &dmrf, &dtmch, &ier );

	dirflg = (Boolean) ( isbcat == SCAT_FCT ||
			     isbcat == SCAT_SFF ||
			     isbcat == SCAT_SNF );	
	
	if (dsrc->catg == CAT_MSC) { 

/*
 *  CAT_MSC data is handled differently from the other sources.
 *  The tmarry[] is not retrieved via tmln_getNewTimes(), so it
 *  must be loaded directly from the source.  Also the source 
 *  times are not wiped like with the other sources.
 */
	    msel=0;
	    for (ii=0; ii < MAX_FRAME; ii++) { 
		if (strlen(dsrc->frm[ii].ftime) > (size_t)0) {
		    strcpy (tmarry[ii], dsrc->frm[ii].ftime);
  		    if (dsrc->frm[ii].selected) { 
			msel++;
		    }
		}
		else {
		    break;
		}
	    }
	    ntimes = ii;
	}
	else {

/*
 *  Update the timeline in these steps:
 *
 *  1)  get the new times for the data source (dsrc)
 *  2)  save off the selected times for the dsrc in selarry[]
 *  3)  wipe the existing time and selected information in the dsrc
 *  4)  write the new times to the dsrc and mark those that match entries
 *					in the selarry[] as selected.
 *  5)  build an array of flags, in select[], to pass into tmln_setTimeInfo()
 */

/*
 *  1)  get the new times for the source
 */
	    dataw_getTimeStr (cur_lp, FALSE, time);
	    tmln_getNewTimes (dsrc, time, &ntimes, tmarry);

/*
 *  2)  save off the currently selected times into selarry[].
 */	    
	    for (ii = 0, nsel = 0; ii < MAX_FRAME; ii++) {
		if (dsrc->frm[ii].selected) {
		    strcpy (selarry[nsel], dsrc->frm[ii].ftime);
		    nsel++;
		}
	    }

/* 
 *  3)  wipe the dsrc times and select flags
 */
	    for (ii = 0; ii < MAX_FRAME; ii++) { 
	        dsrc->frm[ii].ftime[0] = '\0'; 
		dsrc->frm[ii].selected = FALSE; 
	    } 

/*
 *  4)  copy the new times to dsrc and update the selection status
 */
	    msel=0;
	    for (ii = 0; ii < ntimes; ii++) { 
		strcpy (dsrc->frm[ii].ftime, tmarry[ii]); 
	   
		if (dsrc->catg == CAT_VGF && ii == 0) {
		    dsrc->frm[ii].selected = TRUE;
		    msel++;
		}
		else {
		    for (jj = 0; jj < nsel; jj++) {
		        if (strcmp (dsrc->frm[ii].ftime, selarry[jj]) == 0) {
			    dsrc->frm[ii].selected = TRUE;
			    msel++;
			    break;
		        }
		    }
		}
	    }
  
	    if (msel != nsel) loop_setDataChngd (cur_lp, TRUE);
	    
	}

/*
 *  5)  Load the select[] array for the tmln_setTimeInfo() call. 
 *      From 0 to (ntimes-1), the select flag comes from the dsrc.  
 *      From ntimes to MAX_FRAME, there are no times: set these to 
 *      FALSE.  The dsrc should have these times set to False as 
 *      well, but again, let's be defensive.
 */
        for (ii=0; ii < MAX_FRAME; ii++) { 	
	    if ( ii < ntimes ) {
	        select[ii] = dsrc->frm[ii].selected;
  	    }
	    else {
		select[ii] = FALSE;
		tmarry[ii][0] = '\0';
	    }
	}

/*
 *  Now update the time line.
 */
	if (ntimes > 0 ) {

	    dataw_setSkipFactor (dsrc->skip, FALSE);
	    tmln_setTimeInfo(dirflg, ntimes, tmarry, select);
	    tmln_redraw(0);

	    dsrc->num_sel = msel;

	    dataw_setFrameScale(msel, ntimes);
	}    
    }

/*
 *  If no dominant source or no times for source, draw empty time line
 */
    if (dsrc == NULL || ntimes <= 0) {
	dataw_setTmlnSnstv (FALSE);

	if ( _singleTm ) {
	    loop_setTmMode(cur_lp, FALSE);
	    dataw_resetTmMode(cur_lp);
	}
	dataw_setTmModeSnstv (FALSE);
	tmln_clearTimeInfo();
	tmln_drawClear();
    }
}

/*=====================================================================*/

void dataw_setFrameScale ( int sel, int max )
/************************************************************************
 * dataw_setFrameScale                                                  *
 *                                                                      *
 * This function sets the maximum total number of frames.               *
 *                                                                      *
 * void dataw_setFrameScale(sel, max)                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *     sel	int             number of selected frames               *
 *     max	int             maximum number of frames available      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 * E. Safford/GSC	01/00	disable frame slider when max == 1	*
 * S. Law/GSC		06/00	changed scale setting			*
 * T. Lee/GSC		02/01	check single time flag			*
 * E. Safford/SAIC	06/02	rm _currTm variable			*
 ***********************************************************************/
{
    char	str[5];
/*---------------------------------------------------------------------*/

    if ( _singleTm ) return;

    _numFrmAvail = max;
    sprintf(str, "%2d", _numFrmAvail);
    NxmLabel_setStr(_maxFrameW, str);

    if (max == 1) {
        XtSetSensitive( _numFscaleW, FALSE);
        XtSetSensitive( _numFrmsRcW, FALSE);
    }
    else {
	if (!XtIsSensitive (_numFscaleW) ) {
            XtSetSensitive( _numFscaleW, TRUE);
	}
        if (!XtIsSensitive( _numFrmsRcW ) ) {
            XtSetSensitive( _numFrmsRcW, TRUE);
	}

        XtVaSetValues(_numFscaleW, 
		      XmNmaximum,	_numFrmAvail, 
		      XmNvalue,		sel, 
		      NULL);
    }

    sprintf(str, "%2d", sel);
    NxmLabel_setStr(_numFrameW, str);
}

/*=====================================================================*/

void dataw_setTmlnSnstv ( Boolean state )
/************************************************************************
 * dataw_setTmlnSnstv                                                   *
 *                                                                      *
 * This function set the sensitive flags of the time line control       *
 * areas: total frames, time interval, and reference time.              *
 *                                                                      *
 * void dataw_setTmlnSnstv(state)                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *     state	Boolean         sensitive/insensitve switch             *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 * S. Jacobs/NCEP	 1/01	Removed unmanage&manage of arrow widgets*
 * J. Wu/SAIC		07/03	add _rangeBtn                         	*
 * J. Wu/SAIC		08/03	remove _rangeBtn			*
 ***********************************************************************/
{
    _tmlnActv = state;

    XtSetSensitive( _numFscaleW, (int)state);
    XtSetSensitive( _numFrmsRcW, (int)state);
    XtSetSensitive( _skipRcW, (int)state);

}

/*=====================================================================*/

void dataw_setTmModeSnstv ( Boolean state )
/************************************************************************
 * dataw_setTmModeSnstv							*
 *                                                                      *
 * This function set the sensitive flags of the single time loop.	*
 *                                                                      *
 * void dataw_setTmModeSnstv(state)					*
 *                                                                      *
 * Input parameters:                                                    *
 *     state	Boolean         sensitive/insensitve switch             *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC		02/01	initial coding                          *
 ***********************************************************************/
{
    XtSetSensitive( _loopTmModeRcW, (int)state);
}

/*=====================================================================*/

void dataw_getTimeStr ( int lp, Boolean set_flag, char *time )
/************************************************************************
 * dataw_getTimeStr 							*
 *									*
 * This function fills the string 'time' with the reference time, if it	*
 * is appropiate, or the current time, if it is not.  It will also call	*
 * dtmbw_dttmSet if the set_flag is TRUE.  Time is assumed to be of	*
 * type dttms_t (size DTTMS_SIZE).					*
 *									*
 * void dataw_getTimeStr (lp, set_flag, time)				*
 *									*
 * Input parameters:							*
 *	lp		int		loop for which time is sought	*
 *	set_flag	Boolean		whether to call dtmbw_dttmSet	*
 *									*
 * Output parameters:							*
 *	*time		char		GEMPAK date/time string		*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	initial coding				*
 * S. Jacobs/NCEP	10/00	Make sure null is not past end of time	*
 * E. Safford/GSC	06/01	add lp param				*
 * B. Yin/SAIC		03/04	changed css_gtim calling sequences	*
 * T. Lee/SAIC		04/04	removed dtmbw_dttmSet call		*
 ***********************************************************************/
{
    int		tm[5], ier, itype = 1;
/*---------------------------------------------------------------------*/

    if (_domInfo[lp].use_refTm) {
	tm[0] = _domInfo[lp].ref_tm.year;
	tm[1] = _domInfo[lp].ref_tm.mon;
	tm[2] = _domInfo[lp].ref_tm.day;
	tm[3] = _domInfo[lp].ref_tm.hour;
	tm[4] = _domInfo[lp].ref_tm.min;

/*
 *  Move month to range 1 - 12 instead of 0 - 11
 */
	tm[1]++;
	ti_itoc (tm, time, &ier, DTTMS_SIZE);

	time[DTTMS_SIZE-1] = '\0';
    }
    else {
	css_gtim (&itype, time, &ier);
    }
}

/*=====================================================================*/

void dataw_loadDomTimes ( void )
/************************************************************************
 * dataw_loadDomTimes 							*
 *									*
 * This function loads the dominant times of the current loop.		*
 *									*
 * void dataw_loadDomTimes ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	initial coding				*
 * E. Safford/GSC	06/01	update params for dataw_getTimeStr	*
 ***********************************************************************/
{
    int		selected, skip, lp;
    dttms_t	sel_arry[MAX_PIX], time;
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop ();
    dataw_getTimeStr (lp, TRUE, time);
    tmln_getSelected (&selected, sel_arry);
    tmln_getSkipFactor (&skip);

    dataw_loadTimes (_domInfo[lp].src, time, selected, skip);
    dataw_resetDom (); 
}

/*=====================================================================*/

void dataw_updtPnlOptn ( void )
/************************************************************************
 * dataw_updtPnlOptn                                                    *
 *                                                                      *
 *                                                                      *
 * void dataw_updtPnlOptn()	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 ***********************************************************************/
{
/*
Boolean		pnl_snstv;
int		ii;
char		*left_right[]  = { "Left", "Right" };
char		*top_bottom[]  = { "Top", "Bottom" };
char		*four_pnl[]    = { "Upper Left", "Upper Right",
				   "Lower Left", "Lower Right"};
*/
/*---------------------------------------------------------------------*/
/*
    pnl_snstv = TRUE;
    switch (_pnlDsply) {
	case ONE_X_ONE:
	    pnl_snstv = FALSE;
	    break;

	case ONE_X_TWO:
	    for (ii=0; ii < 2; ii++) {
                NxmLabel_setStr (_pnlBtn[ii], left_right[ii]); 
	    }
	    for (ii=2; ii<4; ii++) {
		XtUnmanageChild(_pnlBtn[ii]);
	    }
	    break;

	case TWO_X_ONE:
	    for (ii=0; ii < 2; ii++) {
                NxmLabel_setStr (_pnlBtn[ii], top_bottom[ii]); 
	    }
	    for (ii=2; ii<4; ii++) {
		XtUnmanageChild(_pnlBtn[ii]);
	    }
	    break;

	case TWO_X_TWO:
	    for (ii=2; ii<4; ii++) {
		XtManageChild(_pnlBtn[ii]);
	    }
	    for (ii=0; ii<4; ii++) {
                NxmLabel_setStr (_pnlBtn[ii], four_pnl[ii]); 
	    }
	    break;

	default:
	    break;
    }

    if (pnl_snstv) {
        XtVaSetValues(_pnlOptn,
	    XmNmenuHistory, 		_pnlBtn[0],
	    NULL);
    }
    _pnlVal = 0;

    if (XtIsSensitive(_pnlOptn) != pnl_snstv) {
	XtSetSensitive(_pnlOptn, pnl_snstv);
    }
*/
}

/*=====================================================================*/

void dataw_updtLoopOptn ( void )
/************************************************************************
 * dataw_updtLoopOptn                                                   *
 *                                                                      *
 * This routine updates the loop option menu based upon the current     *
 * setting of the panel arrangement menu.				*
 *                                                                      *
 * void dataw_updtLoopOptn( )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 ***********************************************************************/
{
Boolean		lp_snstv;
int		ii;
/*---------------------------------------------------------------------*/

    lp_snstv = TRUE;

    switch (_pnlDsply) {
	case ONE_X_ONE:
	    for (ii=0; ii < 4; ii++) {
	        XtManageChild(_loopBtn[ii]);
	    }

            XtVaSetValues(_loopOptn,
		XmNmenuHistory, 		_loopBtn[0],
		NULL);
	    break;

	case ONE_X_TWO:
	case TWO_X_ONE:
	    for (ii=0; ii < 2; ii++) {
	        XtManageChild(_loopBtn[ii]);
	    }
	    for (ii=2; ii < 4; ii++) {
	        XtUnmanageChild(_loopBtn[ii]);
	    }

	    break;

	case TWO_X_TWO:
	default:
    	    lp_snstv = FALSE;
	    break;
    }

    if (lp_snstv) {
        XtVaSetValues(_loopOptn,
		XmNmenuHistory, 		_loopBtn[0],
		NULL);
    }

    if (XtIsSensitive(_loopOptn) != lp_snstv) {
	XtSetSensitive(_loopOptn, (int)lp_snstv);
    }
}

/*=====================================================================*/

void dataw_setDsplyMenu ( int btn )
/************************************************************************
 * dataw_setDsplyMenu                                                   *
 *                                                                      *
 * This routine updates the current selection of the panel arrangement  *
 * (or display) menu.							*
 *                                                                      *
 * void dataw_setDsplyMenu ( btn )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *	btn	int	button index					*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 ***********************************************************************/
{
/*  XtVaSetValues(_dsplyOptn,
		XmNmenuHistory, 		_dsplyBtn[btn],
		NULL);

    dataw_updtPnlOptn(); */
}

/*=====================================================================*/

void dataw_setLoopMenu ( int lp )
/************************************************************************
 * dataw_setLoopMenu                                                    *
 *                                                                      *
 * This routine updates the panel and loop menus and sets the current	*
 * selection in each.                     				*
 *                                                                      *
 * void dataw_setLoopMenu ( lp )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int		loop number				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 ***********************************************************************/
{
    if (lp > -1 && lp < MAX_LOOP) {
        XtVaSetValues(_loopOptn,
		XmNmenuHistory, _loopBtn[lp],
		NULL);
    }
}

/*=====================================================================*/

void dataw_resetAutoUpdate ( int lp )
/************************************************************************
 * dataw_resetAutoUpdate                                                *
 *                                                                      *
 * This function resets the auto update menu.                           *
 *                                                                      *
 * void dataw_resetAutoUpdate ( lp )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * E. Safford/GSC	05/00	reset based on dominant source		*
 * J. Wu/SAIC		12/03	set state to FALSE if dom src is not img*
 * E. Safford/SAIC	03/04   rm set state to False if no image	*
 ***********************************************************************/
{
int	updt;
/*---------------------------------------------------------------------*/

    if ( !dataw_isImgDom(lp) ) {

	if (dataw_isUp() && XtIsManaged(_autoOptn) ) {
	    XtUnmanageChild (_autoOptn);
	}
	    
    }
    else {

/*
 *  Auto-update is possible 
 */
	if ( dataw_isUp() ) { 
	
	    if ( XtIsManaged(_autoOptn) ) {
	        XtUnmanageChild (_autoOptn);
	    }

	    updt = (int) loop_getAutoUpdt(lp);
	    XtVaSetValues (_autoOptn,
		XmNmenuHistory,			_autoBtn[updt],
		NULL);

	    XtManageChild (_autoOptn);
	}	
    }
}

/*=====================================================================*/

void dataw_resetRoam ( int lp )
/************************************************************************
 * dataw_resetRoam                                                      *
 *                                                                      *
 * This function resets the roam value option menu.                     *
 *                                                                      *
 * void dataw_resetRoam ( lp )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          11/99   initial coding                          *
 * E. Safford/GSC	10/00	fix bug in callback value on roam menu	*
 * T. Lee/GSC		03/01	changed call seq. of loop_getRoamVal	*
 * J. Wu/SAIC           08/01   remove call of loop_getRoamVal		*
 * M. Li/SAIC           01/02   remove _roamVal                         *
 * J. Wu/SAIC           04/02   remove the increment of ctr		*
 * E. Safford/SAIC	02/06	make wid_val long (64 bit fix)		*
 ***********************************************************************/
{
int	roam_value, ii, ctr;
long	wid_val;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_roamOptn) ) {
        XtUnmanageChild (_roamOptn);
    } 

    roam_value = loop_getRoamVal(lp);

/*
 *  Match the roam value to the correct menu item and set the
 *  menuHistory accordingly.
 */
    ctr = 0;

    for (ii=0; ii < _numRoam; ii++) {
	XtVaGetValues (_roamBtn[ ii ],
		XmNuserData,	&wid_val,
		NULL);

	if (wid_val == ( long )roam_value) {
	    ctr = ii;
	    break;
	}
    }

    XtVaSetValues (_roamOptn,
		XmNmenuHistory,	       _roamBtn[ctr],
		NULL);

    XtManageChild (_roamOptn);	

}

/*=====================================================================*/

void dataw_resetTmMode ( int lp )
/************************************************************************
 * dataw_resetTmMode							*
 *									*
 * This function resets the single time loop button.			*
 *                                                                      *
 * void dataw_resetTmMode ( lp )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 **									*
 * Log:									*
 * T. Lee/GSC		02/01	initial coding				*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * E. Safford/SAIC	06/02	rm _currTm variable			*
 ***********************************************************************/
{
    if ( XtIsManaged(_loopTmModeRcW) ) {
        XtUnmanageChild (_loopTmModeRcW);
    }
	
    _singleTm = loop_getTmMode (lp);
			    
/*
 *  Match the single time selection to the button state.
 */
    XmToggleButtonSetState ( _loopTmModeRb, (int)_singleTm, TRUE );
    XtManageChild (_loopTmModeRcW);	
    if ( _singleTm ) dataw_setTmlnSnstv (FALSE);
}

/*=====================================================================*/

void dataw_setRefTm ( dttmi_t *dttm )
/************************************************************************
 * dataw_setRefTm                                                       *
 *                                                                      *
 * This function sets the reference time and rebuilds the time line.	*
 *                                                                      *
 * void dataw_setRefTm ( dttm )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *	*dttm		dttmi_t	pointer to date/time struct		*
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/00	initial coding                  	*
 ***********************************************************************/
{
int	cur_lp;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();
    _domInfo[cur_lp].ref_tm = *dttm;
}

/*=====================================================================*/

void dataw_setAutoMenu ( int state )
/************************************************************************
 * dataw_setAutoMenu                                                    *
 *                                                                      *
 * This function sets the auto-update menu.          			*
 *                                                                      *
 * void dataw_setAutoMenu ( state )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      state		int	which state                             *
 *                              1 - On, 0 - Off               		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/00	initial coding                          *
 * J. Wu/SAIC		08/01	rename & call dataw_setAutoUpdt()       *
 ***********************************************************************/
{
    int		updt;
/*---------------------------------------------------------------------*/
    
    updt = ( state == AUTO_UPDT_OFF ) ? AUTO_UPDT_OFF : AUTO_UPDT_ON; 
 
    XtVaSetValues (_autoOptn,
		XmNmenuHistory,		_autoBtn[AUTO_UPDT_OFF],
		NULL);
    
    dataw_setAutoUpdt( updt );
}

/*=====================================================================*/

void dataw_cancel ( void )
/************************************************************************
 * dataw_cancel                                                         *
 *                                                                      *
 * This function terminates any active operations of the data window    *
 * and closes it.   						        *
 *                                                                      *
 * void dataw_cancel ( )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/00	initial coding                          *
 * E. Safford/GSC	10/00	remove loop number reset attempt        *
 * E. Safford/GSC	11/00	don't reset map if mapw is still up     *
 * T. Lee/GSC		02/01   added single time status		*
 * T. Lee/GSC		03/01	changed call seq. of roam and time mode	*
 * M. Li/GSC            03/01   added nmp_restormap                     *
 * M. Li/GSC		05/01	replaced nmp_restormap with nmp_restore	*
 * M. Li/GSC		06/01	added nmp_nmp_rstrproj 			*
 * H. Zeng/EAI          07/01   added call to pgpalw_setupOper()        *
 * J. Wu/SAIC		08/01	remove call to loop_restoreRoamVal 	*
 * E. Safford/SAIC	09/01	add call to pgpalw_refreshPgen		*
 * M. Li/SAIC		01/02	added loop_restoreRoamVal		*
 * E. Safford/SAIC	06/02	rm _currTm variable			*
 * H. Zeng/SAIC		09/04	set _usingSPF				*
 ***********************************************************************/
{
int	ii, jj, cur_lp, ier;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  Restore all previous source information
 */
    ngd_rest (&ier);
    nim_rest (&ier);
    nms_rest (&ier);
    nsf_rest (&ier);
    nsn_rest (&ier);
    nmp_restore (&ier);
    nmp_rstrproj (cur_lp, &ier);
    loop_restoreRoamVal();

    for (ii=0; ii < MAX_LOOP; ii++) {
        for (jj=0; jj<MAX_FRMSRC; jj++) {
	    _dsrc[ii][jj] = _dsrcCopy[ii][jj];		
        }
		
        _srcItems[ii] = _srcItemsCopy[ii];

        loop_setDataChngd (ii, FALSE); 
    }


    for (ii=0; ii < MAX_LOOP; ii++) {
        _domInfo[ii] = _domInfoCopy[ii];
    }

    loop_restoreTmMode();

    _singleTm = loop_getTmMode (cur_lp);

    _pnlDsply = _pnlDsplyCopy;

    _usingSPF = _restoreUsingSPF;

    dataw_popdown();
    
/*
 *  Reset product generation
 */
    if ( pgpalw_isUp() ) {
	 pgpalw_refresh();
    }
}    

/*=====================================================================*/
/* ARGSUSED */
void dataw_timeModeCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dataw_timeModeCb                                                    	*
 *                                                                      *
 * Callback function for the single time loop radio buttons.           	*
 *                                                                      *
 * void dataw_timeModeCb (wid, which, cbs)                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which              long		Which item                      *
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC		02/01   Initial coding                          *
 * T. Lee/GSC		03/01	changed call seq. of loop_setTmMode	*
 * E. Safford/SAIC	06/02	mv single time frame select logic here  *
 * S. Jacobs/NCEP	 6/02	Added condition for time select reset	*
 * M. Li/SAIC           02/08   add CAT_ENS                             *
 ***********************************************************************/
{
Boolean		tm_flag, sg_flag;
int		cur_lp, ii;
dsrc_t		*dsrc;
XmToggleButtonCallbackStruct *call = (XmToggleButtonCallbackStruct *)cbs;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();	
    dsrc = _domInfo[cur_lp].src;

    tm_flag = loop_getTmMode (cur_lp);
    sg_flag = (Boolean)call->set;

    if  ( tm_flag != sg_flag )  {

	_singleTm = sg_flag;

	loop_setTmMode(cur_lp, _singleTm);
	loop_setDataChngd (cur_lp, TRUE);

/*
 *  Update the frame selected flags for the dominant source.
 *  First verify the dsrc is not null.  This is just a bit of 
 *  defensive (paranoid) programming, as single time mode should only 
 *  be available if the dominant source is GRID.
 */   
	if ( dsrc ) {

/*
 *  Next, make sure we have the type of dominant source we expect.
 */
	    if ( dsrc->catg == CAT_GRD || dsrc->catg == CAT_ENS ) {

/*
 *  If single time mode has been switched on, set the first frame
 *  to selected (True) and all others to not selected (False).
 *
 *  Else, set all available frames to selected (True).
 */
		if ( _singleTm ) {
		    dsrc->frm[0].selected =  True;
		    for (ii=1; ii<MAX_FRAME; ii++) {
			dsrc->frm[ii].selected = False;
		    }
		}
		else {
		    for (ii=0; ii<MAX_FRAME; ii++) {
			if ( strlen(dsrc->frm[ii].ftime) > (size_t)0 ) {
			    dsrc->frm[ii].selected = True;
			}
		    }
		}
	    }
	}
    }

/*
 *  Now update the timeline display.
 */
    dataw_updateTmln(); 
}

/*=====================================================================*/

static void dataw_updtImgRoam ( void )
/************************************************************************
 * dataw_updtImgRoam                                                  	*
 *                                                                      *
 * The routine updates the Size of Image option in the roam menu.  If  	*
 * an image data source is found, the Size of Image option is activated.*
 *                                                                      *
 * static void dataw_updtImgRoam ( void )                              	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	04/01   Initial coding                          *
 ***********************************************************************/
{
int	cur_lp;
Boolean	img_roam_ok;
/*---------------------------------------------------------------------*/
    
    cur_lp = loop_getCurLoop();	

    img_roam_ok = dataw_isImgInLoop(cur_lp);

    XtSetSensitive(_roamBtn[_imgSzBtn], (int)img_roam_ok);

}

/*=====================================================================*/

static void dataw_resetBtn ( void )
/************************************************************************
 * dataw_resetBtn                                                       *
 *                                                                      *
 * This function resets the modify and edit buttons if source state	*
 * changes.								* 
 *                                                                      *
 * static void dataw_resetBtn ()                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		06/01	initial coding                          *
 * H. Zeng/EAI          08/01   added check to the value of "src"       *
 * J. Wu/SAIC		10/04	set status for _binSrcBtn             	*
 * A. Hardy/NCEp	11/04   Comment out time binning sensitivity    *
 * J. Wu/SAIC		12/04	activate time bin & add dataw_setBinSrc	*
 ***********************************************************************/
{
    int         loop, src;
    char	src_label[256];
    dsrc_t      *active_src;
/*---------------------------------------------------------------------*/

    dataw_getSelIdx (SRC_BTN, &loop, &src);

    if ( src >= 0 ) {
         active_src = &_dsrc[loop][src];
         NxmLabel_getStr(_srcState, src_label);

/*
 *  Reset source state and modify button
 */
         if (active_src->src_on) {
	     if ( strcmp(src_label, SRC_ON) == 0) {
                  NxmLabel_setStr(_srcState, SRC_OFF);
             }

             if ( !XtIsSensitive(_modSrcBtn) ) {
                  XtSetSensitive (_modSrcBtn, TRUE);
             }

         }
         else {
             if ( strcmp(src_label, SRC_OFF) == 0) {
                  NxmLabel_setStr(_srcState, SRC_ON);
             }

             if ( XtIsSensitive(_modSrcBtn) ) {
                  XtSetSensitive (_modSrcBtn, FALSE);
             }
         }

/*
 *  Reset edit button
 */
	dataw_setEdit();
	 
/*
 *  Reset source binning button
 */
	dataw_setBinSrc();

    }        
    else {

/*
 *  Disable source state button if no data
 */
         NxmLabel_setStr(_srcState, SRC_OFF);
	 if ( XtIsSensitive(_srcState) ) {
	      XtSetSensitive (_srcState, FALSE);
	 }
    }       
}

/*=====================================================================*/

static void dataw_setTitles ( void )
/************************************************************************
 * dataw_setTitles							*
 *									*
 * The routine generates an icon name and a window title if an SP file	*
 * has been loaded, or set the icon name and the window title to the 	*
 * DEFAULT_NMAP2_ICON_NAME value. So the icon name and the window title *
 * will either be "nmap2", or "nmap2:joes.spf".               		*
 *									*
 * static void dataw_setTitles ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 **									*
 * Log:									*
 * H. Zeng/SAIC         09/04   Initial coding                          *
 * J. Wu/SAIC         	03/05   rename to dataw_setTitles()		*
 * E. Safford/SAIC	07/07	chng param list for mainw_setTitleName	*
 ***********************************************************************/
{
 int    ier;
 char   attrstr[128], icon_nam[128];
/*---------------------------------------------------------------------*/

    if ( _usingSPF ) {

	attrstr[0] = '\0';
	spf_gtfld ( "file_name", attrstr, &ier);

	if ( strcmp ( attrstr, "NONE" ) != 0 &&
	    attrstr[0] != '\0'                 ) {

	    sprintf( icon_nam, "nmap2: %s", attrstr );
	    mainw_setIconName ( icon_nam );
    	    mainw_setTitleName ( attrstr, NULL );

	    return;
	}
    }

    mainw_setIconName ( DEFAULT_NMAP2_ICON_NAME );
    mainw_setTitleName ( NULL, NULL );
}

/*=====================================================================*/

void dataw_setLoop ( int lp )
/************************************************************************
 * dataw_setLoop	 						*
 *									*
 * This function switches from the current loop to a given loop.	*
 *									*
 * void dataw_setLoop( lp ) 						*
 *									*
 * Input parameters:							*
 *  	lp		int		loop number			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	move from dataw_loopCb()		*
 ***********************************************************************/
{
    int		prev;
/*---------------------------------------------------------------------*/

    if ( lp < 0 || lp > ( MAX_LOOP - 1 ) )  return;    
    
    prev = loop_getCurLoop( );

/*
 *  Switch the main window to this loop & update loop button state.
 */
    loop_changeLoop ( lp );
    dataw_setLpBtns( );

/*
 *  Update the data window menus for this loop.
 */
    dataw_resetTmMode ( lp );
    dataw_updtSources ( prev );
    
    dataw_resetBtn( );
    dataw_resetAutoUpdate ( lp );

    dataw_resetRoam ( lp );  
    dataw_updtImgRoam( );
}

/*=====================================================================*/

void dataw_setLpBtns ( void )
/************************************************************************
 * dataw_setLpBtns                                               	*
 *                                                                      *
 * This function updates the state of the loop selection buttons.  	*
 *                                                                      *
 * void dataw_setLpBtns ( )                        			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/GSC		07/01	revise from mbtnw_setMbtns()		*
 ***********************************************************************/
{
    int		cur_lp;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  Set the loop menu 
 */
    XtVaSetValues (_loopOptn,
		   XmNmenuHistory,	_loopBtn[cur_lp],
		   NULL);
}

/*=====================================================================*/

void dataw_loadsp ( void )
/************************************************************************
 * dataw_loadsp	 							*
 *									*
 * This function loads data settings from a selected SPF file.		*
 *									*
 * Note: the SPF file should have been loaded into SPF buffer before    *
 *       call this function.  Otherwise, nothing will be loaded.	*
 *									*
 * void dataw_loadsp( ) 						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 * J. Wu/GSC		07/01	call spfw_getSource() and adjust frame	*
 *				number & skip factor			*
 * J. Wu/GSC		07/01	validate dom. src & set cursor status	*
 * J. Wu/SAIC		08/01	allow restoring roam factors		*
 * J. Wu/SAIC		08/01	restore auto-update & map settings	*
 * J. Wu/SAIC		08/01	restore map overlay selections		*
 * J. Wu/SAIC		08/01	signal invalid settings and eliminate	*
 *				case-sensitivity for overlay names	*
 * J. Wu/SAIC		08/01	clear roams & report invalid overlays	*
 * J. Wu/SAIC		08/01	clear overlays before restoring		*
 * E. Safford/SAIC	10/01   fix error in count of loaded times	*
 * M. Li/SAIC           01/02   remove _roamVal                         *
 * H. Zeng/EAI          03/02   removed setting _domInfoCopy[lp].src    *
 * J. Wu/SAIC		04/02	restore the data source's attributes	*
 * J. Wu/SAIC		04/02	restore the map overlay attributes	*
 * J. Wu/SAIC		04/02	check the roam factor agst. table value	*
 * J. Wu/SAIC		05/02	restore the single time mode		*
 * E. Safford/SAIC	06/02	rename _selectTm -> _singleTm		*
 * R. Tian/SAIC		02/03	add some initialization calls		*
 * J. Wu/SAIC		07/03	restore the source's range & interval	*
 * T. Lee/SAIC		05/04	set reference time to GUI		*
 * H. Zeng/SAIC		09/04	added _usingSPF flag			*
 * T. Lee/SAIC		10/04	get bin hours				*
 * T. Piper/SAIC	01/05	Added &_spfIonoff to spfw_getTmBin	*
 * m.gamazaychikov/SAIC	01/06	Changed condition to G_DIFF		*
 * H. Zeng/SAIC		03/07   restore reference time value		*
 * M. Li/SAIC           02/08   add CAT_ENS                             *
 * F. J. Yen/NCEP	04/08	Update calling sequence to spfw_getTmBin*
 ***********************************************************************/
{
    int		lp, src, ier, flp_hasData, catgNum, adjFrms, ntimes, ierr;
    int		ii, dom, skip, frms, times, roamFac, srcInd, autoUpdt, iyear;
    int		jj, kk, nn, ovlnum, ovlcnt, in_frms, st_mode, ticknum;
    char	garea_tag[128], tagStr[32], dataStr[200], pathStr[256];
    char	domStr[16], skipStr[16], frmStr[16], st1[16], *st2;
    Boolean	lp_hasSrc, adjFrames, lp_hasDom, ovlMatched, lp_hasOvl;
    char	map[21], proj[25], errStr[256];
    char	ovlStr[1024], *ovlToken, ovlsep[] = "|", *ovlPos;
    double	tmp;
    nmpovlstr_t	ovlNmsStr, ovlnms[MAX_OVL], in_ovlName, tbl_ovlName; 
    nmpovlstr_t	ovlAttr, ovlTmpNms; 
    dsrc_t	*dataSrc;
    dttms_t	tmarry[MAX_FRAME], timeStr;                            
    nmpstr_t	garea[2];
/*---------------------------------------------------------------------*/

    flp_hasData = -1;   /* The first loop which have active sources */
            
    NxmCursor_setCursor( _dataW, CURS_BUSY );
        
/*
 *  Get map overlay number and names.
 */
    nmp_govlnum( &ovlnum, &ier );
    nmp_govlnms( ovlnms, &ier );      

/*
 *  Init related data strctures.
 */
    ngd_init(&ier);
    nim_init(&ier);
    nms_init(&ier);
    nsf_init(&ier);
    nsn_init(&ier);
    
/*
 *  Populate the data window with the data settings in the SPF file.
 */
    for ( lp = 0; lp < MAX_LOOP; lp++ ) {

	lp_hasSrc = FALSE;  /* If a loop has any active sources */
        lp_hasDom = FALSE;  /* If a loop has a valid dominant source */
        adjFrames = TRUE;   /* Adjust the selected frames or not */
	srcInd = 0;
	
/*
 *  Reset loop, roam, and zoom  buttons.
 */
	dataw_setLpBtns( );
	loop_setRoamVal(lp, 0);  /* FIT_TO_SCREEN */
	dataw_resetRoam( lp );
	zoomw_clearZoom (lp);
	_domInfo[lp].use_refTm = FALSE;
	
/*
 *  Validate the dominant source first.
 */
	sprintf( tagStr, "%s%d%s", "loop", lp+1, "_dominant" );
	spf_gtfld ( tagStr, domStr, &ier );
        dom = atoi(domStr);

	if ( dom > 0 ) {
	    
	    spfw_getSource( lp, dom-1, &catgNum, pathStr, &ier );    
	    if ( ier == 0 ) lp_hasDom = TRUE;
	    
	}

/*
 *  Set the reference time info for dominant source before loading 
 *  any source.
 *
 *  It is OK that the SPF file doesn't have "_reftime" tag&value
 *  pair. But try to get its value first. If successful, set the
 *  reference time of the dominant source; If not, just ignore it
 *  and go ahead.
 */
	sprintf( tagStr, "%s%d%s", "loop", lp+1, "_reftime" );
	spf_gtfld ( tagStr, timeStr, &ier );
       
	if ( ier == 0 ) {

	  sscanf ( timeStr, "%02d%02d%02d/%02d%02d", 
        	   &_domInfo[lp].ref_tm.year,
        	   &_domInfo[lp].ref_tm.mon,
        	   &_domInfo[lp].ref_tm.day,
        	   &_domInfo[lp].ref_tm.hour,
        	   &_domInfo[lp].ref_tm.min );
	  _domInfo[lp].ref_tm.mon--;
	  iyear = _domInfo[lp].ref_tm.year;
	  ti_yy24 ( &iyear, &iyear, &ier);
	  _domInfo[lp].ref_tm.year = iyear;
	  _domInfo[lp].use_refTm = TRUE;

          _refTimeStrc = _domInfo[lp].ref_tm;

	}
	
	for ( src = 0;  src < MAX_FRMSRC; src++ ) {
		    
/* 
 *  Get data sources and add to menu if present.
 */					    	    
	    spfw_getSource( lp, src, &catgNum, pathStr, &ier );

	    if ( ier == 0 )  {    
                
		srcInd++;
		
		if ( lp_hasDom && dom == (src+1) )  {
		    dom = srcInd;
		}
		
                if ( !lp_hasSrc )  {
		    lp_hasSrc = TRUE;
    		    loop_changeLoop ( lp );     
		}
				
/*
 *  Retrieve range/interval before adding source to menu.
 */
		spfw_getRangeIntv ( lp, srcInd, &_spfRange,
		                    &_spfInterval, &_spfDelta, &ier );
		spfw_getTmBin ( lp, srcInd, &_spfIonoff, &_spfBfhr,
			&_spfBfmn, &_spfAfhr, &_spfAfmn, &_spfMstrctf, &ier );
		
		dataw_setDataSrc ( NEW_SOURCE, catgNum, pathStr );
		
/*
 *  Restore associated attributes.
 */
		dataSrc = dataw_getDataSrc ( lp, src );
		dataw_loadSrcAttr ( dataSrc, lp, srcInd );
	    		
	    }
	}

/*
 *  If a loop has any sources, set loop-specific settings.
 *  Also remember the first loop that has data.
 */	
	if ( lp_hasSrc == TRUE ) {
                
	    if ( flp_hasData == -1 ) {
	        flp_hasData = lp;
	    }
	                	    	    
            if ( !lp_hasDom ) {
	        dom = 1; 
                ierr = -9;
		sprintf( errStr, "%s%d%s", " Loop ", lp+1, ":  " );
		strcat( errStr, domStr );
		er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );    
	        NxmErr_update( );
	    }
	    
	    _domInfo[lp].src = &(_dsrc[lp][dom-1]);

	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_skip" );
	    spf_gtfld ( tagStr, skipStr, &ier );
	    skip = atoi(skipStr);
	    	     	    	    	    	    
	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_frames" );
	    spf_gtfld ( tagStr, frmStr, &ier );
	    frms = atoi(frmStr);
	    in_frms = frms;

/*
 *  Adjust the selected frames if necessary. The final number of
 *  selected frames is dependent on the skip factor and the number
 *  of available frames.  Start with a count of the actual loaded times.
 */
	    ntimes = 0;
	    for (ii=0; ii<MAX_FRAME; ii++) {
	        if (strlen(_dsrc[lp][dom-1].frm[ntimes].ftime) > (size_t)0 ) {
		    ntimes++;
		}
		else {
		    break;
		}
	    }

	    if ( !isdigit( ( unsigned long )skipStr[0] ) || skip > ntimes )  {
	        skip = 0;	    
                ierr = -10;
		sprintf( errStr, "%s%d%s", " Loop ", lp+1, ":  " );
		strcat( errStr, skipStr );
		er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );    
	        NxmErr_update( );
	    }

	    _dsrc[lp][dom-1].skip = skip;
	    
	    if ( frms <= 0 || frms > ntimes ) { 

		frms = ntimes;
                if ( skip == 0 ) {
                    for ( ii = 0; ii < ntimes; ii++ ) {
	                _dsrc[lp][dom-1].frm[ii].selected = TRUE;
	            }
		   
		   adjFrames = FALSE; 
		}
	    }
           	    	    	   
	    _dsrc[lp][dom-1].num_sel = frms;	    
	    
	    if ( _domInfo[lp].src->catg == CAT_VGF ) adjFrames = FALSE;
	    
	    if ( adjFrames ) {
                		
                for ( ii = 0; ii < ntimes; ii++ ) {
	            _dsrc[lp][dom-1].frm[ii].selected = FALSE; 
	        }
		
	        adjFrms = 0;
		
/* 
 *  Select frames starting from the latest.
 */
		if ( _domInfo[lp].src->catg == CAT_GRD || 
		     _domInfo[lp].src->catg == CAT_ENS ||
		     _domInfo[lp].src->catg == CAT_SFF ||
		     _domInfo[lp].src->catg == CAT_SNF )  { 
	    
		    times = 0;
	            while ( adjFrms < frms && times < ntimes ) {
	                _dsrc[lp][dom-1].frm[times].selected = TRUE;
	                times = times + skip + 1;
		        adjFrms++;		        
	            }		     
		}
		else {

		    times = ntimes - 1;
		    while ( adjFrms < frms && times >= 0 ) {
	                _dsrc[lp][dom-1].frm[times].selected = TRUE;
	                times = times - skip - 1;
		        adjFrms++;
		    }		     		
		} 	        			    	    

		_dsrc[lp][dom-1].num_sel = adjFrms;
	

	    } /* End of frame adjustment */
	    
	    if ( in_frms != _dsrc[lp][dom-1].num_sel ) {
		ierr = -11;
		sprintf( errStr, "%s%d%s%s%s%d", " Loop ", lp+1, ":  ",
		         frmStr, " -> ",  _dsrc[lp][dom-1].num_sel );
		er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );
	        NxmErr_update( );
	    }          
	    	    	    	        	            
/*
 *  Restore single time mode if dominant source is CAT_GRD.
 *  Note the number of frames should be adjusted accordingly.	     
 */
	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_single_time" );
	    spf_gtfld ( tagStr, dataStr, &ier );
	    st2 = cst_split ( dataStr, '|', 128, st1, &ier );		    
               
	    st_mode = atoi( st1 );
	    
	    dataw_getTimeStr ( lp, FALSE, timeStr );
	    tmln_getNewTimes ( _domInfo[lp].src, timeStr, &ntimes, tmarry );

	    if ( _domInfo[lp].src->delta_rt > 0 ) {

		sscanf ( timeStr, "%02d%02d%02d/%02d%02d", 
        	&_domInfo[lp].ref_tm.year,
        	&_domInfo[lp].ref_tm.mon,
        	&_domInfo[lp].ref_tm.day,
        	&_domInfo[lp].ref_tm.hour,
        	&_domInfo[lp].ref_tm.min );
		_domInfo[lp].ref_tm.mon--;
		iyear = _domInfo[lp].ref_tm.year;
		ti_yy24 ( &iyear, &iyear, &ier);
		_domInfo[lp].ref_tm.year = iyear;
		_domInfo[lp].use_refTm = TRUE;
	    }
	        
	    ticknum = 1;
	    if ( st2 != NULL )  ticknum = atoi( st2 );	    
	    if ( ticknum <= 0 || ticknum > ntimes ) ticknum = 1; 	    

	    if ( st_mode == 1 ) {
	        if ( _domInfo[lp].src->catg == CAT_GRD  || _domInfo[lp].src->catg == CAT_ENS ) {
		    _singleTm = (Boolean)st_mode;

		    loop_setTmMode( lp, (Boolean)st_mode );
		    dataw_resetTmMode ( lp );

                    for ( ii = 0; ii < ntimes; ii++ ) {
	                _dsrc[lp][dom-1].frm[ii].selected = FALSE; 
	            }		    
		    
		    _dsrc[lp][dom-1].num_sel = 1;
		    _dsrc[lp][dom-1].frm[ticknum-1].selected = TRUE;		
		
		}
		else {
                    ierr = -15;
		    sprintf( errStr, "%s%d", " Loop ", lp+1 );
		    er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );    
	            NxmErr_update( );		
		}	    
	    }	

/*
 *  Load the dominant menu now. 
 */
	    dataw_loadDomMenu( ); 
	    	    
/*
 *  Restore roam factors.
 */
	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_roam" );
	    spf_gtfld ( tagStr, dataStr, &ier );
	    roamFac = atoi(dataStr);
	    
	    if ( !isdigit( (unsigned long)dataStr[0] ) || !dataw_checkRoamValue(roamFac) ||
	         ( !dataw_isImgInLoop(lp) && roamFac == SIZE_OF_IMAGE ) ) {
		roamFac = 0; 
		ierr = -12;
		sprintf( errStr, "%s%d%s", " Loop ", lp+1, ":  " );
		strcat( errStr, dataStr );
		er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );
	        NxmErr_update( );
	    }

	    dataw_roamCb( NULL, roamFac, NULL );
            dataw_resetRoam( lp );
	    
/*
 *  Restore auto update state.
 */
	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_auto_update" );
	    spf_gtfld ( tagStr, dataStr, &ier );
	    autoUpdt = AUTO_UPDT_ON;
	    if ( isdigit( (unsigned long)dataStr[0] ) ) {
	        tmp = atof(dataStr);
		if (G_DIFF( tmp, 0.0 )) autoUpdt = AUTO_UPDT_OFF;
	    }
	    else {
		ierr = -13;
		sprintf( errStr, "%s%d%s", " Loop ", lp+1, ":  " );
		strcat( errStr, dataStr );
		er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );    
	        NxmErr_update( );
	    }

	    if ( dataw_isImgInLoop(lp) ) {
	        dataw_setAutoMenu( autoUpdt );
	    }

/*
 *  Restore map settings.
 */
	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_map" );
	    spf_gtfld ( tagStr, map, &ier );

	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_proj" );
	    spf_gtfld ( tagStr, proj, &ier );

	    sprintf( tagStr, "%s%d%s", "loop", lp+1, "_garea" );
	    spf_gtfld ( tagStr, garea_tag, &ier );
	    ovlPos = cst_split (garea_tag, ovlsep[0], NMP_STR, garea[0], &ier);
	    if ( ovlPos != NULL )
		ovlPos = cst_split (ovlPos, ovlsep[0], NMP_STR, garea[1], &ier);
	    else
		garea[1][0] = CHNULL;
	    nmp_valid( lp, map, garea, proj, &ier );
	    if ( ier == -14 ) {
	        strcpy( map, "Custom");
	        nmp_valid( lp, map, garea, proj, &ier );
	    }

	    if ( ier == 0 ) {
		nmp_smapattr ( lp, map, proj, garea, FALSE, &ier );
		if ( strlen(garea[1]) > (size_t)0) zoomw_setZoom(lp);
	    }
	    else {

		sprintf( errStr, "%s%d%s", "Loop ", lp+1, ":  "); 

		if ( ier == -18 ) {
		    strcat( errStr, garea_tag);
		}
		else if ( ier == -19 ) {
		    strcat( errStr, proj );
		}
		else {
		    errStr[0] = '\0';
		}		    
                    
		strcat( errStr, " - " );
		er_wmsg ( "NMP", &ier, errStr, &ierr, 3, strlen(errStr) );
		NxmErr_update( );			    

	    }
	    
/*
 *  Restore map overlay settings.	     
 */
            sprintf( tagStr, "%s%d%s", "loop", lp+1, "_map_ovl" );
	    spf_gtfld ( tagStr, ovlStr, &ier );
	    
	    if ( ier != 0 ) {
	        sprintf( errStr, "%s%d", "Loop ", lp+1); 
		strcat( errStr, " has no map overlays presented. Default used." );
		NxmWarn_show ( _dataW, errStr );
	    } 
	    else {    
	        	    	    
	        ovlcnt = 0;
	        lp_hasOvl = FALSE;
		ovlToken = strtok( ovlStr, ovlsep );
	        ovlAttr[0] = '\0';
		while ( ovlToken != NULL && ovlcnt < ovlnum ) {
		 		
	            ovlPos = cst_split ( ovlToken, ':', 128,
		                         ovlTmpNms, &ier );		    
		    if ( ovlPos != NULL ) strcpy ( ovlAttr, ovlPos );
		    
/* 
 *  Chop off the leading/ending space in the token. 
 */                		    		    		    
		    cst_ldsp ( ovlTmpNms, ovlTmpNms, &kk, &ier );
	            cst_lstr ( ovlTmpNms, &kk, &ier );
		    cst_ncpy ( ovlNmsStr, ovlTmpNms, kk, &ier );
                    		    
		    ovlMatched = FALSE;
		    cst_lcuc ( ovlNmsStr, in_ovlName, &ier );
		    for ( jj = 0; jj < ovlnum; jj++ ) {
		    
		        cst_lcuc ( ovlnms[jj], tbl_ovlName, &ier );
		        if ( strcmp( in_ovlName, tbl_ovlName ) == 0 ) {
		            if ( !lp_hasOvl ) {
			        lp_hasOvl = TRUE;
			        for ( nn = 0; nn < ovlnum; nn++ ) {
			            nmp_sovlflg( lp, nn, FALSE, &ier );
			        }
			    }
			    
			    nmp_sovlflg( lp, jj, TRUE, &ier );
			    if ( ovlPos != NULL ) {
			        nmp_sovlattr ( lp, jj, ovlAttr, &ier );
			    }
			    
			    ovlMatched = TRUE;
			    break;		
		        }
		    }

		    if ( !ovlMatched ) {
		        ierr = -14;
		        sprintf( errStr, "%s%d%s", "Loop ", lp+1, ":  "); 
		        strcat( errStr, ovlNmsStr );                    
		        strcat( errStr, " - " );
		        er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );
		        NxmErr_update( );			    		    
		    }

		    ovlToken = strtok( NULL, ovlsep );
	            ovlcnt++;
	        }
	    
	        nmp_setmapstr( lp, &ier );
		
	    } /* End of restore overlays */
	
	} /* End of loop_hasSrc */
    }    
        
/*
 *  Return data window to first loop that has data, or loop 0 if
 *  nothing was loaded.
 */     
    if ( flp_hasData == -1 )  {
        dataw_setLoop ( 0 ); 
    }
    else {
        dataw_setLoop ( flp_hasData );     
	_usingSPF = TRUE;
    }

    NxmCursor_setCursor( _dataW, CURS_DEFAULT );                 
    
}

/*=====================================================================*/

Boolean dataw_noActSrc ( void )
/************************************************************************
 * dataw_noActSrc                                               	*
 *                                                                      *
 * This function checks if there are any active data sources existing.  *
 *                                                                      *
 * Boolean dataw_noActSrc ( )                        			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *   dataw_noActSrc	Boolean		Any active data sources?	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/GSC		07/01	initial coding				*
 ***********************************************************************/
{
    int		ii, jj;
/*---------------------------------------------------------------------*/
    
    for ( ii = 0; ii < MAX_LOOP; ii++ ) {
            	    	    	        
	for ( jj = 0; jj < MAX_FRMSRC; jj++ ) {
		    
           if ( _dsrc[ii][jj].src_on && _dsrc[ii][jj].attridx >= 0 ) 
	       return FALSE;
        }
    }
    return TRUE;
}

/*=====================================================================*/

void dataw_clearDataSel ( int lp ) 
/************************************************************************
 * dataw_clearDataSel                                               	*
 *                                                                      *
 * This function temporarily eliminates any data that has been selected *
 * in the given loop.  The changes will be permanent only when the new	*
 * data selections are loaded.						*
 *                                                                      *
 * void dataw_clearDataSel ( lp )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int		loop number				*
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/GSC            07/01   revise from dataw_clearLoop()           *
 ***********************************************************************/
{
    int		frmsrc, ier;
/*---------------------------------------------------------------------*/

    loop_setDataChngd (lp, TRUE);

/*
 *  Remove the data source information temporarily.
 *  The previous selections are still availbale in "_dsrcCopy".
 */
    for (frmsrc=0; frmsrc<(MAX_FRMSRC); frmsrc++) {
        dataw_clearDsrc (&_dsrc[lp][frmsrc]);
    }
    
    _domInfo[lp].src = NULL;   
    _srcItems[lp] = 0;

    dataw_updtSources(-1);

    nmp_simf(lp, " ", NO_IMG, &ier); 

    dataw_updtImgRoam();
}

/*=====================================================================*/

int dataw_getRoamVal ( int loop )
/************************************************************************
 * dataw_getRoamVal							*
 *									*
 * This function returns the current selection of roam factor.     	*
 *									*
 * int dataw_getRoamVal( loop )                	 			*
 *									*
 * Input parameters:							*
 *	loop	int		loop number				*
 *									*
 * Output parameters:							*
 * dataw_getRoamVal	int	roam factor in the given loop		*
 *				-1 - improper loop number		*
 **									*
 * Log:									*
 * J. Wu/SAIC	08/01	initial coding					*
 * M. Li/SAIC           01/02   remove _roamVal                         *
 ***********************************************************************/
{
    if ( loop >= 0 && loop < MAX_LOOP ) { 
        return loop_getRoamVal(loop); 
    }
    else {
        return -1;
    }
}

/*=====================================================================*/

static void dataw_setAutoUpdt ( int state )
/************************************************************************
 * dataw_setAutoUpdt							*
 *									*
 * This function sets the value of the auto update state.    		*
 *									*
 * static void dataw_setAutoUpdt( state )				*
 *									*
 * Input parameters:							*
 *  	state		int		which state			*
 *					1 - on,  0 - off		*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/01	initial coding                		*
 * E. Safford/SAIC	08/01	make func static			*
 * J. Wu/SAIC		08/01	remove call to auto_startAutoUpdt	*
 ***********************************************************************/
{
    int		cur_lp;
    Boolean	updt;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

    updt = (Boolean)(( state > 0 ) ?  TRUE : FALSE);

    loop_setAutoUpdt( cur_lp, updt );
}

/*=====================================================================*/

void dataw_setImageNav ( void )
/************************************************************************
 * dataw_setImageNav 							*
 *									*
 * This function sets the image navigation, if an image is present in	*
 * the data source list for the current loop.				*
 *									*
 * void dataw_setImageNav ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/01	Initial coding copied from other funcs	*
 * E. Safford/SAIC	05/04	get default LUT and save it to nim	*
 * S. Chiswell/Unidata  05/07   Add check for DEFAULT before adding .tbl*
 ***********************************************************************/
{

    int		lp, imgtyp, nindex, ier, dummyIdx;
    char	imgfile[256];
    char        imtype[MXFLSZ], iminfo[MXFLSZ];
    char        dummy[MXFLSZ],  lutfil[MXFLSZ];
    char        *ptr = NULL;

    Boolean	now_img;
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();
    now_img = dataw_isImgInLoop(lp);

    if ( now_img ) {

/*
 *  Set the image file name
 */
	if ( dataw_isRadSelect(lp, &nindex) ) {
	    imgtyp = RAD_IMG;
	    dataw_getImgInfo(lp, imgfile);
	}
	else if ( dataw_isSatSelect(lp, &nindex) ) {
	    imgtyp = SAT_IMG;
	    dataw_getImgInfo(lp, imgfile);
	}

/*
 *  Set the pre-defined map selection and the image file,
 *  then order the projection reset to these new values, and
 *  update the LUT.
 */
	nmp_simmap ( imgtyp, lp, 0, &ier );
	nmp_simf   ( lp, imgfile, imgtyp, &ier );
	nmp_sproj  ( lp, &ier ); 

	loop_restoreLut( lp );

/*
 *  Get the default LUT for this source and update the nim accordingly. 
*/
	NxmEnhw_getLutfile( imgtyp, lutfil );

/*
 *  If the lut file isn't gray, append a .tbl extension
 */
	ptr = strchr ( lutfil, '.' );
	if  ( ptr == NULL ) {
	    if ( ( strcasecmp( lutfil, "gray") != 0 ) && 
		( strcasecmp( lutfil, "default" ) != 0 ) ) {
	        strcat( lutfil, ".tbl" );
	    }
	}

	nim_qatt( nindex, imtype, iminfo, dummy, &ier );
	nim_satt( nindex, imtype, iminfo, lutfil, &dummyIdx, &ier );

	mbotw_getFadeColor ();
    }
}

/*=====================================================================*/

static void dataw_loadSrcAttr ( dsrc_t *datasrc, int lp, int srcnum )
/************************************************************************
 * dataw_loadSrcAttr	 						*
 *									*
 * This function loads/restores attibute settings for a data source.	*
 *									*
 * static void dataw_loadSrcAttr( datasrc, lp, srcnum )			*
 *									*
 * Input parameters:							*
 *     *datasrc		dsrc_t		Pointer to a data source	*
 *     lp		int		Loop number			*
 *     srcnum		int		Source seq. number in a loop	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/02	initial coding				*
 * H. Zeng/EAI          10/02   added CAT_IMG into switch construct     *
 * R. Tian/SAIC		12/02	add call dfl_tinq to check lut files 	*
 * M. Li/SAIC		04/03	Set the second color for MISC data	*
 * T. Piper/SAIC	07/03	Allocate memory based upon category	*
 * A. Hardy/NCEP	 5/04	Add fcst_flag check for CAT_SNF 	*
 * F. Y. Yen/NCEP	 6/04	Set arrw.ityp for MISC data		*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * A. Gilbert/NCEP       5/06   Increase dimension of types and flags   *
 * F. J. Yen/NCEP	 4/08	Added d7m, d8m, & dmrf to ctb_dtget CS	*
 * M. Li/SAIC		05/08   Retrieve ensemble model list 		*
 ***********************************************************************/
{
    int		ii, ier, isbcat, isub, d3, d4, d5, d6, d7, d7m, d8, d8m,
		dmrf, nn, ier1;
    int         idx, nparm, ntype, nflag, dionoff, dtmch, maxnum;
    int         ipos, intnum, intattr[20];
    char        tagStr[32], newtagStr[32], attrStr[512], **attrArray;
    char        parm[123], colors[123], vcoord[73], filnam[81];
    char        level[73], text[40], filter[10], alias[81], cycle[123];
    char        typestr[20], d1[256], d2[256];
    char        imtype[81], iminfo[81], imlutf[81];
    char        rstfil[256], yddStr[] = "[cycle]", mlist[200], cycidx[40];
    dattm_t     cycle_time;
    NMS_types   types[25];
    NMS_flags   flags[25];
    const int   maxChr = 128;  /* Max. length of a single attribute */
    long        flen;
    char        tmpfil[256];
    Boolean     fcst_flag;
/*---------------------------------------------------------------------*/


    if ( datasrc->catg == CAT_ENS ) {
        sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1, "_source", srcnum,
                          "_model_list");
        spf_gtfld ( tagStr, attrStr, &ier );

        if ( ier == 0 && strlen(attrStr) > 2 ) {
            cst_nocc ( attrStr, '+', 1, 0, &ipos, &ier );
            cst_ncpy ( mlist, attrStr, ipos, &ier );
            strcpy (cycidx, attrStr+ipos+1);

            cst_ilst ( cycidx, ',', -1, 20, intattr, &intnum, &ier );

            for ( ii = 0; ii < intnum/2; ii++ ) {
                dslw_getEnsCycle (intattr[ii*2], intattr[ii*2+1], cycle_time );
                cst_rpst( mlist, yddStr, cycle_time, mlist, &ier );
            }

            dslw_getEnsResfile(datasrc->path, rstfil);
            ngd_satt ( datasrc->attridx, mlist, SCAT_FCT, "", rstfil, &(datasrc->attridx), &ier);
	}
    }
   
    if ( datasrc->catg == CAT_IMG ) {
         sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1, "_source", srcnum,
                          "_lut");
    }
    else {   
         sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1, "_source", srcnum,
                          "_attr");
    }

    spf_gtfld ( tagStr, attrStr, &ier );
	    
    if ( ier != 0 ) return; /* No attribute settings in SPF file */

/* 
 *  Parse attributes & restore them.
 */
    switch ( datasrc->catg ) {

        case CAT_IMG:

	    nim_qatt (datasrc->attridx, imtype, iminfo, imlutf, &ier);
	    cfl_tinq (attrStr, "luts", &flen, tmpfil, &ier);
	    if ( ier == 0 && flen > 0 ) {
                sprintf(imlutf, "%s", attrStr);
	    }
            idx = datasrc->attridx;
            nim_satt(idx, imtype, iminfo, imlutf, &(datasrc->attridx), 
                     &ier); 

            break;

        case CAT_SFC:
        case CAT_SFF:
        case CAT_SND:
        case CAT_SNF:
            
	    fcst_flag = (Boolean)( (datasrc->catg == CAT_SFF) ||
		          (datasrc->catg == CAT_SNF) );
    	    dataw_getStnmName (datasrc->path, fcst_flag, typestr, alias);
			 	                     
            ctb_dtget ( typestr, d1, d2, &d3, &isub, &d4, &d5, &d6, 
		    &dionoff, &d7, &d7m, &d8, &d8m, &dmrf, &dtmch, &ier );
 
/*
 *  Query the source alias/subcategory/cycle.
 */
	    if ( isub == SCAT_SND || isub == SCAT_SNF ) {
                nsn_qatt ( datasrc->attridx, alias, &isbcat, cycle, parm, 
                           colors, level, vcoord, filter, text, &ier);
            }
            else {
                nsf_qatt ( datasrc->attridx, alias, &isbcat, cycle, parm, 
                           colors, filter, text, &ier);
            }	    

/* 
 *  Allocate memory for parsing attributes.
 */
	    attrArray = (char **) malloc ( 6 * sizeof(char *) );
	    for ( ii = 0; ii < 6; ii++) {
	        attrArray[ii] = (char *) malloc ( (size_t)maxChr );
	    }
	  
/*
 *  Parse the attribute string.
 */	    
            cst_clst ( attrStr, '|', "\0", 6, maxChr, 
	               attrArray, &nparm, &ier);
	    	    
	    strcpy ( parm, attrArray[0] );
	    strcpy ( colors, attrArray[1] );

	    if  ( isub == SCAT_SND || isub == SCAT_SNF ) {
	        strcpy ( level, attrArray[2] );
	        strcpy ( vcoord, attrArray[3] );
	        strcpy ( filter, attrArray[4] );
	        strcpy ( text, attrArray[5] );		
	    }
	    else {
	        strcpy ( filter, attrArray[2] );
	        strcpy ( text, attrArray[3] );
	    }

/*
 *  Restore the attributes.
 */
	    if ( isub == SCAT_SND || isub == SCAT_SNF ) {
		nsn_satt ( datasrc->attridx, alias, isbcat, cycle, parm,
			   colors, level, vcoord, filter, text, 
			   &datasrc->attridx, &ier );
	    }
	    else {
		 nsf_satt ( datasrc->attridx, alias, isbcat, cycle, parm,
		   colors, filter, text, &datasrc->attridx, &ier );
	    }

/*
 *  Free memory.
 */
	    for ( ii = 0; ii < 6; ii++) {
	        free ( attrArray[ii] );
	    }
	    free ( attrArray );

	  break;

	case CAT_VGF:
	case CAT_MSC:
	
	    nms_qatt ( datasrc->attridx, alias, &isbcat, filnam,
		      &ntype, types, &nflag, flags, &ier );

/* 
 *  Allocate memory for parsing attributes.
 */
            maxnum = ( ntype > nflag) ? ntype : nflag;
	    attrArray = (char **) malloc ( (size_t)maxnum*4 * sizeof(char *) );
	    for ( ii = 0; ii < maxnum*4; ii++) {
		attrArray[ii] = (char *) malloc ( (size_t)maxChr );
	    }

/*
 *  Get the file name & the numbers of types, flags.
 */
	    cst_clst ( attrStr, '|', "\0", 4, maxChr, 
	               attrArray, &nparm, &ier);
	    strcpy ( filnam, attrArray[0] );
	    ntype = atoi ( attrArray[1] );
	    nflag = atoi ( attrArray[2] );
	    	    
/*
 *  Parse all "type" related attribute settings.
 */
	    if ( ntype > 0 ) {
/*
 *  Parse the "type" attributes.
 */
		strcpy ( newtagStr, tagStr );
		strcat ( newtagStr, "_type" );
		spf_gtfld ( newtagStr, attrStr, &ier );
		
		cst_clst ( attrStr, '|', "\0", 4*ntype, maxChr, 
			   attrArray, &nparm, &ier);
		cst_numb( attrArray[2], &d3, &ier ); 	
	 	cst_numb( attrArray[3], &d4, &ier1 );	
		if ( ier == 0 && ier1 != 0 ) {
		    nn = 4;
		}
		else if ( ier != 0 && ier1 == 0 ) {
		    nn = 3;
		}

		for ( ii = 0; ii < nparm/nn; ii++ ) {
		    cst_numb( attrArray[ii*nn], &(types[ii].ionoff), &ier );
		    cst_numb( attrArray[ii*nn+1], &(types[ii].icolr), &ier );
		    cst_numb( attrArray[ii*nn+2], &d3, &ier );
		    if ( ier == 0 && nn == 4 ) {
		        types[ii].icolr2  = d3; 
			cst_crnm( attrArray[ii*nn+3], &(types[ii].value), &ier );
		    }
		    else {
			types[ii].icolr2  = IMISSD;
			cst_crnm( attrArray[ii*nn+2], &(types[ii].value), &ier );
		    }
		}
		
/*
 *  Parse the "line" attributes.
 */
		strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_line" );
                spf_gtfld ( newtagStr, attrStr, &ier );
                
		cst_clst ( attrStr, '|', "\0", 2*ntype, maxChr, 
	                   attrArray, &nparm, &ier);
		
		for ( ii = 0; ii < nparm/2; ii++ ) {
		    types[ii].line.size = (float)atof ( attrArray[ii*2] );
		    types[ii].line.iwid  = atoi ( attrArray[ii*2+1] );
		}
		
/*
 *  Parse the "sym1" attributes.
 */
	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_sym1" );
                spf_gtfld ( newtagStr, attrStr, &ier );

                cst_clst ( attrStr, '|', "\0", 3*ntype, maxChr, 
	               attrArray, &nparm, &ier);
		
		for ( ii = 0; ii < nparm/3; ii++ ) {
		    types[ii].symb[0].code = (float)atof ( attrArray[ii*3] );
		    types[ii].symb[0].size = (float)atof ( attrArray[ii*3+1] );
		    types[ii].symb[0].iwid = atoi ( attrArray[ii*3+2] );
	        }
		
/*
 *  Parse the "sym2" attributes.
 */
	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_sym2" );
                spf_gtfld ( newtagStr, attrStr, &ier );

                cst_clst ( attrStr, '|', "\0", 3*ntype, maxChr, 
	               attrArray, &nparm, &ier);
		
		for ( ii = 0; ii < nparm/3; ii++ ) {
		    types[ii].symb[1].code = (float)atof ( attrArray[ii*3] );
		    types[ii].symb[1].size = (float)atof ( attrArray[ii*3+1] );
		    types[ii].symb[1].iwid = atoi ( attrArray[ii*3+2] );
	        }

/*
 *  Parse the "arrw" attributes.
 */
	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_arrw" );
                spf_gtfld ( newtagStr, attrStr, &ier );

                cst_clst ( attrStr, '|', "\0", 3*ntype, maxChr, 
	               attrArray, &nparm, &ier);

		for ( ii = 0; ii < nparm/4; ii++ ) {
		    types[ii].arrw.size = (float)atof ( attrArray[ii*4] );
		    types[ii].arrw.hdsz = (float)atof ( attrArray[ii*4+1] );
		    types[ii].arrw.iwid = atoi ( attrArray[ii*4+2] );
		    types[ii].arrw.ityp = atoi ( attrArray[ii*4+3] );
	        }
	            
	    } /* end of "if ( ntype > 0 )" */
	    
/*
 *  Parse the "flag" settings.
 */
	    if ( nflag > 0 ) {
	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_flag" );
                spf_gtfld ( newtagStr, attrStr, &ier );

                cst_clst ( attrStr, '|', "\0", nflag, maxChr, 
	                   attrArray, &nparm, &ier);
		
		ii = 0;
		for ( ii = 0; ii < nparm; ii++ ) {
		    flags[ii].iflg = atoi ( attrArray[ii] );		
		}
	    
	    }
	    
/*
 *  Restore all attribute settings.
 */
            nms_satt ( datasrc->attridx, alias, isbcat, filnam, ntype,
	               types, nflag, flags, &datasrc->attridx, &ier );	    

/*
 *  Free memory.
 */
	    for ( ii = 0; ii < maxnum*4; ii++) {
	        free ( attrArray[ii] );
	    }
	    free ( attrArray );

	break;
	  
    } /* end of switch */ 
}

/*=====================================================================*/

static Boolean dataw_checkRoamValue ( int value )
/************************************************************************
 * dataw_checkRoamValue							*
 *									*
 * This function validates if the given value is valid as a roam factor	*
 *									*
 * static Boolean dataw_checkRoamValue( value )				*
 *									*
 * Input parameters:							*
 *  	value			int	Roam factor to be validated	*
 *									*
 * Output parameters:							*
 *				NONE					*
 *									*
 * Return parameters:							*
 *	dataw_checkRoamValue	static Boolean	True/False, 		*
 *						value valid or not	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/02	initial coding                		*
 ***********************************************************************/
{
    int		ii;
    Boolean	valid = FALSE;
/*---------------------------------------------------------------------*/
        
    for ( ii = 0; ii < _numRoam; ii++ ) {
        if ( value == _roamValues[ii] ) {
	    valid = TRUE;
	    break;
	}
    }
    return valid;
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_rangeCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_rangeCb							*
 *									*
 * Callback for the timeline Range/Interval selection activation button.*
 *									*
 * void dataw_rangeCb ( wid, which, call) 				*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/03	initial coding				*
 * T. Lee/SAIC		09/03	checked dominant data			*
 * H. Zeng/XTRIA	11/03   modified for new Range/Intv GUI		*
 * T. Lee/SAIC		05/04	added delta reference time		*
 ***********************************************************************/
{
    int		lp, range = -1, intv = -1, day, hr, min;
    char	range_day[10], range_hr[10], range_min[10];
    char	intv_day[10],  intv_hr[10],  intv_min[10];
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();

    if ( _domInfo[lp].src == NULL ) return;
  
/*
 *  Set the default range/interval as the dominant source's. 
 */ 
    strcpy (range_day,  "\0");
    strcpy (range_hr,   "\0");
    strcpy (range_min,  "\0");

    strcpy (intv_day,   "\0");
    strcpy (intv_hr,    "\0");
    strcpy (intv_min,   "\0");

    range = _domInfo[lp].src->range;

    if ( range > 0 ) {
       _rangeMin = range;
       dataw_getDHM (_rangeMin, &day, &hr, &min);

       if ( day > 0 ) sprintf (range_day, "%d", day);
       if ( hr > 0  ) sprintf (range_hr, "%d",   hr);
       if ( min > 0 ) sprintf (range_min, "%d", min);
    }

    intv = _domInfo[lp].src->interval;  

    if ( intv > 0 ) {
       _intvMin = intv;
       dataw_getDHM (_intvMin, &day, &hr, &min);

       if ( day > 0 ) sprintf (intv_day, "%d", day);
       if ( hr > 0  ) sprintf (intv_hr, "%d",   hr);
       if ( min > 0 ) sprintf (intv_min, "%d", min);
    }

    XmTextSetString( _rangeDayTxt, range_day );
    XmTextSetString( _rangeHrTxt,  range_hr  );
    XmTextSetString( _rangeMinTxt, range_min );

    XmTextSetString( _intvDayTxt, intv_day );
    XmTextSetString( _intvHrTxt,  intv_hr  );
    XmTextSetString( _intvMinTxt, intv_min );

/*
 *  Set Ref. Time info.
 */
    _timeSetFlag = 0;
    _saveUseRefTm[lp] = _domInfo[lp].use_refTm;
    if ( _domInfo[lp].use_refTm == TRUE || 
	 _domInfo[lp].src->delta_rt > 0 ) {
	_saveRefTm[lp] = _domInfo[lp].ref_tm;
	sprintf ( _currTimeStr, "%02d%02d%02d/%02d%02d",    
		  _domInfo[lp].ref_tm.year%100,
		  _domInfo[lp].ref_tm.mon+1,
		  _domInfo[lp].ref_tm.day,
		  _domInfo[lp].ref_tm.hour,
		  _domInfo[lp].ref_tm.min );
    }
    else {

       strcpy ( _currTimeStr, "\0" );
    }

    XmTextSetString (_refTimeTxt, _currTimeStr);
    XtManageChild ( _rangeSelW );
}

/*=====================================================================*/

void dataw_createRange ( Widget parent )
/************************************************************************
 * dataw_createRange                                                    *
 *                                                                      *
 * This function creates the range/interval selection window.           *
 *                                                                      *
 * void dataw_createRange ( parent )                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                          	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/03	initial	coding				*
 * H. Zeng/XTRIA	11/03   added start/end time text box		*
 * T. Lee/SAIC		02/04	allowed negative range			*
 * T. Lee/SAIC		05/04	added calendar				* 
 * E. Safford/SAIC	05/05	free ctl_btns				*
 * T. Piper/SAIC	10/05	declared nn long			*
 ***********************************************************************/
{
    int		ier;
    char	*btnstr[] = {"Accept", "Cancel", "Default"};
    Widget	rc1, rc2, rc3, rc4, rc5, ctl_rc, timerc;
    WidgetList	ctl_btns;
    XmString	title_string, xmstr;
    long	nn, ignore;
    char	iconfile[256], icon_tip[15];
    static char cflag = 0; /* flag for creating the editting popup */
/*---------------------------------------------------------------------*/
/*
 *  Create a new dialog form.
 */
    _rangeSelW = XmCreateFormDialog ( parent, "dataw_rangew", NULL, 0 );

    title_string = XmStringCreateLocalized ("Range/Interval Input");

    XtVaSetValues ( _rangeSelW, 
		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		XmNnoResize,        	True, 
		XmNdefaultPosition,	True,
		XmNdialogTitle,		title_string,
		XmNautoUnmanage,	False, 	
		NULL );
    XmStringFree (title_string);

/*
 *  Interval/Range Info.
 */
    timerc = XtVaCreateWidget ("timerc",
			       xmRowColumnWidgetClass,	_rangeSelW,
			       XmNorientation,		XmHORIZONTAL,
			       XmNpacking,		XmPACK_TIGHT,
			       NULL);

    rc1 = XtVaCreateWidget ("timelabel",
			    xmRowColumnWidgetClass,	timerc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_TIGHT,
			    NULL);

    rc2 = XtVaCreateWidget ("time_day",
			    xmRowColumnWidgetClass,	timerc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_TIGHT,
			    NULL);

    rc3 = XtVaCreateWidget ("time_hr",
			    xmRowColumnWidgetClass,	timerc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_TIGHT,
                            XmNadjustLast,              FALSE,
                            XmNadjustMargin,            FALSE,
			    NULL);

    rc4 = XtVaCreateWidget ("time_min",
			    xmRowColumnWidgetClass,	timerc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_TIGHT,
                            XmNadjustLast,              FALSE,
                            XmNadjustMargin,            FALSE,
			    NULL);

    XtVaCreateManagedWidget (" ",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		5,
			     NULL, 0);

    XtVaCreateManagedWidget ("  DAY",
			     xmLabelWidgetClass,	rc2,
			     NULL, 0);

    XtVaCreateManagedWidget ("    HR",
			     xmLabelWidgetClass,	rc3,
			     NULL, 0);

    XtVaCreateManagedWidget ("    MIN",
			     xmLabelWidgetClass,	rc4,
			     NULL, 0);

    XtVaCreateManagedWidget ("Interval:",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		5,
                             XmNmarginWidth,            0,
			     NULL, 0);

/*
 *  DAY field for Interval:
 */
    _intvDayTxt = 
	    XtVaCreateManagedWidget("day", 
                                    xmTextFieldWidgetClass, rc2,
				    XmNcolumns,               4, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_intvDayTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_intvTxtCb, (XtPointer) NULL);

/*
 *  Create "HR" boxe for Interval:
 */
    _intvHrTxt = 
	    XtVaCreateManagedWidget("hr", 
                                    xmTextFieldWidgetClass, rc3,
				    XmNcolumns,               4, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_intvHrTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_intvTxtCb, (XtPointer) NULL);

/*
 *  Create "MIN" boxe for Interval:
 */
    _intvMinTxt = 
	    XtVaCreateManagedWidget("min", 
                                    xmTextFieldWidgetClass, rc4,
				    XmNcolumns,               5, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_intvMinTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_intvTxtCb, (XtPointer) NULL);


    XtVaCreateManagedWidget ("Range:",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		12,
                             XmNmarginWidth,            0,
			     NULL, 0);

/*
 *  DAY field for Range:
 */
    _rangeDayTxt = 
	    XtVaCreateManagedWidget("day", 
                                    xmTextFieldWidgetClass, rc2,
				    XmNcolumns,               4, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_rangeDayTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_rangeTxtCb, (XtPointer) NULL);

/*
 *  Create "HR" boxe for Range:
 */
    _rangeHrTxt = 
	    XtVaCreateManagedWidget("hr", 
                                    xmTextFieldWidgetClass, rc3,
				    XmNcolumns,               4, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_rangeHrTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_rangeTxtCb, (XtPointer) NULL);

/*
 *  Create "MIN" boxe for Range:
 */
    _rangeMinTxt = 
	    XtVaCreateManagedWidget("min", 
                                    xmTextFieldWidgetClass, rc4,
				    XmNcolumns,               5, 
                                    XmNmaxLength,             5,
                                    NULL);

    XtAddCallback (_rangeMinTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_rangeTxtCb, (XtPointer) NULL);


    XtVaSetValues (timerc,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtManageChild (rc1);
    XtManageChild (rc2);
    XtManageChild (rc3);
    XtManageChild (rc4);
    XtManageChild (timerc);
    
    xmstr = XmStringCreateLtoR("Ref.  \nTime:", XmFONTLIST_DEFAULT_TAG);

    _refTimeLbl = XtVaCreateManagedWidget ("Ref.  \nTime:",
				      xmLabelWidgetClass,  _rangeSelW,
				      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNleftOffset,       5,
				      XmNtopAttachment,    XmATTACH_WIDGET,
				      XmNtopWidget,	   timerc,
				      XmNlabelString,      xmstr,
				      NULL);
    XmStringFree(xmstr);

    _refTimeTxt = XtVaCreateManagedWidget ("ref_txt",
                                      xmTextFieldWidgetClass,
							   _rangeSelW,
				      XmNcolumns,          11, 
                                      XmNmaxLength,        13,
				      XmNleftAttachment,   XmATTACH_WIDGET,
				      XmNleftWidget,	   _refTimeLbl,
                                      XmNleftOffset,       30,
				      XmNtopAttachment,    XmATTACH_WIDGET,
				      XmNtopWidget,	   timerc,
				      NULL);

    XtAddCallback (_refTimeTxt, XmNlosingFocusCallback, 
	   (XtCallbackProc)dataw_refTimeTxtCb, (XtPointer) NULL);

    cfl_inqr("cal.xbm", ICON_DIR, &ignore, iconfile, &ier);
    strcpy(icon_tip, " ");
    _refTimeCal = (Widget) NxmBxmBtn_create( _rangeSelW, "Cal", NULL,
		  ICON_WIDTH, ICON_HEIGHT, ICON_FGNAME , ICON_BGNAME,
		  NULL, iconfile, icon_tip, True, dataw_calPushbCb, 
		  (XtPointer) NULL );

    XtVaSetValues ( _refTimeCal,
		    XmNleftAttachment,   XmATTACH_WIDGET,
		    XmNleftWidget,	 _refTimeTxt,
                    XmNleftOffset,       25,
		    XmNtopAttachment,    XmATTACH_WIDGET,
		    XmNtopWidget,	 timerc,
		    NULL );

/*
 *  Create control buttons.
 */
    ctl_rc = XtVaCreateManagedWidget("ctl_rc",
                xmRowColumnWidgetClass,      	_rangeSelW,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_refTimeTxt,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			24,
                NULL);

    ctl_btns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));    
        
    rc5 = NxmCtlBtn_create ( ctl_rc, 0, "ctlBtns", 
                   XtNumber(btnstr), btnstr, NULL, ctl_btns );

    XtVaSetValues ( rc5, 
                XmNmarginHeight,	10,
                XmNmarginWidth,		10,
                XmNspacing,		10,
		NULL );
		                    
    for ( nn = 0; nn < 3; nn++ ) {
        XtAddCallback ( ctl_btns[nn], XmNactivateCallback,
   			(XtCallbackProc)dataw_rangeCtlBtnCb, (XtPointer)nn );
    }

    if (!cflag) {
	dttmw_create ( _refTimeCal );
	cflag = 1;
    }

    XtFree( (XtPointer) ctl_btns );
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_calPushbCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * dataw_calPushbCb							*
 *									*
 * Callback function for calendar push buttons.	 It pops up an edit	*
 * window for calendar.							*
 *									*
 * void dataw_pushbCb (w, clnt, call)					*
 *									*
 * Input parameters:							*
 *  w		Widget	   widget ID					*
 *  clnt	XtPointer  not used					*
 *  call	XtPointer  not used					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		05/04	Created					*
 * T. Lee/SAIC		06/04	Checked delta_rt for reference time	*
 ***********************************************************************/
{
time_t    tt;
dttmi_t   dttm, *r_tm;
struct tm *now;
int       cur_lp;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();
    XtSetSensitive ( _autoOptn, FALSE );
    dataw_setAutoMenu( AUTO_UPDT_OFF );
    r_tm = (dttmi_t *)dataw_getRefTm(cur_lp);

/*
 *  Set calendar time.
 */
    if ( r_tm->year == 0 || ( _domInfo[cur_lp].use_refTm == FALSE && 
	 _domInfo[cur_lp].src->delta_rt == -1 ) ) {
	tt = time(NULL);
	now = gmtime(&tt);
	dttm.year = 1900 + now->tm_year;
	dttm.mon  = now->tm_mon;
	dttm.day  = now->tm_mday;
	dttm.hour = now->tm_hour;
	dttm.min  = now->tm_min;
    }
    else {
	dttm.year = r_tm->year;
	dttm.mon  = r_tm->mon;
	dttm.day  = r_tm->day;
	dttm.hour = r_tm->hour;
	dttm.min  = r_tm->min;
    }

    dttmw_popup(_refTimeCal, &dttm);
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_rangeCtlBtnCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_rangeCtlBtnCb							*
 *									*
 * Callback for the control buttons (Accept, Cancel, and Default) in	*
 * the Range/Interval Input widget.  Invoked by the Range/Int button	*
 * on the Data Selection Window.					*
 *									*
 * void dataw_rangeCtlBtnCb ( wid, which, call) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/03	initial coding				*
 * J. Wu/SAIC		07/03	accept interval input only for MSC data	*
 * J. Wu/SAIC		07/03	limit interval to be smaller than range	*
 * J. Wu/SAIC		07/03	gurantee def. intv smaller than range	*
 * J. Wu/SAIC		08/03	set upper limit for ragne/intv to 14400	*
 * T. Lee/SAIC		08/03	update and increase range/intv to 28800	*
 * H. Zeng/XTRIA	11/03   modified for new Range/Intv GUI		*
 * T. Lee/SAIC		12/03	fixed negative range value		*
 * T. Lee/SAIC		03/04	auto-update button in sync with ref time*
 * T. Lee/SAIC		04/04	set delta reference time for default	*
 * T. Lee/SAIC		05/04	calendar window in sync w/ range window	*
 * T. Lee/SAIC		06/04	set navigation				*
 * T. Piper/SAIC	12/07	Removed dataw_setImageNav		*
 * S. Jacobs/NCEP	 2/08	Replaced dataw_setImageNav		*
 ***********************************************************************/
{
    char	*text=NULL;
    int		lp; 
    Boolean	changed = False;
/*---------------------------------------------------------------------*/

    lp = loop_getCurLoop();
    text = XmTextGetString ( _refTimeTxt );	
    switch ( which ) {
        
	case 0:		/* Accept */ 
	case 1:		/* Cancel */
	    break;
	
	case 2:		/* Default */

/*
 *  Retrieve default time interval/range in "datatype.tbl".
 *  set delta_rt to -1.
 */
	    dataw_getDefltIntvRng ( &_intvMin, &_rangeMin );

/*
 *  Reset "Time Selection" to Current Time.
 */
	    _spfDelta = -1;
	    _timeSetFlag = 2;
	    _domInfo[lp].src->delta_rt = -1;
	    _domInfo[lp].use_refTm = FALSE;
	    break;	
    }

/*
 *  Update the changes & reset timeline if "Apply" or "Default".
 */
    if ( which == 0 || which == 2 ) {
	if ( _rangeMin <= 28800 && 
	     _rangeMin != _domInfo[lp].src->range ) {

		_domInfo[lp].src->range = _rangeMin;
		changed = True;
	}
	if ( ( _rangeMin < 0 || _intvMin <= _rangeMin ) &&
	     _intvMin <= 28800 &&
	     _intvMin != _domInfo[lp].src->interval ) {

		_domInfo[lp].src->interval = _intvMin;
		changed = True;
	}
	if ( _timeSetFlag != 0 ) {
	   changed = True;
	}
  
	if ( changed )  {
	    dataw_resetTmln(); 
	}
/*
 *  Set auto update button to be in sync with ref. time.  
 */
	if ( which == 0 ) {
	    text = XmTextGetString ( _refTimeTxt );
            if ( text == NULL || text[0] == '\0' ) {
                XtSetSensitive  (_autoOptn, TRUE );
		_domInfo[lp].use_refTm = FALSE;
            }
            else {
                XtSetSensitive  (_autoOptn, FALSE );
		_domInfo[lp].use_refTm = TRUE;
            }
	    XtFree ( text );
	}
	else if ( which == 2 ) {
            XtSetSensitive  (_autoOptn, TRUE );
	    _domInfo[lp].use_refTm = FALSE;
	}
	/*
	 * Do not remove the call to dataw_setImageNav.
	 * This is here so that the image navigation will be set if 
	 * the user must use Range/Int because initially there were
	 * no valid images in the default time range.
	 */
	dataw_setImageNav ( );
    }
    else {
	if ( _domInfoCopy[lp].use_refTm ) {
	     _domInfo[lp].ref_tm = _saveRefTm[lp];
	}
	_domInfo[lp].use_refTm =  _saveUseRefTm[lp];
    }   
    XtUnmanageChild ( _rangeSelW );
    dttmw_popdown ();
}

/*=====================================================================*/

void dataw_resetTmln ( void )
/************************************************************************
 * dataw_resetTmln                                                     	*
 *                                                                      *
 * This function resets the time line display based on the time interval*
 * and range inputs from user.                       			*
 *                                                                      *
 * void dataw_resetTmln( void )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/03	initial coding based on dataw_updateTmln*
 * H. Zeng/XTRIA	11/03   added ref. time info.			*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * M. Li/SAIC           02/08   add CAT_ENS                             *
 * F. J. Yen/NCEP	 4/08	Added d7m, d8m, & dmrf to ctb_dtget CS	*
 ***********************************************************************/
{
    int		ntimes, ii, jj, msel, cur_lp, ier, skip, idx1, idx2;
    int		isbcat, d3, d4, d5, d6, d7, d7m, d8, d8m, dmrf, dionoff,
		dtmch;
    char	source[256], *alias, d1[256], d2[256], tmpstr[256];
    dttms_t	tmarry[MAX_FRAME], time;
    Boolean	select[MAX_FRAME], dirflg;
    dsrc_t	*dsrc;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();
    dsrc = _domInfo[cur_lp].src;

    if (dsrc != NULL) {

/*
 *  Set ref time info below:
 */
        if ( _timeSetFlag == 1 ) {

/*
 *  Change to use new ref time.
 */
           _domInfo[cur_lp].use_refTm = TRUE;
           dataw_setRefTm ( &_refTimeStrc );
        }
        else if ( _timeSetFlag == 2 ) {

/*
 *  Change to use current time.
 */
           _domInfo[cur_lp].use_refTm = FALSE;
        }

/*
 *  Check time line buttons:
 *
 *  For grid data, if single time flag is ON, set time
 *  line buttons (total frames and skip factor) insensitive.
 *  If OFF, set time line buttons sensitive.
 *
 */
	if ( dsrc->catg == CAT_GRD || dsrc->catg == CAT_ENS ) {
	    dataw_setTmModeSnstv (TRUE);
	    if (_singleTm) {
		dataw_setTmlnSnstv(FALSE);
	    }
	    else {
		if (!_tmlnActv) {
		    dataw_setTmlnSnstv(TRUE);
		}
            }
	}
	else {
/*
 *  For non-grid data, set single time selection button
 *  OFF and insensitive.  Set time line buttons sensitive.
 */
	    if ( _singleTm ) {
		loop_setTmMode(cur_lp, FALSE);
		dataw_resetTmMode(cur_lp);
	    }
	    dataw_setTmModeSnstv (FALSE);
            if (!_tmlnActv) dataw_setTmlnSnstv(TRUE);
	}

/*
 *  Get the data alias name to find the sub-cat number. 
 *  This is used to determine the direction of the time line.
 */
	if  ( dsrc->catg == CAT_VGF )  {
	    strcpy ( tmpstr, "VGF" );
	}
	else if ( dsrc->catg == CAT_ENS ) {
            dslw_getFrstMod ( dsrc->path, tmpstr );
        }
        else {
	    strcpy (source, dsrc->path);
	    alias  = strtok(source, "/");
	    alias  = strtok(NULL, "/");
	    strcpy ( tmpstr, alias );
	}

	ctb_dtget ( tmpstr, d1, d2, &d3, &isbcat, &d4, &d5, &d6, 
		    &dionoff, &d7, &d7m, &d8, &d8m, &dmrf, &dtmch, &ier );

	dirflg = (Boolean) ( isbcat == SCAT_FCT ||
			     isbcat == SCAT_SFF ||
			     isbcat == SCAT_SNF );	
	
/*
 *  Reset the timeline in these steps:
 *
 *  1)  get the new times for the data source (dsrc)
 *  2)  wipe the existing time and selected information in the dsrc
 *  3)  write the new times to the dsrc, mark selected times based
 *      on skip factor and default frame numbers.
 *  4)  build an array of flags, in select[], to pass into
 *      tmln_setTimeInfo()
 */

/*
 *  1)  get the new times for the source
 */
	dataw_getTimeStr ( cur_lp, FALSE, time );
	tmln_getNewTimes ( dsrc, time, &ntimes, tmarry );

/* 
 *  2)  wipe the dsrc times and select flags
 */
	for (ii = 0; ii < MAX_FRAME; ii++) { 
	    dsrc->frm[ii].ftime[0] = '\0'; 
	    dsrc->frm[ii].selected = FALSE; 
	} 
            
/*
 *  3)  copy the new times to dsrc and update the selection status.
 *      The first frame will always be selected. Then pass # of
 *      "skip" frames to select the next frame and so on, until
 *      the total number of selected frames reaches the default
 *      number of frames set in datatype.tbl or the end of timeline
 *      is reached, whichever comes first.
 *  Note: the choice of the first frame depends on the direction
 *      of timeline - forward for SNF/SFF/SND, & backward for others.
 */
	msel=0;
	skip = dsrc->skip;
	if ( skip < 0 ) skip = 0;
	
	if ( ntimes > 0 ) {
	    jj = ntimes / (skip + 1);
	    msel = ( ntimes - jj * (skip + 1) ) > 0 ? (jj+1) : jj;

	    if ( d4 > 0 && d4 < msel ) msel = d4;
			
	    for ( ii = 0; ii < ntimes; ii++ ) { 
	        strcpy ( dsrc->frm[ii].ftime, tmarry[ii] ); 
	    }
		
	    idx1 = skip + 1;
	    idx2 = ntimes - 1;
	    	    
	    if ( !dirflg ) {  /* backward */
	        if ( dsrc->catg == CAT_VGF ) {
	            dsrc->frm[0].selected = TRUE;
		}
		else {
		    for ( ii = 0; ii < msel; ii++ )    	    	
 	                dsrc->frm[idx2 - ii*idx1].selected = TRUE;
		}
	    }
	    else {            /* forward */
	        for ( ii = 0 ; ii < msel; ii++ )   	    	
	            dsrc->frm[ii*idx1].selected = TRUE;
            }
	
	    loop_setDataChngd (cur_lp, TRUE);
	}
	
/*
 *  4)  Load the select[] array for the tmln_setTimeInfo() call. 
 */
        for ( ii = 0; ii < MAX_FRAME; ii++ ) { 	
	    if ( ii < ntimes ) {
	        select[ii] = dsrc->frm[ii].selected;
  	    }
	    else {
		select[ii] = FALSE;
		tmarry[ii][0] = '\0';
	    }
	}

/*
 *  Now update the time line.
 */
	if ( ntimes > 0 ) {

	    dataw_setSkipFactor ( dsrc->skip, FALSE );
	    tmln_setTimeInfo ( dirflg, ntimes, tmarry, select );
	    tmln_redraw ( 0 );

	    dsrc->num_sel = msel;

	    dataw_setFrameScale ( msel, ntimes );
	}    
    }

/*
 *  If no dominant source or no times for source, draw empty time line
 */
    if ( dsrc == NULL || ntimes <= 0 ) {
	dataw_setTmlnSnstv ( FALSE );

	if ( _singleTm ) {
	    loop_setTmMode ( cur_lp, FALSE );
	    dataw_resetTmMode ( cur_lp );
	}
	dataw_setTmModeSnstv ( FALSE );
	tmln_clearTimeInfo ();
	tmln_drawClear ();
    }
}

/*=====================================================================*/

void dataw_getDHM ( int total_min, int *day, int *hr, int *min )
/************************************************************************
 * dataw_getDHM 							*
 *									*
 * This function passes in total minutes and convert it into day, hour, *
 * and minute value.							*
 *									*
 * void dataw_getDHM (total_min, day, hr, min)				*
 *									*
 * Input parameters:							*
 *	total_min	int		total minutes			*
 *									*
 * Output parameters:							*
 *      *day		int		day value			*
 *	*hr		int		hour value			*
 *	*min		int		minute value			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	11/03   initial coding				*
 ***********************************************************************/
{
    *min = total_min % 60;
    *hr  = (total_min / 60) % 24;
    *day = (total_min / 60) / 24;
}

/*=====================================================================*/

void dataw_getDefltIntvRng ( int *intv_min, int *range_min )
/************************************************************************
 * dataw_getDefltIntvRng 						*
 *									*
 * This function gets the default interval and range info from table	*
 * for a particular loop.						*
 *									*
 * void dataw_getDefltInvtRng (intv_min, range_min)			*
 *									*
 * Input parameters:							*
 *									*
 *	NONE								*
 * Output parameters:							*
 *      *intv_min	int		interval minutes		*
 *	*range_min	int		range minutes			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	11/03   initial coding				*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 01/06   Changed dtemp string length to MXTMPL   *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 * F. J. Yen/NCEP	04/08   Add dmnbf,dmnaf, dmstrct to ctb_dtget CS*
 ***********************************************************************/
{
    int		lp, ier, dtmch;
    int		dcatgry, dsubcat, dnframe, dhrsbfr, dhraftr, dionoff;
    int		dmnbf, dmnaf, dmstrct;
    char	*pstr, tmp[12], src_str[LLPATH];
    char	dalias[13], dpath[26], dtemp[MXTMPL];
/*---------------------------------------------------------------------*/
/*
 *  Retrieve default time interval/range in "datatype.tbl"
 */
    lp = loop_getCurLoop();
    
    if ( _domInfo[lp].src->catg == CAT_VGF) {
	 strcpy ( tmp, "VGF" );
    }
    else if ( _domInfo[lp].src->catg == CAT_ENS ) {
            dslw_getFrstMod ( _domInfo[lp].src->path, tmp );
    } 
    else {
         strcpy ( src_str, _domInfo[lp].src->path );
         pstr = strtok ( src_str, "/" );
         pstr = strtok ( NULL, "/" );
	 strcpy ( tmp, pstr );
    }
        
    cst_lcuc ( tmp, dalias, &ier );
    ctb_dtget ( dalias, dpath, dtemp, &(dcatgry), &(dsubcat),
		&(dnframe), range_min, intv_min, 
		&(dionoff), &(dhrsbfr), &(dmnbf), &(dhraftr),
                &(dmnaf), &(dmstrct), &(dtmch), &ier );
		 	  
    if ( *range_min <= 0 ) {   /* no negative value for range */
         if ( dcatgry == CAT_MSC || dcatgry == CAT_IMG || 
              dcatgry == CAT_SFC || dcatgry == CAT_SND )	{
	      *range_min = 360;
	 }
    }

    if ( *intv_min > *range_min ) {
         *intv_min = *range_min;   /* intv. always smaller than range */ 
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_intvTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dataw_intvTxtCb							*
 *									*
 * Callback function for the Interval text boxes in the Range/Interval	*
 * Input widget.  Invoked by the Range/Int button on the Data Selection *
 * Window.								*
 *									*
 * void dataw_intvTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	11/03   initial coding				*
 ***********************************************************************/
{
    float	val;
    int		range_min, day, hr, min, ii;
    char	*txt_str=NULL, intv_day[10], intv_hr[10], intv_min[10];
    Boolean	use_deflt[3], no_change;
/*---------------------------------------------------------------------*/

    day = 0;
    hr  = 0;
    min = 0;
    strcpy (intv_day,   "\0");
    strcpy (intv_hr,    "\0");
    strcpy (intv_min,   "\0");

    no_change = FALSE;
    for ( ii=0; ii<3; ii++ ) use_deflt[ii] = FALSE;

    txt_str = XmTextGetString (_intvDayTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {
       use_deflt[0] = TRUE;
       XmTextSetString (_intvDayTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       day = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    txt_str = XmTextGetString (_intvHrTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {

       use_deflt[1] = TRUE;
       XmTextSetString (_intvHrTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       hr  = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    txt_str = XmTextGetString (_intvMinTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {
       use_deflt[2] = TRUE;
       XmTextSetString (_intvMinTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       min = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    if ( use_deflt[0]==TRUE && use_deflt[1]==TRUE && use_deflt[2]==TRUE ) {

         dataw_getDefltIntvRng ( &_intvMin, &range_min);

         return;
    }
    else if ( !no_change ) {
	 _intvMin = min + hr*60 + day*1440; 
    }

    dataw_getDHM (_intvMin, &day, &hr, &min);

    if ( day > 0 ) sprintf (intv_day, "%d", day);
    if ( hr > 0  ) sprintf (intv_hr, "%d",   hr);
    if ( min > 0 ) sprintf (intv_min, "%d", min);
   
    XmTextSetString( _intvDayTxt, intv_day );
    XmTextSetString( _intvHrTxt,  intv_hr  );
    XmTextSetString( _intvMinTxt, intv_min );
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_rangeTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dataw_rangeTxtCb							*
 *									*
 * Callback function for the Range text boxes in the Range/Interval	*
 * Input widget.  Invoked by the Range/Int button on the Data Selection *
 * Window.								*
 *									*
 * void dataw_rangeTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	11/03   initial coding				*
 ***********************************************************************/
{
    float	val;
    int		intv_min, day, hr, min, ii;
    char	*txt_str=NULL, range_day[10], range_hr[10], range_min[10];
    Boolean	use_deflt[3], no_change;
/*---------------------------------------------------------------------*/

    day = 0;
    hr  = 0;
    min = 0;
    strcpy (range_day,   "\0");
    strcpy (range_hr,    "\0");
    strcpy (range_min,   "\0");

    no_change = FALSE;
    for ( ii=0; ii<3; ii++ ) use_deflt[ii] = FALSE;

    txt_str = XmTextGetString (_rangeDayTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {

       use_deflt[0] = TRUE;
       XmTextSetString (_rangeDayTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       day = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    txt_str = XmTextGetString (_rangeHrTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {

       use_deflt[1] = TRUE;
       XmTextSetString (_rangeHrTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       hr  = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    txt_str = XmTextGetString (_rangeMinTxt);

    if ( txt_str == NULL || txt_str[0] == '\0' ) {
       use_deflt[2] = TRUE;
       XmTextSetString (_rangeMinTxt, "\0");
    }
    else if ( sscanf (txt_str, "%f", &val) == 1 && val > 0 ) {
       min = (int)val;
    }
    else {
       no_change = TRUE;
    }

    XtFree (txt_str);

    if ( use_deflt[0]==TRUE && use_deflt[1]==TRUE && use_deflt[2]==TRUE ) {
         dataw_getDefltIntvRng ( &intv_min, &_rangeMin);
         return;
    }
    else if ( !no_change ) {
	 _rangeMin = min + hr*60 + day*1440; 
    }

    dataw_getDHM (_rangeMin, &day, &hr, &min);

    if ( day > 0 ) sprintf (range_day, "%d", day);
    if ( hr > 0  ) sprintf (range_hr, "%d",   hr);
    if ( min > 0 ) sprintf (range_min, "%d", min);
   
    XmTextSetString( _rangeDayTxt, range_day );
    XmTextSetString( _rangeHrTxt,  range_hr  );
    XmTextSetString( _rangeMinTxt, range_min );
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_refTimeTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dataw_refTimeTxtCb							*
 *									*
 * Callback function for the reference, Ref., Time text box in the	*
 * Range/Interval Input widget.  Invoked by the Range/Int button on the *
 * Data Selection Window.						*
 *									*
 * void dataw_refTimeTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	11/03   initial coding				*
 * H. Zeng/XTRIA	11/03	modified ref_time before updating GUI	*
 * H. Zeng/XTRIA	12/03   stopped auto-update			*
 * B. Yin/SAIC		03/04	changed css_gtim calling sequences	*
 ***********************************************************************/
{
    int		time_arry[5], ier, ier2, itype = 1;
    char	*time_str=NULL, utc_time[DTTMSZ], ref_time[DTTMSZ];
/*---------------------------------------------------------------------*/

    time_str = XmTextGetString (_refTimeTxt);

/*
 *  If ref. time text box is blank, use current time.
 */
    if ( time_str == NULL || time_str[0] == '\0' ) {
       _timeSetFlag = 2;
       XmTextSetString (_refTimeTxt, "\0");

       XtFree (time_str);
       return;
    }

    css_gtim (&itype, utc_time, &ier);
    ti_stan(time_str, utc_time, ref_time, &ier, strlen(time_str), 
            strlen(utc_time), (DTTMSZ-1) );

    if ( ier != 0 ) {
       _timeSetFlag = 0;
       XmTextSetString (_refTimeTxt, _currTimeStr);

       XtFree (time_str);
       return;
    }
   
    ti_ctoi (ref_time, time_arry, &ier2, strlen(ref_time) );

    if ( ier2 != 0 ) {
       _timeSetFlag = 0;
       XmTextSetString (_refTimeTxt, _currTimeStr);

       XtFree (time_str);
       return;
    }

    _refTimeStrc.year = time_arry[0];
    _refTimeStrc.mon  = time_arry[1] - 1;
    _refTimeStrc.day  = time_arry[2];
    _refTimeStrc.hour = time_arry[3];
    _refTimeStrc.min  = time_arry[4];

    _timeSetFlag = 1;
    sprintf ( ref_time, "%02d%02d%02d/%02d%02d",    
		        _refTimeStrc.year%100,
		        _refTimeStrc.mon+1,
			_refTimeStrc.day,
			_refTimeStrc.hour,
			_refTimeStrc.min         );
    XmTextSetString (_refTimeTxt, ref_time);
 
/*
 *  Set auto-update to off when a reference time is selected.
 */ 
    dataw_setAutoMenu( AUTO_UPDT_OFF );
    XtFree (time_str);
}

/*=====================================================================*/
/* ARGSUSED */
void dataw_cmdLineLoadSPF ( char *spFile, Boolean load )
/************************************************************************
 * dataw_cmdLineLoadSPF                                                 *
 *                                                                      *
 * This function starts the data window, loads the spf file and if the  *
 * the load pararmeter is true, loads the data without any user 	*
 * interaction.								*
 *                                                                      *
 * void dataw_cmdLineLoadSPF ( spFile, load )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *   *spFile		char		sp file name                    *
 *   load               Boolean         load flag                       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		01/04						*
 ***********************************************************************/
{
    int         ier, ierr;
/*---------------------------------------------------------------------*/
    dataw_popup ();
    NxmCursor_setCursor( _dataW, CURS_BUSY );

/*
 *  Load the spf file to the buffer.
 */
    spf_load ( spFile, &ier );

    if ( ier != 0 ) {
	ierr = -2;
	er_wmsg ( "SPF", &ierr, spFile, &ier, 3, strlen(spFile) );
	NxmErr_update( );
    }
    else {

/*
 *  Load the settings from the sp file to the data window.
 */
    	dataw_loadsp ();
    
/*
 *  Load data.
 */
    	if ( load ) dataw_loadData ();    
    }
    
    NxmCursor_setCursor( _dataW, CURS_DEFAULT );
}   

/*=====================================================================*/

void dataw_getBaseTm ( int lp, dttms_t btime )
/************************************************************************
 * dataw_getBaseTm							*
 *									*
 * This function gets base time of the time line for each loop.		*
 *									*
 * void dataw_getBaseTm ( lp, btime )					*
 *									*
 * Input parameters:                                                    *
 *	lp		int	loop #					*
 *									*
 * Output parameters:                                                   *
 *	btime           char    base time                               *
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC          01/04   Initial coding                          *
 ***********************************************************************/
{
    if ( lp > MAX_LOOP || lp < 0 )  {
        btime = '\0';
    }
    else {
        strcpy ( btime, _domInfoCopy[lp].base_tm );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void dataw_binSrcCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_binSrcCb							*
 *									*
 * This function is the callback for the "Bin Source" button.        	*
 *									*
 * static void dataw_binSrcCb( wid, which, call) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/04	initial coding				*
 * J. Wu/SAIC		12/04	use the cur. src. instead of dom. src.	*
 * A. Hardy/NCEP	02/05   chg bfrhr check for pos. no.;rm Cbs	*
 * F. J. Yen/NCEP	04/08	Added minutes and most recent flag	*
 ***********************************************************************/
{
    int         lp, src, bfrhr, aftrhr;
    int         bfrmn, aftrmn;
    char	bfrhrstr[10], aftrhrstr[10];
    char	bfrmnstr[10], aftrmnstr[10];
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve the currently-selected data source on the source menu.
 */
    dataw_getSelIdx ( SRC_BTN, &lp, &src );
    active_src = &_dsrc[lp][src];

    if ( active_src == NULL ) return;
  
/*
 *  Use the current active source's binning hours & minutes and most recent
 *  flag as the default.  If the hours and minutes are not valid, the fields
 *  are blanked.  
 */ 
    strcpy ( bfrhrstr,  "\0");
    strcpy ( aftrhrstr,  "\0");
    strcpy ( bfrmnstr,  "\0");
    strcpy ( aftrmnstr,  "\0");

    bfrhr  = active_src->bfhr;
    aftrhr = active_src->afhr;
    bfrmn  = active_src->bfmn;
    aftrmn = active_src->afmn;
    if ( aftrmn >= 0 && aftrmn <= BINMINLIMIT  ) {
	sprintf ( aftrmnstr, "%d", aftrmn );
    }
    if ( bfrmn >= 0 && bfrmn <= BINMINLIMIT  ) {
	sprintf ( bfrmnstr, "%d", bfrmn );
    }

/*
 *  Set the most recent flag button. 
 */
	XtVaSetValues ( _mstRctToggBtn, XmNset, active_src->mstrctf, NULL );

    if ( bfrhr >= 0 && bfrhr <= BINHOURLIMIT  ) {
	sprintf ( bfrhrstr, "%d", bfrhr );
    }
   
    if ( aftrhr >= 0 && aftrhr <= BINHOURLIMIT  ) {
	sprintf ( aftrhrstr, "%d", aftrhr );
    }

/*
 *  Set the hours and mins field. 
 */
    XmTextSetString ( _binhrBfTxt, bfrhrstr );
    XmTextSetString ( _binhrAfTxt, aftrhrstr );
    XmTextSetString ( _binmnBfTxt, bfrmnstr );
    XmTextSetString ( _binmnAfTxt, aftrmnstr );
    
/*
 *  Pops up window.
 */
    XtManageChild ( _srcBinW );
}

/*=====================================================================*/

static void dataw_createSrcBin ( Widget parent )
/************************************************************************
 * dataw_createSrcBin                                              	*
 *                                                                      *
 * This function creates the source bin window.           		*
 *                                                                      *
 * static void dataw_createSrcBin ( parent )                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                          	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/04	initial	coding				*
 * J. Wu/SAIC		12/04	remove on/off option			*
 * A. Hardy/NCEP	02/05   Change callback for hours before text   *
 * F. J. Yen/NCEP	04/08	Added bin minutes & most recent only flg*
 ***********************************************************************/
{
    char	*btnstr[] = {"Apply", "Cancel", "Default", "Help"};
    Widget	pane, form, label, ctl_rc, cntlBtn[4];
    Widget	label2, labela, labelb;
    XmString	title_string;
    long	ii;
/*---------------------------------------------------------------------*/
/*
 *  Create a new dialog form.
 */
    _srcBinW = XmCreateFormDialog ( parent, "dataw_srcbinw", NULL, 0 );

    title_string = XmStringCreateLocalized ( "Source Bin" );

    XtVaSetValues ( _srcBinW, 
		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		XmNnoResize,        	True, 
		XmNdefaultPosition,	True,
		XmNdialogTitle,		title_string,
		XmNautoUnmanage,	False, 	
		NULL );
    XmStringFree ( title_string );
    
/*
 *  Create a pane widge.
 */
    pane = XtVaCreateWidget ("dataw_srcBinPane",
		xmPanedWindowWidgetClass,	_srcBinW,
		XmNsashWidth,  			1,
		XmNsashHeight, 			1,
		NULL);

/*
 *  Create "Hours and Minutes Before" and "Hours and Minutes After" text fields.
 */
    form = XtVaCreateManagedWidget ( "srcbin_form",
		xmFormWidgetClass,		pane,
		NULL );
   

    _binhrBfTxt = XtVaCreateManagedWidget ( "hr_bf_text",
		xmTextWidgetClass,		form,
		XmNcolumns,			3,
		XmNmaxLength,			3,
		XmNeditable,			TRUE,
		XmNvalue,			"",
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			13,
		NULL );
    
    XtAddCallback ( _binhrBfTxt, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );


    _binmnBfTxt = XtVaCreateManagedWidget ( "mn_bf_text",
		xmTextWidgetClass,		form,
		XmNcolumns,			2,
		XmNmaxLength,			2,
		XmNeditable,			TRUE,
		XmNvalue,			"",
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_binhrBfTxt,
		XmNleftOffset,			5,
		NULL );
    
    XtAddCallback ( _binmnBfTxt, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );

    _binhrAfTxt = XtVaCreateManagedWidget ( "hr_af_text",
		xmTextWidgetClass,		form,
		XmNcolumns,			2,
		XmNmaxLength,			2,
		XmNeditable,			TRUE,
		XmNvalue,			"",
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_binmnBfTxt,
		XmNleftOffset,			16,
		NULL );
    
    XtAddCallback ( _binhrAfTxt, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );

    _binmnAfTxt = XtVaCreateManagedWidget ( "mn_af_text",
		xmTextWidgetClass,		form,
		XmNcolumns,			2,
		XmNmaxLength,			2,
		XmNeditable,			TRUE,
		XmNvalue,			"",
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_binhrAfTxt,
		XmNleftOffset,			5,
		NULL );
    
    XtAddCallback ( _binmnAfTxt, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL );
/*
 *  Create "Only Most Recent" check box
 */
    _mstRctToggBtn = XtVaCreateManagedWidget (" ",
                xmToggleButtonWidgetClass, 	form,
  		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			11,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_binmnAfTxt,	
		XmNleftOffset,			38,
                XmNtraversalOn,            	False,
		XmNset,                    	False,
		XmNrightAttachment,		XmATTACH_FORM,
		XmNrightOffset,			5,
                NULL );
		
    XtAddCallback ( _mstRctToggBtn, XmNvalueChangedCallback,
		    (XtCallbackProc)dataw_mstRctToggCb, (XtPointer)ii);
		
    label  = XtVaCreateManagedWidget ( "Hrs",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_binhrBfTxt,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			20,
		NULL ); 
    label2  = XtVaCreateManagedWidget ( "Mins",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_binmnBfTxt,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			label,
		XmNleftOffset,			16,
		NULL ); 
    labelb  = XtVaCreateManagedWidget ( "Before",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			label,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			28,
		NULL ); 
    label  = XtVaCreateManagedWidget ( "Hrs",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_binhrAfTxt,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			label2,
		XmNleftOffset,			14,
		NULL ); 
    label2  = XtVaCreateManagedWidget ( "Mins",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_binmnAfTxt,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			label,
		XmNleftOffset,			13,
		NULL ); 
    labela  = XtVaCreateManagedWidget ( "After",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			label,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			labelb,
		XmNleftOffset,			48,
		NULL ); 
    label  = XtVaCreateManagedWidget ( "Only Most\nRecent",
		xmLabelWidgetClass,		form,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_mstRctToggBtn,
		XmNtopOffset,			12,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			label2,
		XmNleftOffset,			14,
		NULL ); 

/*
 *  Create control buttons.
 */        
    ctl_rc = NxmCtlBtn_create ( pane, 0, "ctlBtns", XtNumber(btnstr),
    		btnstr, (XtCallbackProc)dataw_binCtlBtnCb, cntlBtn );
    
    XtVaSetValues ( ctl_rc, 
                XmNmarginHeight,	5,
                XmNmarginWidth,		20,
                XmNspacing,		16,
		NULL );
    
/*
 *  Manage the pane
 */            
    XtManageChild ( pane );

}

/*=====================================================================*/
/* ARGSUSED */
static void dataw_binCtlBtnCb ( Widget wid, long which, XtPointer call ) 
/************************************************************************
 * dataw_binCtlBtnCb							*
 *									*
 * Callback for the control buttons in the "Bin Source" input window.	*
 *									*
 * static void dataw_binCtlBtnCb ( wid, which, call ) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/04	initial coding				*
 * J. Wu/SAIC		12/04	apply to act. src instead of dom. src.	*
 * T. Piper/SAIC	01/05	Added loop_setDataChngd to case 0 and 2	*
 * A. Hardy/NCEp	02/05   change *BINHOUR?? to BINHOURLIMIT	*
 * F. J. Yen/NCEP	04/08   Added before and after bin minutes	*
 ***********************************************************************/
{
    int         lp, src, bfrhr, aftrhr, ionoff;
    int         bfrmn, aftrmn, imstrctf;
    char	*ss = NULL;
    Boolean	mostrct;
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve the currently-selected data source on the source menu.
 */
    dataw_getSelIdx ( SRC_BTN, &lp, &src );
    active_src = &_dsrc[lp][src];

    switch ( which ) {
        
	case 0:	 
/*
 *  "Apply" - Retrieve user input. If they are valid, 
 *            apply to active data source's attributes.  
 */            
	    XtVaGetValues ( _binhrBfTxt, XmNvalue, &ss, NULL );
            if ( ss && strlen(ss) > (size_t)0 ) {
		bfrhr = atoi ( ss );        
	        if ( bfrhr >= 0 && bfrhr <= BINHOURLIMIT ) {
                    active_src->bfhr = bfrhr;
                }
	    }

	    XtVaGetValues ( _binmnBfTxt, XmNvalue, &ss, NULL );
            if ( ss && strlen(ss) > (size_t)0 ) {
		bfrmn = atoi ( ss );        
	        if ( bfrmn >= 0 && bfrmn <= BINMINLIMIT ) {
                    active_src->bfmn = bfrmn;
                }
	    }
		
            XtVaGetValues ( _binhrAfTxt, XmNvalue, &ss, NULL );
            if ( ss && strlen(ss) > (size_t)0 ) {
                aftrhr = atoi ( ss );    
	        if ( aftrhr >= 0 && aftrhr <= BINHOURLIMIT ) {
                    active_src->afhr = aftrhr;
	        }        
	    }

            XtVaGetValues ( _binmnAfTxt, XmNvalue, &ss, NULL );
            if ( ss && strlen(ss) > (size_t)0 ) {
                aftrmn = atoi ( ss );    
	        if ( aftrmn >= 0 && aftrmn <= BINMINLIMIT ) {
                    active_src->afmn = aftrmn;
	        }        
	    }

	    XtVaGetValues ( _mstRctToggBtn, XmNset, &mostrct, NULL );
	    active_src->mstrctf = (int)mostrct;
		
	    if ( ss )  XtFree ( ss );
	    loop_setDataChngd ( lp, TRUE);
	    
	    break;
        	
	case 1:		
/*
 *  Cancel
 */
	    break;
	
	case 2:	
/*
 *  "Default" - Retrieve from "datatype.tbl". If they are valid, 
 *              apply to current data source's attributes.  
 */            
            dataw_getDefltBinInfo ( &bfrhr, &bfrmn, &aftrhr, &aftrmn,
				    &ionoff, &imstrctf );
            
	    if ( bfrhr >= 0 && bfrhr <= BINHOURLIMIT ) {
                active_src->bfhr = bfrhr;
	    }
	    if ( bfrmn >= 0 && bfrmn <= BINMINLIMIT ) {
                active_src->bfmn = bfrmn;
	    }
	    if (  aftrhr >= 0 && aftrhr <= BINHOURLIMIT ) {
                 active_src->afhr = aftrhr;
	    }
	    if (  aftrmn >= 0 && aftrmn <= BINMINLIMIT ) {
                 active_src->afmn = aftrmn;
	    }
            active_src->mstrctf = imstrctf;

	    loop_setDataChngd ( lp, TRUE);
	           
	    break;	
        
	case 3:	
/*
 *  "Help" - pop up the help window for time binning.
 */
	    NxmHelp_helpBtnCb ( wid, 41, NULL );
	    break;	

    }
    
/*
 *  Pop down the window after "Apply", "Cancel", or "Default" action. 
 */            
    if ( which == 0 || which == 1 || which == 2 ) {
        XtUnmanageChild ( _srcBinW );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void dataw_binbxToggCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dataw_binbxToggCb							*
 *									*
 * Callback for the time binning ON/OFF toggle button.			*
 *									*
 * static void dataw_binbxToggCb ( wid, clnt, cbs ) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *      clnt		XtPointer       Not used			* 
 *      cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/04	initial coding				*
 ***********************************************************************/
{
    int         loop, src;
    Boolean	binnable;
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Check if the button status and set the "Bin Source" button. 
 */
    XtVaGetValues ( wid, XmNset, &binnable, NULL );
    XtSetSensitive ( _binSrcBtn, binnable );
    
/*
 *  Set the current data source's time binning flag.
 */
    dataw_getSelIdx ( SRC_BTN, &loop, &src );
    active_src = &_dsrc[loop][src];

    active_src->ionoff = (int)binnable;
}

/*=====================================================================*/
/* ARGSUSED */
static void dataw_mstRctToggCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dataw_mstRctToggCb							*
 *									*
 * Callback for the most recent only ON/OFF toggle button.		*
 *									*
 * static void dataw_mstRctToggCb ( wid, clnt, cbs ) 			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *      clnt		XtPointer       Not used			* 
 *      cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	04/08	Created					*
 ***********************************************************************/
{
    int         loop, src;
    Boolean	mostrct;
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Check if the button status and set the "Only `Most Recent" button. 
 */
    XtVaGetValues ( wid, XmNset, &mostrct, NULL );
/*
 *  Set the current data source's most recent flag.
 */
    dataw_getSelIdx ( SRC_BTN, &loop, &src );
    active_src = &_dsrc[loop][src];
    active_src->mstrctf = (int)mostrct;
}

/*=====================================================================*/

/*=====================================================================*/

static void dataw_getDefltBinInfo ( int *hrbfr, int *mnbfr, int *hraftr,
		int *mnaftr, int *ionoff, int*mstrct ) 
/************************************************************************
 * dataw_getDefltBinInfo						*
 *									*
 * This function gets default time binning info from datatype.tbl.	*
 *									*
 * static void dataw_getDefltBinInfo ( hrbfr, mnbfr, hraftr, mnaftr,	*
 *				       ionoff, mstrct ) 		*
 *									*
 * Input parameters:							*
 *      *hrbfr		int		Hours before			*
 *      *mnbfr		int		Minutes before			*
 *      *hraftr		int		Hours after			*
 *      *mnaftr		int		Minutes after			*
 *      *ionoff		int		Source binning on/off flag	*
 *      *mstrct		int		Only Most Recent on/off flag	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		09/04	initial coding				*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * J. Wu/SAIC		12/04	add time binning on/off flag		*
 * m.gamazaychikov/SAIC 01/06   Changed dtemp string length to MXTMPL   *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS        	*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 * F. J. Yen/NCEP	04/08	Added binning min & most recent flag-CSC*
 ***********************************************************************/
{
    int		lp, ier, drange, dintv, src;
    int		dcatgry, dsubcat, dnframe, dtmch;
    char	*pstr, tmp[12], src_str[LLPATH];
    char	dalias[13], dpath[26], dtemp[MXTMPL];
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve the currently-selected data source on the source menu.
 */
    dataw_getSelIdx ( SRC_BTN, &lp, &src );    
    active_src = &_dsrc[lp][src];
    
/*
 *  Retrieve default time bin before/after hours in "datatype.tbl"
 *  for the current data source.
 */    
    if ( active_src->catg == CAT_VGF) {
	 strcpy ( tmp, "VGF" );
    }
    else if ( active_src->catg == CAT_ENS ) {
            dslw_getFrstMod ( active_src->path, tmp );
    }
    else {
         strcpy ( src_str, active_src->path );
         pstr = strtok ( src_str, "/" );
         pstr = strtok ( NULL, "/" );
	 strcpy ( tmp, pstr );
    }
        
    cst_lcuc ( tmp, dalias, &ier );

    ctb_dtget ( dalias, dpath, dtemp, &(dcatgry), &(dsubcat),
		&(dnframe), &drange, &dintv, 
		ionoff, hrbfr, mnbfr, hraftr, mnaftr, mstrct,
		&dtmch, &ier );
}

/*=====================================================================*/

static void dataw_setBinSrc ( void )
/************************************************************************
 * dataw_setBinSrc							*
 *									*
 * This function sets the time binning buttons' sensitivity. 		*
 *									*
 * static void dataw_setBinSrc ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/04	initial coding				*
 ***********************************************************************/
{
    int         loop, src;
    Boolean	is_binnable = False;
    dsrc_t	*active_src;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve the currently-selected data source on the source menu.
 */
    dataw_getSelIdx ( SRC_BTN, &loop, &src );

    if ( src >= 0 ) {
        active_src = &_dsrc[loop][src];

/*
 *  Check if the current data source can be binned.
 */
        is_binnable = (Boolean) ( active_src->src_on &&
	                        ( active_src->catg == CAT_SFC || 
		                  active_src->catg == CAT_SFF || 
		                  active_src->catg == CAT_SND || 
			          active_src->catg == CAT_SNF ) );
	}

/*
 *  Set status of the "Bin Source" toggle button and push button.
 */
    if ( is_binnable ) {        
	XtSetSensitive ( _binSrcToggBtn, True );            	    
	XtVaSetValues ( _binSrcToggBtn, XmNset, active_src->ionoff, NULL );
	XtSetSensitive ( _binSrcBtn, active_src->ionoff );
	}
    else {    
        XtVaSetValues ( _binSrcToggBtn, XmNset, False, NULL );
        XtSetSensitive ( _binSrcToggBtn, False );
        XtSetSensitive ( _binSrcBtn, False );
    }
}

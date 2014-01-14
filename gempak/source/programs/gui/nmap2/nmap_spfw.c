#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "Nxm.h"
#include "nmsdef.h"
#include "ctbcmn.h"

#define VISIBLE_ITEM	  10 
#define MAX_FILES	1000

#define SORT_BY_NAME	   0
#define SORT_BY_DATE	   1

#define SAVE_AS_CURRENT    1
#define SAVE_AS_LITERAL	   0

#define RESTORE_POPUP	   0
#define SAVE_POPUP	   1

#define SPF_FILE_EXT	".spf"
#define FILE_PATTERN	"*.spf"
#define LOCAL_DIR	"./"
#define CWD_TITLE	"Local"

typedef XmFileSelectionBoxCallbackStruct FSBCBS;

static Widget	_fileSelW;
static Widget	_dirSel_listW;
static Widget	_fileSel_listW;
static Widget	_fileSel_txtW;

static Widget	_browsePopup;
static Widget	_sortRadioW;
static Widget	_stimeRadioW;
static WidgetList _sortBtn;
static WidgetList _stimeBtn;
static Widget	_sReftimeBtn;


static Widget	_sortPaneW;
static Widget	_selectPaneW;
static Widget	_inputPaneW;
static Widget	_browseBtnPane;
static Widget	_bottomPaneW;
static Widget	_restoreConfW;
static Widget	_rstContentW;
static Widget	_rstConfLblW;
static Widget	_stimePaneW;
static Widget	_refTimePaneW;

static char	_fileName[MXFLSZ]	= "\0";
static char	_dirPath[LLPATH]        = LOCAL_DIR;
static int	_dirPos;

static int	_spfilFunc;	  /* RESTORE_SPF,  SAVE_SPF */
static int	_sortBy;	  /* SORT_BY_NAME or SORT_BY_DATE  */
static int      _stimeBy;	  /* SAVE_AS_CURRENT or SAVE_AS_LITERAL */
static Boolean	_sReftime=FALSE;  /* Whether save reference time or not */

/*
 *  SPF user info data struct
 */
typedef struct {
        char	*title;		/* title name */
        char	*usrpath;	/* full path to this user's SPF dir */
} spfusr_ent_t;

/*
 *  SPF user info table struct
 */
typedef struct {
    int			nitems;	/* total # of users*/
    spfusr_ent_t	*items;	/* pointer to the array of user items */
} spfutbl_t;

static spfutbl_t	_spfUsrTbl;

/*
 *  private callback functions
 */
void spfw_browseBtnCb ( Widget, XtPointer, XtPointer );
void spfw_browseDoneCb ( Widget, XtPointer, XtPointer );
void spfw_btmCtlBtnCb ( Widget, long which, XtPointer );
void spfw_confirmCb (Widget, XtPointer, XtPointer );
void spfw_rstConfCb( Widget, XtPointer, XtPointer );
void spfw_selectCb ( Widget, XtPointer, XtPointer );
void spfw_selectDirCb ( Widget, XtPointer, XtPointer );
void spfw_sortByCb ( Widget, XtPointer, XtPointer );
void spfw_sReftimeCb ( Widget, XtPointer, XmToggleButtonCallbackStruct *cbs );
void spfw_stimeCb ( Widget, XtPointer, XtPointer );
void spfw_txtCb ( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
static void spfw_checkPerms ( char *cfile, Boolean *can_read, 
                                           Boolean *can_write );
static void spfw_managePanes ( int popup_type);
static void spfw_preCheck ( void );
static void spfw_restoreSPF ( void );
static void spfw_saveSPF ( void );  
static void spfw_setFileList ( void );
static void spfw_unmanagePanes ( void );
static void spfw_readUsrTbl ( char	*tblname,
			      spfutbl_t	*spfutbl,
			      int	*iret );
void spfw_createConf( Widget parent );
void spfw_popupConf( void );
void spfw_popdownConf( void );
void spfw_formatConf ( long flen, char *spftext, char *filstr, int *iret );
static void spfw_saveSrcAttr ( dsrc_t *datasrc, FILE *fptr, int lp, int srcnum );

/************************************************************************
 * nmap_spfw.c								*
 *									*
 * This module defines the SPF file selection (RESTORE/SAVE) in Data 	*
 * Selection.								*
 *									*
 * CONTENTS:								*
 *  spfw_create()		creates file selection pallette		*
 *  spfw_popup()		popup the palette window		*
 *  spfw_popdown()		put down the palette window		*
 *  spfw_getFileName()		gets the full name of selected SPF file	*
 *  spfw_getSource() 		gets a specified source in a given loop	*
 *									*
 *  spfw_browseBtnCb()		callback for the browse button		*
 *  spfw_browseDoneCb()		callback for browse OK/Cancel buttons	*
 *  spfw_btmCtlBtnCb()		callback for bottom control buttons	*
 *  spfw_confirmCb()		callback for RESTORE/SAVE confirmation	*
 *  spfw_selectCb()		callback for file list			*
 *  spfw_selectDirCb()		callback for directory list		*
 *  spfw_sortByCb() 		callback for sort by toggles		*
 *  spfw_txtCb()		callback for text input			*
 *  spfw_sReftimeCb()		callback for save ref. time option	*
 *									*
 *  spfw_checkPerms()		checks file permissions			*
 *  spfw_managePanes()		manages the appropiate children of pane	*
 *  spfw_preCheck()		checks the file before restoring/saving	*
 *  spfw_restoreSPF()		restores the specified SPF file		*
 *  spfw_setFileList()		sets the file list for the _dirPath	*
 *  spfw_readUsrTbl()		reads SPF file user table "spf.tbl"	*
 *  spfw_saveSPF()		saves settings into an SPF file		*
 *  spfw_unmanagePanes()	unmanages all the children of pane	*
 *									*
 *  spfw_createConf()		creates restore confirmation pallette	*
 *  spfw_popupConf()		pops up restore confirmation window	*
 *  spfw_popdownConf()		pops down restore confirmation window 	*
 *  spfw_rstConfCb()		callback for restore confirmation	*
 *  spfw_formatConf()		formats source string for restore conf. *
 *  spfw_saveSrcAttr()     	saves a data source's attr. settings	*
 ************************************************************************/

/*=====================================================================*/

void spfw_create ( Widget parent )
/************************************************************************
 * spfw_create								*
 *									*
 * This function creates a file selection dialog box.			*
 *									*
 * void spfw_create ( parent )						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	revise from pgfilw_create() 		*
 * E. Safford/SAIC	11/01	fix interaction problems w/ dataw	*
 * T. Piper/SAIC	10/05	declared ii long			*
 * C. Bailey/HPC	01/06	Added Save Literal Time Pane		*
 * H. Zeng/SAIC		03/07	Added "Save Reference Time" button	*
 ***********************************************************************/
{
    int			nn, toff = 5, ier;
    long		ii;
    char		*btm_btnstr[] = {"OK", "Cancel"}; 
    char		*sort_str[] = {"Name", "Date"};
    char		*stime_str[] = {"Constant", "Latest"};
    Widget		pane, label, label2, frame, sort_label, stime_label;
    Arg         	args[10];
    Cardinal    	argcnt;
    XmString		xmpattern; 
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    _fileSelW = XmCreateFormDialog( parent, "Save as", NULL, 0 );
    XtVaSetValues(_fileSelW,
		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("spfw_pane",
                    xmPanedWindowWidgetClass, _fileSelW,
                    XmNsashWidth,             1,
                    XmNsashHeight,            1,
                    NULL);

    
    /*
     * ******** SORT PANE ********
     * create a form widget to hold the sort by toggles
     */
    _sortPaneW = XtVaCreateWidget("spfw_form",
                    xmFormWidgetClass, 	      pane,
		    XmNnoResize,     	      TRUE,
		    XmNautoUnmanage, 	      FALSE,
		    NULL );

    sort_label = XmCreateLabel( _sortPaneW, 
    		    "Display Files In Order By:", NULL, 0 );
    XtVaSetValues( sort_label,
		    XmNtopAttachment,         XmATTACH_POSITION,
		    XmNtopPosition,           2,
		    XmNleftAttachment,        XmATTACH_POSITION,
		    XmNleftPosition,          1,
		    XmNrightAttachment,       XmATTACH_POSITION,
		    XmNrightPosition,         60,
		    NULL );
    XtManageChild( sort_label );

    _sortRadioW = XtVaCreateManagedWidget ("sort_radio",
		    xmRowColumnWidgetClass,   _sortPaneW,
		    XmNorientation,           XmVERTICAL,
		    XmNradioBehavior,         True,
		    XmNtopAttachment,         XmATTACH_WIDGET,
		    XmNtopWidget,             sort_label,
		    XmNrightAttachment,       XmATTACH_FORM,
		    XmNleftAttachment,        XmATTACH_FORM,
                    NULL);

    nn = XtNumber (sort_str);
    _sortBy = SORT_BY_NAME;
    _sortBtn = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));

    for (ii = 0; ii < nn; ii++) {
        _sortBtn[ii] = XtVaCreateManagedWidget (sort_str[ii],
		    xmToggleButtonWidgetClass,      _sortRadioW,
                    XmNtraversalOn,                 FALSE,
		    NULL);
	XtAddCallback(_sortBtn[ii], XmNdisarmCallback,
	            spfw_sortByCb, (XtPointer) ii);

	XmToggleButtonSetState (_sortBtn[ii], (_sortBy == ii), FALSE);
    }

    XtManageChild(_sortPaneW); 


    /*
     * ******** SELECT PANE ********
     * create a form widget to hold the directory and file lists
     */
    _selectPaneW = XtVaCreateWidget("spfw_selectform",
                    xmFormWidgetClass, 	pane,
                    NULL);
    XtVaSetValues( _selectPaneW,
		    XmNnoResize,     TRUE,
		    XmNautoUnmanage, FALSE,
		    NULL );

    
    /*
     * create the directory list
     */
    label = XmCreateLabel( _selectPaneW, "Select directory:", NULL, 0 );
    XtManageChild( label );
    XtVaSetValues( label,
		    XmNtopAttachment,   XmATTACH_POSITION,
		    XmNtopPosition,     2,
		    XmNleftAttachment,  XmATTACH_POSITION,
		    XmNleftPosition,    1,
		    XmNrightAttachment, XmATTACH_POSITION,
		    XmNrightPosition,   60,
		    NULL );

    frame = XmCreateFrame( _selectPaneW, "_DirSel_frameW", NULL, 0 );
    XtVaSetValues( frame,
		    XmNtopAttachment,    XmATTACH_WIDGET,
		    XmNtopWidget,        label,
		    XmNrightAttachment,  XmATTACH_FORM,
		    XmNleftAttachment,   XmATTACH_FORM,
		    NULL );

    _dirSel_listW = XmCreateScrolledList( frame,
					  "_DirSel_lstW", NULL, 0 );
    XtVaSetValues( _dirSel_listW,
                    XmNvisibleItemCount, VISIBLE_ITEM,
		    NULL );
    XtAddCallback( _dirSel_listW, XmNbrowseSelectionCallback,
		   spfw_selectDirCb, NULL );


    spfw_readUsrTbl("spf.nmap", &_spfUsrTbl, &ier);

    if ( _spfUsrTbl.nitems > 0) {

        xmfils = (XmStringTable) XtMalloc 
	    ((size_t)_spfUsrTbl.nitems * sizeof (XmString *));

        for (ii = 0; ii < _spfUsrTbl.nitems; ii++) {
	    xmfils[ii] = XmStringCreateLocalized (_spfUsrTbl.items[ii].title);
        }

        XtVaSetValues (_dirSel_listW,
		       XmNitems,	xmfils,
		       XmNitemCount,	_spfUsrTbl.nitems,
		       NULL);

        for (ii = 0; ii < _spfUsrTbl.nitems; ii++) {
	    XmStringFree (xmfils[ii]);
        }
	XtFree ((XtPointer)xmfils);

	/*
	 * hi-light the local directory
	 */
	_dirPos = _spfUsrTbl.nitems;
    }

    XtManageChild( _dirSel_listW );
    XtManageChild( frame );

    
    /*
     * create the file list
     */
    label = XmCreateLabel( _selectPaneW, "Select SPF File name:", NULL, 0 );
    XtManageChild (label );
    XtVaSetValues (label,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	frame,
		   XmNtopOffset,	toff,
		   XmNleftAttachment,	XmATTACH_POSITION,
		   XmNleftPosition,	1,
		   XmNrightAttachment,	XmATTACH_POSITION,
		   XmNrightPosition,	60,
		   NULL );

    frame = XmCreateFrame( _selectPaneW, "_FileSel_frameW", NULL, 0 );
    XtVaSetValues (frame,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	label,
		   XmNrightAttachment,	XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    _fileSel_listW = XmCreateScrolledList (frame, "_FileSel_lstW", NULL, 0);

    XtVaSetValues (_fileSel_listW,
		   XmNvisibleItemCount, VISIBLE_ITEM,
		   NULL);

    XtAddCallback (_fileSel_listW, XmNbrowseSelectionCallback,
		   spfw_selectCb, NULL);


    XtManageChild (_fileSel_listW);
    XtManageChild (frame);

    XtManageChild(_selectPaneW);

    
    /*
     * ******** INPUT PANE ********
     * create the file input
     */
    _inputPaneW = XtVaCreateWidget("spfw_inputForm",
				   xmFormWidgetClass, 	pane,
				   NULL);
    XtVaSetValues( _inputPaneW,
		    XmNnoResize,     TRUE,
		    XmNautoUnmanage, FALSE,
		    NULL );

    label2 = XmCreateLabel (_inputPaneW, "Or enter an SPF file name:", NULL, 0);
    XtManageChild (label2);
    XtVaSetValues (label2,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	frame,
		   XmNtopOffset,	toff,
		   XmNleftAttachment,	XmATTACH_POSITION,
		   XmNleftPosition,	3,
		   XmNrightAttachment,	XmATTACH_POSITION,
		   XmNrightPosition,	70,
		   NULL );

    _fileSel_txtW = XmCreateText (_inputPaneW, "spfw_inputText", NULL, 0);
    XtManageChild (_fileSel_txtW);
    XtVaSetValues (_fileSel_txtW,
		   XmNtopAttachment,   XmATTACH_WIDGET,
		   XmNtopWidget,       label2,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNleftAttachment,  XmATTACH_FORM,
		   NULL);
    XtAddCallback (_fileSel_txtW, XmNactivateCallback,
		   spfw_txtCb, NULL ); 

    XtManageChild (_inputPaneW);


    /*
     * ******** BROWSE PANE ********
     */
    _browseBtnPane = 
	XtVaCreateManagedWidget ("Browse",
				 xmPushButtonWidgetClass, pane,
				 NULL);
    XtAddCallback(_browseBtnPane, XmNarmCallback,
		  spfw_browseBtnCb, (XtPointer) NULL); 

    /*
     * ******** SAVE LITERAL TIME PANE ********
     */
    _stimePaneW = XtVaCreateWidget("spfw_stimeform",
                    xmFormWidgetClass,        pane,
                    XmNnoResize,              TRUE,
                    XmNautoUnmanage,          FALSE,
                    NULL );

    stime_label = XmCreateLabel( _stimePaneW,
                    "Save Source Timestamp As:", NULL, 0 );
    XtVaSetValues( stime_label,
                    XmNtopAttachment,         XmATTACH_POSITION,
                    XmNtopPosition,           2,
                    XmNleftAttachment,        XmATTACH_POSITION,
                    XmNleftPosition,          1,
                    XmNrightAttachment,       XmATTACH_POSITION,
                    XmNrightPosition,         60,
                    NULL );
    XtManageChild( stime_label );

    _stimeRadioW = XtVaCreateManagedWidget ("stime_radio",
                    xmRowColumnWidgetClass,   _stimePaneW,
                    XmNorientation,           XmHORIZONTAL,
                    XmNradioBehavior,         True,
                    XmNtopAttachment,         XmATTACH_WIDGET,
                    XmNtopWidget,             stime_label,
                    XmNrightAttachment,       XmATTACH_FORM,
                    XmNleftAttachment,        XmATTACH_FORM,
                    NULL);

    nn = XtNumber (stime_str);
    _stimeBy = SAVE_AS_CURRENT;
    _stimeBtn = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));

    for (ii = 0; ii < nn; ii++) {
        _stimeBtn[ii] = XtVaCreateManagedWidget (stime_str[ii],
                    xmToggleButtonWidgetClass,      _stimeRadioW,
                    XmNtraversalOn,                 FALSE,
                    NULL);
        XtAddCallback(_stimeBtn[ii], XmNdisarmCallback,
                    spfw_stimeCb, (XtPointer) ii);

        XmToggleButtonSetState (_stimeBtn[ii], (_stimeBy == ii), FALSE);
    }

    XtManageChild(_stimePaneW);

    /*
     * "Save Reference Time" toggle button.
     */
    _refTimePaneW = XtVaCreateWidget("_refTimePaneW",
                    xmFormWidgetClass,        pane,
                    XmNnoResize,              TRUE,
                    XmNautoUnmanage,          FALSE,
                    NULL );

    _sReftimeBtn = XtVaCreateManagedWidget("Save Reference Time",
                             xmToggleButtonGadgetClass,  _refTimePaneW, 
			     XmNtopAttachment,           XmATTACH_FORM,
                             XmNleftAttachment,          XmATTACH_FORM,
			     XmNset,			 _sReftime,
                             XmNtraversalOn,             False,  
                             NULL);
 
    XtAddCallback(_sReftimeBtn, XmNvalueChangedCallback, 
		           (XtCallbackProc)spfw_sReftimeCb, NULL);

    XtManageChild(_refTimePaneW);

    /*
     * ******** BOTTOM CONTROL PANE ********
     */
    _bottomPaneW = 
	(Widget) NxmCtlBtn_create (pane, 1, "spfw_btm", 
				   XtNumber(btm_btnstr), btm_btnstr, 
				   (XtCallbackProc)spfw_btmCtlBtnCb, NULL);

    XtManageChild(pane);

    
    /*
     * browse popup
     */
    argcnt = 0;

    xmpattern  = XmStringCreateLocalized(FILE_PATTERN);

    XtSetArg (args[argcnt], XmNpattern, xmpattern); 
    argcnt++;

    XtSetArg (args[argcnt], XmNtitle, "Browse"); 
    argcnt++;

    _browsePopup = XmCreateFileSelectionDialog(parent, "browseBox", 
					       args, argcnt);
    XmStringFree(xmpattern);

    XtAddCallback(_browsePopup, XmNcancelCallback,
		  spfw_browseDoneCb, (XtPointer) 0);
    XtAddCallback(_browsePopup, XmNokCallback,
		  spfw_browseDoneCb, (XtPointer) 1);

    XtUnmanageChild(XmFileSelectionBoxGetChild(_browsePopup, 
					       XmDIALOG_HELP_BUTTON));

    /*
     *  Restoring confirmation popup     
     */
    spfw_createConf( pane );

}

/*=====================================================================*/

void spfw_popup ( int func )
/************************************************************************
 * spfw_popup								*
 *									*
 * This function loads and displays the SPF file names into the file	*
 * selection dialog box.						*
 *									*
 * void spfw_popup ( func )						*
 *									*
 * Input parameters:							*
 *	func		int		Action function			*
 *					    0 - RESTORE_SPF		*
 *					    1 - SAVE_SPF		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	revise from pgfilw_popup() 		*
 ***********************************************************************/
{
    int		ipos;
    XmString	xmstr, dirstr;
/*---------------------------------------------------------------------*/

    _spfilFunc = func;

    /*
     * set the dialog title 
     */
    if ( func == SAVE_SPF ) {
	xmstr = XmStringCreateLocalized("Save To SPF File");
	dirstr = XmStringCreateLocalized(LOCAL_DIR);
	XtVaSetValues(_browsePopup, XmNdirectory, dirstr, NULL);
	XmStringFree (dirstr);

	spfw_managePanes (SAVE_POPUP);
    }
    else {
	xmstr = XmStringCreateLocalized("Restore From SPF File");

	spfw_managePanes (RESTORE_POPUP);
    }

    XtVaSetValues (_fileSelW, XmNdialogTitle, xmstr, NULL);
    XtVaSetValues (_browsePopup, XmNdialogTitle, xmstr, NULL);
    XmStringFree (xmstr);

    /*
     * hi-light the current directory
     */
    ipos = _dirPos;
    XmListSelectPos (_dirSel_listW, ipos, TRUE);

    ipos += (VISIBLE_ITEM / 2);
    if (ipos < VISIBLE_ITEM) ipos = VISIBLE_ITEM;
    if (ipos > _spfUsrTbl.nitems) ipos = _spfUsrTbl.nitems;

    XmListSetBottomPos (_dirSel_listW, ipos);

    /*
     * set input text widget
     */
    XmTextSetString( _fileSel_txtW, _fileName );

    spfw_setFileList ();

    XtManageChild(_fileSelW);

}

/*=====================================================================*/

void spfw_popdown ( void )
/************************************************************************
 * spfw_popdown								*
 *									*
 * This function puts the file window down. 				*
 *									*
 * void spfw_popdown ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_popdown() 		*
 * J. Wu/GSC		07/01	call spfw_popdownConf() 		*
 ***********************************************************************/
{
    spfw_popdownConf (); 
    
    if (XtIsManaged (_fileSelW)) {
    	XtUnmanageChild (_fileSelW);
    }

    if (XtIsManaged (_browsePopup)) {
    	XtUnmanageChild (_browsePopup);
    }

}


/*=====================================================================*/
/* ARGSUSED */
void spfw_selectCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * spfw_selectCb							*
 *									*
 * Callback function for selecting an SPF File.				*
 *									*
 * void spfw_selectCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (NULL)	*
 *	call		XtPointer	Callback trigger		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_selectCb() 		*
 ***********************************************************************/
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
    char * tmp_str;
/*---------------------------------------------------------------------*/

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &tmp_str);

    strcpy (_fileName, tmp_str);

    XmTextSetString (_fileSel_txtW, _fileName);

    XtFree (tmp_str);

}

/*=====================================================================*/
/* ARGSUSED */
void spfw_selectDirCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * spfw_selectDirCb							*
 *									*
 * Callback function for selecting an SPF File.				*
 *									*
 * void spfw_selectDirCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (NULL)	*
 *	call		XtPointer	Callback trigger		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_selectDirCb() 		*
 ***********************************************************************/
{

    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;

/*---------------------------------------------------------------------*/

    _dirPos = cbs->item_position;

    if ( _dirPos == _spfUsrTbl.nitems ) {
        strcpy ( _dirPath, LOCAL_DIR );
    }
    else {
        strcpy ( _dirPath, _spfUsrTbl.items[(_dirPos) - 1].usrpath );
        if ( _dirPath[ ( strlen(_dirPath) - 1 ) ] != '/' ) {
	    strcat ( _dirPath, "/" );
        }
    }
    
    spfw_setFileList ( );

}

/*=====================================================================*/
/* ARGSUSED */
void spfw_txtCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * spfw_txtCb								*
 *									*
 * Callback function for text input widget.				*
 *									*
 * void spfw_txtCb ( wid, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (NULL)	*
 *	call		XtPointer	Callback trigger		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding 				*
 ***********************************************************************/
{
    spfw_btmCtlBtnCb( NULL, 0, NULL ); 
}

/*=====================================================================*/
/* ARGSUSED */ 
void spfw_confirmCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * spfw_confirmCb							*
 *									*
 * Callback function for confirming to restore/save an SPF file.	*
 *									*
 * void spfw_confirmCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding 				*
 * J. Wu/GSC		07/01	load & check SPF file before restoring	*
 * R. Tian/SAIC		01/02	fixed bug when loading large SPF file   *
 * J. Wu/GSC		04/02	fixed crash bug for certain short SPF  	*
 * 				files without valid data sources in it	*
 * M. Li/SAIC		12/02	popup Restore Confirmation only in need *
 ***********************************************************************/
{
    int		ier;
    char	spfile[MXFLSZ], *spftext, lblStr[256];
    char        newfil[LLPATH], tmpStr[256];
    char	prefs_tag[MAX_PREF_STR] = "RESTORE_POPUP";
    long        flen = 0;
    XmString	xmstr;
    const int	addFmtLen = 128; /* Additional string length required to
    				    hold the format message */
    Boolean	bal;
				    
/*---------------------------------------------------------------------*/

    if ( strlen(_fileName) > (size_t)0 ) {
	
	if ( _spfilFunc == RESTORE_SPF ) {
                    
	    /*
             *  Retrieve the selected SPF file and load into SPF buffer.
             */
            spfw_getFileName( spfile );
            spf_load ( spfile, &ier );

	    /*
             *  Pop up confirmation window with SPF file content in it.
             */
            if ( ier < 0 ) {	
	        NxmWarn_show ( _fileSelW, "Failed to load SPF file" );
            }
	    else {	     
	        /*
                 * Query the actual SPF file size in order to deal with a
                 * SPF file with any size.
                 */
	        cfl_inqr(spfile, NULL, &flen, newfil, &ier);	        

                flen += (long)addFmtLen;
		if ( ier < 0 ) {	
	            NxmWarn_show ( _fileSelW, "Failed to query SPF file size" );
                }
                else {
                    if( ! (spftext = XtMalloc((Cardinal)flen))) {
	                NxmWarn_show ( _fileSelW, 
                                       "Failed to allocate space for SPF file" );
                    }
	            else {	     		        
			spfw_formatConf( flen, spftext, lblStr, &ier );

		        if ( ier == 0 ) {
		            XmTextSetString( _rstContentW, spftext );
		            xmstr = XmStringCreateLtoR(lblStr, 
                                                       XmFONTLIST_DEFAULT_TAG);
                            XtVaSetValues( _rstConfLblW,
                                          XmNlabelString, xmstr,
				          XmNalignment,	  XmALIGNMENT_BEGINNING,
				          NULL);
		            XmStringFree( xmstr );               

			    ctb_pfbool(prefs_tag, &bal, &ier);
		 	    if ( bal ) {
		                spfw_popupConf (); 
			    }
			    else {
    				spfw_popdownConf ();
    				spfw_popdown ();
    				spfw_restoreSPF ();
			    }

		        }
			else {
			    sprintf ( tmpStr, "No valid data sources found in %s\n", 
			                       _fileName ); 
			    NxmWarn_show ( _fileSelW, tmpStr );
			}
                      
                         XtFree(spftext);
                    }
		}	       
	    }
	    
	}
	else {  
	     spfw_popdown ();
	     spfw_saveSPF ();
	}	     
    }    
    
}

/*=====================================================================*/
/* ARGSUSED */ 
void spfw_browseBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * spfw_browseBtnCb							*
 *									*
 * Callback function for the browse button on file selection palette.	*
 *									*
 * void spfw_browseBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	revise from pgfilw_browseBtnCb()	*
 ***********************************************************************/
{
    /*
     *  Browse always starts from the local directory.
     */
    strcpy ( _dirPath, LOCAL_DIR );
    
    XtManageChild ( _browsePopup );

}

/*=====================================================================*/
/* ARGSUSED */ 
void spfw_browseDoneCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * spfw_browseDoneCb							*
 *									*
 * Callback function for the Ok/Cancel buttons on the browse palette. 	*
 *									*
 * void spfw_browseDoneCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	revise from pgfilw_browseDoneCb()	*
 * E. Safford/SAIC	04/02	use cfl_isdir instead of local version  *
 ***********************************************************************/
{
    int		which = (long)clnt, ier;
    char 	*text;
    char	filepart[MXFLSZ] = "\0" , pathpart[LLPATH] = "\0";
    Widget	popup;
    FSBCBS 	*fcbs;
/*---------------------------------------------------------------------*/

    if ( which == 1 ) {	/* OK */
	
	fcbs = (FSBCBS *) cbs;
	XmStringGetLtoR ( fcbs->value, XmFONTLIST_DEFAULT_TAG, &text );
	
	if ( cfl_isdir ( text )  ) {
	    _fileName[0] = '\0';	     
	    strcpy ( _dirPath, text ); 	
	} 
	else {
	    cfl_path ( text, pathpart, filepart, &ier ) ;   
            if ( filepart[0] != '\0' ) {
	        strcpy ( _fileName, filepart ); 	
	    }	
            if ( pathpart[0] != '\0' ) {
	        strcpy ( _dirPath, pathpart ); 	
	    }	
	}
        
	if ( _dirPath[ ( strlen(_dirPath) - 1 ) ] != '/' ) {
	    strcat ( _dirPath, "/" );
	}
        	       
	XtFree ( text );
	XmTextSetString ( _fileSel_txtW, _fileName );
	XmTextSetInsertionPosition ( _fileSel_txtW, (long)strlen(_fileName) );

	if ( _fileName[0] == '\0' ) {
	    popup = ( XtIsManaged (_browsePopup)) ? _browsePopup : _fileSelW;
	    NxmWarn_show ( popup, "Please select or input a file name." );
	}
	else {
	    spfw_preCheck ( );
	}
       
    }
    else {  /* CANCEL */
	XtUnmanageChild ( _browsePopup );
    }
    
}

/*=====================================================================*/
/* ARGSUSED */ 
void spfw_sortByCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * spfw_sortByCb							*
 *									*
 * Callback function for the sort by toggles				*
 *									*
 * void spfw_sortByCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_sortByCb()		*
 ***********************************************************************/
{
    int	  new_sort;
/*---------------------------------------------------------------------*/

    new_sort = (long) clnt;

    if (new_sort != _sortBy) {
        _sortBy = new_sort;
 	spfw_popdown( );
        spfw_popup( _spfilFunc );
    }
    
}

/*=====================================================================*/

/* ARGSUSED */
void spfw_stimeCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * spfw_stimeCb								*
 *                                                                      *
 * Callback function for the save literal time toggles                  *
 *                                                                      *
 * void spfw_sortByCb ( wid, clnt, cbs )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          Widget that activated callback  *
 *      clnt          XtPointer       Pointer to client data (GrInfo) *
 *      cbs             XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Bailey/HPC	12/05		Creation			*
 ***********************************************************************/
{
    int   new_stime;
/*---------------------------------------------------------------------*/

    new_stime = (long) clnt;

    if (new_stime != _stimeBy) {
        _stimeBy = new_stime;
    }

}

/*=====================================================================*/

/* ARGSUSED */
void spfw_sReftimeCb ( Widget wid, XtPointer clnt, 
				   XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * spfw_sReftimeCb							*
 *									*
 * Callback function for "Save Reference Time" button.			*
 *									*
 * void spfw_sReftimeCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget				widget ID		*
 *	clnt	XtPointer			not used		*
 *	*cbs	XmToggleButtonCallbackStruct	callback structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		03/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _sReftime = (Boolean)cbs->set;

}

/*=====================================================================*/

static void spfw_restoreSPF ( void )
/************************************************************************
 * spfw_restoreSPF							*
 *									*
 * This function restores the data settings from the SPF file. 		*
 *									*
 * spfw_restoreSPF ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 * J. Wu/GSC		07/01	add actual functionality		*
 * J. Wu/GSC		07/01	call dataw_clearDataSel() to clear data	*
 *				selections tempaerarily			*
 * J. Wu/GSC		07/01	move file loading to spfw_confirmCb()	*
 * J. Wu/SAIC		08/01	call nmp_rdeflts() to reset map/overlay	*
 ***********************************************************************/
{
    int		lp, ier;

/*---------------------------------------------------------------------*/
       
    /*
     *  Clear all current data settings & set default map/overlay. 
     */
    nmp_rdeflts( &ier );
    for ( lp = 0; lp < MAX_LOOP; lp++ ) {
        dataw_clearDataSel( lp );        
        loop_setDataChngd( lp, TRUE );    
    }
        
    /*
     *  Switch to the first loop and load data settings from SPF file.
     */
    dataw_setLoop( 0 ); 
    dataw_loadsp( ); 

}

/*=====================================================================*/

static void spfw_preCheck ( void )
/************************************************************************
 * spfw_preCheck							*
 *									*
 * This function gets the file name, checks the file permissions, and	*
 * confirms the name with the user.					*
 *									*
 * void spfw_preCheck( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 * J. Wu/GSC		07/01	change calling sequence for restoring	*
 ***********************************************************************/
{
    int		ipos, ier;
    char	*ss, fname[FILE_FULLSZ], warning[(FILE_FULLSZ + 80)];
    char	*fptr;
    Boolean	readable, writable;
    Widget	popup;
/*---------------------------------------------------------------------*/

    /*
     *  Retrieve the file name first.
     */
    if ( _spfilFunc == SAVE_SPF ) {
        ss = XmTextGetString ( _fileSel_txtW );
        strcpy ( _fileName, ss );
        XtFree(ss);
    }
    
    if ( strlen( _fileName ) <= (size_t)0 ) {
	NxmWarn_show ( _fileSelW, "No file has been specified" );
	return;    
    }
           
    /*
     *  Retrieve full file name & append .spf extension if necessary.
     */
    strcpy ( fname, _dirPath );
    strcat ( fname, _fileName );
    
    cst_srch ( 0, (int)strlen(_fileName), ".spf", _fileName, &ipos, &ier );
    if ( ier == -4 ) {
        strcat ( fname, ".spf" );
    }
    
    /*
     *  Get an file name without the initial "./" in it if it is under
     *  the local directory.
     */
    if ( strcmp ( _dirPath, LOCAL_DIR ) == 0 ) { 
	fptr = strchr ( fname, '/' );
	fptr ++;
    }
    else {
	fptr = &( fname[0] );
    }
    
    /*
     *  Check file permission before reading/writing and form confirmation.
     *  message.
     */
    spfw_checkPerms ( fname, &readable, &writable );

    popup = ( XtIsManaged (_browsePopup) ) ? _browsePopup : _fileSelW;
    
    if ( _spfilFunc == RESTORE_SPF ) {
	
	if ( readable ) {
            spfw_confirmCb( popup, 0, NULL );
	}
	else { 
	    NxmWarn_show ( popup, "SPF File is not readable" );
	}
	
    }
    else {   /* SAVE_SPF */
    
        if ( writable ) {
            sprintf ( warning,
	        "Are you sure you want to save the current settings to SPF file %s?",
	         fptr );        	
            NxmConfirm_show ( popup, warning, spfw_confirmCb, 
		      NULL, NULL, &ier );
	}
	else {
	    NxmWarn_show ( _fileSelW, "No permission to write SPF File" );
        }    
    }    
}

/*=====================================================================*/

static void spfw_saveSPF ( void )
/************************************************************************
 * spfw_saveSPF								*
 *									*
 * This function saves the data settings into the SPF file.		*
 *									*
 * void spfw_saveSPF ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 * J. Wu/GSC		07/01	add checks to source status & category	*
 * J. Wu/GSC		07/01	check if there are any active sources 	*
 *                 		& correct source string for GRD/SFF/SNF *
 * J. Wu/SAIC		08/01	number sources consectively at saving	*
 * J. Wu/SAIC		08/01	save off the roam factor		*
 * J. Wu/SAIC		08/01	save off auto-update & map settings	*
 * J. Wu/SAIC		08/01	save off map overlay selections		*
 * J. Wu/SAIC		04/02	save off data source's attributes	*
 * J. Wu/SAIC		04/02	save off map overlay's attributes	*
 * J. Wu/SAIC		05/02	save off "single time mode"		*
 * J. Wu/SAIC		07/03	save off sources' range/interval	*
 * T. Lee/SAIC		04/04	save off delta_rt			*
 * T. Lee/SAIC		10/04	save off bin hours for certain data type*
 * C. Bailey/HPC	01/06	added save current/latest time option	*
 * H. Zeng/SAIC		03/07	added reference time tag&value pair	*
 * M. Li/SAIC		03/08	added CAT_ENS				*
 * F. J. Yen/NCEP	04/08	save bin minutes & most recent only flag*
 * M. Li/SAIC		05/08	save ensemble model list		8
 ***********************************************************************/
{
    int		ier, ier1, ipos, hlen, ii, jj, srcNum, domInd, catnum;
    int		noc1, noc2, kk, srcInd, roamFac, autoUpdt;
    int		ovlnum, ovltyp, lastOvlNum, sel_time;
    char	full_name[FILE_FULLSZ], ovlsep[] = "|", ovlStr[1024];
    char	tagStr[32] = "\0", dataStr[200] = "\0", tmp[20] = "\0";
    char	lpStr[20] = "\0", cycStr[] = "[cycle_time]", yddStr[] = "[cycle]";
    char	mlist[200], ydd[6], cycIdx[40];
    FILE	*fptr;
    dsrc_t	*domSrc, *dataSrc;
    Boolean	lp_hasImg, ovlflg[MAX_OVL];
    dttms_t	time_str;
    nmpstr_t	map, proj, garea[2];
    nmpovlstr_t	ovlnms[MAX_OVL], ovlattr[MAX_OVL]; 
       
/*---------------------------------------------------------------------*/
         
    /*
     *  If there are no active sources, return; otherwise, save them.
     */     
    if ( dataw_noActSrc() ) return;

    if ( strlen(_fileName) > (size_t)0 ) {

	strcpy ( full_name, _dirPath );
	strcat ( full_name, _fileName );
               
        /*
         * Append .spf extension if necessary.
         */
        cst_srch ( 0, (int)strlen(_fileName), ".spf", _fileName, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat (full_name, ".spf");
	    strcat (_fileName, ".spf");
	}

        /*
         *  Create the SPF file new to wipe out any previous inf.
         */
        fptr = spf_create ( full_name, &hlen, &ier );

        if ( ier < 0 ) {
	    
	    NxmWarn_show ( _fileSelW, "Failure to write!" );
	    return;	

	}
	
        /*
         *  Get map overlay number and names.
         */
	nmp_govlnum( &ovlnum, &ier );
        nmp_govlnms( ovlnms, &ier );
	
	/*
         *  Write the selected data sources into the SPF file.
         */
	for ( ii = 0; ii < MAX_LOOP; ii++ ) {
            
	    srcNum = dataw_getNumSrcs( ii );
	    srcInd = 0;
	    lp_hasImg = False;
	    
	    if ( srcNum > 0 ) {
	    
	        domSrc = dataw_getDomSrc( ii );
				
		sprintf( lpStr, "%s%d%s", "!\n! Loop ", ii+1, "\n!\n" ); 
		cfl_writ( fptr, (int)strlen(lpStr), (unsigned char*)lpStr, &ier );
	        
		for ( jj = 0; jj < srcNum; jj++ ) {
		    
		    dataSrc = dataw_getDataSrc( ii, jj );
		                        
		    /*
		     *  If the source status is on, write it to SPF file.
		     */
		    if ( dataSrc->src_on ) {
		        
			srcInd++;
		        
			if ( dataSrc == domSrc ) { 
		            domInd = srcInd;		        
                        }
			
			if ( dataSrc->catg == CAT_IMG && 
			    ( strstr( dataSrc->path, "SAT") != NULL ||
			      strstr( dataSrc->path, "RAD") != NULL ) ) {
		            lp_hasImg = True;     
			}
						
			sprintf( tagStr, "%s%d%s%d", "loop", ii+1, 
			                         "_source", srcInd ); 
		        catnum = dataSrc->catg;
		        ctb_dcatitos( &catnum, dataStr, &ier );
		        strcat( dataStr, "|" );
		        strcat( dataStr, dataSrc->path );

			if ( _stimeBy ) {
		            /*
		             *  If the data category is GRID or surface forecast,
		             *  replace the date string with the string 
			     *  "[cycle_time]".
		             */
			    if ( catnum == CAT_GRD || catnum == CAT_SFF ||
			         catnum == CAT_SNF ) {
                            
				cst_nocc ( dataStr, '/', 2, 0, &noc1, &ier );
				cst_nocc ( dataStr, '/', 3, 0, &noc2, &ier );
                            
				for ( kk = noc1+1; kk < noc2; kk++ )
			        {
			            tmp[ kk-noc1-1 ] = dataStr[ kk ];	    
			        }
				tmp[ noc2-noc1-1 ] = '\0';
                                cst_rpst( dataStr, tmp, cycStr, dataStr, &ier );
			    }
		        }
		        spf_write( fptr, tagStr, dataStr, &ier );

			/*
		         *  Write out the source's timeline range & interval.
			 */				    
			sprintf( tagStr, "%s%d%s%d%s", "loop", ii+1,
		                                      "_source", srcInd, "_interval"); 
			sprintf( dataStr, "%d", dataSrc->interval );  
			spf_write( fptr, tagStr, dataStr, &ier );

			sprintf( tagStr, "%s%d%s%d%s", "loop", ii+1,
		                                      "_source", srcInd, "_range"); 
			sprintf( dataStr, "%d", dataSrc->range );  
			spf_write( fptr, tagStr, dataStr, &ier );


			if ( catnum == CAT_ENS ) {
			    dslw_getModList(dataSrc->path, mlist, cycIdx);
			    if ( strlen (mlist) > 2 && strlen(cycIdx) > 0 ) {
                            	sprintf( tagStr, "%s%d%s%d%s", "loop", ii+1,
                                                      "_source", srcInd, "_model_list");
				/*
				 *  replace the date/time string with the string [cycle].
				 */

				while ( ier == 0 ) {
				    cst_nocc ( mlist, '/', 1, 0, &noc1, &ier );
				    cst_ncpy (ydd, mlist+noc1-2, 5, &ier1); 
				    cst_rpst( mlist, ydd, yddStr, mlist, &ier1 );	
				}
				 
				strcat ( mlist, "+" );
				strcat ( mlist, cycIdx );
				mlist[strlen(mlist)] = '\0';
                            	spf_write( fptr, tagStr, mlist, &ier );
			    }
			}

			/* 
			 * Write out bin hours for certain data categories.
			 */
			if ( catnum == CAT_SFC || catnum == CAT_SFF ||
			     catnum == CAT_SND || catnum == CAT_SNF ) {
                            
			    sprintf ( tagStr, "%s%d%s%d%s", "loop", ii+1, 
							"_source", srcInd, "_timbin" );
			    ctb_dhrsitos ( &dataSrc->ionoff, &dataSrc->bfhr, 
					   &dataSrc->bfmn, &dataSrc->afhr,
					   &dataSrc->afmn, &dataSrc->mstrctf,
					   dataStr, &ier );
			    spf_write ( fptr, tagStr, dataStr, &ier );
			}

		        /*
		         *  Write out the source's attributes.
		         */				    
			spfw_saveSrcAttr ( dataSrc, fptr, ii, srcInd );
		    
		    }
		
		} /* end of srcNum loop */
                
		/*
		 *  Write out dominant source index, frame number & skip factor.
		 */				    
		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_dominant" );
                sprintf( dataStr, "%d", domInd );  
		spf_write( fptr, tagStr, dataStr, &ier );

		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_frames" );
                sprintf( dataStr, "%d", domSrc->num_sel );  
		spf_write( fptr, tagStr, dataStr, &ier );

		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_skip" );
                sprintf( dataStr, "%d", domSrc->skip );  
		spf_write( fptr, tagStr, dataStr, &ier );
				
                sprintf( tagStr, "%s%d%s", "loop", ii+1, "_delta_rt"); 
                sprintf( dataStr, "%d", domSrc->delta_rt );  
		spf_write( fptr, tagStr, dataStr, &ier );

                /*
                 * Check _sReftime value and save the reference time accordingly.
                 */
                if ( _sReftime ) {

                  sprintf( tagStr, "%s%d%s", "loop", ii+1, "_reftime"); 
		  dataw_getTimeStr ( ii, FALSE, time_str );
		  spf_write( fptr, tagStr, time_str, &ier );
		}

		/*
		 *  Save off the roam factor.
		 */
		roamFac = dataw_getRoamVal( ii );
                
		if ( roamFac < 0 || ( !lp_hasImg && 
		                      roamFac == SIZE_OF_IMAGE ) ) {
		    roamFac = 0; 
		}
		
		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_roam" );
                sprintf( dataStr, "%d", roamFac );  
		spf_write( fptr, tagStr, dataStr, &ier );
	    
		/*
		 *  Save off the auto-update state.
		 */
		autoUpdt = TRUE;
		if ( !loop_getAutoUpdt( ii ) ) autoUpdt = FALSE;
		
		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_auto_update" );
                sprintf( dataStr, "%d", autoUpdt );  
		spf_write( fptr, tagStr, dataStr, &ier );
				
		/*
		 *  Save off the map settings.
		 */
		nmp_gmapattr( ii, map, proj, garea, &ier );

		if ( ier == 0 ) {
		    sprintf( tagStr, "%s%d%s", "loop", ii+1, "_map" );
                    sprintf( dataStr, "%s", map );  
		    spf_write( fptr, tagStr, dataStr, &ier );
		    
		    sprintf( tagStr, "%s%d%s", "loop", ii+1, "_proj" );
                    sprintf( dataStr, "%s", proj );  
		    spf_write( fptr, tagStr, dataStr, &ier );
		    
		    sprintf( tagStr, "%s%d%s", "loop", ii+1, "_garea" );
                    sprintf( dataStr, "%s|%s", garea[0], garea[1] );  
		    spf_write( fptr, tagStr, dataStr, &ier );
		}
		
		/*
		 *  Save off the map overlay settings.
		 */
                nmp_govlflg( ii, ovlflg, &ier );
		
		if ( ier == 0 ) {
		    
		    sprintf( tagStr, "%s%d%s", "loop", ii+1, "_map_ovl" );
                    ovlStr[0] = '\0';
		    
		    lastOvlNum = 0;
		    for ( kk = 0; kk < ovlnum; kk++ ) {		        
		        if ( ovlflg[kk] ) lastOvlNum = kk;
		    }
		    
		    for ( kk = 0; kk <= lastOvlNum; kk++ ) {
		    
			if ( ovlflg[kk] ) {		            
                            nmp_govlattr ( kk, ii, &ovltyp, ovlattr[kk], &ier );
			    strcat( ovlStr, ovlnms[kk] );
			    strcat( ovlStr, ":" );
			    strcat( ovlStr, ovlattr[kk] );
			        strcat( ovlStr, ovlsep );
			    if ( kk != lastOvlNum ) {
			        strcat( ovlStr, "\n\t\t\t\t" );
			    }
		        }			 			
		    }
		    
		    spf_write( fptr, tagStr, ovlStr, &ier );

		} /* End of saving overlay attribute */

		/*
		 *  Save off the single time mode if the dominant source
		 *  is GRD.
		 */
		sprintf( tagStr, "%s%d%s", "loop", ii+1, "_single_time" );                   
		strcpy( dataStr, "0" );  
		
		if ( (domSrc->catg == CAT_GRD || domSrc->catg == CAT_ENS) && loop_getTmMode(ii) ) {
		    
		    sel_time = 1;
		    
		    for ( kk = 0; kk < MAX_FRAME; kk++ ) {
		       if ( domSrc->frm[kk].selected ) {
		           sel_time = kk+1;
		           break;
		       }
                    }
		    
		    sprintf( dataStr, "1|%d", sel_time );  		    
		}
		    
		spf_write( fptr, tagStr, dataStr, &ier );		 		

	    } /* end of loop-has-source: srcNum > 0 */	
	
	} /* end of MAX_LOOP loop */

        spf_close( fptr, &ier );

    }
    
}

/*=====================================================================*/

static void spfw_managePanes ( int popup_type )
/************************************************************************
 * spfw_managePanes							*
 *									*
 * This function manages the appropiate children of the main pane	*
 * widget based on popup_type.						*
 *									*
 * void spfw_managePanes ( popup_type )					*
 *									*
 * Input parameters:							*
 *	popup_type	int	decides which children to use		*
 *				    0 - RESTORE_POPUP			*
 *				    1 - SAVE_POPUP			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_managePanes()		*
 * C. Bailey/HPC	12/05	add _stimePaneW to SAVE_POPUP		*
 * H. Zeng/SAIC		03/07	add _refTimePaneW to SAVE_POPUP		*
 ***********************************************************************/
{

    spfw_unmanagePanes ();

    switch ( popup_type ) {
        
	case RESTORE_POPUP:
	    XtManageChild (_sortPaneW);
	    XtManageChild (_selectPaneW);
	    XtManageChild (_browseBtnPane);
	    XtManageChild (_bottomPaneW);
	    break;

        case SAVE_POPUP:
	    XtManageChild (_sortPaneW);
	    XtManageChild (_selectPaneW);
	    XtManageChild (_inputPaneW);
	    XtManageChild (_browseBtnPane);
	    XtManageChild (_stimePaneW);
	    XtManageChild (_refTimePaneW);
	    XtManageChild (_bottomPaneW);
	    break;

   }
   
}

/*=====================================================================*/

static void spfw_unmanagePanes ( void )
/************************************************************************
 * spfw_unmanagePanes							*
 *									*
 * This function unmanages all the children of the main pane widget.	*
 *									*
 * void spfw_unmanagePanes ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_unmanagePanes()	*
 * C. Bailey/HPC	12/05	added stimePaneW			*
 * H. Zeng/SAIC		03/07	added _refTimePaneW			*
 ***********************************************************************/
{
    
    if ( XtIsManaged (_sortPaneW) ) {
	XtUnmanageChild (_sortPaneW);
    }

    if ( XtIsManaged (_selectPaneW) ) {
	XtUnmanageChild (_selectPaneW);
    }

    if ( XtIsManaged (_inputPaneW) ) {
	XtUnmanageChild (_inputPaneW);
    }

    if ( XtIsManaged (_browseBtnPane) ) {
	XtUnmanageChild (_browseBtnPane);
    }

    if ( XtIsManaged (_bottomPaneW) ) {
	XtUnmanageChild (_bottomPaneW);
    }

    if ( XtIsManaged (_stimePaneW) ) {
        XtUnmanageChild (_stimePaneW);
    }

    if ( XtIsManaged (_refTimePaneW) ) {
        XtUnmanageChild (_refTimePaneW);
    }

} 

/*=====================================================================*/

static void spfw_setFileList ( void )
/************************************************************************
 * spfw_setFileList							*
 *									*
 * This function sets up the list of files for the slected path.	*
 *									*
 * void spfw_setFileList ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_setFileList()		*
 ***********************************************************************/
{
    int		ii, nf;
    char	fn_list[MAX_FILES][MXFLSZ];
    XmString	xmstr;
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    /*
     * Wipe the list of .spf filenames       
     */
    XmListDeleteAllItems(_fileSel_listW);

    /*
     *  Get list of file names in the directory & write to the fn_list
     */
    nf = cfl_gfil (_sortBy, MAX_FILES, _dirPath, ".spf", fn_list);

    if ( nf > 0 ) {

        xmfils = (XmStringTable)XtMalloc((size_t)nf*sizeof(XmString *));

        for ( ii = 0; ii < nf; ii++ ) {
	    xmfils[ii] = XmStringCreateLocalized( fn_list[ii]);
        }

        XtVaSetValues(_fileSel_listW,
                XmNitems,      xmfils,
                XmNitemCount,  nf,
                NULL);

        for ( ii = 0; ii < nf; ii++ ) {
	    XmStringFree(xmfils[ii]);
        }
	XtFree((XtPointer)xmfils);

	/*
	 * hi-light the selected item if applicable
	 */
 	xmstr = XmStringCreateLocalized (_fileName);
	XmListSelectItem (_fileSel_listW, xmstr, FALSE);
	XmListSetBottomItem (_fileSel_listW, xmstr);
	XmStringFree (xmstr);

    }
}

/*=====================================================================*/
/* ARGSUSED */
void spfw_btmCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * spfw_btmCtlBtnCb							*
 *									*
 * Callback function for the bottom control buttons on the file 	*
 * selection popup window.  						*
 *									*
 * void spfw_btmCtlBtnCb  ( wid, which, call )				*
 *									*
 * Input parameters:                                                    *
 *	wid		Widget		Widget that activated callback	*
 *	which	        long	        which button			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    switch ( which ) {

	case 0:		/* OK */	    
	    
	    spfw_preCheck();	    
	    break;

	case 1:		/* CANCEL */
	    
    	    spfw_popdown( ); 
	    break;
    
    }

}

/*=====================================================================*/

static void spfw_checkPerms ( char *cfile, Boolean *can_read, Boolean *can_write )
/************************************************************************
 * spfw_checkPerms							*
 *									*
 * This function checks the permissions on the passed in file to see if	*
 * the user has read and write permission.  If the file is not found,	*
 * can_read is FALSE, and the write permission of the directory is	*
 * checked.  If the directory is not found, can_write is FALSE.	Due to	*
 * policy, the user is only allowed to write in a directory owned by	*
 * user.								*
 *									*
 * void spfw_checkPerms (cfile, can_read, can_write)			*
 *									*
 * Input parameters:							*
 *	*cfile		char	filename to be checked			*
 *									*
 * Output parameters:							*
 *	*can_read	Boolean	read  permission result			*
 *	*can_write	Boolean	write permission result			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	copy from pgfilw_checkPerms()		*
 ***********************************************************************/
{
    static Boolean	need_ids  = TRUE;
    Boolean		subdir;
    static uid_t	uid;
    static gid_t	gid;
    struct stat		fstat;
    char		cpath[LLPATH], *cptr;
    int			cnt, ii;
/*---------------------------------------------------------------------*/

    /*
     *  Count the number of '/' found in the cfile string.  
     *  1 = just a file name, >1 = a subdirectory and file name.
     */
    cnt = 0;
    for (ii=(int)strlen(cfile); ii>=0; ii--) {
        if (cfile[ii] == '/') cnt++;
    }
    subdir = (Boolean)((cnt > 1) ? TRUE : FALSE);


    if (need_ids) {
	uid = getuid ();
	gid = getgid ();
	need_ids = FALSE;
    }

    *can_read = *can_write = FALSE;

    /*
     * If the file can be found, check the read and write permissions.
     */
    if (stat (cfile, &fstat) == 0) {

	/*
	 * Check read permissions (other, then group, then user).
	 */
	if (fstat.st_mode & S_IROTH || 
	    (gid == fstat.st_gid && (fstat.st_mode & S_IRGRP)) ||
	    (uid == fstat.st_uid && (fstat.st_mode & S_IRUSR))) 
	    *can_read = TRUE;

	/*
	 * Check write permission (other, then group, then user).
	 */
	if (fstat.st_mode & S_IWOTH ||
	        (gid == fstat.st_gid && (fstat.st_mode & S_IWGRP)) ||
		(uid == fstat.st_uid && (fstat.st_mode & S_IWUSR)))
	    *can_write = TRUE;
    }
    else {

	/*
	 *  If the cfile was determined to contain a subdirectory then
	 *  pull file name off and test for write permission on the target 
	 *  directory.
	 */
  	if ( subdir ) { 
  	    strcpy (cpath, cfile);

	    /*
	     *  locate the last '/' marking the end of the directory string
	     *  and terminate the path there (this removes the file name).
	     */
    	    cptr = strrchr (cpath, '/');

	    if (cptr != NULL) {
		*cptr = '\0';
	    }
	}
	else {  
	    /*
	     *  If no subdirectory, use _dirPath for the expanded path.
	     */
  	    strcpy (cpath, _dirPath); 
	}
  

	/*
	 * If the directory can be found, check the write permissions.
	 */
	if (stat (cpath, &fstat) == 0) {
	    /*
	     * Check write permission (other, then group, then user).
	     */
	    if (fstat.st_mode & S_IWOTH ||
	            (gid == fstat.st_gid && (fstat.st_mode & S_IWGRP)) ||
		    (uid == fstat.st_uid && (fstat.st_mode & S_IWUSR))) {
		*can_write = TRUE;
	    }
	}
    }

}

/*=====================================================================*/

static void spfw_readUsrTbl ( char *tblname, spfutbl_t *spfutbl, int *iret )
/************************************************************************
 * spfw_readUsrTbl							*
 *									*
 * This routine will read SPF user table and create the data structure	*
 * to store the information.						*
 *									*
 * void spfw_readUsrTbl( tblname, spfutbl, iret)                        *
 *									*
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *									*
 * Output parameters:                                                   *
 *      *spfutbl        spfutbl_t   	pointer to spf user table struct*
 *      *iret		int             0 - success                     *
 *									*
 * Return code:                                                         *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	revise from vtbl_readUsrTbl()		*
 ***********************************************************************/
{
    int		num, ier;
    char	buffer[256], title[60], path[256], cwd[256], format[256];
    char	dum1[256], dum2[256], dum3[256];
    size_t	ii, nn;
    FILE	*fp;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL; 

    strcpy ( format, "%s %s" );

    fp = cfl_tbop (tblname, "nmap", &ier);

    nn = 0;
    if (fp && ier == 0) { 
	while (!feof(fp)) {
	    /*
	     * read a record
	     */
	    cfl_trln (fp, 256, buffer, &ier);

	    if  ( ier == 0 )  {

	        if  ( nn == (size_t)0 )  {
		    num = sscanf ( buffer, "%s %s %s", dum1, dum2, dum3 );
		    if  ( num == 3 )  {
			strcpy ( format, "%s %*s %s" );
		    }
		}

		nn++;
	    }

	}
    }

    /*
     * allocate one additional item in case the current user
     * has to be added in
     */
    spfutbl->nitems = (int)nn;
    spfutbl->items  = (spfusr_ent_t *) malloc ((nn+1) *sizeof (spfusr_ent_t));

    if (nn > (size_t)0) {
	rewind(fp);

	ii = 0;
	while ( ii < nn ) {
	    cfl_trln( fp, 256, buffer, &ier );

	    if (ier == 0) {
		sscanf(buffer, format, title, path);

		spfutbl->items[ii].title = (char *) malloc (strlen (title) + 1);
		strcpy( spfutbl->items[ii].title, title );

		spfutbl->items[ii].usrpath = (char *) malloc (strlen(path) + 1);
		strcpy( spfutbl->items[ii].usrpath, path );

		ii++;
	    }
	}
    }

    if (fp) fclose(fp);

    /*
     * set default user
     */
    getcwd (cwd, sizeof (cwd));

    ii = (size_t)spfutbl->nitems;
    spfutbl->items[ii].title = (char *) malloc (strlen (CWD_TITLE) + 1);
    strcpy (spfutbl->items[ii].title, CWD_TITLE);

    spfutbl->items[ii].usrpath = (char *) malloc(strlen(cwd) + 1);
    strcpy ( spfutbl->items[ii].usrpath, cwd );

    spfutbl->nitems++;

}

/*=====================================================================*/

void spfw_getFileName ( char *file_name )
/************************************************************************
 * spfw_getFileName							*
 *									*
 * This function gets the file name, including the full path of the file*
 *									*
 * void spfw_getFileName ( file_name )					*
 *									*
 * Input parameters:                                                    *
 *	None								*
 *									*
 * Output parameters:                                                   *
 *	*file_name	char	full name of the selected SPF file	*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 ***********************************************************************/
{
    int		ipos, ier;

/*---------------------------------------------------------------------*/
    
    if ( _fileName == "\0" ) {
    
	file_name[0] = '\0';
	
    }
    else {
	
	strcpy (file_name, _dirPath);
	strcat (file_name, _fileName);
        
	/*
         *  Append .spf extension if necessary.
         */
        cst_srch ( 0, (int)strlen(_fileName), ".spf", _fileName, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat ( file_name, ".spf" );
        }
    
    }
            
}

/*=====================================================================*/

void spfw_createConf ( Widget parent )
/************************************************************************
 * spfw_createConf							*
 *									*
 * This function creates the restore confirmation popup window.		*
 *									*
 * void spfw_createConf( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget			*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 * J. Wu/GSC		07/01	allow horizontal scrolling 		*
 * T. Piper/SAIC	11/01	freed ctl_btns				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Arg		args[10];
    long	ii, nn;
    Cardinal	kk;
    Widget	pane, form, ctl_form;    
    WidgetList	ctl_btns;
    char	spftext[] = "\0", lblStr[] = "\0";
    char	*ctl_btnstr[] = { "OK", "Cancel" };
    
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _restoreConfW = XmCreateFormDialog( parent, "rstconf_popup",
				       NULL, 0);
    XtVaSetValues( _restoreConfW, 
		   XmNnoResize,        True, 
		   NULL);
    XtVaSetValues( XtParent( _restoreConfW ),
		   XmNtitle, "Restore Confirmation",
		   NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("resConf_pane",
			    xmPanedWindowWidgetClass, _restoreConfW,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
    /*
     * create a scrolled text in a form to list SPF file content
     */
    form = XtVaCreateWidget("form",
			    xmFormWidgetClass,	pane,
			    NULL);

    kk = 0;
    XtSetArg(args[kk], XmNrows,                 10); kk++;
    XtSetArg(args[kk], XmNcolumns,              48); kk++;
    XtSetArg(args[kk], XmNeditable,		False); kk++;
    XtSetArg(args[kk], XmNscrollVertical,	True); kk++;
    XtSetArg(args[kk], XmNscrollHorizontal,	True); kk++;
    XtSetArg(args[kk], XmNcursorPositionVisible,	False); kk++;
    XtSetArg(args[kk], XmNeditMode,	    XmMULTI_LINE_EDIT); kk++; 
    XtSetArg(args[kk], XmNvalue,	    	spftext); kk++; 
    
    _rstContentW = (Widget)XmCreateScrolledText( form, 
                                 "rstConf_text", args, kk );

    XtManageChild( _rstContentW );  
    XtManageChild( form );
    
    /*
     *  Create confirm label & buttons contained in a form
     */
    nn = XtNumber( ctl_btnstr );
    ctl_form  = (Widget)XtVaCreateManagedWidget ("_confCtlForm",
                xmFormWidgetClass,      pane,
		XmNfractionBase,   	3*nn+1,
                NULL);

    _rstConfLblW = XtVaCreateManagedWidget(lblStr,
			    xmLabelWidgetClass,	ctl_form,
			    XmNtopAttachment,   XmATTACH_FORM,
                            XmNtopOffset,       5,
			    XmNleftAttachment,  XmATTACH_FORM,
			    NULL);

    ctl_btns = (WidgetList) XtMalloc(nn * sizeof(Widget));
    
    for ( ii = 0; ii < nn; ii++) {

	ctl_btns[ii] = XtVaCreateManagedWidget(ctl_btnstr[ii],
		xmPushButtonWidgetClass, ctl_form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		_rstConfLblW,
                XmNtopOffset,		5,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	3*ii+1,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	3*ii+3,
		NULL);
	
	XtAddCallback(ctl_btns[ii], XmNactivateCallback,
		spfw_rstConfCb, (XtPointer)ii);
    }
    XtFree((XtPointer)ctl_btns); 
    XtManageChild( pane );

}
/*=====================================================================*/

void spfw_popupConf ( void )
/************************************************************************
 * spfw_popupConf							*
 *									*
 * This function pops up the SPF restore confirmation window		*
 *									*
 * void spfw_popupConf ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 ***********************************************************************/
{
    
    XtManageChild( _restoreConfW );

}

/*=====================================================================*/

void spfw_popdownConf ( void )
/************************************************************************
 * spfw_popdownConf							*
 *									*
 * This function puts the restore confirmation down. 			*
 *									*
 * void spfw_popdownConf ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding 				*
 ***********************************************************************/
{

    if (XtIsManaged (_restoreConfW)) {
    	XtUnmanageChild (_restoreConfW);
    }
   
}

/*=====================================================================*/
/* ARGSUSED */
void spfw_rstConfCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * spfw_rstConfCb							*
 *									*
 * Callback function for the restore confirmation window. 		*
 *									*
 * void spfw_rstConfCb (wid, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data 		*
 *	call		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 ***********************************************************************/
{
    int which = (long)clnt;
/*---------------------------------------------------------------------*/

    spfw_popdownConf ();
    
    if ( which == 0 ) {	
        spfw_popdown ();
        spfw_restoreSPF ();
    }

}

/*=====================================================================*/

void spfw_getSource ( int lp, int src, int *catn, char *srcstr, int *iret )
/************************************************************************
 * spfw_getSource 							*
 *									*
 * This function gets a specified source for a given loop.		*
 *									*
 * void spfw_getSource ( lp, src, catn, srcstr, iret ) 			*
 *									*
 * Input parameters:							*
 *    lp	int		Loop number				*
 *    src	int		Source number				*
 *									*
 * Output parameters:							*
 *    *catn	int		Data category number			*
 *    *srcstr	char		source string with full path		*
 *    *iret	int		Return code				*
 *				    0 - Normal				*
 *				   -1 - Source doesn't exist in SPF file*
 *				   -2 - Source is not available 	*
 *				  -16 - Source name is too long 	*
 *									*
 **				    					*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 * J. Wu/GSC		07/01	validate the existence of data src	*
 * J. Wu/SAIC		08/01	Update error message			*
 * J. Wu/SAIC		08/01	Move error report to spfw_formatConf	*
 * S. Chiswell/UCAR	04/06	Add st_null after gd_gcyc		*
 * S. Chiswell/UCAR	04/06	Add = to > (noc2 + 12) logic		*
 * F. J. Yen/NCEP	07/08	Increased size of grdStr for storm-	*
 *				specific model and added check on size	*
 * S. Jacobs/NCEP	 4/12	Increaed the size of moslst to allow	*
 * 				for LLMXGT(1000) strings of size 20	*
 ***********************************************************************/
{
    int		ier, catgNum, ii, noc, noc1, noc2, ncc, lens, nn;
    char	tagStr[32], dataStr[200];
    char	grdStr[100], moslst[20000];
    char	catgStr[8], pathStr[256];
    char	cycTimStr[12], cycStr[] = "[cycle_time]";
                    
/*---------------------------------------------------------------------*/
        
    *iret = G_NORMAL;
        
    /*
     *  Get data string for "loopX_sourceY"
     */
    sprintf( tagStr, "%s%d%s%d", "loop", lp+1, "_source", src+1);
    spf_gtfld ( tagStr, dataStr, &ier );
    /*  
     *  If no data is present, quit.
     */					    	    
    if ( ier != 0 )  {	        
	*catn = 0;
	srcstr[0] = '\0';
	*iret = -1;	
	return;    
    }	    

    /*  If data was found, parse and set data source.  For catogories 
     *  GRD/SFF/SNF, replace the "[cycle_time]" with the latest time.
     */					    	    
    cst_nocc ( dataStr, '|', 1, 0, &noc, &ier ); 
    cst_ncpy ( catgStr, dataStr, noc, &ier );
    ctb_dcatstoi( catgStr, &catgNum, &ier );
		
    if ( catgNum == CAT_GRD || catgNum == CAT_SFF ||
	 catgNum == CAT_SNF ) {

        cst_nocc ( dataStr, '/', 1, 0, &noc1, &ier ); 
	cst_nocc ( dataStr, '/', 2, 0, &noc2, &ier );
                            
	ncc = noc2-noc1-1;
	if ( ncc > 99 ) {
	    noc2 = noc1 + 99;
	    *iret = -16;
	}
	for ( ii = noc1+1; ii < noc2; ii++ ) { 
	    grdStr[ ii-noc1-1 ] = dataStr[ ii ];	    
	}
	if  ( *iret != -16 ) {
	    grdStr[ ncc ] = '\0';
	}
	else {
	    /* Truncate length of grdStr to 50 for error message */
	    grdStr[ 50 ] = '\0';
	    er_wmsg ( "SPF", iret, grdStr, &ier, 3, strlen(grdStr) );
            NxmErr_update( );
	}

	/* 
	 *  Find the latest time string "YYMMDD/HHMM".
	 */
	gd_gcyc( grdStr, ";", &nn, moslst, &ier, strlen(grdStr), 1, sizeof(moslst));
	st_null( moslst, moslst, &lens, &ier, sizeof(moslst), sizeof(moslst));
	cst_rmbl( moslst, moslst, &lens, &ier);
		    		    
	nn = 1;
	ier = noc1 = noc2 = 0;
	while ( ier == 0 ) {
	    noc2 = noc1;
            cst_nocc ( moslst, ';', nn, 0, &noc1, &ier ); 
	    nn++;
	}

	if ( noc2 == 0 ) noc2 = -1;  /* Only one time frame */
	
	if ( isdigit( (int)moslst[noc2+1] ) && 
	     (int)strlen( moslst ) >= (noc2 + 12) ) {
	     
	    for ( ii = noc2 + 1; ii < noc2 + 12; ii++ ) { 
	        cycTimStr[ii - noc2 - 1] = moslst [ii];
	    }
	    cycTimStr[11] = '\0';

	    cst_rpst ( cycTimStr, "/", "_", cycTimStr, &ier );
	    cst_rpst( dataStr, cycStr, cycTimStr, dataStr, &ier ); 
	}
	else {
	    *iret = -2;
	}
		    		    
    }

    for ( ii = noc+1; ii < (int)strlen(dataStr); ii++ ) {
	pathStr[ ii-noc-1 ] = dataStr[ ii ];
    }

    pathStr[ii-noc-1] = '\0';
        
    /*
     * Check if the source is physically valid.
     */        
    if ( !dslw_validSrc( catgNum, pathStr ) ) {    
        *iret = -2;
    }

    *catn = catgNum;
    strcpy( srcstr, pathStr );
            
}

/*=====================================================================*/

Boolean	spfw_isUp ( void )
/************************************************************************
 * spfw_isUp      	  						*
 *									*
 * This function returns true if the spf window is currently managed.   *
 *									*
 * Boolean spfw_isUp( void ) 						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *     none								*
 *									*
 * Return:								*
 *	spfw_isUp	Boolean		True if spf window is managed	*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/01	initial coding     			*
 ***********************************************************************/
{
    return ( XtIsManaged(_fileSelW) );
}

/*=====================================================================*/

void spfw_formatConf ( long flen, char *spftext, char *filstr, int *iret )
/************************************************************************
 * spfw_formatConf	 						*
 *									*
 * This function formats the content of an SPF file into a string to be *
 * displayed in the restore confirmation window.			*
 *									*
 * void spfw_formatConf( flen, spftext, filstr, iret ) 			*
 *									*
 * Input parameters:							*
 *     flen		long	Length of spftext               	*
 *									*
 * Output parameters:							*
 *     *spftext		char	Formated string	from SPF file 		*
 *     *filstr		char	Formated string	containing file name 	*
 *     *iret		int	Return code 				*
 *				 1 - No data sources in the SPF file	*
 *				 0  - Normal				*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		07/01	initial coding				*
 * J. Wu/GSC		07/01	validate dom. src 			*
 * J. Wu/SAIC		08/01	report invalid sources 			*
 * R. Tian/SAIC		01/02	fixed bug when loading large SPF file   *
 * K. Tyle/UAlbany	08/13   increased size of tmpStr1               *
 ***********************************************************************/
{
    int		lp, src, ier, catgNum, dom, ierr;
    char	tagStr[32], dataStr[256]; 
    char	pathStr[256], spfil[MXFLSZ], errStr[256];
    char	tmpStr1[20], *tmpStr2 = NULL, *lpStr = NULL; 
    char	tabStr[] = "	", domStr[] = "     x	"; 
    Boolean	lp_hasSrc, lp_hasDom, src_exist;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    src_exist = FALSE;   
    
    /*
     *  Format the "filstr" and the header for "spftext".  
     */
    if ( strcmp (_dirPath, LOCAL_DIR ) == 0 ) {
        strcpy( spfil, _fileName );
    }
    else {	
	spfw_getFileName( spfil );
    }
        	
    strcpy( spftext, _fileName );
    strcat( spftext, " contents (x = dominant source):\n" );
    
    strcpy( filstr, "Are you sure you want to restore the settings ");
    strcat( filstr, "from SPF file\n");
    strcat( filstr, spfil );
    strcat( filstr, "?\n" );
    
    /*
     * Allocate the temp char array based on the actual SPF file size
     */
    if( !(tmpStr2 = XtMalloc((Cardinal)flen)) || !(lpStr = XtMalloc((Cardinal)flen)) ) {
        XtFree(tmpStr2); XtFree(lpStr);
        *iret = 1;
        return;    
    }
    lpStr[0] = '\0';

    /*
     *  Find data settings in the SPF file.
     */
    for ( lp = 0; lp < MAX_LOOP; lp++ ) {
        
	lp_hasSrc = FALSE;
        lp_hasDom = FALSE;
	
	sprintf( tagStr, "%s%d%s", "loop", lp+1, "_dominant" );
	spf_gtfld ( tagStr, dataStr, &ier );
        dom = atoi(dataStr);
	
	/*
	 *  Validate the dominant source first.
	 */
	if ( dom > 0 ) {
	    
	    spfw_getSource( lp, dom-1, &catgNum, pathStr, &ier );
	    if ( ier == 0 ) lp_hasDom = TRUE;	

	}
	
	sprintf( tmpStr1, "\n  %s	%d\n", "Loop", lp+1 );        	
	strcpy( tmpStr2, "\0" );
	
	for ( src = 0;  src < MAX_FRMSRC; src++ ) {
		    
	    spfw_getSource( lp, src, &catgNum, pathStr, &ier );
	    	    	    
	    if ( ier == 0 )  {
                
		if ( !lp_hasDom ) dom = src + 1;
		if ( !src_exist ) src_exist = TRUE;
	        
		lp_hasSrc = TRUE;				
                lp_hasDom = TRUE;
				
		if ( (src + 1) == dom ) {
		    strcat( tmpStr2, domStr );
		}
		else {
		    strcat( tmpStr2, tabStr );
		}
		
		strcat( tmpStr2, pathStr );
		strcat( tmpStr2, "\n" );
		
	    }	    
            else {   
	        if ( ier == -2 ) {  /*  Report invalid sources */
                    ierr = -8;
                    sprintf( errStr, "%s%d%s", " Loop ", lp+1, ":  ");
		    strcat( errStr, pathStr );
		    er_wmsg ( "SPF", &ierr, errStr, &ier, 3, strlen(errStr) );
                    NxmErr_update( );
                }
	    }
			
	}
	
        /*
         *  If any sources were found in a loop, format it.
         */    
	if ( lp_hasSrc ) {
	    strcat( lpStr, tmpStr1 );
	    strcat( lpStr, tmpStr2 );
	}	            
    
    }
    
    /*
     *  If sources exist, add to source string.
     */    
    if ( src_exist ) {	
	strcat( spftext, lpStr );		
    }
    else {
        *iret = 1;
    }
    
    /*
     * Do not forget to free memory
     */
    XtFree(tmpStr2); XtFree(lpStr);
}

/*=====================================================================*/

static void spfw_saveSrcAttr ( dsrc_t *datasrc, FILE *fptr, int lp, 
                               int srcnum )
/************************************************************************
 * spfw_saveSrcAttr     	  					*
 *									*
 * This function writes a data source's attribute settings to a file.   *
 *									*
 * static void spfw_saveSrcAttr ( datasrc, fptr, lp, srcnum )		*
 *									*
 * Input parameters:							*
 *     *datasrc		dsrc_t		Pointer to a data source	*
 *     *fptr		FILE		Pointer to an SPF file		*
 *     lp		int		Loop number			*
 *     srcnum		int		Source seq. number in a loop	*
 *									*
 * Output parameters:							*
 *     none								*
 *									*
 * Return:								*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/02	initial coding     			*
 * J. Wu/SAIC		05/02	break attr. str. to multiple lines	*
 * H. Zeng/EAI          10/02   added CAT_IMG into switch construct     *
 * R. Tian/SAIC		03/03	added CAT_SNF in dataw_getStnmName call	*
 * M. Li/SAIC		04/03	Added the second colors for MISC	*
 * S. Jacobs/NCEP	 6/03	Increased size of flgStr from 64 to 256	*
 * F. J. Yen/NCEP	 6/04	Added test for change in types[ii].arrw.*
 *				ityp.  Fixed test for arrw.hdsz (-9999).*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * S. Gilbert/NCEP	 5/06	Increased size of types and flags       *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to ctb_dtget CSC*
 * S. Jacobs/NCEP	 9/13	Increased string sizes 512 to 2046	*
 ***********************************************************************/
{
    int		isbcat, isub, d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct,
	        dionoff, dtmch;
    int		ntype, nflag, ii, ier;
    int		attr_save[4] = { -1, -1, -1, -1 };
    char	parm[123], colors[123], vcoord[73], filnam[81];
    char	level[73], text[40], filter[10], alias[81], cycle[123];
    char	typestr[20], d1[256], d2[256];
    char	imtype[81], iminfo[81], imlutf[81];
    char	tagStr[32], attrStr[2046], tmpStr[32], newtagStr[32];
    char	typStr[2046], linStr[2046], sym1Str[2046], sym2Str[2046];
    char	arrwStr[2046], flgStr[256], fmtStr[] = "\n\t\t\t\t";
    char	fmt1[] = "%s|\n\t\t\t\t%s|\n\t\t\t\t%s|\n\t\t\t\t%s|\n\t\t\t\t%s|\n\t\t\t\t%s",
    		fmt2[] = "%s|\n\t\t\t\t%s|\n\t\t\t\t%s|\n\t\t\t\t%s";
    NMS_types	types[25];
    NMS_flags	flags[25];

/*---------------------------------------------------------------------*/
        
    tagStr[0]  = '\0';
    attrStr[0] = '\0';
    typStr[0]  = '\0';
    linStr[0]  = '\0';
    sym1Str[0] = '\0'; 
    sym2Str[0] = '\0'; 
    arrwStr[0] = '\0';
    flgStr[0]  = '\0'; 
    
    sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1, "_source", srcnum, "_attr" ); 
    
    switch ( datasrc->catg ) {

        case CAT_IMG:

            sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1, "_source", srcnum, 
                             "_lut" ); 
	    nim_qatt (datasrc->attridx, imtype, iminfo, imlutf, &ier); 
            spf_write( fptr, tagStr, imlutf, &ier );
  
            break;

        case CAT_SFC:
        case CAT_SFF:
        case CAT_SND:
        case CAT_SNF:

            dataw_getStnmName ( datasrc->path, 
                (Boolean)(datasrc->catg == CAT_SFF || datasrc->catg == CAT_SNF), 
            	typestr, alias );
			 	                     
            ctb_dtget ( typestr, d1, d2, &d3, &isub, &d4, &d5, &d6, 
			&dionoff, &d7, &d7m, &d8, &d8m, &mstrct,
			&dtmch, &ier );
 
	    if ( isub == SCAT_SND || isub == SCAT_SNF ) {
                nsn_qatt ( datasrc->attridx, alias, &isbcat, cycle, parm, 
                           colors, level, vcoord, filter, text, &ier);
		sprintf( attrStr, fmt1,
	                 parm, colors, level, vcoord, filter, text ); 
            }
            else {
                nsf_qatt ( datasrc->attridx, alias, &isbcat, cycle, parm, 
                           colors, filter, text, &ier);
	        sprintf( attrStr, fmt2, parm, colors, filter, text ); 
            }
            	    	    
            spf_write( fptr, tagStr, attrStr, &ier );
	    	    
	  break;

        case CAT_VGF:
        case CAT_MSC:
            nms_qatt ( datasrc->attridx, alias, &isbcat, filnam,
                       &ntype, types, &nflag, flags, &ier );

	    sprintf( attrStr, "%s|%d|%d", filnam, ntype, nflag ); 
            spf_write( fptr, tagStr, attrStr, &ier );
	    
	    /*
	     *  Save the basic NMS_types attribute.
	     */	     
	    strcpy ( newtagStr, tagStr );
 	    strcat ( newtagStr, "_type" );
	    
	    for ( ii = 0; ii < ntype; ii++ ) {
		sprintf ( tmpStr, "%d|%d|%d|%.2f|",
		          types[ii].ionoff,
		          types[ii].icolr,
			  types[ii].icolr2,
			  types[ii].value );
	        if ( ii < (ntype - 1) ) strcat ( tmpStr, fmtStr );
	        strcat ( typStr, tmpStr );
	    }
            
	    spf_write( fptr, newtagStr, typStr, &ier );	    
	    
	    
	    /*
	     *  Check if the line/symb/arrw attr. have been changed.
	     */	     
	    for ( ii = 0; ii < ntype; ii++ ) {
                if ( types[ii].line.size >= 0.0F ||
		     types[ii].line.iwid >= 0 ) {
		    attr_save[0] = ii;
		}	    	    
                
		if ( types[ii].symb[0].code >= 0.0F || 
		     types[ii].symb[0].size >= 0.0F ||
		     types[ii].symb[0].iwid >= 0 ) {
		    attr_save[1] = ii;
		}	    
	    
                if ( types[ii].symb[1].code >= 0.0F ||
		     types[ii].symb[1].size >= 0.0F ||
		     types[ii].symb[1].iwid >= 0 ) {
		    attr_save[2] = ii;
		}	    

                if ( types[ii].arrw.size >= 0.0F ||
                     !G_DIFF( types[ii].arrw.hdsz, (float)-1.0 ) ||
                     types[ii].arrw.iwid >= 0    ||
                     types[ii].arrw.ityp >= 0 ) {
		    attr_save[3] = ii;
		}	    
	    }
		
	    /*
	     *  Save the line/symb/arrw attr. if necessary.
	     */	     
            if ( attr_save[0] >= 0  ) {

	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_line" );

	        for ( ii = 0; ii <= attr_save[0]; ii++ ) {
		    sprintf ( tmpStr, "%.2f|%d|",
		              types[ii].line.size, 
			      types[ii].line.iwid );	    
	            if ( ii < attr_save[0] ) strcat ( tmpStr, fmtStr );
		    strcat ( linStr, tmpStr );
		}

                spf_write( fptr, newtagStr, "!", &ier );	    
                spf_write( fptr, newtagStr, linStr, &ier );	    
	    }
	    
            if ( attr_save[1] >= 0 ) {

	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_sym1" );

	        for ( ii = 0; ii <= attr_save[1]; ii++ ) {
	            sprintf ( tmpStr, "%.2f|%.2f|%d|",
		              types[ii].symb[0].code,
		              types[ii].symb[0].size, 
			      types[ii].symb[0].iwid );
	            if ( ii < attr_save[1] ) strcat ( tmpStr, fmtStr );
	            strcat ( sym1Str, tmpStr );
	        }

                spf_write( fptr, newtagStr, "!", &ier );	    
                spf_write( fptr, newtagStr, sym1Str, &ier );	    
	    }

            if ( attr_save[2] >= 0 ) {

	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_sym2" );

	        for ( ii = 0; ii <= attr_save[2]; ii++ ) {
		    sprintf ( tmpStr, "%.2f|%.2f|%d|", 
		              types[ii].symb[1].code,
		              types[ii].symb[1].size,
			      types[ii].symb[1].iwid );
	            if ( ii < attr_save[2] ) strcat ( tmpStr, fmtStr );
	            strcat ( sym2Str, tmpStr );
	        }

                spf_write( fptr, newtagStr, "!", &ier );	    
                spf_write( fptr, newtagStr, sym2Str, &ier );	    
	    }
	    
            if ( attr_save[3] >= 0 ) {

	        strcpy ( newtagStr, tagStr );
	        strcat ( newtagStr, "_arrw" );

	        for ( ii = 0; ii <= attr_save[3]; ii++ ) {
		    sprintf ( tmpStr, "%.2f|%.2f|%d|%d|",
		              types[ii].arrw.size,
		              types[ii].arrw.hdsz,
			      types[ii].arrw.iwid,
                              types[ii].arrw.ityp );
	            if ( ii < attr_save[3] ) strcat ( tmpStr, fmtStr );
	            strcat ( arrwStr, tmpStr );
	        }

                spf_write( fptr, newtagStr, "!", &ier );	    
                spf_write( fptr, newtagStr, arrwStr, &ier );	    
	    }
	    
	    /*
	     *  Save the flag attribute.
	     */	     
	    strcpy ( newtagStr, tagStr );
	    strcat ( newtagStr, "_flag" );

	    for ( ii = 0; ii < nflag; ii++ ) {
	        sprintf ( tmpStr, "%d|", flags[ii].iflg ); 
	        if ( ii < ( nflag -1 ) ) strcat ( tmpStr, fmtStr );
	        strcat ( flgStr, tmpStr );	        
	    }
            
	    spf_write( fptr, newtagStr, "!", &ier );	    
	    spf_write( fptr, newtagStr, flgStr, &ier );	    
	    spf_write( fptr, newtagStr, "!", &ier );	    
	    spf_write( fptr, newtagStr, "!", &ier );	    
	    	    	    
          break;
	    
    }  /* end of switch */
    
}

/*=====================================================================*/

void spfw_getRangeIntv ( int lp, int src, int *rng, int *intv, int *drt,
			 int *iret )
/************************************************************************
 * spfw_getRangeIntv 							*
 *									*
 * This function gets a specified source's timeline range/interval and	*
 * delta reference time.						*
 *									*
 * void spfw_getRangeIntv ( lp, src, rng, intv, drt, iret ) 		*
 *									*
 * Input parameters:							*
 *    lp	int		Loop number				*
 *    src	int		Source seq. number in a loop		*
 *									*
 * Output parameters:							*
 *    *rng	int		Source's timeline range			*
 *    *intv	int		Source's timeline interval		*
 *    *drt	int		Source's delta reference time		*
 *    *iret	int		Return code				*
 *				    0 - Normal				*
 **				    					*
 * Log:									*
 * J. Wu/SAIC		07/03	initial coding				*
 * T. Lee/SAIC		04/04	added drt to calling seq.		*
 ***********************************************************************/
{
    int		range, interval, delrt, ier;
    char	tagStr[32], dataStr[200];                    
/*---------------------------------------------------------------------*/
        
    *iret = G_NORMAL;
    *rng  = -1;
    *intv = -1;
    *drt  = -1;
            
    /*
     *  Retrieve range/interval values saved in the SP file.
     */
    sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1,
                                   "_source", src, "_interval" );
    spf_gtfld ( tagStr, dataStr, &ier );    
    interval = atoi ( dataStr );
    if ( interval > 0 ) *intv = interval;
    
    sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1,
                                   "_source", src, "_range" );
    spf_gtfld ( tagStr, dataStr, &ier );    
    range = atoi ( dataStr );
    if ( range > 0 ) *rng = range;
                
    sprintf( tagStr, "%s%d%s", "loop", lp+1,
                                   "_delta_rt" );
    spf_gtfld ( tagStr, dataStr, &ier );    
    delrt = atoi ( dataStr );
    if ( delrt > 0 ) *drt = delrt;
}
/*=====================================================================*/

void spfw_getTmBin ( int lp, int src, int *ionoff, int *ibfr, int *mnbfr,
		     int *iaftr, int *mnaftr, int *mstrct, int *iret )
/************************************************************************
 * spfw_getTmBin							*
 *									*
 * This function gets a specified source's bin hours			*
 *									*
 * void spfw_getTmBin ( lp, src, ionoff, ibfr, mnbfr, iaftr, mnaftr,	*
 *		        mstrct, iret )					*
 *									*
 * Input parameters:							*
 *    lp	int		Loop number				*
 *    src	int		Source seq. number in a loop		*
 *									*
 * Output parameters:							*
 *    *ionoff	int		Binning on/off flag			*
 *    *ibfr	int		Source's bin hrs before current time	*
 *    *mnbfr	int		Source's bin minutes before current time*
 *    *iaftr	int		Source's bin hrs after current time	*
 *    *mnaftr	int		Source's bin minutes after current time	*
 *    *mstrct	int		Most recent only flag			*
 *    *iret	int		Return code				*
 *				    0 - Normal				*
 **				    					*
 * Log:									*
 * T. Lee/SAIC		10/04						*
 * T. Piper/SAIC	01/05	Added ionoff parameter			*
 * A. Hardy/NCEP	02/05	changed check for neg. ibfr		*
 * F. J. Yen/NCEP	 4/08	Added bin minutes& most recent only flag*
 ***********************************************************************/
{
    int		ier;
    char	tagStr[32], dataStr[200];                    
/*---------------------------------------------------------------------*/
        
    *ionoff = 0;
    *iret  = G_NORMAL;
    *ibfr  = -99;
    *mnbfr  = -99;
    *iaftr = -99;
    *mnaftr = -99;
    *mstrct = 0;
            
    /*
     *  Retrieve range/interval values saved in the SP file.
     */
    sprintf( tagStr, "%s%d%s%d%s", "loop", lp+1,
                                   "_source", src, "_timbin" );
    spf_gtfld ( tagStr, dataStr, &ier );    
    ctb_dhrsstoi ( dataStr, ionoff, ibfr, mnbfr, iaftr, mnaftr,
		   mstrct, &ier );
    if ( *ibfr < 0 ) *ibfr = 0;
    if ( *mnbfr < 0 ) {
	*mnbfr = 0;
    } else if ( *mnbfr > 59 ) {
	*mnbfr = 59;
    }
    if ( *iaftr < 0 ) *iaftr = 0;
    if ( *mnaftr < 0 ) {
	*mnaftr = 0;
    } else if ( *mnaftr > 59 ) {
	*mnaftr = 59;
    }
}
/*=====================================================================*/

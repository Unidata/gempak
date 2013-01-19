#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "vgftbl.h"
#include "hints.h"
#include "proto_xw.h"

#define VISIBLE_ITEM	  10 
#define MAX_FILES	1000
#define SORT_BY_NAME	   0
#define SORT_BY_DATE	   1

#define OPEN_POPUP	   0
#define SAVE_POPUP	   1
#define BROWSE_POPUP	   2

#define FILE_PATTERN	"*.vgf"
#define LOCAL_DIR	"./"

#define BAD_NUMBER_OF_POINTS ( -28 )

typedef XmFileSelectionBoxCallbackStruct FSBCBS;

static Widget	_fileSelW;
static Widget	_dirSel_listW;
static Widget	_fileSel_listW;
static Widget	_fileSel_txtW;

static Widget	_browsePopup;
static Widget	_sortRadioW;
static WidgetList _sortBtn;
static WidgetList _autoSaveBtn;

static Widget	_sortPaneW;
static Widget	_selectPaneW;
static Widget	_currPaneW;
static Widget	_inputPaneW;
static Widget	_browseBtnPane;
static Widget	_autosvPaneW;
static Widget	_openPaneW;
static Widget	_savePaneW;

static char	_origName[MXFLSZ]	= "\0";
static char	_origPath[LLPATH]	= LOCAL_DIR;
static char	_currPath[LLPATH];
static char	_fzlRange[ STD_STRLEN ];	/* freezinlg level ranges */

static int	_pgfilFunc;	/* FUNC_SAVE_VGF,  FUNC_OPEN_VGF */
static int	_sortBy;	/* SORT_BY_NAME or SORT_BY_DATE  */

static int	_origDirPos;
static int	_currDirPos;

static Boolean	_autoSaveIsOn;
static Boolean	_clearFlag		= TRUE;

static CVG_mtrx_t	*_matrix 	= NULL;
static int		_matrixSize	= 0;

static Boolean		_saveAll	= FALSE;
static int		_saveLayer	= 0;
static int		_origLayer	= 0;
static Boolean		_resetActv      = FALSE;

static XtIntervalId	_timeOutId = (XtIntervalId)NULL;
extern XtAppContext	_appContext;


/*
 *  private callback functions
 */
static void pgfilw_autoSaveCb	( Widget, long, XtPointer );
static void pgfilw_autoSaveTO	( XtPointer clnt, XtIntervalId id );
static void pgfilw_browseBtnCb	( Widget, XtPointer, XtPointer);
static void pgfilw_browseDoneCb ( Widget, XtPointer, XtPointer );
static void pgfilw_openCtlBtnCb ( Widget, long, XtPointer );
static void pgfilw_saveCancelCb ( Widget, XtPointer, XtPointer );
static void pgfilw_saveCtlBtnCb ( Widget, long, XtPointer );  
static void pgfilw_saveAcceptCb ( Widget, XtPointer, XtPointer );
static void pgfilw_selectCb	( Widget, XtPointer, XtPointer );
static void pgfilw_selectDirCb	( Widget, XtPointer, XtPointer );
static void pgfilw_sortByCb	( Widget, XtPointer, XtPointer );
static void pgfilw_txtCb	( Widget, XtPointer, XtPointer );
static void pgfilw_rangeWarningBtnCb	( Widget, XtPointer, XtPointer );
static void pgfilw_fzlToggleBtnCb	( Widget, XtPointer, XtPointer );
static void pgfilw_fzlRangeChgCb	( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
static void pgfilw_checkPerms ( char *cfile, Boolean *can_read, 
                                Boolean *can_write );
static void pgfilw_managePanes ( int popup_type);
static void pgfilw_saveCheck ( void );
static void pgfilw_saveVGF ( void );  
static void pgfilw_setFileList ( void );
static void pgfilw_startAutoSave ( void );
static void pgfilw_stopAutoSave ( void );
static void pgfilw_unmanagePanes ( void );
static void pgfilw_getBackupName ( char *ifil, char *bkfil, int *iret );
static void pgfilw_renumberGrp   ( char grptyp, int grpin, int *grpout );
static void pgfilw_saveAll ( void );
static Boolean pgfilw_cmpFzlRanges( char file1[], char file2[],
				    char range1[], char range2[] );
static void pgfilw_showFzlRangeWarning( Widget parent, char infile[],
					char range1[], char range2[] );
static void pgfilw_appendFzl( void );
static void pgfilw_setFzlRanges( void );

/************************************************************************
 * nmap_pgfilw.c							*
 *									*
 * This module defines the file selection (OPEN/SAVE) in product	*
 * generation.								*
 *									*
 * CONTENTS:								*
 *  pgfilw_create()		creates palette				*
 *  pgfilw_popup()		popup the palette window		*
 *  pgfilw_popdown()		put down the palette window		*
 *  pgfilw_showFileName()	show file name on the proper widget	*
 *  pgfilw_clearFileName()	reset the active file name to null	*
 *  pgfilw_setFileName()        set the active file name		*
 *									*
 *  pgfilw_getFileName()	return the active file name		*
 *  pgfilw_getPathName()	return the open path name		*
 *  pgfilw_isFileSaved()	querry whether there is saved file	*
 *  pgfilw_clearSaveFlag()      clears _fileIsSaved flag                *
 *  pgfilw_isUp()		returns status of the window		*
 *  pgfilw_save()		order file save and updt tracking info	*
 *  pgfilw_startSaveAll()	order file save for all layers   	*
 *									*
 *  pgfilw_selectCb()		callback for file list			*
 *  pgfilw_selectDirCb()	callback for directory list		*
 *  pgfilw_txtCb()		callback for text input			*
 *  pgfilw_openCtlBtnCb()	callback for open control buttons	*
 *  pgfilw_saveCtlBtnCb()	callback for save control buttons	*
 *  pgfilw_saveAcceptCb()	callback for SAVE under confirmation	*
 *  pgfilw_saveCancelCb()	callback to cancel SAVE confirmation	*
 *  pgfilw_browseBtnCb()	callback for the browse button		*
 *  pgfilw_browseDoneCb()	callback for browse OK/Cancel buttons	*
 *  pgfilw_sortByCb() 		callback for sort by toggles		*
 *  pgfilw_autoSaveCb()         callback for auto save toggles		*
 *  pgfilw_autoSaveTO()		time out function for auto save		*
 *									*
 *  pgfilw_openVGF()		open the specified VG file		*
 *  pgfilw_saveCheck()		check the file before saving		*
 *  pgfilw_saveVGF()		save the specified VG file		*
 *  pgfilw_saveAll()		perform a save by layer for all layers  *
 *  pgfilw_checkPerms()		checks file permissions			*
 *  pgfilw_startAutoSave()	starts the time out func for auto save	*
 *  pgfilw_stopAutoSave()	stops  the time out func for auto save	*
 *  pgfilw_managePanes()	manages the appropiate children of pane	*
 *  pgfilw_unmanagePanes()	unmanages all the children of pane	*
 *  pgfilw_setFileList()	sets the file list for the _currPath	*
 *  pgfilw_getBackupName ()	gets the BACKUP file name 		*
 ***********************************************************************/

/*=====================================================================*/

void pgfilw_create ( Widget parent )
/************************************************************************
 * pgfilw_create							*
 *									*
 * This function creates a file selection dialog box.			*
 *									*
 * void pgfilw_create ( parent )					*
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
 * D. Keiser/GSC	 8/97						*
 * C. Lin/EAI	 	10/97	rename from NxmFileSelCr, cleanup	*
 * C. Lin/EAI	 	12/97   redesign, more cleanup			*
 * E. Safford/GSC       01/98   add file append functionality		*
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * S. Law/GSC		06/98	added browse function			*
 * E. Safford		02/99	change radio btns to push btns		*
 * E. Safford		03/99	change _sortBy default         		*
 * H. Zeng/EAI          11/99   added auto save toggles                 *
 * S. Law/GSC		01/00	added directory list			*
 * S. Jacobs/NCEP	 3/00	Renamed header file nmap_vtbl to vgftbl	*
 * S. Law/GSC		05/00	made the panes global and cleanup	*
 * E. Safford/GSC	06/00	re-merge save & open directories 	*
 * J. Wu/SAIC		02/02	add layering			 	*
 * T. Piper/SAIC	02/02	removed vtbl_readUsrTbl - now global	*
 * J. Wu/SAIC		03/02	make _fileSelW win. as APPLICATION_MODAL*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		loff = 20, toff = 5, cur_layer;
    long	ii, nn;
    char	*save_btnstr[] = {"OK", "Cancel"}; 
    char	*open_btnstr[] = {"Replace", "Append", "Cancel"};
    char	*sort_str[] = {"Name", "Date"};
    Widget	pane, label, label2, frame, sort_label;
    Widget      save_label, save_radio;
    Arg         args[10];
    Cardinal    argcnt;
    XmString    xmpattern; 
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    _fileSelW = XmCreateFormDialog( parent, "Save as", NULL, 0 );
    XtVaSetValues(_fileSelW,
		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		NULL);

    cur_layer = pglayer_getCurLayer();

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgfilw_pane",
                    xmPanedWindowWidgetClass, _fileSelW,
                    XmNsashWidth,             1,
                    XmNsashHeight,            1,
                    NULL);

    /*
     * ******** SORT PANE ********
     * create a form widget to hold the sort by toggles
     */
    _sortPaneW = XtVaCreateWidget("pgfilw_form",
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
	            pgfilw_sortByCb, (XtPointer) ii);

	XmToggleButtonSetState (_sortBtn[ii], (_sortBy == ii), FALSE);
    }

    XtManageChild(_sortPaneW); 


    /*
     * ******** SELECT PANE ********
     * create a form widget to hold the directory and file lists
     */
    _selectPaneW = XtVaCreateWidget("pgfilw_selectform",
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
		   pgfilw_selectDirCb, NULL );


    if (_vgfUsrTbl.nitems > 0) {

        xmfils = (XmStringTable) XtMalloc 
	    ((size_t)_vgfUsrTbl.nitems * sizeof (XmString *));

        for (ii = 0; ii < _vgfUsrTbl.nitems; ii++) {
	    xmfils[ii] = XmStringCreateLocalized (_vgfUsrTbl.items[ii].title);
        }

        XtVaSetValues (_dirSel_listW,
		       XmNitems,	xmfils,
		       XmNitemCount,	_vgfUsrTbl.nitems,
		       NULL);

        for (ii = 0; ii < _vgfUsrTbl.nitems; ii++) {
	    XmStringFree (xmfils[ii]);
        }
	XtFree ((XtPointer) xmfils);

	/*
	 * hi-light the local directory
	 */
	pglayer_setDirPos ( cur_layer, _vgfUsrTbl.nitems );
    }

    XtManageChild( _dirSel_listW );
    XtManageChild( frame );

    /*
     * create the file list
     */
    label = XmCreateLabel( _selectPaneW, "Select VG File name:", NULL, 0 );
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
		   pgfilw_selectCb, NULL);


    XtManageChild (_fileSel_listW);
    XtManageChild (frame);

    XtManageChild(_selectPaneW);

    /*
     * ******** CURRENT PANE ********
     */
    _currPaneW = 
	XtVaCreateManagedWidget ("pgfilw_currfile",
				 xmLabelWidgetClass,	pane,
				 NULL);

    /*
     * ******** INPUT PANE ********
     * create the file input
     */
    _inputPaneW = XtVaCreateWidget("pgfilw_inputForm",
				   xmFormWidgetClass, 	pane,
				   NULL);
    XtVaSetValues( _inputPaneW,
		    XmNnoResize,     TRUE,
		    XmNautoUnmanage, FALSE,
		    NULL );

    label2 = XmCreateLabel (_inputPaneW, "Or enter a VG file name:", NULL, 0);
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

    _fileSel_txtW = XmCreateText (_inputPaneW, "pgfilw_inputText", NULL, 0);
    XtManageChild (_fileSel_txtW);
    XtVaSetValues (_fileSel_txtW,
		   XmNtopAttachment,   XmATTACH_WIDGET,
		   XmNtopWidget,       label2,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNleftAttachment,  XmATTACH_FORM,
		   NULL);
    XtAddCallback (_fileSel_txtW, XmNactivateCallback,
		   pgfilw_txtCb, NULL );

    XtManageChild (_inputPaneW);


    /*
     * ******** BROWSE PANE ********
     */
    _browseBtnPane = 
	XtVaCreateManagedWidget ("Browse",
				 xmPushButtonWidgetClass,	pane,
				 NULL);
    XtAddCallback(_browseBtnPane, XmNarmCallback,
		  pgfilw_browseBtnCb, (XtPointer) NULL); 



    /*
     * ******** AUTO SAVE PANE ********
     */
    _autosvPaneW =     
        XtVaCreateWidget("autoSave_form",
                          xmFormWidgetClass, 	  pane,
		          XmNnoResize,            TRUE,
                          NULL );

    save_label = 
        XtVaCreateManagedWidget("Auto Save:",
                                xmLabelWidgetClass,   _autosvPaneW,
                                XmNleftAttachment,    XmATTACH_FORM,
                                XmNleftOffset,        5,
                                XmNtopAttachment,     XmATTACH_FORM,
                                NULL );

    save_radio = 
        XtVaCreateManagedWidget ("autoSave_radio",
		    xmRowColumnWidgetClass,   _autosvPaneW,
		    XmNorientation,           XmHORIZONTAL,
		    XmNradioBehavior,         TRUE,
		    XmNtopAttachment,         XmATTACH_FORM,
		    XmNleftAttachment,        XmATTACH_WIDGET,
                    XmNleftWidget,            save_label,
                    XmNleftOffset,            loff,
                    NULL);

    _autoSaveBtn = (WidgetList) XtMalloc (2 * sizeof (Widget));

    _autoSaveBtn[0] = 
	XtVaCreateManagedWidget ("Off",
				 xmToggleButtonWidgetClass, save_radio,
				 XmNtraversalOn,	FALSE,
				 XmNset,		TRUE,
				 NULL);

    XtAddCallback(_autoSaveBtn[0], XmNarmCallback, 
		  (XtCallbackProc)pgfilw_autoSaveCb, (XtPointer) FALSE);

    _autoSaveBtn[1] = 
	XtVaCreateManagedWidget ("On",
				 xmToggleButtonWidgetClass, save_radio,
				 XmNtraversalOn,	FALSE,
				 XmNset,		FALSE,
				 NULL);

    XtAddCallback (_autoSaveBtn[1], XmNarmCallback, 
		   (XtCallbackProc)pgfilw_autoSaveCb, (XtPointer) TRUE);

    _autoSaveIsOn = FALSE;
    XtManageChild(_autosvPaneW); 


    /*
     * ******** SAVE CONTROL PANE ********
     */
    _savePaneW = 
	(Widget) NxmCtlBtn_create (pane, 1, "pgfilw_save", 
				   XtNumber(save_btnstr), save_btnstr, 
				   (XtCallbackProc)pgfilw_saveCtlBtnCb, NULL);

    /*
     * ******** OPEN CONTROL PANE ********
     */
    _openPaneW = 
	(Widget) NxmCtlBtn_create (pane, 1, "pgfilw_open", 
				   XtNumber(open_btnstr), open_btnstr, 
				   (XtCallbackProc)pgfilw_openCtlBtnCb, NULL); 


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
		  pgfilw_browseDoneCb, (XtPointer) 0);
    XtAddCallback(_browsePopup, XmNokCallback,
		  pgfilw_browseDoneCb, (XtPointer) 1);

    XtUnmanageChild(XmFileSelectionBoxGetChild(_browsePopup, 
					       XmDIALOG_HELP_BUTTON));

}

/*=====================================================================*/

void pgfilw_popup ( int func )
/************************************************************************
 * pgfilw_popup								*
 *									*
 * This function loads and displays the VG files into the file		*
 * selection dialog box.						*
 *									*
 * void pgfilw_popup ( func )						*
 *									*
 * Input parameters:							*
 *	func		int		Action function			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 8/97	Copied from NxmFileShow			*
 * C. Lin/EAI	 	10/97	rename from NxmFileSelSh, re-design	*
 * C. Lin/EAI	 	12/97	more cleanup, modify for new design	*
 * E. Safford/GSC       01/98   add file append functionality		*
 * E. Safford/GSC	03/98	make Undo insensitive when popup is up  *
 * S. Law/GSC		06/98	reset browse directory to '.' for save	*
 * E. Safford/GSC	09/98	change undo toggle			*
 * E. Safford/GSC	02/99   change default to append		*
 * E. Safford/GSC	02/99   change default back to replace		*
 * E. Safford/GSC	02/99   make append & replace buttons 		*
 * S. Law/GSC		01/00	replaced cfl_rdir and sort with cfl_gfil*
 * S. Law/GSC		05/00	moved file setup to pgfilw_setFileList	*
 * E. Safford/GSC	06/00	re-merge open and save directories    	*
 * H. Zeng/EAI          08/00   changed XmListSelectPos() para.         *
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * J. Wu/SAIC		02/02	add layering			 	*
 * J. Wu/SAIC		04/02	attempt SAVE in home directory	 	*
 * E. Safford/SAIC	06/02	set save to _currPath && _currDirPos    *
 ***********************************************************************/
{
    int		ipos, cur_layer;
    char	file_name[MXFLSZ];
    XmString	xmstr, dirstr;
/*---------------------------------------------------------------------*/

    _pgfilFunc = func;
    cur_layer = pglayer_getCurLayer();
    
    /*
     *  Preserve file name/path/pos for later recovery.
     */
    pglayer_getFileName ( cur_layer, _origName );
    pglayer_getFilePath ( cur_layer, _origPath );
    pglayer_getDirPos ( cur_layer, &_origDirPos );
    
    /*
     * set the dialog title 
     */
    if ( func == FUNC_SAVE_VGF ) {
	xmstr = XmStringCreateLocalized("Save VG File as");
	dirstr = XmStringCreateLocalized(LOCAL_DIR);
	XtVaSetValues(_browsePopup, XmNdirectory, dirstr, NULL);
	XmStringFree (dirstr);

	pglayer_setFilePath ( cur_layer, _currPath );
        pglayer_setDirPos ( cur_layer, _currDirPos );
	
	pgfilw_managePanes (SAVE_POPUP);
    }
    else {
	xmstr = XmStringCreateLocalized("Open VG File");

	pgfilw_managePanes (OPEN_POPUP);
    }

    pglayer_getFilePath ( cur_layer, _currPath );    
    pglayer_getDirPos ( cur_layer, &_currDirPos );

    XtVaSetValues (_fileSelW, XmNdialogTitle, xmstr, NULL);
    XtVaSetValues (_browsePopup, XmNdialogTitle, xmstr, NULL);
    XmStringFree (xmstr);

    /*
     * hi-light the current directory
     */
    ipos = _currDirPos;
    XmListSelectPos (_dirSel_listW, ipos, TRUE);

    ipos += (VISIBLE_ITEM / 2);
    if (ipos < VISIBLE_ITEM) ipos = VISIBLE_ITEM;
    if (ipos > _vgfUsrTbl.nitems) ipos = _vgfUsrTbl.nitems;

    XmListSetBottomPos (_dirSel_listW, ipos);

    /*
     * set input text widget
     */
    pglayer_getFileName ( cur_layer, file_name );
    XmTextSetString( _fileSel_txtW, file_name );


    pgfilw_setFileList ();

    XtManageChild(_fileSelW);
}

/*=====================================================================*/

void pgfilw_popdown ( void )
/************************************************************************
 * pgfilw_popdown							*
 *									*
 * This function puts the file window down. 				*
 *									*
 * void pgfilw_popdown ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	 	12/97						*
 * E. Safford/GSC	02/99	add unmanage of open/save controls	*
 * S. Law/GSC		05/00	change to just do main popups		*
 ***********************************************************************/
{
    if (XtIsManaged (_fileSelW)) {
    	XtUnmanageChild (_fileSelW);
    }

    if (XtIsManaged (_browsePopup)) {
    	XtUnmanageChild (_browsePopup);
    }
}

/*=====================================================================*/

void pgfilw_showFileName ( void )
/************************************************************************
 * pgfilw_showFileName							*
 *									*
 * This function shows the current file name on the bottom of the main  *
 * window if any. 							*
 *									*
 * void pgfilw_showFileName ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	 	12/97						*
 * E. Safford/GSC	01/98	modified to use _pgsvdName		*
 * E. Safford/GSC	10/98	mod to always display "VGF: "		*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Jacobs/NCEP	 3/00	Use only the filename part for display	*
 * S. Law/GSC		05/00	changed to use _fileName		*
 * S. Law/GSC		05/00	removed "VGF:" modifier			*
 * J. Wu/SAIC		02/02	remove global "_fileName"	 	*
 * M. Li/SAIC           12/06   Added directory name                    *
 * M. Li/SAIC           01/07   Only keep the lowest level subdirectory *
 ***********************************************************************/
{
    char        file_name[MXFLSZ], tmpfile[MXFLSZ];
    int 	ii, snum;
/*---------------------------------------------------------------------*/

    pgfilw_getFileName ( True, file_name );

   /*
    * Do not display LOCAL_DIR("./") if no vg file presents.
    */
    if ( strcmp(file_name, LOCAL_DIR) == 0 ) {
        file_name[0] = '\0';
    }

    snum = 0;
    for ( ii = strlen(file_name); ii >= 0; ii-- ) {
        if ( file_name[ii] == '/' ) snum++;
	if ( snum == 2 ) {
	    strcpy(tmpfile, file_name + ii + 1);
	    break;
	}
    }

    if ( snum != 2 ) strcpy(tmpfile, file_name);
  	 
    mbotw_pgfileSet ( tmpfile );
}

/*=====================================================================*/

void pgfilw_clearFileName ( void )
/************************************************************************
 * pgfilw_clearFileName							*
 *									*
 * This function clears the current file name.                          *
 *									*
 * void pgfilw_clearFileName ( )					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	01/98	initial coding				*
 * S. Law/GSC		05/00	changed to use _fileName		*
 * E. Safford/GSC	06/00	re-merge open and save paths		*
 * S. Law/GSC		07/00	reset _dirPos				*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    int		cur_layer;
/*---------------------------------------------------------------------*/
    
    cur_layer = pglayer_getCurLayer();
    
    if (_clearFlag) {
	_origName[0] = '\0';
	pglayer_setFileName ( cur_layer, "\0" ); 
	pglayer_setFilePath ( cur_layer, LOCAL_DIR ); 
        pglayer_setDirPos ( cur_layer, _vgfUsrTbl.nitems );
    }
}

/*=====================================================================*/

void pgfilw_setFileName ( int func_flag, char *file_name )
/************************************************************************
 * pgfilw_setFileName							*
 *									*
 * This function sets the current file name and path.			*
 *									*
 * void pgfilw_setFileName (func_flag, file_name)			*
 *									*
 * Input parameters:							*
 *	func_flag	int	FUNC_OPEN_VGF or FUNC_SAVE_VGF		*
 *	*file_name	char	new file name and path			*
 *									*
 * Output parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	10/98	initial coding				*
 * S. Law/GSC		05/00	rewrote					*
 * E. Safford/GSC	06/00	re-merge open and save paths 		*
 * J. Wu/SAIC		02/02	add layering			 	*
 * E. Safford/SAIC	04/02	use cfl_isdir instead of local version	*
 ***********************************************************************/
{
    int		cur_layer;
    char	*cptr, path[LLPATH];
    char	fullname[FILE_FULLSZ], cur_file[MXFLSZ];
    Boolean	is_dir;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
    pglayer_getFilePath ( cur_layer, path );

    is_dir = cfl_isdir( file_name );

    /*
     * if no path is given, use local directory
     */
    if ((cptr = strrchr (file_name, '/')) == (char *) NULL) {
	pglayer_setFileName ( cur_layer, file_name );
	strcpy (path, "./");
    }
    else {
	cptr++;
	pglayer_setFileName ( cur_layer, cptr );
	cptr[0] = '\0';
	strcpy (path, file_name);
    }

    if (is_dir) {
	pglayer_getFileName ( cur_layer, cur_file );
	strcat ( path, cur_file );
	pglayer_setFileName ( cur_layer, "\0" );
    }

    pglayer_setFilePath ( cur_layer, path );
    strcpy (fullname, path);
    pglayer_getFileName ( cur_layer, cur_file );
    strcat ( fullname, cur_file );
    
    xmstr = XmStringCreateLocalized (fullname);
    XtVaSetValues (_currPaneW, XmNlabelString, xmstr, NULL);
    XmStringFree (xmstr);
}


/*=====================================================================*/

void pgfilw_getFileName ( Boolean path_flag, char *file_name )
/************************************************************************
 * pgfilw_getFileName							*
 *									*
 * This function gets the current file name.  This path is prepended	*
 * if the path flag is TRUE.						*
 *									*
 * void pgfilw_getFileName (path_flag, file_name)			*
 *									*
 * Input parameters:                                                    *
 *	path_flag	Boolean	prepend path if TRUE			*
 *									*
 * Output parameters:                                                   *
 *	*file_name	char	current file name			*
 **									*
 * Log:									*
 * E. Safford/GSC	10/98	initial coding				*
 * S. Law/GSC		05/00	combined with pgfilw_qFilename		*
 * E. Safford/GSC	06/00	re-merge open and save paths   		*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    int		cur_layer;
    char	path[LLPATH], cur_file[MXFLSZ];
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
        
    if ( path_flag ) {
        pglayer_getFilePath ( cur_layer, path );
        strcpy ( file_name, path );
	pglayer_getFileName ( cur_layer, cur_file );
        strcat ( file_name, cur_file );        
    }
    else {
        pglayer_getFileName ( cur_layer, file_name );
    }

}

/*=====================================================================*/

void pgfilw_getPathName ( char *path_name )
/************************************************************************
 * pgfilw_getPathName							*
 *									*
 * This function returns the path when the VGF file was opened.		*
 *									*
 * void pgfilw_getPathName (path_name)					*
 *									*
 * Input parameters:                                                    *
 *									*
 * Output parameters:                                                   *
 *	*path_name	char	opened path name			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 5/00						*
 * E. Safford/GSC	06/00	re-merge open and save paths		*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    int		cur_layer;
/*---------------------------------------------------------------------*/    
    
    cur_layer = pglayer_getCurLayer();
    pglayer_getFilePath ( cur_layer, path_name );

}

/*=====================================================================*/

Boolean pgfilw_isFileSaved ( void )
/************************************************************************
 * pgfilw_isFileSaved							*
 *									*
 * This function returns TRUE if there is a file selected and it has	*
 * been saved previously. 						*
 *									*
 * Boolean pgfilw_isFileSaved ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	pgfilw_isFileSaved   Boolean	file is selected and saved	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	initial coding				*
 * J. Wu/SAIC		02/02	add layering				*
 ***********************************************************************/
{
    int		cur_layer;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
    return ( pglayer_isFileSaved (cur_layer) );

}

/*=====================================================================*/

void pgfilw_clearSaveFlag ( void )
/************************************************************************
 * pgfilw_clearSaveFlag							*
 *									*
 * This function clears the _fileIsSaved flag for the current layer. 	*
 *									*
 * void pgfilw_clearSaveFlag()					        *
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/00   initial coding                          *
 * J. Wu/SAIC		02/02	add layering				*
 ***********************************************************************/
{
    int		cur_layer;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
    pglayer_setFileSaved ( cur_layer, FALSE );

}

/*=====================================================================*/

Boolean pgfilw_isUp ( void )
/************************************************************************
 * pgfilw_isUp         							*
 *									*
 * This function returns the status of the window.                      *
 *									*
 * Boolean pgfilw_isUp()       					        *
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 * Return parameters:                                                   *
 *	pgfilw_isUp	Boolean		returns True if _ is managed.	*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/01   initial coding                          *
 ***********************************************************************/
{
    return ( XtIsManaged ( _fileSelW ) );
}

/*=====================================================================*/


void pgfilw_save ( void )
/************************************************************************
 * pgfilw_save         							*
 *									*
 * This function orders the current layer saved to a VG file and        *
 * updates all save related overhead information.			*
 *									*
 * void pgfilw_save( )               					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/03   initial coding                          *
 ***********************************************************************/
{
    char	cur_file[MXFLSZ];
/*---------------------------------------------------------------------*/

    pgfilw_getFileName ( FALSE, cur_file );

    if ( strlen( cur_file ) > (size_t)0) {
	pgfilw_saveVGF ();

	/*
	 *  Save off the file name used for the save
	 */
	strcpy ( _origPath, _currPath );
	strcpy ( _origName, cur_file );
	_origDirPos = _currDirPos;

	pgfilw_showFileName();

        pgfilw_startAutoSave ();
    }

    XtUnmanageChild (_fileSelW);
    XtUnmanageChild (_browsePopup);

    /*
     * There is no undo after a save to a file
     */
    pgundo_initUndo ();
    
    /*
     * Reset to previous oper selection.
     */
    pgpalw_setPrevOper ();

    /*
     *  Exit layering if necessay.
     */
    pglayrxt_exitAll (); 

}

/*=====================================================================*/
void pgfilw_startSaveAll( void )
/************************************************************************
 * pgfilw_startSaveAll         						*
 *									*
 * This function initiates a Save All, setting the _saveAll flag and    *
 * calling pgfilw_saveAll().						*
 *									*
 * void pgfilw_startSaveAll()	       					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/03   initial coding                          *
 * E. Safford/SAIC	11/03	add call to pgfilw_saveAll		*
 ***********************************************************************/
{
    _saveAll   = TRUE;
    _saveLayer = -1;
    _origLayer = pglayer_getCurLayer();

    _resetActv = FALSE;
    pgpalw_getOperWid( FUNC_LAYER );


    pgfilw_saveAll();
}

/*=====================================================================*/

static void pgfilw_saveAll ( void )
/************************************************************************
 * pgfilw_saveAll         						*
 *									*
 * This function performs a Save for all layers in use.                 *
 *									*
 * Because this has to potentially call a Save As if a layer has not    *
 * been saved before, this function must be able to operate             *
 * asynchronously so that it may be called multiple times and continue  *
 * execution appropriately.  To accomplish that it makes use of several *
 * static globals within this file.  It also contains a recursive call  *
 * to continue execution when only a Save is required.  This should be  *
 * ok -- the most the stack could possible get wound up with successive *
 * recursive calls would be the MAX_LAYERS value.			*
 *									*
 * void pgfilw_saveAll()	       					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/03   initial coding                          *
 ***********************************************************************/
{
    int	iret = 0;
/*---------------------------------------------------------------------*/

    /*
     *  Do nothing if _saveAll isn't active
     */
    if ( _saveAll ) {		

        _saveLayer++;
        _saveLayer = pglayer_getChngLayer( _saveLayer );

        if ( _saveLayer >= 0 ) {	/* Save the _saveLayer */	
	
            pglayrw_setActvLayer( _saveLayer, &iret );
	    _resetActv = TRUE;


	    if ( pglayer_isFileSaved( _saveLayer ) ) {
	        pgfilw_save();
	        pgfilw_saveAll();	/* recursive call to continue saveall */
	    }
	    else {
	        pgfilw_popup( FUNC_SAVE_VGF );
	    }
        }
        else {				/* Save All is done */

            pgfilw_popdown(); 
	    
	    if ( _resetActv ) {
	        pglayrw_setActvLayer( _origLayer, &iret );
            } 
	    else {
	        pglayer_setCurLayer( _origLayer );
            }

	    _saveAll   = FALSE;
	    _resetActv = FALSE;

            /*
             * Reset to previous oper
             */
            pgpalw_setPrevOper ();
        }
    }

}


/*=====================================================================*/

/* ARGSUSED */
static void pgfilw_selectCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgfilw_selectCb							*
 *									*
 * Callback function for selecting a VG File.				*
 *									*
 * void pgfilw_selectCb (wid, clnt, call)				*
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
 * D. Keiser/GSC	 8/97						*
 * C. Lin/EAI	 	10/97	rename from NxmFileSelCb, cleanup   	*
 * D.W.Plummer/NCEP	 3/98	change order of strcpy and XtFree	*
 * S. Law/GSC		01/00	set file name with path			*
 * S. Law/GSC		05/00	removed path				*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
    char * tmp_str;
    int		cur_layer;
/*---------------------------------------------------------------------*/

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &tmp_str);

    cur_layer = pglayer_getCurLayer();
    pglayer_setFileName ( cur_layer, tmp_str );

    XmTextSetString ( _fileSel_txtW, tmp_str );

    XtFree (tmp_str);

}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_selectDirCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgfilw_selectDirCb							*
 *									*
 * Callback function for selecting a VG File.				*
 *									*
 * void pgfilw_selectDirCb (wid, clnt, call)				*
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
 * S. Law/GSC		01/00	initial coding				*
 * S. Law/GSC		05/00	pgfilw_popup -> pgfilw_setFileList	*
 * J. Wu/SAIC		02/02	add layering				*
 ***********************************************************************/
{
    int			 cur_layer;
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    _currDirPos = cbs->item_position;
    cur_layer = pglayer_getCurLayer();

    if (_currDirPos == _vgfUsrTbl.nitems) {
	strcpy (_currPath, LOCAL_DIR);
    }
    else {
	strcpy (_currPath, _vgfUsrTbl.items[(_currDirPos) - 1].usrpath);
	if (_currPath[(strlen (_currPath) - 1)] != '/') {
	    strcat (_currPath, "/");
	}
    }

    pglayer_setFilePath ( cur_layer, _currPath );
    pglayer_setDirPos ( cur_layer, _currDirPos );
    pgfilw_setFileList ();
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_txtCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgfilw_txtCb								*
 *									*
 * Callback function for text input widget.				*
 *									*
 * void pgfilw_txtCb (wid, clnt, call )					*
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
 * C. Lin/EAI	 	12/97	  					*
 * E. Safford/GSC	02/99	mod to handle split control btn callbks	*
 ***********************************************************************/
{

    if (_pgfilFunc == FUNC_SAVE_VGF) {
        pgfilw_saveCtlBtnCb( NULL, 0, NULL);
    }
    else {
        pgfilw_openCtlBtnCb( NULL, 0, NULL);
    }

}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_openCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgfilw_openCtlBtnCb							*
 *									*
 * This is the callback function for the open control buttons (REPLACE, *
 * APPEND, and CANCEL) on the bottom of the file popup window.  	*
 *									*
 * void pgfilw_openCtlBtnCb  (wid, which, call )			*
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
 * C. Lin/EAI	 	12/97	 					*
 * E. Safford/GSC	01/98	restore saved file name on cancel       *
 * E. Safford/GSC	03/98	set Undo back to sensitive on exit	*
 * E. Safford/GSC	09/98	change undo toggle                	*
 * E. Safford/GSC	09/98	add setupOper on open	          	*
 * E. Safford/GSC	10/98	add setInitSave on open      		*
 * E. Safford/GSC	02/99	split up open and save callbacks    	*
 * S. Law/GSC		05/00	removed pgpalw_setInitSave		*
 * S. Law/GSC		05/00	added break				*
 * E. Safford/GSC	05/00	added param to pgfilw_openVGF       	*
 * E. Safford/GSC	06/00	re-merge open and save paths        	*
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * H. Zeng/EAI          02/01   reset to the previous oper selection    *
 * A. Hardy/GSC		07/01	added browsePopup check to cancel	*
 * J. Wu/SAIC		02/02	add layering			 	*
 * J. Wu/SAIC		04/02	preserve orig. dir. position if cancel	*
 * T. Lee/SAIC		04/02	called pgfilw_popdown () instead	*
 ***********************************************************************/
{
    int		ier, cur_layer;
/*---------------------------------------------------------------------*/

    ier = 0;
    cur_layer = pglayer_getCurLayer();
    switch ( which ) {

	case 0:		/* REPLACE */
	    pgfilw_openVGF(FALSE, &ier); 

	    break;

	case 1:         /* APPEND */
	    pgfilw_openVGF(TRUE, &ier); 

	    break;

	case 2:		/* CANCEL */
	    pglayer_setFilePath ( cur_layer, _origPath );
	    pglayer_setFileName ( cur_layer, _origName );
	    pglayer_setDirPos ( cur_layer, _origDirPos );
    	    
	    pgfilw_popdown ();

	    break;

    } /* the end of switch */

    /*
     * Reset to previous oper selection.
     */
    if (ier == 0) {
       pgpalw_setPrevOper ();
    }
 
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_saveCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgfilw_saveCtlBtnCb							*
 *									*
 * This is the callback function for the control buttons (OK and CANCEL)*
 * on the bottom of the file popup window.  				*
 *									*
 * void pgfilw_saveCtlBtnCb  (wid, which, call )			*
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
 * C. Lin/EAI	 	12/97	 					*
 * E. Safford/GSC	01/98	restore saved file name on cancel       *
 * E. Safford/GSC	03/98	set Undo back to sensitive on exit	*
 * E. Safford/GSC	09/98	change undo toggle                	*
 * E. Safford/GSC	09/98	add setupOper on open	          	*
 * E. Safford/GSC	10/98	add setInitSave on open      		*
 * E. Safford/GSC	02/99	split up save and open callbacks       	*
 * S. Law/GSC		05/00	changed to use _fileName, etc		*
 * E. Safford/GSC	06/00	re-merge open and save paths           	*
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * H. Zeng/EAI          02/01   reset to the previous oper selection    *
 * A. Hardy/GSC		07/01	added browsePopup check to cancel	*
 * J. Wu/SAIC		02/02	add layering			 	*
 * J. Wu/SAIC		03/02	move pglayrxt_exitAll() to the end	*
 * J. Wu/SAIC		04/02	preserve orig. dir. position if cancel	*
 * E. Safford/SAIC	11/03	if a Save All is in effect return to it *
 ***********************************************************************/
{
    int		cur_layer;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
    switch ( which ) {

	case 0:		/* OK */
	    pgfilw_saveCheck();

	    break;

	case 1:		/* CANCEL */
	    pglayer_setFilePath ( cur_layer, _origPath );
	    pglayer_setFileName ( cur_layer, _origName );
	    pglayer_setDirPos ( cur_layer, _origDirPos );

    	    XtUnmanageChild(_browsePopup);
    	    XtUnmanageChild(_fileSelW);
	    
            /*
             * Reset to previous oper selection.
             */
            pgpalw_setPrevOper ();
    
            /*  
             *  If a Save All action is currently in effect, return to it.
             */
            if ( _saveAll ) {
                pgfilw_saveAll();
            }

            /*
	     *  Exit layering if necessary.
	     */
            pglayrxt_exitAll (); 

	    break;
    }

}

/*=====================================================================*/
/* ARGSUSED */ 
static void pgfilw_saveAcceptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_saveAcceptCb							*
 *									*
 * This is an internal callback function for confirming to save a       *
 * VG file. 								*
 *									*
 * static void pgfilw_saveAcceptCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 8/97						*
 * C. Lin/EAI	 	10/97	rename from NxmFileSvCb, cleanup   	*
 * C. Lin/EAI	 	12/97	rename from pgfilw_saveCb   		*
 * E. Safford/GSC	01/98	loads _pgsvFileName to fix filename bug *
 * E. Safford/GSC  	03/98   set Undo back to sensitive on exit	*
 * E. Safford/GSC  	05/98   mod to use pgundo_initUndo        	*
 * S. Law/GSC		06/98	added call to _pgfilw_shortenName	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC  	09/98   change undo toggle                	*
 * E. Safford/GSC  	10/98   add setInitSave(TRUE)             	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC	03/99	add write to _bkXXX file		*
 * H. Zeng/EAI          11/99   added auto save feature                 *
 * S. Law/GSC		01/00	replaced cfl_rdir with cfl_gfil		*
 * S. Law/GSC		05/00	changed to use file size defines	*
 * S. Law/GSC		05/00	moved file saving to pgfilw_saveVGF	*
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * H. Zeng/EAI          02/01   reset to previous oper selection        *
 * J. Wu/SAIC		02/02	remove global "_fileName"	 	*
 * J. Wu/SAIC		03/02	exit layering if requested	 	*
 * J. Wu/SAIC		03/02	move pglayrxt_exitAll() to the end	*
 * J. Wu/SAIC		04/02	store dir. position for later recovery	*
 * E. Safford/SAIC	11/03	if a Save All is in effect return to it *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pgfilw_save();

    /*  
     *  If a Save All action is currently in effect, return to it.
     */
    if ( _saveAll ) {
        pgfilw_saveAll();
    }
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pgfilw_saveCancelCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_saveCancelCb 							*
 *									*
 * This function cancels the confirmation to save to a VG file.		*
 *									*
 * void pgfilw_saveCancelCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	 Widget		 Widget that activated callback	        *
 *	clnt	 XtPointer	 Pointer to client data 		*
 *	cbs	 XtPointer	 callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * J. Wu/SAIC		03/02	exit layering if requested		*
 * E. Safford/SAIC	11/03	if a Save All is in effect return to it *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);

    /*
     *  Exit layering if necessary.
     */
    pglayrxt_exitAll();

    /*  
     *  If a Save All action is currently in effect, return to it.
     */
    if ( _saveAll ) {
        pgfilw_saveAll();
    }
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pgfilw_browseBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_browseBtnCb							*
 *									*
 * This is an internal callback function for the browse button		*
 *									*
 * void pgfilw_browseBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		06/98	initial coding				*
 ***********************************************************************/
{
    XtManageChild (_browsePopup);
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pgfilw_browseDoneCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_browseDoneCb							*
 *									*
 * This is an internal callback function for the browse buttons,	*
 * Ok and Cancel.							*
 *									*
 * void pgfilw_browseDoneCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		06/98	initial coding				*
 *  S. Law/GSC		05/00	reworked not to return to the main popup*
 *  J. Wu/SAIC		02/02	add layering			 	*
 *  J. Wu/SAIC		01/03	unmanage _fileSelW before remanage it	*
 ***********************************************************************/
{
    int		which, cur_layer;
    char 	*text, file_name[MXFLSZ];
    Widget	popup;
    FSBCBS 	*fcbs;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    cur_layer = pglayer_getCurLayer();
    if (which == 1) {	/* OK */
	fcbs = (FSBCBS *) cbs;
	XmStringGetLtoR (fcbs->value, XmFONTLIST_DEFAULT_TAG, &text);

	pgfilw_setFileName (_pgfilFunc, text); 
	XtFree (text);

	pglayer_getFileName ( cur_layer, file_name ); 
	XmTextSetString (_fileSel_txtW, file_name);
	XmTextSetInsertionPosition (_fileSel_txtW, (long)strlen(file_name) );

	if ( file_name[0] == '\0' ) {
	    popup = (XtIsManaged (_browsePopup)) ? _browsePopup : _fileSelW;
	    NxmWarn_show (popup, "Not a valid file.");
	}
	else {
	    if (_pgfilFunc == FUNC_OPEN_VGF) {
	        XtUnmanageChild (_fileSelW);
		pgfilw_managePanes (BROWSE_POPUP);
		XtUnmanageChild (_browsePopup);
		XtManageChild (_fileSelW);
	    }
	    else {
		pgfilw_saveCheck ();
	    }
	}
    }
    else {
	XtUnmanageChild (_browsePopup);
    }
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pgfilw_sortByCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_sortByCb							*
 *									*
 * This is an internal callback function for the sort by toggles	*
 *									*
 * void pgfilw_sortByCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 *  E. Safford/GSC	02/99	initial coding      			*
 *  E. Safford/GSC	05/00	add popdown call    			*
 ***********************************************************************/
{
int	new_sort;
/*---------------------------------------------------------------------*/

    new_sort = (long)clnt;

    if (new_sort != _sortBy) {
        _sortBy = new_sort;
 	pgfilw_popdown();
        pgfilw_popup( _pgfilFunc );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_autoSaveCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_autoSaveCb							*
 *									*
 * This is the callback function for auto save toggles	                *
 *									*
 * void pgfilw_autoSaveCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		long		whether auto save is on or not	*
 *	cbs		XtPointer	callback structure		*
 *									*
 **									*
 * Log:									*
 *  H. Zeng/EAI		11/99	initial coding				*
 *  S. Law/GSC		05/00	changed client to a Boolean		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _autoSaveIsOn = (Boolean)clnt;
 
}

/*=====================================================================*/
/* ARGSUSED */
static void pgfilw_autoSaveTO ( XtPointer clnt, XtIntervalId id )
/************************************************************************
 * pgfilw_autoSaveTO							*
 *									*
 * Time out function for auto save. Saves the updated VG file.		*
 *									*
 * void pgfilw_autoSaveTO (clnt, id)					*
 *									*
 * Input parameters:							*
 *	clnt		XtPointer	not used			*
 *	id		XtIntervalId	not used			*
 *									*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		11/99	initial coding				*
 * S. Law/GSC		01/00	replaced cfl_rdir with cfl_gfil		*
 * S. Law/GSC		05/00	moved file saving to pgfilw_saveVGF	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pgfilw_saveVGF ();
}

/*=====================================================================*/

void pgfilw_openVGF ( Boolean append, int *iret )
/************************************************************************
 * pgfilw_openVGF							*
 *									*
 * This function gets the VG file name and opens it. 			*
 *									*
 * pgfilw_openVGF ( append, iret )					*
 *									*
 * Input parameters:							*
 *   append		Boolean		control toggle			*
 *					  TRUE  = append to work file	*
 *					  FALSE = replace work file	*
 * Output parameters:							*
 *   *iret		int	 	 0 = normal 			*
 *					-1 = no file name		*
 *					-2 = error on read		*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 8/97						*
 * G. Krueger/EAI	 9/97	Changed NxmWarning -> NxmWarn_show	*
 * E. Wehner/EAi	 9/97	Remove grapics info			*
 * C. Lin/EAI		10/97	rename from NxmFileSelOpOKCb, cleanup	*
 * C. Lin/EAI		12/97	rename from pgfilw_openOkCb, cleanup	*
 * E. Safford/GSC	01/98	add file append functionality		*
 * E. Safford/GSC	01/98	Add crg_init to clear range before load	*
 * E. Safford/GSC	03/98	mod to reset up undo/redo button	*
 * S. Law/GSC		04/98	Added call to pghdlb_deselectAll	*
 * S. Law/GSC		06/98	Added call to pgfilw_checkPerms		*
 * S. Law/GSC		06/98	Added call to pgfilw_shortenName	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	09/98	change undo toggle              	*
 * S. Law/GSC		02/99	added file length check before cvg_cp	*
 * E. Safford/GSC	02/99	add parameter to control replace/append	*
 * E. Safford/GSC	02/99	allow empty vgf file open w/o error msg	*
 * S. Law/GSC		05/00	pgfilw_open -> pgfilw_openVGF		*
 * S. Law/GSC		05/00	reworked append to not use .save file	*
 * E. Safford/GSC	05/00	added iret param                        *
 * E. Safford/GSC	06/00	re-merge open and save paths            *
 * S. Law/GSC		07/00	_fileIsSaved set to FALSE when opened	*
 * E. Safford/GSC	11/00	use the existing file name on appends   *
 * E. Safford/GSC	11/00	_fileIsSaved always set to FALSE        *
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * H. Zeng/EAI          02/01   removed call to pgpalw_setCurOper()     *
 * E. Safford/GSC	02/01	rplcd pgpalw_deleteAllCb w/ _deleteAll  *
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		11/01	remove redundant geplot call		*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	        01/02   redesign the loading algorithm		*
 * J. Wu/SAIC		02/02	remove global _fileName & _dirPath	*
 * E. Safford/SAIC	02/02	add xpgrfrsh()				*
 * J. Wu/SAIC		02/02	set changes_made TRUE after loading	*
 * J. Wu/SAIC		03/02	set changes_made TRUE only for "APPEND"	*
 * E. Safford/SAIC	03/02	removed check on maxbytes <= 1		*
 * E. Safford/SAIC	03/02	don't process 0 length input files	*
 * E. Safford/SAIC	03/02	zero group nums if replace w/o layering *
 * E. Safford/SAIC	04/02	add cvg_ggmtrx() & cvg_ggfrmtrx()	*
 * E. Safford/SAIC	04/02	fixed cvg_gmtrx and cvg_gfrmtrx names   *
 * J. Wu/SAIC		04/02	update dir. position			*
 * T. Lee/SAIC		05/02	popup error message on main drawing area*
 * T. Piper/SAIC	02/03	use "-2" in cvg_write			*
 * H. Zeng/XTRIA	04/03   added .DEFAULT.vgf removal choice	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	04/04	make fileSaved flag True for Append     *
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * E. Safford/SAIC	01/04	treat initial file load w/ append like  *
 *				 replace if no other file was loaded    *
 * S. Danz/AWC		07/06	Update to new cvg_write() parameter     *
 * S. Danz/AWC		07/06	Initialize CVG placement info           *
 * B. Yin/SAIC          01/07   Show warning if appending a vg file with*
 *				different freezing level ranges.	*
 * E. Safford/SAIC	06/07	check ier from cvg_write.  Skip ranging *
 *				 if write failed for any reason.	*
 ***********************************************************************/
{
    char	fname[FILE_FULLSZ], infil[FILE_FULLSZ], outfil[FILE_FULLSZ];
    char	file_name[MXFLSZ], file_path[LLPATH];
    char    	range1[ STD_STRLEN ], range2[ STD_STRLEN ];
    int		ipos, ier, more, flag, orecsz, nrecsz, group_out;
    int		ii, curr_grp, grpnums[MAX_GROUP_TYPE], cur_layer;
    long	cursiz, curpos, maxbytes;
    Boolean	can_read, can_write, initialAppend = False;
    VG_DBStruct	el;
    FILE	*ifp, *ofp;

/*---------------------------------------------------------------------*/

    *iret = 0;
    
    cur_layer = pglayer_getCurLayer();
    pglayer_getFilePath ( cur_layer, file_path );
    pglayer_getFileName ( cur_layer, file_name );
        
    if ( strlen(file_name) <= (size_t)0 ) {
	can_read = FALSE;
	*iret = -1;
    }
    else {
        strcpy (fname, file_path);
        strcat (fname, file_name);

        cst_srch ( 0, (int)strlen(fname), ".vgf", fname, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat(fname, ".vgf");
	}

        pgfilw_checkPerms (fname, &can_read, &can_write);
    }

    if (!can_read) {
	NxmWarn_show ( mcanvw_getDrawingW(), "VG File is not readable" );
	*iret = -2;
    }
    else {        	
	
       /*
        *  Check the integrity of the input file & WORK_FILE. If the  
	*  operation is "Replace", delete all elements first. 
	*/
        cfl_inqr (fname, NULL, &maxbytes, infil, &ier);
        if ( ier < 0 ) {
            *iret = -2;
	    return;
	}	

	cfl_inqr (cvg_getworkfile(), NULL, &cursiz, outfil, &ier);

	if (append) {
	
	    /*
	     *  Show waring message if the appending vg file has
	     *  different freezing level ranges.
	     */
	    if ( !pgfilw_cmpFzlRanges( cvg_getworkfile(), infil,
	    			       range1, range2 ) ) {

		/*
		 *  If one of the range is NULL, use the good range and
		 *  don't show the warning. Otherwise, pop up the warning.
		 */
		if ( ( strlen( range1 ) == 0 && strlen( range2 ) != 0 ) ||
		     ( strlen( range2 ) == 0 && strlen( range1 ) != 0 ) ) {
		   
		   strcpy ( _fzlRange, strlen( range1 ) > 0? range1 : range2 );
	           pgfilw_appendFzl( );
		   
		}
		else {

		   pgfilw_showFzlRangeWarning( _fileSelW, infil, range1, range2 );

		}

		return;

            }

	    if ( (unsigned long)cursiz <= (sizeof(VG_HdrStruct)+sizeof(FileHeadType))) { 
	        cvg_crvgf (cvg_getworkfile(), &ier);
	    }

	    /*
	     *  If the origNameLen is > 0 then use the original file name
	     *  and path as the default.
	     *  If the origNameLen is == 0, then we'll use the new file as
	     *  the default (see below).
	     */
	    if( strlen( _origName ) > (size_t)0 ) {
                pglayer_setFileName ( cur_layer, _origName ); 
                pglayer_setFilePath ( cur_layer, _origPath ); 
	        strcpy (_currPath, _origPath);
	        _currDirPos = _origDirPos;
	    } 
	    else {
		initialAppend = True;
            }

	}	
	else {

	    _clearFlag = FALSE;

            /*
             * Either mark every element in .DEFAULT.vgf file as deleted
             * or simply remove it.
             */
            if ( pgpalw_isLayerActv() ) {

	         pgpalw_deleteAll();
            }
            else {
		 pgpalw_rmDefVGF();
            }

	    _clearFlag = TRUE;
	   
        }

	XtUnmanageChild(_fileSelW);

	/*
	 *  Load the transformation matrix to compress group numbers.
	 */
    	cvg_gmtrx ( fname, &_matrixSize, &_matrix, &ier );

        /*
	 *  For Replace and appends where no other file has been opened
	 *  use this file name/path as the defaults.
	 */
	if( !append || initialAppend ) {

	    strcpy (_origPath, _currPath);
	    strcpy (_origName, file_name);
	    _origDirPos = _currDirPos;
	    
	    pgfilw_showFileName();
            /*
             * If we are replacing, then toss what placement info we have
             */
            cvg_clearplace(&ier);
	}

        /*
         * Get ready for placemnet
         */
        cvg_initplace(&ier);


	if ( maxbytes > 0 ) {

            /* 
	     * Open the input file and WORK_FILE for updating.
	     */
	    cvg_open (infil, G_FALSE, &ifp, &ier);        
	    if ( ier < 0 ) NxmErr_update();

	    cvg_open (cvg_getworkfile(), G_TRUE, &ofp, &ier);
	    if ( ier < 0 ) NxmErr_update();

	    /*
	     *  Reset the group number matrix to zeros.
	     */	
            for ( ii = 0; ii < MAX_GROUP_TYPE; ii++ ) {
	        grpnums[ii] = 0;
            }


            /*
             *  Store the largest group number in WORK_FILE for each 
	     *  group type unless this is a replace and layering is not
	     *  active (meaning only 1 layer is in use).
             */
	    if ( append || pglayrw_isUp() ) {
                curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
                more = G_TRUE;

                while ( ( more ) && ( curpos < cursiz ) ) {

 	        cvg_rdhdr (outfil, ofp, (int)curpos, (int)cursiz, &el, &flag, &ier);

	            if ( ier == 0 && el.hdr.recsz > 0 ) { 

	                orecsz = el.hdr.recsz;
                
	                if ( (int) el.hdr.grptyp != 0 )	{	     
		            curr_grp = (int)el.hdr.grptyp;
		    
		            if ( grpnums[curr_grp] < el.hdr.grpnum ) {
                                grpnums[curr_grp] = el.hdr.grpnum;
	                    }
		        }	        
		    
		        curpos += (long)orecsz;
                    }	    
	        
	            else {
	                *iret = -2;
		        more = G_FALSE;
                    }
	        } 	    
	    }

            /* 
             *  Load valid elements from input file into WORK_FILE. 
             *  Reassign group numbers & build range records.
             */
            more = G_TRUE;
            curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);

            cfl_inqr (cvg_getworkfile(), NULL, &cursiz, outfil, &ier);
	    cfl_seek(ofp, cursiz, 0, &ier);
	    while ( ( more ) && ( curpos < maxbytes ) ) {

	        cvg_rdhdr (infil, ifp, (int)curpos, (int)maxbytes, &el, &flag, &ier);

	        if ( ier == 0 && el.hdr.recsz > 0 ) { 
	        
		    orecsz = el.hdr.recsz;

	            cvg_rdele (&el, (int)curpos, el.hdr.recsz, ifp, &ier);
	            
		    if ( ier != 0 ) {
	                *iret = -2;
	                more = G_FALSE;
	            }

	            /*
	             * Skip deleted elements & file-head element.
 	             */
	            if ( ( more ) && ( el.hdr.delete == 0 ) && 
	    	         ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

	                nrecsz = el.hdr.recsz;

	                /*
	                 * Renumber the groups as needed.
	                 */		    		    
	                if ( (int)el.hdr.grptyp != 0 ) {
		            curr_grp = (int)el.hdr.grptyp;   
		            pgfilw_renumberGrp (el.hdr.grptyp, 
			    			el.hdr.grpnum, &group_out );
			    
                            el.hdr.grpnum = grpnums[curr_grp] + group_out;
                        }
		    
		        cvg_write (&el, -2, nrecsz, ofp, TRUE, &ier);		

		        /*
		         *  Build range record if no error occured on the 
			 *  call to cvg_write().
			 *
			 *  Error -28 (BAD_NUMBER_OF_POINTS) indicates an 
			 *  invalid number of points detected when trying to 
			 *  write the vg element.  Skip that element and 
			 *  continue processing the rest of the file.
			 *
			 *  Any other error is considered non-rcoverable and
			 *  file processing is halted as elegantly as possible.
		         */
		        if ( ier >= 0 ) {		    
			    crg_set (&el, (int)cursiz, cur_layer, &ier);
		            cursiz = cursiz + (long)nrecsz; /* re-position in WORK_FILE */
		        }
			else if ( ier == BAD_NUMBER_OF_POINTS ) {
			    ier = 0;  /* don't increment cursiz */
			}
			else {
		    	    more = G_FALSE;	            
		        }


	            }

                    /*
                       * Free TCA break point/GFA block memory
                       */
                    if ( el.hdr.vg_type == TCA_ELM ) {
                       cvg_freeBkpts ( &el );
                    }
		    else if ( el.hdr.vg_type == GFA_ELM ) {
                        cvg_freeElPtr ( &el );
                    }

	            curpos += (long)orecsz;  /* re-position in input file */

	        }
	    
	        else {
	            *iret = -2;
		    more = G_FALSE;
	        }
	    
            }

	    /* 
	     *  Now, close files, redraw all, and reset "Undo".
	     */
            cfl_clos (ifp, &ier);
            cfl_clos (ofp, &ier);
	} 
		
	/* 
	 * With placement, we could be rearranging quite a bit of the screen
	 * if we append on something already there, so refresh the background
	 * before we redraw
	 */
	if ( cvg_plenabled() && append && !initialAppend ) {
	    xpgrestlp();
	}
	cvg_redraw (NULL, &ier);
	xpgrfrsh();
	
	pgundo_initUndo();

	/* 
	 *  reset file_saved & changes_made flags.
	 */
	pglayer_setFileSaved( cur_layer, 
			(Boolean)(( append && !initialAppend ) ? TRUE : FALSE) );
	pglayer_setChngMade ( cur_layer, 
			(Boolean)(( append && !initialAppend ) ? TRUE : FALSE) );
    }
   
    /*
     *  Free the _matrix memory, and reset _matrixSize. 
     */
    cvg_gfrmtrx (_matrix, &_matrixSize);

}


/*=====================================================================*/

static void pgfilw_saveCheck ( void )
/************************************************************************
 * pgfilw_saveCheck							*
 *									*
 * This function gets the file name, checks the file permissions, and	*
 * and confirms the name with the user.					*
 *									*
 * static void pgfilw_saveCheck( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	08/97						*
 * C. Lin/EAI		10/97	rename from NxmFileSelSvOKCb, cleanup	*
 * C. Lin/EAI		12/97	rename from pgfilw_saveOkCb, cleanup	*
 * S. Law/GSC		06/98	Added call to pgfilw_checkPerms		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	04/99	fix call to NxmConfirm_show		*
 * S. Law/GSC		05/00	pgfilw_save -> pgfilw_saveCheck		*
 * E. Safford/GSC	06/00	re-merge open and save paths		*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    int		ipos, ier, cur_layer;
    char	*ss, fname[FILE_FULLSZ], warning[(FILE_FULLSZ + 80)];
    char	*fptr, file_path[LLPATH];
    Boolean	can_read, can_write;
    Widget	popup;
/*---------------------------------------------------------------------*/

    cur_layer= pglayer_getCurLayer ();
    /*
     * Retrieve the text string from text widget field.
     */
    ss = XmTextGetString ( _fileSel_txtW );
    pglayer_setFileName ( cur_layer, ss );

    pglayer_getFilePath ( cur_layer, file_path );
    strcpy (fname, file_path);
    strcat (fname, ss);

    XtFree(ss);

    /*
     * Append .vgf extension if necessary.
     */
    cst_srch ( 0, (int)strlen(fname), ".vgf", fname, &ipos, &ier );
    if ( ier == -4 ) {
        strcat (fname, ".vgf");
    }

    popup = (XtIsManaged (_browsePopup)) ? _browsePopup : _fileSelW;

    pgfilw_checkPerms (fname, &can_read, &can_write);
    if (!can_write) {
	NxmWarn_show ( _fileSelW, "No permission to write VG File" );
	return;
    }

    /*
     * Confirm save as action with user, but don't present the initial "./"
     * in the file name to the user if writing to the local directory.
     */
    if ( strcmp (file_path, LOCAL_DIR) == 0 ) { 
	fptr = strchr (fname, '/');
	fptr ++;
    }
    else {
	fptr = &(fname[0]);
    }

    sprintf (warning,
	     "Are you sure you want to save the current VG file as %s?",
	     fptr);

    NxmConfirm_show (popup, warning, pgfilw_saveAcceptCb, 
		     pgfilw_saveCancelCb, NULL, &ier );
    mbotw_mouseClear();


}

/*=====================================================================*/

static void pgfilw_saveVGF ( void )
/************************************************************************
 * pgfilw_saveVGF							*
 *									*
 * This function saves the VG file.					*
 *									*
 * static void pgfilw_saveVGF ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	initial coding				*
 * S. Law/GSC		06/00	re-merge open and save paths		*
 * E. Safford/GSC	06/00	append ".vgf" as necessary		*
 * J. Wu/SAIC		01/02	redesign the algorithm to add layering	*
 * J. Wu/SAIC		02/02	use pglayer functions for file name/path*
 * J. Wu/SAIC		02/02	reset changes_made flag			*
 * J. Wu/SAIC		04/02	start auto save only with a valid file	*
 * T. Piper/SAIC	02/03	use "-2" in cvg_write			*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 * S. Danz/AWC		07/06	Update to new cvg_write() parameter     *
 ***********************************************************************/
{
    int		ier, ipos, flag, orecsz, nrecsz, cur_layer, el_layer;
    long        cursiz, curpos, wrk_size;
    char	full_name[FILE_FULLSZ];
    char	file_name[MXFLSZ], file_path[LLPATH] ;
    char	bk_fileName[FILE_FULLSZ], tmp_file[FILE_FULLSZ];
    Boolean	save_file = FALSE, backup = FALSE, more;    
    FILE	*wrkfp, *outfp, *bkfp;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    wrkfp = NULL;
    outfp = NULL;
    bkfp = NULL;

    cur_layer = pglayer_getCurLayer();
    pglayer_getFilePath ( cur_layer, file_path );
    pglayer_getFileName ( cur_layer, file_name );
                
    if ( strlen(file_name) > (size_t)0 ) {
		
	strcpy (full_name, file_path);
	strcat (full_name, file_name);

        /*
         * Append .vgf extension if necessary.
         */
        cst_srch ( 0, (int)strlen(file_name), ".vgf", file_name, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat (full_name, ".vgf");
	    strcat (file_name, ".vgf");
            pglayer_setFileName ( cur_layer, file_name );
	}
        
        /*
         *  Check if the save is necessary.
         */
	if ( strcmp (cvg_getworkfile(), full_name) != 0 ) {
	    cvg_crvgf (full_name, &ier);
	    cfl_inqr (full_name, NULL, &cursiz, tmp_file, &ier);
	    cvg_open (full_name, G_TRUE, &outfp, &ier);        
	    cfl_seek (outfp, cursiz, 0, &ier);
            save_file = TRUE;
       }

        /*
         *  WRITE TO BACKUP FILE if ".DO_BACKUP" file is present in
         *  the user's directory.
         */
        pglayer_getFileName ( cur_layer, file_name );
        pgfilw_getBackupName (file_name, bk_fileName, &ier);
	if ( ier == 0 ) {
	    cvg_crvgf (bk_fileName, &ier);
	    cfl_inqr (bk_fileName, NULL, &cursiz, tmp_file, &ier);
	    cvg_open (bk_fileName, G_TRUE, &bkfp, &ier);        
	    cfl_seek (bkfp, cursiz, 0, &ier);	    
	    backup = TRUE;	    
	}
	
        /* 
         *  Select non-deleted elements on the current PGEN layer 
	 *  from WORK_FILE and save into output/backup files. 
         */
        more = FALSE;
	if ( save_file || backup ) { 
	   more = TRUE;
	   cfl_inqr (cvg_getworkfile(), NULL, &wrk_size, tmp_file, &ier);
	   cvg_open (cvg_getworkfile(), G_FALSE, &wrkfp, &ier);          	
	}
	
	curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
        
	while ( ( more ) && ( curpos < wrk_size ) ) {

	    el_layer = crg_getLayer ((int)curpos);
	    cvg_rdhdr (cvg_getworkfile(), wrkfp, (int)curpos, (int)wrk_size, 
			&el, &flag, &ier);
	    
	    if ( ier == 0 && el.hdr.recsz > 0 ) { 
	        
		orecsz = el.hdr.recsz;

	        cvg_rdele (&el, (int)curpos, el.hdr.recsz, wrkfp, &ier);

	        if ( ier != 0 ) {
	            more = G_FALSE;
	        }

	        /*
	         * Skip non-current-layer elements, deleted elements,  
		 * and file-head element.
 	         */
	        if ( ( more ) && ( el.hdr.delete == 0 ) &&
		     ( el_layer == cur_layer ) &&
	    	     ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

	            nrecsz = el.hdr.recsz;

		    if ( save_file ) {
		        cvg_write (&el, -2, nrecsz, outfp, FALSE, &ier);		
		    }
		    
		    if ( backup ) {
			cvg_write (&el, -2, nrecsz, bkfp, FALSE, &ier);		
		    }
		    
		    cursiz = cursiz + (long)nrecsz; /* re-pos in output & backup */
	        
		}	        
                
	        curpos += (long)orecsz;  /* re-position in WORK_FILE */

                /*
                 * Free TCA break point/GFA block memory
                 */
                if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
                }
		else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr( &el );
                }
	    }	    
	    
	    else {
		more = G_FALSE;
	    }
	    	    
        }  /* End of "while" loop */
	
	
	/*
	 *  Set the file_saved and changes_made flag.
	 */
	pglayer_setFileSaved ( cur_layer, TRUE );
	pglayer_setChngMade ( cur_layer, FALSE );
        
	/* 
         * Close all files.
         */
	cvg_clos (wrkfp, &ier);
	cvg_clos (outfp, &ier);
	cvg_clos (bkfp, &ier);
	
        /*
         *  start the timed callback
         */
        pgfilw_startAutoSave();
    }        
}

/*=====================================================================*/
 
static void pgfilw_checkPerms ( char *cfile, Boolean *can_read, 
						Boolean *can_write )
/************************************************************************
 * pgfilw_checkPerms							*
 *									*
 * This function checks the permissions on the passed in file to see if	*
 * the user has read and write permission.  If the file is not found,	*
 * can_read is FALSE, and the write permission of the directory is	*
 * checked.  If the directory is not found, can_write is FALSE.	Due to	*
 * policy, the user is only allowed to write in a directory owned by	*
 * user.								*
 *									*
 * void pgfilw_checkPerms (cfile, can_read, can_write)			*
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
 * S. Law/GSC		06/98	initial coding				*
 * E. Safford/GSC	10/98	expand write perms to match file perms  *
 * S. Law/GSC		05/00	changed to use _currPath		*
 * T. Lee/GSC		04/01	redeclaration of variables uid, gid	*
 * E. Safford/GSC	04/01   fix directories in cfile problem	*
 * E. Safford/GSC	06/01   repair prev. fix & rm UID check on write*
 * S. Jacobs/NCEP	 3/02	Expand environment variables in file	*
 ***********************************************************************/
{
    static Boolean	need_ids  = TRUE;
    Boolean		subdir;
    static uid_t	uid;
    static gid_t	gid;
    struct stat		fstat;
    char		cpath[LLPATH], *cptr, newfile[MXFLSZ];
    int			cnt, ii, ier;
/*---------------------------------------------------------------------*/

    /*
     * Expand any environment variables in the file name.
     */
    css_envr ( cfile, newfile, &ier );

    /*
     *  Count the number of '/' found in the cfile string.  
     *  1 = just a file name, >1 = a subdirectory and file name.
     */
    cnt = 0;
    for (ii=(int)strlen(newfile); ii >= 0; ii--) {
        if (newfile[ii] == '/') cnt++;
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
    if (stat (newfile, &fstat) == 0) {

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
	 *  If the newfile was determined to contain a subdirectory then
	 *  pull file name off and test for write permission on the target 
	 *  directory.
	 */
  	if ( subdir ) { 
  	    strcpy (cpath, newfile);

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
	     *  If no subdirectory, use _currPath for the expanded path.
	     */
  	    strcpy (cpath, _currPath); 
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
 
static void pgfilw_startAutoSave ( void )
/************************************************************************
 * pgfilw_startAutoSave							*
 *									*
 * This function checks whether the value of _autoSaveIsOn. If TRUE,	*
 * a timed callback is engaged to auto save the VG file after a		*
 * predetermined amount of time.					*
 *									*
 * void pgfilw_startAutoSave ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		11/99	initial coding				*
 * S. Law/GSC		05/00	(int)_autoSave -> (Boolean)_autoSaveIsOn*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    /*
     *  Clear out existing timed callback.
     */
    pgfilw_stopAutoSave ();


    /*
     *  Now restart the timer if _autoSave is ON.
     */

    if (_autoSaveIsOn) {
	/*
	 *  Initiate callback in 5 minutes
	 */
	_timeOutId = XtAppAddTimeOut (_appContext, 300000L,
	  		(XtTimerCallbackProc)pgfilw_autoSaveTO,
			                        (XtPointer)NULL );
    }
} 

/*=====================================================================*/

static void pgfilw_stopAutoSave ( void )
/************************************************************************
 * pgfilw_stopAutoSave                                                  *
 *                                                                      *
 * This function removes the _timeOutId callback and sets the _timeOutId*
 * to NULL.                                                             *
 *                                                                      *
 * void pgfilw_stopAutoSave ()                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          11/99   initial coding                          *
 ***********************************************************************/
{
    if (_timeOutId !=(XtIntervalId)NULL) {

        XtRemoveTimeOut(_timeOutId);
        _timeOutId = (XtIntervalId)NULL;
    }
}

/*=====================================================================*/

static void pgfilw_managePanes ( int popup_type )
/************************************************************************
 * pgfilw_managePanes							*
 *									*
 * This function manages the appropiate children of the main pane	*
 * widget based on popup_type.						*
 *									*
 * void pgfilw_managePanes ( popup_type )				*
 *									*
 * Input parameters:							*
 *	popup_type	int	decides which children to use		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	initial coding				*
 ***********************************************************************/
{
    pgfilw_unmanagePanes ();

    switch (popup_type) {
      case OPEN_POPUP:
	XtManageChild (_sortPaneW);
	XtManageChild (_selectPaneW);
	XtManageChild (_browseBtnPane);
	XtManageChild (_autosvPaneW);
	XtManageChild (_openPaneW);
	break;

      case SAVE_POPUP:
	XtManageChild (_sortPaneW);
	XtManageChild (_selectPaneW);
	XtManageChild (_inputPaneW);
	XtManageChild (_browseBtnPane);
	XtManageChild (_autosvPaneW);
	XtManageChild (_savePaneW);
	break;

      case BROWSE_POPUP:
	XtManageChild (_currPaneW);
	XtManageChild (_openPaneW);
	break;
    }
}

/*=====================================================================*/

static void pgfilw_unmanagePanes ( void )
/************************************************************************
 * pgfilw_unmanagePanes							*
 *									*
 * This function unmanages all the children of the main pane widget.	*
 *									*
 * void pgfilw_unmanagePanes ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_sortPaneW)) {
	XtUnmanageChild (_sortPaneW);
    }

    if (XtIsManaged (_selectPaneW)) {
	XtUnmanageChild (_selectPaneW);
    }

    if (XtIsManaged (_currPaneW)) {
	XtUnmanageChild (_currPaneW);
    }

    if (XtIsManaged (_inputPaneW)) {
	XtUnmanageChild (_inputPaneW);
    }

    if (XtIsManaged (_browseBtnPane)) {
	XtUnmanageChild (_browseBtnPane);
    }

    if (XtIsManaged (_autosvPaneW)) {
	XtUnmanageChild (_autosvPaneW);
    }

    if (XtIsManaged (_openPaneW)) {
	XtUnmanageChild (_openPaneW);
    }

    if (XtIsManaged (_savePaneW)) {
	XtUnmanageChild (_savePaneW);
    }
} 

/*=====================================================================*/

static void pgfilw_setFileList ( void )
/************************************************************************
 * pgfilw_setFileList							*
 *									*
 * This function sets up the list of files for the current path.	*
 *									*
 * void pgfilw_setFileList ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/00	moved from pgfilw_popup			*
 * J. Wu/SAIC		02/02	add layering			 	*
 ***********************************************************************/
{
    int		ii, nf;
    char	fn_list[MAX_FILES][MXFLSZ], file_name[MXFLSZ];
    XmString	xmstr;
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    /*
     * Wipe the list of .vgf filenames       
     */
    XmListDeleteAllItems(_fileSel_listW);

    /*
     *  Get list of file names in the directory & write to the fn_list
     */
    nf = cfl_gfil (_sortBy, MAX_FILES, _currPath, ".vgf", fn_list);

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
 	pgfilw_getFileName ( FALSE, file_name );
	xmstr = XmStringCreateLocalized (file_name);
	XmListSelectItem (_fileSel_listW, xmstr, FALSE);
	XmListSetBottomItem (_fileSel_listW, xmstr);
	XmStringFree (xmstr);
    }
}

/*=====================================================================*/

static void pgfilw_getBackupName ( char *infil, char *bkfil, int *iret )
/************************************************************************
 * pgfilw_getBackupName                                                 *
 *                                                                      *
 * This function gets the BACKUP file name if .DO_BACKUP file exists.	*
 *                                                                      *
 * void pgfilw_getBackupName ( infil, bkfil, iret )			*
 *                                                                      *
 * Input parameters:							*
 *	*infil		char	File name to be backed up		*
 *									*
 * Output parameters:							*
 *	*bkfil		char	Backup file name 			*
 *	*iret		int	Return code				*
 *				-1 = .DO_BACKUP file does not exist	*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/02   initial coding                          *
 ***********************************************************************/
{
    int		ier, bk_files;
    long	ignore;
    char	bk_start[MXFLSZ], tmp_file[MXFLSZ];
    char	dir[] = {"./"}, bk_test[] = {".DO_BACKUP"};
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     *  Compose a BACKUP file name if ".DO_BACKUP" file is present in
     *  the user's current directory.
     */
    cfl_inqr (bk_test, NULL, &ignore, tmp_file, &ier);

    if ( ier != 0 ) {
         *iret = -1;
    }
    else {
        strcpy (bk_start, infil);
	bk_start[strlen(bk_start) - 4] = '\0';

	bk_files = cfl_gfil(0, MAX_FILES, dir, bk_start, NULL);

	sprintf (bkfil, "%s_bk%03d.vgf", bk_start, bk_files);
    }
    
}

/*=====================================================================*/

static void pgfilw_renumberGrp   ( char grptyp, int grpin, int *grpout )
/************************************************************************
 * pgfilw_renumberGrp                                                   *
 *                                                                      *
 * This function gets the remapped group number for a given group type  *
 * and group number from the _matrix structure.				*
 *                                                                      *
 * void pgfilw_renumberGrp ( grptyp, grpin, grpout )			*
 *                                                                      *
 * Input parameters:							*
 *	grptyp		char	group type                 		*
 *	grpin		int	existing group number in file		*
 *									*
 * Output parameters:							*
 *	*grpout		int	remapped group number, or -1 if !found	*
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/SAIC	04/02   initial coding                          *
 ***********************************************************************/
{
    int		ii, jj, new_grp = -1;
    Boolean	found_typ = False;
/*---------------------------------------------------------------------*/

    for (ii=0; (ii<_matrixSize) && !found_typ; ii++) {

	if ( _matrix[ii].grptyp == grptyp ) {
	    found_typ = True;
	    break;
	}
    }


    if ( found_typ ) {

        for (jj=0; jj<_matrix[ii].numgrps; jj++) {
	
	    if ( _matrix[ii].grpin[jj] == grpin ) {
		new_grp = _matrix[ii].grpout[jj];
	        break;
	    }
        }
    }

    *grpout = new_grp;
}

/*=====================================================================*/
                                                                              
static Boolean pgfilw_cmpFzlRanges( char file1[], char file2[],
				    char range1[], char range2[] )
/************************************************************************
 * pgfilw_cmpFzlRanges                                                  *
 *                                                                      *
 * This function compares the freezing level ranges in two vg files.	*
 *                                                                      *
 * static Boolean pgfilw_cmpFzlRanges ( file1, file2, range1, range2 ) 	*
 *                                                                      *
 * Input parameters:							*
 *	file1[]		char	the first input vgf file name.		*
 *	file2[]		char	the second input vgf file name.		*
 *									*
 * Output parameters:							*
 *	range1[]	char	freezing level ranges in file1.		*
 *	range2[]	char	freezing level ranges in file2.		*
 *									*
 * Return parameters:                                                   *
 *                      Boolean	True if the two ranges are different.   *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		01/07	Created		                        *
 ***********************************************************************/
{
    Boolean fzl1, fzl2;
/*---------------------------------------------------------------------*/

    range1[ 0 ] = '\0';
    fzl1 = pggfaw_getFzlRangesFromFile( file1, range1 );

    range2[ 0 ] = '\0';
    fzl2 = pggfaw_getFzlRangesFromFile( file2, range2 );

    /*
     *  If freezing level ranges are found in both of the input file,
     *  and they are different, return False. Otherwise return True.
     */
    if ( fzl1 && fzl2 && strcasecmp( range1, range2 ) != 0 ) return False;
    else return True;

}

/*=====================================================================*/
                                                                              
static void pgfilw_showFzlRangeWarning( Widget parent, char infile[],
					char range1[], char range2[] )
/************************************************************************
 * pgfilw_showFzlRangeWarning                                           *
 *                                                                      *
 * This function creates the waring dialog when the appending vg file	*
 * has different freezing level ranges.					*
 *                                                                      *
 * static void pgfilw_showFzlRangeWarning ( parent, infile, 		*
 *					    range1, range2 ) 		*
 *                                                                      *
 * Input parameters:							*
 *	parent 		Widget	parent widget of the warning dialog.	*
 *	infile 		char	the appending vg file name.		*
 *	range1[]	char	freezing level ranges in work file.	*
 *	range2[]	char	freezing level ranges in append file.	*
 *									*
 * Return parameters:                                                   *
 * 	None					                        *
 *                        						*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		01/07	Created		                        *
 ***********************************************************************/
{
    int         oneRow = 25;
    char	btnStr[] = { "OK" }, lbl[ STD_STRLEN ];
                                                                  
    Widget      pane, btnForm, form1, tglBtn1, tglBtn2, okBtn;
    Widget      rangeWarningW = NULL;

    Boolean	hasRange1 = False;
/*---------------------------------------------------------------------*/
                                                                       
    /*
     *  Create the warning dialog
     */
    rangeWarningW = XmCreateFormDialog ( parent, "rangeWarning", NULL, 0);
                                                                     
    XtVaSetValues ( rangeWarningW,
                    XmNdefaultPosition, False,
                    XmNx,               200,
                    XmNy,               200,
                    XmNnoResize,        True,
                    XmNdialogStyle,     XmDIALOG_FULL_APPLICATION_MODAL,
                    NULL );
                                                                         
    XtVaSetValues ( XtParent ( rangeWarningW ),
                    XmNtitle,           "Warning: Inconsistent Freezing Level Ranges",
                    NULL );
                                                                
    /*
     *  Create a parent pane widget, a form, a warning message and a
     *  rowcol widget.
     */
    pane = XtVaCreateManagedWidget ( "wnpane",
                       xmPanedWindowWidgetClass, rangeWarningW,
                       XmNseparatorOn,           False,
                       XmNmarginHeight,          10,
                       XmNmarginWidth,           0,
                       XmNspacing,               5,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       NULL );
                                                                       
    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, pane, NULL );
                                                                       
    XtVaCreateManagedWidget ( "The freezing level ranges are not consistent. Please select one to use.",
                       xmLabelWidgetClass, form1,
                       XmNmarginRight,     100,
                       XmNx,               10,
                       NULL );
                                                                         
    /*
     *  Following three lines are used to set the initial status
     *  of the radio buttons.
     */
    hasRange1 = (strlen(range1)!=0);

    if ( hasRange1 ) strcpy ( _fzlRange, range1 );
    else strcpy ( _fzlRange, range2 );
                                                                       
    tglBtn1 = XtVaCreateManagedWidget ( "Current Work File ", 
    			xmToggleButtonWidgetClass, 	form1,
                        XmNhighlightThickness,      	0,
		 	XmNset,		   		hasRange1,
                       	XmNx,               		20,
                       	XmNy,               		oneRow,
                        NULL );

    XtVaCreateManagedWidget ( hasRange1? range1 : "NULL",
                       xmLabelWidgetClass, form1,
                       XmNmarginRight,     100,
                       XmNx,               20,
                       XmNy,               2*oneRow,
                       NULL );
                                                                            
    sprintf( lbl, "Input file %s", infile );
		       
    tglBtn2 = XtVaCreateManagedWidget ( lbl, 
    			xmToggleButtonWidgetClass, 	form1,
                        XmNhighlightThickness,      	0,
		 	XmNset,		   		!hasRange1,
                       	XmNx,               		20,
                       	XmNy,               		3*oneRow,
                        NULL );

    XtVaCreateManagedWidget ( strlen(range2)!=0? range2 : "NULL",
                       xmLabelWidgetClass, form1,
                       XmNmarginRight,     100,
                       XmNx,               20,
                       XmNy,               4*oneRow,
                       NULL );

    XtAddCallback ( tglBtn1, XmNarmCallback,
                (XtCallbackProc)pgfilw_fzlToggleBtnCb, (XtPointer)tglBtn2 );

    XtAddCallback ( tglBtn1, XmNvalueChangedCallback,
                (XtCallbackProc)pgfilw_fzlRangeChgCb, (XtPointer)range1 );

    XtAddCallback ( tglBtn2, XmNarmCallback,
                (XtCallbackProc)pgfilw_fzlToggleBtnCb, (XtPointer)tglBtn1 );

    XtAddCallback ( tglBtn2, XmNvalueChangedCallback,
                (XtCallbackProc)pgfilw_fzlRangeChgCb, (XtPointer)range2 );
                                                                           
    XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass, pane, NULL );
                                                                         
    /*
     *  Control buttons
     */
    btnForm = XtVaCreateManagedWidget ( "warningBtn",
                xmFormWidgetClass,              pane,
                NULL );
                                                                               
                                                                                  
    okBtn = XtVaCreateManagedWidget ( btnStr,
                xmPushButtonWidgetClass,        btnForm,
                XmNx,                		260,
                XmNwidth,			80,
                NULL );

    XtAddCallback ( okBtn, XmNactivateCallback,
                (XtCallbackProc)pgfilw_rangeWarningBtnCb, (XtPointer)NULL );

                                                                                 
    XtManageChild ( rangeWarningW );
                                                                                  
    XtVaSetValues ( XtParent( rangeWarningW ),
            XmNminWidth,        600,
            XmNmaxWidth,        600,
            XmNminHeight,       180,
            XmNmaxHeight,       180,
            NULL );
                                                                          
}

/*=====================================================================*/

/* ARGSUSED */ 
static void pgfilw_rangeWarningBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_rangeWarningBtnCb 						*
 *									*
 * This is the callback function for the ok button in the range warning.*
 *									*
 * void pgfilw_rangeWarningBtnCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data 		*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		01/07	Created					*
 ***********************************************************************/
{

   pgfilw_appendFzl();
                                                                  
}

/*=====================================================================*/

/* ARGSUSED */ 
static void pgfilw_fzlToggleBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_fzlToggleBtnCb						*
 *									*
 * Callback function for the radio buttons in the range warning dialog.	*
 *									*
 * static void pgfilw_fzlToggleBtnCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data 		*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/06	Created					*
 ***********************************************************************/
{
    XmToggleButtonCallbackStruct        *cbData;
    Widget                              chkBox;
/*---------------------------------------------------------------------*/
                                                                       
    cbData = (XmToggleButtonCallbackStruct *) cbs;
    chkBox = (Widget)clnt;

    XmToggleButtonSetState( chkBox, cbData->set, True );

}

/*=====================================================================*/

/* ARGSUSED */ 
static void pgfilw_fzlRangeChgCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfilw_fzlRangeChgCb							*
 *									*
 * Callback function for the radio buttons in the range warning dialog.	*
 *									*
 * static void pgfilw_fzlRangeChgCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data 		*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		01/07	Created					*
 ***********************************************************************/
{
    XmToggleButtonCallbackStruct        *cbData;
    char				*range;
/*---------------------------------------------------------------------*/
                                                                       
    cbData = (XmToggleButtonCallbackStruct *) cbs;
    range  = (char*)clnt;

    if ( cbData->set != 0 ) strcpy( _fzlRange, range );

}

/*=====================================================================*/

static void pgfilw_appendFzl ( void )
/************************************************************************
 * pgfilw_appendFzl		                                        *
 *                                                                      *
 * This function appends the input file to the work file and set the	*
 * freezing level ranges for all FZLVL elements.			*
 *                                                                      *
 * static void pgfilw_appendFzl ( )					*
 *                                                                      *
 * Input parameters:							*
 * 	None					                        *
 * Output parameters:							*
 * 	None					                        *
 * Return parameters:                                                   *
 * 	None					                        *
 *                        						*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		01/07	Created		                        *
 ***********************************************************************/
{
    int         ii, curr_grp, grpnums[MAX_GROUP_TYPE], cur_layer;
    int         ipos, ier, more, flag, orecsz, nrecsz, group_out;
    long        cursiz, curpos, maxbytes;
    char        fname[FILE_FULLSZ], infil[FILE_FULLSZ], outfil[FILE_FULLSZ];
    char        file_name[MXFLSZ], file_path[MXFLSZ];

    VG_DBStruct el;
    FILE        *ifp, *ofp;
    Boolean     initialAppend = False;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();
    pglayer_getFilePath ( cur_layer, file_path );
    pglayer_getFileName ( cur_layer, file_name );
        
    strcpy (fname, file_path);
    strcat (fname, file_name);

    cst_srch ( 0, (int)strlen(fname), ".vgf", fname, &ipos, &ier );
    if ( ier == -4 ) {

       strcat(fname, ".vgf");

    }

    /*
     *  If the origNameLen is > 0 then use the original file name
     *  and path as the default.
     *  If the origNameLen is == 0, then we'll use the new file as
     *  the default (see below).
     */
    if ( strlen( _origName ) > (size_t)0 ) {

       pglayer_setFileName ( cur_layer, _origName );
       pglayer_setFilePath ( cur_layer, _origPath );
       strcpy (_currPath, _origPath);
       _currDirPos = _origDirPos;

    }
    else {

      initialAppend = True;

    }

    XtUnmanageChild(_fileSelW);

    /*
     *  Load the transformation matrix to compress group numbers.
     */
    cvg_gmtrx ( fname, &_matrixSize, &_matrix, &ier );

    pglayer_getFileName ( cur_layer, file_name );

    /*
     *  For append where no other file has been opened
     *  use this file name/path as the defaults.
     */
    if ( initialAppend ) {

       strcpy (_origPath, _currPath);
       strcpy (_origName, file_name);
       _origDirPos = _currDirPos;

       pgfilw_showFileName();

    }

    /*
     *  Check the integrity of the input file & WORK_FILE.
     */
    cfl_inqr (fname, NULL, &maxbytes, infil, &ier);
    if ( ier < 0 ) {

       return;

    }

    cfl_inqr (cvg_getworkfile(), NULL, &cursiz, outfil, &ier);

    if ( maxbytes > 0 ) {

       /*
        * Open the input file and WORK_FILE for updating.
        */
       cvg_open (infil, G_FALSE, &ifp, &ier);
       if ( ier < 0 ) NxmErr_update();

       cvg_open (cvg_getworkfile(), G_TRUE, &ofp, &ier);
       if ( ier < 0 ) NxmErr_update();

       /*
        *  Reset the group number matrix to zeros.
        */
       for ( ii = 0; ii < MAX_GROUP_TYPE; ii++ ) {

           grpnums[ii] = 0;

       }

       /*
        *  Store the largest group number in WORK_FILE for each
        *  group type unless this is a replace and layering is not
        *  active (meaning only 1 layer is in use).
        */
       curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
       more = G_TRUE;

       while ( ( more ) && ( curpos < cursiz ) ) {

           cvg_rdhdr (outfil, ofp, (int)curpos, (int)cursiz, &el, &flag, &ier);

           if ( ier == 0 && el.hdr.recsz > 0 ) {

              orecsz = el.hdr.recsz;

              if ( (int) el.hdr.grptyp != 0 ) {

                 curr_grp = (int)el.hdr.grptyp;

                 if ( grpnums[curr_grp] < el.hdr.grpnum ) {
			
                    grpnums[curr_grp] = el.hdr.grpnum;

                 }
              }

              curpos += (long)orecsz;

           }

           else {

              more = G_FALSE;

           }
       }

       /*
        *  Load valid elements from input file into WORK_FILE.
        *  Reassign group numbers & build range records.
        */
       more = G_TRUE;
       curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);

       cfl_inqr (cvg_getworkfile(), NULL, &cursiz, outfil, &ier);
       cfl_seek(ofp, cursiz, 0, &ier);

       while ( ( more ) && ( curpos < maxbytes ) ) {

             cvg_rdhdr (infil, ifp, (int)curpos, (int)maxbytes, &el, &flag, &ier);

             if ( ier == 0 && el.hdr.recsz > 0 ) {

                orecsz = el.hdr.recsz;

                cvg_rdele (&el, (int)curpos, el.hdr.recsz, ifp, &ier);

                if ( ier != 0 ) {
			
                   more = G_FALSE;
		   
                }

                /*
                 * Skip deleted elements & file-head element.
                 */
                if ( ( more ) && ( el.hdr.delete == 0 ) &&
                     ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

                    nrecsz = el.hdr.recsz;

                    /*
                     * Renumber the groups as needed.
                     */
                    if ( (int)el.hdr.grptyp != 0 ) {

                       curr_grp = (int)el.hdr.grptyp;
                       pgfilw_renumberGrp ( el.hdr.grptyp,
                                            el.hdr.grpnum, &group_out );

                       el.hdr.grpnum = grpnums[curr_grp] + group_out;

                    }

                    cvg_write (&el, -2, nrecsz, ofp, FALSE, &ier);

                    /*
                     *  Build range record.
                     */
                    if ( ier == 0 ) {

                       crg_set (&el, (int)cursiz, cur_layer, &ier);

                    }

                    else {

                       more = G_FALSE;

                    }

                    cursiz = cursiz + (long)nrecsz; /* re-position in WORK_FILE */

                }

                /*
                 * Free TCA break point/GFA block memory
                 */
                if ( el.hdr.vg_type == TCA_ELM ) {

                   cvg_freeBkpts ( &el );

                }
                else if ( el.hdr.vg_type == GFA_ELM ) {

                   cvg_freeElPtr ( &el );
		   
                }

                curpos += (long)orecsz;  /* re-position in input file */

           }

           else {

                more = G_FALSE;

           }

        }

        /*
         *  Close files.
         */
        cfl_clos (ifp, &ier);
        cfl_clos (ofp, &ier);

   }

   /*
    *  Reset freezing level ranges.
    */
   pgfilw_setFzlRanges();
   
   /*  
    *  Redraw all and reset undo.
    */
   cvg_redraw (NULL, &ier);
   xpgrfrsh();

   pgundo_initUndo();

   /*
    *  reset file_saved & changes_made flags.
    */
   pglayer_setFileSaved( cur_layer,
                        (Boolean)(( !initialAppend ) ? TRUE : FALSE) );
   pglayer_setChngMade ( cur_layer,
                        (Boolean)(( !initialAppend ) ? TRUE : FALSE) );

   /*
    *  Free the _matrix memory, and reset _matrixSize.
    */
   cvg_gfrmtrx (_matrix, &_matrixSize);

}

/*=====================================================================*/

static void pgfilw_setFzlRanges( void )
/************************************************************************
 * pgfilw_setFzlRanges		                                        *
 *                                                                      *
 * This function sets the freezing level ranges in the work file to	*
 * the value stored in _fzlRange.               			*
 *                                                                      *
 * static void pgfilw_setFzlRanges ( )					*
 *                                                                      *
 * Input parameters:							*
 * 	None					                        *
 * Output parameters:							*
 * 	None					                        *
 * Return parameters:                                                   *
 * 	None					                        *
 *                        						*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		01/07	Created		                        *
 ***********************************************************************/
{
    int         ier, nextEl, curPos, np, cur_layer;
    long        fileSize, newSize;
    char        vgfname[ 128 ], haz[ 128 ], ranges[ STD_STRLEN ];

    FILE        *fptr;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    /*
     *  Open the work file to update.
     */
    cvg_open ( cvg_getworkfile(), 1, &fptr, &ier );
    if ( ier < 0 ) return;

    cfl_inqr ( cvg_getworkfile(), NULL, &fileSize, vgfname, &ier );
    if ( ier < 0 ) return;

    cur_layer = pglayer_getCurLayer();
    curPos  = 0;
    nextEl  = 0;
    ier     = 0;

    /*
     *  Loop through all elements. If the element is an FZLVL
     *  and its freezing level ranges are different from _fzlRange,
     *  set its ranges to _fzlRange.
     */
    while ( nextEl < MAX_EDITABLE_ELEMS && curPos < fileSize )  {

        cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

        if ( ier != 0 ) break;

        if ( el.hdr.recsz > 0 )  {

           if ( (int)el.hdr.vg_type == GFA_ELM &&
                    !el.hdr.delete )  {

              cvg_getFld ( &el, TAG_GFA_AREATYPE, haz, &ier );

	      if ( strcasecmp( haz, "FZLVL" ) == 0 ) {

                 cvg_getFld( &el, TAG_GFA_FZLRANGE, ranges, &ier );
		 
                 if ( ( ier == 0 ) && strcasecmp( ranges, _fzlRange ) != 0 ) {

		     /*
		      *  Set the delete flag to True.
		      */
		     cvg_delet( vgfname, curPos, FALSE, &ier);

		     /*
		      * Set range info for el.
		      */
                     cvg_setFld( &el, TAG_GFA_FZLRANGE, _fzlRange, &ier );

    		     np = el.elem.gfa.info.npts;
    		     el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof(int) * 2 +
                			    sizeof(char) * STD_STRLEN * 
					    el.elem.gfa.info.nblocks ) +
                			    sizeof(float) * np * 2;

    		     cfl_inqr ( cvg_getworkfile(), NULL, &newSize, vgfname, &ier );

		     /*
		      *  Write el at the end of the work file.
		      */
                     cvg_write ( &el, -1, el.hdr.recsz, fptr, FALSE, &ier);

		     /*
		      *  Reset range record.
		      */
                     if ( ier == 0 ) {

                            crg_set (&el, (int)newSize, cur_layer, &ier);

                     }

                 }

	      }

           }

	   if ( (int)el.hdr.vg_type == GFA_ELM ) {

	      cvg_freeElPtr ( &el );

	   }

           curPos += el.hdr.recsz;

        }

        nextEl++;

    }                                       /* read element loop  */

    cvg_clos ( fptr, &ier );
                                                       
}

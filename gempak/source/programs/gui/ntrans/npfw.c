#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmsdef.h"
#include "ctbcmn.h"
#include "model.h"
#include "interface.h"
#include "panel.h"
#include "panelstr.h"

#define VISIBLE_ITEM	  10 
#define MAX_FILES	1000

#define SORT_BY_NAME	   0
#define SORT_BY_DATE	   1

#define RESTORE_POPUP	   0
#define SAVE_POPUP	   1

#define NPF_FILE_EXT	".npf"
#define FILE_PATTERN	"*.npf"
#define LOCAL_DIR	"./"
#define CWD_TITLE	"Local"

typedef XmFileSelectionBoxCallbackStruct FSBCBS;

static Widget	_fileSelW;
static Widget	_dirSel_listW;
static Widget	_fileSel_listW;
static Widget	_fileSel_txtW;

static Widget	_browsePopup;
static Widget	_sortRadioW;
static WidgetList _sortBtn;

static Widget	_sortPaneW;
static Widget	_selectPaneW;
static Widget	_inputPaneW;
static Widget	_browseBtnPane;
static Widget	_bottomPaneW;
static Widget	_restoreConfW;
static Widget	_rstContentW;
static Widget	_rstConfLblW;

static char	_fileName[MXFLSZ]	= "\0";
static char	_dirPath[LLPATH]        = LOCAL_DIR;
static int	_dirPos;

static int	_npfilFunc;	/* RESTORE_NPF,  SAVE_NPF */
static int	_sortBy;	/* SORT_BY_NAME or SORT_BY_DATE  */


/*
 *  NPF user info data struct
 */
typedef struct {
        char	*title;		/* title name */
        char	*usrpath;	/* full path to this user's NPF dir */
} npfusr_ent_t;

/*
 *  NPF user info table struct
 */
typedef struct {
    int			nitems;	/* total # of users*/
    npfusr_ent_t	*items;	/* pointer to the array of user items */
} npfutbl_t;

static npfutbl_t	_npfUsrTbl;

/*
 *  private callback functions
 */
void npfw_browseBtnCb	( Widget, XtPointer, XtPointer );
void npfw_browseDoneCb	( Widget, XtPointer, XtPointer );
void npfw_btmCtlBtnCb	( Widget, long, XtPointer );
void npfw_confirmCb	( Widget, XtPointer, XtPointer );
void npfw_rstConfCb	( Widget, XtPointer, XtPointer );
void npfw_selectCb	( Widget, XtPointer, XtPointer );
void npfw_selectDirCb	( Widget, XtPointer, XtPointer );
void npfw_sortByCb	( Widget, XtPointer, XtPointer );
void npfw_txtCb		( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
static void npfw_checkPerms ( char *cfile, Boolean *can_read, 
                                           Boolean *can_write );
void npfw_createConf ( Widget parent );
void npfw_formatConf ( long flen, char *npftext, char *filstr, int *iret );
static void npfw_getFileName ( char *file_name );
void npfw_getGroup ( int src, int filen, int *groupn, int *iret );
static void npfw_getSource ( int src, int *modeln, int *filen, int *iret );
static void npfw_managePanes ( int popup_type );
void npfw_popdown ( void );
void npfw_popdownConf( void );
void npfw_popupConf( void );
static void npfw_preCheck ( void );
static void npfw_readUsrTbl ( char *tblname, npfutbl_t *npfutbl, int *iret );
static void npfw_restoreNPF ( void );
static void npfw_saveNPF ( void );  
static void npfw_setFileList ( void );
static void npfw_unmanagePanes ( void );

/************************************************************************
 * npfw.c								*
 *									*
 * This module defines the NPF file selection (RESTORE/SAVE) in Data 	*
 * Selection.								*
 *									*
 * CONTENTS:								*
 *  npfw_create()		creates file selection pallette		*
 *  npfw_popup()		popup the palette window		*
 *  npfw_popdown()		put down the palette window		*
 *  npfw_getFileName()		gets the full name of selected NPF file	*
 *  npfw_getSource() 		gets a specified source in a given loop	*
 *									*
 *  npfw_browseBtnCb()		callback for the browse button		*
 *  npfw_browseDoneCb()		callback for browse OK/Cancel buttons	*
 *  npfw_btmCtlBtnCb()		callback for bottom control buttons	*
 *  npfw_confirmCb()		callback for RESTORE/SAVE confirmation	*
 *  npfw_selectCb()		callback for file list			*
 *  npfw_selectDirCb()		callback for directory list		*
 *  npfw_sortByCb() 		callback for sort by toggles		*
 *  npfw_txtCb()		callback for text input			*
 *									*
 *  npfw_checkPerms()		checks file permissions			*
 *  npfw_managePanes()		manages the appropiate children of pane	*
 *  npfw_preCheck()		checks the file before restoring/saving	*
 *  npfw_restoreNPF()		restores the specified NPF file		*
 *  npfw_setFileList()		sets the file list for the _dirPath	*
 *  npfw_readUsrTbl()		reads NPF file user table "npf.nmap"	*
 *  npfw_saveNPF()		saves settingts into an NPF file	*
 *  npfw_unmanagePanes()	unmanages all the children of pane	*
 *									*
 *  npfw_createConf()		creates restore confirmation pallette	*
 *  npfw_popupConf()		pops up restore confirmation window	*
 *  npfw_popdownConf()		pops down restore confirmation window 	*
 *  npfw_rstConfCb()		callback for restore confirmation	*
 *  npfw_formatConf()		formats source string for restore conf. *
 *  npfw_saveSrcAttr()     	saves a data source's attr. settings	*
 ************************************************************************/

/*=====================================================================*/

void npfw_create ( Widget parent )
/************************************************************************
 * npfw_create								*
 *									*
 * This function creates a file selection dialog box.			*
 *									*
 * void npfw_create ( parent )						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 * Log:									*
 * C. Bailey/HPC	04/05	Copy from nmap2_spfw.c			*
 ***********************************************************************/
{
    int			toff = 5, ier;
    long		ii;
    char		*btm_btnstr[] = {"OK", "Cancel"}; 
    char		*sort_str[] = {"Name", "Date"};
    Widget		pane, label, label2, frame, sort_label;
    Arg         	args[10];
    Cardinal    	argcnt, nn;
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
    pane = XtVaCreateWidget("npf_pane",
                    xmPanedWindowWidgetClass, _fileSelW,
                    XmNsashWidth,             1,
                    XmNsashHeight,            1,
                    NULL);

 /*
  * ******** SORT PANE ********
  * create a form widget to hold the sort by toggles
  */
    _sortPaneW = XtVaCreateWidget("npfw_form",
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

    for (ii = 0; ii < (long)nn; ii++) {
        _sortBtn[ii] = XtVaCreateManagedWidget (sort_str[ii],
		    xmToggleButtonWidgetClass,      _sortRadioW,
                    XmNtraversalOn,                 FALSE,
		    NULL);
	XtAddCallback(_sortBtn[ii], XmNdisarmCallback,
	            npfw_sortByCb, (XtPointer) ii);

	XmToggleButtonSetState (_sortBtn[ii], (_sortBy == ii), FALSE);
    }

    XtManageChild(_sortPaneW); 

 /*
  * ******** SELECT PANE ********
  * create a form widget to hold the directory and file lists
  */
    _selectPaneW = XtVaCreateWidget("npfw_selectform",
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
		   npfw_selectDirCb, NULL );


    npfw_readUsrTbl("spf.nmap", &_npfUsrTbl, &ier);

    if ( _npfUsrTbl.nitems > 0) {

        xmfils = (XmStringTable) XtMalloc 
	    ((size_t)_npfUsrTbl.nitems * sizeof (XmString *));

        for (ii = 0; ii < _npfUsrTbl.nitems; ii++) {
	    xmfils[ii] = XmStringCreateLocalized (_npfUsrTbl.items[ii].title);
        }

        XtVaSetValues (_dirSel_listW,
		       XmNitems,	xmfils,
		       XmNitemCount,	_npfUsrTbl.nitems,
		       NULL);

        for (ii = 0; ii < _npfUsrTbl.nitems; ii++) {
	    XmStringFree (xmfils[ii]);
        }
	XtFree ((XtPointer)xmfils);

/*
 * hi-light the local directory
 */
	_dirPos = _npfUsrTbl.nitems;
    }

    XtManageChild( _dirSel_listW );
    XtManageChild( frame );

    
/*
 * create the file list
 */
    label = XmCreateLabel( _selectPaneW, "Select NPF File name:", NULL, 0 );
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
		   npfw_selectCb, NULL);


    XtManageChild (_fileSel_listW);
    XtManageChild (frame);

    XtManageChild(_selectPaneW);

/*
 * ******** INPUT PANE ********
 * create the file input
 */
    _inputPaneW = XtVaCreateWidget("npfw_inputForm",
				   xmFormWidgetClass, 	pane,
				   NULL);
    XtVaSetValues( _inputPaneW,
		    XmNnoResize,     TRUE,
		    XmNautoUnmanage, FALSE,
		    NULL );

    label2 = XmCreateLabel (_inputPaneW, "Or enter an NPF file name:", NULL, 0);
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

    _fileSel_txtW = XmCreateText (_inputPaneW, "npfw_inputText", NULL, 0);
    XtManageChild (_fileSel_txtW);
    XtVaSetValues (_fileSel_txtW,
		   XmNtopAttachment,   XmATTACH_WIDGET,
		   XmNtopWidget,       label2,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNleftAttachment,  XmATTACH_FORM,
		   NULL);
    XtAddCallback (_fileSel_txtW, XmNactivateCallback,
		   npfw_txtCb, NULL ); 

    XtManageChild (_inputPaneW);

/*
 * ******** BROWSE PANE ********
 */
    _browseBtnPane = 
	XtVaCreateManagedWidget ("Browse",
				 xmPushButtonWidgetClass, pane,
				 NULL);
    XtAddCallback(_browseBtnPane, XmNarmCallback,
		  npfw_browseBtnCb, (XtPointer) NULL); 

/*
 * ******** BOTTOM CONTROL PANE ********
 */
    _bottomPaneW = 
	(Widget) NxmCtlBtn_create (pane, 1, "npfw_btm", 
				   XtNumber(btm_btnstr), btm_btnstr, 
				   (XtCallbackProc)npfw_btmCtlBtnCb, NULL);

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
		  npfw_browseDoneCb, (XtPointer) 0);
    XtAddCallback(_browsePopup, XmNokCallback,
		  npfw_browseDoneCb, (XtPointer) 1);

    XtUnmanageChild(XmFileSelectionBoxGetChild(_browsePopup, 
					       XmDIALOG_HELP_BUTTON));

/*
 *  Restoring confirmation popup     
 */
    npfw_createConf( pane );
}

/*=====================================================================*/

void npfw_popup ( int func )
/************************************************************************
 * npfw_popup								*
 *									*
 * This function loads and displays the NPF file names into the file	*
 * selection dialog box.						*
 *									*
 * void npfw_popup ( func )						*
 *									*
 * Input parameters:							*
 *	func		int		Action function			*
 *					    0 - RESTORE_NPF		*
 *					    1 - SAVE_NPF		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	Copy from nmap_spfw.c	 		*
 ***********************************************************************/
{
    int		ipos;
    XmString	xmstr, dirstr;
/*---------------------------------------------------------------------*/

    _npfilFunc = func;

/*
 * set the dialog title 
 */
    if ( func == SAVE_NPF ) {
	xmstr = XmStringCreateLocalized("Save To NPF File");
	dirstr = XmStringCreateLocalized(LOCAL_DIR);
	XtVaSetValues(_browsePopup, XmNdirectory, dirstr, NULL);
	XmStringFree (dirstr);

	npfw_managePanes (SAVE_POPUP);
    }
    else {
	xmstr = XmStringCreateLocalized("Restore From NPF File");

	npfw_managePanes (RESTORE_POPUP);
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
    if (ipos > _npfUsrTbl.nitems) ipos = _npfUsrTbl.nitems;

    XmListSetBottomPos (_dirSel_listW, ipos);

/*
 * set input text widget
 */
    XmTextSetString( _fileSel_txtW, _fileName );

    npfw_setFileList ();

    XtManageChild(_fileSelW);

}

/*=====================================================================*/

void npfw_popdown ( void )
/************************************************************************
 * npfw_popdown								*
 *									*
 * This function puts the file window down. 				*
 *									*
 * void npfw_popdown ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	copy from spfw_popdown	 		*
 ***********************************************************************/
{
    npfw_popdownConf (); 
    
    if (XtIsManaged (_fileSelW)) {
    	XtUnmanageChild (_fileSelW);
    }

    if (XtIsManaged (_browsePopup)) {
    	XtUnmanageChild (_browsePopup);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void npfw_selectCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * npfw_selectCb							*
 *									*
 * Callback function for selecting an NPF File.				*
 *									*
 * void npfw_selectCb (wid, clnt, call)				*
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
 * C. Bailey/HPC	04/05	copy from spfw_selectCb	 		*
 * T. Piper/SAIC	09/06	Replaced XmStringGetLtoR with		*
 *					 XmStringUnparse		*
 ***********************************************************************/
{
    char * tmp_str;
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    tmp_str = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    strcpy (_fileName, tmp_str);
    XmTextSetString (_fileSel_txtW, _fileName);
    XtFree (tmp_str);

}

/*=====================================================================*/
/* ARGSUSED */
void npfw_selectDirCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * npfw_selectDirCb							*
 *									*
 * Callback function for selecting an NPF File.				*
 *									*
 * void npfw_selectDirCb (wid, clnt, call)				*
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
 * C. Bailey/HPC	04/05	modify from spfw_selectDirCb 		*
 ***********************************************************************/
{

    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;

/*---------------------------------------------------------------------*/

    _dirPos = cbs->item_position;

    if ( _dirPos == _npfUsrTbl.nitems ) {
        strcpy ( _dirPath, LOCAL_DIR );
    }
    else {
        strcpy ( _dirPath, _npfUsrTbl.items[(_dirPos) - 1].usrpath );
        if ( _dirPath[ ( strlen(_dirPath) - 1 ) ] != '/' ) {
	    strcat ( _dirPath, "/" );
        }
    }
    
    npfw_setFileList ( );

}

/*=====================================================================*/
/* ARGSUSED */
void npfw_txtCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * npfw_txtCb								*
 *									*
 * Callback function for text input widget.				*
 *									*
 * void npfw_txtCb ( wid, clnt, call )				*
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
 * C. Bailey/HPC	04/05	Modify from spfw_txtCb			*
 ***********************************************************************/
{
    npfw_btmCtlBtnCb( NULL, 0, NULL ); 
}

/*=====================================================================*/
/* ARGSUSED */ 
void npfw_confirmCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * npfw_confirmCb							*
 *									*
 * Callback function for confirming to restore/save an NPF file.	*
 *									*
 * void npfw_confirmCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	Modify from spfw_confirmCb		*
 ***********************************************************************/
{
    int		ier;
    char	npfile[MXFLSZ], *npftext, lblStr[256];
    char        newfil[LLPATH], tmpStr[256];
    char	prefs_tag[MAX_PREF_STR] = "RESTORE_POPUP";
    long        flen = 0;
    XmString	xmstr;
    const int	addFmtLen = 128; /* Additional string length required to
    				    hold the format message */
    Boolean	bal;
				    
/*---------------------------------------------------------------------*/

    if ( strlen(_fileName) > (size_t)0 ) {
	
	if ( _npfilFunc == RESTORE_NPF ) {
                    
/*
 *  Retrieve the selected NPF file and load into NPF buffer.
 */
            npfw_getFileName( npfile );
            npf_load ( npfile, &ier );

/*
 *  Pop up confirmation window with NPF file content in it.
 */
            if ( ier < 0 ) {	
	        NxmWarn_show ( _fileSelW, "Failed to load NPF file" );
            }
	    else {	     
/*
 * Query the actual NPF file size in order to deal with a
 * NPF file with any size.
 */
	        cfl_inqr(npfile, NULL, &flen, newfil, &ier);	        

                flen += (long)addFmtLen;
		if ( ier < 0 ) {	
	            NxmWarn_show ( _fileSelW, "Failed to query NPF file size" );
                }
                else {
                    if( ! (npftext = XtMalloc((Cardinal)flen))) {
	                NxmWarn_show ( _fileSelW, 
                                       "Failed to allocate space for NPF file" );
                    }
	            else {	     		        
			npfw_formatConf( flen, npftext, lblStr, &ier );

		        if ( ier == 0 ) {
		            XmTextSetString( _rstContentW, npftext );
		            xmstr = XmStringCreateLtoR(lblStr, 
                                                       XmFONTLIST_DEFAULT_TAG);
                            XtVaSetValues( _rstConfLblW,
                                          XmNlabelString, xmstr,
				          XmNalignment,	  XmALIGNMENT_BEGINNING,
				          NULL);
		            XmStringFree( xmstr );               

			    ctb_pfbool(prefs_tag, &bal, &ier);
		 	    if ( bal ) {
		                npfw_popupConf (); 
			    }
			    else {
    				npfw_popdownConf ();
    				npfw_popdown ();
    				npfw_restoreNPF ();
			    }

		        }
			else {
			    sprintf ( tmpStr, "No valid data sources found in %s\n", 
			                       _fileName ); 
			    NxmWarn_show ( _fileSelW, tmpStr );
			}
                      
                         XtFree(npftext);
                    }
		}	       
	    }
	}
	else {  
	     npfw_popdown ();
	     npfw_saveNPF ();
	}	     
    }    
}

/*=====================================================================*/
/* ARGSUSED */ 
void npfw_browseBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * npfw_browseBtnCb							*
 *									*
 * Callback function for the browse button on file selection palette.	*
 *									*
 * void npfw_browseBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	Copy from spfw_browseBtnCb		*
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
void npfw_browseDoneCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * npfw_browseDoneCb							*
 *									*
 * Callback function for the Ok/Cancel buttons on the browse palette. 	*
 *									*
 * void npfw_browseDoneCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	modify from spfw_browseDoneCb		*
 * T. Piper/SAIC	10/06	Replaced XmGetStringLtoR with		*
 *					 XmStringUnparse		*
 ***********************************************************************/
{
    int		ier;
    long	which;
    char 	*text;
    char	filepart[MXFLSZ] = "\0", pathpart[LLPATH] = "\0";
    Widget	popup;
    FSBCBS 	*fcbs;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    
    if ( which == 1 ) {	/* OK */
	
	fcbs = (FSBCBS *) cbs;
	text = XmStringUnparse (fcbs->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	
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
	    npfw_preCheck ( );
	}
       
    }
    else {  /* CANCEL */
	XtUnmanageChild ( _browsePopup );
    }
}

/*=====================================================================*/
/* ARGSUSED */ 
void npfw_sortByCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * npfw_sortByCb							*
 *									*
 * Callback function for the sort by toggles				*
 *									*
 * void npfw_sortByCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	copy from nmap_spfw.c			*
 ***********************************************************************/
{
    long  	new_sort;
/*---------------------------------------------------------------------*/

    new_sort = (long)clnt;

    if (new_sort != _sortBy) {
        _sortBy = new_sort;
 	npfw_popdown( );
        npfw_popup( _npfilFunc );
    }
}

/*=====================================================================*/

static void npfw_restoreNPF ( void )
/************************************************************************
 * npfw_restoreNPF							*
 *									*
 * This function restores the data settings from the NPF file. 		*
 *									*
 * npfw_restoreNPF ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	initial coding				*
 ***********************************************************************/
{
    int		col, row, selrow, selcol, npanels, vldset, ier;
    int		modeln, filen, groupn, src, vldsrc, n_frames;
    char	tagStr[32];
    char        colStr[16], rowStr[16], vldStr[10];

/*---------------------------------------------------------------------*/
/*
 *  Clear all current data settings 
 */
    ClearAreas(NULL,NULL,NULL);
    clearValidTime();

/*
 *  Read Multi Panel Dimensions From NPF file
 */
    sprintf (tagStr, "%s", "total_columns");
    spf_gtfld ( tagStr, colStr, &ier );
    col = atoi(colStr);
    
    sprintf (tagStr, "%s", "total_rows");
    spf_gtfld ( tagStr, rowStr, &ier );
    row = atoi(rowStr);
    
/*
 *  Set number of Panels
 */
    Mpanel.columns = col;
    Mpanel.rows = row;

    npanels = col*row;

    createMpFrame();

/*
 *  Find Panel Source with ValidTimeSet == 1 and load first
 */
    vldsrc = -1;
    for (src = 0; src < npanels; src++ ) {

        sprintf( tagStr, "%s%d%s", "panel", src+1, "_validTimeSet" );
	spf_gtfld ( tagStr, vldStr, &ier );
	vldset = atoi(vldStr);

	if ( vldset == 1 ) {
	    vldsrc = src;
	    npfw_getSource(src, &modeln, &filen, &ier);

	    if ( ier == 0 ) {

		strcpy(MetaFile,
			ModelStr[modeln].file_fullnames[filen]);
                
/*
 * Load MetaFile
 */
		OpenModel = 1;
		if(MetaFile[0] != '\0') {
		    if ( !MpanelMode )
			clear_window();
		    load_meta();
		}
/*
 *  Determine multi Panel location
 */
		sprintf (tagStr, "%s%d%s", "panel", src+1, "_column");
		spf_gtfld ( tagStr, colStr, &ier );
		selcol = atoi(colStr);

		sprintf (tagStr, "%s%d%s", "panel", src+1, "_row");
		spf_gtfld ( tagStr, rowStr, &ier );
		selrow = atoi(rowStr);

		Mpanel.selectCol = selcol;
		Mpanel.selectRow = selrow;

/*
 * get Group number
 */
		npfw_getGroup(src, filen, &groupn, &ier);

/*
 * Only load Group and save Panel Source information if
 * a valid group number (groupn > -1) is returned
 */

                if (groupn != -1) {
		  setValidTime();
                  setPanelSrc(src);

                  strcpy (panelSrc.group_name[src],GroupList[SelectGroupNo-1].groupname);
		  load_group(groupn);
                } else {
/* make loop buttons active */
                  n_frames = nFrame();
                  NxmChangePixmapData( PixmapData.current_pixmap,
                                        n_frames);
		  NxmLoopbuttonSensitive ( True );
                }
		break;
	    }
	}
    }

/* 
 * Load in remaining panels
 */
    for (src = 0; src < npanels; src++ ) {
        
/* 
 *  Get Meta File Source
 */
	if(vldsrc == src) {
	    continue;
	}

	npfw_getSource (src, &modeln, &filen, &ier);

	if (ier == 0 ) {

            strcpy(MetaFile,
                  ModelStr[modeln].file_fullnames[filen]);

	    OpenModel = 1;
            if(MetaFile[0] != '\0') {
                if ( !MpanelMode )
                    clear_window();
                load_meta();
            }

/*
 *  Determine multi Panel location
 */
            sprintf (tagStr, "%s%d%s", "panel", src+1, "_column");
            spf_gtfld ( tagStr, colStr, &ier );
            selcol = atoi(colStr);

            sprintf (tagStr, "%s%d%s", "panel", src+1, "_row");
            spf_gtfld ( tagStr, rowStr, &ier );
            selrow = atoi(rowStr);
        
	    Mpanel.selectCol = selcol;
            Mpanel.selectRow = selrow;

            npfw_getGroup(src, filen, &groupn, &ier);
            
/*
 * Only load Group and save Panel Source information if
 * a valid group number (groupn > -1) is returned
 */

            if(groupn != -1) {
	        setPanelSrc(src);
                strcpy (panelSrc.group_name[src],GroupList[SelectGroupNo-1].groupname);
                load_group(groupn);
            } else {
/* make loop buttons active */
		n_frames = nFrame();
		NxmChangePixmapData( PixmapData.current_pixmap,
                                        n_frames);
                NxmLoopbuttonSensitive ( True );
            }
        }
    }
}

/*=====================================================================*/

static void npfw_preCheck ( void )
/************************************************************************
 * npfw_preCheck							*
 *									*
 * This function gets the file name, checks the file permissions, and	*
 * confirms the name with the user.					*
 *									*
 * void npfw_preCheck( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	Modified from spfw_preCheck()		*
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
    if ( _npfilFunc == SAVE_NPF ) {
        ss = XmTextGetString ( _fileSel_txtW );
        strcpy ( _fileName, ss );
        XtFree(ss);
    }
    
    if ( strlen( _fileName ) <= (size_t)0 ) {
	NxmWarn_show ( _fileSelW, "No file has been specified" );
	return;    
    }
           
/*
 *  Retrieve full file name & append .npf extension if necessary.
 */
    strcpy ( fname, _dirPath );
    strcat ( fname, _fileName );
    
    cst_srch ( 0, (int)strlen(_fileName), ".npf", _fileName, &ipos, &ier );
    if ( ier == -4 ) {
        strcat ( fname, ".npf" );
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
    npfw_checkPerms ( fname, &readable, &writable );

    popup = ( XtIsManaged (_browsePopup) ) ? _browsePopup : _fileSelW;
    
    if ( _npfilFunc == RESTORE_NPF ) {
	
	if ( readable ) {
            npfw_confirmCb( popup, 0, NULL );
	}
	else { 
	    NxmWarn_show ( popup, "NPF File is not readable" );
	}
	
    }
    else {   /* SAVE_NPF */
    
        if ( writable ) {
            sprintf ( warning,
	        "Are you sure you want to save the current settings to NPF file %s?",
	         fptr );        	
            NxmConfirm_show ( popup, warning, npfw_confirmCb, 
		      NULL, NULL, &ier );
	}
	else {
	    NxmWarn_show ( _fileSelW, "No permission to write NPF File" );
        }    
    }    
}

/*=====================================================================*/

static void npfw_saveNPF ( void )
/************************************************************************
 * npfw_saveNPF								*
 *									*
 * This function saves the data settings into the NPF file.		*
 *									*
 * void npfw_saveNPF ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	04/05	initial coding				*
 * K. Tyle/UAlbany	11/10	increase dimension of lpStr		*
 ***********************************************************************/
{
    int		model_no, file_no, ier, ipos, hlen, ii;
    int		noc1, noc2, kk, clen, timeset, i;
    int		npanels, check; 
    char	full_name[FILE_FULLSZ];
    char	tagStr[32] = "\0", dataStr[200] = "\0", tmp[20] = "\0", tmp1[20] = "\0";
    char	lpStr[24] = "\0", cycStr[] = "[cycle_date][cycle_hour]";
    char        cycStr1[] = "[cycle_date]", hrStr[] = "[cycle_hour]";
    FILE	*fptr;
       
/*---------------------------------------------------------------------*/
/*
 *  If there are no active sources, return; otherwise, save them.
 */ 

    if ( strlen(_fileName) > (size_t)0 ) {

	strcpy ( full_name, _dirPath );
	strcat ( full_name, _fileName );
/*
 * Append .npf extension if necessary.
 */
        cst_srch ( 0, (int)strlen(_fileName), ".npf", _fileName, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat (full_name, ".npf");
	    strcat (_fileName, ".npf");
	}

/*
 *  Create the NPF file new to wipe out any previous inf.
 */
        fptr = npf_create ( full_name, &hlen, &ier );

        if ( ier < 0 ) {
	    
	    NxmWarn_show ( _fileSelW, "Failure to write!" );
	    return;	

	}
	
        sprintf( lpStr, "%s", "!\n! COLUMN X ROW \n!\n" );
        cfl_writ( fptr, (int)strlen(lpStr), (unsigned char*)lpStr, &ier );
     
        sprintf( tagStr, "%s", "total_columns");
	sprintf(dataStr, "%d",Mpanel.columns);
        spf_write( fptr, tagStr, dataStr, &ier );

	sprintf( tagStr, "%s", "total_rows");
        sprintf(dataStr, "%d",Mpanel.rows);
        spf_write( fptr, tagStr, dataStr, &ier );

        npanels = panelSrc.panelNo;
/*
 *  Write the selected data sources into the NPF file.
 */
	for ( ii = 0; ii < npanels; ii++ ) {
            
	    sprintf( lpStr, "%s%d%s", "!\n! Panel ", ii+1, "\n!\n" ); 
	    cfl_writ( fptr, (int)strlen(lpStr), (unsigned char*)lpStr, &ier );
	        
	    sprintf( tagStr, "%s%d%s", "panel", ii+1, 
			                         "_source"); 
	    if (panelSrc.flag[ii] == 1) {
                model_no = panelSrc.model_no[ii];
                file_no = panelSrc.file_no[ii];

/*
 * Get Model and File Numbers from Panel_srcStr
 */
		strcpy(dataStr,  ModelStr[model_no].model_name);
                strcat(dataStr, "/");
                strcat(dataStr,  ModelStr[model_no].file_names[file_no-1]);

/*
 * Find Date String in Filename
 */
		check = 0;
		i = -1;
		while ( check == 0) {
                    i++;
		    cst_nocc ( dataStr, '_', 1+i, 0, &noc1, &ier );
		    cst_nocc ( dataStr, '_', 2+i, 0, &noc2, &ier );
                    if (ier != 0 ) {
		        noc2 = strlen(dataStr);
		    }
		    
		    for ( kk = noc1+1; kk < noc2; kk++ ) {
		        tmp[ kk-noc1-1 ] = dataStr[ kk ];
		    }
		    tmp[ noc2-noc1-1 ] = '\0';          

                    clen = strlen(tmp);
		    check = atoi(tmp);
		} 

/* 
 * Check Length of Date String and Replace with [cycle_date]
 * and [cycle_hour]
 */
		if ( clen == 10 ) {
                    cst_rpst( dataStr, tmp, cycStr, dataStr, &ier );
                } else if ( clen == 8 ) {

/* 
 * Cycle Date and Hour Seperated by '_', Find Hour and 
 * replace with [cycle_hour]
 */
		    cst_rpst( dataStr, tmp, cycStr1, dataStr, &ier );
                    
		    cst_nocc ( dataStr, '_', 3+i, 0, &noc1, &ier );
		    cst_nocc ( dataStr, '_', 4+i, 0, &noc2, &ier );
		    
		    if (ier != 0) {
			noc2 = strlen(dataStr);
		    }

		    for ( kk = noc1+1; kk < noc2; kk++ ) {
                	tmp1[ kk-noc1-1 ] = dataStr[ kk ];
                    }
                    tmp1[ noc2-noc1-1 ] = '\0';

/* 
 * check if string tmp1 is a number. 
 * If check = 0, make sure string tmp1 = "00"
 */  
		    check = atoi(tmp1);
		    
		    if(check != 0 || strcmp(tmp1,"00") == 0) {
		        cst_rpst( dataStr, tmp1, hrStr, dataStr, &ier );
                    }
		}
		
/* 
 * Write Source to NPF file with cycle date/time replaced
 */
		spf_write( fptr, tagStr, dataStr, &ier );
		
/*
 * Write ValidTimeSet to NPF file
 */
		sprintf( tagStr, "%s%d%s", "panel", ii+1,
                                             "_validTimeSet");
                if(panelSrc.valid_time[ii].valid_tm_set == 0) {
                    timeset = 0;
                } else {
                    timeset = 1;
                }
		sprintf( dataStr, "%d", timeset );
		spf_write( fptr, tagStr, dataStr, &ier );

/*
 *  Write out the source's row and column location.
 */				    
		sprintf( tagStr, "%s%d%s", "panel", ii+1,
		                             "_column"); 
		sprintf( dataStr, "%d", panelSrc.column[ii] );  
		spf_write( fptr, tagStr, dataStr, &ier );
		sprintf( tagStr, "%s%d%s", "panel", ii+1,
		                              "_row"); 
		sprintf( dataStr, "%d", panelSrc.row[ii] );  
		spf_write( fptr, tagStr, dataStr, &ier );

/*
 * Write Group Name to NPF file
 */
                sprintf( tagStr, "%s%d%s", "panel", ii+1,
                                              "_group");

                sprintf( dataStr, "%s", panelSrc.group_name[ii] );
                spf_write( fptr, tagStr, dataStr, &ier );
		    
	    }
		
	} /* end of srcNum loop */
                
        spf_close( fptr, &ier );

    }
}

/*=====================================================================*/

static void npfw_managePanes ( int popup_type )
/************************************************************************
 * npfw_managePanes							*
 *									*
 * This function manages the appropiate children of the main pane	*
 * widget based on popup_type.						*
 *									*
 * void npfw_managePanes ( popup_type )					*
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
 * C. Bailey/HPC	05/05	modified from spfw_managePanes()	*
 ***********************************************************************/
{

    npfw_unmanagePanes ();

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
	    XtManageChild (_bottomPaneW);
	    break;
   }
}

/*=====================================================================*/

static void npfw_unmanagePanes ( void )
/************************************************************************
 * npfw_unmanagePanes							*
 *									*
 * This function unmanages all the children of the main pane widget.	*
 *									*
 * void npfw_unmanagePanes ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	copy from spfw_unmanagePanes()		*
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
} 

/*=====================================================================*/

static void npfw_setFileList ( void )
/************************************************************************
 * npfw_setFileList							*
 *									*
 * This function sets up the list of files for the slected path.	*
 *									*
 * void npfw_setFileList ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	copy from spfw_setFileList()		*
 ***********************************************************************/
{
    int		ii, nf;
    char	fn_list[MAX_FILES][MXFLSZ];
    XmString	xmstr;
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/
/*
 * Wipe the list of .npf filenames       
 */
    XmListDeleteAllItems(_fileSel_listW);

/*
 *  Get list of file names in the directory & write to the fn_list
 */
    nf = cfl_gfil (_sortBy, MAX_FILES, _dirPath, ".npf", fn_list);

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
void npfw_btmCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * npfw_btmCtlBtnCb							*
 *									*
 * Callback function for the bottom control buttons on the file 	*
 * selection popup window.  						*
 *									*
 * void npfw_btmCtlBtnCb  ( wid, which, call )				*
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
 * C. Bailey/HPC	05/05	copy from spfw_btmCtlBtnCb		*
 ***********************************************************************/
{
    switch ( which ) {
	case 0:		/* OK */	    
	    npfw_preCheck();	    
	    break;
	case 1:		/* CANCEL */
    	    npfw_popdown( ); 
	    break;
    }
}

/*=====================================================================*/

static void npfw_checkPerms ( char *cfile, Boolean *can_read, Boolean *can_write )
/************************************************************************
 * npfw_checkPerms							*
 *									*
 * This function checks the permissions on the passed in file to see if	*
 * the user has read and write permission.  If the file is not found,	*
 * can_read is FALSE, and the write permission of the directory is	*
 * checked.  If the directory is not found, can_write is FALSE.	Due to	*
 * policy, the user is only allowed to write in a directory owned by	*
 * user.								*
 *									*
 * void npfw_checkPerms (cfile, can_read, can_write)			*
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
 * C. Bailey/HPC	05/05	copy from spfw_checkPerms()		*
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

static void npfw_readUsrTbl ( char *tblname, npfutbl_t *npfutbl, int *iret )
/************************************************************************
 * npfw_readUsrTbl							*
 *									*
 * This routine will read NPF user table and create the data structure	*
 * to store the information.						*
 *									*
 * void npfw_readUsrTbl( tblname, npfutbl, iret)                        *
 *									*
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *									*
 * Output parameters:                                                   *
 *      *npfutbl        npfutbl_t   	pointer to npf user table struct*
 *      *iret		int             0 - success                     *
 *									*
 * Return code:                                                         *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	modify from spfw_readUsrTbl()		*
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
    npfutbl->nitems = (int)nn;
    npfutbl->items  = (npfusr_ent_t *) malloc ((nn+1) *sizeof (npfusr_ent_t));

    if (nn > (size_t)0) {
	rewind(fp);

	ii = 0;
	while ( ii < nn ) {
	    cfl_trln( fp, 256, buffer, &ier );

	    if (ier == 0) {
		sscanf(buffer, format, title, path);

		npfutbl->items[ii].title = (char *) malloc (strlen (title) + 1);
		strcpy( npfutbl->items[ii].title, title );

		npfutbl->items[ii].usrpath = (char *) malloc (strlen(path) + 1);
		strcpy( npfutbl->items[ii].usrpath, path );

		ii++;
	    }
	}
    }

    if (fp) fclose(fp);

/*
 * set default user
 */
    getcwd (cwd, sizeof (cwd));

    ii = (size_t)npfutbl->nitems;
    npfutbl->items[ii].title = (char *) malloc (strlen (CWD_TITLE) + 1);
    strcpy (npfutbl->items[ii].title, CWD_TITLE);

    npfutbl->items[ii].usrpath = (char *) malloc(strlen(cwd) + 1);
    strcpy ( npfutbl->items[ii].usrpath, cwd );

    nn = (size_t)cfl_gfil (0, MAX_FILES, cwd, NPF_FILE_EXT, NULL);

    npfutbl->nitems++;
}

/*=====================================================================*/

static void npfw_getFileName ( char *file_name )
/************************************************************************
 * npfw_getFileName							*
 *									*
 * This function gets the file name, including the full path of the file*
 *									*
 * void npfw_getFileName ( file_name )					*
 *									*
 * Input parameters:                                                    *
 *	None								*
 *									*
 * Output parameters:                                                   *
 *	*file_name	char	full name of the selected NPF file	*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	copy spfw_getFileName			*
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
 *  Append .npf extension if necessary.
 */
        cst_srch ( 0, (int)strlen(_fileName), ".npf", _fileName, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat ( file_name, ".npf" );
        }
    }
}

/*=====================================================================*/

void npfw_createConf ( Widget parent )
/************************************************************************
 * npfw_createConf							*
 *									*
 * This function creates the restore confirmation popup window.		*
 *									*
 * void npfw_createConf( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget			*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	Modified from spfw_createConf		*
 ***********************************************************************/
{
    Arg		args[10];
    long	ii;
    Cardinal	kk, nn;
    Widget	pane, form, ctl_form;    
    WidgetList	ctl_btns;
    char	npftext[] = "\0", lblStr[] = "\0";
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
 * create a scrolled text in a form to list NPF file content
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
    XtSetArg(args[kk], XmNvalue,	    	npftext); kk++; 
    
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
    
    for ( ii = 0; ii < (long)nn; ii++) {

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
		npfw_rstConfCb, (XtPointer)ii);
    }
    XtFree((XtPointer)ctl_btns); 
    XtManageChild( pane );

}
/*=====================================================================*/

void npfw_popupConf ( void )
/************************************************************************
 * npfw_popupConf							*
 *									*
 * This function pops up the NPF restore confirmation window		*
 *									*
 * void npfw_popupConf ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	copy spfw_popupConf			*
 ***********************************************************************/
{
    XtManageChild( _restoreConfW );
}

/*=====================================================================*/

void npfw_popdownConf ( void )
/************************************************************************
 * npfw_popdownConf							*
 *									*
 * This function puts the restore confirmation down. 			*
 *									*
 * void npfw_popdownConf ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	copy spfw_popdownConf			*
 ***********************************************************************/
{
    if (XtIsManaged (_restoreConfW)) {
    	XtUnmanageChild (_restoreConfW);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void npfw_rstConfCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * npfw_rstConfCb							*
 *									*
 * Callback function for the restore confirmation window. 		*
 *									*
 * void npfw_rstConfCb (wid, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data 		*
 *	call		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	05/05	modify from spfw_rstConfCb		*
 ***********************************************************************/
{
    long		which;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    npfw_popdownConf ();
    
    if ( which == 0 ) {	
        npfw_popdown ();
        npfw_restoreNPF ();
    }
}

/*=====================================================================*/

static void npfw_getSource ( int src, int *modeln, int *filen, int *iret )
/************************************************************************
 * npfw_getSource 							*
 *									*
 * This function gets a specified source for a given panel.		*
 *									*
 * void npfw_getSource ( src, modeln, filen, iret ) 			*
 *									*
 * Input parameters:							*
 *    src	int		Source number				*
 *									*
 * Output parameters:							*
 *    *modeln	int		Model number				*
 *    *filen	int		Model file number			*
 *    *iret	int		Return code				*
 *				    0 - Normal				*
 *				   -1 - Source doesn't exist in NPF file*
 *				   -2 - Source is not available 	*
 *									*
 **				    					*
 * Log:									*
 * C. Bailey/HPC	05/05	initial coding				*
 ***********************************************************************/
{
    int		ier, ii, noc, noc1, noc2, noc3, len, streq;
    int         itime, jj, kk, flen, tlen, model_num, file_num;
    char	tagStr[32], dataStr[200], tmpstr[12];
    char	grdStr[16], moslst[1024], dataStr1[200];
    char	cycTimStr[12], cycDatStr[10], cycHrStr[2];
    char	cycStr[] = "[cycle_date]", hourStr[] = "[cycle_hour]";
    char        timStr[12], hrStr[2], fulltmStr[12], temptmStr[12], filename[72];
                    
/*---------------------------------------------------------------------*/
        
    *iret = G_NORMAL;
    moslst[0] = '\0';

/*
 * Reload Model source strings into ModelStr 
 */
    update_modellist();

/*
 *  Get data string for "panelX_source"
 */
    sprintf( tagStr, "%s%d%s", "panel", src+1, "_source");

    spf_gtfld ( tagStr, dataStr, &ier );

/*  
 *  If no data is present, quit.
 */					    	    
    if ( ier != 0 )  {	        
	*modeln = -1;
	*filen = -1;
	*iret = -1;	
	return;    
    }	    

/*
 * Compare data source found in NPF file to those loaded into ModelStr
 * after replacing [cycle_date]  and [cycle_hour] with current cycle time.
 */
    cst_nocc ( dataStr, '/', 1, 0, &noc1, &ier ); 

    for ( ii = 0; ii < noc1; ii++ ) { 
        grdStr[ ii ] = dataStr[ ii ];	    
    }
    grdStr[ noc1 ] = '\0';
    for (ii = 0; ii < ModelNo; ii++ ) {			
        streq = strcmp(grdStr, ModelStr[ii].model_name);
        if (streq == 0) {
            model_num = ii;
	    break;
	}
    }
    
    strcpy(dataStr1,dataStr);

/*
 *  Loop thru files in model directory saving time stamp
 */
    for ( jj = 0; jj < ModelStr[model_num].nfile; jj++ ) {
	strcpy(filename, ModelStr[model_num].file_names[jj]);
	flen = strlen ( filename );
       
	noc = -1;
	itime = 0;
	while ( itime == 0 ) {
	    noc++;
	    cst_nocc ( filename, '_', 1+noc, 0, &noc1, &ier );
	    if (ier != 0) {
         	break;
            }
            cst_nocc ( filename, '_', 2+noc, 0, &noc2, &ier );
            if (ier != 0) {
            	noc2 = flen;
            }
            for (kk = noc1+1; kk < noc2; kk++) {
            	timStr[ kk-noc1-1 ] = filename[kk];
            }
            timStr[noc2-noc1-1] = '\0';

/*
 * Convert timStr varable to an integer. If > 0, timStr is
 * the time stamp from file, else continue search for time stamp
 * in file name.
 */
	    itime = atoi(timStr);
	}

/*
 * Determine length of date string
 */
        tlen = strlen(timStr);
        strcpy(fulltmStr, timStr);

/*
 * If date string is 8 characters than YYYYMMDD and continue
 * search after the 2nd '_' for HH
 */
	if (tlen == 8) {
	    cst_nocc ( filename, '_', noc+3, 0, &noc3, &ier );
            
            if (ier != 0) {
                noc3 = flen;
            }
             
            for (kk = noc2+1; kk < noc3; kk++) {
                hrStr[ kk-noc2-1 ] = filename[kk];
            }
            hrStr[noc3-noc2-1] = '\0';
            itime = atoi(hrStr);

            if (itime != 0 || strcmp(hrStr,"00") == 0) {
            
/* 
 * Convert the hrStr variable. If is > 0, add to end of timStr
 * and exit the loop, else continue search.
 */
                strcat(fulltmStr, "_");
                strcat(fulltmStr,hrStr);
             } else {
                strcat(fulltmStr, "_\0");
             }
	     streq =  strcmp(temptmStr, fulltmStr);

/*
 * add ';' to time list then add latest time
 */
	    if(streq != 0) {
            	if(jj != 0) {
	            strcat(moslst, ";");
	    	}
	        strcat(moslst, fulltmStr);
	    }
	    strcpy(temptmStr, fulltmStr);

        } else if (tlen == 10) {
            cst_ncpy(tmpstr, fulltmStr, 8, &ier);
            strcat(tmpstr, "_");
            tmpstr[9] = fulltmStr[8];
            tmpstr[10] = fulltmStr[9];
	    tmpstr[11] =  '\0';

            strcpy(fulltmStr,tmpstr);

            streq =  strcmp(temptmStr, fulltmStr);

/*
 * add ';' to time list then add latest time
 */
            if(streq != 0) {
                if(jj != 0) {
                    strcat(moslst, ";");
                }
                strcat(moslst, fulltmStr);
            }
            strcpy(temptmStr, fulltmStr);
        }
    }
    strcpy(dataStr,dataStr1);

/* 
 *  Find the latest time string "YYMMDD/HHMM" in time list.
 */
    len = sizeof( moslst );
    cst_rmbl ( moslst, moslst, &len, &ier);
    ier = noc1 = noc2 = -1;
    
    cst_nocc ( moslst, ';', 1, 0, &noc2, &ier );	

    if ( noc2 == 0 ) noc2 = -1;  /* Only one time frame in list*/
	
    if ( isdigit( (int)moslst[noc2+1] ) && 
	     (int)strlen( moslst ) > (noc1 + 12) ) {
	
        for ( ii = noc1+1; ii < noc2; ii++ ) { 
    	    cycTimStr[ii-noc1-1] = moslst[ii];
        }
	cycTimStr[noc2-noc1-1] = '\0';
	noc1 = -1;
	noc3 = strlen(cycTimStr);

/*
 * Split up cycTimeStr into hours and date if '_' present
 */
	cst_nocc ( moslst, '_', 1, 0, &noc2, &ier );

	if (ier == 0 ) {
	    for ( ii = noc1+1; ii < noc2; ii++ ) {
		cycDatStr[ii-noc1-1] = cycTimStr[ii];
	    }
	    cycDatStr[noc2-noc1-1] = '\0';
	    cst_rpst( dataStr, cycStr, cycDatStr, dataStr, &ier );
	    
	    for ( ii = noc2+1; ii < noc3; ii++ ) {
		cycHrStr[ii-noc2-1] = cycTimStr[ii];
	    }
	    cycHrStr[noc3-noc2-1] = '\0';
	}
	cst_rpst( dataStr, hourStr, cycHrStr, dataStr, &ier );
    } else {
        *iret = -2;
    }

/*
 * Compare reconstructed file name (dataStr) with model names in directory
 * (ModelStr) to get file number
 */
    cst_nocc ( dataStr, '/', 1, 0, &noc1, &ier );

    for (ii = noc1+1; ii < (int)strlen(dataStr); ii++ ) {
        filename[ii-noc1-1] = dataStr[ii];
    }
    filename[strlen(dataStr)-noc1-1] = '\0';

    streq = 0;
    for (ii = 0; ii < ModelStr[model_num].nfile; ii++) {
        streq = strcmp(filename, ModelStr[model_num].file_names[ii]);
        if(streq == 0) {
             file_num = ii;
             break;
        } else {
	    file_num = -1;
	}
    }

/*
 * Set File and Model numbers
 */
    if( file_num == -1 ) {
	*iret = -2;
    }
    SelectModelNo = model_num;
    SelectFileNo = file_num+1;
    *filen = file_num;
    *modeln = model_num;
}

/*=====================================================================*/

void npfw_formatConf ( long flen, char *npftext, char *filstr, int *iret )
/************************************************************************
 * npfw_formatConf	 						*
 *									*
 * This function formats the content of an NPF file into a string to be *
 * displayed in the restore confirmation window.			*
 *									*
 * void npfw_formatConf( flen, npftext, filstr, iret ) 			*
 *									*
 * Input parameters:							*
 *     flen		long	Length of npftext               	*
 *									*
 * Output parameters:							*
 *     *npftext		char	Formated string	from NPF file 		*
 *     *filstr		char	Formated string	containing file name 	*
 *     *iret		int	Return code 				*
 *				 1 - No data sources in the NPF file	*
 *				 0  - Normal				*
 *									*
 **									*
 * Log:									*
 * C. Bailey		05/05	Modified from spfw_formatConf		*
 * T. Piper/SAIC	06/06	Increased npfil to FILE_FULLSZ		*
 ***********************************************************************/
{
    int		src, ier, modelno, filenum, ierr;
    char	pathStr[256], npfil[FILE_FULLSZ], errStr[256];
    char	tmpStr1[10], *tmpStr2 = NULL, *lpStr = NULL; 
    char	tabStr[] = "	";
    Boolean	lp_hasSrc, src_exist;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    src_exist = FALSE;   
    lp_hasSrc = FALSE;

/*
 *  Format the "filstr" and the header for "npftext".  
 */
    if ( strcmp (_dirPath, LOCAL_DIR ) == 0 ) {
        strcpy( npfil, _fileName );
    }
    else {	
	npfw_getFileName( npfil );
    }

    strcpy( npftext, _fileName );
    strcat( npftext, " contents (x = dominant source):\n" );
    
    strcpy( filstr, "Are you sure you want to restore the settings ");
    strcat( filstr, "from NPF file\n");
    strcat( filstr, npfil );
    strcat( filstr, "?\n" );
    
/*
 * Allocate the temp char array based on the actual NPF file size
 */
    if( !(tmpStr2 = XtMalloc((Cardinal)flen)) || !(lpStr = XtMalloc((Cardinal)flen)) ) {
        XtFree(tmpStr2); XtFree(lpStr);
        *iret = 1;
        return;    
    }
    lpStr[0] = '\0';

/*
 *  Find data settings in the NPF file.
 */
	
    for ( src = 0;  src < 36; src++ ) {
		    
	npfw_getSource( src, &modelno, &filenum, &ier );
	
	if ( ier == 0 )  {

	    strcpy(pathStr, ModelStr[modelno].file_fullnames[filenum]);

	    if ( !src_exist ) src_exist = TRUE;
	        
	    lp_hasSrc = TRUE;				
            
	    strcat( tmpStr2, tabStr );
	    strcat( tmpStr2, pathStr );
	    strcat( tmpStr2, "\n" );
			    
        } else {   
	    if ( ier == -2 ) {  /*  Report invalid sources */
                ierr = -8;
                sprintf( errStr, "%s%d%s", " Panel ", src+1, ":  ");
	        er_wmsg ( "NPF", &ierr, errStr, &ier, 3, strlen(errStr) );
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
     
/*
 *  If sources exist, add to source string.
 */    
    if ( src_exist ) {	
	strcat( npftext, lpStr );		
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


void npfw_getGroup ( int src, int filen, int *groupn, int *iret)
/************************************************************************
 * npfw_getGroup							*
 *									*
 * This function gets the specified Meta Files group Number.		*
 *									*
 * void npfw_getGroup (src, filen, groupn, iret)			*
 *									*
 * Input parameters:							*
 *    src	int		Src number				*
 *    filen	int		File number				*
 *									*
 *    *groupn	int		Source group number			*
 *    *iret	int		Return code				*
 *				    0 - Normal				*
 **									*
 * Log:									*
 * C. Bailey/HPC	4/05		initial coding			*
 ***********************************************************************/
{
    int         ii, ipos, ier;
    char        tagStr[32], dataStr[200];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *groupn = -1;

/*
 * Read Group Name From NPF file
 */
    sprintf( tagStr, "%s%d%s", "panel", src+1, "_group" );
    spf_gtfld ( tagStr, dataStr, &ier );

/*
 * Compare Group name to those in Metafile and return Group Number
 */
    for (ii = 0; ii < MAX_NO_GROUPS; ii++ ) {
        cst_srch(0, (int)strlen(GroupList[ii].groupname), dataStr, 
				GroupList[ii].groupname, &ipos, &ier);
	if ( ier == 0 ) {
	    *groupn = ii+1;
	    SelectGroupNo = ii+1;
	    break;
	}
    }
}

#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "drwids.h"
#include "hints.h"

#define VISIBLE_ITEM	  10 
#define MAX_FILES	1000
#define SORT_BY_NAME	   0
#define SORT_BY_DATE	   1

#define OPEN_POPUP	   0
#define BROWSE_POPUP	   1

#define LPF_FILE_EXT	".lpf"
#define FILE_PATTERN	"*.lpf"
#define LOCAL_DIR	"./"
#define CWD_TITLE       "Local"

typedef XmFileSelectionBoxCallbackStruct FSBCBS;

static Widget	_fileSelW;
static Widget	_dirSel_listW;
static Widget	_fileSel_listW;

static Widget	_browsePopup;
static Widget	_sortRadioW;
static WidgetList _sortBtn;

static Widget	_sortPaneW;
static Widget	_selectPaneW;
static Widget	_browseBtnPane;
static Widget	_openPaneW;

static char	_fileName[MXFLSZ]  = "\0";
static char	_dirPath[LLPATH];
static int	_dirPos;
static int	_sortBy;	/* SORT_BY_NAME or SORT_BY_DATE  */

extern XtAppContext	_appContext;

/*
 *  LPF user info data struct
 */
typedef struct {
        char    *title;         /* title name */
        char    *usrpath;       /* full path to this user's LPF dir */
} lpfusr_ent_t;

/*
 *  LPF user info table struct
 */
typedef struct {
    int                 nitems; /* total # of users*/
    lpfusr_ent_t        *items; /* pointer to the array of user items */
} lpfutbl_t;

static lpfutbl_t        _lpfUsrTbl;


/*
 *  private callback functions
 */
static void pglpfw_browseBtnCb  ( Widget, XtPointer, XtPointer );
static void pglpfw_browseDoneCb ( Widget, XtPointer, XtPointer );
static void pglpfw_openCtlBtnCb ( Widget, long, XtPointer );
static void pglpfw_selectCb     ( Widget, XtPointer, XtPointer );
static void pglpfw_selectDirCb  ( Widget, XtPointer, XtPointer );
static void pglpfw_sortByCb     ( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
static void pglpfw_managePanes ( int popup_type);
static void pglpfw_setFileList ( void );
static void pglpfw_unmanagePanes ( void );
static void pglpfw_readUsrTbl ( char *tblname, lpfutbl_t *lpfutbl, 
				int *iret );
static void pglpfw_prepLPF ( int num_layer );

/************************************************************************
 * nmap_pglpfw.c							*
 *									*
 * This module defines the layer product file (LPF) selection in 	*
 * product generation.							*
 *									*
 * CONTENTS:								*
 *  pglpfw_create()		creates LPF selection window		*
 *  pglpfw_popup()		popup the LPF selection window		*
 *  pglpfw_popdown()		put down the LPF selection window	*
 *									*
 *  pglpfw_isUp()		returns status of the window		*
 *									*
 *  pglpfw_selectCb()		callback for file list			*
 *  pglpfw_selectDirCb()	callback for directory list		*
 *  pglpfw_openCtlBtnCb()	callback for open control buttons	*
 *  pglpfw_browseBtnCb()	callback for the browse button		*
 *  pglpfw_browseDoneCb()	callback for browse OK/Cancel buttons	*
 *  pglpfw_sortByCb() 		callback for sort by toggles		*
 *									*
 *  pglpfw_managePanes()	manages the appropiate children of pane	*
 *  pglpfw_unmanagePanes()	unmanages all the children of pane	*
 *  pglpfw_setFileList()	sets the file list for the _currPath	*
 *  pglpfw_readUsrTbl()		read user table "lpf.tbl"		*
 *  pglpfw_loadLPF()		load a LP file into display		*
 *  pglpfw_prepLPF()		prepare layers for loading LP files	*
 *  pglpfw_setPath()		set _dirPath variable			*
 *  pglpfw_setName()		set _fileName variable			*
 ***********************************************************************/

/*=====================================================================*/

void pglpfw_create ( Widget parent )
/************************************************************************
 * pglpfw_create							*
 *									*
 * This function creates a file selection dialog box.			*
 *									*
 * void pglpfw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 *									* 
 * T. Lee/SAIC		04/02	Created					*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		toff = 5, ier;
    long	ii, nn;
    char	*open_btnstr[] = {"Open", "Cancel"};
    char	*sort_str[] = {"Name", "Date"};
    Widget	pane, label, frame, sort_label;
    Arg         args[10];
    Cardinal    argcnt;
    XmString    xmpattern; 
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    _fileSelW = XmCreateFormDialog( parent, "Open product", NULL, 0 );
    XtVaSetValues(_fileSelW,
		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pglpfw_pane",
                    xmPanedWindowWidgetClass, _fileSelW,
                    XmNsashWidth,             1,
                    XmNsashHeight,            1,
                    NULL);

    /*
     * ******** SORT PANE ********
     * create a form widget to hold the sort by toggles
     */
    _sortPaneW = XtVaCreateWidget("pglpfw_form",
                    xmFormWidgetClass, 	      pane,
		    XmNnoResize,     	      TRUE,
		    XmNautoUnmanage, 	      FALSE,
		    XmNfractionBase,          110,
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
    _sortBtn = (WidgetList)XtMalloc(nn*sizeof(Widget));

    for (ii = 0; ii < nn; ii++) {
        _sortBtn[ii] = XtVaCreateManagedWidget (sort_str[ii],
		    xmToggleButtonWidgetClass,      _sortRadioW,
                    XmNtraversalOn,                 FALSE,
		    NULL);
	XtAddCallback(_sortBtn[ii], XmNdisarmCallback,
	            pglpfw_sortByCb, (XtPointer) ii);

	XmToggleButtonSetState (_sortBtn[ii], (_sortBy == ii), FALSE);
    }

    XtManageChild(_sortPaneW); 


    /*
     * ******** SELECT PANE ********
     * create a form widget to hold the directory and file lists
     */
    _selectPaneW = XtVaCreateWidget("pglpfw_selectform",
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
 		   pglpfw_selectDirCb, NULL );

    pglpfw_readUsrTbl("lpf.nmap", &_lpfUsrTbl, &ier);

    if (_lpfUsrTbl.nitems > 0) {

        xmfils = (XmStringTable) XtMalloc 
	    (_lpfUsrTbl.nitems * sizeof (XmString *));

        for (ii = 0; ii < _lpfUsrTbl.nitems; ii++) {
	    xmfils[ii] = XmStringCreateLocalized (_lpfUsrTbl.items[ii].title);
        }

        XtVaSetValues (_dirSel_listW,
		       XmNitems,	xmfils,
		       XmNitemCount,	_lpfUsrTbl.nitems,
		       NULL);

        for (ii = 0; ii < _lpfUsrTbl.nitems; ii++) {
	    XmStringFree (xmfils[ii]);
        }
	XtFree ((XtPointer) xmfils);

	/*
	 * hi-light the local directory
	 */
	_dirPos = _lpfUsrTbl.nitems;
    }

    XtManageChild( _dirSel_listW );
    XtManageChild( frame );

    /*
     * create the file list
     */
    label = XmCreateLabel( _selectPaneW, "Select layer product file:", NULL, 0 );
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
		   pglpfw_selectCb, NULL);


    XtManageChild (_fileSel_listW);
    XtManageChild (frame);

    XtManageChild(_selectPaneW);

    /*
     * ******** BROWSE PANE ********
     */
    _browseBtnPane = 
	XtVaCreateManagedWidget ("Browse",
				 xmPushButtonWidgetClass,	pane,
				 NULL);
    XtAddCallback(_browseBtnPane, XmNarmCallback,
		  pglpfw_browseBtnCb, (XtPointer) NULL); 



    /*
     * ******** OPEN CONTROL PANE ********
     */
    _openPaneW = 
	(Widget) NxmCtlBtn_create (pane, 1, "pglpfw_open", 
				   XtNumber(open_btnstr), open_btnstr, 
				   (XtCallbackProc)pglpfw_openCtlBtnCb, NULL); 


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
		  pglpfw_browseDoneCb, (XtPointer) 0);
    XtAddCallback(_browsePopup, XmNokCallback,
		  pglpfw_browseDoneCb, (XtPointer) 1);

    XtUnmanageChild(XmFileSelectionBoxGetChild(_browsePopup, 
					       XmDIALOG_HELP_BUTTON));

}

/*=====================================================================*/

void pglpfw_popup ( void )
/************************************************************************
 * pglpfw_popup								*
 *									*
 * This function loads and displays the layer product files into the	*
 * file selection dialog box.						*
 *									*
 * void pglpfw_popup ( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02	Created					*
 ***********************************************************************/
{
    int		ipos;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    /*
     * set the dialog title 
     */
    xmstr = XmStringCreateLocalized("Open Layer Product File");

    pglpfw_managePanes (OPEN_POPUP);

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
    if (ipos > _lpfUsrTbl.nitems) ipos = _lpfUsrTbl.nitems;

    XmListSetBottomPos (_dirSel_listW, ipos);

    /*
     * set file list.
     */

    pglpfw_setFileList ();

    XtManageChild(_fileSelW);
}

/*=====================================================================*/

void pglpfw_popdown ( void )
/************************************************************************
 * pglpfw_popdown							*
 *									*
 * This function puts the file window down. 				*
 *									*
 * void pglpfw_popdown ( )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02	Created					*
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

static void pglpfw_readUsrTbl ( char *tblname, lpfutbl_t *lpfutbl, int *iret )
/************************************************************************
 * pglpfw_readUsrTbl							*
 *									*
 * This routine will read LPF user table and create the data structure  *
 * to store the information.                                            *
 *									*
 * void pglpfw_readUsrTbl( tblname, lpfutbl, iret)                      *
 *									*
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *									*
 * Output parameters:                                                   *
 *      *lpfutbl        lpfutbl_t       pointer to lpf user table struct*
 *      *iret           int             0 - success                     *
 *									*
 * Return code:                                                         *
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02	revise from vtbl_readUsrTbl()		*
 ***********************************************************************/
{
    int         ii, nn, num, ier;
    char        buffer[256], title[60], path[256], cwd[256], format[256];
    char        dum1[256], dum2[256], dum3[256];
    FILE        *fp;

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

                if  ( nn == 0 )  {
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
    lpfutbl->nitems = nn;
    lpfutbl->items  = (lpfusr_ent_t *) malloc ((nn+1) *sizeof (lpfusr_ent_t));

    if (nn > 0) {
        rewind(fp);

        ii = 0;
        while ( ii < nn ) {
            cfl_trln( fp, 256, buffer, &ier );

            if (ier == 0) {
                sscanf(buffer, format, title, path);

                lpfutbl->items[ii].title = (char *) malloc (strlen (title) + 1);
                strcpy( lpfutbl->items[ii].title, title );

                lpfutbl->items[ii].usrpath = (char *) malloc (strlen(path) + 1);
                strcpy( lpfutbl->items[ii].usrpath, path );

                ii++;
            }
        }
    }

    if (fp) fclose(fp);

    /*
     * set default user
     */
    getcwd (cwd, sizeof (cwd));

    ii = lpfutbl->nitems;
    lpfutbl->items[ii].title = (char *) malloc (strlen (CWD_TITLE) + 1);
    strcpy (lpfutbl->items[ii].title, CWD_TITLE);

    lpfutbl->items[ii].usrpath = (char *) malloc(strlen(cwd) + 1);
    strcpy ( lpfutbl->items[ii].usrpath, cwd );

    nn = cfl_gfil (0, MAX_FILES, cwd, LPF_FILE_EXT, NULL);

    lpfutbl->nitems++;

}

/*=====================================================================*/

Boolean pglpfw_isUp ( void )
/************************************************************************
 * pglpfw_isUp         							*
 *									*
 * This function returns the status of the window.                      *
 *									*
 * Boolean pglpfw_isUp()       					        *
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 * Return parameters:                                                   *
 *	pglpfw_isUp	Boolean		returns True if _ is managed.	*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    return ( XtIsManaged ( _fileSelW ) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pglpfw_selectCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pglpfw_selectCb							*
 *									*
 * Callback function for selecting a layer product file.		*
 *									*
 * void pglpfw_selectCb (wid, clnt, call)				*
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
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
    char 	*tmp_str;
/*---------------------------------------------------------------------*/

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &tmp_str);

    strcpy (_fileName, tmp_str);

    XtFree (tmp_str);

}

/*=====================================================================*/
/* ARGSUSED */
static void pglpfw_selectDirCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pglpfw_selectDirCb							*
 *									*
 * Callback function for selecting a directory.				*
 *									*
 * void pglpfw_selectDirCb (wid, clnt, call)				*
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
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    _dirPos = cbs->item_position;

    if (_dirPos == _lpfUsrTbl.nitems) {
	strcpy (_dirPath, LOCAL_DIR);
    }
    else {
	strcpy (_dirPath, _lpfUsrTbl.items[(_dirPos) - 1].usrpath);
	if (_dirPath[(strlen (_dirPath) - 1)] != '/') {
	    strcat (_dirPath, "/");
	}
    }

    pglpfw_setFileList ();
}

/*=====================================================================*/
/* ARGSUSED */
static void pglpfw_openCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pglpfw_openCtlBtnCb							*
 *									*
 * This is the callback function for the open control buttons (OK, and	*
 * CANCEL) on the bottom of the file popup window.  			*
 *									*
 * void pglpfw_openCtlBtnCb  (wid, which, call )			*
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
 * T. Lee/SAIC		04/02						*
 * T. Lee/SAIC		05/02	Removed open check			*
 ***********************************************************************/
{
    int		ier, ier1;
/*---------------------------------------------------------------------*/

    ier = 0;
    switch ( which ) {

	case 0:		/* Open */
	    /*
	     *  Exit layering if necessay.
	     */
	    if ( pglayrw_isUp () ) {
		pglayrw_exit ();
	    }

	    if ( strlen( _fileName ) > (size_t)0) {
		pglpfw_loadLPF (&ier);
	    }
	      else {
		ier = -1;
		er_wmsg ( "CTB", &ier, _fileName, &ier1, 3, strlen(_fileName) );
		NxmErr_update();
	    }

	    pglpfw_popdown ();
	    pgundo_initUndo ();

	    break;

	case 1:		/* CANCEL */
    	    pglpfw_popdown ();
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
static void pglpfw_browseBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pglpfw_browseBtnCb							*
 *									*
 * This is an internal callback function for the browse button		*
 *									*
 * void pglpfw_browseBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	Callback struct			*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    XtManageChild (_browsePopup);
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pglpfw_browseDoneCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pglpfw_browseDoneCb							*
 *									*
 * This is an internal callback function for the browse buttons,	*
 * Ok and Cancel.							*
 *									*
 * void pglpfw_browseDoneCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	Callback struct			*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 * E. Safford/SAIC	04/02	use cfl_isdir instead of local version	*
 ***********************************************************************/
{
    int		which, ier;
    char 	*text;
    char	filepart[MXFLSZ] = "\0" , pathpart[LLPATH] = "\0";
    Widget	popup;
    FSBCBS 	*fcbs;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    if (which == 1) {	/* OK */
	fcbs = (FSBCBS *) cbs;
	XmStringGetLtoR (fcbs->value, XmFONTLIST_DEFAULT_TAG, &text);

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

	XtFree (text);


	if ( _fileName[0] == '\0' ) {
	    popup = (XtIsManaged (_browsePopup)) ? _browsePopup : _fileSelW;
	    NxmWarn_show (popup, "Not a valid file.");
	}
	else {
	    pglpfw_openCtlBtnCb ( NULL, 0, NULL );
	}
    }
    else {	/* CANDEL */
	XtUnmanageChild (_browsePopup);
    }
}

/*=====================================================================*/
/* ARGSUSED */ 
static void pglpfw_sortByCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pglpfw_sortByCb							*
 *									*
 * This is an internal callback function for the sort by toggles	*
 *									*
 * void pglpfw_sortByCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	Callback struct			*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
int	new_sort;
/*---------------------------------------------------------------------*/

 	new_sort = (long)clnt;

    if (new_sort != _sortBy) {
        _sortBy = new_sort;
 	pglpfw_popdown();
        pglpfw_popup();
    }
}

/*=====================================================================*/

static void pglpfw_managePanes ( int popup_type )
/************************************************************************
 * pglpfw_managePanes							*
 *									*
 * This function manages the appropiate children of the main pane	*
 * widget based on popup_type.						*
 *									*
 * void pglpfw_managePanes ( popup_type )				*
 *									*
 * Input parameters:							*
 *	popup_type	int	decides which children to use		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    pglpfw_unmanagePanes ();

    switch (popup_type) {
      case OPEN_POPUP:
	XtManageChild (_sortPaneW);
	XtManageChild (_selectPaneW);
	XtManageChild (_browseBtnPane);
	XtManageChild (_openPaneW);
	break;

      case BROWSE_POPUP:
	break;
    }
}

/*=====================================================================*/

static void pglpfw_unmanagePanes ( void )
/************************************************************************
 * pglpfw_unmanagePanes							*
 *									*
 * This function unmanages all the children of the main pane widget.	*
 *									*
 * void pglpfw_unmanagePanes ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    if (XtIsManaged (_sortPaneW)) {
	XtUnmanageChild (_sortPaneW);
    }

    if (XtIsManaged (_selectPaneW)) {
	XtUnmanageChild (_selectPaneW);
    }


    if (XtIsManaged (_browseBtnPane)) {
	XtUnmanageChild (_browseBtnPane);
    }

    if (XtIsManaged (_openPaneW)) {
	XtUnmanageChild (_openPaneW);
    }

} 

/*=====================================================================*/

static void pglpfw_setFileList ( void )
/************************************************************************
 * pglpfw_setFileList							*
 *									*
 * This function sets up the list of files for the current path.	*
 *									*
 * void pglpfw_setFileList ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 ***********************************************************************/
{
    int		ii, nf;
    char	fn_list[MAX_FILES][MXFLSZ];
    XmString	xmstr;
    XmStringTable	xmfils;
/*---------------------------------------------------------------------*/

    /*
     * Wipe the list of .lpf filenames       
     */
    XmListDeleteAllItems(_fileSel_listW);

    /*
     *  Get list of file names in the directory & write to the fn_list
     */
    nf = cfl_gfil (_sortBy, MAX_FILES, _dirPath, ".lpf", fn_list);

    if ( nf > 0 ) {

        xmfils = (XmStringTable)XtMalloc(nf*sizeof(XmString *));

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

void pglpfw_loadLPF ( int *iret )
/************************************************************************
 * pglpfw_loadLPF							*
 *									*
 * This function opens a LP file and load the VG files into different	*
 * layers by the specified color, fill modes, and group types. If the	*
 * the VG file does not exist, it will be created.			*
 *									*
 * void pglpfw_loadLPF ( iret )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *   *iret		int	 	 0 = normal 			*
 *					-1 = no file name		*
 *					-2 = error on read		*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		04/02						*
 * T. Lee/SAIC		05/02	added layer display mode		*
 * E. Safford/SAIC	06/02	param change to ctb_lygetname		*
 * J. Wu/SAIC		12/02	fix the crashing from multi-exposures	*
 * H. Zeng/XTRIA	03/03   removed one call to pglayer_setDefGrp() *
 * E. Safford/SAIC	04/04	ChngMade flag TRUE if outfile specified	*
 ***********************************************************************/
{
    Widget	curwid;
    char	fname[FILE_FULLSZ], vfile[FILE_FULLSZ];

    char	outfile[FILE_FULLSZ];
    char	path[LLPATH];

    char	gtype[10], ugtype[10], laynam[10], mode[4], umode[4];
    char	cmode[5], ucmode[5], *cptr;
    int		ipos, ier, ier1, ntop, icolr, layer_num;
    int		ii, grpid;

    Boolean	can_open, outfile_ok;

/*---------------------------------------------------------------------*/

    *iret      = 0;
    can_open   = FALSE;
    outfile_ok = FALSE;


    if ( strlen(_fileName) <= (size_t)0 ) {
	*iret = -1;
	return;
    }
    else {
        strcpy (fname, _dirPath);
        strcat (fname, _fileName);

        cst_srch ( 0, strlen(fname), ".lpf", fname, &ipos, &ier );
        if ( ier == -4 ) {
	    strcat(fname, ".lpf");
	}

    }
       
    /*
     *  Open the layer product file.
     */
	    
    ctb_lyrd ( fname, &ntop, &ier );

    if ( ntop < 0 || ier < 0 ) { 
	er_wmsg ( "CTB", &ier, fname, &ier1, 3, strlen(fname) );
 	NxmErr_update();
   	pglayer_setChngMade ( 0, FALSE );
	*iret = -2;
 	return; 
    }

    /* 
     * Prompt error message if default is used.
     */
    else if ( ier > 0 ) {
	er_wmsg ( "CTB", &ier, " ", &ier1, 3, 1 );
 	NxmErr_update();
    }

    /* 
     *  Delete all records and clear drawing area
     */ 
    pgpalw_deleteAll();

    /*
     *  Store the LPF into layer structure.
     */
    ntop = G_MIN ( ntop, MAX_LAYERS );
    curwid = pgpalw_getOperWid ( FUNC_LAYER );
    pgpalw_setCurOper ( curwid );
    
    pgpalw_actvLayer ( False );
    
    for ( ii = 0; ii < ntop; ii++ ) {

	/*
	 * Set color mode.
	 */
	ctb_lygetcmode ( ii, cmode, &ier );
	cst_lcuc ( cmode, ucmode, &ier1 );
	if ( strcmp ( ucmode, "MONO" ) == 0 )  {
	    pglayer_setDsplClr ( ii, FALSE );
	}
	else {
	    pglayer_setDsplClr ( ii, TRUE );
	}

	/*
	 * Set color ID.
	 */
	ctb_lygetcolor ( ii, &icolr, &ier );

	if ( ier == 0 ) {
	    pglayer_setMonoClr ( ii, icolr );
	}
	else {
	    icolr = 19;
	    pglayer_setMonoClr ( ii, icolr );
	}


	/*
	 * Get the output VG file name
	 */
	outfile_ok = FALSE;
        ctb_lygetoutfile( ii, outfile, &ier );

	if ( strlen( outfile ) > (size_t)0 ) {

	    /*
	     * Check if the outfile is valid VG file. If the file does not exist,
	     * create it. If the file is invalid, an error message is generated. 
	     *
	     * The outfile_ok flag signals a good outfile condition.  Otherwise 
	     * we have no outfile available.
	     */
	    cvg_valid ( outfile, &ier );
            if ( ier >= 0 ) {
                outfile_ok = TRUE;
	        if ( ier > 0 ) {
	            cvg_crvgf( outfile, &ier );
		    ier = 2;
		    er_wmsg( "CVG", &ier, outfile, &ier1, 3, strlen(outfile) );
		    NxmErr_update();
	        }
            }
	    else {
	        er_wmsg ( "CVG", &ier, outfile, &ier1, 3, strlen(vfile));
 	        NxmErr_update();
            }
        }


	/* 
	 * Set input VG file name.
	 */
	ctb_lygetfile ( ii, vfile, &ier );


	/*
	 * Check if this is a valid VG file. 
	 *
	 * If the file does not exist, and we did not find a file and there, 
	 * is no specified output file, then create one.  If the file is 
	 * invalid, an error message is written out.
	 */
	cvg_valid ( vfile, &ier );
	if ( ier >= 0 ) {
	    can_open = TRUE;
	    if ( ier > 0 && !outfile_ok ) {
		cvg_crvgf ( vfile, &ier );
		ier = 2;
	    	er_wmsg ( "CVG", &ier, vfile, &ier1, 3, strlen(vfile));
		NxmErr_update();
	    }
	}
	else {
	    can_open = FALSE;
	    er_wmsg ( "CVG", &ier, vfile, &ier1, 3, strlen(vfile));
 	    NxmErr_update();
	}



	if ( (cptr = strrchr (vfile, '/') ) == (char *) NULL) {
	    pglayer_setFileName ( ii, vfile );
	    strcpy (path, "./");
	}
	else {
	    cptr++;
	    pglayer_setFileName ( ii, cptr );
	    cptr[0] = '\0';
	    strcpy ( path, vfile );
	}
	pglayer_setFilePath ( ii, path );


	/* 
	 * Set group type.
	 */
	ctb_lygetgrptyp ( ii, gtype, &ier );
	cst_lcuc ( gtype, ugtype, &ier1 );
	if ( ier == 0 ) {
	    ces_gtgid ( ugtype, &grpid, &ier1 );

            if ( ier1 == 0 ) {
	         pglayer_setDefGrp ( ii, grpid );
            }
	}

	/*
	 * Set fill mode.
	 */
	ctb_lygetfmode ( ii, mode, &ier );
	cst_lcuc ( mode, umode, &ier1 );
	if ( strcmp (umode, "OFF") == 0 ) {
	    pglayer_setFill ( ii, FALSE );
	}
	else {
	    pglayer_setFill ( ii, TRUE );
	}

	/*
	 * Set display mode.
	 */
	ctb_lygetdsply ( ii, mode, &ier );
	cst_lcuc ( mode, umode, &ier1 );
	if ( strcmp (umode, "OFF") == 0 ) {
	    pglayer_setDsplOn ( ii, FALSE );
	}
	else {
	    pglayer_setDsplOn ( ii, TRUE );
	}

	/* 
	 * Set layer name.
	 */
	ctb_lygetname ( ii, sizeof(laynam), laynam, &ier );
	if ( ier == 0 ) {
	    pglayer_setName ( ii, laynam );
	}
	
	/* 
	 * Prepare to draw in a new layer.
	 */	
	pglpfw_prepLPF ( ii );
	
	/*
	 * Reset flags for change mode.
	 */
  	pglayer_setChngMade ( ii, FALSE );

 	/*
	 * Load VG files.
	 */
	if ( can_open ) {
	    pgfilw_openVGF (FALSE, &ier);
	}



	/*
	 *  If the outfile_ok is true, then reset the file name for the layer. 
	 *
	 *  Also, since there is a specified output file in the lpf, set the 
	 *  save flag so any save to it is done without the initial Save As.
	 */
	if ( outfile_ok ) {
        
	    if ( (cptr = strrchr (outfile, '/') ) == (char *) NULL) {
	        pglayer_setFileName ( ii, outfile );
	        strcpy (path, "./");
	    }
	    else {
	        cptr++;
	        pglayer_setFileName ( ii, cptr );
	        cptr[0] = '\0';
	        strcpy ( path, outfile );
	    }
	    pglayer_setFilePath ( ii, path );
  	    pglayer_setChngMade ( ii, TRUE );
            pglayer_setFileSaved( ii, True );
        }
    } 
   

    /*
     * If reaching MAX_LAYERS, set "ADD LAYER" button insensitive.
     */
    layer_num = pglayer_getNumLayers();
    if ( layer_num == MAX_LAYERS ) {
        pgpalw_setBtnSntv ( FUNC_LAYER, FALSE );        
    }    
    
    /*
     *  Bring up the layering window.
     */
    pglayrw_manageLayers ();
        
}

/*=====================================================================*/

static void pglpfw_prepLPF ( int num_layer )
/************************************************************************
 * pglpfw_prepLPF							*
 *									*
 * This function prepares to load a VG file in a LP file onto its 	*
 * specifed layer by switching layers and setting the specified color,	*
 * fill modes, and group types. 					*
 *									*
 * static void pglpfw_prepLPF ( num_layer )				*
 *									*
 * Input parameters:							*
 *	num_layer	int	Number of layers to be prepared		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/02	initial coding				*
 * E. Safford/SAIC	11/03	call pglayrw_setActvLayer not _nameCb   *
 ***********************************************************************/
{
    int		iret, nn;
/*---------------------------------------------------------------------*/
    
    /* 
     *  Update the settings for the first layer.
     */
    if ( num_layer == 0 ) {
        pglayrw_updtSettings ( 0 );
	return;	
    }
    

    /* 
     *  Update settings for layer 1 and up.
     */
    for ( nn = 1; nn <= num_layer; nn++ ) {                         

	if ( !pglayer_getInUse ( nn ) ) { 
             pglayer_setInUse ( nn, TRUE );

	     pglayrw_setActvLayer( nn, &iret );
             break;
        }
    }     
}

/*=====================================================================*/

void pglpfw_setPath ( char* path )
/************************************************************************
 * pglpfw_setPath							*
 *									*
 * This function sets the _dirPath variable. 				*
 *									*
 * void pglpfw_setPath ( path )						*
 *									*
 * Input parameters:                                                    *
 *	path		char*	directory path				*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04	initial coding				*
 ***********************************************************************/
{

    strcpy (_dirPath, path);

}

/*=====================================================================*/

void pglpfw_setName ( char* name )
/************************************************************************
 * pglpfw_setName							*
 *									*
 * This function sets the _fileName variable. 				*
 *									*
 * void pglpfw_setName ( name )						*
 *									*
 * Input parameters:                                                    *
 *	name		char*	file name				*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04	initial coding				*
 ***********************************************************************/
{

    strcpy (_fileName, name);

}

/*=====================================================================*/



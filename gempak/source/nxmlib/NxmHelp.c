#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


#define HELP_MAXFILENUM 	50  /* maximum number of help files	   */
#define VISI_NUM		4   /* # of visible items in title list    */
#define NXM_HELP_MAXLINESIZE	200 /* maximum characters in one help line */
#define NXM_HELP_MAXLINES	2000 /* maximum lines in one help file	   */

static Widget	_hlpW;		/* main help widget	  */
static Widget	_hlptxtW;	/* help text widget	  */
static Widget	_hlpList;	/* help title list widget */


typedef struct {
	int	hlp_num;	/* # of help files	*/
	int	*fileid;	/* help file id array	*/
	char	**filename;	/* help file name array */
	char	**title;	/* help title array	*/
	} Hlp_str;

static	Hlp_str  _helpInfo;


/*
 *  Private functions
 */
void NxmHelp_list_cb ( Widget, XtPointer, XmListCallbackStruct *cbs );
int  NxmHelp_loadFile ( Widget text_widget, char *filename );
int  NxmHelp_popup ( char *filename );
void NxmHelp_readTbl ( Widget parent, char *index_file );


/************************************************************************
 * NxmHelp.c								*
 *									*
 * This module contains help functions. 				*
 *									*
 * CONTENTS:								*
 *									*
 * NxmHelp_create()		Create a main HELP dialog.		*
 * NxmHelp_helpBtnCb()		HELP button on main HELP widget.	*
 * NxmHelp_popup()		Displays help file in main HELP widget. *
 *									*
 * NxmHelp_loadFile()		Loads help file into HELP text widget.	*
 * NxmHelp_list_cb()		Obtains help file name from HELP list.	*
 * NxmHelp_readTbl()		Fills _helpInfo table.			*
 ***********************************************************************/

/*=====================================================================*/


Widget NxmHelp_create ( Widget parent, char *dialogw_name, 
				char *helpw_name, char *hlp_table, 
						int rows, int columns )
/************************************************************************
 * NxmHelp_create							*
 *									*
 * This function creates a Help popup widget.				*
 *									*
 * Widget NxmHelp_create(parent, dialogw_name, helpw_name,		*
 *		hlp_table, rows, columns)				*
 *									*
 * Input parameters:							*
 *  parent	    Widget	ID of parent widget.			*
 *  *dialogw_name   char	name of the popup widget.		*
 *  *helpw_name	    char	name of the help text widget.		*
 *  *hlp_table	    char	name of the help table file.		*
 *  rows	    int        number of rows in help text widget.	*
 *  columns	    int        number of columns in help text widget.	*
 *									*
 * Return parameters:							*
 * NxmHelp_create	Widget	The dialog widget			*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    05/94						*
 * S. Wang/GSC	    07/96  add title selection list			*
 * G. Krueger/EAI   09/97  _NxmClosePopupCallback -> NxmClose_popupCb	*
 * G. Krueger/EAI   11/97  Cleaned up headers; Removed NxmHelpstrcmp;	*
 *			   Renamed functions and internal variables	*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 * T. Piper/GSC		07/01	fix free of str_list			*
 * E. Safford/SAIC	10/01	use monospaced font 			*
 * T. Piper/SAIC	12/01	freed flentry				*
 * E. Safford/SAIC	04/05	free fontlist				*
 ***********************************************************************/
{
Widget		button, rowcol;
Cardinal	argcnt;
Arg		arg1[5], args[10];
int		ii, ix, iy, kk;
int		visi_num;
Dimension	width, height;
char		char_edit[HELP_MAXFILENUM][100];
char		fontname[] ="-adobe-courier-bold-r-normal--17-120-*-*-m-*-*-*";
XmFontListEntry flentry;
XmFontList	fontlist;
Display		*dsp;
XmStringTable	str_list;
XmString	xmstr;

/*---------------------------------------------------------------------*/
/*
 *  The help popup widget is a BulletinBoardDialog widget
 *  holding a ScrolledText widget ( help widget ) and a CLOSE button.
 */

	NxmHelp_readTbl(parent, hlp_table);

	str_list = (XmStringTable)XtMalloc((size_t)_helpInfo.hlp_num *
						sizeof(XmString));

/*
 * sort the titles in alphabetic order
 */

	for ( ii = 0; ii < _helpInfo.hlp_num; ii++)
		strcpy(char_edit[ii], _helpInfo.title[ii]);

/*
 * Sort the list of help topics using QSORT.
 * Function STRCASECMP is cast into a type compatible with QSORT.
 */
	qsort( (char *)char_edit, (size_t)_helpInfo.hlp_num, 100*sizeof(char),
	       (int (*)(const void *, const void *))strcasecmp );

	for (kk = 0; kk < _helpInfo.hlp_num; kk++)
		str_list[kk] = XmStringCreateLocalized(char_edit[kk]);

	argcnt = 0;
	XtSetArg(args[argcnt], XmNallowOverlap, False); argcnt++;
	xmstr = XmStringCreateLocalized("Help");
	XtSetArg(args[argcnt], XmNdialogTitle, xmstr); argcnt++;
	XtSetArg(args[argcnt], XmNnoResize, True); argcnt++;

	_hlpW = XmCreateBulletinBoardDialog(parent, dialogw_name,
			args, argcnt);

	dsp = XtDisplay (_hlpW);
	flentry  = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
	fontlist = XmFontListAppendEntry (NULL, flentry);
	XmFontListEntryFree(&flentry);
	rowcol = XtVaCreateManagedWidget( "rc", xmRowColumnWidgetClass,
			_hlpW, NULL);

	visi_num = VISI_NUM;

	argcnt = 0;
	XtSetArg(arg1[argcnt], XmNitems, str_list); argcnt++;
	XtSetArg(arg1[argcnt], XmNitemCount, _helpInfo.hlp_num); argcnt++;
	XtSetArg(arg1[argcnt], XmNvisibleItemCount, visi_num); argcnt++;
	XtSetArg(arg1[argcnt], XmNfontList, fontlist); argcnt++;

	_hlpList = XmCreateScrolledList( rowcol, "HelpTopic", arg1,
								argcnt );
	XtManageChild(_hlpList);

	XtAddCallback( _hlpList, XmNbrowseSelectionCallback,
			(XtCallbackProc)NxmHelp_list_cb, NULL);

	argcnt = 0;
	XtSetArg(args[argcnt], XmNnoResize, True); argcnt++;
	XtSetArg(args[argcnt], XmNautoShowCursorPosition, False); argcnt++;
	XtSetArg(args[argcnt], XmNscrollBarDisplayPolicy, XmAS_NEEDED);
						argcnt++;
	XtSetArg(args[argcnt], XmNscrollingPolicy, XmAUTOMATIC); argcnt++;
	XtSetArg(args[argcnt], XmNeditMode, XmMULTI_LINE_EDIT); argcnt++;
	XtSetArg(args[argcnt], XmNeditable, False); argcnt++;
	XtSetArg(args[argcnt], XmNcolumns, columns); argcnt++;
	XtSetArg(args[argcnt], XmNrows, rows); argcnt++;
	XtSetArg(args[argcnt], XmNfontList, fontlist); argcnt++;

	_hlptxtW = XmCreateScrolledText(rowcol, helpw_name,
				args, argcnt);

	XtManageChild(_hlptxtW);

	XtVaGetValues(	_hlptxtW,
			XmNwidth,  &width,
			XmNheight, &height,
			NULL);

	ix = (int)((float)width/2.0F) - 10 ;
	iy = (int)height + 160 ;

	button = XtVaCreateManagedWidget("Close",
			xmPushButtonWidgetClass,_hlpW,
			XmNx,			ix,
			XmNy,			iy,
			NULL);

	XtAddCallback( button, XmNactivateCallback,
		(XtCallbackProc)NxmClose_popupCb,_hlpW);

	XmStringFree (xmstr);
	for( kk = 0; kk < _helpInfo.hlp_num; kk++ ) {
		XmStringFree( str_list[kk] );
	}
	XtFree ( (XtPointer)str_list);

        XmFontListFree( fontlist ); 
	return(_hlpW);

}

/*=====================================================================*/
/* ARGSUSED */
void NxmHelp_helpBtnCb ( Widget wid, long fileid, XtPointer cbs )
/************************************************************************
 * NxmHelp_helpBtnCb							*
 *									*
 * This is a convenient callback function for help button.		*
 *									*
 * void NxmHelp_helpBtnCb(wid, fileid, cbs)				*
 *									*
 * Input parameters:							*
 *  wid		   Widget     ID of the calling widget. 		*
 *  fileid	   long	      id of help file.				*
 *  cbs		   XtPointer  callback structure (not used).		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 *									*
 * S. Wang/GSC	    07/96						*
 * C. Lin/EAI	    11/96   add XtIsManaged checking			*
 * G. Krueger/EAI   11/97   Renamed functions and internal variables	*
 * T. Piper/SAIC	02/04	Added NxmWarn_show			*
 ***********************************************************************/
{
int	 kk;
char	 temp[100];
char	 message[48] = "WARNING:  We told you help would not work!";
XmString str;

/*---------------------------------------------------------------------*/
	for ( kk = 0; kk < _helpInfo.hlp_num; kk++ )
	    if ( (int)fileid == _helpInfo.fileid[kk] )
	        break;

	if ( kk >= _helpInfo.hlp_num )  {
	    NxmWarn_show(wid, message);
	    return;
	}

	strcpy( temp, _helpInfo.title[kk] );

	str = XmStringCreateLocalized( temp );
	XmListSelectItem( _hlpList, str, False );

/*
 * make sure the selected item is visible
 */
	XmListSetBottomItem(_hlpList, str);

	XmStringFree(str);

	if ( XtIsManaged(_hlpW) )
		XtUnmanageChild( _hlpW );

	NxmHelp_popup( _helpInfo.filename[kk] );
}

/*=====================================================================*/

int NxmHelp_popup ( char *filename )
/************************************************************************
 * NxmHelp_popup							*
 *									*
 * This function loads a help file and displays the file into the	*
 * help text widget of the help popup window.				*
 *									*
 * int NxmHelp_popup(filename)						*
 *									*
 * Input parameters:							*
 *  *filename	   char    name of the help file.			*
 *									*
 * Return parameters:							*
 *  NxmHelp_popup	int		1 --- success.			*
 *			 0 --- need to create help dialog first. 	*
 *	       		-1 --- unable to open the help file.		*
 *									*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    05/94						*
 * G. Krueger/EAI   11/97   Renamed functions and internal variables	*
 ***********************************************************************/
{
int ret;

/*---------------------------------------------------------------------*/
/*
 * This function cannot be called before the HelpPopup
 * widget is created.
 */

	if ( _hlptxtW == NULL ) {
		return(0);
	}

	ret = NxmHelp_loadFile( _hlptxtW, filename );

	XtManageChild( _hlpW );

	return(ret);

}

/*=====================================================================*/

int NxmHelp_loadFile ( Widget text_widget, char *filename )
/************************************************************************
 * NxmHelp_loadFile							*
 *									*
 * This function loads a help file and displays the file into the	*
 * help text widget of the help popup window.				*
 *									*
 * int	NxmHelp_loadFile(text_widget, filename) 			*
 *									*
 * Input parameters:							*
 *  text_widget    Widget	widget id 				*
 *  *filename	   char		name of the help file.			*
 *									*
 * Return parameters:							*
 *  NxmHelp_loadFile	int	1 --- success.				*
 *			 0 --- need to create help dialog first.	*
 *			-1 --- unable to open the help file.		*
 *									*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    05/94						*
 * S. Wang/EAI	    07/96   add file directory checking			*
 * S. Wang/EAI	    10/96   bug fix 					*
 * G. Krueger/EAI   11/97   Renamed functions and internal variables;	*
 *			    Cleaned up code				*
 * T. Piper/SAIC	02/04	Added NxmWarn_show			*
 ***********************************************************************/
{
FILE	*fp;
char	buf[NXM_HELP_MAXLINESIZE];
char	textstr[NXM_HELP_MAXLINES][NXM_HELP_MAXLINESIZE];
int	ii, nn, iret;
char	*ptr, filecur[256], filepath[256], message[LLPATH];
char    messhdr[32] = "ERROR opening help file";
char    messtag[64] = "Cannot display help text.  See system administrator.";
XmTextPosition	textpos;

/*---------------------------------------------------------------------*/
	ptr = strrchr(filename, '/');
	strcpy(filecur, filename);
	filepath[0] = '\0';
	if ( ptr != NULL ) {
	    strcpy(filecur, ptr+1);
	    cst_ncpy(filepath, filename, ptr-filename, &iret);
	}
	fp = cfl_ropn( filecur, filepath, &iret );

	if ( fp != (FILE *)NULL ) {

	    nn = 0;
	    while (( (fgets(buf,NXM_HELP_MAXLINESIZE,fp)) != NULL)
		&& (nn < NXM_HELP_MAXLINES) ) {
	        strcpy(textstr[nn],buf);
	        nn++;
	    }

/*
 * display the help text in the text widget
 */
	    XmTextSetString(text_widget, textstr[0]);

	    for (ii = 1; ii < nn; ii++){
	        textpos = XmTextGetLastPosition(text_widget);
	        XmTextInsert(text_widget, textpos, textstr[ii]);
	    }

	    fclose(fp);
	}
	else {
	    sprintf(message, "%s '%s'.\n%s", messhdr, filename, messtag); 
	    NxmWarn_show(text_widget, message);
	    XtVaSetValues(text_widget, XmNvalue, "", NULL);
	}
	return(1);

}

/*=====================================================================*/
/* ARGSUSED */
void NxmHelp_list_cb ( Widget w, XtPointer clnt, XmListCallbackStruct *call )
 /***********************************************************************
 * NxmHelp_list_cb							*
 *									*
 * This is the callback function for help list				*
 *									*
 * void NxmHelp_list_cb(w, clnt, call)					*
 *									*
 * Input parameters:							*
 *  w		   Widget		ID of the calling widget.	*
 *  clnt	   XtPointer		never used			*
 *  call	   XmListCallbackStruct client data			*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	    07/96						*
 * G. Krueger/EAI   11/97   Renamed functions and internal variables	*
 ***********************************************************************/
{
int	kk;
char	*cur_str;
/*---------------------------------------------------------------------*/
/*
 * find the corresponding help file
 */

	XmStringGetLtoR ( call->item, XmFONTLIST_DEFAULT_TAG,
					&cur_str );

	for ( kk = 0; kk < _helpInfo.hlp_num; kk++ )
		if ( strcmp(cur_str, _helpInfo.title[kk]) == 0 )
		    break;

	if ( kk < _helpInfo.hlp_num )
		NxmHelp_popup( _helpInfo.filename[kk] );

	XtFree(cur_str);

}

/*=====================================================================*/

void NxmHelp_readTbl ( Widget parent, char *index_file )
 /***********************************************************************
 * NxmHelp_readTbl							*
 *									*
 * This function reads help files and titles.				*
 *									*
 * void NxmHelp_readTbl(parent, index_file)				*
 *									*
 * Input parameters:							*
 *	parent		Widget	Widget ID of parent			*
 *	*index_file     char	name of help index table file		*
 *									*
 * Return parameters:							*
 *		NULL							*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	    07/96						*
 * S. Wang/GSC	    10/96	bug fix(strtok cannot interlaced with	*
 *					cst_* function calls		*
 * G. Krueger/EAI   11/97   Renamed functions and internal variables;	*
 *			    Cleaned up code				*
 * T. Piper/SAIC	02/04	Added NxmWarn_show			*
 ***********************************************************************/
{
int	ii, nn, iret, length, intg;
char	idstr[20], titlestr[80], filestr[80];
char	buf[80];
char	*ptr;
char	filepath[256], filecur[256], message[LLPATH];
char	**textstr;
char	messhdr[32] = "ERROR opening help file";
char	messtag[64] = "Help button will not work.  See system administrator.";
FILE	*fp;

/*---------------------------------------------------------------------*/
	ptr = strrchr(index_file, '/');
	strcpy(filecur, index_file);
	filepath[0] = '\0';
	if ( ptr != NULL ) {
	    strcpy(filecur, ptr+1);
	    cst_ncpy(filepath, index_file, ptr-index_file, &iret);
	}
	fp = cfl_ropn( filecur, filepath, &iret );

	if ( fp == (FILE *)NULL ) {
	    sprintf(message, "%s '%s'.\n%s", messhdr, index_file, messtag);
	    NxmWarn_show(parent, message);
	    return;
	}

	textstr = (char **)malloc(HELP_MAXFILENUM * sizeof(char *));

	nn = 0;
	while ((fgets(buf, 80, fp)) != NULL && nn < HELP_MAXFILENUM) {
	    if( *buf == '#' )
		continue;
	    textstr[nn] = (char *)malloc(strlen(buf) +1);
	    strcpy(textstr[nn],buf);
	    nn++;
	}

	fclose(fp);

	_helpInfo.hlp_num  = nn;
	_helpInfo.fileid   = (int *)malloc((size_t)nn * sizeof(int));
	_helpInfo.filename = (char **)malloc((size_t)nn * sizeof(char *));
	_helpInfo.title    = (char **)malloc((size_t)nn * sizeof(char *));

	for (ii = 0; ii < nn; ii++) {
	    strcpy(idstr, strtok(textstr[ii], "|"));
	    strcpy(titlestr, strtok(NULL, "|"));
	    strcpy(filestr, strtok(NULL, " |\t\n"));

	    cst_lstr(idstr, &length, &iret);
	    idstr[length] = '\0';
	    cst_numb( idstr, &intg, &iret);
	    if ( iret != 0 ) {
		printf(" Syntax error in file ID.\n");
		exit(0);
	    }

	    _helpInfo.fileid[ii] = intg;

	    cst_lstr(titlestr, &length, &iret);
	    if ( iret != 0 )  return;

	    titlestr[length] = '\0';
	    _helpInfo.title[ii] = (char *)malloc((size_t)length+1);
	    strcpy(_helpInfo.title[ii], titlestr);

	    _helpInfo.filename[ii] = (char *)malloc(strlen(filestr) + 1);
	    strcpy(_helpInfo.filename[ii], filestr);
	}

	for ( ii = 0; ii < nn; ii++ )
	    free( textstr[ii] );
	free(textstr);
}

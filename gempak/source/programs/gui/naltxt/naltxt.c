#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "naltxt.h"

/*
 *  Private functions
 */
#ifdef Linux
void cancel_cb ( Widget, Widget, XtPointer ) __attribute__((noreturn));
#else
void cancel_cb ( Widget, Widget, XtPointer );
#endif
void CreateFileLists ( Widget parent, char path[] );
void delete_cb ( Widget, XtPointer, XtPointer );
void file_cb ( Widget, XtPointer, XmListCallbackStruct* );
int  FillInFileName ( Widget file_widget, char path[], int max_files );
void FillViewTextShort ( char fullpath[] );
void print_cb ( Widget, char file_in[150], XtPointer );

#define  MAX_FILES	500

#define	 RESOURCE_FILE	"Naltxt"

extern Widget	viewfile_widget;
extern Widget	viewlabel_widget;
extern Widget   viewerror_widget;
extern Boolean	viewerror_up;

char            afos_temp[128];
char            afos_print_temp[128];
char            alarm_dir[128];

static Widget   filelist;
static char     current_file[128];
int		total_file = 0; 
Widget	  	cancel;
Widget	  	wdelete;
Widget	  	print;
char		blank[] = "      ";
int		space, mark_pos;
char		mark = '*';
char 		fullpath[128];
XmString	viewfile_label;

XmStringCharSet	charset = XmFONTLIST_DEFAULT_TAG; 

/************************************************************************
 * naltxt.c								*
 *									*
 * This module displays AFOS alarm products.				*
 *									*
 * CONTENTS:								*
 *									*
 *   file_cb()								*
 *   cancel_cb()							*
 *   delete_cb()							*
 *   print_cb()								*
 *   FillInFileName()							*
 *   CreateFileLists()							*
 *   main()		Main function.					*
 **									*
 * Log:									*
 * G. Krueger/EAI	10/97	Add NxmVers_showTitle & NxmRes_check;	*
 *				Improve header				*
 * S. Jacobs/NCEP	11/97	Changed print command from lp to $LP	*
 * S. Jacobs/NCEP	 3/99	Fixed error in for statement		*
 * H. Zeng/EAI           8/99   List alarmed afos product files in      *
 *                              alphabetical order;                     *
 *                              Get rid of "done" button and reorganize *
 *                              other buttons                           *
 ***********************************************************************/

/*=====================================================================*/

void FillViewTextShort ( char filename[] )
/************************************************************************
 * FillViewTextShort							*
 *									*
 * void FillViewTextShort( filename )					*
 *									*
 * Input parameters:							*
 *	filename[]	char						*
 **									*
 ************************************************************************/
{ 

	Boolean	status;
	Arg	args[10];
	Cardinal num_args;
	char	string[400];
	XmString	motif_string;
	char 	*shortname, *ptr;
	char 	tmpname[128];

/*---------------------------------------------------------------------*/

	strcpy(tmpname, filename);

	ptr = strtok(tmpname,"/");
	while ((ptr = strtok(NULL, "/")) != NULL) {
		shortname = ptr;
		}

	status = FillWidgetWithFile( viewfile_widget, filename);
	if (status == False ) {
		sprintf( string, "Error in loading [%s]", filename);
		motif_string = XmStringCreateLocalized( "Error in loading file");
		num_args = 0;
		XtSetArg( args[num_args], XmNmessageString, motif_string); num_args++;
		XtSetValues( viewerror_widget, args, num_args);
		XmStringFree( motif_string);

		if (viewerror_up == False) {
			XtManageChild( viewerror_widget );
			viewerror_up = True;
			}
		NxmLabel_setStr ( viewlabel_widget, string );
		} /* end of if */
	else	
		NxmLabel_setStr ( viewlabel_widget, shortname);

} 

/*=====================================================================*/
/* ARGSUSED */
void file_cb ( Widget wdgt, XtPointer clnt, 
				XmListCallbackStruct *list )
/************************************************************************ 
 * file_cb								*
 * 									*
 * setup file info							*
 *									*
 * void file_cb( wdgt, clnt, list)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt		Widget						*
 *	clnt		XtPointer					*
 *	*list		XmListCallbackStruct				*
 **                                                                     *
 * T. Piper/SAIC	1/02	Fixed memory leak; freed new_item	*
 ***********************************************************************/
{
	char *choice;
	char newstring[128];
	int  ii, jj;

	XmStringTable new_item;

/*---------------------------------------------------------------------*/

	new_item = (XmStringTable) XtMalloc ( (size_t)list->selected_item_count
						* sizeof(XmString*) );

	for ( ii=0; ii < list->selected_item_count; ii++) {

	  XmStringGetLtoR( list->selected_items[ii],
			charset, &choice);
	  strcpy( current_file, choice+space);
	  strcpy( newstring, choice );
	  newstring[mark_pos] = mark;
	  for ( jj=0 ; jj < space && jj != mark_pos ; jj++ )
		newstring[jj] = blank[jj];
	  new_item[ii] = XmStringCreateLtoR(newstring, charset); 
	  XmListReplaceItems(wdgt, &list->selected_items[ii], 
	  			1, &new_item[ii]);
	  XtFree(choice);
	  }

	XmListSelectItem (filelist, new_item[list->selected_item_count-1], False);

	for (ii=0; ii < list->selected_item_count; ii++) {
	    XmStringFree(new_item[ii]);
	}
	XtFree((XtPointer)new_item);

	if ( ( strlen( current_file ) > (size_t)0 ) &&
	     ( strcmp( current_file, "No Files.." ) != 0)) {
	     strcpy(fullpath, alarm_dir);
	     strcat(fullpath, "/");
	     strcat(fullpath, current_file);

	     XtSetSensitive(wdelete, True);
	     XtSetSensitive(print, True);

	     FillViewTextShort( fullpath );
	}
}

/*=====================================================================*/
/* ARGSUSED */
void cancel_cb ( Widget w, Widget shell, XtPointer call )
/************************************************************************
 * cancel_cb								*
 *									*
 * void cancel_cb(w, shell, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      w	        Widget                                          *
 *      shell		Widget						*
 *      call		XtPointer			                *
 *									*
 **                                                                     *
 ***********************************************************************/
{ 
  XtDestroyWidget(shell);
  exit(0);
}

/*=====================================================================*/
/* ARGSUSED */
void delete_cb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * delete_cb								*
 *									*
 * void delete_cb(w, clnt, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      w		Widget                                          *
 *      clnt		XtPointer	Client data			*
 *      call		XtPointer			                *
 **                                                                     *
 * T. Piper/SAIC	1/02	Fixed FMR; added XtVaGetValues		*
 ***********************************************************************/
{

  XmString	*strlist;
  char 		*text;
  int		cnt;
  Boolean	clean = False;

/*---------------------------------------------------------------------*/

  XtVaGetValues(filelist, XmNitemCount, &cnt, XmNitems, &strlist, NULL);
  while(cnt--) {
   	if (!XmStringGetLtoR(strlist[cnt], charset, &text)) {
	    break;
        }
	if (*(text + mark_pos) == mark) {
	     strcpy(fullpath, alarm_dir);
	     strcat(fullpath, "/");
	     strcat(fullpath, text+space);
	     remove(fullpath);
	     XmListDeleteItem(filelist, strlist[cnt]);
	     XtVaGetValues(filelist, XmNitemCount, &cnt, XmNitems, 
							&strlist, NULL);
	     if (!clean) {
		clean = True;
		XmTextSetString(viewfile_widget, blank);
		XtVaSetValues(viewlabel_widget, 
				XmNlabelString, viewfile_label, 
				NULL);
	     } 
	} /* end of if text ... */

        XtFree(text);

  } /* end of while */
  XtSetSensitive(wdelete, False);
  XtSetSensitive(print, False);
}

/*=====================================================================*/
/* ARGSUSED */
void print_cb ( Widget w, char file_in[150], XtPointer call )
/************************************************************************
 * print_cb								*
 *									*
 * void print_cb(w, file_in, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      w		Widget                                          *
 *      file_in[150]	char	                                        *
 *      call		XtPointer			                *
 **                                                                     *
 * Log:									*
 * S. Jacobs/NCEP	 3/04	Changed to print string from widget	*
 * 				instead of directly from the file	*
 ***********************************************************************/
{
    int		ier, iret;

    char	filnam[73], *printText = NULL;
    FILE	*fpout; 

/*---------------------------------------------------------------------*/
/*      
 *  Open the temporary file.
 */
    strcpy( filnam, "/tmp/nalarm.prnt" );
    fpout = cfl_wopn( filnam, &iret );

    if  ( iret != 0 )  return; 

/*      
 * Write the text to the temporary file.
 */
    printText = XmTextGetString ( viewfile_widget );
    ier = (int)fwrite( printText, sizeof(char), strlen(printText),
			    fpout );

    XtFree ( printText );
    cfl_clos ( fpout, &iret );
    fpout = NULL;

/*      
 * Print the temporary file to the default printer.
 */     
    if ( ier != 0 ) {
	system ( "$LP /tmp/nalarm.prnt" );

/*      
 * Remove the temporary file.
 */     
	unlink( filnam );
    }
}    
     
/*=====================================================================*/

int FillInFileName ( Widget file_widget, char path[], int max_files )
/************************************************************************
 * FillInFileName                                              		*
 *                                                                      *
 * This function lists the files in directory "path" in "file_widget"   *
 *                                                                      *
 * int FillInFileName(file_widget,path,max_files)                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  file_widget  Widget		widget that is going to display files   *
 *  path[]       char		the directory that files are located    *
 *  max_files    int	Maximum number of files that can be displayed   *
 *                                                                      *  
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *  FillInFileName	int   actual number of files that are displayed *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          08/99  Use a different function so that the     *
 *                             output files are displayed in            *
 *                             alphabetical order.                      *
 *                                                                      *
 * T. Piper/SAIC	10/07	Add if check of free dnlist		*
 ***********************************************************************/
{

int	first_file = True;
int	file_count = 0;
int     nsdir; /* Number of subdirectories */
int     ii, jj;    /* index variable */

DIR	*dp;
struct  dirent **dnlist=NULL; /* Array of files in the directory */
struct stat statbuf;
char	filename[128];
char	*ptr;

/*-----------------------------------------------------------------------*/

    if ((dp = opendir( path )) == NULL) { 
	return( file_count);
    }
    closedir(dp);

    strcpy(fullpath, path);
    ptr = fullpath + strlen(fullpath);
    *ptr++ = '/';
    *ptr = 0;
        
    file_count = cfl_rdir(0, path, NULL, &dnlist, &nsdir); 

/* 
 * Read thru the directory
 */

    for ( ii = 0; ii < file_count; ii++ ) {

	strcpy(ptr, dnlist[ii]->d_name);

	if ( lstat(fullpath, &statbuf) < 0 ) {
	    fprintf(stderr, "stat error for %s\n", fullpath);
	    for ( jj = ii; jj < file_count; jj++ ) {
		free(dnlist[jj]);
	    }
	    free(dnlist[ii]);
 	    exit(1);
	}

	if (S_ISREG(statbuf.st_mode)) {
	    if (ii < max_files) { 
		if (first_file == True ) {
		    strcpy(filename, blank);
		    strcat(filename, dnlist[ii]->d_name);
		    AddToList( file_widget, filename, 1);
		    first_file = False;
		}
	        else {
		    strcpy(filename, blank);
		    strcat(filename, dnlist[ii]->d_name);
		    AddToList( file_widget, filename, 0);
	        }
	    }
        }
	free(dnlist[ii]);
    }
    if ( dnlist != NULL ) free(dnlist);

    if ( first_file == True )
	AddToList( file_widget, "No Files...", 1);

    return( file_count );

}

/*=====================================================================*/

void CreateFileLists ( Widget parent, char path[] )
/************************************************************************
 * CreateFileLists							*
 *									*
 * void CreateFileLists( parent, path)					*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 *	path[]		char						*
 **									*
 ***********************************************************************/
{
 	filelist =  XmCreateScrolledList(parent, "filelist", NULL, 0);
 	XtVaSetValues(filelist, XmNitemCount, 0,
 				XmNselectionPolicy, XmEXTENDED_SELECT,
 				XmNvisibleItemCount, 10,
 				NULL); 
	XtManageChild( filelist);
	XtAddCallback( filelist, XmNextendedSelectionCallback,
			(XtCallbackProc)file_cb, NULL);
	space = (int)strlen(blank);
	mark_pos = (space / 2) + (space % 2) - 1;
	total_file = FillInFileName( filelist, path, MAX_FILES);
}

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * main									*
 *                                                                      *
 * This is the main function for naltxt.c                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI  	08/99	Get rid of "done" button and reorganize *
 *                             	other buttons                           *
 * J. Wu/GSC		05/01	free XmStrings                          *
 *                                                                      *
 ***********************************************************************/
  {
  XtAppContext    app;
  Widget	  toplevel;
  Widget	  pane; 
  Widget	  form; 
  XmString        msg;
  struct stat     statbuf;
  struct dirent   *dirp;
  DIR		  *dp;
  char 		  *ptr;
  int 		  file_exist;
  XmString	  cancel_label = XmStringCreateLocalized("exit");

/*---------------------------------------------------------------------*/
/* retrieve application resources */

  strcpy(alarm_dir, argv[1]);

  if ( (dp = opendir(alarm_dir)) == NULL) {
  	strcpy(alarm_dir, argv[1]);
  	strcat(alarm_dir, "/");
  	strcat(alarm_dir, "nawips");
  	if ( (dp = opendir(alarm_dir)) == NULL) {
  		fprintf(stderr, "Can't read directory %s\n", alarm_dir);
  		exit(1);
		}
  	}

  strcpy(afos_temp, alarm_dir);
  ptr = afos_temp + strlen(afos_temp);
  *ptr++ = '/';
  *ptr = 0;

  file_exist = 0;
  while ( (dirp = readdir(dp)) != NULL ) {
	if ((strcmp(dirp->d_name, ".") == 0) ||
	    (strcmp(dirp->d_name, "..") == 0) )
	  continue;

	strcpy(ptr, dirp->d_name);
	if (lstat(afos_temp, &statbuf) < 0) {
	  fprintf(stderr, "stat error for %s\n", afos_temp);
	  exit(1);
	  }

	if (S_ISREG(statbuf.st_mode)) {
	  file_exist = 1;
	  break;
	  }
	} /* end of while */

  closedir(dp);

  if (!file_exist) {
  	fprintf(stderr, "alarm product not found!\n");
  	exit(1);
	}

  toplevel = XtVaAppInitialize(&app, RESOURCE_FILE,
	       NULL, 0, &argc, argv, NULL, NULL);

/*
 * check resource file
 */
  NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

/*
 * display version in title string
 */
  NxmVers_showTitle(toplevel);

  pane = XtVaCreateManagedWidget("pane" ,
     xmPanedWindowWidgetClass, toplevel,
     XmNheight, 480,
     XmNwidth, 560,
     XmNallowResize, True,
     XmNseparatorOn, True,
     NULL);

  msg = XmStringCreateLtoR("ALARMED AFOS PRODUCT:",
			XmFONTLIST_DEFAULT_TAG);

  XtVaCreateManagedWidget("title",
		xmLabelWidgetClass, pane,
		XmNlabelString, msg,
		XmNx, 10,
		XmNy, 20,
		NULL);  
  XmStringFree( msg );
  CreateFileLists(pane, alarm_dir);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, pane, NULL);

  cancel = XtVaCreateManagedWidget("cancel",
    		xmPushButtonWidgetClass, form,
    		XmNlabelString,		cancel_label,		 
                NULL ); 
  XmStringFree( cancel_label );
  XtAddCallback(cancel, XmNactivateCallback, 
		(XtCallbackProc)cancel_cb, toplevel);  
  XtSetSensitive(cancel, True);

  wdelete = XtVaCreateManagedWidget("delete",
    		xmPushButtonWidgetClass, form,
                XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                XmNtopWidget,         cancel,
                XmNtopOffset,         0,
                XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                XmNbottomWidget,      cancel,
                XmNbottomOffset,      0, NULL );
  XtAddCallback(wdelete, XmNactivateCallback, 
		(XtCallbackProc)delete_cb, toplevel);  
  XtSetSensitive(wdelete, False);

  print = XtVaCreateManagedWidget("print",
    		xmPushButtonWidgetClass, form,
                XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                XmNtopWidget,         wdelete,
                XmNtopOffset,         0,
                XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                XmNbottomWidget,      wdelete,
                XmNbottomOffset,      0, NULL );
  XtAddCallback(print, XmNactivateCallback, 
		(XtCallbackProc)print_cb, fullpath);  
  XtSetSensitive(print, False);

  CreateViewText(pane);
  viewfile_label = XmStringCreateLocalized("View Product");
  XtVaSetValues(viewlabel_widget, 
		XmNlabelString, viewfile_label, 
		NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  return(0);
}

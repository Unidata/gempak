#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#define MODEL
#include "model.h"
#include "panel.h"
#include "Nxm.h"
#include "xwcmn.h"  /* gemmap, gemvis */
extern WidgetList subfile_buttons;

/************************************************************************
* models.c								*
*									*
*   Module to take care of creating model selection panel.		*
*									*
**									*
* Log:									*
*   Chien Lin/EAI	9/93						*
*   Chien Lin/EAI      11/96 fix crashing caused by too many files	*
*   G. Krueger/EAI	9/97 Changed NxmWarning -> NxmWarn_show 	*
*   G. Krueger/EAI     11/97 Renamed NxmHelp functions			*
*   T. Piper/GSC	1/00 Replaced NxmWarn_show with printf		*
*   G. Grosshans/SPC   10/00 Changed MAX_FILENO from 100 to 200         *
*   S. Jacobs/NCEP	4/01 Fixed off by 1 error in getdir		*
*   C. Bailey/HPC      10/05 Added NPF fuctionality			*
************************************************************************/

struct	_model_id {
    int	model;
    int	file;
};


/*
 * Private functions
 */
int  compare (const void *, const void *);
void create_model_list ( Widget parent );
int  getdir ( char *dirname, char **model_name, int nmax );
void get_model_files ( char *model_dir, int model_id );
void get_models ( void );
void listmodel_Callback ( Widget, long, XtPointer );
void menu_select_models ( Widget, long, XtPointer );
void model_confirm	( Widget, long, XtPointer );
void model_filelist ( Widget parent );
void Update_Callback	( Widget, XtPointer, XtPointer );
void ModelSelect_Callback ( Widget, XtPointer, XmListCallbackStruct* );

Widget model_listW;
Widget model_toplevel;

/*=============================================================*/

void create_models ( Widget parent )
{
Arg			args[10];
Cardinal		argcnt;
Widget			pane, form, rowcol;
static char		first = 1;

/*------------------------------------------------------------*/

/* Create a popup shell & list widget for the file list. */
	argcnt = 0;
	XtSetArg(args[argcnt], XmNcolormap, gemmap); argcnt++;
	XtSetArg(args[argcnt], XmNvisual,   gemvis); argcnt++;
	model_toplevel = XmCreateBulletinBoardDialog(parent,
			"ModelSelect",args, argcnt);

/*
 * models and files were read through create_model_menu()
 * first time
 */
	if ( !first ) {
		get_models();
		first = 0;
	}

	pane = XtVaCreateManagedWidget("modelpane",
				xmPanedWindowWidgetClass,
				model_toplevel,
				XmNsashWidth,  1,
				XmNsashHeight, 1,
				NULL);

	rowcol = XtVaCreateManagedWidget("ModelRC",
				  xmRowColumnWidgetClass, pane,
				  XmNorientation, XmVERTICAL,
				  XmNpacking, XmPACK_COLUMN,
				  NULL );

	create_model_list(rowcol);

	form = XtVaCreateManagedWidget("ModelFileFrame",
				  xmFormWidgetClass, pane,
				  NULL );

	model_filelist(form);

	rowcol = XtVaCreateManagedWidget("ModelConfirm",
				  xmRowColumnWidgetClass, pane,
				  XmNorientation, XmHORIZONTAL,
				  NULL );

	create_std_buttons( rowcol, (XtCallbackProc)model_confirm );

}

/*=====================================================================*/

void create_model_menu ( Widget parent )
{
int	   ii;
long	   jj;
Widget	   widget, submenu, ssubmenu;
Arg	    args[5];
Cardinal    argcnt;

/*----------------------------------------------------------------------*/

	get_models();

	argcnt = 0;
	XtSetArg(args[argcnt], XmNcolormap, gemmap); argcnt++;
	XtSetArg(args[argcnt], XmNvisual,   gemvis); argcnt++;
	submenu = XmCreatePulldownMenu( XtParent(parent), "submenu",
					args, argcnt);

	XtVaSetValues(parent, XmNsubMenuId, submenu, NULL);

	for(ii = 0; ii < ModelNo; ii++) {
		widget = XtVaCreateManagedWidget ( ModelStr[ii].model_name,
				   xmCascadeButtonWidgetClass, submenu,
				   NULL );

		if ( ModelStr[ii].nfile > 0 ) {

		   argcnt = 0;
		   XtSetArg(args[argcnt], XmNcolormap, gemmap); argcnt++;
		   XtSetArg(args[argcnt], XmNvisual,   gemvis); argcnt++;
		   ssubmenu = XmCreatePulldownMenu( submenu, "submenu",
					args, argcnt);

		   XtVaSetValues( widget, XmNsubMenuId, ssubmenu, NULL);

		   for( jj=0; jj < ModelStr[ii].nfile; jj++) {
		      widget = XtVaCreateManagedWidget (
				   ModelStr[ii].file_names[jj],
				   xmPushButtonWidgetClass, ssubmenu,
				   XmNuserData, ii,
				   NULL );

		      XtAddCallback(widget, XmNactivateCallback,
				(XtCallbackProc)menu_select_models,
				(XtPointer)jj );
		   }

		}
	}

}

/*=====================================================================*/
/* ARGSUSED */
void menu_select_models ( Widget w, long file_id, XtPointer call )
{
long	in, lm;

/*-----------------------------------------------------------------------*/

	OpenModel = 1;

	XtVaGetValues(w, XmNuserData, &lm, NULL);
	in = file_id;

	strcpy(MetaFile,
		  ModelStr[lm].file_fullnames[in]);

	if(MetaFile[0] != '\0') {
		if ( !MpanelMode )
		    clear_window();
		load_meta();
	}
}

/*=================================================================*/

void get_models ( void )
{
int	   i;
char	   *defdir, model_dir[150];
char	   **tempnames;

/*------------------------------------------------------------------*/

	tempnames = (char **)malloc(MAX_MODEL * sizeof (char *) );

	defdir = getenv("NTRANS_META");
	strcpy(model_dir, defdir);

	ModelNo = getdir( model_dir, tempnames, MAX_MODEL );

	for( i = 0; i < ModelNo; i++ ) {

		strcpy( ModelStr[i].model_name, tempnames[i] );

		strcpy( ModelStr[i].model_fullpath, model_dir );
		strcat( ModelStr[i].model_fullpath, "/" );
		strcat( ModelStr[i].model_fullpath, tempnames[i]);
		free(tempnames[i]);
		get_model_files( ModelStr[i].model_fullpath, i );
	}

	free( tempnames );

/*
for( i = 0; i < ModelNo; i++ ) {
printf("###return model[%d] -- %s\n", i, ModelStr[i].model_name);
	for(j=0; j< ModelStr[i].nfile; j++)
printf("  ###file[%d] -- %s\n", j, ModelStr[i].file_names[j]);
}
*/

}

/*==============================================================*/

void create_model_list ( Widget parent )
{
long	ii;
Widget	   widget;

/*-----------------------------------------------------------------*/

	for(ii = 0; ii < ModelNo; ii++) {
		widget = XtVaCreateManagedWidget ( ModelStr[ii].model_name,
				   xmPushButtonWidgetClass, parent,
				   NULL );
		XtAddCallback(widget, XmNactivateCallback,
			  (XtCallbackProc)listmodel_Callback,
			  (XtPointer)ii );
	}
}

/*=============================================================*/

void get_model_files ( char *model_dir, int model_id )
{
int	i, n;
char	**tempnames;

/*-----------------------------------------------------------------*/

	tempnames      = (char **)malloc(MAX_FILENO * sizeof (char *) );

	ModelStr[model_id].nfile = getdir( model_dir, tempnames, MAX_FILENO );

	n =  ModelStr[model_id].nfile;

	for( i = 0; i < n; i++ ) {
		strcpy( ModelStr[model_id].file_names[i], tempnames[n-1-i]);

		strcpy( ModelStr[model_id].file_fullnames[i], model_dir );
		strcat( ModelStr[model_id].file_fullnames[i], "/" );
		strcat( ModelStr[model_id].file_fullnames[i], tempnames[n-1-i]);
		free(tempnames[n-1-i]);
	}

	free( tempnames );
}

/*=====================================================================*/
/* ARGSUSED */
void listmodel_Callback ( Widget w, long model_id, XtPointer call )
{
XmString	*xm_string;
int		ii;

/*---------------------------------------------------------------------*/

	SelectModelNo = model_id;

	xm_string = (XmString *)XtMalloc(sizeof(XmString) *
			(size_t)(ModelStr[model_id].nfile) );

	for ( ii = 0; ii < ModelStr[model_id].nfile; ii++) {
	   xm_string[ii] = XmStringCreate( ModelStr[model_id].file_names[ii],
					XmFONTLIST_DEFAULT_TAG);
	}

	XtVaSetValues(model_listW,
			XmNitems, xm_string,
			XmNitemCount, ModelStr[model_id].nfile,
			NULL);

	if ( ModelStr[model_id].nfile > 0 ) {
		for ( ii = 0; ii < ModelStr[model_id].nfile; ii++ )
			XmStringFree(xm_string[ii]);
		XtFree ( (XtPointer)xm_string);
	}
}

/*=====================================================================*/

int getdir ( char *dirname, char **model_name, int nmax )
{
DIR	*dirp;
struct dirent *dp;
int	n;
size_t	len;
char	message[256];

/*---------------------------------------------------------------------*/

	dirp = opendir(dirname);
	if (dirp == NULL) {
		return (0);
	}

	n = 0;
	while ((dp = readdir(dirp)) != NULL) {
	  if ( dp->d_name[0] != '.'){
		len = strlen(dp->d_name);
		model_name[n] = (char *)malloc( len * sizeof(char)+1 );
		strcpy( model_name[n], dp->d_name );
		n++;
	  }
	  if (n >= nmax) {
	      sprintf(message,
		"Too many files in directory\n %s (max = %d).\n Delete some.",
			dirname, nmax);
	      printf("\n%s\n", message);
	      break;
	  }
	}

	qsort( (void *)model_name, (size_t)n, sizeof(char *), compare );

	(void) closedir(dirp);
	
	return n;
}

/*========================================================================*/

int compare ( const void *a, const void *b )
{
    return strcmp( *(char **)a, *(char **)b );
}

/*=========================================================================*/

void model_filelist ( Widget parent )
{
Arg	    args[10];
Cardinal    argcnt;
Widget	    button;

/*----------------------------------------------------------------------------*/

	argcnt = 0;
	XtSetArg(args[argcnt], XmNselectionPolicy, XmSINGLE_SELECT); argcnt++;
	XtSetArg(args[argcnt], XmNscrollBarPlacement, XmBOTTOM_RIGHT); argcnt++;
	XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_POSITION); argcnt++;
	XtSetArg(args[argcnt], XmNtopPosition, 2); argcnt++;
	XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_POSITION); argcnt++;
	XtSetArg(args[argcnt], XmNleftPosition, 2); argcnt++;
	XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_POSITION); argcnt++;
	XtSetArg(args[argcnt], XmNrightPosition, 98); argcnt++;
	XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_POSITION); argcnt++;
	XtSetArg(args[argcnt], XmNbottomPosition, 80); argcnt++;
	model_listW = XmCreateScrolledList(parent,"model_list",args,argcnt);

	XtAddCallback(model_listW, XmNsingleSelectionCallback,
			(XtCallbackProc)ModelSelect_Callback, NULL);

	XtManageChild(model_listW);

	button = XtVaCreateManagedWidget("Update",
				  xmPushButtonWidgetClass, parent,
				  XmNtopAttachment,	  XmATTACH_POSITION,
				  XmNtopPosition,	  85,
				  XmNleftAttachment,	  XmATTACH_POSITION,
				  XmNleftPosition,	  30,
				  NULL );

	XtAddCallback(button, XmNactivateCallback,
			(XtCallbackProc)Update_Callback, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void Update_Callback ( Widget w, XtPointer clnt, XtPointer call )
{
Position	  x, y;

/*-------------------------------------------------------------------------*/

	create_model_menu( subfile_buttons[0] );

	if ( model_toplevel ) {
		XtVaGetValues( model_toplevel,
					XmNx, &x,
					XmNy, &y,
					NULL);
		XtDestroyWidget( model_toplevel );
	}

	create_models(toplevel_form);

	x -= 6;
	y -= 28;
	XtVaSetValues( model_toplevel,
				XmNdefaultPosition, False,
				XmNx,		    x,
				XmNy,		    y,
				NULL);
	XtManageChild( model_toplevel );

	if ( SelectModelNo > -1 )
		listmodel_Callback(w, SelectModelNo, NULL);

}

/*=====================================================================*/
void update_modellist ( void ) 
{
    get_models();
}

/*=====================================================================*/
/* ARGSUSED */
void ModelSelect_Callback ( Widget w, XtPointer clnt, 
					XmListCallbackStruct *list )
{
    SelectFileNo = list->item_position;
}

/*========================================================================*/

void create_std_buttons ( Widget parent, void (*callback)(Widget,XtPointer,XtPointer) )
{
char		*labels[] = { " Select ", " Help "," Close " };
Arg		args[4];
long		ii;
Cardinal	argcnt;
Widget		pushbutton;

/*--------------------------------------------------------------------------*/

	for( ii = 0; ii< (long)XtNumber(labels); ii++) {
		argcnt = 0;
		XtSetArg(args[argcnt], XmNshadowThickness,3);  argcnt++;
		pushbutton = XtCreateManagedWidget(labels[ii],
					  xmPushButtonWidgetClass,
					  parent,
					  args,
					  argcnt);

		XtAddCallback(pushbutton, XmNactivateCallback,
			(XtCallbackProc)callback,
			(XtPointer)ii);
	}
}

/*=========================================================================*/
/* ARGSUSED */
void model_confirm ( Widget w, long which, XtPointer call )
{

    switch (which) {

	case	0:     /* Select */
		if ( ( SelectFileNo == -1 ) |
			( SelectModelNo == -1) )return;
		strcpy(MetaFile,
		  ModelStr[SelectModelNo].file_fullnames[SelectFileNo - 1]);
		XmListDeselectAllItems(model_listW);
		if ( !MpanelMode )
		    clear_window();
		load_meta();
		break;

	case	1:	/* HELP */
		NxmHelp_helpBtnCb( NULL, 8, NULL);
		break;

	case	2:	/* CANCEL */
		XtUnmanageChild(model_toplevel);
		break;

    }
}

#include "geminc.h"
#include "Nxm.h"
#include "interface.h"
#include "panel.h"

Widget  local_toplevel;


/************************************************************************
*       LOCAL.C                                                    	*
*                                                                  	*
* S. Wang   01/97             clean up                             	*
* J. Wu     05/01             free XmStrings 		               	*
*                                                                  	*
************************************************************************/

void create_local_products ( Widget parent )
{
Arg             args[10];
Cardinal        argcnt, ii;
struct stat     buf;
char            *default_dir, name[150];
Widget          ignorew;
XmStringTable	xmstr;
/*---------------------------------------------------------------------*/
/* Create a popup shell & list widget for the file list. */
        default_dir = (char *)getenv("HOME");
	strcpy(name, default_dir);
	strcat(name , "/meta");

	if ( lstat(name , &buf ) != 0 ) 
		return;

	xmstr = (XmStringTable)XtMalloc(5*sizeof(XmString *));
        argcnt = 0;
        xmstr[argcnt] = XmStringCreateLocalized("Local Products");
	XtSetArg(args[argcnt], XmNtitle, xmstr[argcnt]); 
	argcnt++;
	
        xmstr[argcnt] = XmStringCreateLocalized("Update");
	XtSetArg(args[argcnt], XmNapplyLabelString, xmstr[argcnt]);  
	argcnt++;
        
	xmstr[argcnt] = XmStringCreateLocalized("Close");
        XtSetArg(args[argcnt], XmNcancelLabelString, xmstr[argcnt]);  
	argcnt++;
        
	xmstr[argcnt] = XmStringCreateLocalized(name);
        XtSetArg(args[argcnt], XmNdirectory, xmstr[argcnt]); 
	argcnt++;
        
	xmstr[argcnt] = XmStringCreateLocalized("Select");
        XtSetArg(args[argcnt], XmNokLabelString, xmstr[argcnt]);  
	argcnt++;

        local_toplevel = XmCreateFileSelectionDialog(parent,
                "LocalProduct",args,argcnt);

	for ( ii = 0; ii < argcnt; ii++ ) {
	    XmStringFree( xmstr[ ii ] );
	}	
	XtFree((XtPointer)xmstr);
	
	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_DIR_LIST);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_TEXT);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_SELECTION_LABEL);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_FILTER_TEXT);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_FILTER_LABEL);
	XtUnmanageChild(ignorew);

	ignorew = XmFileSelectionBoxGetChild(local_toplevel,
		XmDIALOG_DIR_LIST_LABEL);
	XtUnmanageChild(ignorew);

        XtAddCallback(local_toplevel, XmNokCallback,
			(XtCallbackProc)LocalProduct_Callback, (XtPointer)0);

        XtAddCallback(local_toplevel, XmNcancelCallback,
			(XtCallbackProc)LocalProduct_Callback, (XtPointer)1);

        XtAddCallback(local_toplevel, XmNapplyCallback,
			(XtCallbackProc)LocalProduct_Callback, (XtPointer)2);
}

/*=====================================================================*/
/* ARGSUSED */
void LocalProduct_Callback ( Widget w, long which, 
				XmFileSelectionBoxCallbackStruct *call )
{
Widget list;
int    itemc;
/*---------------------------------------------------------------------*/
	switch( which ) {

	    case 0: /* ok callback (select) */
		OpenModel = 1;
		Select_File_Callback( local_toplevel,
			NULL, call);
		XtManageChild( group_select_toplevel );
		break;

	    case 1: /* cancel callback (close)  */
		XtUnmanageChild( local_toplevel );
		break;

	    case 2: /* apply callback ( update model ) */
		list = XmFileSelectionBoxGetChild(
			local_toplevel,
				XmDIALOG_LIST);
		XtVaGetValues(list, 
		    XmNitemCount, &itemc,
		    NULL); 
		XmListDeselectAllItems(list);
		XmFileSelectionDoSearch( local_toplevel,
			call->mask);
		XmListSetHorizPos(list, call->dir_length);
		XtVaSetValues(list, 
		    XmNitemCount, itemc,
		    NULL); 
		break;
	}
}

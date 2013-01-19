#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

static	char	_applWin[20];	     /* application window name */

					/* global widgets	*/
static	Widget	_gifW;
static	Widget	_fileTxtW, _frameFil;

/*
 *  Private functions
 */
void _gifFileName (     Widget  parent );
void _gifCtlBtn (       Widget  parent );
void _gifOkCb (         Widget, XtPointer, XtPointer );
void _gifCancelCb (     Widget, XtPointer, XtPointer );
void _gifHelpCb (       Widget, XtPointer, XtPointer );
void _exportToGif (     char  *gif_file );


/************************************************************************
 * NxmGif.c								*
 *									*
 * This module contains functions that creates the export Gif panel. 	*
 *									*
 * CONTENTS:								*
 *									*
 * NxmGif_create()	create the print popup module.			*
 * NxmGif_gifWPopup()	pops up the Gif widget panel. 			*
 *									*
 * _gifFileName()	create PS file name input field.		*
 * _gifCtlBtn() 	create the control buttons for the popup	*
 *									*
 * _gifOkCb()		ok button callback function			*
 * _gifCancelCb()	cancel button callback function 		*
 * _gifHelpCb() 	help button callback function			*
 *									*
 * _exportToGif() 	print to a PostScript file			*
 ***********************************************************************/


/*=====================================================================*/

Widget NxmGif_create ( char *wname, Widget parent )
/************************************************************************
 * NxmGif_create							*
 *									*
 * This function create the export Gif popup panel			*
 *									*
 * Widget NxmPrt_create( wname, parent, print_func )			*
 *									*
 * Input parameters:							*
 *	wname		char*	 window name				*
 *	parent		Widget	 parent widget ID			*
 *	*print_func()	void	application printing function		*
 *									*
 * Output parameters:							*
 *	NxmGif_create	Widget	 ID of the print popup panel		*
 *									*
 ** Log:								*
 *  C.Bailey/HPC	2/05						*
 *  E. Safford/SAIC	04/05	rm XtRealizeWidget on _gifW		*
 ***********************************************************************/
{
Widget		pane;
XmString	str;

/*---------------------------------------------------------------------*/

	strcpy(_applWin, wname);

	_gifW = XmCreateBulletinBoardDialog(parent,
		"gifPanel", NULL, 0);

	str = XmStringCreateSimple("Export GIF Setup");

	XtVaSetValues(_gifW,
		XmNnoResize,		True,
		XmNdialogTitle, 	str,
		NULL);

	XmStringFree(str);

	pane = XtVaCreateManagedWidget("gifpane",
		xmPanedWindowWidgetClass,	_gifW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL);

	_gifFileName(pane);
        _gifCtlBtn(pane);

	return( _gifW );

}

/*=====================================================================*/

void NxmGif_gifWPopup ( void )
/************************************************************************
 * NxmGif_gifWPopup							*
 *									*
 * This function pops up the Export GIF panel				*
 *									*
 * void NxmGif_gifWPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *  C. Bailey/HPC	02/05						*
 ***********************************************************************/
{
    XtManageChild(_gifW);
}

/*=====================================================================*/

void _gifFileName ( Widget parent )
/************************************************************************
 * _gifFileName								*
 *									*
 * This function create GIF file name input area.			*
 *									*
 * void _gifFileName(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		parent widget ID		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * C. Bailey/HPC	02/05						*
 ***********************************************************************/
{
Widget	bb;
char	label[100];
XmString  str;

/*---------------------------------------------------------------------*/

	_frameFil = XtVaCreateManagedWidget("frmFil",
		xmFrameWidgetClass,	parent,
		NULL);

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, _frameFil,
		NULL);

	strcpy(label, "Export to File:");

	str = XmStringCreateSimple(label);

	XtVaCreateManagedWidget("fileLabel",
		xmLabelWidgetClass,	bb,
		XmNlabelString, 	str,
		XmNx,			20,
		XmNy,			14,
		NULL);

	XmStringFree(str);

	_fileTxtW = XtVaCreateManagedWidget("fileTxt",
		xmTextFieldWidgetClass, bb,
		XmNx,			160,
		XmNy,			6,
		XmNcolumns,		16,
		NULL);

	XmTextSetString(_fileTxtW, "gempak.gif");

}

/*=====================================================================*/

void _gifCtlBtn ( Widget parent )
/************************************************************************
 * _gifCtlBtn								*
 *									*
 * This function create the control buttons				*
 *									*
 * void _gifCtlBtn(parent)						*
 *									*
 * Input parameters:							*
 *	parent	  Widget	parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	C. Bailey/HPC   02/05						*
 ***********************************************************************/
{
Widget	   bb, button ;
XmString   str;
char *buttonlist[] = { "OK", "Help", "Cancel" };

/*---------------------------------------------------------------------*/

	bb = XtVaCreateManagedWidget("bb",
		xmBulletinBoardWidgetClass, parent,
		XmNy,			20,
		NULL);

	str = XmStringCreateSimple(buttonlist[0]);
	button = XtVaCreateManagedWidget("ok_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
		      (XtCallbackProc)_gifOkCb, NULL );

	str = XmStringCreateSimple(buttonlist[1]);
	button = XtVaCreateManagedWidget("help_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20+120,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
				   _gifHelpCb, NULL );

	str = XmStringCreateSimple(buttonlist[2]);
	button = XtVaCreateManagedWidget("cancel_bt",
			xmPushButtonWidgetClass, bb,
			XmNwidth,		 80,
			XmNx,			 20+2*120,
			XmNlabelString, 	 str,
			NULL);
	XmStringFree(str);
	XtAddCallback(button, XmNactivateCallback,
				   _gifCancelCb, NULL );
}

/*=====================================================================*/
/* ARGSUSED */
void _gifOkCb ( Widget w, XtPointer clnt, XtPointer call )
 /***********************************************************************
 * _gifOkCb								*
 *									*
 * This is the callback function for the OK control button		*
 *									*
 * void _gifOkCb(w, clnt, call) 					*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *  C. Bailey/HPC 	    02/05					*
 ***********************************************************************/
{
char	giffile[256];
char	*str;

/*---------------------------------------------------------------------*/
/*
 * get GIF file name
 */
        str = XmTextGetString(_fileTxtW);
	strcpy (giffile, str);
	XtFree(str);

	if (giffile[0] == '\0') {
            XmTextSetString(_fileTxtW, "gempak.gif");
	}
	
/*
 * pops down the export GIF panel
 */
	XtUnmanageChild(_gifW);

/*
 * export to GIF file
 */
	_exportToGif(giffile);
}

/*=====================================================================*/
/* ARGSUSED */
void _gifCancelCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * _gifCancelCb 							*
 *									*
 * This is the cancel button callback function				*
 *									*
 * void _gifCancelCb(w, clnt, call)					*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	C. Bailey/HPC		02/05					*
 ***********************************************************************/
{
char	giffile[256], *str;
/*---------------------------------------------------------------------*/

	str = XmTextGetString(_fileTxtW);
	strcpy (giffile, str);
	XtFree(str); 

	if ( giffile[0] == '\0' )
	    XmTextSetString(_fileTxtW, "gempak.gif");

	XtUnmanageChild(_gifW);

}

/*=====================================================================*/
/* ARGSUSED */
void _gifHelpCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * _gifHelpCb								*
 *									*
 * This is the callback function for HELP button			*
 *									*
 * void _gifHelpCb(w, clnt, call)					*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget id			*
 *	clnt	XtPointer	never used				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 *	C. Bailey/HPC	 02/05						*
 ***********************************************************************/
{
    NxmHelp_helpBtnCb(w, 46, NULL);
}

/*=====================================================================*/

void _exportToGif ( char *gif_file )
 /***********************************************************************
 * _exportToGif								*
 *									*
 * this function print to a PostScript file				*
 *									*
 * void _exportToGif(gif_file)						*
 *									*
 * Input parameters:							*
 *  gif_file	*char	 gif file name					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Bailey/HPC	 2/05						*
 * E. Safford/SAIC	04/05	use NxmWarn_show to report errors	*
 ***********************************************************************/
{
int	ier, iframe, nframe;

/*---------------------------------------------------------------------*/
/*
 * Set frame index and number of frames to 1
 */
    iframe = 1;
    nframe = 1;

/*
 * Call graphic save function
 */
    ggsave (gif_file,&iframe,&nframe,&ier,strlen(gif_file));

/*
 * reset print in process flag
 */
    if ( ier != 0) {
	NxmWarn_show( _gifW, "File Extension not .gif -No File Created" ); 
    }
}

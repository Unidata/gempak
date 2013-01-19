#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"
#include "proto_xw.h"

/*
 *  Private functions
 */
void _NxmColTabLoadCallback  ( Widget, XtPointer, XmFileSelectionBoxCallbackStruct* );
void _NxmColTabPanelCallback ( Widget,      long, XtPointer );
int  _NxmColTabSaveCallback  ( Widget,    String, XtPointer );
void  NxmGetColorInTable ( int index, float *red, float *green, float *blue );
int   NxmSaveColorTable ( Widget w, char *filename, int ncolors, 
				Pixel color_pixels[] );
/************************************************************************
 * NxmColorTable.c							*
 *									*
 * CONTENTS:								*
 *	NxmLoadColorTable						*
 *	_NxmColTabLoadCallback						*
 *	_NxmColTabPanelCallback						*
 *	_NxmColTabSaveCallback						*
 *	NxmGetColorInTable						*
 *	NxmSaveColorTable						*
 ***********************************************************************/

int NxmLoadColorTable ( Widget w, char *filename )
/************************************************************************
 * NxmLoadColorTable                                              	*
 *                                                                      *
 * This function reads a color table file consisting of the RGB         *
 * definitions of each color pixel and changes the corresponding colors.*
 * The value of each RGB component in the file is between 0 and 255.	*
 *									*
 * int NxmLoadColorTable (w, filename )          			*
 *                                                                      *
 * Input parameters:                                                    *
 *  w       	 	Widget  widget ID                              	*
 *  *filename   	char	name of the color table file		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      int	--  0: cannot open the specified file	*
 *                                  1: success                          *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    11/97	replace XStoreColor() with gscrgb()	*
 * I. Durham/GSC    05/98	changed underscore decl. to an include	*
 * T. Piper/SAIC	01/04	replaced fopen with cfl_tbop		*
 * T. Piper/SAIC	01/04	replaced fscanf with cfl_trln & sscanf	*
 * T. Piper/SAIC	01/04	changed calling sequence		*
 ***********************************************************************/
{
FILE	*fp;
char	buff[128], dummy[20];
int	ired[GRAPH_COLORS], igreen[GRAPH_COLORS], iblue[GRAPH_COLORS];
int     ii, ncolors, ier;

/*---------------------------------------------------------------------*/

	fp = cfl_tbop(filename, CLR_DIR, &ier);
	if ( fp == NULL  ||  ier != 0 ) {
	    printf("Warning: Cannot find graphic color table %s.\n",
                    CLR_TBL );
	    return(0);
	} 

	ii = 0;

	while ( !feof(fp) ) {
            cfl_trln(fp, sizeof(buff), buff, &ier);
	    if ( ier == 0 ) {
	        sscanf(buff, "%s %s %d %d %d %s", 
			dummy, dummy, &ired[ii], 
			&igreen[ii], &iblue[ii], dummy); 
	        ii++;
	    }
	}
	ncolors = ii--;
	if ( ncolors > GRAPH_COLORS ) {
	    ncolors = GRAPH_COLORS;
	}
	

	for (ii = 1; ii <ncolors ; ii++){

            NXMtabRed[ii]   = (float)ired[ii]/255.0F;
            NXMtabGreen[ii] = (float)igreen[ii]/255.0F;
            NXMtabBlue[ii]  = (float)iblue[ii]/255.0F;
            gscrgb( &ii, &ired[ii], &igreen[ii], &iblue[ii], &ier );

	}

	if ( !NXMisInitialized )
	    NxmInitialize( w );

	fclose(fp);
	return(1);
}

/*======================================================================*/

int NxmSaveColorTable ( Widget w, char *filename, int ncolors, 
						Pixel color_pixels[] )
/************************************************************************
 * NxmSaveColorTable                                                    *
 *                                                                      *
 * This function writes the RGB definition of each color pixel into a   *
 * color table file. The value of each RGB component in the file is     *
 * between 0.0 - 1.0.    						*
 *                                                                      *
 * int NxmSaveColorTable (w, filename, ncolors, color_pixels )          *
 *                                                                      *
 * Input parameters:                                                    *
 *  w            	Widget	widget ID                               *
 *  *filename    	char	name of the color table file            *
 *  ncolors      	int	number of colors                        *
 *  color_pixels[]	Pixel	index array of color pixels             *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      int     --  0: cannot open the specified file   *
 *                                  1: success                          *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * G. Krueger/EAI   09/97	Changed NxmWarning -> NxmWarn_show	*
 * T. Piper/SAIC	07/04	Replaced 256 with COLR_SCAL		*
 ***********************************************************************/
{
int    ii, temp[3];
FILE   *fp;
XColor xcolor;

/*---------------------------------------------------------------------*/

	fp = fopen(filename,"w");

	if ( fp == NULL ) {
            NxmWarn_show(w, "You may not have write permission in "
			   "this directory.");
	    return(0);
	}
	else {

	    if ( !NXMisInitialized )
	        NxmInitialize( w );

	    for ( ii = 1; ii < ncolors; ii++) {

		xcolor.pixel = color_pixels[ii];
		xcolor.flags = DoRed | DoGreen | DoBlue;
		XQueryColor(NXMdisplay, NXMcmap, &xcolor);

		temp[0] = xcolor.red/COLR_SCAL;
		temp[1] = xcolor.green/COLR_SCAL;
		temp[2] = xcolor.blue/COLR_SCAL;

		fprintf(fp, "Color%d C%d %d %d %d XC%d\n",
                        ii, ii, temp[0], temp[1], temp[2], ii);
	    }
	}
	fclose(fp);
	return(1);
}

/*=====================================================================*/

void NxmSetColorInTable ( int index, float red, float green, float blue )
/************************************************************************
 * NxmSetColorInTable							*
 *                                                                      *
 * This function sets the RGB components for the specified color in the *
 * color table.								*
 *                                                                      *
 * void NxmSetColorInTable(index, red, green, blue)          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  index        int     color pixel index in the color table		*
 *  red          float   red component of the color between 0.0 - 1.0   *
 *  green        float   green component of the color between 0.0 - 1.0 *
 *  blue         float   blue component of the color between 0.0 - 1.0  *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    NXMtabRed[index]   = red;
    NXMtabGreen[index] = green;
    NXMtabBlue[index]  = blue;
}

/*=====================================================================*/

void NxmGetColorInTable ( int index, float *red, float *green, float *blue )
/************************************************************************
 * NxmGetColorInTable                               			*
 *                                                                      *
 * This function gets the RGB components for the specified color in the *
 * color table.								*
 *                                                                      *
 * void NxmGetColorInTable(index, red, green, blue)          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  index        int     color pixel index in the color table		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *red         float   red component of the color between 0.0 - 1.0   *
 *  *green       float   green component of the color between 0.0 - 1.0 *
 *  *blue        float   blue component of the color between 0.0 - 1.0  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    *red   = NXMtabRed[index];
    *green = NXMtabGreen[index];
    *blue  = NXMtabBlue[index];
}

/*=====================================================================*/

void NxmColorTablePanelCreate ( Widget parent )
/************************************************************************
 * NxmColorTablePanelCreate                               		*
 *                                                                      *
 * This function creates the color table panel which has three buttons: *
 * load ( load color table ), save ( save color table ) and default     *
 * (load default color table). The callback functions and the popup up  *
 * dialogs for load and save actions are also created here.   		*
 *                                                                      *
 * void NxmColorTablePanelCreate(parent)          			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget     parent Widget ID				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	   05/94						*
 * C. Lin/EAI	   01/95 change help.hlp from NTRANS_HELP to		*
 *			 NAWIPS_HELP					*
 * G. Krueger/EAI  08/97 Eliminate non-functioning help button		*
 * G. Krueger/EAI  09/97 Changed NxmWarning -> NxmWarn_show		*
 * G. Krueger/EAI  09/97 _NxmClosePopupCallback -> NxmClose_popupCb	*
 * G. Krueger/EAI  11/97 NxmPromptPopupCreate->NxmPrompt_create		*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
Widget		form, rc, button, fb, helpw, prompt;
Cardinal	argcnt;
Arg		args[10];
char		*labels[] = { "Load", "Save", "Default"};
long		ii;
XmString	onestr, twostr, thrstr;

/*---------------------------------------------------------------------*/

        form = XtCreateManagedWidget("form",
                xmFormWidgetClass, parent,
                NULL, 0);

	XtVaCreateManagedWidget("Color Table Options:",
		xmLabelWidgetClass,     form,
                XmNleftAttachment,      XmATTACH_POSITION,
                XmNleftPosition,        2,
                XmNtopAttachment,       XmATTACH_POSITION,
                XmNtopPosition,         10,
		NULL);
 
        rc = XtVaCreateManagedWidget("rc",
                xmRowColumnWidgetClass, form,
        	XmNleftAttachment,      XmATTACH_POSITION,
        	XmNleftPosition,        10,
        	XmNtopAttachment,       XmATTACH_POSITION,
        	XmNtopPosition,         50,
        	XmNorientation,         XmHORIZONTAL,
		NULL);

/* create the file selection box for LOAD button */
        argcnt = 0;
	onestr = XmStringCreateLocalized("Search");
	twostr = XmStringCreateLocalized("Select");
	thrstr = XmStringCreateLocalized("*.clr");
        XtSetArg(args[argcnt], XmNtitle,
		"Color Table File"); argcnt++;
        XtSetArg(args[argcnt], XmNapplyLabelString, onestr);  argcnt++;
        XtSetArg(args[argcnt], XmNokLabelString, twostr);  argcnt++;
        XtSetArg(args[argcnt], XmNpattern, thrstr); argcnt++;

        fb = XmCreateFileSelectionDialog(rc,
                "fb",args,argcnt);

	XmStringFree (onestr);
	XmStringFree (twostr);
	XmStringFree (thrstr);

        XtAddCallback(fb, XmNokCallback,
                (XtCallbackProc)_NxmColTabLoadCallback, NULL);

        XtAddCallback(fb, XmNcancelCallback,
                (XtCallbackProc)NxmClose_popupCb, fb);

	helpw = XmFileSelectionBoxGetChild(fb, XmDIALOG_HELP_BUTTON);

	XtUnmanageChild(helpw);

/* create the prompt dialog for SAVE button */

	prompt = NxmPrompt_create(rc,
                   "Save Color Table", "File Name",
                   (XtCallbackProc)_NxmColTabSaveCallback);

        for (ii=0; ii<(long)XtNumber(labels); ii++) {

            button = XtVaCreateManagedWidget(labels[ii],
			xmPushButtonWidgetClass, rc,
                        NULL);

	if ( ii == 0 )
	    XtVaSetValues( button, XmNuserData, fb, NULL);

	if ( ii == 1 )
	   XtVaSetValues( button, XmNuserData, prompt, NULL);

            XtAddCallback(button, XmNactivateCallback,
			(XtCallbackProc)_NxmColTabPanelCallback, 
			(XtPointer)ii);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmColTabPanelCallback ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * _NxmColTabPanelCallback                                              *
 *                                                                      *
 * _NxmColTabPanelCallback ( w, which, cbs )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w                   Widget  widget ID                               *
 *  which             	long					        *
 *  cbs                XtPointer                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        01/04   modified for NxmLoadColorTable change	*
 ***********************************************************************/
{
Widget  fb, popup;
XmString dir_mask;

/*---------------------------------------------------------------------*/

    switch (which) {

        case 0:     /* Load Color Table */
	    XtVaGetValues( w, XmNuserData, &fb, NULL);
	    XtVaGetValues(fb, XmNdirMask, &dir_mask, NULL);
	    XmFileSelectionDoSearch(fb, dir_mask);
	    XtManageChild(fb);
	    break;

        case 1:     /* Save Color Table */
	    XtVaGetValues( w, XmNuserData, &popup, NULL);
       	    XtManageChild(popup);
            break;

        case 2:     /* Load Default Table */
	    NxmLoadColorTable( w, CLR_TBL );
	    NxmColorbarSetBlack();
            break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmColTabLoadCallback ( Widget w, XtPointer clnt,
				XmFileSelectionBoxCallbackStruct *select )
/************************************************************************
 * _NxmColTabLoadCallback                                               *
 *                                                                      *
 * _NxmColTabLoadCallback ( w, clnt, select )                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w                   Widget  widget ID                               *
 *  clnt              	XtPointer				        *
 *  *select		XmFileSelectionBoxCallbackStruct                *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 ***********************************************************************/
{
char *filename;

/*----------------------------------------------------------------------*/

        if (!XmStringGetLtoR(select->value,
                XmFONTLIST_DEFAULT_TAG,&filename))
        return;

        if (!*filename) {
            printf("\n No color table file has been selected.\n");
        }
        else {
            XtUnmanageChild(w);
            NxmLoadColorTable( w, filename );
        }
	XtFree(filename);
}

/*=====================================================================*/
/* ARGSUSED */
int _NxmColTabSaveCallback ( Widget w, String fname, XtPointer cbs )
/************************************************************************
 * _NxmColTabSaveCallback                                               *
 *                                                                      *
 * int _NxmColTabSaveCallback ( w, fname, cbs )          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w                   Widget  widget ID                               *
 *  fname              String    name of the color table file            *
 *  cbs		XtPointer					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *  _NxmColTabSaveCallback int  --  0: cannot open the specified file   *
 *                                  1: success                          *
 **                                                                     *
 ***********************************************************************/
{
int             status;
char            filename[150];

/*---------------------------------------------------------------------*/

	strcpy( filename, fname );

        if (filename[0] != (char)NULL){
	    strcat( filename, ".clr" );
            status = NxmSaveColorTable( w, filename, (int)NXMtotalEditColor, 
		    				NXMcolrEditPixels );
	}
      	else {
            NxmWarn_show(w, "Please Specify the File Name.");
            status = 0;
      	}

	return( status );
}

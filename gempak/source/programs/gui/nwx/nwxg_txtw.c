#include "nwx_cmn.h"
#include "nwx_gui.h"


void txtw_prvnxtCb ( Widget, XtPointer, XtPointer );
void txtw_prntbCb ( Widget, XtPointer, XtPointer );
void txtw_modeCb ( Widget, long which, XmToggleButtonCallbackStruct *cbs);

Widget prodtxtW;	/* product group/name display */
Widget dttmtxtW;	/* date/time display widget in text window  */
Widget textW;		/* widget to display text report */

Widget prevBtnW, nextBtnW, prntBtnW; /* previous,next,and print button */

/************************************************************************
 * txtw.c                                                               *
 *                                                                      *
 * This module creates the text display window and defines its          *
 * callbacks.								*
 *                                                                      *
 * CONTENTS:                                                            *
 *      txtw_create()     creates the text report window.               *
 *      txtw_prvnxtCb()   callback for the previous and next button.    *
 *      txtw_prntbCb()    callback for the print button.        	*
 *      txtw_prdgrpSet()  set the prod/group name in text window.       *
 *      txtw_dttmSet()    set date/time info in text window.        	*
 *	txtw_setPrevNext  set the previous & next buttons		*
 *      txtw_dsplyReport  display the text report			*
 ***********************************************************************/

/*=====================================================================*/

Widget txtw_create ( Widget parent )
/************************************************************************
 * txtw_create								*
 *									*
 * Widget txtw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		The top level widget		*
 *									*
 * Return parameters:							*
 *      txtw_create	Widget          The text report window widget   *
 **									*
 * Log:									*
 * C. Lin/EAI            8/95    restructured from nwx2.1               *
 * L. Williams/EAI	 5/96	 remove XmNwidth, XmNheight and         *
 *				 XmNcolumns from textW                  *
 * G. Krueger/EAI	 9/97	 NxmMcloseNone -> NxmClose_menuRmEntry	*
 * R. Tian/SAIC		 4/03	 add Replace/Append radio buttons	*
 * T. Piper/SAIC	02/04	Removed check for Motif 1.1		*
 * H. Zeng/SAIC         07/04	re-designed to use FormDialog		*
 * H. Zeng/SAIC		07/04	removed location info			*
 ***********************************************************************/
{
Widget		txtform, frame, form, textbb, textcntl,
		mode_radio;
Cardinal	n;
Arg		wargs[10];
XmString	msg, replace, append;

/*---------------------------------------------------------------------*/
/*
 * Create the text output dialog window.
 */
        txtform = XmCreateFormDialog ( parent, "texttop", NULL, 0 );

	XtVaSetValues(XtParent(txtform), 
		      XmNtitle,            "Text Report", 
		      XmNdeleteResponse,   XmDO_NOTHING,
                      NULL);

/* 
 * Make text window none resizable.
 */
        XtVaSetValues ( txtform, 
		        XmNnoResize,		 True, 
		        XmNdefaultPosition,      False,
		        NULL );

/*
 * widgets for group/product info and date/time info
 */
	frame = XtVaCreateWidget("frame",
			xmFrameWidgetClass, txtform,
			XmNshadowType,      XmSHADOW_IN,
			XmNtopAttachment,   XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL);

	form = XtVaCreateWidget("form",
			xmFormWidgetClass, frame,
			NULL);

	msg = XmStringCreateLocalized(" ");
	prodtxtW = XtVaCreateManagedWidget("prodtxtW",
			xmLabelWidgetClass, form,
			XmNlabelString,     msg,
			XmNtopAttachment,   XmATTACH_POSITION,
			XmNtopPosition,     20,
                        XmNleftAttachment,  XmATTACH_FORM,
			NULL);
	XmStringFree(msg);

	dttmtxtW = XtVaCreateManagedWidget("dttmtxtW",
			xmTextFieldWidgetClass,   form,
			XmNeditable, 		  False,
			XmNcursorPositionVisible, False,
			XmNcolumns, 		  20,
			XmNtopAttachment, 	  XmATTACH_FORM,
			XmNrightAttachment, 	  XmATTACH_FORM,
			NULL);

	textbb   = XtVaCreateManagedWidget( "textbb",
			xmBulletinBoardWidgetClass, txtform,
			XmNtopAttachment,   	    XmATTACH_WIDGET,
			XmNtopWidget, 		    frame,
			XmNrightAttachment, 	    XmATTACH_FORM,
			XmNleftAttachment,          XmATTACH_FORM,
			NULL );
	XtManageChild( form ); 
	XtManageChild( frame ); 

/*
 * create text display window.  
 */ 
	n = 0;
	XtSetArg( wargs[n], XmNrows,              900 ); n++;
	XtSetArg( wargs[n], XmNscrollVertical,   True ); n++;
	XtSetArg( wargs[n], XmNscrollHorizontal, True ); n++;
	XtSetArg( wargs[n], XmNeditMode,         XmMULTI_LINE_EDIT ); n++;
	XtSetArg( wargs[n], XmNeditable,         False); n++;
	XtSetArg( wargs[n], XmNcursorPositionVisible,   False); n++;
	textW  = XmCreateScrolledText( textbb, "textwin", wargs, n );
	XtManageChild( textW ); 

	textcntl = XtVaCreateManagedWidget( "textbb",
			xmBulletinBoardWidgetClass, txtform,
			XmNtopAttachment,    	    XmATTACH_WIDGET,
			XmNtopWidget,               textbb, 
			XmNrightAttachment,         XmATTACH_FORM,
			XmNleftAttachment,          XmATTACH_FORM,
			XmNbottomAttachment,        XmATTACH_FORM,
			NULL );

/*
 * create replace/append radio buttons
 */
	msg = XmStringCreateLocalized("Text Mode:");
	XtVaCreateManagedWidget("mode_label",
			xmLabelWidgetClass, textcntl,
			XmNlabelString,     msg,
			XmNx,               25, 
			XmNy,               0,
			NULL);
	XmStringFree(msg);
	replace = XmStringCreateLocalized("Replace");
	append  = XmStringCreateLocalized("Append");
        mode_radio = XmVaCreateSimpleRadioBox(textcntl,
                        "mode_radio", 0, (XtCallbackProc)txtw_modeCb,
                        XmVaRADIOBUTTON,     replace, NULL, NULL, NULL,
                        XmVaRADIOBUTTON,     append, NULL, NULL, NULL,
			XmNmarginHeight,     0,
                        XmNnumColumns,       2,
                        XmNspacing,          50,
                        XmNradioBehavior,    TRUE,
			XmNx,                120,
			XmNy,		     0,
                        NULL);
        XtManageChild(mode_radio);
        XmStringFree(replace);
        XmStringFree(append);

/*
 * create previous/next/print buttons
 */
	prevBtnW = XtVaCreateManagedWidget( " Previous ",
			xmPushButtonWidgetClass, textcntl,
			XmNx,  50, 
			XmNy,  45,
			NULL );
	XtAddCallback( prevBtnW, XmNactivateCallback, txtw_prvnxtCb,
			(XtPointer) -1 );
		    
	nextBtnW = XtVaCreateManagedWidget( "   Next   ",
			xmPushButtonWidgetClass, textcntl,
			XmNx, 250,
			XmNy,  45,
			NULL );
	XtAddCallback( nextBtnW, XmNactivateCallback, txtw_prvnxtCb,
			(XtPointer) 1 );

	prntBtnW = XtVaCreateManagedWidget ( "  Print  ",
			xmPushButtonWidgetClass, textcntl,
			XmNx, 450,
			XmNy,  45,
			NULL ); 

	XtAddCallback( prntBtnW, XmNactivateCallback, txtw_prntbCb,
			NULL );

        txtw_setPrevNext( G_FALSE, G_FALSE );

	XtManageChild(txtform);

	return(txtform);
	
}

/*=====================================================================*/
/* ARGSUSED */
void txtw_prvnxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * txtw_prvnxtCb							*
 *                                                                      *
 * This routine is the callback for the previous/next report button.    *
 * The user may request the previous/next report for single stations    *
 * only.   								*
 *                                                                      *
 * txtw_prvnxtCb ( wid, clnt, cbs )                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          The input widget                *
 *      clnt            XtPointer       The input data for the widget   *
 *      cbs		XtPointer       The callback data for the widget*
 *                                                                      *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * L. Williams/EAI      12/94   Add code to process UVI info            *
 * C. Lin/EAI	         9/95    					*
 * D. Kidwell/NCEP	 8/98   Add code to process watch box		*
 * T. Piper/GSC		 6/01	Initialized iret; note no iret on 	*
 *							       fosd_wbox*
 * T. Piper/SAIC	01/04	Added NxmWarn_show			*
 * E. Safford/SAIC	12/07	wrap NxmWarn_show()			*
 ***********************************************************************/
{
int             which, iret=0;
char message[55] = "NWX:  ERROR loading data.  See system administrator.";
/*---------------------------------------------------------------------*/

	which = (long)clnt;

/*
 * set the flag in usrSelect structure
 */
	usrSelect.prvnxt = 1;

/*
 * Set the search flag.
 */
	srchInfo.sflag = which;

	switch ( which ) {

	    case -1:	/* previous button */

/*
 * activate next button
 */
        	if (!XtIsSensitive(nextBtnW) )
           	    XtSetSensitive(nextBtnW, True);
		
		break;
	
	    case 1:     /* next button */

/*
 * activate previous button
 */
        	if (!XtIsSensitive(prevBtnW) )
           	    XtSetSensitive(prevBtnW, True);
		
		break;
	}

	if  ( plotData.mode == WATCHBOX )  {
	    fosd_wbox ( 1 );
	}
	else {
/*
 * read/display the text 
 */
	    iret = fosd_txtrd( 1 );
	    if ( iret == -9 ) {
		wnxm_NxmWarn_show(wid, message);
/*
 * decode and plot data when necessary
 */
	    }
	    else if ( (iret == 0) || (iret == -3) ) {
		if ( ( plotData.mode == VALUE    ) ||
		     ( plotData.mode == GRAPHIC  ) ) {
		    fosd_decode();
		    fosd_plot( 1 );
		}
	    }
	}

        if ( (iret == -6) || (iret == -3) ) {

/*
 * no more previous/next report
 */
	   if ( which == -1 )
           	XtSetSensitive ( prevBtnW, False );
	   if ( which == 1 )
           	XtSetSensitive ( nextBtnW, False );

        }
}

/*=====================================================================*/
/* ARGSUSED */
void txtw_prntbCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * txtw_prntbCb                                                         *
 *                                                                      *
 * This routine is the callback for the print report button.            *
 *                                                                      *
 * txtw_prntbCb ( w, clnt, cbs )                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      clnt            XtPointer       The input data for the widget   *
 *      cbs		XtPointer       The output data for the widget  *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * S. Jacobs/NMC         9/94   Added print capability          	*
 * S. Jacobs/NMC         9/94   Updated to print large files    	*
 * C. Lin/EAI            9/95   Modified 				*
 * G. Krueger/EAI	 3/96	CFL_SWOP -> CFL_WOPN			*
 * S. Jacobs/NCEP	12/98	Changed fclose to cfl_clos		*
 * S. Jacobs/NCEP	12/98	Removed MTMACH check for print command	*
 * R. Tian/SAIC		 4/03	Modified to print printText only	*
 * R. Tian/SAIC		 6/03	Undo modification made in 4/03		*
 * S. Jacobs/NCEP	 9/05	Added leading blank line		*
 * E. Safford/SAIC	12/07	use pdata_getReportText() & txtw_prnt	*
 ***********************************************************************/
{
int 	ier;
char    report[ REPMAX ]; 

/*---------------------------------------------------------------------*/
    pdata_getReportText( report, &ier );
    txtw_prnt( report, &ier );

}

/*=====================================================================*/
/* ARGSUSED */
void txtw_modeCb ( Widget w, long which, XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * txtw_modeCb                                                          *
 *                                                                      *
 * This routine is the callback for the text mode radio buttons.        *
 *                                                                      *
 * txtw_modeCb ( w, which, cbs )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      which           long            The item number for the menu    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *cbs	        XmToggleButtonCallbackStruct    output data     *
 **                                                                     *
 * R. Tian/SAIC		04/03                                           *
 * R. Tian/SAIC		05/03	Disable prev/nxt for Append mode        *
 * R. Tian/SAIC		11/03	Disable auto-update for Append mode     *
 * E. Safford/SAIC	12/07	use txtw_setPrevNext()			*
 ***********************************************************************/
{
/*
 * Set the global variable for the text mode.
 *    0 = Replace Mode 
 *    1 = Append Mode
 */
    if (cbs->set) {
        usrSelect.modeflg = (textmode_t)which;

        if ( which == 1 ) {
	    txtw_setPrevNext( G_FALSE, G_FALSE );

/*
 * For Append Mode, disable auto-update.
 */
            XtSetSensitive(autoMenuBtnW, False);
	    auto_stopAutoUpdt();
        }
        else if ( which == 0 ) {
/*
 * For Replace Mode, resume auto-update.
 */
            XtSetSensitive(autoMenuBtnW, True);
	    auto_startAutoUpdt();
	}
    }
}

/*=====================================================================*/

void txtw_prdgrpSet ( void )
/************************************************************************
 * txtw_prdgrpSet                                                       *
 *                                                                      *
 * This function will display the group and product name on the text    *
 * window as follows:                                                   *
 *                                                                      *
 *                group name:  product name                             *
 *                                                                      *
 *                                                                      *
 * void txtw_prdgrpSet ()                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI      08/95                                           *
 * C. Lin/EAI		10/95						*
 ***********************************************************************/
{
XmString msg;
char     title[100];
char     *p;
/*---------------------------------------------------------------------*/

  	p = (usrSelect.group)->prod[usrSelect.prod].prdname;
	while ( *p ) {
        	if (*p == '*') *p = ' ';
		p++;
	}

  	sprintf(title, "%s:  %s", (usrSelect.group)->grpname, 
		(usrSelect.group)->prod[usrSelect.prod].prdname);

  	msg = XmStringCreateLocalized(title);
  	XtVaSetValues(prodtxtW,
                XmNlabelString, msg,
                NULL);
  	XmStringFree(msg);

}

/*=====================================================================*/

void txtw_setPrevNext( G_Boolean prevStatus, G_Boolean nextStatus )
/************************************************************************
 * txtw_setPrevNext                                                     *
 *                                                                      *
 * This function updates the status (sensitivity) of the previous and   *
 * next buttons.                                                        *
 *                                                                      *
 * void txtw_setPrevNext ( prevStatus, nextStatus )                     *
 *                                                                      *
 * Input parameters:							*
 *	prevStatus	G_Boolean	status of previous btn		*
 *	nextStatus	G_Boolean	status of next btn		*
 *                                                                      *
 * Output parameters:							*
 *			None						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    if( prevStatus == G_TRUE || prevStatus == G_FALSE ) {
        XtSetSensitive( prevBtnW, prevStatus );
    }
    if( nextStatus == G_TRUE || nextStatus == G_FALSE ) {
        XtSetSensitive( nextBtnW, nextStatus );
    }

}

/*=====================================================================*/

void txtw_dsplyReport( int *iret )
/************************************************************************
 * txtw_dsplyReport                                                     *
 *                                                                      *
 * This function displays the contents of the text report to the text   *
 * window.                                                              *
 *                                                                      *
 * void txtw_dsplyReport ( iret )             	                        *
 *                                                                      *
 * Input parameters:							*
 *			None						*
 * Output parameters:							*
 *	*iret		int	return code				*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    char	text[ REPMAX ];
    int		ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    pdata_getReportText( text, &ier );

    if( ier != G_NORMAL ) {
        *iret = -1;
    }
    else {
        XmTextSetString( textW, text );
    }
}

/*=====================================================================*/

void txtw_dsplyDateTime( int *iret  )          
/************************************************************************
 * txtw_clearDateTime                                                   *
 *                                                                      *
 * This function updates the date/time displayed in the dttmtxtW        *
 * Widget.                                                              *
 *                                                                      *
 * void txtw_dsplyDateTime ( iret )  	                                *
 *                                                                      *
 * Input parameters:							*
 * 	         	None                				*
 * Output parameters:							*
 *	*iret		int	return code				*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding                          *
 ***********************************************************************/
{
    char	date[ REPMAX ];
    int		ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    pdata_getDateTime( date, &ier );

    if( ier != G_NORMAL ) {
        *iret = -1;
    }
    else {
        XmTextFieldSetString( dttmtxtW, date );
    }
}

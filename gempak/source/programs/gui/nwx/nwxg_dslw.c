#include "nwx_cmn.h"
#include "nwx_gui.h"


/*
 * Private functions
 */
void dslw_closeCb ( Widget, XtPointer, XtPointer );
void dslw_dttmCb ( Widget, long which, XtPointer );
/*void dslw_dttmSensitive ( G_Boolean state ); */
void dslw_groupCb ( Widget, XtPointer, XtPointer );
void dslw_prodCb ( Widget, XtPointer, XtPointer );
void dslw_slctCb ( Widget, long which, XmToggleButtonCallbackStruct *cbs);

Widget 	    dataSelectW;	/* data selection window widget */

stnlist_t   stnList;		/* info of stations report the selected prods */

Widget      _prodlistW;      	/* product list widget */

WidgetList  _timeButtons; 	/* pre-defined map toggle buttons */

char	    *HourVals[] = { "1", "3", "6", "12", "24", "48"};
char	    *DayVals[]  = { "1", "2", "3",  "5", "30", "90"};

Widget	    _timeLabel, _dttmFrameW;

/************************************************************************
 * nwxg_dslw.c                                                          *
 *                                                                      *
 * This GUI module creates the data type selection popup window and     *
 * defines its callbacks.                                               *
 *                                                                      *
 * CONTENTS:                                                            *
 *      dslw_create()   creates the data type selection popup window.   *
 *      dslw_groupCb()  callback for selecting group in the group list. *
 *      dslw_prodCb()   callback for selecting product in the prod list.*
 *      dslw_dttmCb()   callback for the search time contraint. 	*
 *      dslw_closeCb()  callback for the close button on popup.         *
 *      dslw_dttmset()  function to change time selection labels	*
 ***********************************************************************/

/*=====================================================================*/

Widget dslw_create ( Widget w )
/************************************************************************
 * dslw_create                                                          *
 *                                                                      *
 * This function creates the data selection popup window.               *
 *                                                                      *
 * Widget dslw_create ( w )		                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w		Widget	The parent widget                    	*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *      dslw_create	Widget	The data selection popup  widget        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI       7/95   for nwx2.1                		*
 * C. Lin/EAI            8/95                   			*
 * D.W.Plummer/NCEP	 9/96	Commented out "Select by ..."; changed	*
 *				time selection to hours instead of days	*
 * D.W.Plummer/NCEP	11/96	removed commented section from 9/96	*
 *                              changed time selection from hours only	*
 *				to either hours or days depending on	*
 *				product type (eg., W or O, etc.)	*
 * G. Krueger/EAI	11/97	Renamed NxmHelp functions		*
 * I. Durham/GSC 	 5/98	changed call for underscore		*
 * T. Piper/GSC		 6/01	Freed xmstr				*
 * T. Piper/SAIC	 7/02	Reinstated the select by state option	*	
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * E. Safford/SAIC	12/07	renamed guidInit to idata_init()	*
 ***********************************************************************/
{
struct guid_grp *grp_ptr;
Widget		paned_w, topform; 
Widget		groups_frame, products_frame;
Widget		rc, form, form2, form3, form4, frame3;
Widget		radio_box1, close_button, help_button;
Widget   	label1, label2, label3, label4;
Widget		data_group_list;

XmString	station, state, *xmstr, msg;
Arg 		wargs[10];
Pixel		fg, bg;
int		jj, ier;
long		ii, nn;

/*---------------------------------------------------------------------*/

	nn = 0;
	XtSetArg(wargs[nn], XmNtitle, "Data Selection"); nn++;
        dataSelectW = XmCreateBulletinBoardDialog(w, "data_selection",
						   wargs, nn);

	paned_w = XtVaCreateManagedWidget("paned_w",
			xmPanedWindowWidgetClass, dataSelectW,
			XmNsashWidth,             1,
			XmNsashHeight,            1,
			NULL);

	XtVaGetValues(paned_w, 
		      XmNforeground, &fg, 
		      XmNbackground, &bg, 
		      NULL);

	topform = XtVaCreateManagedWidget("topform",
			xmFormWidgetClass, paned_w,
			NULL);
	
/*
 *	Create Group List
 */
	msg = XmStringCreateLocalized("Select Data Type Group");
	label1 = XtVaCreateManagedWidget("Group label",
			xmLabelWidgetClass, topform,
			XmNlabelString,     msg,
			XmNtopAttachment,   XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);
	XmStringFree(msg);

	groups_frame = XtVaCreateWidget("groups_frame",
			xmFrameWidgetClass, topform,
			XmNshadowType,      XmSHADOW_IN,
			XmNtopAttachment,   XmATTACH_WIDGET,
			XmNtopWidget,       label1, 
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);

	form = XtVaCreateWidget("form", xmFormWidgetClass, groups_frame, NULL);

	guid_ngrp = idata_init( &ier );
	if ( ier != G_NORMAL ) {
	    printf("NWX:  ERROR reading guidata.tbl.  See system administrator.\n");
	    exit(guid_ngrp);
	}

	grp_ptr=guid_grp_head;

	xmstr = (XmString *) XtMalloc(sizeof (XmString) * (size_t)guid_ngrp);
	for (ii = 0; ii < guid_ngrp; ii++) {
      		xmstr[ii] = XmStringCreateLocalized(grp_ptr->grpname);
      		grp_ptr = grp_ptr->nextgrp;
	}

	nn = 0;
	XtSetArg(wargs[nn], XmNitems,            xmstr);            nn++;
	XtSetArg(wargs[nn], XmNitemCount,        guid_ngrp);        nn++;
	XtSetArg(wargs[nn], XmNvisibleItemCount, 7);                nn++;
	XtSetArg(wargs[nn], XmNselectionPolicy,  XmSINGLE_SELECT);  nn++;
	XtSetArg(wargs[nn], XmNscrollingPolicy,  XmAUTOMATIC);      nn++;
	XtSetArg(wargs[nn], XmNtopAttachment,    XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNleftAttachment,   XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNrightAttachment,  XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNbottomAttachment, XmATTACH_FORM);    nn++;
	data_group_list = XmCreateScrolledList(form, "data_group", wargs, nn);

	XtManageChild(data_group_list);

	XtAddCallback(data_group_list, XmNsingleSelectionCallback,
			dslw_groupCb, _prodlistW);

	XtManageChild(form);
	XtManageChild(groups_frame);

	for (jj = 0; jj < guid_ngrp; jj++)
      		XmStringFree(xmstr[jj]);
	XtFree((XtPointer)xmstr);
	
/*
 *	Create Product List
 */
	msg = XmStringCreateLocalized("Select Data Type Product");
	label2 = XtVaCreateManagedWidget("product label1",
			xmLabelWidgetClass, topform,
			XmNlabelString,     msg,
			XmNtopAttachment,   XmATTACH_WIDGET,
			XmNtopWidget,       groups_frame,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);
	XmStringFree(msg);

	msg = XmStringCreateLocalized("  * Product Unavailable  ");
	label3 = XtVaCreateManagedWidget("product label2",
			xmLabelWidgetClass, topform,
			XmNlabelString,     msg,
			XmNtopAttachment,   XmATTACH_WIDGET,
			XmNtopWidget,       label2,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);
	XmStringFree(msg);

	products_frame = XtVaCreateWidget("products_frame", 
			xmFrameWidgetClass, topform,
			XmNshadowType,      XmSHADOW_IN,
			XmNtopAttachment,   XmATTACH_WIDGET,
			XmNtopWidget,       label3,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);

	form = XtVaCreateWidget ("form", xmFormWidgetClass, 
				  products_frame, NULL);

	 nn = 0;
	XtSetArg(wargs[nn], XmNvisibleItemCount, 7);                nn++;
	XtSetArg(wargs[nn], XmNwordWrap,         True);             nn++;
	XtSetArg(wargs[nn], XmNselectionPolicy,  XmSINGLE_SELECT);  nn++;
	XtSetArg(wargs[nn], XmNscrollingPolicy,  XmAUTOMATIC);      nn++;
	XtSetArg(wargs[nn], XmNtopAttachment,    XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNleftAttachment,   XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNrightAttachment,  XmATTACH_FORM);    nn++;
	XtSetArg(wargs[nn], XmNbottomAttachment, XmATTACH_FORM);    nn++;
	_prodlistW = XmCreateScrolledList(form, "data_type", wargs, nn);

	XtManageChild(_prodlistW);

	XtAddCallback(_prodlistW, XmNsingleSelectionCallback,
			 dslw_prodCb, NULL);

	XtManageChild(form);
	XtManageChild(products_frame);

/*
 *  Bottom Pane
 */
	form = XtVaCreateManagedWidget("form",
			xmFormWidgetClass, paned_w,
			XmNfractionBase, 1,
			NULL);

/*
 *  "Time Covered"
 */
	_dttmFrameW = XtVaCreateManagedWidget("_dttmFrameW",
			xmFrameWidgetClass,  form,
			XmNshadowType,       XmSHADOW_IN,
			XmNtopAttachment,    XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment,   XmATTACH_POSITION,
			XmNleftPosition,     0,
			XmNrightAttachment,  XmATTACH_POSITION,
			XmNrightPosition,    1,
			NULL);

	form2 = XtVaCreateManagedWidget("form2", xmFormWidgetClass, 
						_dttmFrameW, NULL);

        pdata_setTimeCovered( EVENT, &ier );
/*	timeCoveredType = EVENT; */
	msg = XmStringCreateLocalized("Time Covered (Hours)");
	_timeLabel = XtVaCreateManagedWidget("Time",
			xmLabelWidgetClass, form2,
			XmNlabelString,     msg,
			XmNtopAttachment,   XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);
	XmStringFree(msg);

	rc = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass, form2,
                        XmNorientation,         XmHORIZONTAL,
                        XmNradioBehavior,       True,
                        XmNtraversalOn,         False,
                        XmNtopAttachment,       XmATTACH_WIDGET,
                        XmNtopWidget,           _timeLabel,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL);

	nn = (long)XtNumber ( HourVals );
	_timeButtons = (WidgetList)XtMalloc(nn * sizeof(Widget));

	for ( ii = 0 ; ii < nn ; ii++ ) {
                _timeButtons[ii] =
                        XtVaCreateManagedWidget( HourVals[ii],
                            xmToggleButtonWidgetClass, rc,
                            NULL );
                XtAddCallback(_timeButtons[ii], XmNarmCallback,
                        (XtCallbackProc)dslw_dttmCb,
                        (XtPointer)ii);
        }

	XmToggleButtonSetState( _timeButtons[0], True, False );
	XtManageChild(rc);
	dslw_dttmSensitive ( False );

/*
 *  Select By
 */
	form = XtVaCreateManagedWidget("form",
                        xmFormWidgetClass, paned_w,
                        XmNfractionBase, 1,
                        NULL);

	frame3 = XtVaCreateManagedWidget("frame3",
                        xmFrameWidgetClass,  form,
                        XmNshadowType,       XmSHADOW_IN,
                        XmNtopAttachment,    XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment,   XmATTACH_POSITION,
                        XmNleftPosition,     0,
                        XmNrightAttachment,  XmATTACH_POSITION,
                        XmNrightPosition,    1,
                        NULL);


	form4 = XtVaCreateManagedWidget("form", xmFormWidgetClass, frame3, NULL);

	msg = XmStringCreateLocalized("Select By");
        label4 = XtVaCreateManagedWidget("select_by",
                        xmLabelWidgetClass, form4,
                        XmNlabelString,     msg,
                        XmNtopAttachment,   XmATTACH_FORM,
                        XmNleftAttachment,  XmATTACH_FORM,
                        NULL);
        XmStringFree(msg);

        station = XmStringCreateLocalized("station");
        state = XmStringCreateLocalized("state");
        radio_box1 = XmVaCreateSimpleRadioBox(form4,
                        "radio_box1", 0, (XtCallbackProc)dslw_slctCb,
                        XmVaRADIOBUTTON,     station, NULL, NULL, NULL,
                        XmVaRADIOBUTTON,     state, NULL, NULL, NULL,
			XmNnumColumns,	     2,
			XmNspacing,	     55,
                        XmNradioBehavior,    TRUE,
                        XmNtopAttachment,    XmATTACH_WIDGET,
                        XmNtopWidget,        label4,
                        XmNleftAttachment,   XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL);
        XtManageChild(radio_box1);

        XmStringFree(station);
        XmStringFree(state);

/*
 *  CLOSE, and HELP
 */
	form3 = XtVaCreateManagedWidget("form3", 
			xmFormWidgetClass, paned_w,
			XmNfractionBase, 2,
			NULL);

	close_button = XtVaCreateManagedWidget("CLOSE",
			xmPushButtonGadgetClass, form3,
			XmNshadowType,           XmSHADOW_IN,
			XmNtopAttachment,        XmATTACH_FORM,
			XmNbottomAttachment,     XmATTACH_FORM,
			XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         0,
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        1,
			NULL);

	XtAddCallback ( close_button, XmNactivateCallback, 
		        (XtCallbackProc)dslw_closeCb, NULL);

	help_button = XtVaCreateManagedWidget("HELP",
			xmPushButtonGadgetClass, form3,
			XmNshadowType,           XmSHADOW_IN,
			XmNtopAttachment,        XmATTACH_FORM,
			XmNbottomAttachment,     XmATTACH_FORM,
			XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         1,
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        2,
			NULL);

	XtAddCallback ( help_button, XmNactivateCallback, 
			(XtCallbackProc)NxmHelp_helpBtnCb,
			(XtPointer)2);

	return(dataSelectW);

}

/*=====================================================================*/
/* ARGSUSED */
void dslw_prodCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * dslw_prodCb                                                          *
 *                                                                      *
 * This routine is the callback for the data type menu.                 *
 *                                                                      *
 * dslw_prodCb ( wid, clnt, cbs )                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          The input widget                *
 *      clnt		XtPointer       The input data for the widget   *
 *      cbs		XtPointer       The input data for the callback *
 *                                                                      *
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * L. Williams/EAI      12/94   Add code to process UVI info    	*
 * L. Williams/EAI      06/95   Add code to display available   	*
 *                              products                        	*
 * L. Williams/EAI      08/95   Set previous and next buttons   	*
 *                              sensitive                       	*
 * C. Lin/EAI		09/95   modified from nwx2.1			*
 * L. Williams/EAI	05/96	Add call to mapw_rmselstn		*
 * D.W.Plummer/NCEP	 9/96	Changed time selection to hours		*
 * D.W.Plummer/NCEP	11/96	Only processes selection, does not apply*
 * T. Piper/SAIC	 6/02	Fixed ABR; added check on srchInfo.idtyp*
 * D. Kidwell/NCEP	 4/03	Desensitize time covered for decoded TAF*
 * R. Tian/SAIC		 7/03	Added mapw_rmappstn()			*
 * R. Tian/SAIC		11/03	Made autoMenuBtnW sensitive		*
 * R. Tian/SAIC		12/03	Added call to mapw_rmselstn()		*
 * E. Safford/SAIC	12/07	move half of code to dslw_srchMaster()	*
 ***********************************************************************/
{
    int  nselect, item, *position_list, position_count, ier;
/*---------------------------------------------------------------------*/

/*
 * Make the auto-update menu sensitive.
 */
	XtSetSensitive ( autoMenuBtnW, True );

/*
 * remove any previously appeneded stations.
 */
	mapw_rmappstn();

/*
 * Remove any previously selected stations
 */
	mapw_rmselstn();

/*
 * return when no selection was made
 */
        XtVaGetValues(_prodlistW,
                        XmNselectedItemCount, &nselect,
                        NULL);
        if ( nselect == 0 )
                return;

/*
 * get the product position on the list
 */
        XmListGetSelectedPos(wid, &position_list, &position_count);
        item = position_list[0] - 1;
	usrSelect.prod = item;
        XtFree((XtPointer)position_list);

	
/*
 * If product is available and index is unknown
 * search master table for data type and set index.
 */
        dslw_srchMaster( item, &ier );

}

/*=====================================================================*/

/* ARGSUSED */
void dslw_dttmCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_dttmCb								*
 *									*
 * This routine is the callback for the date/time menu. 		*
 *									*
 * dslw_dttmCb ( wid, which, cb_data )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		The input widget		*
 *	which		long		The which button		*
 *									*
 * Output parameters:							*
 *	cbs		XtPointer	The output data 		*
 **									*
 * S. Jacobs/NMC	 8/94						*
 * D.W.Plummer/NCEP	 9/96	changed time selection to hours 	*
 * D.W.Plummer/NCEP	11/96	change for processing EVENT / SCHEDULED *
 ***********************************************************************/
{
    int	timeCovered;
/*---------------------------------------------------------------------*/
/*
 * Set the global variable for the date/time.
 */

        timeCovered = pdata_getTimeCovered( );

	if ( timeCovered == EVENT ) {
		usrSelect.ddttm = atoi ( HourVals[which] );
	}
	else if ( timeCovered == SCHEDULED ) {
		usrSelect.ddttm = atoi ( DayVals[which] ) * 24;
	}

	dslw_apply ( );
}

/*=====================================================================*/
/* ARGSUSED */
void dslw_groupCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * dslw_groupCb 							*
 *									*
 * This functions display the group products. It is the callback for	*
 * group list.								*
 *									*
 * void dslw_groupCb(w, clnt, call) 					*
 *									*
 * Input parameters:							*
 *  w			Widget						*
 *  clnt 		XtPointer					*
 *  call		XtPointer					*
 *									*
 **									*
 * Log: 								*
 * L. Williams/EAI     06/95						*
 * C. Lin/EAI	       10/95  add comments				*
 * J. Wu/GSC	       05/01  free XmStrings				*
 * T. Piper/GSC		6/01	Freed xmstr				*
 * T. Piper/SAIC	10/06	Replaced XmStringGetLtoR with		*
 *					 XmStringUnparse		*
 ***********************************************************************/
{
struct guid_grp *group_ptr;
XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;

XmString  *xmstr;
char	  str[100];
int	  ii;
int	  prdcnt, nselect;
char	  *selected_item;

/*---------------------------------------------------------------------*/
/*
 * return when no selection was made
 */
	XtVaGetValues(w,
		XmNselectedItemCount, &nselect,
		NULL);
	if ( nselect == 0 )
		return;

	XmListDeselectAllItems(_prodlistW);

/*
 * get the selected group name
 */
	selected_item = XmStringUnparse (cbs->item, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

/*
 * Search the link list for the selected item
 */
	group_ptr = guid_grp_head;
	while ( group_ptr ) {
	    if (strcmp(group_ptr->grpname, selected_item) == 0) {
		usrSelect.group = group_ptr;
		break;
	    }
	    else
		group_ptr = group_ptr->nextgrp;
	}

/*
 * If products exist move product name into array.
 */
	if (group_ptr->nprod) {
	  xmstr = (XmString *) XtMalloc(sizeof(XmString) * (size_t)group_ptr->nprod);
	  for (ii=0; ii<group_ptr->nprod; ii++) {
	    if ( group_ptr->prod[ii].index == -2 ) {

/*
 * the selected product is not available
 */
		sprintf(str, "%s *", group_ptr->prod[ii].prdname);
		xmstr[ii] = XmStringCreateLocalized(str);
	    }
	    else
		xmstr[ii] = XmStringCreateLocalized(group_ptr->prod[ii].prdname);
	  }
	}
	else {
/*
 * No products exist.
 */
	  xmstr = (XmString *) XtMalloc (sizeof(XmString));
	  xmstr[0] = XmStringCreateLocalized("No products available");
	}

	XtFree( selected_item );

/*
 * display "No products available"
 */
	prdcnt=group_ptr->nprod;
	if (!prdcnt) ++prdcnt;

	XtVaSetValues(_prodlistW,
		XmNitems, xmstr,
		XmNitemCount, prdcnt,
		NULL);
	
	for (ii=0; ii<prdcnt; ii++) {
            XmStringFree( xmstr[ii] );
	}
	XtFree((XtPointer)xmstr);	
}

/*=====================================================================*/
/* ARGSUSED */
void dslw_closeCb ( Widget w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * dslw_closeCb 							*
 *									*
 * This function destroys the data selection window.			*
 *									*
 * void dslw_closeCb(w, clnt, call) 					*
 *									*
 * Input parameters:							*
 *	w		Widget						*
 *	clnt		XtPointer					*
 *	call		XtPointer					*
 *									*
 **									*
 * Log: 								*
 * L. Williams		06/95						*
 ***********************************************************************/
{
    XtUnmanageChild(dataSelectW);
}

/*=====================================================================*/

void dslw_toggle ( void )
/************************************************************************
 * dslw_toggle								*
 *									*
 * This function toggles the data selection popup window.		*
 *									*
 * void dslw_toggle()							*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI		07/95						*
 ***********************************************************************/
{
    if ( XtIsManaged(dataSelectW) )
	XtUnmanageChild(dataSelectW);
    else
	XtManageChild(dataSelectW);
}

/*=====================================================================*/

void dslw_dttmSensitive ( G_Boolean state )
/************************************************************************
 * dslw_dttmSensitive							*
 *									*
 * This function toggles the time selection area sensitive/insensitive	*
 *									*
 * void dslw_dttmSensitive ( state )					*
 *									*
 * Input parameters:							*
 *	state		Boolean 	toggle state			*
 *									*
 **									*
 * Log: 								*
 * D.W.Plummer/NCEP	11/96						*
 * E. Safford/SAIC	12/07	change state type to G_Boolean	 	*
 ***********************************************************************/
{
    XtSetSensitive( _dttmFrameW, (int)state );
}

/*=====================================================================*/
/* ARGSUSED */
void dslw_slctCb ( Widget wid, long which, XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * dslw_slctCb                                                          *
 *                                                                      *
 * This routine is the callback for the selection type menu.            *
 *                                                                      *
 * dslw_slctCb ( wid, which, cb_data )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          The input widget                *
 *      which           long            The item number for the menu    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *cbs		XmToggleButtonCallbackStruct	output data	*
 **                                                                     *
 * S. Jacobs/NMC         8/94                                           *
 * R. Tian/SAIC		 5/03	Disable prev/nxt for Station type	*
 ***********************************************************************/
{
/*
 * Set the global variable for the selection type.
 *    0 = Select by Station
 *    1 = Select by State
 */
    if (cbs->set) {
    	usrSelect.selct_by = (selectby_t)which;
	   
        if ( which == 1 ) {
	    txtw_setPrevNext( G_FALSE, G_FALSE );
	}
    }
}

/*=====================================================================*/

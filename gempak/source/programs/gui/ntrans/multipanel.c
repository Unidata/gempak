#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "mppatterns.h"
#include "Nxm.h"
#include "model.h"
#define PANELSTR
#include "panelstr.h"

int		model_vldTime;
int		file_vldTime;
extern Widget	group_listW, model_toplevel;

int           ValidTimeFrame[500];
Widget ValidTimeInfoW;
Widget loadPanelLb;

/*
 * Private functions
 */
void mpPattern_Callback ( Widget, long, XtPointer );
void setMpanelRc	( Widget, long, XtPointer );
void setValidTm		( Widget, long, XtPointer );
void mpanel_Callback	( Widget, XtPointer, XtPointer );
void create_selectMpanel ( void );
void selectMpanel_Callback ( Widget, long, XEvent* );


/************************************************************************
* MULTI_PANEL		                                              	*
*                                                                  	*
*   Module to take care of creating multi_panel selection panel.   	*
*                                                                  	*
*   Log:                                                           	*
*   Chien Lin/EAI       1/94 			                   	*
*   S. Wang/GSC 	1/97	clean up			   	*
*   S. Wang/GSC 	4/97	change pxm button call parameter   	*
*   G. Krueger/EAI	9/97	Changed NxmWarning -> NxmWarn_show   	*
*   I. Durham/GSC	5/98    changed call for underscore	   	*
*   J. Wug/GSC 		5/01	free XmStrings			   	*
*   R. Tian/SAIC       01/03	add True flag to NxmBxmBtn_create(Multi)*
*   C. Bailey/HPC	4/05	Add PanelSrc struct settings and	*
				 CurrentPanel variable			*
************************************************************************/

void create_multipanel ( Widget parent )
/************************************************************************
 * create_multipanel							*
 *									*
 *	Create multi-panel.						*
 *									*
 * void create_multipanel (  parent )					*
 *									*
 * Input parameters:							*
 * parent	Widget		Parent Widget ID			*
 **									*
 ************************************************************************/
{
Widget 		button, bb, rc;
long		ii;
char    	text[2];
char            *vtString[] = { "  set", "clear" };
XmString	xmstr;

/*----------------------------------------------------------------------*/

        MpanelBb = XtVaCreateManagedWidget ("mpanelBb",
			xmPanedWindowWidgetClass, parent,
       		        XmNsashWidth,  1,
               		XmNsashHeight, 1,
                	NULL );

        bb = XtVaCreateManagedWidget ("bb1",
			xmBulletinBoardWidgetClass, MpanelBb,
                	NULL );
	
	xmstr = XmStringCreateLocalized("Valid Time");
        XtVaCreateManagedWidget ("validtimeLb",
                xmLabelWidgetClass, bb,
        		XmNlabelString, xmstr,
                NULL );
	XmStringFree( xmstr );
	
        rc = XtVaCreateManagedWidget ("validtimeRc",
                xmRowColumnWidgetClass, bb,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmHORIZONTAL,
                XmNnumColumns,          1,
                NULL );

        for ( ii = 0; ii < (long)XtNumber(vtString); ii++ ) {
            button = XtVaCreateManagedWidget(vtString[ii],
                xmPushButtonGadgetClass, rc,
                NULL);

            XtAddCallback(button, XmNarmCallback,
                (XtCallbackProc)setValidTm, (XtPointer)ii);

        }

	xmstr = XmStringCreateLocalized("       valid time not set          ");
        ValidTimeInfoW = XtVaCreateManagedWidget ("validtimeInfo",
                xmLabelWidgetClass, bb,
       		XmNlabelString, xmstr,
                NULL );
	XmStringFree( xmstr );

        bb = XtVaCreateManagedWidget ("bb2",
		xmBulletinBoardWidgetClass, MpanelBb,
                NULL );

	xmstr = XmStringCreateLocalized("Panel structure");
        XtVaCreateManagedWidget ("structureLb",
                xmLabelWidgetClass, bb,
       		XmNlabelString, xmstr,
                NULL );
	XmStringFree( xmstr );
	
        rc = XtVaCreateManagedWidget ("patternsRc",
                xmRowColumnWidgetClass, bb,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmHORIZONTAL,
                XmNnumColumns,          1,
		XmNspacing,		10,
		XmNx,			80,
                NULL );

	NxmBxmBtn_create(rc, "mp_patterns", NULL,
        	16, 16, "black", "yellow",
        	NULL, (char*)p1x1_bits, NULL, True,
        	(XtCallbackProc)mpPattern_Callback, 0 );

	NxmBxmBtn_create(rc, "mp_patterns", NULL,
        	16, 16, "black", "yellow",
        	NULL, (char*)p1x2_bits, NULL, True,
        	(XtCallbackProc)mpPattern_Callback, (XtPointer)1 );

	NxmBxmBtn_create(rc, "mp_patterns", NULL,
        	16, 16, "black", "yellow",
        	NULL, (char*)p2x1_bits, NULL, True, 
        	(XtCallbackProc)mpPattern_Callback, (XtPointer)2 );

	NxmBxmBtn_create(rc, "mp_patterns", NULL,
        	16, 16, "black", "yellow",
        	NULL, (char*)p2x2_bits, NULL, True,
        	(XtCallbackProc)mpPattern_Callback, (XtPointer)3 );

	xmstr = XmStringCreateLocalized("Rows:");
        XtVaCreateManagedWidget ("rowLb",
                xmLabelWidgetClass, bb,
                XmNlabelString, xmstr,
                NULL );
	XmStringFree( xmstr );

	rc = XtVaCreateManagedWidget ("rowRc",
                xmRowColumnWidgetClass, bb,
                XmNtopAttachment, XmATTACH_POSITION,
                XmNleftAttachment, XmATTACH_POSITION,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmHORIZONTAL,
                XmNnumColumns, 		1,
                NULL );

        for ( ii = 2; ii < 6; ii++ ) {
            sprintf(text, "%d", (int)ii+1);
            button = XtVaCreateManagedWidget(text,
                xmPushButtonGadgetClass, rc,
		XmNuserData, ii,
                NULL);

            XtAddCallback(button, XmNactivateCallback,
                (XtCallbackProc)setMpanelRc, (XtPointer)0);

        }


        rowText = XtVaCreateManagedWidget ("rowTxt",
                xmTextFieldWidgetClass, bb,
                XmNvalue, "1",
                XmNcolumns, 1,
                NULL );

        XtAddCallback(rowText, XmNactivateCallback,
                      (XtCallbackProc)mpanel_Callback, NULL);

	xmstr = XmStringCreateLocalized("Columns:");
        XtVaCreateManagedWidget ("colLb",
                xmLabelWidgetClass, bb,
                XmNlabelString, xmstr,
                NULL );
	XmStringFree( xmstr );

        rc = XtVaCreateManagedWidget ("colRc",
                xmRowColumnWidgetClass, bb,
                XmNtopAttachment, XmATTACH_POSITION,
                XmNleftAttachment, XmATTACH_POSITION,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmHORIZONTAL,
                XmNnumColumns,          1,
                NULL );

        for ( ii = 2; ii < 6; ii++ ) {
            sprintf(text, "%d", (int)ii+1);
            button = XtVaCreateManagedWidget(text,
                xmPushButtonGadgetClass, rc,
		XmNuserData, ii,
                NULL);

            XtAddCallback(button, XmNactivateCallback,
                (XtCallbackProc)setMpanelRc, (XtPointer)1);

        }

        colText = XtVaCreateManagedWidget ("columnTxt",
                xmTextFieldWidgetClass, bb,
                XmNvalue,   		"1",
                XmNcolumns, 		1,
                NULL );

        XtAddCallback(colText, XmNactivateCallback,
                (XtCallbackProc)mpanel_Callback, NULL);

        MpanelSelFm = XtVaCreateManagedWidget ("form",
		xmFormWidgetClass, MpanelBb,
                NULL );

	xmstr = XmStringCreateLocalized("Load panel");
        loadPanelLb = XtVaCreateManagedWidget ("mpanelSelLb",
                xmLabelWidgetClass, MpanelSelFm,
        	XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
                NULL );
	XmStringFree( xmstr );

	create_selectMpanel();

}

/*========================================================================*/
/* ARGSUSED */
void mpPattern_Callback ( Widget w, long which, XtPointer call )
/************************************************************************
 * mpPattern_Callback							*
 *									*
 *	mpPattern Callback.						*
 *					`				*
 * void mpPattern_Callback( w, which, call )				*
 *									*
 * Input parameters:							*
 * w		Widget							*
 * which	long							*
 * call		XtPointer						*
 **									*
 ***********************************************************************/
 {
char text[2];

/*----------------------------------------------------------------------*/

    switch ( which ) {

    case 0:
	Mpanel.rows    = 1; 
	Mpanel.columns = 1; 
	break;

    case 1:
	Mpanel.rows    = 1; 
	Mpanel.columns = 2; 
	break;

    case 2:
	Mpanel.rows    = 2; 
	Mpanel.columns = 1; 
	break;

    case 3:
	Mpanel.rows    = 2; 
	Mpanel.columns = 2; 
	break;

    case 4:
	Mpanel.rows    = 2; 
	Mpanel.columns = 3; 
	break;

    case 5:
	Mpanel.rows    = 3; 
	Mpanel.columns = 2; 
	break;
    }

    sprintf(text, "%d", Mpanel.rows);
    XtVaSetValues(rowText, XmNvalue, text, NULL);
    sprintf(text, "%d", Mpanel.columns);
    XtVaSetValues(colText, XmNvalue, text, NULL);
    mpanel_Callback(w,NULL,NULL);
}

/*========================================================================*/
/* ARGSUSED */
void setMpanelRc ( Widget w, long which, XtPointer call )
{
long	number;
char    text[2];

/*----------------------------------------------------------------------*/

	XtVaGetValues(w, XmNuserData, &number, NULL);
	sprintf(text, "%d", (int) (number+1));

	if (which == 0)
	    XtVaSetValues(rowText, XmNvalue, text, NULL);
	else 
	    XtVaSetValues(colText, XmNvalue, text, NULL);

	mpanel_Callback(w,NULL,NULL);
}

/*=======================================================================*/
/* ARGSUSED */
void setValidTm ( Widget w, long which, XtPointer call )
/************************************************************************
 * setValidTm								*
 *									*
 *	Set valid time.							*
 *									*
 * void setValidTm(w, which, call)					*
 *									*
 * Input parameters:							*
 * w		Widget							*
 * which	int							*
 * call		XtPointer						*
 *									*
 ** Log:								*
 * C. Bailey/HPC	5/05		Moved valid time options code to*
					setValidTime & clearValidTime	*
 ************************************************************************/
 {
	int		selct;

/*----------------------------------------------------------------------*/

	switch ( which ) {
            case 0: /* set valid time */

                XtVaGetValues(group_listW,
                        XmNselectedItemCount, &selct,
                        NULL);

                if ( selct == 0 ) {

                    NxmWarn_show( DrawingW,
                                      " Please Select a Group. ");
                }
                else {

		    setValidTime();

		}
                break;

            case 1: /* clear valid time */

		clearValidTime();
                break;
        }
}

/*======================================================================*/

void clearValidTime ( void )
/************************************************************************
 * clearValidTime							*
 *									*
 *	Clear valid time.						*
 *									*
 ** Log:								*
 * C. Bailey/HPC	5/05		Separated clear valid time code *
 *					from setValidTm			*
 ************************************************************************/
 {
XmString        xmstr;

/*---------------------------------------------------------------------*/

	ValidTimeSet = 0;
        xmstr = XmStringCreateLocalized("    valid time not set"),
        XtVaSetValues( ValidTimeInfoW,
                       XmNlabelString, xmstr,
                       NULL);
        XmStringFree(xmstr);

}
/*======================================================================*/

void setValidTime ( void )
/************************************************************************
 * setValidTime								*
 *									*
 *	Set valid time.							*
 *									*
 ** Log:								*
 * C. Bailey/HPC	5/05		Separated Code From setValidTm	*
 ************************************************************************/
 {
int             ii, fm, increase;
char            name[50], date1[3], date2[3];
XmString	xmstr;

/*------------------------------------------------------------------------*/

	model_vldTime = SelectModelNo;
	file_vldTime = SelectFileNo;
         
	for ( ii = 0; ii < GroupList[SelectGroupNo-1].frame_num; 
		ii++) {
			
	    fm = GroupList[SelectGroupNo-1].frames[ii] - 1;
	    strncpy(ValidTimeString[ii], meta_st.frame[fm].label, 5);
	    ValidTimeString[ii][5] = '\0';
/*
 * add frame number to Array
 */
	    ValidTimeFrame[ii] = fm;
        }
		
	ValidTimeSet = (char)GroupList[SelectGroupNo-1].frame_num;

/**************************** NOTE: *************************************
	The index number I've used for different arrays really stings. 
	SelectGroupNo starts from 1.
	GroupList internal index starts from 0. 
	GroupList.frames starts from 1, 
	but meta_st.frame starts from 0. Have to be very careful about this.
************************************************************************/

	if ( ValidTimeSet ) {

	    strncpy(date1, ValidTimeString[0], 2);
	    date1[2] = '\0';
	    strncpy(date2, ValidTimeString[1], 2);
	    date2[2] = '\0';
	    increase = (atoi(date2) - atoi(date1))*24 +
	    atoi( &(ValidTimeString[1][3])) -
		atoi( &(ValidTimeString[0][3]));
			
	    sprintf(name, "VT:(%sZ)->(%sZ),I-%dHr",
		    ValidTimeString[0], 
		    ValidTimeString[ValidTimeSet - 1],
		    increase);
	    xmstr = XmStringCreateLocalized(name);
	        	XtVaSetValues( ValidTimeInfoW, 
			XmNlabelString, xmstr,
		    	NULL);
	    XmStringFree(xmstr);
	}
}

/*========================================================================*/
/* ARGSUSED */
void mpanel_Callback ( Widget w, XtPointer clnt, XtPointer call )
{
char text[3], *tmp;
int	rows, cols, iret;

/*----------------------------------------------------------------------*/

	strcpy (text,  tmp = XmTextGetString(rowText));
	XtFree(tmp);
	cst_numb(text, &rows, &iret);
	if ( iret == 0 )
	    Mpanel.rows = rows;
	else {
	    Mpanel.rows = 1;
	    XtVaSetValues(rowText, XmNvalue, "1", NULL);
	}

	strcpy (text, tmp = XmTextGetString(colText));
	XtFree(tmp);
	cst_numb(text, &cols, &iret);
	if ( iret == 0 )
                Mpanel.columns = cols;
	else {
                Mpanel.columns = 1;
	        XtVaSetValues(colText, XmNvalue, "1", NULL);
	}
	create_selectMpanel();
}

/*=====================================================================*/

void create_selectMpanel ( void )
 /***********************************************************************
 * create_selectMpanel                                                  *
 *                                                                      *
 * This function creates the multipanel selection buttons.		*
 *                                                                      *
 * void create_selectMpanel()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 ** Log:                                                                *
 *      L. Chien/EAI							*
 *      S. Wang/GSC             01/97     add more functionalities      *
 *      A. Hardy/GSC            03/99     changed first to static int   *
 *      C. Bailey/HPC		05/05	  add PanelSrc and moved	*
 *					     multipanel button code to	*
 *						createMpFrame() 	*
 * S. Jacobs/NCEP	 5/13	Increased the valid time array from 	*
 * 				50 to 500				*
 ***********************************************************************/
{
int    ii, jj, iret;
static int first = 1;
/*--------------------------------------------------------------------*/
/*
 * clear all flags in prt_multiPanel_info[][]
 */
	for ( ii = 0; ii < MAXPANEL; ii++ ) 
	    for ( jj = 0; jj < MAXPANEL; jj++ ) 
		prt_multiPanel_info[ii][jj].flag = 0;

/*
 * initialize panelSrc flags
 */
        for (ii = 0; ii < 36; ii++ ) {
            panelSrc.flag[ii] = 0;
	    panelSrc.valid_time[ii].valid_tm_set = 0;
	    for (jj = 0; jj < 500; jj++ ) {
		panelSrc.valid_time[ii].valid_tm_frames[jj] = -1;
	    }
	}

	if ( first ) {
		gclear(&iret);
		PixmapData.old_pixmap_no = 1;
		first = 0;
	}
	else
		ClearAreas(NULL, NULL, NULL);

        createMpFrame();
}

/*======================================================================*/

void createMpFrame( void )
/************************************************************************
 * createMpFrame 							*
 *									*
 ** Log:								*
 * C. Bailey		5/05		Separate Code from 		*
 *					  create_selectMpanel		*
 ************************************************************************/
{	
	int    nn;
        long   ii;
	Widget button;
	char  text[3];

/*-----------------------------------------------------------------------*/
	XtUnmanageChild( MultipanelFrame );
        XtUnmanageChild( MpanelSelFm );

	if ( ( Mpanel.rows > 6 ) | (Mpanel.rows < 1) ) {
	    Mpanel.rows = 1;
	    XtVaSetValues(rowText, XmNvalue, "1", NULL);
	}

        if ( ( Mpanel.columns > 6 ) | (Mpanel.columns < 1 ) ) {
                Mpanel.columns = 1;
                XtVaSetValues(colText, XmNvalue, "1", NULL);
	}

	if (MpanelRc)
		XtDestroyWidget(MpanelRc);

        MpanelRc = XtVaCreateManagedWidget ("mpanelRc",
                xmRowColumnWidgetClass, MpanelSelFm,
                XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget ,    loadPanelLb,
                XmNleftAttachment, XmATTACH_FORM,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmHORIZONTAL,
                NULL );

	XtVaSetValues(MpanelRc, 
		XmNnumColumns, Mpanel.rows,
		NULL);
 
	nn = (Mpanel.rows)*(Mpanel.columns);

/*
 * set number of panels
 */
        panelSrc.panelNo = nn;
	for( ii = 0; ii < nn; ii++ ) {
	    sprintf(text, "%2d", (int)ii+1);
            button = XtVaCreateManagedWidget(text,
                xmPushButtonWidgetClass, MpanelRc,
                NULL);

            XtAddEventHandler(button,
                ButtonReleaseMask, FALSE ,
                (XtEventHandler)selectMpanel_Callback, (XtPointer)ii);
        }

        if ( ( Mpanel.rows > 1 ) | (Mpanel.columns > 1) ) {
            MpanelMode = 1;
	    NxmColorbarReset(1);
	}
        else {
            MpanelMode = 0;
	    NxmColorbarReset(0);
	}

        if ( !MpanelMode ) 
            setValidTm(NULL, 1, NULL);

	XtManageChild( MpanelSelFm );
	XtManageChild( MultipanelFrame );
}

/*========================================================================*/
/* ARGSUSED */
void selectMpanel_Callback ( Widget w, long which, XEvent *call )
{

/*--------------------------------------------------------------------------*/

        switch ( call->xbutton.button ) {
		
	  case Button1:
		Mpanel.selectCol = which%(Mpanel.columns); 
		Mpanel.selectRow = which/(Mpanel.columns);
		
		/*
		 * Set Panel Source variables
		 */

                setPanelSrc(which);

                CurrentPanel = which;
		ok_select(DrawingW);

		break;

	  case Button3:

		Mpanel.selectCol = which%(Mpanel.columns); 
		Mpanel.selectRow = which/(Mpanel.columns);
		
		setPanelSrc(which);

                XtUnmanageChild( group_select_toplevel );
                XtUnmanageChild( model_toplevel );

		CurrentPanel = which;
		ok_select(DrawingW);

		break;

	}
}

/*-------------------------------------------------------------------*/

void setPanelSrc (int which)

/************************************************************************
 * setPanelSrc (which)							*
 *									*
 * Function to Set Panel Source information				*
 *									*
 ** Log:								*
 *	C. Bailey/HPC 	8/05						*
 ***********************************************************************/
{
	int     ii;

/*----------------------------------------------------------------------*/
/*
 * Set Panel Source variables
 */
        panelSrc.flag[which] = 1;
        panelSrc.row[which] = Mpanel.selectRow;
        panelSrc.column[which] = Mpanel.selectCol;
        panelSrc.model_no[which] = SelectModelNo;
        panelSrc.file_no[which] = SelectFileNo;

/*
 * Check Model Valid Time is Set 
 */
        if(model_vldTime == SelectModelNo && file_vldTime == SelectFileNo) {
            panelSrc.valid_time[which].valid_tm_set = ValidTimeSet;
            for (ii = 0; ii < ValidTimeSet; ii++) {
                panelSrc.valid_time[which].valid_tm_frames[ii] = ValidTimeFrame[ii];
            }
         }
}

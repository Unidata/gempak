#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

#define	  FORECASTERS_TBL	"forecasters.tbl"
#define   TIMERANGE_TBL		"outlooktimes.tbl"


#define	NINFO	 1	/* Number of categories in the table    	*/

#define FCST	 0	/* Forecaster's name				*/


#define	MXELE	50	/* Max elements per category			*/
#define MXRANGE 10      /* Max number of clock time ranges allowed      */


#define LSTR	80	/* Size of a long string */

typedef struct of_info_t {
    char	range[MXELE][MXCHR];	/* Element range		*/
    int		nelems;			/* Number of elements in range	*/
} Of_Info_t;

typedef struct time_info {
    int 	time_range[2];	        /* Clock time range             */
    int		initial_time[2];	/* Initial time	                */
    int         expiration_time[2];     /* Expiration time              */
} TIME_Info;

typedef struct bulk_process {
    Boolean 	process_is_on;	        /* Whether bulk process is on   */
    int		start_layer;	        /* Starting layer	        */
    int         curr_layer;		/* Current layer                */
    int		total_layers;		/* Total layers			*/
} BULK_PROCESS;


static Of_Info_t	_ofInfo[NINFO];
static TIME_Info        _day1[MXRANGE], _day2[MXRANGE], _day3[MXRANGE],
                        _day8[MXRANGE],
                        _enh20[MXRANGE], _enh00[MXRANGE],
                        _enh04[MXRANGE], _enh12[MXRANGE],
                        _enh16[MXRANGE], _fday1[MXRANGE], _fday2[MXRANGE],
                        _ext_fire[MXRANGE];
static BULK_PROCESS	_bulkProcess = { FALSE, 0, 0, 1 };
static int              _day1Count = 0, _day2Count = 0, _day3Count = 0,
                        _day8Count = 0,
                        _enh20Count = 0, _enh00Count = 0,
                        _enh04Count = 0, _enh12Count = 0,
                        _enh16Count = 0, _fday1Count = 0, _fday2Count = 0,
                        _ext_fireCount = 0;
static int              _WaitingTime;   /* The amount of time until the 
                                           outlook becomes valid        */
static int              _CurrDuration;  /* The duration of the outlook  */

struct dtgW {
    Widget	year;
    Widget	month;
    Widget	day;
    Widget	hhmm;
};

static struct dtgW 	_wTimes[2];	/* initial and expiration times */

static Widget		_pgofmtWin;
static WidgetList	_dayW;

static Widget		_fcstTxtW;
static Widget		_groupTypeW;

static int		_dayOpt    = 0;
static int		_dayOptNum;


/*
 *  private callback functions
 */
void pgofmt_ctlBtnCb (       Widget, long which, XtPointer );
void pgofmt_dayChoiceOptCb ( Widget, long which, XtPointer );
void pgofmt_menuTextCb (     Widget,  XtPointer, XtPointer );

/*
 *  private functions
 */
Widget pgofmt_createMenuText( Widget parent, char *labelstr, int ncol, 
				int textoff, int info_type, 
				Widget *textwid );
Widget pgofmt_createOptArea ( Widget parent, char *labelstr, int lbl_spc, 
				int nopt, char opts[][20], WidgetList optw,
				int opt_spc, int nrow, XtPointer optvalp );
void pgofmt_rdFcstInfo ( int *iret );
void pgofmt_rdTimeInfo ( int *iret );
void pgofmt_setTime ( int which );


/************************************************************************
 * nmap_pgofmt.c							*
 *									*
 * This module defines a format outlook popup window for product        *
 * generation.							        *
 *									*
 * CONTENTS:								*
 *	pgofmt_create()		create the format outlook window	*
 *	pgofmt_popup()		pop up the format outlook window	*
 *	pgofmt_popdown()	pop down the format outlook window	*
 *	pgofmt_update()		update the outlook message text area	*
 *									*
 *	pgofmt_isUp()		query if the window is up 		*
 *									*
 *	pgofmt_dayChoiceOptCb()	callback for the option buttons         *
 *	pgofmt_menuTextCb()	callback for the menu text widgets	*
 *	pgofmt_ctlBtnCb()	callback for control buttons 		*
 *									*
 *	pgofmt_createOptArea()	creates an option radio box		*
 *	pgofmt_createMenuText()	creates a text widget with a menu	*
 *	pgofmt_rdFcstInfo()	reads the outlook info table	        *
 *      pgofmt_rdTimeInfo()     reads the time range info table         *
 *	pgofmt_setTime()	sets the initial and expiration time	*
 *	pgofmt_bulkProcessOn()	checks the bulk processing flag		*
 *	pgofmt_bulkProcessNext()goes to the next vgf layer for outlook  *
 *	pgofmt_bulkProcessEnd() end bulk outlook message processing	*
 *				associated with "Otlk All" button	*
 ***********************************************************************/

/*=====================================================================*/

Widget pgofmt_create ( Widget parent )
/************************************************************************
 * pgofmt_create							*
 *									*
 * This function creates the format outlook popup window.		*
 *									*
 * Widget pgofmt_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pgofmt_create	Widget  ID of the format outlook popup window	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	        09/99	initial coding, copied formatting from  *
 *                              pgwfmt_create()	                        *
 * H. Zeng/EAI          02/00   added call to pgofmt_rdTimeInfo()       *
 * M. Li/GSC	        01/01	added group type			*
 * T. Piper/GSC		 7/01	freed btnw				*
 * E. Safford/SAIC	03/02	replace group type popup w/ label	*
 * J. Wu/SAIC		05/02	verify input to "month/day/year/hhmm"	*
 * G. Grosshans/SPC	11/04	Added DAY4 - DAY4-8 options		*
 * F. J. Yen/NCEP	04/07	Added ENH_PD1 and ENH_PD2 options	*
 * H. Zeng/SAIC		04/07	added "Otlk All" button			*
 * G. Grosshans/SPC     04/07   Added EXT_FIRE DAY 3-8. Removed DAY 4 - *
 *                              DAY 7 buttons				*
 * G. Grosshans/SPC	03/09	Deleted ENH_PD1 and ENH_PD2 and added	*
 *				ENH20, ENH00, ENH04, ENH12		*
 * G. Grosshans/SPC	02/10	Added ENH16				*
 * G. Grosshans/SPC	02/10	Added Day 1 & 2 Fire Weather options	*
 *				and re-ordered Day 3-8 Fire.		*
 ***********************************************************************/
{
    Widget	pane, vtimrc, rc1, rc2, rc3, rc4, rc5;
    Widget	fcstform, group_form;
    Widget	dayopt_form, format_form;
    WidgetList	btnw;

    int		nn, iret, ii, toff = 10, loff = 5;

    char        dayopts[][20] = {"Day 1", "Day 2", "Day 3",
                                 "Day 4-8", "Enh00", "Enh04",
                                 "Enh12", "Enh16", "Enh20",
				 "Day 1 Fire", "Day 2 Fire",
				 "Day 3-8 Fire"};

    char	*btnstrs[] = {"Continue", "Otlk All", "Cancel"};
/*---------------------------------------------------------------------*/

    /*
     *  Read in the forecaster information.
     */
    pgofmt_rdFcstInfo( &iret );

    /*
     *  Read in the time range information.
     */
    pgofmt_rdTimeInfo( &iret );

    /*
     * create dialog shell
     */
    _pgofmtWin = XmCreateFormDialog(parent, "pgofmt_popup",
				    NULL, 0);
    XtVaSetValues(_pgofmtWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgofmtWin),
		  XmNtitle, "FORMAT OUTLOOK",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgofmt_pane",
		    	xmPanedWindowWidgetClass, 	_pgofmtWin,
		    	XmNsashWidth,             	1,
			XmNsashHeight,            	1,
			NULL);
    
    /*
     * create FORMATTING area
     */
    format_form = XtVaCreateWidget("form",
			xmFormWidgetClass,      	pane,
			NULL);

    

    /*
     * day/prod choice
     */
    nn = XtNumber(dayopts);
    _dayW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    dayopt_form = pgofmt_createOptArea (format_form, "Day/Prod\nChoice", 
				       0, nn, dayopts, _dayW, 0, 
				       3, (XtPointer) &_dayOpt);
    _dayOptNum = nn;


    /*
     *  valid time
     */
    vtimrc = XtVaCreateWidget ("vtimrc",
			xmRowColumnWidgetClass,		format_form,
		       	XmNorientation,			XmHORIZONTAL,
			XmNpacking,			XmPACK_TIGHT,
			NULL);

    rc1 = XtVaCreateWidget ("vtimlabel",
			xmRowColumnWidgetClass,		vtimrc,
			XmNorientation,			XmVERTICAL,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

    rc2 = XtVaCreateWidget ("vtimmonth",
			xmRowColumnWidgetClass,		vtimrc,
			XmNorientation,			XmVERTICAL,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

    rc3 = XtVaCreateWidget ("vtimday",
			xmRowColumnWidgetClass,		vtimrc,
			XmNorientation,			XmVERTICAL,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

    rc4 = XtVaCreateWidget ("vtimyear",
			xmRowColumnWidgetClass,		vtimrc,
			XmNorientation,			XmVERTICAL,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

    rc5 = XtVaCreateWidget ("vtimhhmm",
			xmRowColumnWidgetClass,		vtimrc,
			XmNorientation,			XmVERTICAL,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

    nn = 8;
    XtVaCreateManagedWidget (" ",
			xmLabelWidgetClass,		rc1,
			XmNmarginHeight,		nn,
			NULL);

    XtVaCreateManagedWidget ("MM",
			xmLabelWidgetClass,		rc2,
			NULL);

    XtVaCreateManagedWidget ("DD",
			xmLabelWidgetClass,		rc3,
			NULL);

    XtVaCreateManagedWidget ("YYYY",
			xmLabelWidgetClass,		rc4,
			NULL);

    XtVaCreateManagedWidget ("HHMM",
			xmLabelWidgetClass,		rc5,
			NULL);

    XtVaCreateManagedWidget ("Initial Time",
			xmLabelWidgetClass,		rc1,
			XmNmarginHeight,		nn,
			NULL);

    XtVaCreateManagedWidget ("Expiration Time",
			xmLabelWidgetClass,		rc1,
			XmNmarginHeight,		nn,
			NULL);


    for (ii = 0; ii < 2; ii++) {
	_wTimes[ii].month = 
	    XtVaCreateManagedWidget("month", 
	    		xmTextFieldWidgetClass, 	rc2,
			XmNcolumns, 			2, 
			NULL);

	_wTimes[ii].day = 
	    XtVaCreateManagedWidget("day",   
	    		xmTextFieldWidgetClass, 	rc3,
			XmNcolumns, 			2, 
			NULL);

	_wTimes[ii].year = 
	    XtVaCreateManagedWidget("year",  
	    		xmTextFieldWidgetClass, 	rc4,
			XmNcolumns, 			4, 
			NULL);

	_wTimes[ii].hhmm = 
	    XtVaCreateManagedWidget("hhmm", 
	    		xmTextFieldWidgetClass, 	rc5,
			XmNcolumns, 			4, 
			NULL);

        XtAddCallback(_wTimes[ii].month, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
        XtAddCallback(_wTimes[ii].day, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
        XtAddCallback(_wTimes[ii].year, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
        XtAddCallback(_wTimes[ii].hhmm, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
   
    }


    XtVaSetValues (vtimrc,
		   	XmNtopAttachment,		XmATTACH_WIDGET,
		   	XmNtopWidget,			dayopt_form,
		   	XmNleftAttachment,		XmATTACH_FORM,
		   	NULL);



    XtManageChild (rc1);
    XtManageChild (rc2);
    XtManageChild (rc3);
    XtManageChild (rc4);
    XtManageChild (rc5);
    XtManageChild (vtimrc);


    /*
     * forecaster
     */
    fcstform = pgofmt_createMenuText (format_form, 
				      "Forecaster: ", MXCHR, 
				      loff, FCST, &_fcstTxtW);

    XtVaSetValues(fcstform, 
		  	XmNtopAttachment,		XmATTACH_WIDGET,
		  	XmNtopWidget,			vtimrc,
		  	XmNtopOffset,			toff,
		  	NULL);

    /*
     *  Group Type 
     */
    group_form = (Widget)XtVaCreateWidget("group_form",
                  	xmFormWidgetClass,             	format_form,
			XmNtopAttachment,		XmATTACH_WIDGET,
			XmNtopWidget,			fcstform,
			XmNtopOffset,			10,
			XmNleftAttachment,		XmATTACH_FORM,
			XmNrightAttachment,		XmATTACH_FORM,
			XmNfractionBase,		100,
                  	NULL);

    XtVaCreateManagedWidget ("Group Type:",
			xmLabelWidgetClass,		group_form,
			XmNleftAttachment,		XmATTACH_POSITION,
			XmNleftPosition,		2,
			XmNmarginHeight,		8,
			NULL);

    _groupTypeW = XtVaCreateManagedWidget ("group type",
    			xmLabelWidgetClass,		group_form,
			XmNleftAttachment,		XmATTACH_POSITION,
  			XmNleftPosition,		36,
			XmNmarginHeight,		8,
			NULL);

    XtManageChild (group_form);


    XtManageChild (format_form);


    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    btnw = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgofmt_ctlBtn", nn, btnstrs, 
			(XtCallbackProc)pgofmt_ctlBtnCb, btnw);
    XtFree((XtPointer)btnw);
    XtManageChild(pane);

    return(_pgofmtWin);
    
}

/*=====================================================================*/

void pgofmt_popup ( void )
/************************************************************************
 * pgofmt_popup								*
 *									*
 * This function pops up the format outlook popup window.		*
 *									*
 * void pgofmt_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 * E. Safford/SAIC	03/02	add group label set up			*
 ***********************************************************************/
{
	char		grptyp, grpname[20];
	XmString	xmstr;
/*---------------------------------------------------------------------*/

    /*
     * Set default initial and expiration time according to the 
     * default day choice. Because the function is time sensitive,
     * it is purposely put here instead of in pgofmt_create(). 
     */
    pgofmt_setTime(_dayOpt);

    /*
     *  Set the group type label
     */
    pgolk_getGroup (&grptyp, grpname);

    xmstr = XmStringCreateLocalized ( grpname );
    XtVaSetValues ( _groupTypeW,
    		    XmNlabelString,		xmstr,
		    NULL);
    XmStringFree (xmstr);


    XtManageChild (_pgofmtWin);

}

/*=====================================================================*/

void pgofmt_popdown ( void ) 
/************************************************************************
 * pgofmt_popdown							*
 *									*
 * This function pops down the format outlook window.			*
 *									*
 * void pgofmt_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pgofmtWin)) {
	XtUnmanageChild (_pgofmtWin);

    }
}

/*=====================================================================*/

void pgofmt_update ( int *iret )
/************************************************************************
 * pgofmt_update                                                        *
 *                                                                      *
 * This function creates the text for outlook message file.		*
 *                                                                      *
 * void pgofmt_update ( iret)                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret   int             Return value                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      	09/99   initial coding                          *
 * H. Zeng/EAI      	02/00   added new info. into outlook message    *
 * T. Piper/SAIC	12/01	free XmTextFieldGetString		*
 * G. Grosshans/SPC 	11/04	updated for special filename for	*
 * 				Day 4-8 product				*
 * F. J. Yen/NCEP	04/07	Set to DAY1 in filename,if enhanced TSTM*
 * G. Grosshans/SPC     04/07   Added EXT_FIRE DAY 3-8. Removed DAY 4 - *
 *                              DAY 7 buttons                           *
 * G. Grosshans/SPC	03/09	Deleted ENH_PD1 and ENH_PD2 and added	*
 *				ENH20, ENH00, ENH04, ENH12		*
 * G. Grosshans/SPC	02/10	Added ENH16				*
 * G. Grosshans/SPC	02/10	Added Fire Day 1 and Fire Day 2		*
 *				and moved Fire Day 3-8 down		*
 * G. Grosshans/SPC	12/13	Updated for Day 4-8 changes.		*
 * 				The LAYER NAMES (i.e. LPF) are NOW 	*
 * 				hard-coded to work.			*
 ***********************************************************************/
{
    int         which_day;
    int		ier;
    char	str[LSTR], *tmp[2], *layer_name; 
/*---------------------------------------------------------------------*/

    *iret = 0;
 
    /*
     *  Day choice
     *  If which_day = 4 then this is the day 4-8 summary graphic.
     *  If which_day is 5 - 9, then enhanced TSTM.  So, set day choice to 1.
     *  If which_day is 10 then this is the Fire Day 1. So, set day choice to 1.
     *  If which_day is 11 then this is the Fire Day 2. So, set day choice to 2. 
     *  If which_day is 12 then this is the Fire Day 3-8. 
     */
    which_day = _dayOpt+1;
    if ( which_day == 4 ) {
	which_day = 48 ;
	layer_name = pglayer_getName (_bulkProcess.curr_layer);
	if ( strcasecmp(layer_name, "DAY_4") == 0 ) which_day = 4;
	if ( strcasecmp(layer_name, "DAY_5") == 0 ) which_day = 5;
	if ( strcasecmp(layer_name, "DAY_6") == 0 ) which_day = 6;
	if ( strcasecmp(layer_name, "DAY_7") == 0 ) which_day = 7;
	if ( strcasecmp(layer_name, "DAY_8") == 0 ) which_day = 8;
    }
    else if ( which_day == 5 || which_day == 6 || which_day == 7 ||
              which_day == 8 || which_day == 9 ) {
	which_day = 1;
    }
    else if ( which_day == 10 ) {
	which_day = 1;
    }
    else if ( which_day == 11 ) {
	which_day = 2;
    }
    else if ( which_day == 12 ) {
	which_day = 38;
    }
    
    sprintf(str, "DAY%d\n", which_day );
    pgprd_putstr( str, &ier );

    /*
     *  Forecaster name
     */
    sprintf(str, "%s\n", 
	    tmp[0] = XmTextFieldGetString(_fcstTxtW) );
    XtFree(tmp[0]);
    pgprd_putstr( str, &ier );

    /*
     *  Initial time and expiration time (DD HHMM)
     */
    sprintf(str, "%s%sZ - ",
	    tmp[0] = XmTextFieldGetString(_wTimes[0].day),
	    tmp[1] = XmTextFieldGetString(_wTimes[0].hhmm) );
    XtFree(tmp[0]);
    XtFree(tmp[1]);
    pgprd_putstr( str, &ier );
    sprintf(str, "%s%sZ\n",
	    tmp[0] = XmTextFieldGetString(_wTimes[1].day),
	    tmp[1] = XmTextFieldGetString(_wTimes[1].hhmm) );
    XtFree(tmp[0]);
    XtFree(tmp[1]);
    pgprd_putstr( str, &ier );


}

/*=====================================================================*/

Boolean pgofmt_isUp ( void ) 
/************************************************************************
 * pgofmt_isUp								*
 *									*
 * Thi!s function queries whether the format outlook window is up.	*
 *									*
 * Boolean pgofmt_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgofmt_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_pgofmtWin));
}

/*=====================================================================*/
/* ARGSUSED */
void pgofmt_dayChoiceOptCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgofmt_dayChoiceOptCb						*
 *									*
 * Callback function for Day Choice radio buttons.			*
 *									*
 * void pgofmt_dayChoiceOptCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    _dayOpt = (int)which;

   /*
    * Set the initial and expiration time according to the day choice.
    */

    pgofmt_setTime((int)which);
    
}

/*=====================================================================*/
/* ARGSUSED */
void pgofmt_menuTextCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgofmt_menuTextCb							*
 *									*
 * Callback function for Forecaster menu buttons.			*
 *									*
 * void pgofmt_menuTextCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	copied from pgwfmt_menuTextCb()		*
 ***********************************************************************/
{
    char	*ptext;
    XmString	xmstr;
    XtPointer	userdata;
    Widget	*twid;
/*---------------------------------------------------------------------*/

    /*
     * menu callback
     */

  	XtVaGetValues (wid, 
		       XmNlabelString,	&xmstr, 
		       XmNuserData,	&userdata, 
		       NULL);

	XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);

	twid = (Widget *) userdata;
	XtVaSetValues (*twid, XmNvalue, ptext, NULL);

	XmStringFree (xmstr);
	XtFree (ptext);
   
}

/*=====================================================================*/
/* ARGSUSED */
void pgofmt_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgofmt_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of format        *
 * outlook popup window.						*
 *									*
 * void pgofmt_ctlBtnCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/99   copied from pgwfmt_ctlBtnCb()           *
 * H. Zeng/EAI          02/00   added duration check popup window       *
 * A. Hardy/NCEP	 6/03	added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * H. Zeng/SAIC		04/07	added processing for "Otlk All"		*
 * G. Grosshans/SPC	02/09	Added logic to skip Prob2cat layer,	*
 *				similar to the GEN-TSTM.		*
 * G. Grosshans/SPC	12/13	Updated for Day 4-8 changes.		*
 * 				NEEDED to handle NON-OTLK-ALL option.	*
 * 				The LAYER NAMES (i.e. LPF) are now 	*
 * 				hard-coded to work.			*
 ***********************************************************************/
{
    char	mesg[128], *ptr, *layer_name, *pvalue, tmzn[4];
    int         ctime[5], itime[5], etime[5], hhmm, isec, julian, ier;
    int		itype = 1;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Continue */
	/*
	 * Need to check and see if there are LAYERS and if there are
	 * LAYERS see if its for Day 4-8 and handle appropriately.
	 */

        _bulkProcess.start_layer   = pglayer_getCurLayer();
        _bulkProcess.curr_layer    = _bulkProcess.start_layer;
        _bulkProcess.total_layers  = pglayer_getNumLayers();
	layer_name = pglayer_getName (_bulkProcess.curr_layer);

      case 1:	/* Otlk All */

        /*
         * Set _bulkProcess value according to the btn seletion.
         */
        if ( which == 0 ) {

          /*
           * For "Continue", set _bulkProcess.process_is_on to FALSE.
           */
	  _bulkProcess.process_is_on = FALSE;
        }
	else if ( which == 1 ) {

          /* 
           * For "Otlk All", if _bulkProcess.process_is_on is TRUE,
           * continue on, if not, start the process by setting the
           * flag on.
           */
          if ( !_bulkProcess.process_is_on ) {
	    _bulkProcess.process_is_on = TRUE;
	    _bulkProcess.start_layer   = pglayer_getCurLayer();
	    _bulkProcess.curr_layer    = _bulkProcess.start_layer;
	    _bulkProcess.total_layers  = pglayer_getNumLayers();
          }

          /*
           * For "Otlk All", if the layer name is "GEN-TSTM" or "PR2CAT",
           * skip this layer and go to the next one.
           */
          layer_name = pglayer_getName (_bulkProcess.curr_layer);
          if ( strcasecmp(layer_name, "GEN-TSTM") == 0 ||
               strcasecmp(layer_name, "PR2CAT") == 0 ) {

	    pgofmt_popdown();
	    pgprd_popdown ();
	    pgofmt_bulkProcessNext();

	    break;
          }

        } /* the end of else if ( which == 1... */

        pvalue = XmTextFieldGetString(_wTimes[0].year);
        sscanf(pvalue, "%d", &(itime[0]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[0].month);
        sscanf(pvalue, "%d", &(itime[1]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[0].day);
        sscanf(pvalue, "%d", &(itime[2]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[0].hhmm);
        sscanf(pvalue, "%d", &hhmm );
        XtFree(pvalue);
        itime[3] = hhmm/100;
        itime[4] = hhmm%100;

        pvalue = XmTextFieldGetString(_wTimes[1].year);
        sscanf(pvalue, "%d", &(etime[0]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[1].month);
        sscanf(pvalue, "%d", &(etime[1]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[1].day);
        sscanf(pvalue, "%d", &(etime[2]) );
        XtFree(pvalue);
        pvalue = XmTextFieldGetString(_wTimes[1].hhmm);
        sscanf(pvalue, "%d", &hhmm );
        XtFree(pvalue);
        etime[3] = hhmm/100;
        etime[4] = hhmm%100;

        css_date ( &itype, &(ctime[0]), &(ctime[1]), &(ctime[2]), 
                   &(ctime[3]), &(ctime[4]), &isec, &julian, tmzn, &ier);

        ti_mdif(itime, ctime, &(_WaitingTime), &ier);
        ti_mdif(etime, itime, &(_CurrDuration), &ier);

        sprintf(mesg, "The duration of your outlook will be%3dh%2dm", 
	        (_CurrDuration/60), (_CurrDuration%60) );
        if(_WaitingTime >= 0) {
          ptr = mesg + strlen(mesg);
          sprintf(ptr, "\nThe outlook will become valid in%3dh%2dm", 
	          (_WaitingTime/60), (_WaitingTime%60) );
        }
        else {
          ptr = mesg + strlen(mesg);
          sprintf(ptr, "\nThe outlook became valid %3dh%2dm ago", 
	          (_WaitingTime*(-1)/60), (_WaitingTime*(-1)%60) );
        }

	NxmConfirm_show(_pgofmtWin, mesg, (XtCallbackProc)pgofmt_ctlBtnCb,
				NULL, (XtPointer)3, &ier );

	break;

      case 2:	/* Cancel */
	_bulkProcess.process_is_on = FALSE;

	pgofmt_popdown ();
	pgprd_popdown ();

	break;

      case 3:	/* DURATION CHECK (called from case 0 & 1) */
        pgofmt_popdown();
        pgprd_popup (); 
       
	break;

    } /* the end of switch(which) */

}

/*=====================================================================*/

Widget pgofmt_createOptArea ( Widget parent, char *labelstr, int lbl_spc, 
			      int nopt, char opts[][20], WidgetList optw, 
			      int opt_spc, int nrow, XtPointer optvalp )
/************************************************************************
 * pgofmt_createOptArea							*
 *									*
 * This function creates an radio box selection area for options.	*
 *									*
 * Widget pgofmt_createOptArea ( parent, labelstr, lbl_spc, nopt, opts,	*
 *				 optw, opt_spc, nrow, optvalp )		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label for the selection area		*
 *	lbl_spc		int	spacing between the label and option box*
 *	nopt		int	number of options			*
 *	opts[][20]	char	option names				*
 *	optw		WidgetList option button widgets		*
 *	opt_spc		int	spacing between the options		*
 *	nrow		int	number of rows				*
 *	optvalp		XtPointer pointer to the current option		*
 *									*
 * Output parameters:							*
 * pgofmt_createOptArea Widget	Widget ID of a form container widget 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/99   copied from pgwfmt_createOptArea()      *
 * E. Safford/GSC	12/00	fixed compiler type mismatch warnings   *
 ***********************************************************************/
{
    Widget	form, label, optrc, button;
    int		*optval, pos;
    long	ii;
/*---------------------------------------------------------------------*/

    optval = (int *) optvalp;

    /*
     * create a form container 
     */
    form = XtVaCreateWidget("form", xmFormWidgetClass, parent, NULL);

    /*
     * create label 
     */
    if (nrow > 0) {
	pos = 20/nrow;
    }
    else {
	pos = 20;
    }

    label = XtVaCreateManagedWidget("label",
				    xmLabelWidgetClass,	form,
				    XmNtopAttachment,	XmATTACH_POSITION,
				    XmNtopPosition,	pos,
				    NULL);
    NxmLabel_setStr(label, labelstr);

    /*
     * create option radio box 
     */
    if (lbl_spc == 0) {
	lbl_spc = 10;
    }

    if (opt_spc == 0) {
	opt_spc = 10;
    }

    optrc = XtVaCreateWidget("optrc",
			     xmRowColumnWidgetClass,	form,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_COLUMN,
			     XmNnumColumns,		nrow,
			     XmNspacing,		opt_spc,
			     XmNradioBehavior,		True,
			     XmNtopAttachment,		XmATTACH_FORM,
			     XmNleftAttachment,		XmATTACH_WIDGET,
			     XmNleftWidget,		label,
			     XmNleftOffset,		lbl_spc,
			     NULL);

    for (ii = 0; ii < nopt; ii++) {

	button = XtVaCreateManagedWidget(opts[ii],
					 xmToggleButtonWidgetClass, optrc,
					 XmNuserData,               optvalp,
                                         XmNtraversalOn,            FALSE,
					 NULL);

	XtAddCallback (button, XmNarmCallback,
		       (XtCallbackProc)	pgofmt_dayChoiceOptCb,
		       (XtPointer) ii);

	if (ii == *optval) {
	    XmToggleButtonSetState (button, True, False);
	}

	if (optw) optw[ii] = button;
    }

    XtManageChild (optrc);

    XtManageChild (form);

    return (form);
}

/*=====================================================================*/

Widget pgofmt_createMenuText ( Widget parent, char *labelstr, int ncol, 
			       int textoff, int info_type, Widget *textwid )
/************************************************************************
 * pgofmt_createMenuText						*
 *									*
 * Creates a labeled text field widget with a pulldown menu.	        *
 *									*
 * Widget pgofmt_createMenuText ( parent, labelstr, ncol, textoff,	*
 *					info_type, textwid )		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label string				*
 *	ncol		int	number of text columns			*
 *	textoff		int	offset between text and label		*
 *	info_type	int	type of information			*
 *									*
 * Output parameters:							*
 *	*textwid	Widget	text field widget			*
 * pgofmt_createMenuText Widget	Widget ID of the form widget		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/99   copied from pgwfmt_createMenuText()     *
 * M. Li/SAIC		12/01	arrow -> menu_arrow			*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	form, label, menub, cascade, menu, button;
    int		iret, toff = 5;
    XmString	xmstr;
    Pixel	fg, bg;
    long	ii, ignore;
    char	filename[256];
    static Pixmap	menu_pxm;
    static Boolean	first = TRUE;
/*---------------------------------------------------------------------*/

    /*
     * create a row column container
     */
    form = XtVaCreateWidget("form",
			    xmFormWidgetClass,	parent,
			    XmNnumColumns,	1,
			    XmNorientation,	XmHORIZONTAL,
			    XmNradioBehavior,	FALSE,
			    XmNpacking,		XmPACK_TIGHT,
			    NULL );

    /*
     * create label 
     */
    label = XtVaCreateManagedWidget ("tmlabel",
				     xmLabelWidgetClass,	form,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff,
				     NULL);
    NxmLabel_setStr(label, labelstr);

    /*
     * create text field 
     */
    *textwid = XtVaCreateManagedWidget ("tmtext", 
					xmTextFieldWidgetClass, form,
					XmNcolumns,		ncol, 
					XmNleftAttachment,	XmATTACH_WIDGET,
					XmNleftWidget,		label,
					XmNleftOffset,		textoff,
					NULL);

    XtVaSetValues (*textwid,  XmNuserData, info_type, NULL);


    /*
     * create menu
     */
    menub = XmCreateMenuBar (form, "tmbar", NULL, 0);

    XtVaSetValues (menub, 
		   XmNleftAttachment,		XmATTACH_WIDGET,
		   XmNleftWidget,		*textwid,
		   XmNleftOffset,		0,
		   XmNmarginHeight,		0,
		   XmNmarginWidth,		0,
		   XmNborderWidth,		0,
		   XmNwidth,			5,
		   XmNhighlightThickness,	1,
		   XmNshadowThickness,		1,
		   NULL);

    menu  = XmCreatePulldownMenu (menub, "tmmenu", NULL, 0);

    cascade = XtVaCreateManagedWidget ("tmcascade", 
				       xmCascadeButtonWidgetClass, menub,
				       XmNsubMenuId, menu, 
				       NULL);

    if (first) {
	first = FALSE;

	XtVaGetValues (parent,
		       XmNforeground,	&fg,
		       XmNbackground, 	&bg,
		       NULL);

	cfl_inqr ("menu_arrow.xbm", "$NAWIPS/icons/nmap", &ignore, filename, &iret);

	if (iret == 0) {
	    menu_pxm = XmGetPixmap (XtScreen (parent), filename, fg, bg);
	}
	else {
	    menu_pxm = XmUNSPECIFIED_PIXMAP;
	}
    }

    if (menu_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP) {
	xmstr = XmStringCreateLocalized ("\\/");

	XtVaSetValues (cascade,
		       XmNlabelString, xmstr,
		       NULL);

	XmStringFree (xmstr);
    }
    else {
	XtVaSetValues (cascade, 
		       XmNlabelType,		XmPIXMAP,
		       XmNlabelPixmap,		menu_pxm,
		       XmNmarginHeight,		0,
		       XmNmarginWidth,		0,
		       NULL);
    }

    for (ii = 0; ii < _ofInfo[info_type].nelems; ii++) {
	xmstr = XmStringCreateLocalized (_ofInfo[info_type].range[ii]);
	button = XtVaCreateManagedWidget ("tmbutton", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	xmstr,
					  XmNuserData,		*textwid,
					  NULL);
	XmStringFree (xmstr);

	XtAddCallback (button, XmNactivateCallback,
		       (XtCallbackProc) pgofmt_menuTextCb,
		       (XtPointer) ii);

	
    }

    XtManageChild(menub);
    XtManageChild(form);
    return (form);
}

/*=====================================================================*/

void pgofmt_rdFcstInfo ( int *iret )
/************************************************************************
 * pgofmt_rdFcstInfo							*
 *									*
 * This function reads the forecasters table.	             	        *
 * The table is read in only once.					*
 *									*
 * void pgofmt_rdFcstInfo( iret )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *                               -1 - Unable to open table      	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/99   copied from pgwfmt_rdInfo()             *
 * R. Curtis/EAI        05/00   reads from modified forecasters table   *
 * T. Piper/SAIC	12/01	close file				*
 * S. Jacobs/NCEP	 9/09	Increased number of forecasters to 50	*
 ***********************************************************************/
{
    int		ii, ier;
    char	buff[256], fnm[32],  name[17];
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Open the forecasters table. If not found, return an error.
     */
    strcpy(fnm, FORECASTERS_TBL);
    fp = cfl_tbop(fnm, "config", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    /*
     *  Initialize the information structures.
     */
    for (ii = 0; ii < NINFO; ii++)  {
        _ofInfo[ii].nelems = 0;
    }


    /*
     *  Scan table line-by-line.
     */

    while ( !feof(fp) )  {

	cfl_trln(fp, sizeof(buff), buff, &ier);

	if ( ier == 0 ) {

	  /*
           *  Clear memory and scan in forecasters name
           */
            memset(name, '\0', 17);
            sscanf(buff, "%s", name);

		/*
		 *  Process entry for forecaster name.
		 */
		strcpy ( _ofInfo[FCST].range[_ofInfo[FCST].nelems], name );
		_ofInfo[FCST].nelems++;
        }

    }
    cfl_clos(fp, &ier);
}

/*=====================================================================*/

void pgofmt_rdTimeInfo ( int *iret )
/************************************************************************
 * pgofmt_rdTimeInfo							*
 *									*
 * This function reads the time range information table.		*
 * The table is read in only once.					*
 *									*
 * void pgofmt_rdTimeInfo( iret )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *                               -1 - Unable to open table      	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/00   initial coding                          *
 * T. Piper/SAIC	12/01	Close file				*
 * G. Grosshans/SPC	11/04	Added DAY4 - DAY4-8 logic		*
 * F. J. Yen/NCEP	04/07	Added ENH_PD1 and ENH_PD2 logic		*
 * G. Grosshans/SPC     04/07   Added EXT_FIRE DAY 3-8. Removed DAY 4 - *
 *                              DAY 7 buttons                           *
 * G. Grosshans/SPC	03/09	Deleted ENH_PD1 and ENH_PD2 and added	*
 *				ENH20, ENH00, ENH04, ENH12		*
 * G. Grosshans/SPC	02/10	Added ENH16				*
 * G. Grosshans/SPC	02/10	Added Fire Day 1 & 2			*
 ***********************************************************************/
{
    int		ier;
    char	buff[256], fnm[32], day[12];
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Open the information table. If not found, return an error.
     */
    strcpy(fnm, TIMERANGE_TBL);
    fp = cfl_tbop(fnm, "pgen", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    /*
     *  Scan table line-by-line.
     */
    while ( !feof(fp) )  {

	cfl_trln(fp, sizeof(buff), buff, &ier);

        if ( ier == 0 )  {

	    sscanf ( buff, "%s", day );

            if (strcmp(day, "DAY1") == 0) {

		/*
		 *  Process entry for day 1.
		 */
	       sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_day1[_day1Count].time_range[0]),
                       &(_day1[_day1Count].time_range[1]),
                       &(_day1[_day1Count].initial_time[0]),
                       &(_day1[_day1Count].initial_time[1]),
                       &(_day1[_day1Count].expiration_time[0]),
                       &(_day1[_day1Count].expiration_time[1])  );

               _day1Count++;

            }
            else if(strcmp(day, "DAY2") == 0) {

		/*
		 *  Process entry for day 2.
		 */
	       sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_day2[_day2Count].time_range[0]),
                       &(_day2[_day2Count].time_range[1]),
                       &(_day2[_day2Count].initial_time[0]),
                       &(_day2[_day2Count].initial_time[1]),
                       &(_day2[_day2Count].expiration_time[0]),
                       &(_day2[_day2Count].expiration_time[1])  );

               _day2Count++;
            }
            else if(strcmp(day, "DAY3") == 0) {

		/*
		 *  Process entry for day 3.
		 */
	       sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_day3[_day3Count].time_range[0]),
                       &(_day3[_day3Count].time_range[1]),
                       &(_day3[_day3Count].initial_time[0]),
                       &(_day3[_day3Count].initial_time[1]),
                       &(_day3[_day3Count].expiration_time[0]),
                       &(_day3[_day3Count].expiration_time[1])  );

               _day3Count++;
            }
            else if (strcmp(day, "DAY4-8") == 0) { 
		/*
		 *  Process entry for day 8 (i.e. "DAY4-8").
		 */
	       sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_day8[_day8Count].time_range[0]),
                       &(_day8[_day8Count].time_range[1]),
                       &(_day8[_day8Count].initial_time[0]),
                       &(_day8[_day8Count].initial_time[1]),
                       &(_day8[_day8Count].expiration_time[0]),
                       &(_day8[_day8Count].expiration_time[1])  );

               _day8Count++;
            }
            else if (strcmp(day, "ENH00") == 0) {

                /*
                 *  Process entry for ENH00 (Enhanced TSTM Period 2).
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_enh00[_enh00Count].time_range[0]),
                       &(_enh00[_enh00Count].time_range[1]),
                       &(_enh00[_enh00Count].initial_time[0]),
                       &(_enh00[_enh00Count].initial_time[1]),
                       &(_enh00[_enh00Count].expiration_time[0]),
                       &(_enh00[_enh00Count].expiration_time[1])  );

               _enh00Count++;
            }
            else if (strcmp(day, "ENH04") == 0) {

                /*
                 *  Process entry for ENH04 (Enhanced TSTM Period 3).
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_enh04[_enh04Count].time_range[0]),
                       &(_enh04[_enh04Count].time_range[1]),
                       &(_enh04[_enh04Count].initial_time[0]),
                       &(_enh04[_enh04Count].initial_time[1]),
                       &(_enh04[_enh04Count].expiration_time[0]),
                       &(_enh04[_enh04Count].expiration_time[1])  );

               _enh04Count++;
            }
            else if (strcmp(day, "ENH12") == 0) {

                /*
                 *  Process entry for ENH12 (Enhanced TSTM Period 4).
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_enh12[_enh12Count].time_range[0]),
                       &(_enh12[_enh12Count].time_range[1]),
                       &(_enh12[_enh12Count].initial_time[0]),
                       &(_enh12[_enh12Count].initial_time[1]),
                       &(_enh12[_enh12Count].expiration_time[0]),
                       &(_enh12[_enh12Count].expiration_time[1])  );

               _enh12Count++;
            }
            else if (strcmp(day, "ENH16") == 0) {

                /*
                 *  Process entry for ENH16 (Enhanced TSTM Period 5).
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_enh16[_enh16Count].time_range[0]),
                       &(_enh16[_enh16Count].time_range[1]),
                       &(_enh16[_enh16Count].initial_time[0]),
                       &(_enh16[_enh16Count].initial_time[1]),
                       &(_enh16[_enh16Count].expiration_time[0]),
                       &(_enh16[_enh16Count].expiration_time[1])  );

               _enh16Count++;
            }
            else if (strcmp(day, "ENH20") == 0) {

                /*
                 *  Process entry for ENH20 (Enhanced TSTM Period 1).
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_enh20[_enh20Count].time_range[0]),
                       &(_enh20[_enh20Count].time_range[1]),
                       &(_enh20[_enh20Count].initial_time[0]),
                       &(_enh20[_enh20Count].initial_time[1]),  
                       &(_enh20[_enh20Count].expiration_time[0]),
                       &(_enh20[_enh20Count].expiration_time[1])  );

               _enh20Count++;
            }
            else if (strcmp(day, "FDAY1") == 0) {

                /*
                 *  Process entry for Fire Weather Day 1.
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_fday1[_fday1Count].time_range[0]),
                       &(_fday1[_fday1Count].time_range[1]),
                       &(_fday1[_fday1Count].initial_time[0]),
                       &(_fday1[_fday1Count].initial_time[1]),
                       &(_fday1[_fday1Count].expiration_time[0]),
                       &(_fday1[_fday1Count].expiration_time[1])  );

               _fday1Count++;
            }
            else if (strcmp(day, "FDAY2") == 0) {

                /*
                 *  Process entry for Fire Weather Day 2.
                 */
               sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_fday2[_fday2Count].time_range[0]),
                       &(_fday2[_fday2Count].time_range[1]),
                       &(_fday2[_fday2Count].initial_time[0]),
                       &(_fday2[_fday2Count].initial_time[1]),
                       &(_fday2[_fday2Count].expiration_time[0]),
                       &(_fday2[_fday2Count].expiration_time[1])  );

               _fday2Count++;
            }
            else if (strcmp(day, "DAY3-8") == 0) {

		/*
		 *  Process entry for day 8 (i.e. "DAY3-8_FIRE").
		 */
	       sscanf (buff, "%s %d %d %d %d %d %d", day,
                       &(_ext_fire[_ext_fireCount].time_range[0]),
                       &(_ext_fire[_ext_fireCount].time_range[1]),
                       &(_ext_fire[_ext_fireCount].initial_time[0]),
                       &(_ext_fire[_ext_fireCount].initial_time[1]), 
                       &(_ext_fire[_ext_fireCount].expiration_time[0]),
                       &(_ext_fire[_ext_fireCount].expiration_time[1])  );

               _ext_fireCount++;
            }

        } /* the end of if ( ier == 0 ) */

    } /* the end of while */
    cfl_clos(fp, &ier);
}

/*=====================================================================*/

void pgofmt_setTime ( int which )
/************************************************************************
 * pgofmt_setTime							*
 *									*
 * Sets the defaults for the initial and expiration times.		*
 *									*
 * void pgofmt_setTime ( which )					*
 *									*
 * Input parameters:							*
 *          which       int      Day choice                             *
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/99   initial coding                          *
 * H. Zeng/EAI          02/00   modified for new time range table       *
 * A. Hardy/NCEP	 6/03	added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * G. Grosshans/SPC     11/04   Added DAY4 - DAY4-8 logic               *
 * F. J. Yen/NCEP	04/07	Added ENH_PD1 and ENH_PD2 logic		*
 * G. Grosshans/SPC     04/07   Added EXT_FIRE DAY 3-8. Removed DAY 4 - *
 *                              DAY 7 buttons                           *
 * G. Grosshans/SPC	02/09	Added ENH_PD3, ENH_PD4 and ENH_PD5	*
 *				logic. 					*
 * G. Grosshans/SPC	02/10	Added ENH16				*
 * G. Grosshans/SPC	02/10	Added Fire Day 1 & 2 and re-ordered	*
 *				entries.				*
 ***********************************************************************/
{
    int         iyear, imon, iday, ihour, imin, isec, julian, iret;
    int         itime[5], etime[5], curr_hhmm, ii;
    int         tintv=1440, itype = 1;
    char        temp[10], tmzn[4];
/*---------------------------------------------------------------------*/

    css_date ( &itype, &iyear, &imon, &iday, &ihour, &imin, &isec, &julian, 
               tmzn, &iret);

    switch(which) {
       case 0:      
	               
       /*  
        * Day 1 is chosen 
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _day1Count;  ii++) {
	     if(curr_hhmm >= _day1[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_day1[ii].initial_time[0] / 100;
                 itime[4]=_day1[ii].initial_time[0] % 100;
                 if(_day1[ii].initial_time[1]) {
                   tintv = 1440*(_day1[ii].initial_time[1]);
	           ti_addm(itime, &tintv, itime, &iret); 
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_day1[ii].expiration_time[0] / 100;   
                 etime[4]=_day1[ii].expiration_time[0] % 100;
                 if(_day1[ii].expiration_time[1]) {
                   tintv = 1440*(_day1[ii].expiration_time[1]);
	           ti_addm(etime, &tintv, etime, &iret); 
                 }

                 break;

	     }

	   } /* the end of for */      

           break;

       case 1:

       /*  
        * Day 2 is chosen 
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _day2Count;  ii++) {
	     if(curr_hhmm >= _day2[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_day2[ii].initial_time[0] / 100;
                 itime[4]=_day2[ii].initial_time[0] % 100;
                 if(_day2[ii].initial_time[1]) {
                   tintv = 1440*(_day2[ii].initial_time[1]);
	           ti_addm(itime, &tintv, itime, &iret); 
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_day2[ii].expiration_time[0] / 100;   
                 etime[4]=_day2[ii].expiration_time[0] % 100;
                 if(_day2[ii].expiration_time[1]) {
                   tintv = 1440*(_day2[ii].expiration_time[1]);
	           ti_addm(etime, &tintv, etime, &iret); 
                 }

                 break;

	     }

	   } /* the end of for */      

           break;

       case 2:      
	               
       /*  
        * Day 3 is chosen 
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _day3Count;  ii++) {
	     if(curr_hhmm >= _day3[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_day3[ii].initial_time[0] / 100;
                 itime[4]=_day3[ii].initial_time[0] % 100;
                 if(_day3[ii].initial_time[1]) {
                   tintv = 1440*(_day3[ii].initial_time[1]);
	           ti_addm(itime, &tintv, itime, &iret); 
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_day3[ii].expiration_time[0] / 100;   
                 etime[4]=_day3[ii].expiration_time[0] % 100;
                 if(_day3[ii].expiration_time[1]) {
                   tintv = 1440*(_day3[ii].expiration_time[1]);
	           ti_addm(etime, &tintv, etime, &iret); 
                 }

                 break;

	     }

	   } /* the end of for */      

           break;

       case 3:      
	               
       /*  
        * Day 4-8 is chosen 
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _day8Count;  ii++) {
	     if(curr_hhmm >= _day8[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_day8[ii].initial_time[0] / 100;
                 itime[4]=_day8[ii].initial_time[0] % 100;
                 if(_day8[ii].initial_time[1]) {
                   tintv = 1440*(_day8[ii].initial_time[1]);
	           ti_addm(itime, &tintv, itime, &iret); 
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_day8[ii].expiration_time[0] / 100;   
                 etime[4]=_day8[ii].expiration_time[0] % 100;
                 if(_day8[ii].expiration_time[1]) {
                   tintv = 1440*(_day8[ii].expiration_time[1]);
	           ti_addm(etime, &tintv, etime, &iret); 
                 }

                 break;

	     }

	   } /* the end of for */      

           break;

       case 4:

       /*
        * ENH00 is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _enh00Count;  ii++) {
             if(curr_hhmm >= _enh00[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_enh00[ii].initial_time[0] / 100;
                 itime[4]=_enh00[ii].initial_time[0] % 100;
                 if(_enh00[ii].initial_time[1]) {
                   tintv = 1440*(_enh00[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_enh00[ii].expiration_time[0] / 100;
                 etime[4]=_enh00[ii].expiration_time[0] % 100;
                 if(_enh00[ii].expiration_time[1]) {
                   tintv = 1440*(_enh00[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of ENH00*/

           break;


       case 5:

       /*
        * ENH04 is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _enh04Count;  ii++) {
             if(curr_hhmm >= _enh04[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_enh04[ii].initial_time[0] / 100;
                 itime[4]=_enh04[ii].initial_time[0] % 100;
                 if(_enh04[ii].initial_time[1]) {
                   tintv = 1440*(_enh04[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_enh04[ii].expiration_time[0] / 100;
                 etime[4]=_enh04[ii].expiration_time[0] % 100;
                 if(_enh04[ii].expiration_time[1]) {
                   tintv = 1440*(_enh04[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of ENH04*/

           break;


       case 6:

       /*
        * ENH12 is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _enh12Count;  ii++) {
             if(curr_hhmm >= _enh12[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_enh12[ii].initial_time[0] / 100;
                 itime[4]=_enh12[ii].initial_time[0] % 100;
                 if(_enh12[ii].initial_time[1]) {
                   tintv = 1440*(_enh12[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_enh12[ii].expiration_time[0] / 100;
                 etime[4]=_enh12[ii].expiration_time[0] % 100;
                 if(_enh12[ii].expiration_time[1]) {
                   tintv = 1440*(_enh12[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of ENH12*/

           break;


       case 7:

       /*
        * ENH16 is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _enh16Count;  ii++) {
             if(curr_hhmm >= _enh16[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_enh16[ii].initial_time[0] / 100;
                 itime[4]=_enh16[ii].initial_time[0] % 100;
                 if(_enh16[ii].initial_time[1]) {
                   tintv = 1440*(_enh16[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_enh16[ii].expiration_time[0] / 100;
                 etime[4]=_enh16[ii].expiration_time[0] % 100;
                 if(_enh16[ii].expiration_time[1]) {
                   tintv = 1440*(_enh16[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of ENH16*/

           break;


       case 8:

       /*
        * ENH20 is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _enh20Count;  ii++) {
             if(curr_hhmm >= _enh20[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_enh20[ii].initial_time[0] / 100;
                 itime[4]=_enh20[ii].initial_time[0] % 100;
                 if(_enh20[ii].initial_time[1]) {
                   tintv = 1440*(_enh20[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_enh20[ii].expiration_time[0] / 100;
                 etime[4]=_enh20[ii].expiration_time[0] % 100;
                 if(_enh20[ii].expiration_time[1]) {
                   tintv = 1440*(_enh20[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of ENH20*/

           break;


       case 9:

       /*
        * Day 1 Fire is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _fday1Count;  ii++) {
             if(curr_hhmm >= _fday1[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_fday1[ii].initial_time[0] / 100;
                 itime[4]=_fday1[ii].initial_time[0] % 100;
                 if(_fday1[ii].initial_time[1]) {
                   tintv = 1440*(_fday1[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_fday1[ii].expiration_time[0] / 100;
                 etime[4]=_fday1[ii].expiration_time[0] % 100;
                 if(_fday1[ii].expiration_time[1]) {
                   tintv = 1440*(_fday1[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of Day 1 Fire*/

           break;


       case 10:

       /*
        * Day 2 Fire is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _fday2Count;  ii++) {
             if(curr_hhmm >= _fday2[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_fday2[ii].initial_time[0] / 100;
                 itime[4]=_fday2[ii].initial_time[0] % 100;
                 if(_fday2[ii].initial_time[1]) {
                   tintv = 1440*(_fday2[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_fday2[ii].expiration_time[0] / 100;
                 etime[4]=_fday2[ii].expiration_time[0] % 100;
                 if(_fday2[ii].expiration_time[1]) {
                   tintv = 1440*(_fday2[ii].expiration_time[1]);
                   ti_addm(etime, &tintv, etime, &iret);
                 }

                 break;

             }

           } /* the end of Day 2 Fire*/

           break;


       case 11:      
	               
       /*  
        * Day 3-8 Fire is chosen
        */
           curr_hhmm = ihour*100 + imin;
           for(ii = 0; ii < _ext_fireCount;  ii++) {
             if(curr_hhmm >= _ext_fire[ii].time_range[0]) {
                 itime[0]=iyear; itime[1]=imon; itime[2]=iday;
                 itime[3]=_ext_fire[ii].initial_time[0] / 100;
                 itime[4]=_ext_fire[ii].initial_time[0] % 100;
                 if(_ext_fire[ii].initial_time[1]) {
                   tintv = 1440*(_ext_fire[ii].initial_time[1]);
                   ti_addm(itime, &tintv, itime, &iret);
                 }

                 etime[0]=iyear; etime[1]=imon; etime[2]=iday;
                 etime[3]=_ext_fire[ii].expiration_time[0] / 100;
                 etime[4]=_ext_fire[ii].expiration_time[0] % 100;
                 if(_ext_fire[ii].expiration_time[1]) {
                   tintv = 1440*(_ext_fire[ii].expiration_time[1]);
	           ti_addm(etime, &tintv, etime, &iret); 
                 }

                 break;

	     }

	   } /* the end of for */      

           break;



    } /* the end of switch */

    sprintf(temp, "%02d", itime[1]);
    XmTextSetString(_wTimes[0].month, temp);
    sprintf(temp, "%02d", etime[1]);
    XmTextSetString(_wTimes[1].month, temp);
    sprintf(temp, "%02d", itime[2]);
    XmTextSetString(_wTimes[0].day, temp);
    sprintf(temp, "%02d", etime[2]);
    XmTextSetString(_wTimes[1].day, temp);
    sprintf(temp, "%04d", itime[0]);
    XmTextSetString(_wTimes[0].year, temp);
    sprintf(temp, "%04d", etime[0]);
    XmTextSetString(_wTimes[1].year, temp);
    sprintf(temp, "%02d", itime[3]);
    sprintf((temp+2), "%02d", itime[4]);
    XmTextSetString(_wTimes[0].hhmm, temp);
    sprintf(temp, "%02d", etime[3]);
    sprintf((temp+2), "%02d", etime[4]);
    XmTextSetString(_wTimes[1].hhmm, temp);


}

/*=====================================================================*/

Boolean pgofmt_bulkProcessOn ( void ) 
/************************************************************************
 * pgofmt_bulkProcessOn							*
 *									*
 * This function checks if the bulk processing flag is on.		*
 *									*
 * void pgofmt_bulkProcessOn ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *   pgofmt_bulkProcessOn	Boolean		on/off			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (_bulkProcess.process_is_on);
}

/*=====================================================================*/

void pgofmt_bulkProcessNext ( void ) 
/************************************************************************
 * pgofmt_bulkProcessNext						*
 *									*
 * This function switches to the next vgf layer and pops up the "FORMAT *
 * OUTLOOK" window & "CONFIRM" window.					*
 *									*
 * void pgofmt_bulkProcessNext ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/07	initial coding				*
 * F. J. Yen/NCEP	04/07	Update button numbers due to adding EXT_*
 *				FIRE Day 3-8 and removing Day 4 - Day 7	*
 * G. Grosshans/SPC	02/09	Added ENH_PD3, ENH_PD4 and ENH_PD5	*
 *				logic. 					*
 * G. Grosshans/SPC	02/09	Added logic to skip Prob2cat layer,	*
 *				similar to the GEN-TSTM.		*
 * G. Grosshans/SPC	02/10	Added logic for ENH16			*
 * G. Grosshans/SPC	02/10	Modified to handle change in		*
 *				_dayOpt and which order.		*
 ***********************************************************************/
{
    int    grpid, ii, ier;
    char   grpname[128], *layer_name;
/*---------------------------------------------------------------------*/

    /*
     * If there is only one layer, don't need to go to the next layer.
     */
    if ( _bulkProcess.total_layers <= 1 )  {

      _bulkProcess.process_is_on = FALSE;
      return;
    }

    /*
     * Advance to the next layer, but skip the layer with
     * the layer name "GEN-TSTM" and "PR2CAT".    
     */
    do {

      _bulkProcess.curr_layer 
           = (_bulkProcess.curr_layer+1) % _bulkProcess.total_layers;
      if ( _bulkProcess.curr_layer == _bulkProcess.start_layer )  {

        _bulkProcess.process_is_on = FALSE;
        return;
      }
      layer_name = pglayer_getName (_bulkProcess.curr_layer);

    } while ( strcasecmp(layer_name, "GEN-TSTM") == 0 ||
              strcasecmp(layer_name, "PR2CAT") == 0 );


    /*
     * Switch to the next layer. It is like clicking on the next layer
     * name button on the Layer Window.
     */
    pglayrw_setActvLayer( _bulkProcess.curr_layer, &ier );

    /*
     * Pop up "Format Outlook" window by clicking on "OUT LOOK" object.
     */
    pgpalw_objCb ( pgpalw_getObjWid(OBJ_OUTLOOK), NULL, NULL );

    /*
     * Check the group type name of the current layer, there are
     * special processings for "ENH20", "ENH00", 
     * "ENH04", "ENH12, "ENH16" 
     */
    if ( _dayOpt == 0 || _dayOpt == 4 || _dayOpt == 5 || _dayOpt == 6 || 
	 _dayOpt == 7 || _dayOpt == 8 ) {

      grpid = pglayer_getDefGrp( _bulkProcess.curr_layer );
      ces_gtgnam( grpid, grpname, &ier );

      if ( strcasecmp(grpname, "ENH00") == 0 ) {

        if ( _dayOpt != 4 ) {
	  _dayOpt = 4;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }
      else if ( strcasecmp(grpname, "ENH04") == 0 ) {

        if ( _dayOpt != 5 ) {
	  _dayOpt = 5;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }
      else if ( strcasecmp(grpname, "ENH12") == 0 ) {

        if ( _dayOpt != 6 ) {
	  _dayOpt = 6;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }
      else if ( strcasecmp(grpname, "ENH16") == 0 ) {

        if ( _dayOpt != 7 ) {
	  _dayOpt = 7;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }
      else if ( strcasecmp(grpname, "ENH20") == 0 ) {

        if ( _dayOpt != 8 ) {
	  _dayOpt = 8;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }
      else {

        if ( _dayOpt != 0 ) {
	  _dayOpt = 0;
          for ( ii = 0; ii < _dayOptNum; ii++ ) 
	       XmToggleButtonSetState (_dayW[ii], (ii==_dayOpt), FALSE);
          pgofmt_dayChoiceOptCb (NULL, (long)_dayOpt, NULL);        
        }
      }

    } /* the end of if < _dayOpt == 0... */

    /*
     * Pop up "Confirm" window by clicking on "Otlk All" button.
     */
    pgofmt_ctlBtnCb ( NULL, 1, NULL );

}

/*=====================================================================*/

void pgofmt_bulkProcessEnd ( void ) 
/************************************************************************
 * pgofmt_bulkProcessEnd						*
 *									*
 * This function ends the bulk outlook message processing associated	*
 * with "Otlk All" button.						*
 *									*
 * void pgofmt_bulkProcessEnd ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		04/07	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _bulkProcess.process_is_on = FALSE;
}

/*=====================================================================*/



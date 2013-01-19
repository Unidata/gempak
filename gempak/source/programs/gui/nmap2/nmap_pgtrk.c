#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define EOL             "\n"
#define DTG_YEAR        0
#define DTG_MONTH       1
#define DTG_DAY         2
#define DTG_CYCLE       3
#define TEXT_FIELD	-1
#define NOTP		7

struct dtgW {
    Widget      year;
    Widget      month;
    Widget      day;
    Widget      cycle;
};

static struct dtgW      _dateTime;

static Widget   _mainWin;
static Widget   _strmIdW;
static Widget	_oceanRb[4];
static Widget	_centerRb[3];

WidgetList btnw;

static int      _currYear;
static int      _currMonth;
static int      _currDay;
static int      _currHour;
static int 	_cycle;
static int	_currOcean = 0;
static int	_currCenter = 0;
static char	_currId[4] = { "01" };

static char	*_oceanOpt[] = {"AL", "WP", "CP", "EP"};
static char	*_centerOpt[] = {"HPC", "OPC", "TPC"};


/*
 *  private callback functions
 */
void    pgtrk_ctlBtnCb ( Widget, long, XtPointer );
void    pgtrk_dtgTxtCb ( Widget, long, XtPointer );
void    pgtrk_getStrmInfo ( char *idnum, char *ocean, int *iret );
void    pgtrk_optCenterCb ( Widget, long, XtPointer );
void    pgtrk_optOceanCb ( Widget, long, XtPointer );
void    pgtrk_strmIdCb ( Widget, XtPointer, XtPointer );


/************************************************************************
 * nmap_pgtrk.c                                                         *
 *                                                                      *
 * This module creates the hurricane track product.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *	pgtrk_create()	      create the hurricane track pop-up window	*
 *      pgtrk_update()	      create hurricane track control		*
 *	pgtrk_popup()	      manage the Hurricane track popup		*
 *	pgtrk_isUp()	      querry whether the window is up		*
 *	pgtrk_popdown()	      unmanage the Hurricane track popup	*
 *   	pgtrk_getfname()      build the hurricane track text file name	*
 *	pgtrk_getStrmInfo()     get the storm Id from the VG file name	*
 *									*
 *	pgtrk_ctlBtnCb()      callback for the control buttons		*
 *	pgtrk_dtgTxtCb()      callback for the date/cycle		*
 *	pgtrk_optOceanCb()    callback for the ocaen option buttons	*
 * 	pgtrk_optCenterCb()   callback for the center option buttons	*
 *	pgtrk_strmIdCb()      callback for the storm Id input		*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgtrk_create ( Widget parent )
/************************************************************************
 * pgtrk_create                                                         *
 *                                                                      *
 * This function creates the hurricane track pop-up window.             *
 *                                                                      *
 * void pgtrk_create (parent)                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget  parent widget                           *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		04/00	initial coding                          *
 * M. Li/GSC		04/00	hour -> cycle				*
 * J. Wu/SAIC		05/02	verify input to storm ID and date	*
 * T. Lee/SAIC		04/03	Changed MPC to OPC			*      
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int         txsize = 2;
    long	ii, nn;
    char        *btnstrs[] = {"Continue", "Cancel"};
    char	id_text[3];

    Widget      pane, rowrc, colrc;
    Widget	form, rca, form1, form2, label1, label2, rc1, rc2;
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _mainWin = XmCreateFormDialog (parent, "pgtrk_popup", NULL, 0);
    XtVaSetValues(_mainWin,
                  XmNnoResize,        True,
                  XmNdefaultPosition, False,
                  NULL);
    XtVaSetValues(XtParent(_mainWin),
                  XmNtitle, "FORMAT HURRICANE TRACK",
                  NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgtrk_pane",
                            xmPanedWindowWidgetClass, _mainWin,
                            XmNsashWidth,             1,
                            XmNsashHeight,            1,
                            NULL);
 

    /*
     * Storm ID
     */
    sprintf(id_text, "%s", _currId);
    NxmTxtIn_create(pane, "Storm Id", txsize,
                                     &_strmIdW);

    XtVaSetValues (_strmIdW, XmNvalue, id_text, NULL);    

    XtAddCallback(_strmIdW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback ( _strmIdW, XmNactivateCallback,
		    (XtCallbackProc)pgtrk_strmIdCb, (XtPointer) NULL);
    XtAddCallback ( _strmIdW, XmNlosingFocusCallback,
		    (XtCallbackProc)pgtrk_strmIdCb, (XtPointer) NULL);
 

    /*
     * create a form to hold the radio buttons.
     */
    form = XtVaCreateWidget ( "form",
				xmFormWidgetClass,  pane,
				NULL );

    rca = XtVaCreateWidget ( "opt_col",
				xmRowColumnWidgetClass, form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmVERTICAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );
    /*
     * Ocean
     */
    form1 = XtVaCreateWidget ( "form1",
				 xmFormWidgetClass, rca,
				 NULL );

    label1 = XtVaCreateManagedWidget ( "Ocean",
				xmLabelWidgetClass, form1,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       form1,
				XmNmarginHeight,    7,
				NULL );

    rc1 = XtVaCreateManagedWidget ( "ocean_rowcol",
				xmRowColumnWidgetClass, form1,
				XmNradioBehavior,       TRUE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          label1,
				NULL );

    nn = XtNumber ( _oceanOpt );
    for ( ii = 0; ii < nn; ii++ )  {
	_oceanRb[ii] = XtVaCreateManagedWidget ( _oceanOpt[ii],
			    xmToggleButtonWidgetClass, rc1,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currOcean == ii),
			    XmNuserData,               &_currOcean,
			    NULL );

	XtAddCallback ( _oceanRb[ii], XmNarmCallback,
			(XtCallbackProc)pgtrk_optOceanCb, (XtPointer) ii);


    }

    /*
     * Center
     */
    form2 = XtVaCreateWidget ( "form2",
				 xmFormWidgetClass, rca,
				 NULL );

    label2 = XtVaCreateManagedWidget ( "Center",
				xmLabelWidgetClass, form2,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       form2,
				XmNmarginHeight,    7,
				NULL );

    rc2 = XtVaCreateManagedWidget ( "Center_rowcol",
				xmRowColumnWidgetClass, form2,
				XmNradioBehavior,       TRUE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          label2,
				NULL );

    nn = XtNumber ( _centerOpt );
    for ( ii = 0; ii < nn; ii++ )  {
	_centerRb[ii] = XtVaCreateManagedWidget ( _centerOpt[ii],
			    xmToggleButtonWidgetClass, rc2,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currCenter == ii),
			    XmNuserData,               &_currCenter,
			    NULL );

	XtAddCallback ( _centerRb[ii], XmNarmCallback,
			(XtCallbackProc)pgtrk_optCenterCb, (XtPointer) ii);

    }


    /*
     *  date and time
     */
    rowrc = XtVaCreateWidget ("rowrc",
                               xmRowColumnWidgetClass,  pane,
                               XmNorientation,          XmHORIZONTAL,
                               XmNpacking,              XmPACK_TIGHT,
                               NULL);

    /*
     * month
     */
    colrc = XtVaCreateWidget ("timemonth",
                            xmRowColumnWidgetClass,     rowrc,
                            XmNorientation,             XmVERTICAL,
                            XmNpacking,                 XmPACK_COLUMN,
                            XmNorientation,             XmVERTICAL,
                            XmNpacking,                 XmPACK_COLUMN,
                            NULL);

    XtVaCreateManagedWidget ("MM",
                             xmLabelWidgetClass,        colrc,
                             NULL, 0);

    _dateTime.month =
        XtVaCreateManagedWidget("month", xmTextWidgetClass, colrc,
                                XmNcolumns, 2, NULL);

    XtAddCallback(_dateTime.month, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_dateTime.month, XmNactivateCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_MONTH);
    XtAddCallback (_dateTime.month, XmNlosingFocusCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_MONTH);

    XtManageChild (colrc);


    /*
     * day
     */
    colrc = XtVaCreateWidget ("timeday",
                            xmRowColumnWidgetClass,     rowrc,
                            XmNorientation,             XmVERTICAL,
                            XmNpacking,                 XmPACK_COLUMN,
                            NULL);

    XtVaCreateManagedWidget ("DD",
                             xmLabelWidgetClass,        colrc,
                             NULL, 0);

    _dateTime.day =
        XtVaCreateManagedWidget("day",   xmTextWidgetClass, colrc,
                                XmNcolumns, 2, NULL);

    XtAddCallback(_dateTime.day, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_dateTime.day, XmNactivateCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_DAY);
    XtAddCallback (_dateTime.day, XmNlosingFocusCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_DAY);

    XtManageChild (colrc);


    /*
     * year
     */
    colrc = XtVaCreateWidget ("timeyear",
                            xmRowColumnWidgetClass,     rowrc,
                            XmNorientation,             XmVERTICAL,
                            XmNpacking,                 XmPACK_COLUMN,
                            NULL);

    XtVaCreateManagedWidget ("YYYY",
                             xmLabelWidgetClass,        colrc,
                             NULL, 0);

    _dateTime.year =
        XtVaCreateManagedWidget("year",  xmTextWidgetClass, colrc,
                                XmNcolumns, 4, NULL);

    XtAddCallback(_dateTime.year, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_dateTime.year, XmNactivateCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_YEAR);
    XtAddCallback (_dateTime.year, XmNlosingFocusCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_YEAR);

    XtManageChild (colrc);


    /*
     * Cycle
     */
    colrc = XtVaCreateWidget ("Cycle",
                            xmRowColumnWidgetClass,     rowrc,
                            XmNorientation,             XmVERTICAL,
                            XmNpacking,                 XmPACK_COLUMN,
                            NULL);

    XtVaCreateManagedWidget ("CC",
                             xmLabelWidgetClass,        colrc,
                             NULL, 0);

    _dateTime.cycle =
        XtVaCreateManagedWidget("cycle", xmTextWidgetClass, colrc,
                                XmNcolumns, 2, NULL);

    XtAddCallback(_dateTime.cycle, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_dateTime.cycle, XmNactivateCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_CYCLE);
    XtAddCallback (_dateTime.cycle, XmNlosingFocusCallback,
                   (XtCallbackProc)pgtrk_dtgTxtCb, (XtPointer) DTG_CYCLE);

    XtManageChild (colrc);

    XtManageChild (rowrc);


    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    btnw = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgtrk_ctlBtn", nn,
                     btnstrs, (XtCallbackProc)pgtrk_ctlBtnCb, btnw);
    
    XtManageChild(form1);
    XtManageChild(form2);
    XtManageChild(rca);
    XtManageChild(form);
    XtManageChild(pane);
}
/*=====================================================================*/

void pgtrk_popup ( void )
/************************************************************************
 * pgtrk_popup                                                          *
 *                                                                      *
 * This function manages the hurricane track window.                    *
 *                                                                      *
 * void pgtrk_popup ()                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC           04/00    Copied from pgccfp_popup                *
 * M. Li/GSC	       04/00    hour -> cycle				*
 * A. Hardy/GSC         5/00	Changed cycle algorithm			*
 * M. Li/GSC		5/00	Added pgtrk_getStrmId			*
 * D.W.Plummer/NCEP	6/01	Chg call to pgtrk_getStrmInfo		*
 * A. Hardy/NCEP	6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{
    int         imin, isec, julian, iret, itype = 1;
    char        dtg[10], ocean[4], tmzn[4];
/*---------------------------------------------------------------------*/

    css_date (&itype, &_currYear, &_currMonth, &_currDay, &_currHour,
              &imin, &isec, &julian, tmzn, &iret);

    sprintf (dtg, "%4.4d", _currYear);
    XmTextSetString (_dateTime.year, dtg);

    sprintf (dtg, "%2.2d", _currMonth);
    XmTextSetString (_dateTime.month, dtg);

    sprintf (dtg, "%2.2d", _currDay);
    XmTextSetString (_dateTime.day, dtg);

    if (_currHour >= 5 && _currHour < 11 )
	_cycle = 6;
    else if (_currHour >= 11 && _currHour < 17)
	_cycle = 12;
    else if (_currHour >= 17 && _currHour < 23)
	_cycle = 18;
    else  
  	_cycle = 0;

    sprintf (dtg, "%2.2d", _cycle);
    XmTextSetString (_dateTime.cycle, dtg);

    pgtrk_getStrmInfo(_currId, ocean, &iret);
    XmTextSetString(_strmIdW, _currId);

    /*
     *  Nice to add here toggle button set for ocean...
     */

    XtManageChild (_mainWin);
}

/*=====================================================================*/
Boolean pgtrk_isUp ( void )
/************************************************************************
 * pgtrk_isUp                                                          	*
 *                                                                      *
 * This function returns a boolean value specifying whether the track   *
 * dialog is managed or not.                                            *
 *                                                                      *
 * Boolean pgtrk_isUp ()                                               	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pgtrk_isUp  Boolean         True -- up,     False -- down           	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC   	05/02						*
 ***********************************************************************/
{

    return (XtIsManaged(_mainWin));

}

/*=====================================================================*/

void pgtrk_popdown ( void )
/************************************************************************
 * pgtrk_popdown                                                        *
 *                                                                      *
 * This function unmanages the Hurricane track popup.                   *
 *                                                                      *
 * void pgtrk_popdown ()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC           04/00   initial coding                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtUnmanageChild (_mainWin);
}

/*=====================================================================*/

void pgtrk_update ( char *fname, int *iret )
/************************************************************************
 * pgtrk_update                                                    	*
 *                                                                      *
 * This function creates a hurricane track text product from a vgf file *
 *                                                                      *
 * void pgtrk_update( fname, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname		char	VG Filename				*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int	Return code				*
 *				-1 = unable to convert			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 8/98						*
 * D.W.Plummer/NCEP	 9/98	Bug fix - check length of teststr array	*
 * D.W.Plummer/NCEP	 5/99	Adjust format of text product		*
 * S. Jacobs/NCEP	 5/99	Changed to always output 5 latlon pairs	*
 * M. Li/GSC		 4/00	Added header and tailing info.		*
 * A. Hardy/GSC          5/00   Added MPC header information            *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * M. Li/GSC		05/01	Assigned output for XmTextGetString     *
 * F. J. Yen/NCEP	 5/01	Updated for new format.			*
 * D.W.Plummer/NCEP	 6/01	Omit output for nonexistent points	*
 * J. Wu/SAIC		01/02	update the current layer only		*
 * E. Safford/SAIC	04/02	add check on fpos			*
 * T. Lee/SAIC		04/03	extend track to 120 hrs			*
 * T. Piper/SAIC	02/04	removed unused variable textstr		*
 ***********************************************************************/
{
int    		ier, nel, fpos, cur_layer, el_layer;
int    		grpnum, grpid, grpnumspt, grpnumsym, outyr;
char		grptyp, vg_class, vg_type;

VG_DBStruct     el;

char            tempstr1[39], tempstr2[15], tempstr3[12];
char		*str, tempstr[132];
char		tau[NOTP][4] = { "12", "24", "36", "48", "72", 
		"96", "120"};

int		done, n, np;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Clear the display area.
     */
    pgprd_clear();

    /*
     * Get basin and annual cyclone number
     */
    str = XmTextGetString(_strmIdW);
    sscanf(str, "%s", _currId);
    XtFree(str);
    sprintf(tempstr1, "%s, %s, ", _oceanOpt[_currOcean], _currId);

    /*
     * YYYYMMDDCC
     */
    if (_currYear >=1900)
        outyr = _currYear;

    sprintf(tempstr2, "%04d%02d%02d%02d, ", outyr, _currMonth, _currDay, _cycle);

    /*
     * Which center
     */
    if ( strcmp ( _centerOpt[_currCenter], "OPC") == 0 ) {
        sprintf(tempstr3, "40, O%s,", _centerOpt[_currCenter]);
    }
    else {
        sprintf(tempstr3, "05, O%s,", _centerOpt[_currCenter]);
    }
    strcat ( tempstr1, tempstr2 );
    strcat ( tempstr1, tempstr3 );

    /*
     *  Process all simple lines (convert longitudes to positive west)
     */

    ces_gtgid( "TROPICL", &grpid, &ier);
    cur_layer = pglayer_getCurLayer ();
    
    done = G_FALSE;
    for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {

	if ( !done )  {

	    crg_gtyp( nel, &vg_class, &vg_type, &ier );
	    crg_ggrp( nel, &grptyp, &grpnum, &ier );
	    crg_goffset( nel, &fpos, &ier);
	    if ( fpos < 0 ) continue;

            el_layer = crg_getLayer( fpos );
	    
	    if ( el_layer == cur_layer && (int)vg_type == LINE_ELM && 
		 (int)grptyp  != 0 && fpos > 0 )  {

	        cvg_rdrec( fname, fpos, &el, &ier );

	        np = el.elem.lin.info.numpts;
	        for ( n = 0; n < (int)G_MIN(np,(int)(sizeof(tau)/sizeof(tau[0]))); n++ )  {
		    pgprd_putstr(tempstr1, &ier);
		    sprintf(tempstr, "%4.4s,%4dN, %4dW,   0\n", 
			    tau[n],
		            G_NINT( 10.0F*el.elem.lin.latlon[n]),
		            G_NINT(-10.0F*el.elem.lin.latlon[n+np]));
	            pgprd_putstr ( tempstr, &ier );
	        }

	        done = G_TRUE;

	    }

	}

    }

    if ( !done )  {

      for ( n = 0; n < NOTP; n++ )  {

	for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {

	  crg_gtyp( nel, &vg_class, &vg_type, &ier );
          crg_ggrp( nel, &grptyp, &grpnumspt, &ier );
          crg_goffset( nel, &fpos, &ier);
	  if ( fpos < 0 ) continue;

	  el_layer = crg_getLayer( fpos );
	  
	  if ( el_layer == cur_layer &&
	       (int)vg_type == SPTX_ELM && (int)grptyp == grpid )  {

	    cvg_rdrec( fname, fpos, &el, &ier );
	    if ( strcmp(el.elem.spt.text,tau[n]) == 0 )  {

	      for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {

		crg_gtyp( nel, &vg_class, &vg_type, &ier );
                crg_ggrp( nel, &grptyp, &grpnumsym, &ier );
                crg_goffset( nel, &fpos, &ier);
	        if ( fpos < 0 ) continue;

		if ( (int)vg_type == SPSYM_ELM && (int)grptyp == grpid && 
			grpnumsym == grpnumspt )  {

	          cvg_rdrec( fname, fpos, &el, &ier );
		  pgprd_putstr(tempstr1, &ier);
		  sprintf(tempstr, "%4.4s,%4dN, %4dW,   0\n",
                          tau[n],
                          G_NINT( 10.0F*el.elem.sym.data.latlon[0]),
                          G_NINT(-10.0F*el.elem.sym.data.latlon[1]));
                  pgprd_putstr ( tempstr, &ier );

		}
	      }
	    }
	  }
	}
      }
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgtrk_dtgTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtrk_dtgTxtCb							*
 *									*
 * Callback function for the date/cycle.				*
 *									*
 * void pgtrk_dtgTxtCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which text				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * M. Li/GSC		04/00	Copied from pgccfp_dtgTxtCb		*
 * M. Li/GSC		04/00	hour -> cycle				*
 ***********************************************************************/
{
    int 	inew;
    char	*cnew, redo[10];
/*---------------------------------------------------------------------*/

    switch (which) {
      case DTG_YEAR:
	cnew = XmTextGetString (_dateTime.year);
	sscanf (cnew, "%d", &inew);
	XtFree (cnew);

	if (1000 <= inew && inew < 10000) {
	    _currYear = inew;
	}

	sprintf (redo, "%4.4d", _currYear);
	XmTextSetString (_dateTime.year, redo);

	break;

      case DTG_MONTH:
	cnew = XmTextGetString (_dateTime.month);
	sscanf (cnew, "%d", &inew);
	XtFree (cnew);

	if (1 <= inew && inew <= 12) {
	    _currMonth = inew;
	}

	sprintf (redo, "%2.2d", _currMonth);
	XmTextSetString (_dateTime.month, redo);

	break;

      case DTG_DAY:
	cnew = XmTextGetString (_dateTime.day);
	sscanf (cnew, "%d", &inew);
	XtFree (cnew);

	if (1 <= inew && inew <= 31) {
	    _currDay = inew;
	}

	sprintf (redo, "%2.2d", _currDay);
	XmTextSetString (_dateTime.day, redo);

	break;

      case DTG_CYCLE:
	cnew = XmTextGetString (_dateTime.cycle);
	sscanf (cnew, "%d", &inew);
	XtFree (cnew);

	if (0 <= inew && inew < 24) {
	    _cycle = inew;
	}  

	sprintf (redo, "%2.2d", _cycle);
	XmTextSetString (_dateTime.cycle, redo);

	break;

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgtrk_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtrk_ctlBtnCb							*
 *									*
 * Callback function for control buttons.				*
 *									*
 * void pgtrk_ctlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * M. Li/GSC		04/00	initial coding				*
 ***********************************************************************/
{
    char        mesg[128];
    int		id;
/*---------------------------------------------------------------------*/

    switch (which) {
      case 0:	/* Continue */
	sscanf ( _currId, "%d", &id );
        if(id <= 0 ) {
            sprintf(mesg, 
                   "invalid storm Id"); 
	    NxmWarn_show(_mainWin, mesg);        
        }
 	else {
 	    pgtrk_popdown();
            pgprd_popup (); 
        }

	break;

      case 1:	/* Cancel */
	pgtrk_popdown ();
	pgprd_popdown ();

	break;
    }
}

/*=====================================================================*/

void pgtrk_getfname ( char *text, char *fname, int *iret )
/************************************************************************
 * pgtrk_getfname                                                  	*
 *                                                                      *
 * Build filename from hurricane track text string.			*
 *                                                                      *
 * void pgtrk_getfname(text, fname, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *text	char	Outlook text string.				*
 *                                                                      *
 * Output parameters:                                                   *
 *  *fname	char	Filename					*
 *  *iret	int	Return Code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 8/98						*
 * M. Li/GSC	 	 4/00	get 'vgfname.track'			*
 * S. Law/GSC		05/00	pgfilw_qFilename -> pgfilw_getFileName	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    char   fnm[MXFLSZ], prefix[30];
    int	   fnlen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    pgfilw_getFileName (FALSE, fnm);

    fnlen = (int)strlen (fnm);
    if (fnlen == 0 ||
        strncmp (fnm, cvg_getworkfile(), strlen (cvg_getworkfile())) == 0) {
        strcpy (fname, "hurr.track");
    }
    else {
        cst_ncpy (prefix, fnm, fnlen-4, iret);
        sprintf (fname, "%s.track", prefix);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgtrk_optOceanCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgtrk_optOceanCb                                                     *
 *                                                                      *
 * Callback function for the ocean radio buttons.			*
 *                                                                      *
 * void  pgtrk_optOceanCb (wid, which, cbs)	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		04/00	Initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	_currOcean = (int)which;

}

/*=====================================================================*/
/* ARGSUSED */
void pgtrk_optCenterCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgtrk_optCenterCb                                                    *
 *                                                                      *
 * Callback function for the Center radio buttons.			*
 *                                                                      *
 * void  pgtrk_optCenterCb (wid, which, cbs)		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		04/00	Initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	_currCenter = (int)which;

}

/*=====================================================================*/
/* ARGSUSED */
void pgtrk_strmIdCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgtrk_strmIdCb                                	                *
 *                                                                      *
 * Callback function for the storm Id input.	                        *
 *                                                                      *
 * void pgtrk_strmIdCb (wid, clnt, cbs ) 		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt	        int	        not used                        *
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC	04/00	Initial coding					*
 ***********************************************************************/
{
    int		idnum;
    char	*idstr, newstr[3];
/*---------------------------------------------------------------------*/

    idstr = XmTextGetString (_strmIdW);

    sscanf (idstr, "%d", &idnum);

    XtFree (idstr);

    if ( idnum > 0 && idnum < 100) {
	sprintf ( _currId, "%02d", idnum );
    }
    else {
	strcpy ( _currId, "01" );	  
    }

    sprintf (newstr, "%s", _currId);
    XmTextSetString (_strmIdW, newstr);
 
}

/*=====================================================================*/

void pgtrk_getStrmInfo ( char *idnum, char *ocean, int *iret )
/************************************************************************
 * pgtrk_getStrmInfo                                                  	*
 *                                                                      *
 * This function gets the storm Id and ocean basin from the vgf name.	*
 *                                                                      *
 * void pgtrk_getStrmInfo(idnum, ocean, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *  *idnum	char	storm Id					*
 *  *ocean	char	ocean basin					*
 *  *iret	int	Return Code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC	 	 5/00	Create					*
 * M. Li/GSC		 5/00	idtxt[3] -> idtxt[2]			*
 * D.W.Plummer/NCEP	 6/01	pgtrk_getStrmId to pgtrk_getStrmInfo	*
 * 				add ocean basin				*
 * J. Wu/SAIC	 	 05/02	avoid undefined idnum/ocean 		*
 ***********************************************************************/
{
    int	   len;
    char   fnm[MXFLSZ];
/*---------------------------------------------------------------------*/

    *iret = 0;

    pgfilw_getFileName (FALSE, fnm);
    len = (int)strlen ( fnm );

    strcpy ( idnum, "01" );
    strcpy ( ocean, "" );

    if  ( len > 0 ) {
    strncpy ( ocean, &(fnm[0]), 2 );
    ocean[2] = '\0';
    }

    if ( len > 2 ) {
	strncpy ( idnum, &(fnm[2]), 2 );
        idnum[2] = '\0';
    }
    
}

/*=====================================================================*/

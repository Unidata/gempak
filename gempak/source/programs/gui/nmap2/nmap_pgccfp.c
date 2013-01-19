#include "geminc.h"
#include "Nxm.h"
#include "gemprm.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "vgstruct.h"
#include "drwids.h"

#define	EOL		"\n"
#define	CCFP_TBL	"ccfp.tbl"
#define DIR_TOWARD	TRUE

static	int			_nIssue;
static	char			_issue[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_issueStrc;

static	int			_nValid[MAXNOPT];
static	char			_valid[MAXNOPT][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_validStrc;

static Widget	_mainWin;


/* 
 *  private callback functions 
 */
void pgccfp_ctlBtnCb ( Widget, long, XtPointer );
void pgccfp_timeCb (   Widget, long, XtPointer );

/*
 * private functions -- action
 */
void pgccfp_readTable ( int *iret );


/************************************************************************
 * nmap_pgccfp.c							*
 *									*
 * This module provides the Collective Convection Forecast (CCF)	*
 * Product.								*
 *									*
 * CONTENTS:								*
 *	pgccfp_create()		creates the date popup			*
 *	pgccfp_popup()		manages the date popup			*
 *	pgccfp_popdown()	unmanages the date popup		*
 *	pgccfp_update()		creates the CCF product message		*
 *	pgccfp_isUp()		returns the current window status	* 
 *									*
 *	pgccfp_ctlBtnCb()	callback for the control buttons	*
 *	pgccfp_timeCb()		callback for the time pulldown menu	*
 *									*
 *	pgccfp_readTable()	reads the CCFP table			*
 ***********************************************************************/

/*=====================================================================*/

void pgccfp_create ( Widget parent )
/************************************************************************
 * pgccfp_create							*
 *									*
 * This function creates the date popup window.				*
 *									*
 * void pgccfp_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	initial coding				*
 * S. Law/GSC		04/00	changed to use issue/valid times	*
 * E. Safford/GSC	11/00	add cast to pgutls_createOptionMenu call*
 * T. Piper/GSC		07/01	freed btnw				*
 ***********************************************************************/
{
    int		nn, ier;
    char	*btnstrs[] = {"Continue", "Cancel"};
    Widget	pane, form;
    WidgetList	btnw;
/*---------------------------------------------------------------------*/

    pgccfp_readTable (&ier);

    /*
     * create dialog shell
     */
    _mainWin = XmCreateFormDialog (parent, "pgccfp_popup", NULL, 0);
    XtVaSetValues(_mainWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_mainWin),
		  XmNtitle, "CCFP",
		  NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgccfp_pane",
			    xmPanedWindowWidgetClass, _mainWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);


    /*
     *  initial and valid date and time menus
     */
    form = (Widget) XtVaCreateManagedWidget ("pgccfp_timeform",
					     xmFormWidgetClass,	 pane,
					     NULL);
    _issueStrc.current = 0;
    pgutls_createOptionMenu (form, MAXNOPT, (XtPointer)&_issueStrc.current, 
			     "Issue Time:", (XtCallbackProc)&pgccfp_timeCb, &_issueStrc.form, 
			     &_issueStrc.label, &_issueStrc.menu, 
			     _issueStrc.pb, NULL);
 
    XtVaSetValues (_issueStrc.form, XmNleftAttachment, XmATTACH_FORM, NULL);

    pgutls_setOptMenu ("", _issue, _nIssue, &_issueStrc);

    _validStrc.current = 0;
    pgutls_createOptionMenu (form, MAXNOPT, (XtPointer)&_validStrc.current, 
			     "Valid Time:", NULL, &_validStrc.form, 
			     &_validStrc.label, &_validStrc.menu, 
			     _validStrc.pb, NULL);
 
    XtVaSetValues (_validStrc.form, 
		   XmNtopAttachment,	XmATTACH_WIDGET, 
		   XmNtopWidget,	_issueStrc.form,
		   NULL);

    pgutls_setOptMenu ("", _valid[0], _nValid[0], &_validStrc);



    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    btnw = (WidgetList)XtMalloc(nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgccfp_ctlBtn", nn, btnstrs, 
			(XtCallbackProc)pgccfp_ctlBtnCb, btnw);
    XtFree((XtPointer)btnw);
    XtManageChild(pane);
}

/*=====================================================================*/

void pgccfp_popup ( void )
/************************************************************************
 * pgccfp_popup								*
 *									*
 * This function manages the date popup.				*
 *									*
 * void pgccfp_popup ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 * S. Law/GSC		04/00	removed date/time information		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtManageChild (_mainWin);
}

/*=====================================================================*/

void pgccfp_popdown ( void )
/************************************************************************
 * pgccfp_popdown							*
 *									*
 * This function unmanages the date popup.				*
 *									*
 * void pgccfp_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtUnmanageChild (_mainWin);
}

/*=====================================================================*/

void pgccfp_update ( char *fname, int *iret )
/************************************************************************
 * pgccfp_update							*
 *									*
 * This function creates the CCF product message.			*
 *									*
 * void pgccfp_update (fname, iret)					*
 *									*
 * Input parameters:							*
 *	*fname		char	VG Filename				*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 * S. Law/GSC		04/00	changed to use issue/valid times	*
 * D.W.Plummer/NCEP	 4/00	changes to format of this text product	*
 * S. Law/GSC		04/00	added DIR_TOWARD check			*
 * S. Law/GSC		05/00	simplified format			*
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 * J. Wu/SAIC		01/02	update the current layer only		*
 * L. Hinson/AWC        07/09   Add support for High/Med Line Coverage  *
 *                              For Lines write out Cov. value of 1 or 2*
 ***********************************************************************/
{
#define	STRLEN	256
    int         ii, jj, fpos, np, found, ier, cur_layer, el_layer;
    int         na, nl, fpa[50], fpl[50], grptyp, grpnum;
    char        class, type;
    char        textstr[STRLEN], tmpstr[STRLEN];
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Clear the display area.
     */
    pgprd_clear();

    /*
     *  Display preliminary information
     */
    sprintf (textstr, "CCFP %s %s", _issue[_issueStrc.current],
	     _valid[_issueStrc.current][_validStrc.current]); 
    strcat ( textstr, EOL ); pgprd_putstr (textstr, &ier);

    /*
     *  Determine number of areas and lines; save their file locations
     */
    na = 0;
    nl = 0;
    cur_layer = pglayer_getCurLayer ();    
    for (ii = 0; ii < MAX_EDITABLE_ELEMS; ii++)  {

        crg_gtyp (ii, &class, &type, &ier);

        if ((int) class == CLASS_SIGMETS && (int)type == SIGCCF_ELM && 
            !(crg_isauxrec(ii, &ier)))  {

            crg_goffset (ii, &fpos, &ier);
	    el_layer = crg_getLayer( fpos );
            
	    if ( el_layer == cur_layer && fpos > 0 )  {
                cvg_rdrec ( fname, fpos, &el, &ier );
                if ( el.elem.ccf.info.subtype == SIGTYP_AREA )  {
                    fpa[na] = fpos;
                    na++;
                }
                if ( el.elem.ccf.info.subtype == SIGTYP_LINE_HIGH ||
                     el.elem.ccf.info.subtype == SIGTYP_LINE_MED )  {
                    fpl[nl] = fpos;
                    nl++;
                }
            }
        }
    }

    /*
     *  Process areas
     */
    for (ii = 0; ii < na; ii++) {

        cvg_rdrec ( fname, fpa[ii], &el, &ier );

	np = el.elem.ccf.info.npts;
        sprintf (textstr, "AREA %d %d %d %d %d %d %d ", 
		 el.elem.ccf.info.cover, el.elem.ccf.info.prob, 
		 el.elem.ccf.info.growth, el.elem.ccf.info.tops,
		 G_NINT(el.elem.ccf.info.spd), G_NINT(el.elem.ccf.info.dir),
		 (np + 1));

        for (jj = 0; jj < np; jj++) {
            sprintf (tmpstr, "%d %d ",
		      (int) (el.elem.ccf.latlon[jj] * 10.0F),
		      -(int) (el.elem.ccf.latlon[np + jj] * 10.0F));
            if ((strlen (textstr) + strlen (tmpstr) + 2) < (size_t)STRLEN) {
		strcat (textstr, tmpstr);
	    }
        }

        /*
         *  for areas, repeat last point
         */
        sprintf (tmpstr, "%d %d ",
		 (int) (el.elem.ccf.latlon[0] * 10.0F),
		 -(int) (el.elem.ccf.latlon[np] * 10.0F) );
        if ((strlen(textstr)+strlen(tmpstr) + 2) < (size_t)STRLEN) {
            strcat (textstr, tmpstr);
	}

        grptyp = el.hdr.grptyp;
        grpnum = el.hdr.grpnum;

        jj = 0;
        found = G_FALSE;
        while ( jj < MAX_EDITABLE_ELEMS  &&  !found )  {

            crg_gtyp ( jj, &class, &type, &ier );

            if ( (int)class == CLASS_TEXT && (int)type == SPTX_ELM )  {

                crg_goffset ( jj, &fpos, &ier );
                if ( fpos > 0 )  {
                    cvg_rdrec ( fname, fpos, &el, &ier );
                    if ( ( el.hdr.grptyp == grptyp )  &&
                      ( el.hdr.grpnum == grpnum )  &&
                      ( strstr( el.elem.spt.text, "Hght" ) != (char *)NULL ) ) {

                        sprintf ( tmpstr, "%d %d ",
                            (int)(el.elem.spt.info.lat*10.0F),
                           -(int)(el.elem.spt.info.lon*10.0F) );
                        strcat ( textstr, tmpstr );
                        found = G_TRUE;

                    }
                }

            }
            jj++;

        }

        strcat ( textstr, EOL ); pgprd_putstr (textstr, &ier);

    }

    /*
     *  Process lines
     */
    for ( ii = 0; ii < nl; ii++ )  {

        cvg_rdrec ( fname, fpl[ii], &el, &ier );

        sprintf ( textstr, "LINE %d ", el.elem.ccf.info.subtype );

        np = el.elem.ccf.info.npts;
        sprintf ( tmpstr, "%d ", np );
        strcat ( textstr, tmpstr );

        for ( jj = 0; jj < np; jj++ )  {
            sprintf ( tmpstr, "%d %d ",
                (int)(el.elem.ccf.latlon[   jj]*10.0F),
               -(int)(el.elem.ccf.latlon[np+jj]*10.0F) );
            if ( strlen(textstr)+strlen(tmpstr)+2 < (size_t)STRLEN )
                strcat ( textstr, tmpstr );
        }

        if ( strlen(textstr)+strlen(EOL)+2 < (size_t)STRLEN )
            strcat ( textstr, EOL ); 
	pgprd_putstr (textstr, &ier);

    }

}

/*=====================================================================*/

Boolean pgccfp_isUp ( void )
/************************************************************************
 * pgccfp_isUp   							*
 *									*
 * This function returns the status of the window.			*
 *									*
 * Boolean pgccfp_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 *	pgccfp_isUp	Boolean	True if the window is presently managed.*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/01	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged( _mainWin ) );
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfp_timeCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgccfp_timeCb							*
 *									*
 * Callback function for the issue time pulldown menu and is used to	*
 * update the valid time pulldown menu.					*
 *									*
 * void pgccfp_timeCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which issue time			*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S.Law/GSC		04/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _issueStrc.current = which;
    _validStrc.current = 0;

    pgutls_setOptMenu ("", _valid[which], _nValid[which], &_validStrc);
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfp_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgccfp_ctlBtnCb							*
 *									*
 * Callback function for control buttons of the date popup.		*
 *									*
 * void pgccfp_ctlBtnCb (wid, which, call)				*
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
 * S.Law/GSC		03/00	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch (which) {
      case 0:	/* Continue */
        pgccfp_popdown();
        pgprd_popup (); 
       
	break;

      case 1:	/* Cancel */
	pgccfp_popdown ();
	pgprd_popdown ();

	break;
    }
}

/*=====================================================================*/

void pgccfp_getfname ( char *fname, int *iret )
/************************************************************************
 * pgccfp_getfname							*
 *									*
 * Return standard filename for CCFP text product.			*
 *									*
 * void pgccfp_getfname (fname, iret)					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *fname	char	Filename					*
 * *iret	int	Return code					*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 * S. Law/GSC		04/00	removed date/time stuff			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    sprintf (fname, "%11s.ccfp", _issue[_issueStrc.current]);
}

/*=====================================================================*/

void pgccfp_readTable ( int *iret )
/************************************************************************
 * pgccfp_readTable							*
 *									*
 * This function loads and converts the information from the CCFP table.*
 *									*
 * void pgccfp_readTable (iret)						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*iret		int	return value (-1 = unable to open file)	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 ***********************************************************************/
{
    int		ier, hour, tarry[5], tarry2[5], aday = 1440;
    char	buff[MAXTBLNAME], fname[32], hourstr[MAXTBLNAME], *pchar;
    struct tm	*utctime;
    time_t	tp;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    strcpy (fname, CCFP_TBL);
    fp = cfl_tbop(fname, "pgen", &ier);
    if (fp == NULL || ier != 0) {
        *iret = -1;
        return;
    }

    /* 
     * set up current times
     */
    tp = time (NULL);
    utctime = gmtime (&tp);

    tarry[0] = utctime->tm_year + 1900;
    tarry[1] = utctime->tm_mon + 1;
    tarry[2] = utctime->tm_mday;
    tarry[3] = utctime->tm_hour;
    tarry[4] = utctime->tm_min;

    /*
     *  Scan table line-by-line.
     */
    _nIssue = 0;
    while (!feof(fp) && _nIssue < MAXNOPT) {

	cfl_trln (fp, sizeof(buff), buff, &ier);

	if (ier == 0) {
	    pchar = (char *) cst_split (buff, ';', MAXTBLNAME, hourstr, &ier);
	    sscanf (hourstr, "%d", &hour);
	    if (hour < 0 || 24 < hour) continue;

	    tarry2[0] = tarry[0];
	    tarry2[1] = tarry[1];
	    tarry2[2] = tarry[2];
	    tarry2[3] = hour;
	    tarry2[4] = tarry[4];

	    /*
	     * if past the current time, assume next day
	     */
	    if ((hour + 1) < tarry[3]) {
		ti_addm (tarry2, &aday, tarry2, &ier);
	    }

	    sprintf (_issue[_nIssue], "%4.4d%2.2d%2.2d_%2.2d00", 
		     tarry2[0], tarry2[1], tarry2[2], tarry2[3]);

	    _nValid[_nIssue] = 0;
	    while (pchar != NULL) {
		pchar = (char *) cst_split (pchar, ';', MAXTBLNAME, 
					    hourstr, &ier);
		sscanf (hourstr, "%d", &hour);
		if (hour < 0 || 24 < hour) continue;

		/*
		 * if before the previous hour, assume next day
		 */
		if (hour < tarry2[3]) {
		    ti_addm (tarry2, &aday, tarry2, &ier);
		}

		tarry2[3] = hour;

		sprintf (_valid[_nIssue][_nValid[_nIssue]], 
			 "%4.4d%2.2d%2.2d_%2.2d00", 
			 tarry2[0], tarry2[1], tarry2[2], tarry2[3]);

		_nValid[_nIssue]++;
	    }

	    if (_nValid[_nIssue] > 0) _nIssue++;
	}
    }

    cfl_clos (fp, &ier);
}

/*=====================================================================*/

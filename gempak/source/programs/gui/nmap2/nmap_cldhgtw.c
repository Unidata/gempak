#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "Nxm.h"


#define CLDHGT_TBL     "nmap_cldhgt.tbl"
#define MAX_DIST	3
#define MAX_TEMP	2
#define MAX_HGHT	2
#define MAX_DATA	2
#define MAX_LEVEL	2
#define MAX_PIXL	2
#define MAX_PVAL	2


static Widget		_cldhgtwWin;
static Widget		_formatForm;

static Widget           _cldhgtLabel, _cldhgtList;
static Widget           _infoTxtW[5];  

static float            _currLat, _currLon;

static Widget		_cldhgtwOpt;

static Widget           _cldhgtTlabel;
static Widget           _cldhgtDlabel;
static Widget           _cldhgtSlabel;

static Widget		_distRb[3];
static Widget		_tempRb[2];
static Widget		_hghtRb[2];
static Widget		_dataRb[2];
static Widget           _dataRb2[2];
static Widget           _levelRb[2];
static Widget		_pixlRb[2];
static Widget		_pvalRb[2];

static Widget           _distForm;
static Widget           _distText;

static Widget           _areaForm;
static Widget           _areaText;

static Widget           _pvalForm;

static Widget		_statusList;

static int		_currDist = 2;
static int		_currTemp = 0;
static int		_currHght = 0;
static int		_currData = 0;
static int              _currData2= 0;
static int              _currLevel= 0;
static float		_currDstm = 1000000.0F;
static int		_currPixl = 1;
static int		_currArea = 10;
static int		_currPval = 0;

static int		_saveDist;
static int		_saveTemp;
static int		_saveHght;
static int		_saveData;
static int              _saveData2;
static float		_saveDstm;
static int              _saveLevel;
static int		_savePixl;
static int		_saveArea;
static int		_savePval;

static int		_defDist = 2;
static int		_defTemp = 0;
static int		_defHght = 0;
static int		_defData = 0;
static int              _defData2= 0;
static float		_defDstm = 1000000.00F;
static int              _defLevel= 0;
static int		_defPixl = 1;
static int		_defArea = 10;
static int		_defPval = 0;

static char		*_dunits[] = { "km", "mi", "nm" };
static char		*_tunits[] = { "C", "K" };
static char		*_hunits[] = { "ft", "m" };

static int		nermsg_save;


/*
 *  private callback functions
 */
void cldhgtw_pointerEh ( Widget, XtPointer, XEvent*, Boolean* );
void cldhgtw_ctlBtnCb  ( Widget, long, XtPointer );
void cldhgtw_optctlBtnCb(Widget, long, XtPointer );
void cldhgtw_optdstkCb ( Widget, XtPointer, XtPointer );
void cldhgtw_optareaCb ( Widget, XtPointer, XtPointer );
void cldhgtw_optdistCb ( Widget, long, XtPointer );
void cldhgtw_opttempCb ( Widget, long, XtPointer );
void cldhgtw_opthghtCb ( Widget, long, XtPointer );
void cldhgtw_optdataCb ( Widget, long, XtPointer );
void cldhgtw_optdata2Cb( Widget, long, XtPointer );
void cldhgtw_optlevelCb( Widget, long, XtPointer );
void cldhgtw_optpixlCb ( Widget, long, XtPointer );
void cldhgtw_optpvalCb ( Widget, long, XtPointer );

/*
 *  private functions
 */
void cldhgtw_ghostPts ( Boolean make_new );
void cldhgtw_update ( void );
void cldhgtw_optcreate ( Widget parent );
void cldhgtw_optpopup ( void );
void cldhgtw_optpopdown ( int savflg );
void cldhgtw_rdTbl ( void );
void cldhgtw_status ( void );

/************************************************************************
 * nmap_cldhgtw.c							*
 *									*
 * This module defines a cloud height popup window for nmap             *
 *									*
 * CONTENTS:								*
 *	cldhgtw_create()	create the cloud height window	        *
 *	cldhgtw_ghostPts()	draw cursor ghost			*
 *	cldhgtw_popup()		pop up the cloud height window	        *
 *	cldhgtw_popdown()	pop down the cloud height window	*
 *	cldhgtw_update()	update the height-pressure list in   	*
 *                              cloud height window                     *
 *									*
 *	cldhgtw_optcreate	create the options window		*
 *	cldhgtw_optpopup	pop up the options window		*
 *	cldhgtw_optpopdown	pop down the options window		*
 *									*
 *	cldhgtw_isUp()		query if the window is up 		*
 *									*
 *	cldhgtw_ctlBtnCb()	callback for control buttons 		*
 *	cldhgtw_optctlBtnCb	callback for option control buttons	*
 *	cldhgtw_optdstkCb	callback for distance text input	*
 *	cldhgtw_optareaCb	callback for pixel area radio buttons	*
 *	cldhgtw_optdistCb	callback for distance radio buttons	*
 *	cldhgtw_opttempCb	callback for temperature radio buttons	*
 *	cldhgtw_opthghtCb	callback for height radio buttons	*
 *	cldhgtw_optdataCb	callback for data sel type radio buttons*
 *      cldhgtw_optdata2Cb      callback for data type 2 radio buttons  *
 *      cldhgtw_optlevelCb      callback for level radio buttons        *
 *	cldhgtw_optpixlCb	callback for pixel sel type radio button*
 *	cldhgtw_optpvalCb	callback for pixel area value text input*
 *								        *
 *      cldhgtw_pointerEh()	event handler for input from the map    *
 *                              widget	                                *
 *      cldhgtw_rdTbl()         read default info. from nmap_cldhgt.tbl *
 *	cldhgtw_status		update status message			*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

Widget cldhgtw_create ( Widget parent )
/************************************************************************
 * cldhgtw_create							*
 *									*
 * This function creates the cloud height popup window.		        *
 *									*
 * Widget cldhgtw_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * cldhgtw_create	Widget	ID of the cloud height popup window	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	     09/99	initial coding                          *
 * H. Zeng/EAI       10/99      redesigned the two control buttons      *
 * S. Jacobs/NCEP    11/99	Changed the temperature label to Celsius*
 * S. Jacobs/NCEP    12/99	Added options dialog box		*
 * H. Zeng/EAI	     04/00	Added status message and default table	*
 ***********************************************************************/
{
    Widget	pane;
    Widget      inforc, status_label;
    Widget      rc1, rc2;
    Widget      form;
    Widget      button;

    int		nn, toff = 10, nitems;
    long	ii;

    char	*btnstrs[] = {"Take Control", "Options...", "Close"};

    char        lstr[128];
/*---------------------------------------------------------------------*/
/*
 * read default info. from nmap_cldhgt.tbl
 */
    cldhgtw_rdTbl();

/*
 * create dialog shell
 */
    _cldhgtwWin = XmCreateFormDialog(parent, "cldhgtw_popup",
				    NULL, 0);
    XtVaSetValues(_cldhgtwWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_cldhgtwWin),
		  XmNtitle, "CLOUD HEIGHT",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("cldhgtw_pane",
			    xmPanedWindowWidgetClass, _cldhgtwWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
/*
 * create FORMATTING area
 */
    _formatForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);
 
/*
 *  creat information list
 */
    inforc = XtVaCreateWidget ("inforc",
			       xmRowColumnWidgetClass,	_formatForm,
			       XmNorientation,		XmHORIZONTAL,
			       XmNpacking,		XmPACK_TIGHT,
			       NULL);

    rc1 = XtVaCreateWidget ("infolabel",
			    xmRowColumnWidgetClass,	inforc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    rc2 = XtVaCreateWidget ("infotext",
			    xmRowColumnWidgetClass,	inforc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);


    nn = 7;
    XtVaCreateManagedWidget ("Lat/Lon ",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);

    XtVaCreateManagedWidget ("Closest Station",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);

    _cldhgtDlabel = XtVaCreateManagedWidget ("Distance",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);

    XtVaCreateManagedWidget ("Pixel Value",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);

    _cldhgtTlabel = XtVaCreateManagedWidget ("Temperature",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);


    for (ii = 0; ii < 5; ii++) {
	_infoTxtW[ii] = 
	    XtVaCreateManagedWidget("cldhgt_info",
                                    xmTextFieldWidgetClass,    rc2,
				    XmNcolumns,                20,
                                    XmNeditable,               False,
                                    XmNcursorPositionVisible,  False,
                                    XmNhighlightThickness,     0,
                                    XmNheight,                 30,
                                    NULL);

    }


    XtVaSetValues (inforc,
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtManageChild (rc1);
    XtManageChild (rc2);
    XtManageChild (inforc);

/*
 * create the height-pressure list label 
 */
    _cldhgtLabel = XtVaCreateManagedWidget("cldhgt_label",
				 xmLabelWidgetClass,	_formatForm,
				 XmNtopAttachment,	XmATTACH_WIDGET,
                                 XmNtopWidget,          inforc,
				 XmNtopOffset,	        (toff+5),
				 NULL);

    sprintf (lstr, "Distance (%s)", _dunits[_currDist] );
    NxmLabel_setStr (_cldhgtDlabel, lstr);

    sprintf (lstr, "Temperature (%s)", _tunits[_currTemp] );
    NxmLabel_setStr (_cldhgtTlabel, lstr);

    sprintf (lstr, "      Height (%s)            Pressure (mb)",
	     _hunits[_currHght] );
    NxmLabel_setStr (_cldhgtLabel, lstr);

/*
 * create the height-pressure list 
 */
    _cldhgtList = XmCreateScrolledList(_formatForm, "cldhgt_list", NULL, 0);
    XtVaSetValues(XtParent(_cldhgtList),
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		_cldhgtLabel,
		  XmNtopOffset,		(toff-6),
		  XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		  XmNleftWidget,	_cldhgtLabel,
		  XmNrightAttachment,	XmATTACH_FORM,
		  NULL);

    XtVaSetValues(_cldhgtList,
		  XmNscrollingPolicy,		XmAPPLICATION_DEFINED,
		  XmNscrollBarDisplayPolicy,	XmSTATIC,
		  XmNitemCount,			0,
                  XmNvisibleItemCount,          3,  
		  NULL);
    
    XtManageChild(_cldhgtList);

/*
 * status message
 */
    status_label = XtVaCreateManagedWidget("Status Message",
		   xmLabelWidgetClass,	_formatForm,
		   XmNleftAttachment,	XmATTACH_FORM,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,          XtParent(_cldhgtList),
		   NULL);


    _statusList = XmCreateScrolledList(_formatForm, "status_list", NULL, 0);
    XtVaSetValues(XtParent(_statusList),
		  XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		  XmNleftWidget,        status_label,
                  XmNrightAttachment,	XmATTACH_FORM,
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,         status_label,
		  NULL);

    XtVaSetValues(_statusList,
		  XmNscrollingPolicy,		XmAPPLICATION_DEFINED,
		  XmNscrollBarDisplayPolicy,	XmSTATIC,
		  XmNitemCount,			0,
                  XmNvisibleItemCount,          2,  
		  NULL);
    
    XtManageChild(_statusList);

    XtManageChild (_formatForm);

/*
 * create control buttons
 */
    nitems = XtNumber ( btnstrs );
    form = XtVaCreateWidget("form",
                            xmFormWidgetClass, pane,
			    XmNfractionBase,   (nitems * 100),
                            NULL                       ); 

    for ( ii = 0; ii < nitems; ii++ )  {

	button = XtVaCreateManagedWidget ( btnstrs[ii], 
			xmPushButtonWidgetClass, form,
			XmNheight,               25,
			XmNwidth,                100,
                        XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         (ii * 100),
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        ((ii + 1) * 100),
			NULL );
   
	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)cldhgtw_ctlBtnCb, (XtPointer)ii );
    }

    XtManageChild(form);
    XtManageChild(pane);
    cldhgtw_optcreate ( parent );
    return(_cldhgtwWin);
}

/*=====================================================================*/

void cldhgtw_popup ( void )
/************************************************************************
 * cldhgtw_popup				      			*
 *									*
 * This function pops up the cloud height popup window .		*
 *									*
 * void cldhgtw_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 * T. Piper/SAIC	12/04	Added cldhgtw_refresh			*
 ***********************************************************************/
{
    XtManageChild (_cldhgtwWin);
    mcanvw_setDynamicFunc ((XtEventHandler)&cldhgtw_pointerEh,
			   (XtEventHandler)NULL,
			   (XtEventHandler)NULL, CURS_POINT_SELECT);
    cldhgtw_refresh(TRUE);
}

/*=====================================================================*/

void cldhgtw_popdown ( void ) 
/************************************************************************
 * cldhgtw_popdown							*
 *									*
 * This function pops down the cloud height window.			*
 *									*
 * void cldhgtw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 * J. Wu/SAIC		01/02	reset PGEN operation when necessary	*
 ***********************************************************************/
{
    if (XtIsManaged (_cldhgtwWin)) {
	XtUnmanageChild (_cldhgtwWin);
    }
    
    mcanvw_disarmDynamic();
    mcanvw_setCursor (CURS_DEFAULT);
    
    if ( pgpalw_isUp() ) {
        pgpalw_setupOper( );
    }
}

/*=====================================================================*/

void cldhgtw_update ( void )
/************************************************************************
 * cldhgtw_update                                                       *
 *                                                                      *
 * This function updates the height-pressure list in cloud height	*
 * window if an IR image has been loaded.  		        	*
 *                                                                      *
 * void cldhgtw_update ()                                     	        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                   NONE                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      09/99   initial coding                              *
 * T. Lee/GSC	    10/99   Displayed pixel and temp value if exist	*
 * E. Safford/GSC   10/99   Enabled use with nmap2			*
 * E. Safford/GSC   10/99   dataw_getCurLoop -> loop_getCurLoop		*
 * T. Lee/GSC	    11/99   Added maxdst to im_gpix calling sequence	*
 * S. Jacobs/NCEP   11/99   Display temperature in Celsius		*
 * T. Lee/GSC	    12/99   Added sounding table flag			*
 * T. Lee/GSC	    12/99   Added pixel area and mode retrieval		*
 * S. Jacobs/NCEP   12/99   Added options dialog box			*
 * T. Lee/GSC	    01/00   Added mstflg				*
 * T. Lee/GSC	    03/00   Computed max search radius			*
 * T. Lee/GSC	    04/00   Added status message			*
 * M. Li/GSC	    03/01   Added nmp_gmapattr				*
 * J. Wu/GSC        05/01   free XmStrings    				*
 * T. Lee/SAIC	    04/03   Displayed pixel value if unable to get hght	*
 * J. Wu/SAIC       01/04   call er_gnumerr				*
 * M. Li/SAIC	    03/05   Move dataw_getIRInfo to the beginning	*
 ***********************************************************************/
{
    Widget      draw_w;
    int		ier, nn, nc, lp, ierr, which;
    char        imgfil[256], garea[256], dattim[20], 
                stid[256],   lstr[128] ;
    char        mesg[] = "Unable to invoke CLOUD HEIGHT.\n\nPlease load an IR image!\n" ;
    float       tmpk, temp_dsp, dist, dist_dsp, hght_m, hght_dsp;
    float       pres[15], hght[15]; 
    int         iarea, mode, ipix, npt, maxdst, tblflg, mstflg;
    nmpstr_t    mapDrp, projDrp, gareaDrp[2];
    XmString	xmstr;
/*---------------------------------------------------------------------*/
/*
 * Read the IR image.
 */
    dataw_getIRInfo(dattim, imgfil, &ier);
    if ( ier != 0) {
        draw_w = (Widget)mcanvw_getDrawingW();
        NxmWarn_show (draw_w, mesg);
        return;
    }

/*
 * Update the labels with the current units.
 */
    sprintf (lstr, "Distance (%s)", _dunits[_currDist] );
    NxmLabel_setStr (_cldhgtDlabel, lstr);

    sprintf (lstr, "Temperature (%s)", _tunits[_currTemp] );
    NxmLabel_setStr (_cldhgtTlabel, lstr);

    sprintf (lstr, "      Height (%s)            Pressure (mb)",
	     _hunits[_currHght] );
    NxmLabel_setStr (_cldhgtLabel, lstr);

/*
 *  Flush the cloud height window
 */
    lp = loop_getCurLoop();

    for(nn = 0; nn<5; nn++) {
       XmTextSetString(_infoTxtW[nn], "\0");
    }

    XtVaGetValues(_cldhgtList, XmNitemCount, &nn, NULL);

    if (nn !=0) {
        XmListDeleteAllItems(_cldhgtList);
    }

/*
 *  Flush the status message.
 */
    XtVaGetValues(_statusList, XmNitemCount, &nn, NULL);

    if (nn !=0) XmListDeleteAllItems(_statusList);

    nmp_gmapattr(lp, mapDrp, projDrp, gareaDrp, &ierr);

    if (strlen(gareaDrp[1]) > (size_t)0) {
         which = 1;
    } else {
         which = 0;
    }

    strcpy(garea, gareaDrp[which]);

    stid[255] = '\0';

    maxdst = (int)_currDstm;

    if  ( _currData == 0 )  {
	tblflg = 0;
    }
    else {
	tblflg = 1;
    }

    if  ( _currData2 == 0 )  {
	mstflg = 0;
    }
    else {
	mstflg = 1;
    }

    if  ( _currPixl == 0 )  {
	iarea = 0;
	mode  = 0;
    }
    else {
	iarea = _currArea;
	mode  = _currPval;
    }

    er_gnumerr ( &nermsg_save, &ier );

    im_gpix (imgfil, garea, dattim, &_currLat, &_currLon, 
            &maxdst, &tblflg, &mstflg, &iarea, &mode, &ipix,
 	    &tmpk, stid, &dist, pres, hght, &npt, &ier,
            strlen(imgfil), strlen(garea), strlen(dattim), 255);

    if ( ier > 0 || ier == -11 ) {

/*
 * Write status message if ier > 0, but write error message
 * when unable to get cloud height.  Either case, the pixel
 * value and station info will be displayed on the cloud
 * height window.
 */
	if ( ier > 0 )  {
	    cldhgtw_status(); 
	}
	else {
	    er_wmsg( "IM", &ier, " ", &ierr, 2, 1);
	    NxmErr_update();
	}

/*
 * Display the pixel value.
 */
        sprintf(lstr, "%7.2f/%-10.2f", _currLat, _currLon);
        XmTextSetString(_infoTxtW[0], lstr);

        sprintf(lstr, "%-7d", ipix);
        XmTextSetString(_infoTxtW[3], lstr);

	if  ( _currTemp == 0 )  {
/* C */
	    temp_dsp = pr_tmkc ( &tmpk );
	}
	else {
/* K */
	    temp_dsp = tmpk;
	}
        sprintf(lstr, "%-9.1f", temp_dsp );
        XmTextSetString(_infoTxtW[4], lstr);

/*
 * Display pressure and height values
 */
	for ( nn = 0; nn < npt; nn++ ) {

/*
 *  Convert meter into feet
 */
            hght_m = hght[nn];
	    if  ( _currHght == 0 )  {
/* ft */
		hght_dsp = pr_hgmf ( &hght_m );
	    }
	    else {
/* m */
		hght_dsp = hght_m;
	    }
           
/*
 * List the item into height-pressure list. No listing
 * if both are missing.
 */
	    if ( hght_dsp >= 0.0F || pres [nn] >= 0.0F ) {
        	sprintf (lstr, "      %-10.0f               %-10.0f",
			hght_dsp, pres[nn]);
	        xmstr = XmStringCreateLocalized(lstr);
        	XmListAddItemUnselected (_cldhgtList, xmstr, 0);
	        XmStringFree( xmstr );
	    }
	}

/*
 * Display station information. 
 */
        cst_lstr(stid, &nc, &ier);
        *(stid+nc) = '\0';
        XmTextSetString(_infoTxtW[1], stid);
    
	if  ( _currDist == 0 )  {
/* km */
	    dist_dsp = dist / 1000.0F;
	}
	else if  ( _currDist == 1 )  {
/* mi */
	    dist_dsp = dist * M2SM;
	}
	else {
/* nm */
	    dist_dsp = dist * M2NM;
	}
	if ( dist >= 0.0F ) {
            sprintf(lstr, "%-12.1f", dist_dsp);
            XmTextSetString(_infoTxtW[2], lstr);
	}
    }
    else if ( ier < 0 ) {
        er_wmsg( "IM", &ier, " ", &ierr, 2, 1);
	NxmErr_update(); 

    }
    else {

/*
 *  Update the cloud height window
 */
        sprintf(lstr, "%7.2f/%-10.2f", _currLat, _currLon);
        XmTextSetString(_infoTxtW[0], lstr);
    
        cst_lstr(stid, &nc, &ier);
        *(stid+nc) = '\0';
        XmTextSetString(_infoTxtW[1], stid);
    
	if  ( _currDist == 0 )  {
/* km */
	    dist_dsp = dist / 1000.0F;
	}
	else if  ( _currDist == 1 )  {
/* mi */
	    dist_dsp = dist * M2SM;
	}
	else {
/* nm */
	    dist_dsp = dist * M2NM;
	}
        sprintf(lstr, "%-12.1f", dist_dsp);
        XmTextSetString(_infoTxtW[2], lstr);

        sprintf(lstr, "%-7d", ipix);
        XmTextSetString(_infoTxtW[3], lstr);

	if  ( _currTemp == 0 )  {
/* C */
	    temp_dsp = pr_tmkc ( &tmpk );
	}
	else {
/* K */
	    temp_dsp = tmpk;
	}
        sprintf(lstr, "%-9.1f", temp_dsp );
        XmTextSetString(_infoTxtW[4], lstr);
    
	for ( nn = npt-1; nn >= 0; nn-- ) {

/*
 *  Convert meter into feet
 */
            hght_m = hght[nn];
	    if  ( _currHght == 0 )  {
/* ft */
		hght_dsp = pr_hgmf ( &hght_m );
	    }
	    else {
/* m */
		hght_dsp = hght_m;
	    }
           
/*
 * List the item into height-pressure list
 */
            sprintf(lstr, "      %-10.0f               %-10.0f",
		    hght_dsp, pres[nn]);
            xmstr = XmStringCreateLocalized(lstr);
	    XmListAddItemUnselected(_cldhgtList, xmstr, 0);
	    XmStringFree( xmstr );

	    if  ( _currLevel == 0 ) {
		if ( nn > 0 ) {
		    ier = +9;
		    er_wmsg ( "SN", &ier, " ", &ierr, 2, 1 );
		    cldhgtw_status();
		}
		break;
	    }
	} /* end of for */

    } /* end of else */
}

/*=====================================================================*/

Boolean cldhgtw_isUp ( void ) 
/************************************************************************
 * cldhgtw_isUp								*
 *									*
 * This function queries whether the cloud height window is up.	        *
 *									*
 * Boolean cldhgtw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * cldhgtw_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_cldhgtwWin));
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_pointerEh ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * cldhgtw_pointerEh							*
 *									*
 * This is the event handler for input from the map widget by the	*
 * mouse.								*
 *									*
 * void cldhgtw_pointerEh ( wid, clnt, event, ctdr )			*
 *									*
 * Input parameters:							*
 *	wid	Widget	the widget calling this function		*
 *	clnt	XtPointer	client data				*
 *	*event	XEvent	the event callback structure			*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean	continue to dispatch return flag		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		09/99	copied from seekw_pointeEh()            *
 * E. Safford/GSC	10/99	update for new xwcmn.h			*
 * M. Li/GSC		01/00	Used string variables in gtrans		*
 * T. Lee/GSC		03/00	Set cursor function			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * T. Piper/SAIC	12/04	Added cldhgtw_ghostPts			*
 ***********************************************************************/
{
    int		npts, ier, xoff, yoff;
    float	xx[1], yy[1];
/*---------------------------------------------------------------------*/
/*
 *  "which" will always be MCANVW_PRESS
 */
    if (event->xbutton.button == Button1) {

	mcanvw_setCursor (CURS_BUSY);

	xgtoff (&xoff, &yoff, &ier);
	xx[0] = (float) (event->xbutton.x + xoff);
	yy[0] = (float) (event->xbutton.y + yoff);

	npts = 1;
	gtrans (sys_D, sys_M, &npts, xx, yy, 
		&_currLat, &_currLon, &ier, strlen(sys_D), strlen(sys_M));

	cldhgtw_ghostPts ( FALSE);	
	cldhgtw_ghostPts ( TRUE );
	cldhgtw_update ();

	mcanvw_setCursor (CURS_POINT_SELECT);
    }
}

/*=====================================================================*/

void cldhgtw_refresh ( Boolean make_new )
/************************************************************************
 * cldhgtw_refresh                                                      *
 *                                                                      *
 * This function redraws the ghosting.                                  *
 *                                                                      *
 * void cldhgtw_refresh ( make_new )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 * make_new	Boolean	Flag for make_new or using existing location	*
 * 									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	12/04	Modified from seekw_refresh		*
 ***********************************************************************/
{
    if ( cldhgtw_isUp()) {
        cldhgtw_ghostPts(make_new); /* FALSE means use existing point */
    }
}

/*=====================================================================*/

void cldhgtw_ghostPts ( Boolean make_new )
/************************************************************************
 * cldhgtw_ghostPts                                                     *
 *                                                                      *
 * This function shows or hides the ghosting based on make_new.         *
 *                                                                      *
 * void cldhgtw_ghostPts ( make_new )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      make_new        Boolean         recalculate cursor position     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	12/04	Modified from seekw_ghostPts	        *
 ***********************************************************************/
{
    int			np, ier;
    static float	pt1x[1], pt1y[1];
/*---------------------------------------------------------------------*/

    pggst_veilGhost (FALSE);

    if (make_new) {
	np = 1;
	gtrans (sys_M, sys_D, &np, &_currLat, &_currLon,
		pt1x, pt1y, &ier, strlen(sys_M), strlen(sys_D) );
    }

    pggst_cursorGhost ( pt1x, pt1y, &ier);
    pggst_clearGhost (TRUE);
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * cldhgtw_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of cloud height  *
 * popup window.						        *
 *									*
 * void cldhgtw_ctlBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          09/99   copied from pgwfmt_ctlBtnCb()           *
 * S. Jacobs/NCEP	12/99	Added options dialog box		*
 ***********************************************************************/
{
   
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Take Control */

        pgpalw_classPopdown();
        mcanvw_setDynamicFunc ( (XtEventHandler)&cldhgtw_pointerEh, 
				(XtEventHandler)NULL, 
				(XtEventHandler)NULL, CURS_POINT_SELECT);
	break;

      case 1:	/* Options... */

	cldhgtw_optpopup ();
	break;

      case 2:	/* Close */
	cldhgtw_popdown ();
	cldhgtw_optpopdown ( 0 );
	break;
    }
}

/*=====================================================================*/

void cldhgtw_optcreate ( Widget parent )
/************************************************************************
 * cldhgtw_optcreate							*
 *									*
 * This function creates the cloud height options popup window.		*
 *									*
 * void cldhgtw_optcreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * 	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * H. Zeng/EAI          01/00   Added sounding data type 2              *
 * T. Lee/GSC		03/00	Set max search radius unit		*
 * H. Zeng/EAI		04/00	Added status message			*
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{

    Widget	pane, form, button, formbtn, label;
    Widget	rca, rcb, rcc;
    Widget	form1, form2, form3, level_form;
    Widget	label1, label2, label3, level_label;
    Widget	rc1, rc2, rc3, rc4, rc5, rc6, rc7, level_rc;

    char	*diststr[] = {"km", "mi", "nm"};
    char	*tempstr[] = {"Celsius", "Kelvin"};
    char	*hghtstr[] = {"feet", "meters"};
    char	*datastr[] = {"Closest Station", "Standard Atmosphere"};
    char        *datastr2[]= {"Standard Search", "Moist-Adiabatic"};
    char        *levelstr[]= {"single", "multiple"};
    char	*pixlstr[] = {"Single Point", "Pixel Area"};
    char	*pvalstr[] = {"Maximum", "Mode"};
    char	*btnstrs[] = {"OK", "Defaults", "Cancel"};
    char	start_dist[13], start_area[7], lstr[32];

    long	ii, nn;
    float       dist;

/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _cldhgtwOpt = XmCreateFormDialog ( parent, "cldhgtw_opt_popup",
				       NULL, 0);
    XtVaSetValues ( _cldhgtwOpt, 
		    XmNnoResize,        True, 
		    XmNdefaultPosition, False, 
		    NULL);
    XtVaSetValues ( XtParent(_cldhgtwOpt),
		    XmNtitle, "CLOUD HEIGHT OPTIONS",
		    NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("cldhgtw_opt_pane",
			    xmPanedWindowWidgetClass, _cldhgtwOpt,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
/*
 * create a form to hold the radio buttons.
 */
    form = XtVaCreateWidget ( "form",
				xmFormWidgetClass,  pane,
				NULL );

    rca = XtVaCreateWidget ( "units_col",
				xmRowColumnWidgetClass, form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmVERTICAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );

    label = XtVaCreateManagedWidget ( "Units:",
				xmLabelWidgetClass, rca,
				NULL );

    form1 = XtVaCreateWidget ( "form1",
				 xmFormWidgetClass, rca,
				 NULL );

    label1 = XtVaCreateManagedWidget ( "Distance",
				xmLabelWidgetClass, form1,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       form1,
				XmNmarginHeight,    7,
				NULL );

    rc1 = XtVaCreateManagedWidget ( "dist_rowcol",
				xmRowColumnWidgetClass, form1,
				XmNradioBehavior,       False,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          label1,
				NULL );

    nn = XtNumber ( diststr );
    for ( ii = 0; ii < nn; ii++ )  {
	_distRb[ii] = XtVaCreateManagedWidget ( diststr[ii],
			    xmToggleButtonWidgetClass, rc1,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currDist == ii),
			    XmNuserData,               &_currDist,
			    NULL );

	XtAddCallback ( _distRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optdistCb, (XtPointer)ii);
    }

    form2 = XtVaCreateWidget ( "form2",
				 xmFormWidgetClass, rca,
				 NULL );

    label2 = XtVaCreateManagedWidget ( "Temperature",
				xmLabelWidgetClass, form2,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       form2,
				XmNmarginHeight,    7,
				NULL );

    rc2 = XtVaCreateManagedWidget ( "temp_rowcol",
				xmRowColumnWidgetClass, form2,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          label2,
				NULL );

    nn = XtNumber ( tempstr );
    for ( ii = 0; ii < nn; ii++ )  {
	_tempRb[ii] = XtVaCreateManagedWidget ( tempstr[ii],
			    xmToggleButtonWidgetClass, rc2,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currTemp == ii),
			    XmNuserData,               &_currTemp,
			    NULL );

	XtAddCallback ( _tempRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_opttempCb, (XtPointer)ii);
    }

    form3 = XtVaCreateWidget ( "form3",
				 xmFormWidgetClass, rca,
				 NULL );

    label3 = XtVaCreateManagedWidget ( "Height",
				xmLabelWidgetClass, form3,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       form3,
				XmNmarginHeight,    7,
				NULL );

    rc3 = XtVaCreateManagedWidget ( "hght_rowcol",
				xmRowColumnWidgetClass, form3,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          label3,
				NULL );

    nn = XtNumber ( hghtstr );
    for ( ii = 0; ii < nn; ii++ )  {
	_hghtRb[ii] = XtVaCreateManagedWidget ( hghtstr[ii],
			    xmToggleButtonWidgetClass, rc3,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currHght == ii),
			    XmNuserData,               &_currHght,
			    NULL );

	XtAddCallback ( _hghtRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_opthghtCb, (XtPointer)ii);
    }

    rcb = XtVaCreateWidget ( "data_col",
				xmRowColumnWidgetClass, form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmVERTICAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNtopAttachment,       XmATTACH_WIDGET,
				XmNtopWidget,           rca,
				NULL );

    XtVaCreateManagedWidget ( "Type of sounding data:",
				xmLabelWidgetClass, rcb,
				NULL );

    rc4 = XtVaCreateManagedWidget ( "data_rowcol",
				xmRowColumnWidgetClass, rcb,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );

    nn = XtNumber ( datastr );
    for ( ii = 0; ii < nn; ii++ )  {
	_dataRb[ii] = XtVaCreateManagedWidget ( datastr[ii],
			    xmToggleButtonWidgetClass, rc4,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currData == ii),
			    XmNuserData,               &_currData,
			    NULL );

	XtAddCallback ( _dataRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optdataCb, (XtPointer)ii);
    }

    rc7 = XtVaCreateManagedWidget ( "data_rowcol",
				xmRowColumnWidgetClass, rcb,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );

    nn = XtNumber ( datastr2 );
    for ( ii = 0; ii < nn; ii++ )  {
	_dataRb2[ii] = XtVaCreateManagedWidget ( datastr2[ii],
			    xmToggleButtonWidgetClass, rc7,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currData2 == ii),
			    XmNuserData,               &_currData2,
			    NULL );

	XtAddCallback ( _dataRb2[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optdata2Cb, (XtPointer)ii);
    }


    _distForm = XtVaCreateWidget ( "dist_form",
				xmFormWidgetClass, form,
				XmNtopAttachment,	XmATTACH_WIDGET,
				XmNtopWidget,		rcb,
				NULL );

    sprintf ( lstr, "Maximum search radius (%s)", 
	      _dunits [_currDist] );
    switch(_currDist) {
       case 0: /* km */
          dist = _currDstm / 1000.0F;
          break;
       case 1: /* mi */
          dist = _currDstm * M2SM; 
          break;
       case 2: /* nm */
          dist = _currDstm * M2NM;
          break;
  
    }
    sprintf (start_dist, "%-12.2f", dist);

    _cldhgtSlabel = XtVaCreateManagedWidget (
			lstr,
			xmLabelWidgetClass,  _distForm,
			XmNleftAttachment,   XmATTACH_FORM,
			XmNmarginHeight,     7,
			NULL );

    _distText =
	(Widget) XtVaCreateManagedWidget ( "dist_text",
				xmTextWidgetClass,    _distForm,
				XmNcolumns,           10,
				XmNvalue,             start_dist,
				XmNleftAttachment,    XmATTACH_WIDGET,
				XmNleftWidget,        _cldhgtSlabel,
				NULL);

    XtAddCallback ( _distText, XmNactivateCallback,
		    (XtCallbackProc)cldhgtw_optdstkCb, (XtPointer)NULL);
    XtAddCallback ( _distText, XmNlosingFocusCallback,
		    (XtCallbackProc)cldhgtw_optdstkCb, (XtPointer)NULL);

/*
 * Level 
 */
    level_form = XtVaCreateWidget ( "level_form",
				 xmFormWidgetClass, form,
				 XmNtopAttachment,	XmATTACH_WIDGET,
				 XmNtopWidget,		_distForm,
				 NULL );

    level_label = XtVaCreateManagedWidget ( "Level:",
				xmLabelWidgetClass, level_form,
				XmNtopAttachment,   XmATTACH_FORM,
				XmNmarginHeight,    7,
				NULL );

    level_rc = XtVaCreateManagedWidget ( "level_rowcol",
				xmRowColumnWidgetClass, level_form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          level_label,
				NULL );

    nn = XtNumber ( levelstr );
    for ( ii = 0; ii < nn; ii++ )  {
	_levelRb[ii] = XtVaCreateManagedWidget ( levelstr[ii],
			    xmToggleButtonWidgetClass, level_rc,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currLevel == ii),
			    XmNuserData,               &_currLevel,
			    NULL );
 
	XtAddCallback ( _levelRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optlevelCb, (XtPointer)ii);

    }

/*
 * pixel selection.
 */
    rcc = XtVaCreateWidget ( "pixsel_col",
				xmRowColumnWidgetClass, form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmVERTICAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNtopAttachment,       XmATTACH_WIDGET,
				XmNtopWidget,           level_form,
				NULL );

    XtVaCreateManagedWidget ( "Type of pixel selection:",
				xmLabelWidgetClass, rcc,
				NULL );

    rc5 = XtVaCreateManagedWidget ( "pixsel_rowcol",
				xmRowColumnWidgetClass, rcc,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );

    nn = XtNumber ( pixlstr );
    for ( ii = 0; ii < nn; ii++ )  {
	_pixlRb[ii] = XtVaCreateManagedWidget ( pixlstr[ii],
			    xmToggleButtonWidgetClass, rc5,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currPixl == ii),
			    XmNuserData,               &_currPixl,
			    NULL );

	XtAddCallback ( _pixlRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optpixlCb, (XtPointer)ii);
    }

    _areaForm = XtVaCreateWidget ( "area_form",
				xmFormWidgetClass, form,
				XmNtopAttachment,  XmATTACH_WIDGET,
				XmNtopWidget,      rcc,
				NULL );

    label = XtVaCreateManagedWidget (
			"Radius of area in pixels",
			xmLabelWidgetClass,  _areaForm,
			XmNleftAttachment,   XmATTACH_FORM,
			XmNmarginHeight,     7,
			NULL );

    sprintf (start_area, "%-6d", _currArea);
    _areaText =
	(Widget) XtVaCreateManagedWidget ( "area_text",
				xmTextWidgetClass,    _areaForm,
				XmNcolumns,           6,
				XmNvalue,             start_area,
				XmNleftAttachment,    XmATTACH_WIDGET,
				XmNleftWidget,        label,
				NULL);

    XtAddCallback ( _areaText, XmNactivateCallback,
		    (XtCallbackProc)cldhgtw_optareaCb, (XtPointer)NULL);
    XtAddCallback ( _areaText, XmNlosingFocusCallback,
		    (XtCallbackProc)cldhgtw_optareaCb, (XtPointer)NULL);

    _pvalForm = XtVaCreateWidget ( "pixval_col",
				xmRowColumnWidgetClass, form,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNtopAttachment,       XmATTACH_WIDGET,
				XmNtopWidget,           _areaForm,
				NULL );

    XtVaCreateManagedWidget ( "Pixel value:",
				xmLabelWidgetClass, _pvalForm,
				NULL );

    rc6 = XtVaCreateManagedWidget ( "pixval_rowcol",
				xmRowColumnWidgetClass, _pvalForm,
				XmNradioBehavior,       FALSE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				NULL );

    nn = XtNumber ( pvalstr );
    for ( ii = 0; ii < nn; ii++ )  {
	_pvalRb[ii] = XtVaCreateManagedWidget ( pvalstr[ii],
			    xmToggleButtonWidgetClass, rc6,
			    XmNhighlightThickness,     0,
			    XmNset,                    (_currPval == ii),
			    XmNuserData,               &_currPval,
			    NULL );

	XtAddCallback ( _pvalRb[ii], XmNarmCallback,
			(XtCallbackProc)cldhgtw_optpvalCb, (XtPointer)ii);
    }

/*
 * create control buttons
 */
    nn = XtNumber ( btnstrs );
    formbtn = XtVaCreateWidget("formbtn",
                            xmFormWidgetClass, pane,
			    XmNfractionBase,   (nn * 100),
                            NULL                       ); 

    for ( ii = 0; ii < nn; ii++ )  {

	button = XtVaCreateManagedWidget ( btnstrs[ii], 
			xmPushButtonWidgetClass, formbtn,
			XmNheight,               25,
			XmNwidth,                100,
                        XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         (ii * 100),
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        ((ii + 1) * 100),
			NULL );
   
	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)cldhgtw_optctlBtnCb, (XtPointer)ii );
    }


    XtManageChild(form1);
    XtManageChild(form2);
    XtManageChild(form3);
    XtManageChild(rca);

    XtManageChild(rcb);

    XtManageChild(_distForm);

    XtManageChild(level_form);

    XtManageChild(rcc);

    XtManageChild(_areaForm);

    XtManageChild(_pvalForm);

    XtManageChild(form);
    XtManageChild(formbtn);
    XtManageChild(pane);

}

/*=====================================================================*/

void cldhgtw_optpopup ( void )
/************************************************************************
 * cldhgtw_optpopup				      			*
 *									*
 * This function pops up the cloud height options popup window.		*
 *									*
 * void cldhgtw_optpopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * H. Zeng/EAI          01/00   Added sounding data type 2              *
 * T. Lee/GSC		03/00	Set max search radius unit		*
 * H. Zeng/EAI		04/00	Saved sounding level information	*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
	char	newstr[13], lstr[128];
        float   dist;
	int	ii;

/*---------------------------------------------------------------------*/

    _saveDist = _currDist;
    for (ii = 0; ii < MAX_DIST; ii++) {
    	if ( ii == _currDist ) 
	    XmToggleButtonSetState ( _distRb[ii], True, True );
	else
	    XmToggleButtonSetState ( _distRb[ii], False, False);
    }

    _saveTemp = _currTemp;
    for (ii = 0; ii < MAX_TEMP; ii++) {
        if ( ii == _currTemp )
            XmToggleButtonSetState ( _tempRb[ii], True, True );
        else
            XmToggleButtonSetState ( _tempRb[ii], False, False);
    }

    _saveHght = _currHght;
    for (ii = 0; ii < MAX_HGHT; ii++) {
        if ( ii == _currHght )
            XmToggleButtonSetState ( _hghtRb[ii], True, True );
        else
            XmToggleButtonSetState ( _hghtRb[ii], False, False);
    }

    _saveData = _currData;
    for (ii = 0; ii < MAX_DATA; ii++) {
        if ( ii == _currData )
            XmToggleButtonSetState ( _dataRb[ii], True, True );
        else
            XmToggleButtonSetState ( _dataRb[ii], False, False);
    }

    _saveData2= _currData2;
    for (ii = 0; ii < MAX_DATA; ii++) {
        if ( ii == _currData2 )
            XmToggleButtonSetState ( _dataRb2[ii], True, True );
        else
            XmToggleButtonSetState ( _dataRb2[ii], False, False);
    }

    _saveLevel = _currLevel;
    for (ii = 0; ii < MAX_LEVEL; ii++) {
        if ( ii == _currLevel )
            XmToggleButtonSetState ( _levelRb[ii], True, True );
        else
            XmToggleButtonSetState ( _levelRb[ii], False, False);
    }

    sprintf ( lstr, "Maximum search radius (%s)",
    _dunits [_currDist] );
    NxmLabel_setStr (_cldhgtSlabel, lstr);

    _saveDstm = _currDstm;

    switch(_currDist) {
       case 0: /* km */
          dist = _currDstm / 1000.0F;
          break;
       case 1: /* mi */
          dist = _currDstm * M2SM; 
          break;
       case 2: /* nm */
          dist = _currDstm * M2NM;
          break;
  
    }

    sprintf (newstr, "%-12.2f", dist);
    XmTextSetString ( _distText, newstr );
    if  ( _currData == 0 )  {
	XtSetSensitive ( _distForm, True );
    }
    else {
	XtSetSensitive ( _distForm, False );
    }

    _savePixl = _currPixl;
    for (ii = 0; ii < MAX_PIXL; ii++) {
        if ( ii == _currPixl )
            XmToggleButtonSetState ( _pixlRb[ii], True, True );
        else
            XmToggleButtonSetState ( _pixlRb[ii], False, False);
    }

    _saveArea = _currArea;
    sprintf (newstr, "%-6d", _currArea);
    XmTextSetString ( _areaText, newstr );
    if  ( _currPixl == 0 )  {
	XtSetSensitive ( _areaForm, False );
    }
    else {
	XtSetSensitive ( _areaForm, True );
    }

    _savePval = _currPval;
    for (ii = 0; ii < MAX_PVAL; ii++) {
        if ( ii == _currPval )
            XmToggleButtonSetState ( _pvalRb[ii], True, True );
        else
            XmToggleButtonSetState ( _pvalRb[ii], False, False);
    }

    if  ( _currPixl == 0 )  {
	XtSetSensitive ( _pvalForm, False );
    }
    else {
	XtSetSensitive ( _pvalForm, True );
    }
 
    XtManageChild (_cldhgtwOpt);
    
}

/*=====================================================================*/

void cldhgtw_optpopdown ( int savflg ) 
/************************************************************************
 * cldhgtw_optpopdown							*
 *									*
 * This function pops down the cloud height options window.		*
 *									*
 * void cldhgtw_optpopdown ( savflg )					*
 *									*
 * Input parameters:							*
 *	savflg		int		Flag to save the changes	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * H. Zeng/EAI          01/00   Added sounding data type 2              *
 * H. Zeng/EAI		04/00	Added sounding level information	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_cldhgtwOpt)) {

	if  ( ! savflg )  {
	    _currDist = _saveDist;
	    _currTemp = _saveTemp;
	    _currHght = _saveHght;
	    _currData = _saveData;
            _currData2= _saveData2;
            _currLevel= _saveLevel;
	    _currPixl = _savePixl;
	    _currDstm = _saveDstm;
	    _currArea = _saveArea;
	    _currPval = _savePval;
	}

	XtUnmanageChild (_cldhgtwOpt);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * cldhgtw_optctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of cloud height  *
 * popup window.						        *
 *									*
 * void cldhgtw_optctlBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          09/99   copied from pgwfmt_ctlBtnCb()           *
 * H. Zeng/EAI          01/00   Added sounding data type 2              *
 * T. Lee/GSC		03/00	Set max search radius unit		*
 * H. Zeng/EAI		04/00	Added sounding level			*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
    char	newstr[13], lstr[128];
    float       dist;   
    int	 	ii;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* OK */
	cldhgtw_optpopdown ( 1 );

	cldhgtw_update ();

	break;

      case 1:	/* Defaults */
	_currDist = _defDist;
	for (ii = 0; ii < MAX_DIST; ii++) {
            if ( ii == _currDist )
            	XmToggleButtonSetState ( _distRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _distRb[ii], False, False);
    	}

	_currTemp = _defTemp;
	for (ii = 0; ii < MAX_TEMP; ii++) {
            if ( ii == _currTemp )
            	XmToggleButtonSetState ( _tempRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _tempRb[ii], False, False);
    	}

	_currHght = _defHght;
	for (ii = 0; ii < MAX_HGHT; ii++) {
            if ( ii == _currHght )
            	XmToggleButtonSetState ( _hghtRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _hghtRb[ii], False, False);
    	}

	_currData = _defData;
    	for (ii = 0; ii < MAX_DATA; ii++) {
            if ( ii == _currData )
            	XmToggleButtonSetState ( _dataRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _dataRb[ii], False, False);
    	}

	_currData2= _defData2;
	for (ii = 0; ii < MAX_DATA; ii++) {
            if ( ii == _currData2 )
            	XmToggleButtonSetState ( _dataRb2[ii], True, True );
            else
            	XmToggleButtonSetState ( _dataRb2[ii], False, False);
    	}

	_currLevel = _defLevel;
	for (ii = 0; ii < MAX_LEVEL; ii++) {
            if ( ii == _currLevel )
            	XmToggleButtonSetState ( _levelRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _levelRb[ii], False, False);
    	}

	sprintf ( lstr, "Maximum search radius (%s)",
        _dunits [_currDist] );
        NxmLabel_setStr (_cldhgtSlabel, lstr);

	_currDstm = _defDstm;
        switch(_currDist) {
           case 0: /* km */
              dist = _currDstm / 1000.0F;
              break;
           case 1: /* mi */
              dist = _currDstm * M2SM; 
              break;
           case 2: /* nm */
              dist = _currDstm * M2NM;
              break;
  
        }
	sprintf (newstr, "%-12.2f", dist);
	XmTextSetString ( _distText, newstr );
	if  ( _currData == 0 )  {
	    XtSetSensitive ( _distForm, True );
	}
	else {
	    XtSetSensitive ( _distForm, False );
	}

	_currPixl = _defPixl;
	for (ii = 0; ii < MAX_PIXL; ii++) {
            if ( ii == _currPixl )
            	XmToggleButtonSetState ( _pixlRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _pixlRb[ii], False, False);
    	}

	_currArea = _defArea;
	sprintf (newstr, "%-6d", _currArea);
	XmTextSetString ( _areaText, newstr );
	if  ( _currPixl == 0 )  {
	    XtSetSensitive ( _areaForm, False );
	}
	else {
	    XtSetSensitive ( _areaForm, True );
	}

	_currPval = _defPval;
	for (ii = 0; ii < MAX_PVAL; ii++) {
            if ( ii == _currPval )
            	XmToggleButtonSetState ( _pvalRb[ii], True, True );
            else
            	XmToggleButtonSetState ( _pvalRb[ii], False, False);
    	}

	if  ( _currPixl == 0 )  {
	    XtSetSensitive ( _pvalForm, False );
	}
	else {
	    XtSetSensitive ( _pvalForm, True );
	}

	break;

      case 2:	/* Cancel */
	cldhgtw_optpopdown ( 0 );

	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optdstkCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * cldhgtw_optdstkCb                                                    *
 *                                                                      *
 * Callback function for the distance text box.                         *
 *                                                                      *
 * void cldhgtw_optdstkCb (wid, clnt, cbs)     		        	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt		XtPointer	not used                        *
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * H. Zeng/EAI		04/00	Set unit				*
 * T. Piper/SAIC	04/02	Fixed UMR; initialized dist, dstr	*
 ***********************************************************************/
{
    float	dist=0.0F;
    char	*dstr=NULL, newstr[13];
/*---------------------------------------------------------------------*/

    dstr = XmTextGetString (_distText);

    sscanf (dstr, "%f", &dist);

    XtFree (dstr);

    if (0.0F <= dist && dist <= 100000.0F) {
	sprintf (newstr, "%-12.2f", dist);
        switch(_currDist) {
           case 0: /* km */
              _currDstm = dist * 1000.0F;
              break;
           case 1: /* mi */
              _currDstm = dist / M2SM; 
              break;
           case 2: /* nm */
              _currDstm = dist / M2NM;
              break;
        }
    }
    else {
        switch(_currDist) {
           case 0: /* km */
              dist = _currDstm / 1000.0F;
              break;
           case 1: /* mi */
              dist = _currDstm * M2SM; 
              break;
           case 2: /* nm */
              dist = _currDstm * M2NM;
              break;
        }
	sprintf (newstr, "%-12.2f", dist);
    }
    XmTextSetString (_distText, newstr);
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optareaCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * cldhgtw_optareaCb                                                    *
 *                                                                      *
 * Callback function for the pixel area text box.                       *
 *                                                                      *
 * void cldhgtw_optareaCb (wid, clnt, cbs ) 		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt	        XtPointer	not used                        *
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 ***********************************************************************/
{
    int		irad;
    char	*dstr, newstr[7];
/*---------------------------------------------------------------------*/

    dstr = XmTextGetString (_areaText);

    sscanf (dstr, "%d", &irad);

    XtFree (dstr);

    if (0 <= irad && irad <= 100) {
	sprintf (newstr, "%-6d", irad);

	_currArea = irad;
    }
    else {
	sprintf (newstr, "%-6d", _currArea);
    }

    XmTextSetString (_areaText, newstr);
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optdistCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_optdistCb                                                    *
 *                                                                      *
 * Callback function for the distance units radio buttons.		*
 *                                                                      *
 * void cldhgtw_optdistCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * T. Lee/GSC		03/00	Set max search radius unit		*
 * H. Zeng/EAI		04/00	Set unit				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
	char	lstr[128], newstr[13];
        float   dist;
	int	ii;
/*---------------------------------------------------------------------*/
	_currDist = (int)which;

	sprintf ( lstr, "Maximum search radius (%s)", 
	_dunits [_currDist] );
 	NxmLabel_setStr (_cldhgtSlabel, lstr);

        switch(_currDist) {
           case 0: /* km */
              dist = _currDstm / 1000.0F;
              break;
           case 1: /* mi */
              dist = _currDstm * M2SM; 
              break;
           case 2: /* nm */
              dist = _currDstm * M2NM;
              break;
  
        }
	sprintf (newstr, "%-12.2f", dist);
        XmTextSetString (_distText, newstr);

	for (ii = 0; ii < MAX_DIST; ii++) {
            XmToggleButtonSetState ( _distRb[ii], False, False);
    	}
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_opttempCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_opttempCb                                                    *
 *                                                                      *
 * Callback function for the temperature units radio buttons.		*
 *                                                                      *
 * void cldhgtw_opttempCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    _currTemp = (int)which;
    for (ii = 0; ii < MAX_TEMP; ii++) {
        XmToggleButtonSetState ( _tempRb[ii], False, False);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_opthghtCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_opthghtCb                                                    *
 *                                                                      *
 * Callback function for the height units radio buttons.		*
 *                                                                      *
 * void cldhgtw_opthghtCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    _currHght = (int)which;
    for (ii = 0; ii < MAX_HGHT; ii++) {
        XmToggleButtonSetState ( _hghtRb[ii], False, False);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optdataCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_optdataCb                                                    *
 *                                                                      *
 * Callback function for the data selection type radio buttons.		*
 *                                                                      *
 * void cldhgtw_optdataCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * H. Zeng/EAI          01/00   Added interactions between two types    *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int 	ii;
/*---------------------------------------------------------------------*/
	_currData = (int)which;

	for (ii = 0; ii < MAX_DATA; ii++) {
            XmToggleButtonSetState ( _dataRb[ii], False, False);
        }

	if  ( _currData == 0 )  {
	    XtSetSensitive ( _distForm, True );
	}
	else {
	    XtSetSensitive ( _distForm, False );
            
/*
 * "Standard Atmosphere" must be paired with "Standard Search".
 */
            _currData2 = 0;
	    XmToggleButtonSetState (_dataRb2[_currData2], 
                                    True, True );
	    XmToggleButtonSetState (_dataRb2[1],
                                    False, False);
	}
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optdata2Cb ( Widget wid, long which, XtPointer cbs ) 
/************************************************************************
 * cldhgtw_optdata2Cb                                                   *
 *                                                                      *
 * Callback function for the data selection type 2 radio buttons.	*
 *                                                                      *
 * void cldhgtw_optdata2Cb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/00   Initial coding                          *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * M. Li/SAIC		12/02	Reset _dataRb				*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
	_currData2 = (int)which;
	for (ii = 0; ii < MAX_DATA; ii++) {
            XmToggleButtonSetState ( _dataRb2[ii], False, False);
        }

	if  ( _currData2 == 1 )  {

	    /*
             * "Moist Adiabatic" data must be paired with 
             * "Closest Station".
             */
            _currData = 0;
            XmToggleButtonSetState ( _dataRb[_currData], TRUE, False);
	    XmToggleButtonSetState ( _dataRb[1], False, False);
	    XtSetSensitive ( _distForm, True );
	}
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optlevelCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_optlevelCb                                                   *
 *                                                                      *
 * Callback function for the level radio buttons.		        *
 *                                                                      *
 * void cldhgtw_optlevelCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        04/00	Initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    _currLevel = (int)which;
    for (ii = 0; ii < MAX_LEVEL; ii++) {
        XmToggleButtonSetState ( _levelRb[ii], False, False);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optpixlCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_optpixlCb                                                    *
 *                                                                      *
 * Callback function for the pixel selection type radio buttons.	*
 *                                                                      *
 * void cldhgtw_optpixlCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
	_currPixl = (int)which;

	if  ( _currPixl == 0 )  {
	    XtSetSensitive ( _areaForm, False );
	    XtSetSensitive ( _pvalForm, False );
	}
	else {
	    XtSetSensitive ( _areaForm, True );
	    XtSetSensitive ( _pvalForm, True );
	}

	for (ii = 0; ii < MAX_PIXL; ii++) {
            XmToggleButtonSetState ( _pixlRb[ii], False, False);
    	}
}

/*=====================================================================*/
/* ARGSUSED */
void cldhgtw_optpvalCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * cldhgtw_optpvalCb                                                    *
 *                                                                      *
 * Callback function for the pixel value type radio buttons.		*
 *                                                                      *
 * void cldhgtw_optpvalCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs                XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    _currPval = (int)which;
    for (ii = 0; ii < MAX_PVAL; ii++) {
        XmToggleButtonSetState ( _pvalRb[ii], False, False);
    }
}

/*=====================================================================*/

void cldhgtw_rdTbl ( void )
/************************************************************************
 * cldhgtw_rdTbl                                                   	*
 *                                                                      *
 * This function reads the cloud height table.                         	*
 *                                                                      *
 * void cldhgtw_rdTbl()                                            	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI         04/00    initial coding                          *
 * L. Hinson/AWC       06/12    Add Table Option to set default for     *
 *                                Standard Search vs. Moist-Adiabatic   *
 ***********************************************************************/
{
int  i, nr = 0, value, iret, ier;
char buffer[256], name[22];
FILE *fp;

/*---------------------------------------------------------------------*/

        fp = cfl_tbop( CLDHGT_TBL, "nmap", &iret );
	if ( iret == 0 ) { 
	    cfl_tbnr( fp, &nr, &iret);
	}
	    
	if ( nr == 0 ) {
	    cfl_clos(fp, &ier);
        }
        else {
          i = 0;
          while ( i < nr ) {
            cfl_trln(fp, 256, buffer, &iret);
            if ( iret == 0 ) {
                sscanf(buffer, "%s %d", name, &value);

                if( strcmp(name, "DISTANCE")==0 ) {
                   _defDist = value;
                   _currDist = _defDist;
                }   
                else if( strcmp(name, "TEMPERATURE")==0 ) {
                   _defTemp = value;
                   _currTemp = _defTemp;
                }
                else if( strcmp(name, "HEIGHT")==0 ) {
                   _defHght = value;
                   _currHght = _defHght;
                }
                else if( strcmp(name, "MOIST_ADIABATIC")==0 ) {
                  _defData2 = value;
                  _currData2 = _defData2;
                }
                else if( strcmp(name, "MAX_SEARCH_RADIUS")==0 ) {
                   switch(_currDist) {
                      case 0: /* km */
                         _defDstm = (float)value * 1000.0F;
                         break;
                      case 1: /* mi */
                         _defDstm = (float)value / M2SM; 
                         break;
                      case 2: /* nm */
                         _defDstm = (float)value / M2NM;
                         break;
  
                   }
                   _currDstm = _defDstm;

                }
                else if( strcmp(name, "LEVEL")==0 ) {
                   _defLevel = value;
                   _currLevel = _defLevel;
                }
                else if( strcmp(name, "TYPE_OF_PIXEL")==0 ) {
                   _defPixl = value;
                   _currPixl = _defPixl;
                }
                else if( strcmp(name, "RADIUS_OF_AREA")==0 ) {
                   _defArea = value;
                   _currArea = _defArea;
                }
                else if( strcmp(name, "PIXEL_VALUE")==0 ) {
                   _defPval = value;
                   _currPval = _defPval;
                }
                else {
                }

            }
            i++;
	  }

	  cfl_clos(fp, &ier);

	}/* the end of else part */

}
/*=====================================================================*/

void cldhgtw_status ( void )
/************************************************************************
 * cldhgtw_status							*
 *                                                                      *
 * This function updates the cloud height status message.		*
 *                                                                      *
 * void cldhgtw_status()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC		04/00	initial coding				*
 * J. Wu/GSC       	05/01	free XmStrings    			*
 * T. Lee/SAIC		04/03	handle if error message maxed out 	*
 * J. Wu/SAIC       	01/04	call er_gnumerr & er_gerrmsg		*
 * J. Wu/SAIC       	02/04	change call to er_gerrmsg		*
 * T. Piper/SAIC	10/06	Increased errmsg to 513			*
 ***********************************************************************/
{
int		ii, nermsg, ier;
char		errmsg[513];
char		*sptr;
XmString	xmstr;
/*---------------------------------------------------------------------*/

	er_gnumerr ( &nermsg, &ier );

	if ( nermsg == 0 ) {
	    return;
	}

/*
 * Write status message.
 */	
	if ( nermsg_save == MXERST ) {

	    nermsg_save--;
	    er_gerrmsg ( &nermsg_save, errmsg, &ier );
	    sptr = (char *) strstr ( errmsg, "]" );
	    xmstr = XmStringCreateLocalized(sptr+2);
	    XmListAddItemUnselected(_statusList, xmstr, 0);
	    XmStringFree( xmstr );
	}
	else {
	    for ( ii = nermsg_save; ii < nermsg; ii++ ) {

	        er_gerrmsg ( &ii, errmsg, &ier );
	        sptr = (char *) strstr ( errmsg, "]" );
                xmstr = XmStringCreateLocalized(sptr+2);
	        XmListAddItemUnselected(_statusList, xmstr, 0);
	        XmStringFree( xmstr );
	    }
	}
}

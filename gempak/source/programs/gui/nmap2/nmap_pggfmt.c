#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "nmap_data.h"


#define CYCLE    0      /* CYCLE    */
#define GDATIM   1      /* GDATIM   */
#define HISTGRD  2      /* HISTGRD  */

#define MAXNOPT	     	20    /* max number of option menu items */

#define MAXNPARMS	50    /* max number of input parm options */
static	char	*_ggParms[MAXNPARMS];
static	int	_ggNparms;

struct optMenuStrc {
    int		current, deflt; 
    Widget	form;
    Widget	label;
    Widget	menu;
    Widget	pb[MAXNOPT];
};

static	struct	optMenuStrc	_prodStrc;

/*
 *  define the number of cycles and forecast hours for selection menu.
 */
static	int	_nCycs=4;	/* Number of cycles in menu.		*/
static	int	_nFhrs=13;	/* Number of 6-hr fcst hours in menu.	*/
static	char	_ggTmplt[129];	/* VGF template				*/
static	char	_inGDOUTF[128]={""};	/* initial restore GDOUTF value	*/
static	char	_inCNTRFL[128]={""};    /* initial restore CNTRFL value */
static	char	_curVGF[FILE_FULLSZ] = {""};
static	char	_pppp[] = { "PPPP" };

static Widget		_pggfmtWin;
static Widget           _productForm;
static Widget           _basicInfoFrame, _advanInfoFrame;
static Widget		_basicInfoForm, _advanInfoForm;
static WidgetList       _controlBtnW;
static WidgetList       _cycleMenuBtnW, _fcsthrMenuBtnW;

static Widget   *_ggTxtW;

/*
 *  define the graph-to-grid processing type labels and associated table paths.
 */
#define	GRPHGD_TBL	"grphgd.tbl"
#define	GRPHGD_PDF	"$GEMPAK/pdf/grphgd.pdf"

static	int	_nLbl;
static  char    *_lbl[MAXNOPT];
static  char    *_tbl[MAXNOPT];


/*
 *  private callback functions
 */
void pggfmt_ctlBtnCb	( Widget, long, XtPointer );
void pggfmt_cycleMenuCb ( Widget, XtPointer, XtPointer );
void pggfmt_fcsthrMenuCb( Widget, XtPointer, XtPointer );
void pggfmt_prodOptCb	( Widget, XtPointer, XtPointer );

/*
 *  private functions
 */
void pggfmt_buildCycles ( void );
void pggfmt_buildFhrs ( void );
void pggfmt_buildFiles ( void );
void pggfmt_buildGdattim ( void );
void pggfmt_createPulldownMenu ( Widget parent, Widget textwid, 
						int info_type );
void pggfmt_ipPutv ( int *iret );
void pggfmt_mkName ( char *infile, char *curname, char *suffix, 
					char *fname, int *iret );
void pggfmt_rdMaster ( int *iret );
void pggfmt_setPDF ( int *iret );
void pggfmt_setTime ( void );
void pggfmt_SetTxtW ( char *parm, char *value, int *iret );
void pggfmt_tblInit ( char *tbl, int *iret );


/************************************************************************
 * nmap_pggfmt.c							*
 *									*
 * This module defines a graph-to-grid popup window for product         *
 * generation.							        *
 *									*
 * CONTENTS:								*
 *	pggfmt_create()		create the graph-to-grid window	        *
 *	pggfmt_popup()		pop up the graph-to-grid window	        *
 *	pggfmt_popdown()	pop down the graph-to-grid window	*
 *									*
 *	pggfmt_isUp()		query if the window is up 		*
 *									*
 *      pggfmt_prodOptCb()      callback for product pulldown           *
 *	pggfmt_cycleMenuCb()	callback for "CYCLE" pulldown           *
 *      pggfmt_fcsthrMenuCb()   callback for "GDATIM"  pulldown         *
 *	pggfmt_ctlBtnCb()	callback for control buttons 		*
 *									*
 *	pggfmt_createPulldownMenu()    creates a pulldown menu		*
 *	pggfmt_setTime()	sets the current time			*
 ***********************************************************************/

/*=====================================================================*/

Widget pggfmt_create ( Widget parent )
/************************************************************************
 * pggfmt_create							*
 *									*
 * This function creates the graph-to-grid formatting popup window.    	*
 *									*
 * Widget pggfmt_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pggfmt_create	Widget	ID of the county list popup window	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          10/99   initial coding                          *
 * H. Zeng/EAI          12/99   visual enhancement for irix5&6          *
 * H. Zeng/EAI          03/00   added product pulldown menu             *
 * E. Safford/GSC	12/00	Add XtCallbackProc cast			*
 * T. Piper/GSC		 7/01	Freed flentry				*
 * T. Piper/SAIC	12/01	Freed label				*
 * H. Zeng/SAIC		04/05	minor modification on GUI		*
 * E. Safford/SAIC	05/05	free fontlist				*
 ***********************************************************************/
{
    WidgetList  label;
    Widget      pane, rowcol0, rowcol1, product_label;

    int		nn, ii, ier, toff = 10, loff = 5, toff2 = 10,
                txsize = 15, txadvsize = 30;

    char	*btnstrs[] = { "Advanced...", 
			       "Show Extensions", 
			       "MAKE GRID", 
			       "CANCEL" };
    XmFontListEntry flentry;
    XmFontList  fontlist;
    char  fontname[]=
	"-adobe-times-bold-r-normal-*-10-*-75-75-p-*-iso8859-1";
    Display     *dsp;
/*---------------------------------------------------------------------*/
/*
 * read in graph-to-grid master table to get product labels, etc.
 */
    pggfmt_rdMaster ( &ier );

/*
 * Set up parameters from PDF sources.
 */
    pggfmt_setPDF ( &ier );

/*
 * Allocate graph-to-grid text widgets.
 */
    _ggTxtW = (Widget *)malloc( (size_t)_ggNparms * sizeof ( Widget ) );

/*
 * create dialog shell
 */
    _pggfmtWin = XmCreateFormDialog(parent, "pggfmt_popup",
				    NULL, 0);
    XtVaSetValues(_pggfmtWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pggfmtWin),
		  XmNtitle, "GRAPH-to-GRID PROCESSING",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pggfmt_pane",
			    xmPanedWindowWidgetClass, _pggfmtWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

/*
 * create product choosing area.
 */
    _productForm = XtVaCreateWidget ("form",
				xmFormWidgetClass,  pane,
				NULL );

/*
 * PRODUCT
 */
    product_label = XtVaCreateManagedWidget("PRODUCT",
				     xmLabelWidgetClass,	_productForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

/*
 * Create product menu
 */
    nn = _nLbl;
    _prodStrc.deflt = 0;
    _prodStrc.current = _prodStrc.deflt;
    pgutls_createOptionMenu(_productForm, nn, (XtPointer)&_prodStrc.current, 
		    NULL, pggfmt_prodOptCb, &_prodStrc.form, &_prodStrc.label,
			     &_prodStrc.menu, _prodStrc.pb, _lbl);
 
    XtVaSetValues (_prodStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	product_label,
                   XmNleftOffset,       loff+65,
		   NULL);

    XtManageChild(_productForm);

/*
 *  create basic information frame and title
 */
    _basicInfoFrame = XtVaCreateWidget("frame",
				       xmFrameWidgetClass, pane,
                                       XmNshadowType,      XmSHADOW_IN,
				       NULL);

    XtVaCreateManagedWidget("Basic Info:",
				    xmLabelGadgetClass, _basicInfoFrame,
				    XmNchildType,	XmFRAME_TITLE_CHILD,
                                    XmNchildHorizontalAlignment,
                                                        XmALIGNMENT_CENTER,
				    XmNchildVerticalAlignment, 
				                        XmALIGNMENT_CENTER,
				    NULL);

/*
 * create basic information formatting area
 */
    _basicInfoForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      _basicInfoFrame,
				   NULL);

/*
 *  create label widgets
 */
    nn = _ggNparms;
    label = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    
/*
 * Base date or cycle
 */

    label[0] = XtVaCreateManagedWidget ( _ggParms[0],
				     xmLabelWidgetClass,	_basicInfoForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    rowcol0 = XtVaCreateWidget("",
			    xmRowColumnWidgetClass, _basicInfoForm,
			    XmNnumColumns,	    1,
			    XmNorientation,	    XmHORIZONTAL,
			    XmNradioBehavior,	    FALSE,
			    XmNpacking,		    XmPACK_TIGHT,
                            XmNtopAttachment,       XmATTACH_FORM,
			    XmNleftAttachment,	    XmATTACH_WIDGET,
                            XmNleftWidget,          label[0],
			    XmNleftOffset,	    loff+20,
                            XmNborderWidth,         0,
                            XmNmarginHeight,        0,
                            XmNmarginWidth,         0,
			    NULL);

    _ggTxtW[0] = XtVaCreateManagedWidget ("", 
					xmTextFieldWidgetClass, rowcol0,
					XmNcolumns,		txsize, 
					NULL);


    pggfmt_createPulldownMenu(rowcol0, _ggTxtW[0], CYCLE);

    XtManageChild(rowcol0);

/*
 *  FORECAST HOUR
 */
    label[1] = XtVaCreateManagedWidget ( _ggParms[1],
		     xmLabelWidgetClass,	_basicInfoForm,
		     XmNtopAttachment,		XmATTACH_FORM,
		     XmNtopOffset,		toff,
                     XmNleftAttachment,         XmATTACH_WIDGET,
                     XmNleftWidget,             rowcol0,
		     XmNleftOffset,	        loff+20,
		     NULL);

    rowcol1 = XtVaCreateWidget("",
			    xmRowColumnWidgetClass, _basicInfoForm,
			    XmNnumColumns,	    1,
			    XmNorientation,	    XmHORIZONTAL,
			    XmNradioBehavior,	    FALSE,
			    XmNpacking,		    XmPACK_TIGHT,
			    XmNtopAttachment,	    XmATTACH_FORM,
                            XmNleftAttachment,      XmATTACH_WIDGET,
			    XmNleftWidget,          label[1],
			    XmNleftOffset,	    loff+20,
                            XmNborderWidth,         0,
                            XmNmarginHeight,        0,
                            XmNmarginWidth,         0,
			    NULL);

    _ggTxtW[1] = XtVaCreateManagedWidget ("", 
					xmTextFieldWidgetClass, rowcol1,
					XmNcolumns,		txsize, 
					NULL); 

    pggfmt_createPulldownMenu(rowcol1, _ggTxtW[1], GDATIM);
    XtManageChild(rowcol1);

    XtManageChild(_basicInfoForm);
    XtManageChild(_basicInfoFrame);

/*
 *  create advanced information frame and title
 */
    _advanInfoFrame = XtVaCreateWidget("frame",
			xmFrameWidgetClass, pane,
                        XmNshadowType,      XmSHADOW_IN,
			NULL);

    XtVaCreateManagedWidget("Advanced Info:",
			    xmLabelGadgetClass, _advanInfoFrame,
			    XmNchildType,	XmFRAME_TITLE_CHILD,
                            XmNchildHorizontalAlignment, XmALIGNMENT_CENTER,
			    XmNchildVerticalAlignment,   XmALIGNMENT_CENTER,
			    NULL);

/*
 * create advanced information formatting area
 */
    _advanInfoForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      _advanInfoFrame,
				   NULL);

    dsp = XtDisplay (_pggfmtWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

/*
 *  GFUNC
 */
    label[2] = XtVaCreateManagedWidget ( _ggParms[2],
			xmLabelWidgetClass,	_advanInfoForm,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		toff,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          loff,
			XmNfontList,            fontlist,
			NULL);

    _ggTxtW[2] = XtVaCreateManagedWidget ("", 
			xmTextFieldWidgetClass, _advanInfoForm,
			XmNcolumns,		txadvsize, 
                        XmNtopAttachment,       XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_WIDGET,
                        XmNleftWidget,          label[2],
			XmNleftOffset,	        loff+34,
			XmNfontList,            fontlist,
			NULL);

    label[3] = XtVaCreateManagedWidget ( _ggParms[3],
			xmLabelWidgetClass,	_advanInfoForm,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		toff,
                        XmNleftAttachment,      XmATTACH_WIDGET,
			XmNleftWidget,		_ggTxtW[2],
                        XmNleftOffset,          loff,
			XmNfontList,            fontlist,
			NULL);

    _ggTxtW[3] = XtVaCreateManagedWidget ("", 
			xmTextFieldWidgetClass, _advanInfoForm,
			XmNcolumns,		txadvsize, 
                        XmNtopAttachment,       XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_WIDGET,
                        XmNleftWidget,          label[3],
			XmNleftOffset,	        loff+34,
			XmNfontList,            fontlist,
			NULL);


    for ( ii = 4; ii < _ggNparms; ii++ )  {

        _ggTxtW[ii] = XtVaCreateManagedWidget ("", 
			xmTextFieldWidgetClass, _advanInfoForm,
			XmNcolumns,		txadvsize,
                        XmNtopAttachment,       XmATTACH_WIDGET,
                        XmNtopWidget,           _ggTxtW[ii-2],
                        XmNleftAttachment,      XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget,          _ggTxtW[ii-2],
			XmNfontList,            fontlist,
			NULL);

        label[ii] = XtVaCreateManagedWidget ( _ggParms[ii],
			xmLabelWidgetClass,	_advanInfoForm,
			XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget,		_ggTxtW[ii],
                        XmNtopOffset,           toff2,
                        XmNleftAttachment,      XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget,          label[ii-2],
			XmNfontList,            fontlist,
			NULL);
    }


    XtManageChild(_advanInfoForm);
    XtFree((XtPointer)label);

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    _controlBtnW = (WidgetList) XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 0, "pggfmt_ctlBtn", nn, btnstrs, 
		     (XtCallbackProc)pggfmt_ctlBtnCb, _controlBtnW);  

    XtManageChild(pane);

/*
 *  build cycle and forecast hour pulldowns
 */
    pggfmt_buildCycles ();
    pggfmt_buildFhrs ();

/*
 *  initialize form with default (first) information
 */
    pggfmt_tblInit ( _tbl[_prodStrc.deflt], &ier );

    XmFontListFree( fontlist );

    return(_pggfmtWin);
    
}

/*=====================================================================*/

void pggfmt_popup ( void )
/************************************************************************
 * pggfmt_popup								*
 *									*
 * This function pops up the graph-to-grid formatting popup window.    	*
 *									*
 * void pggfmt_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/99	initial coding				*
 * S. Law/GSC		05/00	added parameter to pgfilw_getFileName	*
 * D.W.Plummer/NCEP	 8/00	added ip_ library calls			*
 * D.W.Plummer/NCEP	 9/01	added call to er_stat			*
 ***********************************************************************/
{
    int		respnd, iperr, ier;
    int		level, bufflg, dttmflg;
    char	vgfn[FILE_FULLSZ];
/*---------------------------------------------------------------------*/
/*
 * G2G parameters will be passed via IP library. 
 */
    ip_init( &respnd, &iperr );

/*
 * set the error bufferring scheme
 * different level may be set by the application
 */
    level   = 0;
    bufflg  = 1;
    dttmflg = 1;
    er_stat( &level, &bufflg, &dttmflg, &ier);

/*
 * Set the current time for "CYCLE" text field and pulldown menu. 
 * Because the function is time sensitive, it is purposely put here 
 * instead of in pggfmt_create(). 
 */
    pggfmt_buildCycles();

    pggfmt_tblInit( _tbl[_prodStrc.current], &ier );

/*
 *  Re-do the time calculations if the VG filename has changed.
 */
    pgfilw_getFileName (TRUE, vgfn);
    if ( strcmp ( vgfn, _curVGF ) != 0 )  pggfmt_setTime();
    strcpy ( _curVGF, vgfn );

    XtManageChild (_pggfmtWin);
}

/*=====================================================================*/

void pggfmt_popdown ( void ) 
/************************************************************************
 * pggfmt_popdown							*
 *									*
 * This function pops down the graph-to-grid formatting window.	       	*
 *									*
 * void pggfmt_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/99	initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pggfmtWin)) {
	XtUnmanageChild (_pggfmtWin);
    }
}

/*=====================================================================*/

Boolean pggfmt_isUp ( void ) 
/************************************************************************
 * pggfmt_isUp								*
 *									*
 * This function queries whether the graph-to-grid formatting window    *
 * is up.	                                                        *
 *									*
 * Boolean pggfmt_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pggfmt_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/99	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pggfmtWin));
}

/*=====================================================================*/
/* ARGSUSED */
void pggfmt_prodOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfmt_prodOptCb							*
 *									*
 * This is the callback function for product option menu.		*
 *									*
 * void pggfmt_prodOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          03/00   initial coding                          *
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/
    _prodStrc.current = (long)clnt;
    pggfmt_tblInit( _tbl[_prodStrc.current], &ier );
}

/*=====================================================================*/
/* ARGSUSED */
void pggfmt_cycleMenuCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfmt_cycleMenuCb				       			*
 *									*
 * Callback function for "CYCLE" pulldown menu buttons.			*
 *					        			*
 * void pggfmt_cycleMenuCb (wid, clnt, call)		       		*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	Xtpointer	client data				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	        10/99	initial coding				*
 * D.W.Plummer/NCEP     11/99   added call to pggfmt_buildFhrs		*
 ***********************************************************************/
{   char	*ptext;
    XmString	xmstr;
    XtPointer	userdata;
    Widget	textwid;
/*---------------------------------------------------------------------*/
/*
 *  Set "CYCLE" text field value according to the chosen  pulldown menu 
 *  button.
 */
    XtVaGetValues (wid, 
		   XmNlabelString,   &xmstr, 
		   XmNuserData,	     &userdata, 
		   NULL);

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);

    XmStringFree(xmstr);

    textwid = (Widget) userdata;
    XtVaSetValues (textwid, XmNvalue, ptext, NULL);

    pggfmt_buildGdattim ();

    pggfmt_buildFiles ();

    XtFree (ptext);

}

/*=====================================================================*/
/* ARGSUSED */
void pggfmt_fcsthrMenuCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfmt_fcsthrMenuCb					       		*
 *									*
 * Callback function for "GDATIM" pulldown menu buttons.	       	*
 *									*
 * void pggfmt_fcsthrMenuCb (wid, clnt, call)			       	*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	client data				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/99	initial coding				*
 * D.W.Plummer/NCEP     11/99   added call to pggfmt_buildFiles         *
 ***********************************************************************/
{
    char	*ptext;
    XmString	xmstr;
    XtPointer	userdata;
    Widget	textwid;
/*---------------------------------------------------------------------*/
/*
 * Set the "GDATIM" text field value according to the chosen pulldown
 * menu item.
 */
    XtVaGetValues (wid, 
		   XmNlabelString,   &xmstr, 
		   XmNuserData,	     &userdata, 
		   NULL);

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);

    XmStringFree(xmstr);

    textwid = (Widget) userdata;
    XtVaSetValues (textwid, XmNvalue, ptext, NULL);

    pggfmt_buildGdattim ();

    pggfmt_buildFiles ();

    XtFree (ptext);

}

/*=====================================================================*/
/* ARGSUSED */
void pggfmt_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfmt_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of graph-to-grid	*
 * popup window.							*
 *									*
 * void pggfmt_ctlBtnCb (wid, which, call)				*
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
 * Log:                                                                 *
 * H. Zeng/EAI          10/99   initial coding           	        *
 * S. Jacobs/NCEP	11/99	Save and reset the current map proj	*
 * H. Zeng/EAI          04/00   changed cursor name           	        *
 * T. Lee/GSC		06/00	Flushed error messages			*
 * D.W.Plummer/NCEP	 9/01	Flushed error messages w/o iret check	*
 * T. Lee/SAIC          10/01   Added fill types to GCFILL calling seq. *
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * D.W.Plummer/NCEP	03/05	G_MALLOC grid and hist arrays		*
 * M. Li/SAIC		04/05	Modified ggdriv to process vector	*
 * D.W.Plummer/NCEP	07/05	Replace contour code w/ call to gdpltb	*
 * T. Lee/SAIC		08/05	Attached PATH to output file		*
 * D.W.Plummer/NCEP	09/05	Add error chk from ggdriv; add DG_NEND	*
 * M. Li/SAIC		09/05	pgggc_update -> gg_update		*
 * m.gamazaychikov/SAIC	12/05	Change CS for gg_update			*
 * m.gamazaychikov/SAIC	01/06	Added check for a blank CONTROL file	*
 * m.gamazaychikov/SAIC	02/06	Add check for n/err rtrn from gg_update *
 * S. Jacobs/NCEP	10/12	Changed value of IJSKIP from yes to no	*
 ***********************************************************************/
{
#define	LEN	80
    int		ier, iret, nc, rspflg=G_FALSE, iret_ggdriv, icntr, ierm;
    XmString    label;
    char	info[FILE_FULLSZ];
    char        gdfile_tmp[LEN], path[LEN], gdfile[LEN], type[LEN];
    char        path_name[LEN], cntrfl[LEN], bounds[LEN], oabnd[LEN];
    char        cint[LEN], line[LEN];
    char        fint[LEN], fline[LEN], clrbar[LEN];
    char 	gdattim[LEN], glevel[LEN], gvcord[LEN], wind[LEN];
    char 	gfunc[LEN];

    char	svproj[4], catmap[256];

    float           llx, lly, urx, ury;
    float	svang1, svang2, svang3, svlat1, svlon1, svlat2, svlon2;

    float	*grid, *grid1, *hist, *work1, *work2, *work3, *buffer;

    int		kx, ky, npoints, cur_layer, plt_ext;
    int		logF=G_FALSE, logT=G_TRUE, iframe=2;
    char	*cptr;
    char        ctlStrs[2][16] = {"Advanced...", "    Basic...   " };
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	

/* 
 * Toggle between Advanced Info and Basic Info  
 */
        if( XtIsManaged(_advanInfoFrame) ) {
            XtUnmanageChild(_advanInfoFrame);
            label = XmStringCreateLocalized( ctlStrs[0] );
        }
        else {
            XtManageChild(_advanInfoFrame);
            label = XmStringCreateLocalized( ctlStrs[1] );
	}

        XtVaSetValues (_controlBtnW[0],
                       XmNlabelString,      label,
                       NULL);
        XmStringFree(label);
	break;

      case 1:	

/* 
 * Display contour line extensions.
 */
        cur_layer = pglayer_getCurLayer ();
        plt_ext = G_TRUE;
 
        pggfmt_GetTxtW ( "CATMAP", catmap, &ier);
        in_catminp ( catmap, &ier );
 
        gqbnd ( sys_D, &llx, &lly, &urx, &ury, &ier, strlen(sys_D) );
        pgutls_refresh (llx, ury, urx, lly, &ier);

	gg_update( cvg_getworkfile(), "\0", &cur_layer, catmap,
                   &plt_ext, &iret );

	break;

      case 2:	

/*
 * Make the grid.
 *
 * First, save the current map projection.
 */
	gqmprj ( svproj, &svang1, &svang2, &svang3,
		 &svlat1, &svlon1, &svlat2, &svlon2,
		 &ier, sizeof(svproj) );
	svproj[3] = '\0';

        mcanvw_setCursor(CURS_BUSY);

/*
 * Generate ".info" file... put in directory where output grid 
 * is going unless a path is already explicitly specified.
 */
	info[0] = '\0';
        pggfmt_GetTxtW( "CNTRFL", cntrfl, &iret );

        cst_lstr (cntrfl, &icntr, &iret);
                                                                                       
        if ( icntr != 0 ) {
	   cptr = strchr ( cntrfl, '/' );

	   if ( cptr == (char *)NULL )  {

              pgfilw_getPathName( path_name );

	      strcpy ( info, path_name );
	      strcat ( info, cntrfl );

	   }
	   else  {
	      strcpy ( info, cntrfl );
	   }

           pggfmt_SetTxtW( "CNTRFL", info, &iret );

           cur_layer = pglayer_getCurLayer ();
           plt_ext = G_FALSE;
 
           pggfmt_GetTxtW ( "CATMAP", catmap, &ier);
           in_catminp ( catmap, &ier );
 
           gqbnd ( sys_D, &llx, &lly, &urx, &ury, &ier, strlen(sys_D) );
           pgutls_refresh (llx, ury, urx, lly, &ier);

	   gg_update( cvg_getworkfile(), info, &cur_layer, catmap,
                      &plt_ext, &iret );

	   if ( iret == 0 )  {

/*
 *  Add PATH to GDOUTF and save (temporarily)
 */
              pggfmt_GetTxtW( "GDOUTF", gdfile, &iret );
              pggfmt_GetTxtW( "PATH", path, &iret );
              if ( strlen(path) == (size_t)0 )
                 sprintf( gdfile_tmp, "./%s", gdfile );
              else
                 sprintf( gdfile_tmp, "%s/%s", path, gdfile );
              pggfmt_SetTxtW( "GDOUTF", gdfile_tmp, &iret );

/*
 *  Generate ".grd" GEMPAK file
 */
	      pggfmt_ipPutv ( &iret );
              pggfmt_GetTxtW( "BOUNDS", bounds, &iret );
	      strcpy ( oabnd, "OABND" );
	      ip_putv ( oabnd, bounds, &ier, 
	   	     strlen(oabnd), strlen(bounds) );

	      npoints = LLMXTG;
	      G_MALLOC ( grid, float, npoints, "Unable to allocate grid array." );
	      G_MALLOC ( grid1, float, npoints, "Unable to allocate grid1 array." );
	      G_MALLOC ( hist, float, npoints, "Unable to allocate hist array." );
	      G_MALLOC ( work1, float, npoints, "Unable to allocate work1 array." );
	      G_MALLOC ( work2, float, npoints, "Unable to allocate work2 array." );
	      G_MALLOC ( work3, float, npoints, "Unable to allocate work3 array." );
	      G_MALLOC ( buffer, float, npoints, "Unable to allocate work3 buffer." );

              ggdriv( grid, grid1, &kx, &ky, hist, work1, work2, work3, buffer,
 	  	      &rspflg, &iret_ggdriv );
 
/*
 *  Reset CNTRFL and GDOUTF back to what they were originally.
 */
              pggfmt_SetTxtW( "CNTRFL", cntrfl, &ier );
              pggfmt_SetTxtW( "GDOUTF", gdfile, &ier );

/*
 * Reset the saved map projection.
 */
	      gsmprj ( svproj, &svang1, &svang2, &svang3,
		      &svlat1, &svlon1, &svlat2, &svlon2,
		      &ier, strlen(svproj) );

	      NxmErr_update();

              pggfmt_GetTxtW( "TYPE", type, &iret );

	      cst_lcuc ( type, type, &ier );
	      cst_ldsp ( type, type, &nc, &ier );
	      if ( iret_ggdriv == G_NORMAL )  {
	      if ( strncmp(type,"B",1) == 0 || strncmp(type,"A",1) == 0 )  {

/*
 * Plot winds here...
 */
	         gdpstp ( "GDFILE", gdfile_tmp, &ier, strlen("GDFILE"), strlen(gdfile_tmp) );
	         gdpstp ( "GDPFUN", "WND", &ier, strlen("GDPFUN"), strlen("WND") );

                 pggfmt_GetTxtW( "GDATTIM", gdattim, &iret );
                 pggfmt_GetTxtW( "GLEVEL", glevel, &iret );
                 pggfmt_GetTxtW( "GVCORD", gvcord, &iret );
                 pggfmt_GetTxtW( "TYPE", type, &iret );
                 pggfmt_GetTxtW( "WIND", wind, &iret );
	         gdpstp ( "GDATTIM", gdattim, &ier, strlen("GDATTIM"), strlen(gdattim) );
	         gdpstp ( "GLEVEL", glevel, &ier, strlen("GLEVEL"), strlen(glevel) );
	         gdpstp ( "GVCORD", gvcord, &ier, strlen("GVCORD"), strlen(gvcord) );
	         gdpstp ( "TYPE", type, &ier, strlen("TYPE"), strlen(type) );
	         gdpstp ( "WIND", wind, &ier, strlen("WIND"), strlen(wind) );

	         gdpstp ( "MAP", "0", &ier, strlen("MAP"), strlen("0") );
	         gdpstp ( "LATLON", "0", &ier, strlen("LATLON"), strlen("0") );

	         gdpstt ( "PLOT_MAP", &logF, &ier, strlen("PLOT_MAP") );
	         gdpstt ( "VERBOSE", &logT, &ier, strlen("VERBOSE") );
	         gdpstt ( "CLEAR", &logF, &ier, strlen("CLEAR") );

	         gdpltb ( &iframe, " ", &ier, strlen(" ") );
              }
	      else  {

/*
 *  Draw contours here...
 */
	         gdpstp ( "GDFILE", gdfile_tmp, &ier, strlen("GDFILE"), strlen(gdfile_tmp) );
                 pggfmt_GetTxtW( "GFUNC", gfunc, &iret );
	         gdpstp ( "GDPFUN", gfunc, &ier, strlen("GDPFUN"), strlen(gfunc) );

                 pggfmt_GetTxtW( "GDATTIM", gdattim, &iret );
                 pggfmt_GetTxtW( "GLEVEL", glevel, &iret );
                 pggfmt_GetTxtW( "GVCORD", gvcord, &iret );
                 pggfmt_GetTxtW( "FINT", fint, &iret );
                 pggfmt_GetTxtW( "FLINE", fline, &iret );
	         pggfmt_GetTxtW( "CLRBAR", clrbar, &iret );
                 pggfmt_GetTxtW( "CINT", cint, &iret );
                 pggfmt_GetTxtW( "LINE", line, &iret );

	         gdpstp ( "GDATTIM", gdattim, &ier, strlen("GDATTIM"), strlen(gdattim) );
	         gdpstp ( "GLEVEL", glevel, &ier, strlen("GLEVEL"), strlen(glevel) );
	         gdpstp ( "GVCORD", gvcord, &ier, strlen("GVCORD"), strlen(gvcord) );
	         gdpstp ( "FINT", fint, &ier, strlen("FINT"), strlen(fint) );
	         gdpstp ( "FLINE", fline, &ier, strlen("FLINE"), strlen(fline) );
	         gdpstp ( "CLRBAR", clrbar, &ier, strlen("CLRBAR"), strlen(clrbar) );
	         gdpstp ( "CINT", cint, &ier, strlen("CINT"), strlen(cint) );
	         gdpstp ( "LINE", line, &ier, strlen("LINE"), strlen(line) );
	         gdpstp ( "IJSKIP", "no", &ier, strlen("IJSKIP"), strlen("no") );
	         gdpstp ( "SCALE", "0", &ier, strlen("SCALE"), strlen("0") );

	         gdpstp ( "MAP", "0", &ier, strlen("MAP"), strlen("0") );
	         gdpstp ( "LATLON", "0", &ier, strlen("LATLON"), strlen("0") );

	         gdpstt ( "PLOT_MAP", &logF, &ier, strlen("PLOT_MAP") );
	         gdpstt ( "VERBOSE", &logF, &ier, strlen("VERBOSE") );
	         gdpstt ( "CLEAR", &logF, &ier, strlen("CLEAR") );

	         type[0] = '\0';
	         if ( strlen(cint) > (size_t)0 )  strcat ( type, "c" );
	         if ( strlen(fint) > (size_t)0 )  strcat ( type, "f" );
	         gdpstp ( "TYPE", type, &ier, strlen("TYPE"), strlen(type) );
	         gdpltb ( &iframe, " ", &ier, strlen(" ") );

	      }
	      dg_nend ( &ier );
	   }

	   G_FREE ( grid, float );
           G_FREE ( grid1, float );
           G_FREE ( hist, float );
           G_FREE ( work1, float );
           G_FREE ( work2, float );
           G_FREE ( work3, float );
           G_FREE ( buffer, float );

           }
           NxmErr_update();
           pggfmt_SetTxtW( "CNTRFL", cntrfl, &ier );

          } else {
                                                                                       
           ierm = -14;
           er_wmsg ( "GRPHGD", &ierm, " ", &ier, strlen("GRPHGD"), strlen(" ") );
           NxmErr_update();
                                                                                       
        }

/*
 *  Flush buffer
 */
	geplot ( &iret );

/*
 *  Redraw VG file
 */
        cvg_redraw ( cvg_getworkfile(), &iret );

        mcanvw_setCursor(CURS_DEFAULT);

	break;

      case 3:  

/* 
 * Cancel 
 */
	pggfmt_popdown ();
	break;
    }
}

/*=====================================================================*/

void pggfmt_createPulldownMenu ( Widget parent, Widget textwid, 
							int info_type )
/************************************************************************
 * pggfmt_createPulldownMenu						*
 *									*
 * Creates a pulldown menu.		                                *
 *									*
 * void pggfmt_createPulldownMenu ( parent, textwid, info_type )	*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *      textwid         Widget  the associated text field widget        *
 *	info_type	int	type of information			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		        NONE 				                *
 **									*
 * Log:									*
 * H. Zeng/EAI		10/99	initial coding				*
 * M. Li/SAIC		12/01	arrow -> menu_arrow			*
 ***********************************************************************/
{
    Widget	menub, cascade, menu;
    int		iret;
    XmString	xmstr;
    Pixel	fg, bg;
    long	ii, ignore;
    char	filename[FILE_FULLSZ], temp[20];
    static Pixmap	menu_pxm;
    static Boolean	first = TRUE;
/*---------------------------------------------------------------------*/
/*
 * create menu
 */
    menub = XmCreateMenuBar (parent, "", NULL, 0);

    XtVaSetValues (menub, 
		   XmNmarginHeight,		0,
		   XmNmarginWidth,		0,
		   XmNborderWidth,		0,
		   XmNpacking,		        XmPACK_TIGHT,
		   XmNwidth,			5,
		   XmNhighlightThickness,	1,
		   XmNshadowThickness,		1,
		   NULL);

    menu  = XmCreatePulldownMenu (menub, "", NULL, 0);

    cascade = XtVaCreateManagedWidget ("", 
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

	if ( iret == 0 ) {
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

    switch(info_type) {
           case CYCLE :
              _cycleMenuBtnW = (WidgetList) XtMalloc( (size_t)_nCycs*sizeof(Widget) );
              for(ii = 0; ii < (long)_nCycs; ii++) {
     	         xmstr = XmStringCreateLocalized ("yymmdd/hh00");
	         _cycleMenuBtnW[ii] = XtVaCreateManagedWidget ("", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	   xmstr,
			     		  XmNuserData,		   textwid,  
					  NULL);

	         XtAddCallback (_cycleMenuBtnW[ii], XmNactivateCallback,
		                 (XtCallbackProc) pggfmt_cycleMenuCb,
		                 (XtPointer) ii);
              XmStringFree(xmstr);
	      }

              break;

           case GDATIM :
              _fcsthrMenuBtnW = (WidgetList) XtMalloc( (size_t)_nFhrs*sizeof(Widget) );
              for(ii = 0; ii < (long)_nFhrs; ii++) {
                 sprintf(temp, "yymmdd/hh00f%03d", 6*(int)ii);
     	         xmstr = XmStringCreateLocalized ( temp );
	         _fcsthrMenuBtnW[ii] = XtVaCreateManagedWidget ("", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	   xmstr,
			     		  XmNuserData,		   textwid,  
					  NULL);

	         XtAddCallback (_fcsthrMenuBtnW[ii], XmNactivateCallback,
		                 (XtCallbackProc) pggfmt_fcsthrMenuCb,
		                 (XtPointer) ii);
              XmStringFree(xmstr);
	      }

              break;
    }
    XtManageChild(menub);
}

/*=====================================================================*/

void pggfmt_buildCycles ( void )
/************************************************************************
 * pggfmt_buildCycles							*
 *									*
 * Sets the current time for "CYCLE" pulldown menu items and text field	*
 *									*
 * void pggfmt_buildCycles ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          10/99   initial coding                          *
 * D.W.Plummer/NCEP     11/99   re-worked for parsing of vgf filename   *
 * A. Hardy/NCEP	 6/03   added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{
    int         ii, iret;
    int         cyear, cmon, cday, chour, cmin, csec, julian;
    int         time[5];
    int         tintv = 720, itype = 1;
    char        **item, tmzn[4]; 
    dattm_t	defdat, dattim;
    XmString    xmstr;
    Boolean	found;
/*---------------------------------------------------------------------*/
/*
 *  Allocate memory.
 */
    item = (char **)malloc( (size_t)_nCycs * sizeof(char*) );
    for ( ii = 0; ii < _nCycs; ii++ )  {
	item[ii] = (char *)malloc( sizeof(char) * DTTMSZ );
    }

/*
 *  Set the current time for "CYCLE" text field and pulldown menu items.
 *  There are _nCycs positions for CYCLE times... 
 *  in this case, starting w/ 12 hrs forward and working backwards.
 */
    css_date ( &itype, &cyear, &cmon, &cday, &chour, &cmin, &csec, &julian, 
               tmzn, &iret);

    time[0]=cyear%100; time[1]=cmon; time[2]=cday; time[3]=chour; time[4]=cmin;

    ti_addm(time, &tintv, time, &iret);

    for ( ii = 0; ii < _nCycs; ii++ )  {
	sprintf( item[ii], "%02d%02d%02d/%02d00",
		time[0], time[1], time[2], (time[3]/12)*12 );
        xmstr = XmStringCreateLocalized ( item[ii] );
        XtVaSetValues( _cycleMenuBtnW[ii], XmNlabelString, xmstr, NULL );
        XmStringFree( xmstr ); 
	ti_subm( time, &tintv, time, &iret );
    }
    sprintf ( dattim, "%sf000", item[0] );
    strcpy ( defdat, dattim );

    defdat[11] = '\0';
    pggfmt_SetTxtW( "CYCLE", defdat, &iret );

    found = FALSE;
    ii = 0;
    while ( !found && ii < _nCycs )  {
	found = (Boolean)(( strcmp( item[ii], defdat ) == 0 ));
	ii++;
    }
    if ( !found )  {
        xmstr = XmStringCreateLocalized ( defdat );
        XtVaSetValues( _cycleMenuBtnW[_nCycs-1], XmNlabelString, xmstr, NULL );
        XmStringFree( xmstr ); 
    }

    pggfmt_SetTxtW( "FCST_HR", &(dattim[11]), &iret );

/*
 *  Free memory.
 */
    for ( ii = 0; ii < _nCycs; ii++ )  free ( item[ii] );
    free ( item );
}

/*=====================================================================*/

void pggfmt_setTime ( void )
/************************************************************************
 * pggfmt_setTime							*
 *									*
 * Sets the current time for "CYCLE" pulldown menu items and text field	*
 *									*
 * void pggfmt_setTime ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          10/99   initial coding                          *
 * D.W.Plummer/NCEP     11/99   re-worked for parsing of vgf filename   *
 * S. Law/GSC		05/00	added parameter to pgfilw_getFileName	*
 ***********************************************************************/
{
    int         len, iret;
    dattm_t	defdat, dattim;
    char        cycle[20], fhr[8], *delim = { "_" };
    char	vgfn[FILE_NAMESZ], gfunc[13];
/*---------------------------------------------------------------------*/
/*
 *  Set the current time for "CYCLE" text field and pulldown menu items.
 *  There are _nCycs positions for CYCLE times... 
 *  in this case, starting w/ 12 hrs forward and working backwards.
 */
    pggfmt_GetTxtW( "CYCLE", cycle, &iret );
    pggfmt_GetTxtW( "FCST_HR", fhr, &iret );
    sprintf ( dattim, "%s%s", cycle, fhr );
    strcpy ( defdat, dattim );
    pggfmt_SetTxtW( "GDATTIM", dattim, &iret );

/*
 *  If filename fits template, use it's time, etc., as defaults.
 */
    pgfilw_getFileName (FALSE, vgfn);

    if ( strlen(_ggTmplt) != (size_t)0  &&  strlen(vgfn) != (size_t)0 )  {

/*
 *  Both template and filename exist... try to decode.
 */
	strcpy ( gfunc, "0123456789AB" );
	st_gtst( _ggTmplt, _pppp, delim, vgfn, gfunc, &len, &iret,
		strlen(_ggTmplt), strlen(_pppp), strlen(delim), 
		strlen(vgfn), strlen(gfunc) );

	if ( iret == 0 )  {

            cfl_mdat ( vgfn, _ggTmplt, defdat, dattim, &iret );

	    if ( iret == 0 )  {
	
/*
 *  Set GFUNC, CYCLE and FCST_HR
 */
        	pggfmt_SetTxtW( "GFUNC", gfunc, &iret );
    		pggfmt_SetTxtW( "GDATTIM", dattim, &iret );
    		pggfmt_SetTxtW( "FCST_HR", &(dattim[11]), &iret );
    		dattim[11] = '\0';
    		pggfmt_SetTxtW( "CYCLE", dattim, &iret );

	    }
	}
    }
    pggfmt_buildFiles ();
}

/*=====================================================================*/

void pggfmt_buildFhrs ( void )
/************************************************************************
 * pggfmt_buildFhrs							*
 *									*
 * Creates the forecast hour pulldowns.					*
 *									*
 * void pggfmt_buildFhrs ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/99	initial coding				*
 ***********************************************************************/
{
    int         ii;
    XmString    xmstr;
    char	fcsthr[20];
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < _nFhrs; ii++ )  {
       sprintf( fcsthr, "f%03d", 6*ii );
       xmstr = XmStringCreateLocalized ( fcsthr );
       XtVaSetValues(_fcsthrMenuBtnW[ii], XmNlabelString, xmstr, NULL);
       XmStringFree(xmstr);
    }
}

/*=====================================================================*/

void pggfmt_buildGdattim ( void )
/************************************************************************
 * pggfmt_buildGdattim							*
 *									*
 * Creates the GDATTIM parameter.					*
 *									*
 * void pggfmt_buildGdattim ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 ***********************************************************************/
{
int     ier;
char	cycle[20], fhr[8], gdattim[20];
/*---------------------------------------------------------------------*/
    pggfmt_GetTxtW( "CYCLE", cycle, &ier );
    pggfmt_GetTxtW( "FCST_HR", fhr, &ier );
    sprintf ( gdattim, "%s%s", cycle, fhr );
    pggfmt_SetTxtW( "GDATTIM", gdattim, &ier );
}

/*=====================================================================*/

void pggfmt_buildFiles ( void )
/************************************************************************
 * pggfmt_buildFiles							*
 *									*
 * Creates the GDOUTF and CNTRFL filenames.				*
 *									*
 * void pggfmt_buildFiles ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/99	initial coding				*
 ***********************************************************************/
{
int     ier;
char	fname[128], curname[128], suffix[8];
/*---------------------------------------------------------------------*/

    strcpy( suffix, ".grd" );
    pggfmt_GetTxtW( "GDOUTF", curname, &ier );
    pggfmt_mkName ( _inGDOUTF, curname, suffix, fname, &ier );

    pggfmt_SetTxtW( "GDOUTF", fname, &ier );

    strcpy( suffix, ".info" );
    pggfmt_GetTxtW( "CNTRFL", curname, &ier );
    pggfmt_mkName ( _inCNTRFL, curname, suffix, fname, &ier );

    pggfmt_SetTxtW( "CNTRFL", fname, &ier );

}

/*=====================================================================*/

void pggfmt_mkName ( char *infile, char *curname, char *suffix, 
						char *fname, int *iret )
/************************************************************************
 * pggfmt_mkName							*
 *									*
 * Makes a filename based on an input template string (possibly NULL).	*
 *									*
 * void pggfmt_mkName ( infile, curname, suffix, fname, iret )		*
 *									*
 * Input parameters:							*
 *  *infile	char	Input file template string			*
 *  *curname	char	Current value of filename			*
 *  *suffix	char	Desired suffix, if necessary			*
 *									*
 * Output parameters:							*
 *  *fname	char	Output filename string				*
 *  *iret	int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 * S. Law/GSC		05/00	added parameter to pgfilw_getFileName	*
 ***********************************************************************/
{
    int		len, ier;
    char	gdatim[20], cycle[20], fcsthr[20];
    char	*pptr, vgfn[FILE_NAMESZ], gfunc[13];
/*---------------------------------------------------------------------*/

    *iret = 0;

    pggfmt_GetTxtW(   "CYCLE",  cycle, iret );
    pggfmt_GetTxtW( "FCST_HR", fcsthr, iret );
    pggfmt_GetTxtW(   "GFUNC",  gfunc, iret );
    cst_lstr ( gfunc, &len, &ier );
    gfunc[len] = '\0';

    if ( strlen( infile ) != (size_t)0 )  {

/*
 *  Build current GDATIM value from CYCLE and FCSTHR;
 *  Replace any date/time reference in template w/ gdatim.
 */
        sprintf( gdatim, "%s%s", cycle, fcsthr );
        cfl_mnam ( gdatim, infile, fname, iret );

/*
 *  Replace any 'PPPP' reference in template w/ gfunc.
 */
	cst_rpst ( fname, _pppp, gfunc, fname, iret );

    }
    else if ( strlen( curname ) == (size_t)0 )  {

/*
 *  Use .vgf filename, or if not indicated, use 'none'.
 */

	pgfilw_getFileName (FALSE, vgfn);

	if ( vgfn[0] != '\0' )  {
	    pptr = strrchr( vgfn, '.' );
	    if ( pptr != (char *)NULL ) {
	        pptr--;
	    }
	    else {
	        pptr = vgfn + strlen(vgfn);
	    }
	    len = pptr - vgfn + 1;
            strncpy( fname, vgfn, (size_t)len ); fname[len]='\0';
	}
	else  {
            strcpy( fname, "none" );
	}

        pptr = strrchr( fname, '.' );
        if ( pptr == (char *)NULL )  strcat( fname, suffix );
     }
    else  {
	strcpy ( fname, curname );
    }
}

/*=====================================================================*/

void pggfmt_setPDF ( int *iret )
/************************************************************************
 * pggfmt_setPDF							*
 *									*
 * Reads the GRPHGD PDF file and stores the parameter names.		*
 *									*
 * void pggfmt_setPDF ( iret )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *  *iret	int	Return code					*
 *			= -1 - error opening pdf file			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 * M. Li/SAIC		 4/05	Added SKIP and WIND			*
 * m.gamazaychikov/SAIC 01/05	Added skipping of GGVGF parameter	*
 ***********************************************************************/
{
int	ii, npdf, bufsiz, len, ier;
char	buffer[80], parm[20];
FILE    *fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    fp = cfl_tbop ( GRPHGD_PDF, "", &ier );

    if ( ier == 0 )  {

	bufsiz = sizeof ( buffer );

	cfl_tbnr ( fp, &npdf, &ier );

/*
 *  Total number should be CYCLE and FCST_HR + the GRPHGD pdfs
 *  plus 5 contouring parameters.
 */
	_ggNparms = 0;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("CYCLE")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "CYCLE" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("FCST_HR")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "FCST_HR" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("PATH")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "PATH" );
	_ggNparms++;

	for ( ii = 0; ii < npdf; ii++ )  {

	    cfl_trln ( fp, bufsiz, buffer, &ier );
            strncpy ( parm, buffer, 8 );
	    parm[8] = CHNULL;
	    cst_lstr ( parm, &len, &ier );
	    parm[len] = CHNULL;

            if ( strcmp (parm, "GGVGF" ) != 0 ) {
	       _ggParms[_ggNparms] = 
	           (char *)malloc( (strlen(parm)+1) * sizeof(char) );
	       strcpy( _ggParms[_ggNparms], parm );
	       _ggNparms++;
            }
	}

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("CINT")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "CINT" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("LINE")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "LINE" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("FINT")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "FINT" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("FLINE")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "FLINE" );
	_ggNparms++;

	_ggParms[_ggNparms] = 
	    (char *)malloc( (strlen("CLRBAR")+1) * sizeof(char) );
	strcpy( _ggParms[_ggNparms], "CLRBAR" );
	_ggNparms++;

        _ggParms[_ggNparms] =
            (char *)malloc( (strlen("SKIP")+1) * sizeof(char) );
        strcpy( _ggParms[_ggNparms], "SKIP" );
        _ggNparms++;

        _ggParms[_ggNparms] =
            (char *)malloc( (strlen("WIND")+1) * sizeof(char) );
        strcpy( _ggParms[_ggNparms], "WIND" );
        _ggNparms++;

	cfl_clos ( fp, &ier );
    }
    else  {
	*iret = -1;
    }
}

/*=====================================================================*/

void pggfmt_rdMaster ( int *iret )
/************************************************************************
 * pggfmt_rdMaster							*
 *									*
 * Reads the master NMAP graph-to-grid table and stores info in local	*
 * global.								*
 *									*
 * void pggfmt_rdMaster ( iret )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *  *iret	int	Return code					*
 *			= -1 - error opening master table		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 ***********************************************************************/
{
int	ii, bufsiz, nlbl, ier;
char	buffer[80], lbl[20], tbl[128];
FILE    *fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    fp = cfl_tbop ( GRPHGD_TBL, "grphgd", &ier );

    if ( ier == 0 )  {

        bufsiz = sizeof ( buffer );

        cfl_tbnr ( fp, &nlbl, &ier );
	_nLbl = nlbl;

        if ( _nLbl >= MAXNOPT )  _nLbl = MAXNOPT - 1;

        for ( ii = 0; ii < _nLbl; ii++ )  {

            cfl_trln ( fp, bufsiz, buffer, &ier );
            sscanf ( buffer, "%s %s", lbl, tbl );

            _lbl[ii] = (char *)malloc( (strlen(lbl)+1) * sizeof(char) );
            _tbl[ii] = (char *)malloc( (strlen(tbl)+1) * sizeof(char) );

            strcpy ( _lbl[ii], lbl );
            strcpy ( _tbl[ii], tbl );

        }

        strcpy ( lbl, "-none-" );
        _lbl[_nLbl] = (char *)malloc( (strlen(lbl)+1) * sizeof(char) );
        strcpy ( _lbl[_nLbl], lbl );

        strcpy ( tbl, "" );
        _tbl[_nLbl] = (char *)malloc( (strlen(tbl)+1) * sizeof(char) );
        strcpy ( _tbl[_nLbl], tbl );

        _nLbl++;

        cfl_clos ( fp, &ier );

    }
    else  {
        *iret = -1;
    }
}

/*=====================================================================*/

void pggfmt_tblInit ( char *tbl, int *iret )
/************************************************************************
 * pggfmt_tblInit							*
 *									*
 * Reads in a graph-to-grid restore table and sets the GUI values.	*
 *									*
 * void pggfmt_tblInit ( tbl, iret )					*
 *									*
 * Input parameters:							*
 * *tbl	char	graph-to-grid restore table				*
 *									*
 * Output parameters:							*
 *  *iret	int	Return code					*
 *			= -1 - error opening table			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 * T. Piper/SAIC	12/01	Close file				*
 ***********************************************************************/
{
int	ii, bufsiz, len, ier_file, ier;
char	buffin[128], buffout[128], parm[20], value[128], *cptr;
FILE    *fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Initialize...
 *  Skip first two - do not clear out cycle or forecast hour.
 */
    for ( ii = 2; ii < _ggNparms; ii++ )  {
        pggfmt_SetTxtW( _ggParms[ii], "", &ier ); 
    }
    _ggTmplt[0] = '\0';
    strcpy ( _inGDOUTF, "" );
    strcpy ( _inCNTRFL, "" );

/*
 *  No file name implies clear out but don't load anything.
 */
    if ( strlen(tbl) == (size_t)0 )  {
	return;
    }

    fp = cfl_tbop ( tbl, "grphgd", &ier );

    if ( ier == 0 )  {

	bufsiz = sizeof ( buffin );

	cfl_trln ( fp, bufsiz, buffin, &ier_file );

	while ( ier_file == 0 )  {

	    cst_ldsp ( buffin, buffin, &len, &ier );
	    cst_rxbl ( buffin, buffout, &len, &ier );

	    if ( buffout[len] == '\n' )  buffout[len] = '\0';
	    cst_unpr ( buffout, buffout, &ier );

	    cptr = (char *) cst_split ( buffout, ' ', sizeof(parm), 
				        parm, &ier );
	    if ( cptr != (char *)NULL )  {
		strcpy ( value, cptr );
	    }
	    else  {
		value[0] = '\0';
	    }

	    cst_lcuc ( parm, parm, &ier );

	    if ( strlen( value ) != (size_t)0 )  {

	        if ( strcmp ( parm, "VGF_TEMPLATE" ) == 0 )  {
		    strcpy ( _ggTmplt, value );
	        }
	        else  {
                    pggfmt_SetTxtW( parm, value, &ier ); 
		}

	    }

	    cfl_trln ( fp, bufsiz, buffin, &ier_file );

	}  

/*
 *  Save off initial values of GDOUTF and CNTRFL.
 */
        pggfmt_GetTxtW( "GDOUTF", _inGDOUTF, &ier ); 
        pggfmt_GetTxtW( "CNTRFL", _inCNTRFL, &ier ); 

	pggfmt_setTime ();

	cfl_clos(fp, &ier);
    }
    else  {
	*iret = -1;
    }
}

/*=====================================================================*/

void pggfmt_SetTxtW ( char *parm, char *value, int *iret )
/************************************************************************
 * pggfmt_SetTxtW							*
 *									*
 * Sets GUI values.							*
 *									*
 * void pggfmt_SetTxtW ( parm, value, iret )				*
 *									*
 * Input parameters:							*
 *  *parm	char	Parameter name to set with value		*
 *  *value	char	Value						*
 *									*
 * Output parameters:							*
 *  *iret	int	Return code					*
 *			= -1 - invalid parm 				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 ***********************************************************************/
{
    int		ii;
    Boolean	found;
/*---------------------------------------------------------------------*/

    *iret = 0;

    ii = 0;
    found = FALSE;
    while ( ii < _ggNparms )  {
        if ( strcmp( parm, _ggParms[ii] ) == 0 )  {
	    found = TRUE;
	    break;
	}
	ii++;
    }

    if ( found )  {
	XtVaSetValues( _ggTxtW[ii], XmNvalue, value, NULL ); 
    }
    else  {
        *iret = -1;
    }
}

/*=====================================================================*/

void pggfmt_GetTxtW ( char *parm, char *value, int *iret )
/************************************************************************
 * pggfmt_GetTxtW							*
 *									*
 * Gets GUI values.							*
 *									*
 * void pggfmt_GetTxtW ( parm, value, iret )				*
 *									*
 * Input parameters:							*
 *  *parm	char	Parameter name to get value			*
 *									*
 * Output parameters:							*
 *  *value	char	Value						*
 *  *iret	int	Return code					*
 *			= -1 - invalid parm 				*
 *			= -2 - unable to get string			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 ***********************************************************************/
{
    int		ii;
    char	*cptr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    ii = 0;
    while ( strcmp( parm, _ggParms[ii]) != 0 && ii < _ggNparms )  ii++;

    if ( ii < _ggNparms )  {
	cptr = XmTextFieldGetString( _ggTxtW[ii] );
	if ( cptr != (char *)NULL )  {
	    strcpy ( value, cptr );
	}
	else  {
	    *iret = -2;
	}
	XtFree ( cptr );
    }
    else  {
	value[0] = '\0';
        *iret = -1;
    }
}

/*=====================================================================*/

void pggfmt_ipPutv ( int *iret )
/************************************************************************
 * pggfmt_ipPutv							*
 *									*
 * Puts all parameters into the GEMPAK IP common library.		*
 *									*
 * void pggfmt_ipPutv ( iret )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *  *iret	int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 * m.gamazaychikov/SAIC	01/06	Added ahndling of GGVGF parameter	*
 ***********************************************************************/
{
int	ii, ier;
char	value[256];
/*---------------------------------------------------------------------*/

    *iret = 0;

    for ( ii = 0; ii < _ggNparms; ii++ )  {

	pggfmt_GetTxtW ( _ggParms[ii], value, &ier );

	ip_putv ( _ggParms[ii], value, &ier, 
		  strlen(_ggParms[ii]), strlen(value) );

    }
 
    value[0]= '\0';
    ip_putv ( "GGVGF", value, &ier, strlen("GGVGF"), strlen(value) );

}

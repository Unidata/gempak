#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgcmn.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define MAX_CCFTYP	 3

#define ELTYP_NON	-1
#define ELTYP_LVL	 0
#define ELTYP_CCF	 2

static int	_currSubType	= SIGTYP_AREA;

static Boolean	_waitFlag	= FALSE;

static VG_DBStruct	_ccfElm;
static VG_DBStruct	_lvlElm;

static struct	optMenuStrc	_covrStrc;
static struct	optMenuStrc	_topsStrc;
static struct	optMenuStrc	_probStrc;
static struct	optMenuStrc	_growStrc;
static struct	optMenuStrc	_spdStrc;
static struct	optMenuStrc	_dirStrc;

static char	*_topsLabels[]	= {"400+", "350-390", "300-340", "250-290"}; 
static char	*_growLabels[]	= {"+", "NC", "-"}; 
static char	*_probLabels[]	= {"50-100%", "25-49%"}; 
static char	*_covrLabels[]	= {"75-100%", "40-74%", "25-39%"}; 
static char	*_spdLabels[]	= {"0", "5", "10", "15", "20", "25", "30", 
				   "35", "40", "45", "50", "55", "60"}; 
static char	*_dirLabels[]	= {"N", "NNE", "NE", "ENE", "E", "ESE", "SE", 
				   "SSE", "S", "SSW", "SW", "WSW", "W", "WNW",
				   "NW", "NNW"}; 

static Widget	_mainW;
static Widget	_typeForm;
static Widget	_typePbs[MAX_CCFTYP];
static Widget	_editForm;
static Widget	_cntlForm;

static	XtCallbackProc _editCbFunc;

static Boolean _autoPlacement  = False;              /* text box auto placement flag */	
static Boolean _txtActive = False;
static Boolean _textDrawn = True;


/*
 *	Private Callback functions
 */
void pgccfw_cntlCb	( Widget, XtPointer, XtPointer );
void pgccfw_txtDragEh	( Widget, XtPointer, XEvent*, Boolean* );
void pgccfw_txtPressEh	( Widget, XtPointer, XEvent*, Boolean* );
void pgccfw_typeCb	( Widget, long, XtPointer );

/*
 *	Private Functions
 */
void pgccfw_setText ( void );
void pgccfw_setupElm ( VG_DBStruct *el, int type, int grpnum,
				Boolean is_new );


/************************************************************************
 * pgccfw.c								*
 *									*
 * This file contains the subroutines necessary to build and operate	*
 * the Collaborative Convective Forecast (CCF) popup.			*
 *									*
 * CONTENTS:								*
 *	pgccfw_create		creates the CCF popup			*
 *	pgccfw_popup		manages the CCF popup			*
 *	pgccfw_popdown		unmanages the CCF popup			*
 *	pgccfw_saveNew		saves a new CCF	group of elements	*
 *									*
 *	pgccfw_getAttr		returns the current attribute values	*
 *	pgccfw_isUp		returns status of the window		*
 *									*
 *	pgccfw_cntlCb		callback for the control buttons	*
 *	pgccfw_txtPressEh	event handler for text press		*
 *	pgccfw_txtDragEh	event handler for text drag		*
 *									*
 *	pgccfw_setupElm		sets an elements information		*
 *	pgccfw_setText		sets up the text placement		*
 ***********************************************************************/

/*=====================================================================*/

void pgccfw_create ( Widget parent )
/************************************************************************
 * pgccfw_create							*
 *									*
 * This function creates the CCF popup.					*
 *									*
 * void pgccfw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	Parent widget				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 * S. Law/GSC		04/00	add NOGRPLINE ifdef stuff		*
 * S. Law/GSC		05/00	changed speed/direction to menus	*
 * D. Kidwell/NCEP	02/02	corrected direction typo SNS to WNW     *
 * M. Li/SAIC           12/02   Radio box -> Check Box                  * 
 * E. Safford/SAIC	12/03	Cleaned up widget alignment		*
 * H. Zeng/SAIC		11/04	renamed two labels			*
 * T. Piper/SAIC	10/05	declared ii long			*
 * L. Hinson/AWC        06/09   Enable Autoplacement.                   *
 *                              Add medium Line Coverage Support        *
 *                              Set default storm speed to 0; Prev 25.  * 
 * L. Hinson/AWC        01/10   Set default growth to "NC"; Prev "+".   *
 ***********************************************************************/
{
    int         ier;
    int		nn, hoff = 3;
    long	ii;
    char	*type_labels[] = {"Area", "Line", "Line(Med)"}; 
    char	*cntl_labels[] = {"Apply", "Cancel"}, autoPlaceStr[ 32 ]; 

    XmString	xmstr;
    Widget	pane, rc, button;
/*---------------------------------------------------------------------*/
    ctb_pfstr ( "ENABLE_AUTOPLACE", autoPlaceStr, &ier);
    
    if ( ier == 0 && strlen( autoPlaceStr ) > 0
         && strcasecmp( autoPlaceStr, "TRUE" ) == 0 ) {
                                                                                  
         _autoPlacement = True;
                                                                            
    }

    _mainW = XmCreateFormDialog (parent, "ccf_main", NULL, 0);

    xmstr = XmStringCreateLocalized ("Collaborative Convective Forecast");

    XtVaSetValues(_mainW, 
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	False, 	
		  XmNdialogTitle,	xmstr,
		  NULL);

    XmStringFree (xmstr);

    pane =  XtVaCreateWidget ("pane",
			      xmPanedWindowWidgetClass, _mainW,
			      XmNsashWidth,	1,
			      XmNsashHeight,	1,	
			      XmNtopAttachment,	XmATTACH_FORM,
			      NULL);

/*
 * type pane
 */
    _typeForm = XtVaCreateManagedWidget("ccf_edit_form",
					xmFormWidgetClass, pane,
					NULL);

    rc = XtVaCreateManagedWidget ("ccf_type_rowcol", 
				  xmRowColumnWidgetClass, _typeForm, 
				  XmNorientation,	XmHORIZONTAL,
				  XmNradioAlwaysOne,	TRUE,
				  XmNradioBehavior,	False,
				  XmNleftAttachment,	XmATTACH_WIDGET,
				  XmNrightAttachment,	XmATTACH_WIDGET,
				  NULL);

    nn = XtNumber (type_labels);
    if (nn > MAX_CCFTYP) nn = MAX_CCFTYP;
    for (ii = 0; ii < nn; ii++) {
	_typePbs[ii] = XtVaCreateManagedWidget (type_labels[ii], 
						xmToggleButtonWidgetClass, rc,
						XmNset,	(ii == _currSubType),
						XmNuserData,	&_currSubType,
						NULL);

	XtAddCallback(_typePbs[ii], XmNvalueChangedCallback,
		      (XtCallbackProc)pgccfw_typeCb, (XtPointer) ii);
    }

/* 
 * edit pane
 */
    _editForm = XtVaCreateManagedWidget("ccf_edit_form",
					 xmFormWidgetClass, pane,
					 XmNfractionBase, 100,
					 NULL);

    _covrStrc.current = CCFLVL_LOW;
    nn = XtNumber (_covrLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_covrStrc.current, 
			     "Coverage:", NULL, &_covrStrc.form, 
			     &_covrStrc.label, &_covrStrc.menu, 
			     _covrStrc.pb, _covrLabels);

    XtVaSetValues( _covrStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			NULL); 

    XtVaSetValues( _covrStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    			
    XtVaSetValues( _covrStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );
    			
  
    _topsStrc.current = CCFLVL_LOW+1;
    nn = XtNumber (_topsLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_topsStrc.current, 
			     "Echo Tops:", NULL, &_topsStrc.form, 
			     &_topsStrc.label, &_topsStrc.menu, 
			     _topsStrc.pb, _topsLabels);

    XtVaSetValues( _topsStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopAttachment, 	XmATTACH_WIDGET,
			XmNtopWidget,		_covrStrc.menu,
			XmNtopOffset,		5,
			NULL); 

    XtVaSetValues( _topsStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    		
    XtVaSetValues( _topsStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );
    
  
    _probStrc.current = CCFLVL_HIGH;
    nn = XtNumber (_probLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_probStrc.current, 
			     "Confidence:", NULL, &_probStrc.form, 
			     &_probStrc.label, &_probStrc.menu, 
			     _probStrc.pb, _probLabels);

    XtVaSetValues( _probStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopAttachment, 	XmATTACH_WIDGET,
			XmNtopWidget,		_topsStrc.menu,
			XmNtopOffset,		5,
			NULL); 

    XtVaSetValues( _probStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    			
    XtVaSetValues( _probStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );

    _growStrc.current = 1;
    nn = XtNumber (_growLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_growStrc.current, 
			     "Growth:", NULL, &_growStrc.form, 
			     &_growStrc.label, &_growStrc.menu, 
			     _growStrc.pb, _growLabels);

    XtVaSetValues( _growStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopAttachment, 	XmATTACH_WIDGET,
			XmNtopWidget,		_probStrc.menu,
			XmNtopOffset,		5,
			NULL); 

    XtVaSetValues( _growStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    			
    XtVaSetValues( _growStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );

    
    _spdStrc.current = 0;
    nn = XtNumber (_spdLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_spdStrc.current, 
			     "Speed (kts):", NULL, &_spdStrc.form, 
			     &_spdStrc.label, &_spdStrc.menu, 
			     _spdStrc.pb, _spdLabels);

    XtVaSetValues( _spdStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopAttachment, 	XmATTACH_WIDGET,
			XmNtopWidget,		_growStrc.menu,
			XmNtopOffset,		5,
			NULL); 

    XtVaSetValues( _spdStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    			
    XtVaSetValues( _spdStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );


    _dirStrc.current = 0;
    nn = XtNumber (_dirLabels);
    pgutls_createOptionMenu (_editForm, nn, (XtPointer)&_dirStrc.current, 
			     "Direction:", NULL, &_dirStrc.form, 
			     &_dirStrc.label, &_dirStrc.menu, 
			     _dirStrc.pb, _dirLabels);

    XtVaSetValues( _dirStrc.form,
			XmNfractionBase,	100,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopAttachment, 	XmATTACH_WIDGET,
			XmNtopWidget,		_spdStrc.menu,
			XmNtopOffset,		5,
			NULL); 

    XtVaSetValues( _dirStrc.label,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
		   	NULL );
    			
    XtVaSetValues( _dirStrc.menu,
    			XmNtopAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNleftPosition,	45,
		   	NULL );

/*
 * control pane
 */
    nn = XtNumber (cntl_labels) * 100;
    _cntlForm = XtVaCreateManagedWidget ("ccf_cntl_form",
					 xmFormWidgetClass, pane,
					 XmNfractionBase,	nn,
					 NULL);

    nn = XtNumber (cntl_labels);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget 
	    (cntl_labels[ii], 
	     xmPushButtonWidgetClass, _cntlForm, 
	     XmNleftAttachment,		XmATTACH_POSITION,
	     XmNleftPosition,		((ii * 100) + hoff),
	     XmNrightAttachment,	XmATTACH_POSITION,
	     XmNrightPosition,		(((ii + 1) * 100) - hoff),
	     NULL);

	XtAddCallback(button, XmNactivateCallback,
		      (XtCallbackProc)pgccfw_cntlCb, (XtPointer) ii);
    }

    XtManageChild (pane); 
    XtUnmanageChild (_cntlForm);
}

/*=====================================================================*/

void pgccfw_popup ( VG_DBStruct *el, XtCallbackProc callback )
/************************************************************************
 * pgccfw_popup								*
 *									*
 * This function manages the CCF popup.					*
 *									*
 * void pgccfw_popup (el, callback)					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 *	callback	XtCallbackProc	edit callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 * S. Law/GSC		04/00	added DIR_TOWARD check			*
 * S. Law/GSC		05/00	changed speed/direction to menus	*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * H. Zeng/SAIC		02/05	used 1 & 3 for hi & low prob.		*
 * L. Hinson/AWC        07/09   Revised _growStrc.current formula       *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    if (el == NULL) {

    }
    else {
	if (callback != NULL) _editCbFunc = callback;

	XtManageChild (_cntlForm);

	_currSubType = el->elem.ccf.info.subtype;
	for (ii = 0; ii < MAX_CCFTYP; ii++) {
	    if (ii == _currSubType)
		XmToggleButtonSetState (_typePbs[ii], TRUE, TRUE);
	    else
		XmToggleButtonSetState (_typePbs[ii], FALSE, FALSE);
	}
	    

	_covrStrc.current = el->elem.ccf.info.cover - 1;
	XtVaSetValues (_covrStrc.menu, 
		       XmNmenuHistory, _covrStrc.pb[_covrStrc.current], 
		       NULL);

	_topsStrc.current = el->elem.ccf.info.tops - 1;
	XtVaSetValues (_topsStrc.menu, 
		       XmNmenuHistory, _topsStrc.pb[_topsStrc.current], 
		       NULL);

        _probStrc.current = (el->elem.ccf.info.prob - 1) / 2;
	XtVaSetValues (_probStrc.menu, 
		       XmNmenuHistory, _probStrc.pb[_probStrc.current], 
		       NULL);

	_growStrc.current = el->elem.ccf.info.growth - 2;
	XtVaSetValues (_growStrc.menu, 
		       XmNmenuHistory, _growStrc.pb[_growStrc.current], 
		       NULL);

	_spdStrc.current = (int) ((el->elem.ccf.info.spd / 5.0F) + 0.5F);
	XtVaSetValues (_spdStrc.menu, 
		       XmNmenuHistory, _spdStrc.pb[_spdStrc.current], 
		       NULL);

	_dirStrc.current = (int) ((el->elem.ccf.info.dir / 22.5F) + 0.5F);
	XtVaSetValues (_dirStrc.menu, 
		       XmNmenuHistory, _dirStrc.pb[_dirStrc.current], 
		       NULL);
    }

    XtManageChild (_mainW);
}

/*=====================================================================*/

void pgccfw_popdown ( void )
/************************************************************************
 * pgccfw_popdown							*
 *									*
 * This function unmanages the CCF popup.				*
 *									*
 * void pgccfw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_mainW)) {
	XtUnmanageChild (_cntlForm);
	XtUnmanageChild (_mainW);
	mcanvw_disarmDynamic ();
    }
}

/*=====================================================================*/

void pgccfw_saveNew ( int np, float *lats, float *lons )
/************************************************************************
 * pgccfw_saveNew							*
 *									*
 * This function saves a new CCF group of elements.			*
 *									*
 * void pgccfw_saveNew (np, lats, lons)					*
 *									*
 * Input parameters:							*
 *	np	int		number of points			*
 *	*lats	float		latitudes				*
 *	*lons	float		longitudes				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	split out of pgccfw_save		*
 * S. Law/GSC		04/00	add NOGRPLINE ifdef stuff		*
 * S. Law/GSC		05/00	removed spd/dir group items		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * L. Hinson/AWC        07/09   Revised to support placement, setting   *
 *                              the default text box location to be the *
 *                              polygon's centroid in _ccfElm           *
 *                              Drop setup and writing of _lvlElm       *
 ***********************************************************************/
{
    int         loc, loc2, one_pt = 1;
    int		ier, ii, grpnum;
    float	ulat, rlon, llat, llon, clat, clon;
    float	lvl_lat, lvl_lon;
    float	ccf_lats[MAX_SIGMET], ccf_lons[MAX_SIGMET];   
    char        value[32];
/*---------------------------------------------------------------------*/

    crg_ggnxt (GRPTYP_CCF, &grpnum, &ier);

/*
 * set element information
 */
    pgccfw_setupElm (&_ccfElm, ELTYP_CCF, grpnum, TRUE);

/*
 * find group range and center
 */
    crg_gbnd (sys_M, sys_M, np, lats, lons, 
	      &llat, &llon, &ulat, &rlon, &clat, &clon);

/*
 * set locations
 */
    for (ii = 0; ii < np; ii++) {
	ccf_lats[ii] = lats[ii];
	ccf_lons[ii] = lons[ii];
    }
    
    /* Set the arrow endpoint */
    _ccfElm.elem.ccf.info.arrowlat = clat;
    _ccfElm.elem.ccf.info.arrowlon = clon;

    /* Set the text box location */    
    if (cvg_plenabled()) {
      _ccfElm.elem.ccf.info.textlat = clat;
      _ccfElm.elem.ccf.info.textlon = clon;
    } else {
      _ccfElm.elem.ccf.info.textlat = -999.0;
      _ccfElm.elem.ccf.info.textlon = -999.0;
    }

    pgvgf_saveNewElm( NULL, sys_M, &_ccfElm, np, ccf_lats,
                      ccf_lons, TRUE, &loc, &ier );
    pgvgf_dsp (loc, &ier );
    geplot (&ier);
    if (cvg_placed(loc, &_ccfElm) != TRUE) {
      /* We don't want a ghost box for Lines in placement or non-placement... */
      if (!(_ccfElm.elem.ccf.info.subtype == SIGTYP_LINE_HIGH 
          || _ccfElm.elem.ccf.info.subtype == SIGTYP_LINE_MED)) {
        pgactv_setActvElm( &_ccfElm, loc);
        pgccfw_setText ();    
      }
    } else {
      pgundo_newStep ();
      pgundo_storeThisLoc ( loc, UNDO_ADD, &ier );
      pgundo_endStep ();
    }
  
}

/*=====================================================================*/



/*=====================================================================*/

void pgccfw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pgccfw_getAttr							*
 *									*
 * This routine returns the current values of the CCF element.		*
 *									*
 * void pgccfw_getAttr ( el )						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	current element			*
 *									*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 * S. Law/GSC		05/00	changed current value system		*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/SAIC		11/04	removed CCFLVL_MEDIUM			*
 * H. Zeng/SAIC		02/05	added back CCFLVL_MEDIUM		*
 * L. Hinson/AWC        07/09   Add read to settings table to get/set   *
 *                              the appropriate fills for low/med/hi    *
 *                              coverage areas; get/set colors for      *
 *                              hi/low probability areas.               *
 *                              Revise formula for setting the growth   *
 ***********************************************************************/
{
    int ier;
    el->hdr.vg_class	= CLASS_SIGMETS;
    el->hdr.vg_type	= SIGCCF_ELM;
    el->hdr.smooth	= 0;
    
    /* NEW Read the Settings.tbl to get color info */
    ces_get (_currSubType, el, &ier);

    if (_currSubType == SIGTYP_AREA) {
	el->hdr.closed  = 1;

	if (_covrStrc.current == CCFLVL_LOW) {
            el->hdr.filled = el->elem.ccf.info.filllow;
	}
	else if (_covrStrc.current == CCFLVL_MEDIUM) {
            el->hdr.filled = el->elem.ccf.info.fillmed;
	}
	else {
            el->hdr.filled = el->elem.ccf.info.fillhi;
	}

	if (_probStrc.current == CCFLVL_HIGH) {
            el->hdr.min_col = el->hdr.maj_col;
	}
	else {

/*
 * Confidence level is low 
 * but _probStrc.current != CCFLVL_LOW(2)
 */
            el->hdr.maj_col = el->hdr.min_col;
	}

    }
    else {
 	el->hdr.closed  = 0;
	el->hdr.filled  = 0;	/* 0 = no fill */
             
    }
   
    el->elem.ccf.info.subtype	= _currSubType; 

    el->elem.ccf.info.cover	= _covrStrc.current + 1;
    el->elem.ccf.info.tops	= _topsStrc.current + 1;
    el->elem.ccf.info.prob	= _probStrc.current + 1;
    el->elem.ccf.info.prob      = _probStrc.current * 2 + 1;
    el->elem.ccf.info.growth	= _growStrc.current + 2;
    el->elem.ccf.info.spd	= ((float) _spdStrc.current) * 5.0F;
    el->elem.ccf.info.dir	= ((float) _dirStrc.current) * 22.5F;
}

/*=====================================================================*/

Boolean pgccfw_isUp ( void )
/************************************************************************
 * pgccfw_isUp   							*
 *									*
 * This routine returns the current status of the window.     		*
 *									*
 * Boolean pgccfw_isUp ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 *	Boolean		True if the window is presently managed.	*	
 **									*
 * Log:									*
 * E. Safford/SAIC	12/01	intial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged( _mainW ) );
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfw_cntlCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * pgccfw_cntlCb							*
 *									*
 * This is the callback function for the control buttons.		*
 *									*
 * void pgccfw_cntlCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		XtPointer	which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 ***********************************************************************/
{
    _editCbFunc (NULL, which, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfw_typeCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgccfw_typeCb							*
 *									*
 * This is the callback function for the control buttons.		*
 *									*
 * void pgccfw_typeCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which type			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/00	initial coding				*
 * M. Li/SAIC           12/02   Radio box -> Check Box                  *
 * L. Hinson/AWC        07/09   Add support for High/Med Coverage Lines *
 ***********************************************************************/
{
    _currSubType = (int)which;

    if (_currSubType == SIGTYP_AREA) {
	if (!XtIsManaged (_editForm)) {
	    XtManageChild (_editForm);
	}
	XmToggleButtonSetState (_typePbs[0], TRUE, TRUE);
	XmToggleButtonSetState (_typePbs[1], FALSE, FALSE);
        XmToggleButtonSetState (_typePbs[2], FALSE, FALSE);
    } else {
        if (_currSubType == SIGTYP_LINE_HIGH || _currSubType == SIGTYP_LINE_MED) {
	  if (XtIsManaged (_editForm)) {
	    XtUnmanageChild (_editForm);
	  }
          if (_currSubType == SIGTYP_LINE_HIGH) {
	    XmToggleButtonSetState (_typePbs[0], FALSE, FALSE);
	    XmToggleButtonSetState (_typePbs[1], TRUE, TRUE);
            XmToggleButtonSetState (_typePbs[2], FALSE, FALSE);
          } else {
            XmToggleButtonSetState (_typePbs[0], FALSE, FALSE);
	    XmToggleButtonSetState (_typePbs[1], FALSE, FALSE);
            XmToggleButtonSetState (_typePbs[2], TRUE, TRUE);
          }
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfw_txtPressEh ( Widget wid, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgccfw_txtPressEh							*
 *									*
 * Press event handler for placing the CCF level text.			*
 *									*
 * void pgccfw_txtPressEh (wid, clnt, event)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget				*
 *	clnt		XtPointer	not used			*
 *	*event		XEvent		event				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * L. Hinson/AWC        07/09   Revised to support placement            *
 ***********************************************************************/
{
    int         location;
    int         ntxt = 1, np, ii;
    float       xx, yy, lats[MAXPTS], lons[MAXPTS], newlat, newlon;
    float       llx, lly, urx, ury;
    
    int		one_pt = 1, ier, xoff, yoff;
    float	lvl_x, lvl_y, lvl_lat, lvl_lon;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (_waitFlag) return;

    pgnew_setArmDynamic();

    el.hdr.vg_class = 0;
    pggst_setText (&el);
    pggst_clearGhost (TRUE);
    
    location = pgactv_getElmLoc ();
    pgutls_prepNew (location, &_ccfElm, &llx, &lly, &urx, &ury, &ier );
    
    xgtoff ( &xoff, &yoff, &ier );
    xx = (float) ( event->xbutton.x + xoff );
    yy = (float) ( event->xbutton.y + yoff );

    gtrans ( sys_D, sys_M, &ntxt, &xx, &yy, &newlat, &newlon, 
	     &ier, strlen (sys_D), strlen (sys_M) );
    
    if ( event->xbutton.button == Button1 ||
      ( event->xbutton.button == Button2 && !_txtActive )) {
      _ccfElm.elem.ccf.info.textlat = newlat;
      _ccfElm.elem.ccf.info.textlon = newlon;
    }
    
    np = _ccfElm.elem.ccf.info.npts;
    for ( ii = 0; ii < np; ii++ ) {
      lats[ii] = _ccfElm.elem.ccf.latlon[ii];
      lons[ii] = _ccfElm.elem.ccf.latlon[ii+np];
    }
    if ( event->xbutton.button == Button1 && _txtActive ) {
	pgundo_newStep (); 
        if ( _textDrawn ) pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
        pgvgf_saveNewElm ( NULL, sys_M, &_ccfElm, np, lats, lons, TRUE, 
		&location, &ier );    
        pgundo_storeThisLoc ( location, UNDO_ADD, &ier );
        pgundo_endStep ();
    }
    else {
      pgundo_newStep ();
      if ( _txtActive ) pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
        
      pgvgf_saveNewElm ( NULL, sys_M, &_ccfElm, np, lats, lons, TRUE,
		&location, &ier );    
      pgundo_storeThisLoc ( location, UNDO_ADD, &ier );   
    }
    pgvgf_dsp ( location, &ier );
    pgutls_redraw ( location, &_ccfElm, &ier );
    _textDrawn = True;
}

/*=====================================================================*/
/* ARGSUSED */
void pgccfw_txtDragEh ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgccfw_txtDragEh							*
 *									*
 * Drag event handler for placing the CCF level text.			*
 *									*
 * void pgccfw_txtDragEh (wid, clnt, event)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget				*
 *	clnt		XtPointer	not used			*
 *	*event		XEvent		event				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	initial coding				*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{
    int		ier, xoff, yoff;
    float	xx[1], yy[1];
/*---------------------------------------------------------------------*/

    if (_waitFlag) return;

/*
 * Erase the ghost line
 */
    pggst_clearGhost (TRUE);

/*
 *  Submit new ghostpoint
 */ 
    xgtoff (&xoff, &yoff, &ier);
    xx[0] = (float) (event->xbutton.x + xoff);
    yy[0] = (float) (event->xbutton.y + yoff);
    pggst_addGhostPts (1, xx, yy, &ier);

/*
 * redraw the ghost line
 */
    pggst_drawGhost(GST_NORMAL);
}

/*=====================================================================*/

void pgccfw_setupElm ( VG_DBStruct *el, int type, int grpnum, 
							Boolean is_new )
/************************************************************************
 * pgccfw_setupElm							*
 *									*
 * This function sets all the information for an element		*
 *									*
 * void pgccfw_setupElm (el, type, grpnum, is_new)			*
 *									*
 * Input parameters:							*
 *	*el	VG_DBStruct	element					*
 *	type	int		type of element				*
 *	grpnum	int		group number				*
 *	is_new	Boolean		whether this is a new element		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	split out of pgccfw_save		*
 * S. Law/GSC		05/00	fixed lincol/filcol problem		*
 * S. Law/GSC		05/00	removed speed/direction elements	*
 * H. Zeng/SAIC		11/04	changed spetial text content		*
 * H. Zeng/SAIC		02/05	changed "Confi:" to "Conf:"		*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    switch (type) {
      case ELTYP_LVL:
	if (is_new) {
	    el->hdr.vg_class = CLASS_TEXT;
	    el->hdr.vg_type  = SPTX_ELM;

	    ces_get(4, el, &ier);

	    el->hdr.grptyp		= GRPTYP_CCF;
	    el->hdr.grpnum		= grpnum;
	    el->elem.spt.info.sptxtyp	= 4;
	    el->elem.spt.info.ialign	= -1;
	    el->elem.spt.info.txtcol	= el->hdr.maj_col;
	    el->elem.spt.info.lincol	= el->hdr.maj_col;
	    el->elem.spt.info.filcol	= el->hdr.maj_col;
	    el->elem.spt.info.offset_x	= 0;
	    el->elem.spt.info.offset_y	= 0;
	}

	sprintf (el->elem.spt.text, 
		 "Hght: %s\nGwth: %s\nConf: %s\nCvrg: %s",
		 _topsLabels[_topsStrc.current], 
		 _growLabels[_growStrc.current],
		 _probLabels[_probStrc.current], 
		 _covrLabels[_covrStrc.current]);

	pgtxt_setLabelValue (el->elem.spt.text);

	break;

      case ELTYP_CCF:
	if (is_new) {
	    el->hdr.vg_class		= CLASS_SIGMETS;
	    el->hdr.vg_type		= SIGCCF_ELM;
	    el->elem.ccf.info.subtype	= -99;

	    ces_get (-99, el, &ier);

	    el->hdr.grptyp	= GRPTYP_CCF;
	    el->hdr.grpnum	= grpnum;
	}

	pgccfw_getAttr (el);

	break;
    }
}

/*=====================================================================*/

/*=====================================================================*/

void pgccfw_setText ( void )
/************************************************************************
 * pgccfw_setText							*
 *									*
 * This function sets the ghosting and event handling for the level	*
 * text box placement.							*
 *									*
 * void pgccfw_setText ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/00	split out of pgccfw_save		*
 * S. Law/GSC		04/00	cleaned up ghosting			*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * L. Hinson/AWC        07/09   Revised to support placement.           *
 *                              Dropped reference to _lvlElm            *
 ***********************************************************************/
{
    int		one_pt = 1, ier;
    float	xx, yy;
    VG_DBStruct txt_el;
    int np;
    
/*---------------------------------------------------------------------*/
    _textDrawn = False;
    
    cds_ccftxt(&_ccfElm, &txt_el, &ier);
    np = _ccfElm.elem.ccf.info.npts;
    txt_el.elem.spt.info.lat = _ccfElm.elem.gfa.latlon[np-1];
    txt_el.elem.spt.info.lon = _ccfElm.elem.gfa.latlon[2*np-1];

    pgtxt_setLabelValue ( txt_el.elem.spt.text );

    txt_el.elem.spt.info.offset_x = 0;
    txt_el.elem.spt.info.offset_y = 0;
        
    _waitFlag = TRUE;

    mcanvw_setDynamicFunc ((XtEventHandler)&pgccfw_txtPressEh, 
	    		   (XtEventHandler)&pgccfw_txtDragEh, 
			   (XtEventHandler)NULL, CURS_DEFAULT);

    pggst_veilGhost (TRUE);
    
    pggst_setText ( &txt_el );

    gtrans (sys_M, sys_D, &one_pt, &(txt_el.elem.spt.info.lat), 
	    &(txt_el.elem.spt.info.lon), &xx, &yy, 
	    &ier, strlen (sys_M), strlen (sys_D)); 


    mcanvw_setDynActFlag (TRUE);
    
    mbotw_mouseSet (LMHINT_PUT, MMHINT_DONE);

    _waitFlag = FALSE;
}

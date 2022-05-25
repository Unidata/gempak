#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"

#define PREF_TBL "prefs.tbl"
#define VAA_TBL  "vaa.tbl"

static  struct  optMenuStrc     _typStrc;
static	struct	optMenuStrc	_fhrStrc;

static  char    **_typStr;
static  char	*_fhrStr[] = {"F00", "F06", "F12", "F18"};

static  int     _nFhr = 5;

static  Widget  _pgvacwWin, _attrForm,_ctlForm;
static  Widget  _dirTxtW, _flTxtW, _fl2TxtW, _spdTxtW;
static  Widget  _distForm, _distText, _colorPb;

static  WidgetList       _ctlBtnW, _typePb;

static	int	_vgType;
static	int	_subType;
static	int	_attrColor;
static	float	_currDist;

static	char	***_otherFcstInfo;
static	int	_otherFcstNum;

/*
 *  private functions -- callback
 */
void    pgvacw_typeCb       ( Widget, long, XtPointer );
void    pgvacw_distanceCb   ( Widget, XtPointer, XtPointer );
void    pgvacw_ctlBtnCb     ( Widget, long, XtPointer );
void    pgvacw_typPbCb      ( Widget, long, XtPointer );

/*
 * private functions -- action
 */
void    pgvacw_attrSave (        void );
void    pgvacw_updtType ( int subtype );

/************************************************************************
 * 
 * nmap_pgvacw.c							*
 *									*
 * This module defines everything for Ash Cloud's formatting.		*
 *									*
 * CONTENTS:								*
 *	pgvacw_create()		creates the popup window		*
 *	pgvacw_popup()		manages the popup window		*
 *	pgvacw_popdown()	unmanages the popup window		*
 *									*
 *	pgvacw_getAttr()	get the current attributes		*
 *	pgvacw_isUp()		query whether the window is up		*
 *	pgvacw_getSubType()	returns the subtype			*
 *	pgvacw_getGrptyp()	gets the group type info.		*
 *      pgvacw_attrSave()	saves the current attr. to the element	*
 *	pgvacw_updtType()	updates the sub type on GUI		*
 *									*
 *	pgvacw_typeCb()		callback for type toggle buttons	*
 *	pgvacw_typPbCb()	callback for type option menu		*
 *	pgvacw_distanceCb()	callback for distance text		*
 *	pgvacw_ctlBtnCb()	callback for control buttons		*
 *      pgvolw_rdWords()        reads VAA wording text                  *
 **									*
 * Log:									*
 * H.Zeng/XTRIA		10/03   initial coding				*
 * R. Tian/SAIC		 2/06	added pgvolw_rdWords			*
 ***********************************************************************/

/*=====================================================================*/

void pgvacw_create ( Widget parent )
/************************************************************************
 * pgvacw_create							*
 *									*
 * This function creates VAA Ash Cloud Create/Edit Window.		*
 *									*
 * void pgvacw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03   initial coding				*
 * H. Zeng/XTRIA	11/03   modified _spdTxtW attributes		*
 * H. Zeng/XTRIA	01/04   removed SOL menu			*
 * T. Piper/SAIC	02/04	removed unused variable _numType	*
 * H. Zeng/SAIC		03/05	added more types			*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * T. Piper/SAIC	12/07	Change dir to free form text widget	*
 * B. Hebbard/NCEP	03/20	NAWIPS-125 cond. disable LINE option	*
 ***********************************************************************/
{
    int		jj, mm;
    long	ii, nn;
    char	*btnstrs[] = {"Apply", "Cancel"};
    char	*typstr[] = {"Area", "Line", "NotSeen", "Others-FCST"};
    Widget	label, pane, type_form, form1;
    Widget	fl_rc, spd_rc, rc1;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    _pgvacwWin = XmCreateFormDialog (parent, "vacw_edit", NULL, 0);
    xmstr = XmStringCreateLocalized("VAA Ash Cloud Create/Edit");

    XtVaSetValues(_pgvacwWin,
		  XmNnoResize,			TRUE,
		  XmNdefaultPosition,           False,
		  XmNdialogTitle,		xmstr,
		  NULL);

    XmStringFree(xmstr);

    pane = (Widget) XtVaCreateManagedWidget ("vacw_pane",
			xmPanedWindowWidgetClass, 	_pgvacwWin,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
			NULL);

/*
 * VAA Ash Cloud type form
 */
    type_form = (Widget) XtVaCreateWidget ("vacw_typeform",
			              xmFormWidgetClass, pane, NULL);

    rc1  = XtVaCreateManagedWidget ("vacw_rowcol",
			      xmRowColumnWidgetClass,	type_form,
			      XmNradioBehavior,	        TRUE,
			      XmNorientation,	        XmHORIZONTAL,
			      XmNpacking,		XmPACK_TIGHT,
			      XmNtopAttachment,		XmATTACH_FORM,
			      XmNleftAttachment,        XmATTACH_FORM,
			      NULL                                     ); 

    nn = XtNumber (typstr);
    _typePb = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));

    for (ii = 0; ii < nn; ii++) {

#ifndef DISPLAY_LINE_OPTION
        if (ii == ASHCLD_LINE) continue;
#endif

	_typePb[ii] = XtVaCreateManagedWidget (typstr[ii],
				xmToggleButtonWidgetClass, rc1,
				XmNhighlightThickness,       0,
				NULL                            );

	XtAddCallback (_typePb[ii], XmNarmCallback, 
		       (XtCallbackProc)pgvacw_typeCb, (XtPointer) ii);

#ifdef  DISPLAY_LINE_OPTION
	if (ii == ASHCLD_LINE) {
/*
 * Distance text field.
 */
            _distForm = (Widget) XtVaCreateManagedWidget ("distnum_form",
				 xmFormWidgetClass,   rc1,
				 NULL);
 
            label  = XtVaCreateManagedWidget ("Width:",
				 xmLabelWidgetClass,	_distForm,
				 XmNtopAttachment,      XmATTACH_FORM,
				 XmNtopOffset,		9,
				 XmNleftAttachment,	XmATTACH_FORM,
				 XmNleftOffset,         4,
				 NULL); 

            _distText = (Widget) XtVaCreateManagedWidget ("distance_text",
				 xmTextWidgetClass,	_distForm,
				 XmNcolumns,		5,
				 XmNtopAttachment,      XmATTACH_FORM,
				 XmNleftAttachment,	XmATTACH_WIDGET,
				 XmNleftWidget,	        label,
				 NULL);

            XtSetSensitive (_distForm, FALSE);

            XtAddCallback (_distText, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgvacw_distanceCb, (XtPointer) NULL);
	}
#endif

	if (ii == ASHCLD_OTHERS) {

/*
 * Obtain total number and pointer to other fcst info
 */
	    _otherFcstNum  = pgvolw_getFcstNum();
	    _otherFcstInfo = pgvolw_getFcstInfo();

	    mm = _otherFcstNum;
            _typStr = (char**) malloc( sizeof(char *) * mm );

	    for ( jj = 0; jj < mm; jj++ ) {

              _typStr[jj] = (char*) malloc( sizeof(char) * 
				    (strlen(_otherFcstInfo[jj][0])+5) );
	      strcpy (_typStr[jj], _otherFcstInfo[jj][0]);
            }
	
            _typStrc.current = 0;
            pgutls_createOptionMenu (rc1, mm, (XtPointer)&_typStrc.current, 
			             NULL, (XtCallbackProc)pgvacw_typPbCb, &_typStrc.form, 
				     &_typStrc.label, &_typStrc.menu, 
				     _typStrc.pb, _typStr);

            XtSetSensitive (_typStrc.form, FALSE);

	}
    }

/*
 * create color widget
 */
    form1 = XtVaCreateManagedWidget("vacclrfrm",
		      xmFormWidgetClass,   type_form,
		      XmNtopAttachment,    XmATTACH_FORM,
		      XmNtopOffset,	   12,
		      XmNleftAttachment,   XmATTACH_WIDGET,
		      XmNleftWidget,	   rc1,
		      XmNleftOffset,	   5,
		      NULL);

    label = XtVaCreateManagedWidget("Color:",
		      xmLabelWidgetClass,  form1,
		      XmNtopAttachment,    XmATTACH_FORM,
		      XmNleftAttachment,   XmATTACH_FORM,
		      NULL);

    _colorPb = XtVaCreateManagedWidget(" ",
		      xmPushButtonWidgetClass,	form1,
		      XmNtopAttachment,    XmATTACH_FORM,
		      XmNleftAttachment,   XmATTACH_WIDGET,
		      XmNleftWidget,	   label,
		      XmNwidth,		   25,
		      XmNheight,	   20,
		      NULL);

    XtAddCallback(_colorPb, XmNactivateCallback, NxmClrW_popup, 
		  &_attrColor);

    XtManageChild( type_form );

/*
 * VAA Ash Cloud attribute form.
 */
    _attrForm = (Widget) XtVaCreateWidget ("vacw_attrform",
				      xmFormWidgetClass, pane, NULL);

    mm = XtNumber (_fhrStr);
    _fhrStrc.current = 0;
    pgutls_createOptionMenu (_attrForm, mm, (XtPointer)&_fhrStrc.current,
			     "FHR", NULL, &_fhrStrc.form, &_fhrStrc.label,
			     &_fhrStrc.menu, _fhrStrc.pb, _fhrStr);

    XtVaSetValues (_fhrStrc.label,
		   XmNleftAttachment,	XmATTACH_FORM,
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   NULL);

    XtVaSetValues (_fhrStrc.form,
		   XmNleftAttachment,	XmATTACH_FORM,
		   XmNtopAttachment,	XmATTACH_FORM,
		   NULL);

    fl_rc = (Widget)NxmTxtIn_create(_attrForm, "FL", 5, &_flTxtW);

    XtVaSetValues(_flTxtW, XmNmaxLength, 14, NULL);

    XtVaSetValues (fl_rc,
		   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftOffset,       10,
		   XmNleftWidget,       _fhrStrc.form,
		   XmNtopAttachment,    XmATTACH_FORM,
		   NULL);

    label  = XtVaCreateManagedWidget ("-",
		   xmLabelWidgetClass,	_attrForm,
		   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	fl_rc,
		   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopOffset,	10,
		   XmNtopWidget,	fl_rc,
		   NULL );

    _fl2TxtW = XtVaCreateManagedWidget ("fl_text",
		   xmTextFieldWidgetClass,  _attrForm,
		   XmNcolumns,		5,
		   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftOffset,	2,
		   XmNleftWidget,	label,
		   XmNmaxLength,	14,
		   XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
		   XmNtopOffset,	3,
		   XmNtopWidget,	fl_rc,
		   NULL );

/*
 * Direction text field.
 */
	label  = XtVaCreateManagedWidget ("DIR",
				xmLabelWidgetClass,	_attrForm,
				XmNleftAttachment,	XmATTACH_WIDGET,
				XmNleftOffset,		10,
				XmNleftWidget,		_fl2TxtW,
				XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
				XmNtopOffset,		10,
				XmNtopWidget,		fl_rc,
				NULL);

	_dirTxtW = (Widget) XtVaCreateManagedWidget ("distance_text",
				xmTextFieldWidgetClass,	_attrForm,
				XmNcolumns,		5,
				XmNleftAttachment,	XmATTACH_WIDGET,
				XmNleftOffset,		2,
				XmNleftWidget,		label,
				XmNmaxLength,		14,
				XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
				XmNtopOffset,		3,
				XmNtopWidget,		fl_rc,
				NULL);

    spd_rc = (Widget)NxmTxtIn_create(_attrForm, "SPD", 5, &_spdTxtW);

    XtVaSetValues (spd_rc,
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftOffset,	10,
		   XmNleftWidget,	_dirTxtW,
		   XmNtopAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_spdTxtW, XmNmaxLength, 14, NULL);

    label  = XtVaCreateManagedWidget ("kts",
		   xmLabelWidgetClass,	_attrForm,
		   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	spd_rc,
		   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopOffset,	10,
		   XmNtopWidget,	spd_rc,
		   NULL );

    XtManageChild( _attrForm );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    _ctlBtnW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    _ctlForm = (Widget) XtVaCreateManagedWidget ("vacw_ctlform",
				  xmFormWidgetClass, pane, NULL);
    NxmCtlBtn_create(_ctlForm, 0, "pgvacw_ctlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvacw_ctlBtnCb, _ctlBtnW);  

    XtManageChild(pane);
}

/*=====================================================================*/

void pgvacw_popup ( VG_DBStruct *el )
/************************************************************************
 * pgvacw_popup								*
 *									*
 * This function pops up the VAA Ash Cloud Create/Edit window.		*
 *									*
 * void pgvacw_popup ( el )						*
 *									*
 * Input  parameters:							*
 *	*el		VG_DBStruct	current element			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03	initial coding				*
 * H. Zeng/XTRIA	11/03   removed decimal pts for Distance on GUI	*
 * H. Zeng/XTRIA	01/04	added call to pgvacw_updtType()		*
 * H. Zeng/XTRIA	02/04   changed color per layer info		*
 * H. Zeng/SAIC		04/05	added more types			*
 ***********************************************************************/
{
    int			obj_type, tmp, dummy, ii, cur_layer, ier;
    float		zero = 0.0F;
    char		curr_dist[10], fhr_str[10], *nm_ptr=NULL;
    VG_DBStruct		newel;
/*---------------------------------------------------------------------*/

    if ( el == NULL ) {

	obj_type = pgpalw_getCurObjId();
	pgobj_getId (CLASS_SIGMETS, obj_type, &_vgType, &tmp, &dummy);
	newel.hdr.vg_class = CLASS_SIGMETS;
	newel.hdr.vg_type  = (char)_vgType;
	newel.elem.ash.info.subtype = _subType = ASHCLD_AREA;
	ces_get(-99, &newel, &ier);

/*
 * Indicate _subType on GUI accordingly.
 */
        pgvacw_updtType ( _subType );

	XtSetSensitive ( _typePb[ASHCLD_AREA],    TRUE  );
#ifdef  DISPLAY_LINE_OPTION
	XtSetSensitive ( _typePb[ASHCLD_LINE],    TRUE  );
#endif
	XtSetSensitive ( _typePb[ASHCLD_NOTSEEN], TRUE  );
	XtSetSensitive ( _typePb[ASHCLD_OTHERS],  TRUE  );

	for ( ii = 0; ii < _otherFcstNum; ii++ ) {
	    XtSetSensitive ( _typStrc.pb[ii], TRUE );
        }

	_attrColor = newel.hdr.maj_col;
	XtVaSetValues(_colorPb,
		      XmNbackground,		NxmColrP_getColorPixel (_attrColor),
		      XmNtopShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      XmNbottomShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      NULL);
 
	_currDist = 25.0F;
        if ( _subType == ASHCLD_LINE ) {
             sprintf (curr_dist, "%-6.0f", _currDist);
        }
        else {
	     sprintf (curr_dist, "%-6.0f", zero);
        }
#ifdef  DISPLAY_LINE_OPTION
        XmTextSetString (_distText,  curr_dist);
#endif

        _fhrStrc.current = 0;
        XtVaSetValues (_fhrStrc.menu, 
		       XmNmenuHistory, _fhrStrc.pb[_fhrStrc.current], 
		       NULL);
      
        XmTextSetString (_flTxtW,  "SFC");
        XmTextSetString (_fl2TxtW, "\0" );
        XmTextSetString (_spdTxtW, "0\0" );

/*
 * If layering is active, change Color value and FHR option
 * according to the info from current layer.
 */
        if ( pgpalw_isLayerActv() ) {

            cur_layer  = pglayer_getCurLayer();  
      
	    _attrColor = pglayer_getMonoClr ( cur_layer );
	    XtVaSetValues(_colorPb,
		      XmNbackground,		NxmColrP_getColorPixel (_attrColor),
		      XmNtopShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      XmNbottomShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      NULL);

	    nm_ptr = pglayer_getName ( cur_layer );

	    if ( strcasecmp( nm_ptr, "OBS") == 0 ) {

                 _fhrStrc.current = 0;
	    }
	    else if ( strcasecmp( nm_ptr, "F06") == 0 ) {

                 _fhrStrc.current = 1;
	    }
	    else if ( strcasecmp( nm_ptr, "F12") == 0 ) {

                 _fhrStrc.current = 2;
	    }
	    else if ( strcasecmp( nm_ptr, "F18") == 0 ) {

                 _fhrStrc.current = 3;
	    }

            XtVaSetValues (_fhrStrc.menu, 
		       XmNmenuHistory, _fhrStrc.pb[_fhrStrc.current], 
		       NULL);
        }       

	XtUnmanageChild (_ctlForm);

    }
    else {

	_subType = el->elem.ash.info.subtype;

/*
 * Indicate _subType on GUI accordingly.
 */
        pgvacw_updtType ( _subType );

/*
 * Don't allow changes between AREA-LINE and other types.
 */
	if ( _subType == ASHCLD_AREA || _subType == ASHCLD_LINE ) {

	     XtSetSensitive ( _typePb[ASHCLD_AREA],    TRUE  );
#ifdef  DISPLAY_LINE_OPTION
	     XtSetSensitive ( _typePb[ASHCLD_LINE],    TRUE  );
#endif
	     XtSetSensitive ( _typePb[ASHCLD_NOTSEEN], FALSE );
	     XtSetSensitive ( _typePb[ASHCLD_OTHERS],  FALSE );
	}
        else if ( _subType == ASHCLD_NOTSEEN ) {

	     XtSetSensitive ( _typePb[ASHCLD_AREA],    FALSE );
#ifdef  DISPLAY_LINE_OPTION
	     XtSetSensitive ( _typePb[ASHCLD_LINE],    FALSE );
#endif
	     XtSetSensitive ( _typePb[ASHCLD_NOTSEEN], TRUE  );
	     XtSetSensitive ( _typePb[ASHCLD_OTHERS],  FALSE );
	}
        else if ( _subType >= ASHCLD_OTHERS ) {

	     XtSetSensitive ( _typePb[ASHCLD_AREA],    FALSE );
#ifdef  DISPLAY_LINE_OPTION
	     XtSetSensitive ( _typePb[ASHCLD_LINE],    FALSE );
#endif
	     XtSetSensitive ( _typePb[ASHCLD_NOTSEEN], FALSE );
	     XtSetSensitive ( _typePb[ASHCLD_OTHERS],  TRUE  );

	     for ( ii = 0; ii < _otherFcstNum; ii++ ) {

	       XtSetSensitive ( _typStrc.pb[ii],  
				(ii == (_subType-ASHCLD_OTHERS)) );
             }
	}

	_attrColor = el->hdr.maj_col;
	XtVaSetValues(_colorPb,
		      XmNbackground,		NxmColrP_getColorPixel (_attrColor),
		      XmNtopShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      XmNbottomShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      NULL);
 
	_currDist = el->elem.ash.info.distance;
        sprintf (curr_dist, "%-6.0f", _currDist);
#ifdef  DISPLAY_LINE_OPTION
        XmTextSetString (_distText,  curr_dist);
#endif

        sprintf (fhr_str, "F%02d", el->elem.ash.info.fhr);
	_fhrStrc.current = 0;
        for ( ii = 0; ii < _nFhr; ii++ ) {

	  if ( strcmp (fhr_str, _fhrStr[ii]) == 0 ) {

	       _fhrStrc.current = ii;
	       break;
          }
        }
        XtVaSetValues (_fhrStrc.menu, 
		       XmNmenuHistory, _fhrStrc.pb[_fhrStrc.current], 
		       NULL);

        XmTextSetString (_flTxtW,  el->elem.ash.info.flvl1);
        XmTextSetString (_fl2TxtW, el->elem.ash.info.flvl2);
        XmTextSetString (_spdTxtW, el->elem.ash.info.spds );

	XtManageChild (_ctlForm);
    }

    XtManageChild ( _pgvacwWin );
}

/*=====================================================================*/

void pgvacw_popdown ( void )
/************************************************************************
 * pgvacw_popdown							*
 *									*
 * This function pops down VAA Ash Cloud Create/Edit Window.		*
 *									*
 * void pgvacw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03   initial coding				*
 * J. Wu/SAIC		12/03   pop down color pallette			*
 ***********************************************************************/
{
   if ( XtIsManaged ( _pgvacwWin ) ) {
        NxmClrW_popdown();    	
	XtUnmanageChild ( _pgvacwWin );
    }
}

/*=====================================================================*/

Boolean pgvacw_isUp ( void ) 
/************************************************************************
 * pgvacw_isUp								*
 *									*
 * This function queries whether the VAA Ash Cloud window is up.	*
 *									*
 * Boolean pgvacw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvacw_isUp		Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvacwWin));
}

/*=====================================================================*/
/* ARGSUSED */
void pgvacw_typeCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgvacw_typeCb							*
 *									*
 * Callback function for the ASH CLOUD type (not seen, line, or area).	*
 *									*
 * void pgvacw_typeCb (wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		ASH CLOUD type			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	10/03   initial coding				*
 * H. Zeng/XTRIA	11/03   removed decimal pts for Distance on GUI	*
 * H. Zeng/XTRIA	01/04   added more Ash Cloud types		*
 * H. Zeng/SAIC		07/04	removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		03/05	added more types			*
 ***********************************************************************/
{
    float		zero = 0.0F;
    char		curr_dist[10];
    int			menu_idx;
/*---------------------------------------------------------------------*/

    _subType = (clnt < 3) ? clnt : (3 + _typStrc.current);

/*
 * Force Button "client" to be selected on GUI to fix a certain
 * problem of radio button group on linux 2.4
 */
    XmToggleButtonSetState ( _typePb[clnt], TRUE, TRUE );      

/*
 * For "Others-FCST" choices, call pgvacw_typPbCb() to set up option
 * menu items.
 */
    if ( _subType != ASHCLD_AREA && _subType != ASHCLD_LINE &&
	 _subType != ASHCLD_NOTSEEN ) {

         menu_idx = _typStrc.current;
         pgvacw_typPbCb ( NULL, menu_idx, NULL );
    }

/*
 * Set distance text widget according to the sub type.
 */
    if ( _subType == ASHCLD_LINE ) {
         sprintf (curr_dist, "%-6.0f", _currDist);
    }
    else {
	 sprintf (curr_dist, "%-6.0f", zero);
    }

#ifdef  DISPLAY_LINE_OPTION
    XmTextSetString (_distText,  curr_dist);  
    XtSetSensitive ( _distForm, (_subType == ASHCLD_LINE));
#endif

    XtSetSensitive ( _typStrc.form, 
		     (_subType != ASHCLD_AREA && 
		      _subType != ASHCLD_LINE &&
		      _subType != ASHCLD_NOTSEEN)        );

    pgnew_setMultiPt ( (char)
		       (_subType == ASHCLD_AREA || 
		        _subType == ASHCLD_LINE)         );
}

/*=====================================================================*/
/* ARGSUSED */
void pgvacw_typPbCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvacw_typPbCb							*
 *									*
 * Callback function for type option menu push buttons.			*
 *									*
 * void pgvacw_typPbCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	01/04	initial coding				*
 * H. Zeng/SAIC		07/04   removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		03/05	added more types			*
 ***********************************************************************/
{
    _typStrc.current = (int)which;
    _subType = 3 + _typStrc.current;

    XtVaSetValues (_typStrc.menu, 
		   XmNmenuHistory, _typStrc.pb[_typStrc.current], 
		   NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvacw_distanceCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgvacw_distanceCb							*
 *									*
 * Callback function for the distance text box.				*
 *									*
 * void pgvacw_distanceCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	10/03   initial coding				*
 * H. Zeng/XTRIA	11/03   removed decimal pts for Distance on GUI	*
 ***********************************************************************/
{
    float	dist;
    int		lenstr, ier;
    char	*dstr, newstr[8];
/*---------------------------------------------------------------------*/

#ifdef  DISPLAY_LINE_OPTION
    dstr = XmTextGetString (_distText);
#endif

    if ( sscanf (dstr, "%f", &dist) == 1 ) {

         if (0.0F <= dist && dist <= 1000.0F) {
	     sprintf (newstr, "%-6.0f", dist);
	     _currDist = dist;
         }
         else {
	     sprintf (newstr, "%-6.0f", _currDist);
         }
    }
    else {

	 sprintf (newstr, "%-6.0f", _currDist);
    }

    cst_rmbl ( newstr, newstr, &lenstr, &ier );
#ifdef  DISPLAY_LINE_OPTION
    XmTextSetString (_distText, newstr);
#endif

    XtFree (dstr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvacw_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvacw_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of VAA Ash Cloud *
 * window.								*
 *									*
 * void pgvacw_ctlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03	initial coding				*
 ***********************************************************************/
{
    switch(which) {

      case 0:	/* Apply */
        mcanvw_setCursor(CURS_BUSY);
        pgvacw_attrSave (); 
        mcanvw_setCursor(CURS_DEFAULT);

	break;

      case 1:	/* Cancel */  
 
        pgvacw_popdown();    

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/

int pgvacw_getSubType ( void )
/************************************************************************
 * pgvacw_getSubType							*
 *									*
 * This function returns the Ash Cloud subtype.				*
 *									*
 * int pgvacw_getSubType ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	pgvacw_getSubType	int	Ash Cloud  subtype		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03   intial coding				*
 ***********************************************************************/
{
    return (_subType);
}

/*=====================================================================*/

char pgvacw_getGrptyp ( void ) 
/************************************************************************
 * pgvacw_getGrptyp                                                     *
 *                                                                      *
 * returns the current group type 					*
 *                                                                      *
 * char pgvacw_getGrptyp( )      					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 * Return:								*
 *	pgvacw_getGrptyp    char	current group type		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  H. Zeng/XTRIA	09/03	initial coding				*
 ***********************************************************************/
{
char	grptyp;
/*---------------------------------------------------------------------*/

    grptyp = (char)(0);
    return ( grptyp );

}

/*=====================================================================*/

void pgvacw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pgvacw_getAttr							*
 *									*
 * This function retrieves the current attributes.			*
 *									*
 * void pgvacw_getAttr (el)						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el	VG_DBStruct	element structure			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03   initial coding				*
 * H. Zeng/XTRIA	11/03   added "filled" info			*
 * H. Zeng/XTRIA	01/04	removed "FL" prefix			*
 * H. Zeng/XTRIA	03/04	restored whole-width distance		*
 * H. Zeng/SAIC		08/04	removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		04/05	added more types			*
 * H. Zeng/SAIC		05/05	misc changes				*
 * H. Zeng/SAIC		05/05	manipulated "FL" prefix			*
 * H. Zeng/SAIC		11/05	changed "MOVING TO" to "MOVING FROM"	*
 * R. Tian/SAIC		 2/06	add read VAA prefs table		*
 * H. Zeng/SAIC		09/07	modified text for NOTSEEN type		*
 * T. Piper/SAIC	12/07	Change dir to free-form text widget	*
 * D.W.Plummer/NCEP	01/08	Formatting changes per Annex 3		*
 ***********************************************************************/
{
    int         fhr_val, ier;
    char	fhr_str[10], fl_info[32], *ptext=NULL;
    char	slash[2], *wdstr, wdtxt[8];
/*---------------------------------------------------------------------*/
    el->hdr.maj_col = _attrColor;
    el->hdr.closed  = (char)((_subType == ASHCLD_AREA) ? 1 : 0);

    el->elem.ash.info.subtype   = _subType;
    el->elem.ash.info.distance  = (_subType == ASHCLD_LINE) ? 
				   _currDist : 0.0F; 

    strcpy (fhr_str, _fhrStr[_fhrStrc.current]);
    if ( sscanf (fhr_str+1, "%d", &fhr_val) == 1 ) {
         el->elem.ash.info.fhr = fhr_val; 
    }
    else {
         el->elem.ash.info.fhr = 0; 
    }

    el->elem.ash.info.lintyp    = 1; 
    el->elem.ash.info.linwid    = 2; 
    el->elem.ash.info.sol       = 0;

    XtVaGetValues (_flTxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {
       strcpy(el->elem.ash.info.flvl1, ptext);
    }
    else {
       strcpy(el->elem.ash.info.flvl1, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);

    XtVaGetValues (_fl2TxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {
       strcpy(el->elem.ash.info.flvl2, ptext);
    }
    else {
       strcpy(el->elem.ash.info.flvl2, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);

    XtVaGetValues (_dirTxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {
       strcpy(el->elem.ash.info.dir, ptext); 
    }
    else {  
       strcpy(el->elem.ash.info.dir, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);

    XtVaGetValues (_spdTxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {
       strcpy(el->elem.ash.info.spds, ptext);
    }
    else {
       strcpy(el->elem.ash.info.spds, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);

/*
 * Set "filled" info. there is no GUI representation for it currently.
 */
    el->hdr.filled = 5;

/*
 * Initialization for the special text part.
 */
    el->elem.ash.spt.info.rotn      = 0.0F;
    el->elem.ash.spt.info.sztext    = 1.1F;
    el->elem.ash.spt.info.sptxtyp   = 4;
    el->elem.ash.spt.info.turbsym   = 0;
    el->elem.ash.spt.info.itxfn     = 1;
    el->elem.ash.spt.info.ithw      = 2;
    el->elem.ash.spt.info.iwidth    = 1;
    el->elem.ash.spt.info.txtcol    = 5;
    el->elem.ash.spt.info.lincol    = 5;
    el->elem.ash.spt.info.filcol    = 32;
    el->elem.ash.spt.info.ialign    = 0;
    el->elem.ash.spt.info.lat       = 0.0F;
    el->elem.ash.spt.info.lon       = 0.0F;
    el->elem.ash.spt.info.offset_x  = 0;
    el->elem.ash.spt.info.offset_y  = 0;

    ptext = el->elem.ash.spt.text;

    switch ( el->elem.ash.info.subtype ) {

       case ASHCLD_AREA:
       case ASHCLD_LINE:
            strcpy (ptext, "\0");

            break;

       case ASHCLD_NOTSEEN:

/*
 *  For forecast hour F00, the text starts with "VA NOT IDENTIFIABLE
 *  FROM SATELLITE DATA".  For other forecast hour, this piece of text
 *  is skipped and the text starts with "WIND ".
 */
	    if (_fhrStrc.current == 0) {

              strcpy  ( ptext, "VA NOT IDENTIFIABLE FROM SATELLITE DATA\n");
              sprintf ( &ptext[strlen(ptext)], "WINDS ");
            }
	    else {

              strcpy  ( ptext, "WINDS ");
            }

	    if ( strcasecmp (el->elem.ash.info.flvl1, "SFC") == 0 ) {

              sprintf ( &ptext[strlen(ptext)], "%s/", el->elem.ash.info.flvl1 );
	    }
            else if ( (el->elem.ash.info.flvl1[0] == 'F' ||
		       el->elem.ash.info.flvl1[0] == 'f')   && 
                      (el->elem.ash.info.flvl1[1] == 'L' ||
		       el->elem.ash.info.flvl1[1] == 'l')      ) {

              sprintf ( &ptext[strlen(ptext)], "%s/", el->elem.ash.info.flvl1 );
	    }
	    else {

              sprintf ( &ptext[strlen(ptext)], "FL%s/", el->elem.ash.info.flvl1 );
            }

	    if ( (el->elem.ash.info.flvl2[0] == 'F' ||
		  el->elem.ash.info.flvl2[0] == 'f')   && 
                 (el->elem.ash.info.flvl2[1] == 'L' ||
		  el->elem.ash.info.flvl2[1] == 'l')      ) {

              sprintf ( &ptext[strlen(ptext)], "%s", el->elem.ash.info.flvl2 );
	    }
	    else {

              sprintf ( &ptext[strlen(ptext)], "FL%s", el->elem.ash.info.flvl2 );
            }

	    pgvolw_rdWords ( PREF_TBL, VAA_TBL, &wdstr, &ier );
	    if ( ier != 0 ) return;
	    cst_gtag ( "<TOF>", wdstr, " ", wdtxt, &ier );

	    if ( strncmp(wdtxt, "FROM", 4) == 0 ) {
		strcpy(slash, "/");
		strcpy ( wdtxt, "" );
	    }
	    else {
		strcpy(slash, " ");
	    }

            sprintf ( &ptext[strlen(ptext)], "%s %s%s%sKT",
	        wdtxt, el->elem.ash.info.dir, slash, el->elem.ash.info.spds );

	    G_FREE ( wdstr, char );
	    break;

       default:

/*
 *  Subtypes other than the above.
 */
            strcpy (ptext, 
		    _otherFcstInfo[el->elem.ash.info.subtype-ASHCLD_OTHERS][1]);

	    fl_info[0] = '\0';
	  
	    if ( strcasecmp (el->elem.ash.info.flvl1, "SFC") == 0 ) {
	      strcat (fl_info, el->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
	    }
            else if ( (el->elem.ash.info.flvl1[0] == 'F' ||
		       el->elem.ash.info.flvl1[0] == 'f')   && 
                      (el->elem.ash.info.flvl1[1] == 'L' ||
		       el->elem.ash.info.flvl1[1] == 'l')      ) {

	      strcat (fl_info, el->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
	    }
	    else {
	      strcat (fl_info, "FL");
	      strcat (fl_info, el->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
            }

	    if ( (el->elem.ash.info.flvl2[0] == 'F' ||
		  el->elem.ash.info.flvl2[0] == 'f')   && 
                 (el->elem.ash.info.flvl2[1] == 'L' ||
		  el->elem.ash.info.flvl2[1] == 'l')      ) {

	      strcat (fl_info, el->elem.ash.info.flvl2);
	    }
	    else {
	      strcat (fl_info, "FL");
	      strcat (fl_info, el->elem.ash.info.flvl2);
            }

	    while ( strstr(ptext, "{FL}") != NULL ) {
	      cst_rpst (ptext, "{FL}", fl_info, ptext, &ier);
            }

	    break;
    }
}

/*=====================================================================*/

void pgvacw_attrSave ( void )
/************************************************************************
 * pgvacw_attrSave							*
 *									*
 * This function saves the attributes setting on ash cloud edit GUI to  *
 * current ash cloud element.						*
 *									*
 * void pgvacw_attrSave( void )						*
 *									*
 * Input   parameters:							*
 * Output  parameters:							*
 * Return  parameters:							*
 *		   NONE				                        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	10/03   initial coding				*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		location, np, ier;
    float	llx, lly, urx, ury, *x_coords, *y_coords;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    location = pgactv_getElmLoc();

    pgactv_getDevPts (&np, &x_coords, &y_coords);
    pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);

    pgundo_newStep();
    pgundo_storeThisLoc(location, UNDO_DEL, &ier);

    pgvacw_getAttr ( &el );	
    pgvgf_saveNewElm (NULL, sys_D, &el, np, 
				  x_coords, y_coords, TRUE, &location, &ier); 

    pgundo_storeThisLoc(location, UNDO_ADD, &ier);
    pgundo_endStep();

    pgutls_redraw (location, &el, &ier);

}

/*=====================================================================*/

void pgvacw_updtType ( int subtype )
/************************************************************************
 * pgvacw_updtType							*
 *									*
 * This function updates the subtype choice on the GUI.			*
 *									*
 * void pgvacw_updtType ( )						*
 *									*
 * Input   parameters:							*
 *	subtype    int		passed in sub type choice		*
 * Output  parameters:							*
 * Return  parameters:							*
 *		   NONE				                        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		03/05	added more types			*
 ***********************************************************************/
{
    long		radio_idx;
/*---------------------------------------------------------------------*/

    radio_idx = (long)(( subtype < 3 ) ? subtype : 3);
    _typStrc.current  = ( subtype < 3 ) ? 
			_typStrc.current  : (subtype - 3);
    pgvacw_typeCb ( NULL, radio_idx, NULL );
}

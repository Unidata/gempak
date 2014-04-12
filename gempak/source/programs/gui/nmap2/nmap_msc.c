#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmapprm.h"
#include "nmap_data.h"  
#include "nmap_dttm.h"  
#include "nmsdef.h"


/*
 * Define parameters
 */
#define ICON_DIR	"$NAWIPS/icons/nmap"
#define NUM_MARK	5
#define FIRST_MRK	17	/* Valid markers 17-21 */

#define MIN_SIZE	 0.1F
#define MAX_SIZE	10.0F
#define FACT_SIZE	10.0F

#define MIN_WIDTH	 1
#define MAX_WIDTH	10

/*
 * Declare module global variables.
 */
char		_alias[81], _filnam[81];
int		_indx, _isbcat, _ntype, _nflag;
NMS_types	_types[25];
NMS_flags	_flags[35];

Widget		_mscEditW;
Widget		_mscLineW;
Widget		_mscSymbW;
Widget		_mscArrwW;

int		_lnIndex, _lnWidth;
float		_lnSize;
Widget		_lnwid_text;
Widget		_lnwid_scale;
Widget		_lnsiz_text;
Widget		_lnsiz_scale;

int		_syIndex, _syCount, _syWidth, _syCode;
float		_sySize;
Widget		_sycode_type;
Widget		_sywid_text;
Widget		_sywid_scale;
Widget		_sysiz_text;
Widget		_sysiz_scale;

int		_arIndex, _arWidth, _arItyp;
float		_arSize, _arHdsz;
Widget		_arwid_text;
Widget		_arwid_scale;
Widget		_arsiz_text;
Widget		_arsiz_scale;
Widget		_arhsz_text;
Widget		_arhsz_scale;

static WidgetList _mscTypeBtn;
static WidgetList _mscColrBtn;
static WidgetList _mscColrBtn2;
static WidgetList _mscTextBox;
static WidgetList _mscLineBtn;
static WidgetList _mscSym1Btn;
static WidgetList _mscSym2Btn;
static WidgetList _mscArrwBtn;

static WidgetList _mscFlagBtn;

static WidgetList _mrktypBtn;


/*
 *  Private functions
 */
Widget msc_create ( Widget   parent );
void msc_createLine ( void );
void msc_createSymb ( void );
void msc_createArrw ( void );

/*
 * Private Callback functions
 */
void msc_arhszTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_arhszSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_arrwCb ( Widget wid, long which, XtPointer cbs );
void msc_arrwCtlBtnCb ( Widget wid, long which, XtPointer cbs );
void msc_arsizTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_arsizSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_arwidTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_arwidSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_ctlBtnCb ( Widget wid, long which, XtPointer cbs );
void msc_colrCb ( Widget wid, long which, XtPointer cbs );
void msc_colrCb2 ( Widget wid, long which, XtPointer cbs );
void msc_lineCb ( Widget wid, long which, XtPointer cbs );
void msc_flagCb ( Widget wid, long which, XtPointer cbs );
void msc_lnwidTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_lnwidSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_lnsizTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_lnsizSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_lineCtlBtnCb ( Widget wid, long which, XtPointer cbs );
void msc_syCodeCb ( Widget  wid, long which, XtPointer cbs );
void msc_symbCb ( Widget wid, long which, XtPointer cbs );
void msc_sym2Cb ( Widget wid, long which, XtPointer cbs );
void msc_sywidTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_sywidSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_sysizTxtCb ( Widget wid, XtPointer clnt, XmTextVerifyCallbackStruct *cbs );
void msc_sysizSclCb ( Widget wid, XtPointer clnt, XmScaleCallbackStruct *cbs );
void msc_symbCtlBtnCb ( Widget wid, long which, XtPointer cbs );
void msc_textCb ( Widget wid, long which, XtPointer cbs );
void msc_typeCb ( Widget wid, long which, XtPointer cbs );
void msc_typewindCb ( Widget wid, long which, XtPointer cbs );

/************************************************************************
 * nmap_msc.c                                                           *
 *                                                                      *
 * This module contains functions related to displaying miscellaneous   *
 * (VGF, WTCH, WARN, etc.) data for NMAP.				*
 *                                                                      *
 * CONTENTS:                                                            *
 *	msc_setAttr()		Set the data driver attributes		*
 *	msc_popup()		Pop up the edit dialog box		*
 *	msc_popdown()		Pop down the edit dialog box		*
 *									*
 *	msc_create()		Create the edit dialog box		*
 *	msc_createLine()	Create the line edit dialog box		*
 *	msc_createSymb()	Create the symbol edit dialog box	*
 *	msc_createArrw()	Create the arrow edit dialog box	*
 *									*
 *	msc_ctlBtnCb()		Callback for the control buttons	*
 *	msc_typeCb()		Callback for the TYPE on/off toggle	*
 *	msc_colrCb()		Callback for setting the color		*
 *      msc_colrCb2()           Callback for setting the second color   *
 *	msc_textCb()		Callback for setting the value		*
 *	msc_lineCb()		Callback for setting line attributes	*
 *	msc_symbCb()		Callback for setting symbol attributes	*
 *	msc_sym2Cb()		Callback for setting 2nd symbol attr	*
 *	msc_arrwCb()		Callback for setting arrow attributes	*
 *	msc_flagCb()		Callback for the FLAG on/off toggle	*
 *	msc_lnwidTxtCb()	Callback for line width setting		*
 *	msc_lnwidSclCb()	Callback for line width setting		*
 *	msc_lnsizTxtCb()	Callback for line size setting		*
 *	msc_lnsizSclCb()	Callback for line size setting		*
 *	msc_lineCtlBtnCb()	Callback for line control buttons	*
 *	msc_syCodeCb()		Callback for symbol code setting	*
 *	msc_sywidTxtCb()	Callback for symbol width setting	*
 *	msc_sywidSclCb()	Callback for symbol width setting	*
 *	msc_sysizTxtCb()	Callback for symbol size setting	*
 *	msc_sysizSclCb()	Callback for symbol size setting	*
 *	msc_symbCtlBtnCb()	Callback for symbol control buttons	*
 *	msc_arwidTxtCb()	Callback for arrow width setting	*
 *	msc_arwidSclCb()	Callback for arrow width setting	*
 *	msc_arsizTxtCb()	Callback for arrow size setting		*
 *	msc_arsizSclCb()	Callback for arrow size setting		*
 *	msc_arhszTxtCb()	Callback for arrow head size setting	*
 *	msc_arhszSclCb()	Callback for arrow head size setting	*
 *	msc_arrwCtlBtnCb()	Callback for arrow control buttons	*
 *	msc_typewindCb()	Callback for type of wind vector	*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void msc_setAttr ( dsrc_t *dsrc )
/************************************************************************
 * msc_setAttr                                                          *
 *                                                                      *
 * This function sets the data driver attributes for the msc source.	*
 *                                                                      *
 * void msc_setAttr ( dsrc )						*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dsrc       dsrc_t   data source data structure                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       09/99   Initial coding                          *
 * S. Law/GSC		10/99	_WWN -> _MSC				*
 * S. Jacobs/NCEP	11/99	Added call to nms_rtbl to get attributes*
 * S. Law/GSC		11/99	Changed to use new defines		*
 * S. Jacobs/NCEP	 1/00	Fixed to send correct data to routines	*
 * S. Jacobs/NCEP	 3/00	Changed variable names to avoid conflict*
 * S. Gilbert/NCEP	 5/06	increased dimension of typ and flg      *
 * L. Hinson/AWC         5/12   Increased dimension of flg to 35        *
 * L. Hinson/AWC        10/12   Allow Symbol markers to plot with EDR   *
 ***********************************************************************/
{
    int		isub, idx, ier;

    char	fulnam[LLMXLN], basnam[LLMXLN], dirnam[LLMXLN];
    char	source[LLMXLN], alnam[LLMXLN], flnm[LLMXLN];
    char	fname[LLMXLN], path[LLMXLN];

    int		ntp, nfl;
    NMS_types	typ[25];
    NMS_flags	flg[35];
/*---------------------------------------------------------------------*/

    isub = SCAT_NIL;
    idx  = dsrc->attridx;

    strcpy (fulnam, dsrc->path);

    if  ( dsrc->catg == CAT_VGF )  {
	cfl_path ( fulnam, dirnam, basnam, &ier );
	strcpy ( fname, basnam );

	strcpy ( fulnam, dirnam );
	cfl_path ( fulnam, dirnam, basnam, &ier );

	strcpy ( source, basnam );
	strcpy ( alnam,  dirnam );

	/*
	 * Get directory path for data file from table info.
	 */
	vtbl_getPath ( source, path, &ier );
	sprintf ( flnm, "%s/%s", path, fname );
    }
    else {
	cfl_path ( fulnam, dirnam, basnam, &ier );
	if  ( strchr ( dirnam, '/' ) == NULL )  {
	    strcpy ( flnm, "NONE" );
	    strcpy ( alnam,  basnam );
	    strcpy ( source, dirnam );
	}
	else {
	    strcpy ( flnm, basnam );

	    strcpy ( fulnam, dirnam );
	    cfl_path ( fulnam, dirnam, basnam, &ier );

	    strcpy ( alnam,  basnam );
	    strcpy ( source, dirnam );
	}
    }

    nms_rtbl ( alnam, &ntp, typ, &nfl, flg, &ier );
    nms_satt ( idx, alnam, isub, flnm, ntp, typ, nfl, flg,
		&(dsrc->attridx), &ier );

}

/*=====================================================================*/

Widget msc_create ( Widget parent )
/************************************************************************
 * msc_create								*
 *									*
 * This function creates the MISC data types attribute edit window.	*
 *									*
 * Widget msc_create ( parent )						*
 *									*
 * Input parameters:							*
 *	parent		Widget	The parent widget for the window	*
 *									*
 * Output parameters:							*
 * msc_create		Widget	Widget ID				*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * S. Jacobs/NCEP	 7/01	Added check for displaying color button	*
 * S. Jacobs/NCEP	 7/01	Added check for button name		*
 * S. Jacobs/NCEP	10/01	Check alias before setting def sym type	*
 * M. Li/SAIC		04/03	Added the second color widget		*
 * M. Li/SAIC		08/03	Added check for color and flag buttons	*
 * A. Hardy/NCEP	 9/03   Added XtParent to 'create dialog box'	*
 * F. J. Yen/NCEP	 6/04	Added "Arrw/Barb" as label for bname.	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{

	int		ix, iy, ilen, maxlen;
	long		ii;
	char		tmpstr[LLMXLN], bname[10];
	Pixel		jcolr;
	Widget		pane, form_type, form_flag, rc_flag;

	char		*btnstr[] = { "OK", "Cancel" };

/*---------------------------------------------------------------------*/

/*
 *	Create the dialog box for the attribute editing.
 */
	_mscEditW = XmCreateFormDialog ( XtParent(parent), "msc_popup", 
	                                 NULL, 0 );

	XtVaSetValues ( _mscEditW,
			XmNnoResize,        True,
			XmNdefaultPosition, False,
			NULL );

/*
 *	Set the window title.
 */
	sprintf ( tmpstr, "%s Attributes", _alias );
	XtVaSetValues ( XtParent(_mscEditW),
			XmNtitle, tmpstr,
			NULL );

/*
 *	Create a parent window pane.
 */
	pane = XtVaCreateWidget ( "msc_pane",
			xmPanedWindowWidgetClass, _mscEditW,
			XmNsashWidth,             1,
			XmNsashHeight,            1,
			NULL );

/*
 *	Create a form container for the TYPE information.
 */
	if  ( _ntype > 0 )  {

/*
 *	    Find the longest name for spacing in the form.
 */
	    maxlen = 10;
	    for ( ii = 0; ii < _ntype; ii++ )  {
		ilen   = (int)strlen ( _types[ii].name );
		maxlen = G_MAX ( maxlen, ilen );
	    }

/*
 *	    Create the form.
 */
	    form_type = XtVaCreateWidget ( "form_type",
			    xmFormWidgetClass, pane,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNtopAttachment,  XmATTACH_FORM,
			    NULL );

/*
 *	    Allocate memory for the different kinds of widgets.
 */
	    _mscTypeBtn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscColrBtn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscColrBtn2 = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscTextBox = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscLineBtn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscSym1Btn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscSym2Btn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));
	    _mscArrwBtn = (WidgetList)XtMalloc((size_t)_ntype*sizeof(Widget));

/*
 *	    Loop over all the TYPE entries.
 */
	    for ( ii = 0; ii < _ntype; ii++ )  {

/*
 *		Start the offset positions for placing the
 *		widgets in the form.
 */
		ix = 2;
		iy = ii * 35;

/*
 *		Create the toggle buttons for turning on/off the
 *		selected display item. Then increment the offset
 *		for the next widget.
 */
		_mscTypeBtn[ii] = XtVaCreateManagedWidget (
			_types[ii].name,
			xmToggleButtonWidgetClass, form_type,
			XmNleftAttachment,         XmATTACH_FORM,
			XmNleftOffset,             ix,
			XmNtopAttachment,          XmATTACH_FORM,
			XmNtopOffset,              iy + 5,
			NULL );

		XtAddCallback ( _mscTypeBtn[ii], XmNvalueChangedCallback,
				(XtCallbackProc) msc_typeCb,
				(XtPointer) ii );

		if ( _types[ii].ionoff == 0 ) { 
		    XmToggleButtonSetState ( _mscTypeBtn[ii], FALSE, FALSE );
		}
		else {
		    XmToggleButtonSetState ( _mscTypeBtn[ii], TRUE, TRUE );
		}

		ix += ( maxlen * 10 );

/*
 *		If the color is not missing, create the color buttons.
 */
		if  ( ! ERMISS ( (float)_types[ii].icolr ) )  {
		    _mscColrBtn[ii] = XtVaCreateManagedWidget ( " ",
			xmPushButtonWidgetClass, form_type,
			XmNwidth,                25,
			XmNheight,               20,
			XmNleftAttachment,       XmATTACH_FORM,
			XmNleftOffset,           ix, 
			XmNtopAttachment,        XmATTACH_FORM,
			XmNtopOffset,            iy + 7,
			NULL );

		    XtAddCallback ( _mscColrBtn[ii], XmNactivateCallback,
				    (XtCallbackProc) msc_colrCb,
				    (XtPointer) ii );

		    jcolr = NxmColrP_getColorPixel ( _types[ii].icolr );

		    XtVaSetValues ( _mscColrBtn[ii],
				    XmNbackground,        jcolr,
				    XmNtopShadowColor,    jcolr,
				    XmNbottomShadowColor, jcolr,
				    NULL );
		}
/*
 *              Increment the offset for the next widget.
 */
                ix += 35;

/*
 *		Create the second color buttons.
 */
		if  ( ! ERMISS ( (float)_types[ii].icolr2 ) )  {
                    _mscColrBtn2[ii] = XtVaCreateManagedWidget ( " ",
                        xmPushButtonWidgetClass, form_type,
                        XmNwidth,                25,
                        XmNheight,               20,
                        XmNleftAttachment,       XmATTACH_FORM,
                        XmNleftOffset,           ix,
                        XmNtopAttachment,        XmATTACH_FORM,
                        XmNtopOffset,            iy + 7,
                        NULL );

                    XtAddCallback ( _mscColrBtn2[ii], XmNactivateCallback,
                                    (XtCallbackProc) msc_colrCb2,
                                    (XtPointer) ii );

                    jcolr = NxmColrP_getColorPixel ( _types[ii].icolr2 );

                    XtVaSetValues ( _mscColrBtn2[ii],
                                    XmNbackground,        jcolr,
                                    XmNtopShadowColor,    jcolr,
                                    XmNbottomShadowColor, jcolr,
                                    NULL );
                }
/*
 *		Increment the offset for the next widget.
 */
		ix += 35;

/*
 *		If the value is not missing, create a text input
 *		widget for editing the value. Then increment the 
 *		offset for the next widget.
 */
		if  ( ! ERMISS ( (float)_types[ii].value ) )  {
		    
		    sprintf ( tmpstr, "%.2f", _types[ii].value );
		    _mscTextBox[ii] = XtVaCreateManagedWidget ( " ",
			xmTextFieldWidgetClass,   form_type,
			XmNcolumns,               6,
			XmNvalue,                 tmpstr,
			XmNcursorPositionVisible, True,
			XmNleftAttachment,        XmATTACH_FORM,
			XmNleftOffset,            ix, 
			XmNtopAttachment,         XmATTACH_FORM,
			XmNtopOffset,             iy,
			NULL );
		
		    XtAddCallback ( _mscTextBox[ii],
				    XmNactivateCallback,
				    (XtCallbackProc) msc_textCb,
				    (XtPointer) ii );
		    XtAddCallback ( _mscTextBox[ii],
				    XmNlosingFocusCallback,
				    (XtCallbackProc) msc_textCb,
				    (XtPointer) ii );

		    ix += 75;

		}

/*
 *		If this TYPE entry has an associated line in the
 *		display, create a button to pop a dialog box for
 *		editing the line attributes. Then increment the 
 *		offset for the next widget.
 */
		if  ( _types[ii].line.iwid >= 0 )  {

		    _mscLineBtn[ii] = XtVaCreateManagedWidget ( "Line",
			    xmPushButtonWidgetClass, form_type,
			    XmNleftAttachment,       XmATTACH_FORM,
			    XmNleftOffset,           ix, 
			    XmNtopAttachment,        XmATTACH_FORM,
			    XmNtopOffset,            iy,
			    NULL );

		    XtAddCallback ( _mscLineBtn[ii],
				    XmNactivateCallback,
				    (XtCallbackProc) msc_lineCb,
				    (XtPointer) ii );

		    ix += 60;

		}

/*
 *		If this TYPE entry has an associated symbol in the
 *		display, create a button to pop a dialog box for
 *		editing the symbol attributes. Then increment the 
 *		offset for the next widget.
 */
		if  ( _types[ii].symb[0].iwid >= 0 )  {

		    _mscSym1Btn[ii] = XtVaCreateManagedWidget ( "Symb",
			    xmPushButtonWidgetClass, form_type,
			    XmNleftAttachment,       XmATTACH_FORM,
			    XmNleftOffset,           ix, 
			    XmNtopAttachment,        XmATTACH_FORM,
			    XmNtopOffset,            iy,
			    NULL );

		    XtAddCallback ( _mscSym1Btn[ii],
				    XmNactivateCallback,
				    (XtCallbackProc) msc_symbCb,
				    (XtPointer) ii );

		    ix += 60;
		}

/*
 *		If this TYPE entry has an associated 2nd symbol in the
 *		display, create a button to pop a dialog box for
 *		editing the 2nd symbol attributes. Then increment the 
 *		offset for the next widget.
 */
		if  ( _types[ii].symb[1].iwid >= 0 )  {

		    _mscSym2Btn[ii] = XtVaCreateManagedWidget ( "Sym2",
			    xmPushButtonWidgetClass, form_type,
			    XmNleftAttachment,       XmATTACH_FORM,
			    XmNleftOffset,           ix, 
			    XmNtopAttachment,        XmATTACH_FORM,
			    XmNtopOffset,            iy,
			    NULL );

		    XtAddCallback ( _mscSym2Btn[ii],
				    XmNactivateCallback,
				    (XtCallbackProc) msc_sym2Cb,
				    (XtPointer) ii );

		    ix += 60;
		}

/*
 *		If this TYPE entry has an associated arrow in the
 *		display, create a button to pop a dialog box for
 *		editing the arrow attributes. Then increment the 
 *		offset for the next widget.
 */
		if  ( _types[ii].arrw.iwid >= 0 ) {

		    if  ( _types[ii].arrw.ityp > 2 &&
				 _types[ii].arrw.ityp < 6 )  {
		    	strcpy ( bname, "Arrw/Barb" );
		    }
		    else if (  _types[ii].arrw.ityp == 2 ) {
			strcpy ( bname, "Barb" );
		    }
		    else if ( _types[ii].arrw.ityp == 1 ) {
		   	strcpy ( bname, "Arrw" );
		    }
		    else {
		    	strcpy ( bname, "Arrw/Barb" );
		    }
		    _mscArrwBtn[ii] = XtVaCreateManagedWidget ( bname,
			    xmPushButtonWidgetClass, form_type,
			    XmNleftAttachment,       XmATTACH_FORM,
			    XmNleftOffset,           ix, 
			    XmNtopAttachment,        XmATTACH_FORM,
			    XmNtopOffset,            iy,
			    NULL );

		    XtAddCallback ( _mscArrwBtn[ii],
				    XmNactivateCallback,
				    (XtCallbackProc) msc_arrwCb,
				    (XtPointer) ii );

		}

	    }

	    XtManageChild ( form_type );
	}

/*
 *	Create a form container for the FLAG information.
 */
	if  ( _nflag > 0 )  {

/*
 *	    Create the form and row-column container for the
 *	    toggle buttons.
 */
	    form_flag = XtVaCreateWidget ( "form_flag",
			    xmFormWidgetClass, pane,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNtopAttachment,  XmATTACH_FORM,
			    NULL );

	    rc_flag = XtVaCreateWidget ( "rc_flag",
                        xmRowColumnWidgetClass, form_flag,
                        XmNorientation,         XmVERTICAL,
                        XmNpacking,             XmPACK_COLUMN,
                        XmNnumColumns,          1,
                        XmNradioBehavior,       FALSE,
                        XmNtraversalOn,         FALSE,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,      XmATTACH_FORM,
                        NULL );

/*
 *	    Allocate memory for the toggle buttons.
 */
	    _mscFlagBtn = (WidgetList) XtMalloc((size_t)_nflag * sizeof(Widget));

	    for ( ii = 0; ii < _nflag; ii++ )  {

/*
 *		Create the toggle buttons for turning on/off the
 *		selected flag item.
 */
		_mscFlagBtn[ii] = XtVaCreateManagedWidget (
					_flags[ii].name,
					xmToggleButtonWidgetClass,
					rc_flag,
					NULL );

		XtAddCallback ( _mscFlagBtn[ii], XmNvalueChangedCallback,
				(XtCallbackProc) msc_flagCb,
				(XtPointer) ii );

		if ( _flags[ii].iflg == 0 ) { 
		    XmToggleButtonSetState ( _mscFlagBtn[ii], FALSE, FALSE );
		}
		else {
		    XmToggleButtonSetState ( _mscFlagBtn[ii], TRUE, TRUE );
		}

	    }

	    XtManageChild ( rc_flag );
	    XtManageChild ( form_flag );

	}

/*
 *	Create the OK and Cancel control buttons.
 */
	NxmCtlBtn_create ( pane, 1, "msc_ctlBtn", XtNumber(btnstr),
			   btnstr, (XtCallbackProc)msc_ctlBtnCb, NULL );

	XtManageChild ( pane );

	return ( _mscEditW );

}

/*=====================================================================*/

void msc_popup ( Widget parent, int index )
/************************************************************************
 * msc_popup								*
 *									*
 * This function pops up the MISC data types attribute edit window.	*
 *									*
 * void msc_popup ( parent, index )					*
 *									*
 * Input parameters:							*
 *	parent		Widget	The parent widget for the window	*
 *	index		int	Data type index number			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * A. Hardy/NCEP	 9/03	Added 'XtParent' to msc_create		*
 ***********************************************************************/
{

	int	ier;

/*---------------------------------------------------------------------*/

/*
 *	If the window is still open, close it.
 */
	msc_popdown ( );

/*
 *	Get the attributes for the current MISC data type
 *	represented by index.
 */
	nms_qatt ( index, _alias, &_isbcat, _filnam,
		   &_ntype, _types, &_nflag, _flags, &ier );

	_indx = index;

/*
 *	Create the edit popup.
 */
	msc_create ( XtParent(parent) );

	XtManageChild ( _mscEditW );

}

/*=====================================================================*/

void msc_popdown ( void )
/************************************************************************
 * msc_popdown								*
 *									*
 * This function pops down the MISC data types attribute edit window.	*
 *									*
 * void msc_popdown ( )							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * E. Safford/GSC	07/01	add popdown to color picker		*
 * M. Li/SAIC		04/03	Added the second color for QSCT		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	if  ( _mscEditW != NULL )  {

	    if  ( XtIsManaged(_mscEditW) )  {

		NxmClrW_popdown();

		XtUnmanageChild ( _mscEditW );

/*
 *		Free any memory that has been allocated before
 *		destroying the parent widget.
 */
		if  ( _ntype > 0 )  {
		    XtFree ( (XtPointer)_mscTypeBtn );
		    XtFree ( (XtPointer)_mscColrBtn );
		    XtFree ( (XtPointer)_mscColrBtn2 );
		    XtFree ( (XtPointer)_mscTextBox );
		    XtFree ( (XtPointer)_mscLineBtn );
		    XtFree ( (XtPointer)_mscSym1Btn );
		    XtFree ( (XtPointer)_mscSym2Btn );
		    XtFree ( (XtPointer)_mscArrwBtn );
		}

		if  ( _nflag > 0 )  {
		    XtFree ( (XtPointer)_mscFlagBtn );
		}

		XtDestroyWidget ( _mscEditW );
		_mscEditW = (Widget) NULL;

	    }

	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_typeCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_typeCb								*
 *									*
 * This callback function sets the on/off flag for the given TYPE entry.*
 *									*
 * void msc_typeCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

XmToggleButtonCallbackStruct *call = (XmToggleButtonCallbackStruct *)cbs;

/*---------------------------------------------------------------------*/

	_types[which].ionoff = call->set;

}

/*=====================================================================*/

void msc_colrCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_colrCb								*
 *									*
 * void msc_colrCb ( wid, which, cbs )					*
 *									*
 * This callback function sets the color for the given TYPE entry.	*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	NxmClrW_popup ( wid, (XtPointer) &_types[which].icolr, cbs );

}

/*=====================================================================*/

void msc_colrCb2 ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_colrCb2                                                          *
 *                                                                      *
 * void msc_colrCb2 ( wid, which, cbs )                                 *
 *                                                                      *
 * This callback function sets the second color for the given TYPE entry*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid             Widget          Widget ID                       *
 *      which           long            Which display item              *
 *      cbs             XtPointer       Call back structure             *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      NONE                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC          	4/03   Created                                  *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

        NxmClrW_popup ( wid, (XtPointer) &_types[which].icolr2, cbs );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_textCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_textCb								*
 *									*
 * This callback function sets the value for the given TYPE entry.	*
 *									*
 * void msc_textCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * M. Li/SAIC		12/03	Reset value for the Time Stamp Interval	*
 * H. Zeng/SAIC		06/07	Adjust value for Fcst Hour		*
 ***********************************************************************/
{

	char	*valstr, newval[LLMXLN];
/*---------------------------------------------------------------------*/
	valstr = XmTextGetString (wid);
	sscanf ( valstr, "%f", &_types[which].value );
	XtFree ( valstr );

	if ( strcmp("Time Stamp Interval", _types[which].name) == 0 &&
	     _types[which].value <= 0 ) _types[which].value = 30.0;

	if ( strcmp(_types[which].name, "Fcst Hour") == 0 ) {

	  if (_types[which].value > 120.0) {
              _types[which].value = 120.0;
          }
	  else if (_types[which].value < 0.0) {
              _types[which].value = 0.0;
          }
          else {
	    _types[which].value = 
	      ((int)_types[which].value) - ((int)_types[which].value) % 6;
          }

        }

	sprintf ( newval, "%.2f", _types[which].value );
	XmTextSetString ( wid, newval );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lineCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_lineCb								*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lineCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	_lnIndex = (int)which;
	msc_createLine ( );

	XtManageChild ( _mscLineW );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_symbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_symbCb								*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_symbCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	_syIndex = (int)which;
	_syCount = 0;
	msc_createSymb ( );

	XtManageChild ( _mscSymbW );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_sym2Cb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_sym2Cb								*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * 2nd symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_sym2Cb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	_syIndex = (int)which;
	_syCount = 1;
	msc_createSymb ( );

	XtManageChild ( _mscSymbW );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arrwCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_arrwCb								*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arrwCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	_arIndex = (int)which;
	msc_createArrw ( );

	XtManageChild ( _mscArrwW );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_flagCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_flagCb								*
 *									*
 * This callback function sets the flag for the given FLAG entry.	*
 *									*
 * void msc_flagCb ( wid, which, cbs )					*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

XmToggleButtonCallbackStruct *call = (XmToggleButtonCallbackStruct *)cbs;

/*---------------------------------------------------------------------*/

	_flags[which].iflg = call->set;

}

/*=====================================================================*/
/* ARGSUSED */
void msc_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_ctlBtnCb								*
 *									*
 * This callback function handles when the user chooses the OK		*
 * or CANCEL buttons.							*
 *									*
 * void msc_ctlBtnCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * E. Safford/GSC	05/00	added loop_setDataChngd call to OK	*
 ***********************************************************************/
{

	int	jindex, ier;

/*---------------------------------------------------------------------*/

	switch ( which ) {

/*
 *	    OK button: Set the attributes as selected by the user.
 */
	    case 0:
		    nms_satt ( _indx, _alias, _isbcat, _filnam,
			       _ntype, _types, _nflag, _flags,
			       &jindex, &ier );

		    loop_setDataChngd (loop_getCurLoop(), TRUE);

		    break;

/*
 *	    Cancel button: Do nothing.
 */
	    case 1:
		    break;

	}

/*
 *	Remove the window.
 */
	msc_popdown ( );

}

/*=====================================================================*/

void msc_createLine ( void )
/************************************************************************
 * msc_createLine							*
 *									*
 * This function creates the Line Attributes Edit window.		*
 *									*
 * void msc_createLine ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char		tmpstr[LLMXLN];

	Widget		pane_line, rc_line, form_width, form_size;

	XmString	xmstr;

	char		*btnstr[] = { "OK", "Cancel" };

/*---------------------------------------------------------------------*/

	if  ( _mscLineW != NULL )  {
	    if  ( XtIsManaged(_mscLineW) )  {
		XtUnmanageChild ( _mscLineW );
		XtDestroyWidget ( _mscLineW );
		_mscLineW = (Widget) NULL;
	    }
	}

	_lnWidth = _types[_lnIndex].line.iwid;
	_lnSize  = _types[_lnIndex].line.size;

/*
 *	Create the Line Attribute Edit dialog.
 */
	_mscLineW = XmCreateFormDialog ( _mscEditW, "msc_line",
					 NULL, 0 );

	XtVaSetValues ( _mscLineW,
			XmNnoResize,        True,
			XmNdefaultPosition, False,
			NULL );

	sprintf ( tmpstr, "%s Line Attributes", _alias );
	XtVaSetValues ( XtParent(_mscLineW),
			XmNtitle, tmpstr,
			NULL );

/*
 *	Create a parent window pane.
 */
	pane_line = XtVaCreateWidget ( "msc_paneline",
			    xmPanedWindowWidgetClass, _mscLineW,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL );

/*
 *	Add sliders for the width and size settings.
 */
	rc_line = XtVaCreateWidget ( " ",
			    xmRowColumnWidgetClass, pane_line,
			    XmNorientation,         XmVERTICAL,
			    XmNradioAlwaysOne,      FALSE,
			    NULL );

	form_width = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass, rc_line,
			    NULL );

	sprintf ( tmpstr, "%d", _lnWidth );
	_lnwid_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_width,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	XtAddCallback ( _lnwid_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_lnwidTxtCb,
			NULL );

	XmTextFieldSetString ( _lnwid_text, tmpstr );

	_lnwid_scale = (Widget)XmCreateScale ( form_width,
					       "width",
					       NULL, 0 );
	XtManageChild ( _lnwid_scale );

	xmstr = XmStringCreateLocalized ( "Width" );
	XtVaSetValues ( _lnwid_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             MIN_WIDTH,
		      XmNmaximum,             MAX_WIDTH,
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               _lnWidth,
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _lnwid_text,
		      NULL );
	XmStringFree ( xmstr );

	XtAddCallback ( _lnwid_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_lnwidSclCb,
			NULL );

	XtAddCallback ( _lnwid_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_lnwidSclCb,
			NULL );

	XmScaleSetValue ( _lnwid_scale, _lnWidth );

	XtManageChild ( form_width );

	if  ( _lnSize > 0.0F )  {
	    form_size = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass,  rc_line,
			    XmNtopAttachment,   XmATTACH_WIDGET,
			    XmNtopWidget,       form_width,
			    XmNleftAttachment,  XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    NULL );

	    sprintf ( tmpstr, "%.1f", _lnSize );
	    _lnsiz_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_size,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	    XtAddCallback ( _lnsiz_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_lnsizTxtCb,
			NULL );

	    XmTextFieldSetString ( _lnsiz_text, tmpstr );

	    _lnsiz_scale = (Widget)XmCreateScale ( form_size,
					      "size",
					      NULL, 0 );
	    XtManageChild ( _lnsiz_scale );

	    xmstr = XmStringCreateLocalized ( "Size" );
	    XtVaSetValues ( _lnsiz_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             (int)(MIN_SIZE*FACT_SIZE),
		      XmNmaximum,             (int)(MAX_SIZE*FACT_SIZE),
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               (int)(_lnSize*FACT_SIZE),
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _lnsiz_text,
		      NULL );
	    XmStringFree ( xmstr );

	    XtAddCallback ( _lnsiz_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_lnsizSclCb,
			NULL );

	    XtAddCallback ( _lnsiz_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_lnsizSclCb,
			NULL );

	    XmScaleSetValue ( _lnsiz_scale, (int)(_lnSize*FACT_SIZE) );

	    XtManageChild ( form_size );
	}

	XtManageChild ( rc_line );

/*
 *	Create the OK and Cancel control buttons.
 */
	NxmCtlBtn_create ( pane_line, 1, "msc_lineCtlBtn",
			   XtNumber(btnstr),
			   btnstr, (XtCallbackProc)msc_lineCtlBtnCb, NULL );

	XtManageChild ( pane_line );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lnwidTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_lnwidTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lnwidTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, iwdth;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _lnwid_scale, &ival );
	valstr = XmTextFieldGetString ( _lnwid_text );
	sscanf ( valstr, "%d", &iwdth );
	XtFree ( valstr );

	if  ( MIN_WIDTH <= iwdth && iwdth <= MAX_WIDTH )  {
	    if  ( iwdth != ival )  {
		XmScaleSetValue ( _lnwid_scale, iwdth );
		_lnWidth = iwdth;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lnwidSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_lnwidSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lnwidSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	int	iwdth;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	iwdth = cbs->value;
	XmScaleSetValue ( wid, iwdth );

	sprintf ( valstr, "%d", iwdth );
	XmTextFieldSetString ( _lnwid_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _lnWidth = iwdth;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lnsizTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_lnsizTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lnsizTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, isize;
	float	fval;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _lnsiz_scale, &ival );
	valstr = XmTextFieldGetString ( _lnsiz_text );
	sscanf ( valstr, "%f", &fval );
	XtFree ( valstr );
	isize = (int) (fval * FACT_SIZE);

	if  ( MIN_SIZE <= fval && fval <= MAX_SIZE )  {
	    if  ( isize != ival )  {
		XmScaleSetValue ( _lnsiz_scale, isize );
		_lnSize = fval;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lnsizSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_lnsizSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lnsizSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	float	fsize;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	fsize = (float)cbs->value / FACT_SIZE;

	sprintf ( valstr, "%.1f", fsize );
	XmTextFieldSetString ( _lnsiz_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _lnSize = fsize;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_lineCtlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_lineCtlBtnCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * line attributes, then sets the values in the global structure.	*
 *									*
 * void msc_lineCtlBtnCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	switch ( which )  {

/*
 *	    OK button: Set the global variables.
 */
	    case 0:
		    _types[_lnIndex].line.iwid = _lnWidth;
		    _types[_lnIndex].line.size = _lnSize;
		    break;

/*
 *	    Cancel button: Do nothing.
 */
	    case 1:
		    break;

	}

	XtUnmanageChild ( _mscLineW );
	XtDestroyWidget ( _mscLineW );
	_mscLineW = (Widget) NULL;

}

/*=====================================================================*/

void msc_createSymb ( void )
/************************************************************************
 * msc_createSymb							*
 *									*
 * This function creates the Symbol Attributes Edit window.		*
 *									*
 * void msc_createSymb ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * S. Jacobs/NCEP	 4/01	Added check for symbol code out of range*
 * M. Li/SAIC		12/01	Changed marker path			*
 * T. Piper/SAIC	 5/02	Freed _mrktypBtn			*
 * m.gamazaychikov/SAIC 11/02	Added WSTM type to include symbol marker*
 * T. Piper/SAIC	11/02	Fixed marker icon names			*
 * M. Li/SAIC		03/03	Added WOU & WCN to include symbol marker*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{

	int		iret;
	long		ii, ignore;
	char		tmpstr[LLMXLN],
			filnam[LLMXLN], typid[8];

	Widget		pane_symb, rc_symb,
			form_width, form_size, rc_code,
			pulld_code, optm_code;

	XmString	xmstr;
	Pixel		fg, bg;
	Pixmap		mrkpxm[NUM_MARK];
	Arg		args[5], args1[5];
	Cardinal	nn;
	char		*btnstr[] = { "OK", "Cancel" };

	char		*mrklst[] = { "marker17.xbm", "marker18.xbm",
				      "marker19.xbm", "marker20.xbm",
				      "marker21.xbm" };

/*---------------------------------------------------------------------*/

	if  ( _mscSymbW != NULL )  {
	    if  ( XtIsManaged(_mscSymbW) )  {
		XtUnmanageChild ( _mscSymbW );
		XtDestroyWidget ( _mscSymbW );
		_mscSymbW = (Widget) NULL;
	    }
	}

	_syWidth = _types[_syIndex].symb[_syCount].iwid;
	_sySize  = _types[_syIndex].symb[_syCount].size;
	_syCode  = (int) _types[_syIndex].symb[_syCount].code;

/*
 *	Check for symbol code out of range.
 */
	if  ( ( strcmp ( _alias, "WARN" ) == 0 ) ||
	      ( strcmp ( _alias, "SVRL" ) == 0 ) ||
	      ( strcmp ( _alias, "WSTM" ) == 0 ) ||
	      ( strcmp ( _alias, "WOU" ) == 0 )  ||
	      ( strcmp ( _alias, "WCN" ) == 0 )  ||
              ( strcmp ( _alias, "EDR" ) == 0 ))  {
	    if  ( ( _syCode < FIRST_MRK ) ||
	    	  ( _syCode >= (FIRST_MRK+NUM_MARK) ) )  {
		_syCode = FIRST_MRK;
		_types[_syIndex].symb[_syCount].code = (float) FIRST_MRK;
	    }
	}

/*
 *	Create the Symbol Attribute Edit dialog.
 */
	_mscSymbW = XmCreateFormDialog ( _mscEditW, "msc_symb",
					 NULL, 0 );

	XtVaSetValues ( _mscSymbW,
			XmNnoResize,        True,
			XmNdefaultPosition, False,
			NULL );

	sprintf ( tmpstr, "%s Symbol Attributes", _alias );
	XtVaSetValues ( XtParent(_mscSymbW),
			XmNtitle, tmpstr,
			NULL );

/*
 *	Create a parent window pane.
 */
	pane_symb = XtVaCreateWidget ( "msc_panesymb",
			    xmPanedWindowWidgetClass, _mscSymbW,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL );

/*
 *	For only the WARN data, add a pop-up menu of valid
 *	marker types.
 */
	if  ( ( strcmp ( _alias, "WARN" ) == 0 ) ||
	      ( strcmp ( _alias, "SVRL" ) == 0 ) ||
	      ( strcmp ( _alias, "WSTM" ) == 0 ) ||
              ( strcmp ( _alias, "WOU" ) == 0 )  ||
              ( strcmp ( _alias, "WCN" ) == 0 )  ||
              ( strcmp ( _alias, "EDR" ) == 0 ) ) {

	    rc_code = XtVaCreateWidget ( " ",
			    xmRowColumnWidgetClass, pane_symb,
			    XmNorientation,         XmHORIZONTAL,
			    XmNtopAttachment,       XmATTACH_FORM,
			    NULL );

	    xmstr = XmStringCreateLocalized ( "Marker Type" );
	    XtVaCreateManagedWidget ( " ",
			    xmLabelWidgetClass, rc_code,
			    XmNlabelString,     xmstr,
			    NULL );
	    XmStringFree ( xmstr );
	    
	    _mrktypBtn = (WidgetList)XtMalloc(NUM_MARK*sizeof(Widget));

	    nn = 0;
	    XtSetArg ( args[nn], XmNorientation, XmVERTICAL    ); nn++;
	    XtSetArg ( args[nn], XmNpacking,     XmPACK_COLUMN ); nn++;
	    XtSetArg ( args[nn], XmNnumColumns,  1             ); nn++;
	    pulld_code = XmCreatePulldownMenu ( rc_code,
						"optmenu",
						args, nn );

	    XtVaGetValues ( rc_code,
			    XmNforeground, &fg,
			    XmNbackground, &bg,
			    NULL );

	    for ( ii = 0; ii < NUM_MARK; ii++ )  {

		sprintf ( typid, "%ld", ii+FIRST_MRK );

		/*
		strcpy ( icondir, "$NAWIPS/icons/nmap" );
		cfl_inqr ( mrklst[ii], icondir, &ignore, filnam, &iret );
		*/

		cfl_inqr ( mrklst[ii], ICON_DIR, &ignore, filnam, &iret );

		mrkpxm[ii] = XmGetPixmap ( XtScreen(rc_code),
					  filnam, fg, bg );

		if  ( mrkpxm[ii] == (Pixmap)XmUNSPECIFIED_PIXMAP )  {
		    _mrktypBtn[ii] = XtVaCreateManagedWidget ( typid,
				    xmPushButtonWidgetClass, pulld_code,
				    NULL );
		}
		else {
		    _mrktypBtn[ii] = XtVaCreateManagedWidget ( typid,
				    xmPushButtonWidgetClass, pulld_code,
				    XmNlabelType,            XmPIXMAP,
				    XmNlabelPixmap,          mrkpxm[ii],
				    NULL );
		}
		XtAddCallback ( _mrktypBtn[ii], XmNactivateCallback,
				(XtCallbackProc) msc_syCodeCb,
				(XtPointer) ii );
	    }

	    xmstr = XmStringCreateLocalized ( "  " );
	    nn = 0;
	    XtSetArg ( args1[nn], XmNsubMenuId,   pulld_code ); nn++;
	    XtSetArg ( args1[nn], XmNlabelString, xmstr      ); nn++;
	    XtSetArg ( args1[nn], XmNmenuHistory,
			       _mrktypBtn[_syCode-FIRST_MRK] ); nn++;
	    optm_code = XmCreateOptionMenu ( rc_code, " ", args1, nn );
	    XmStringFree ( xmstr );

	    XtManageChild ( optm_code );
	    _sycode_type = XmOptionButtonGadget ( optm_code );

	    XtManageChild ( rc_code );
	    XtFree((XtPointer)_mrktypBtn);
	}

/*
 *	Add sliders for the width and size settings.
 */
	rc_symb = XtVaCreateWidget ( " ",
			    xmRowColumnWidgetClass, pane_symb,
			    XmNorientation,         XmVERTICAL,
			    XmNradioAlwaysOne,      FALSE,
			    NULL );

	form_width = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass, rc_symb,
			    NULL );

	sprintf ( tmpstr, "%d", _syWidth );
	_sywid_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_width,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	XtAddCallback ( _sywid_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_sywidTxtCb,
			NULL );

	_sywid_scale = (Widget)XmCreateScale ( form_width,
					       "width",
					       NULL, 0 );
	XtManageChild ( _sywid_scale );

	xmstr = XmStringCreateLocalized ( "Width" );
	XtVaSetValues ( _sywid_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             MIN_WIDTH,
		      XmNmaximum,             MAX_WIDTH,
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               _syWidth,
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _sywid_text,
		      NULL );
	XmStringFree ( xmstr );

	XtAddCallback ( _sywid_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_sywidSclCb,
			NULL );

	XtAddCallback ( _sywid_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_sywidSclCb,
			NULL );

	XtManageChild ( form_width );

	form_size = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass,  rc_symb,
			    XmNtopAttachment,   XmATTACH_WIDGET,
			    XmNtopWidget,       form_width,
			    XmNleftAttachment,  XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
     			    NULL );

	sprintf ( tmpstr, "%.1f", _sySize );
	_sysiz_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_size,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	XtAddCallback ( _sysiz_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_sysizTxtCb,
			NULL );

	_sysiz_scale = (Widget)XmCreateScale ( form_size,
					      "size",
					      NULL, 0 );
	XtManageChild ( _sysiz_scale );

	xmstr = XmStringCreateLocalized ( "Size" );
	XtVaSetValues ( _sysiz_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             (int)(MIN_SIZE*FACT_SIZE),
		      XmNmaximum,             (int)(MAX_SIZE*FACT_SIZE),
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               (int)(_sySize*FACT_SIZE),
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _sysiz_text,
		      NULL );
	XmStringFree ( xmstr );

	XtAddCallback ( _sysiz_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_sysizSclCb,
			NULL );

	XtAddCallback ( _sysiz_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_sysizSclCb,
			NULL );

	XtManageChild ( form_size );

	XtManageChild ( rc_symb );

/*
 *	Create the OK and Cancel control buttons.
 */
	NxmCtlBtn_create ( pane_symb, 1, "msc_symbCtlBtn",
			   XtNumber(btnstr),
			   btnstr, (XtCallbackProc)msc_symbCtlBtnCb, NULL );

	XtManageChild ( pane_symb );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_syCodeCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_syCodeCb								*
 *									*
 * This callback function sets the marker type selected by the user.	*
 *									*
 * void msc_syCodeCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	_syCode = (int)which + FIRST_MRK;

}

/*=====================================================================*/
/* ARGSUSED */
void msc_sywidTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_sywidTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_sywidTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, iwdth;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _sywid_scale, &ival );
	valstr = XmTextFieldGetString ( _sywid_text );
	sscanf ( valstr, "%d", &iwdth );
	XtFree ( valstr );

	if  ( MIN_WIDTH <= iwdth && iwdth <= MAX_WIDTH )  {
	    if  ( iwdth != ival )  {
		XmScaleSetValue ( _sywid_scale, iwdth );
		_syWidth = iwdth;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_sywidSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_sywidSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_sywidSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	int	iwdth;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	iwdth = cbs->value;
	XmScaleSetValue ( wid, iwdth );

	sprintf ( valstr, "%d", iwdth );
	XmTextFieldSetString ( _sywid_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _syWidth = iwdth;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_sysizTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_sysizTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_sysizTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, isize;
	float	fval;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _sysiz_scale, &ival );
	valstr = XmTextFieldGetString ( _sysiz_text );
	sscanf ( valstr, "%f", &fval );
	XtFree ( valstr );
	isize = (int) (fval * FACT_SIZE);

	if  ( MIN_SIZE <= fval && fval <= MAX_SIZE )  {
	    if  ( isize != ival )  {
		XmScaleSetValue ( _sysiz_scale, isize );
		_sySize = fval;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_sysizSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_sysizSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_sysizSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	float	fsize;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	fsize = (float)cbs->value / FACT_SIZE;

	sprintf ( valstr, "%.1f", fsize );
	XmTextFieldSetString ( _sysiz_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _sySize = fsize;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_symbCtlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_symbCtlBtnCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * symbol attributes, then sets the values in the global structure.	*
 *									*
 * void msc_symbCtlBtnCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	switch ( which )  {

/*
 *	    OK button: Set the global variables.
 */
	    case 0:
		_types[_syIndex].symb[_syCount].iwid = _syWidth;
		_types[_syIndex].symb[_syCount].size = _sySize;
		_types[_syIndex].symb[_syCount].code = (float) _syCode;
		break;

/*
 *	    Cancel button: Do nothing.
 */
	    case 1:
		break;

	}

	XtUnmanageChild ( _mscSymbW );
	XtDestroyWidget ( _mscSymbW );
	_mscSymbW = (Widget) NULL;

}

/*=====================================================================*/

void msc_createArrw ( void )
/************************************************************************
 * msc_createArrw							*
 *									*
 * This function creates the Arrow Attributes Edit window.		*
 *									*
 * void msc_createArrw ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * S. Jacobs/NCEP	 7/01	Added check for _arHdsz non negative	*
 * F. J. Yen/NCEP	 6/04	Added _arItyp.  Created radio box for	*
 *				selecting type of wind vector.		*
 ***********************************************************************/
{

	char		tmpstr[LLMXLN];

	Widget		pane_arrw, rc_arrw,
			form_width, form_size, form_hdsz, typ_windv;

	XmString	xmstr, darrw, rarrw, wbarb;

	char		*btnstr[] = { "OK", "Cancel" };

/*---------------------------------------------------------------------*/

	if  ( _mscArrwW != NULL )  {
	    if  ( XtIsManaged(_mscArrwW) )  {
		XtUnmanageChild ( _mscArrwW );
		XtDestroyWidget ( _mscArrwW );
		_mscArrwW = (Widget) NULL;
	    }
	}

	_arWidth = _types[_arIndex].arrw.iwid;
	_arSize  = _types[_arIndex].arrw.size;
	_arHdsz  = _types[_arIndex].arrw.hdsz;
	_arItyp  = _types[_arIndex].arrw.ityp;

/*
 *	Create the Arrow Attribute Edit dialog.
 */
	_mscArrwW = XmCreateFormDialog ( _mscEditW, "msc_arrw",
					 NULL, 0 );

	XtVaSetValues ( _mscArrwW,
			XmNnoResize,        True,
			XmNdefaultPosition, False,
			NULL );

	sprintf ( tmpstr, "%s Arrow Attributes", _alias );
	XtVaSetValues ( XtParent(_mscArrwW),
			XmNtitle, tmpstr,
			NULL );

/*
 *	Create a parent window pane.
 */
	pane_arrw = XtVaCreateWidget ( "msc_panearrw",
			    xmPanedWindowWidgetClass, _mscArrwW,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL );

/*
 *	Add sliders for the width and size settings.
 */
	rc_arrw = XtVaCreateWidget ( " ",
			    xmRowColumnWidgetClass, pane_arrw,
			    XmNorientation,         XmVERTICAL,
			    XmNradioAlwaysOne,      FALSE,
			    NULL );
	if ( _arItyp > 2 ) {
/*
 *	    Create the RadioBox for selecting type of wind vector
 */

	    darrw = XmStringCreateLtoR ( "Directional\n  arrow", XmFONTLIST_DEFAULT_TAG );
	    rarrw = XmStringCreateLtoR ( "Regular\narrow", XmFONTLIST_DEFAULT_TAG );
	    wbarb = XmStringCreateLtoR ( "Wind\nbarb", XmFONTLIST_DEFAULT_TAG );
	    typ_windv = XmVaCreateSimpleRadioBox(rc_arrw,
			"type_windv", _arItyp-3, (XtCallbackProc)msc_typewindCb,
			XmVaRADIOBUTTON, darrw, NULL, NULL, NULL,
			XmVaRADIOBUTTON, rarrw, NULL, NULL, NULL,
			XmVaRADIOBUTTON, wbarb, NULL, NULL, NULL,
			XmNorientation, XmHORIZONTAL,
			NULL);
	    XmStringFree(darrw);
	    XmStringFree(rarrw);
	    XmStringFree(wbarb);

	    XtManageChild(typ_windv);
	}

	form_width = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass, rc_arrw,
			    NULL );

	sprintf ( tmpstr, "%d", _arWidth );
	_arwid_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_width,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	XtAddCallback ( _arwid_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arwidTxtCb,
			NULL );

	_arwid_scale = (Widget)XmCreateScale ( form_width,
					       "width",
					       NULL, 0 );
	XtManageChild ( _arwid_scale );

	xmstr = XmStringCreateLocalized ( "Width" );
	XtVaSetValues ( _arwid_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             MIN_WIDTH,
		      XmNmaximum,             MAX_WIDTH,
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               _arWidth,
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _arwid_text,
		      NULL );
	XmStringFree ( xmstr );

	XtAddCallback ( _arwid_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arwidSclCb,
			NULL );

	XtAddCallback ( _arwid_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_arwidSclCb,
			NULL );

	XtManageChild ( form_width );

	form_size = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass,  rc_arrw,
			    XmNtopAttachment,   XmATTACH_WIDGET,
			    XmNtopWidget,       form_width,
			    XmNleftAttachment,  XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
     			    NULL );

	sprintf ( tmpstr, "%.1f", _arSize );
	_arsiz_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_size,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	XtAddCallback ( _arsiz_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arsizTxtCb,
			NULL );

	_arsiz_scale = (Widget)XmCreateScale ( form_size,
					      "size",
					      NULL, 0 );
	XtManageChild ( _arsiz_scale );

	xmstr = XmStringCreateLocalized ( "Size" );
	XtVaSetValues ( _arsiz_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             (int)(MIN_SIZE*FACT_SIZE),
		      XmNmaximum,             (int)(MAX_SIZE*FACT_SIZE),
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               (int)(_arSize*FACT_SIZE),
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _arsiz_text,
		      NULL );
	XmStringFree ( xmstr );

	XtAddCallback ( _arsiz_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arsizSclCb,
			NULL );

	XtAddCallback ( _arsiz_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_arsizSclCb,
			NULL );

	XtManageChild ( form_size );

	if  ( _arHdsz >= 0.0F && _arItyp != 2 )  {
	    form_hdsz = XtVaCreateManagedWidget ( " ",
			    xmFormWidgetClass,  rc_arrw,
			    XmNtopAttachment,   XmATTACH_WIDGET,
			    XmNtopWidget,       form_width,
			    XmNleftAttachment,  XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
     			    NULL );

	    sprintf ( tmpstr, "%.1f", _arHdsz );
	    _arhsz_text = XtVaCreateManagedWidget ( " ",
			    xmTextFieldWidgetClass,   form_hdsz,
			    XmNcolumns,               4,
			    XmNvalue,                 tmpstr,
			    XmNcursorPositionVisible, True,
			    XmNrightAttachment,       XmATTACH_FORM,
			    NULL );

	    XtAddCallback ( _arhsz_text,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arhszTxtCb,
			NULL );

	    _arhsz_scale = (Widget)XmCreateScale ( form_hdsz,
					      "size",
					      NULL, 0 );
	    XtManageChild ( _arhsz_scale );

	    xmstr = XmStringCreateLocalized ( "Head Size" );
	    XtVaSetValues ( _arhsz_scale,
		      XmNorientation,         XmHORIZONTAL,
		      XmNminimum,             (int)(MIN_SIZE*FACT_SIZE),
		      XmNmaximum,             (int)(MAX_SIZE*FACT_SIZE),
		      XmNprocessingDirection, XmMAX_ON_RIGHT,
		      XmNvalue,               (int)(_arHdsz*FACT_SIZE),
		      XmNscaleMultiple,       1,
		      XmNshowValue,           False,
		      XmNtitleString,         xmstr,
		      XmNtopAttachment,       XmATTACH_FORM,
		      XmNleftAttachment,      XmATTACH_FORM,
		      XmNrightAttachment,     XmATTACH_WIDGET,
		      XmNrightWidget,         _arhsz_text,
		      NULL );
	    XmStringFree ( xmstr );

	    XtAddCallback ( _arhsz_scale,
			XmNvalueChangedCallback,
			(XtCallbackProc) msc_arhszSclCb,
			NULL );

	    XtAddCallback ( _arhsz_scale,
			XmNdragCallback,
			(XtCallbackProc) msc_arhszSclCb,
		 	NULL );
	}

	if ( _arItyp > 2 ) {

	    if ( _arItyp == 5 ) {
	        XtSetSensitive ( _arhsz_text, FALSE );
	        XtSetSensitive ( _arhsz_scale, FALSE );
	    }
	    else {
	        XtSetSensitive ( _arhsz_text, TRUE );
	        XtSetSensitive ( _arhsz_scale, TRUE );
	    }
	}

	XtManageChild ( rc_arrw );

/*
 *	Create the OK and Cancel control buttons.
 */
	NxmCtlBtn_create ( pane_arrw, 1, "msc_arrwCtlBtn",
			   XtNumber(btnstr),
			   btnstr, (XtCallbackProc)msc_arrwCtlBtnCb, NULL );

	XtManageChild ( pane_arrw );

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arwidTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_arwidTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arwidTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, iwdth;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _arwid_scale, &ival );
	valstr = XmTextFieldGetString ( _arwid_text );
	sscanf ( valstr, "%d", &iwdth );
	XtFree ( valstr );

	if  ( MIN_WIDTH <= iwdth && iwdth <= MAX_WIDTH )  {
	    if  ( iwdth != ival )  {
		XmScaleSetValue ( _arwid_scale, iwdth );
		_arWidth = iwdth;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arwidSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_arwidSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arwidSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	int	iwdth;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	iwdth = cbs->value;
	XmScaleSetValue ( wid, iwdth );

	sprintf ( valstr, "%d", iwdth );
	XmTextFieldSetString ( _arwid_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _arWidth = iwdth;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arsizTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_arsizTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arsizTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, isize;
	float	fval;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _arsiz_scale, &ival );
	valstr = XmTextFieldGetString ( _arsiz_text );
	sscanf ( valstr, "%f", &fval );
	XtFree ( valstr );
	isize = (int) (fval * FACT_SIZE);

	if  ( MIN_SIZE <= fval && fval <= MAX_SIZE )  {
	    if  ( isize != ival )  {
		XmScaleSetValue ( _arsiz_scale, isize );
		_arSize = fval;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arsizSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_arsizSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arsizSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	float	fsize;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	fsize = (float)cbs->value / FACT_SIZE;

	sprintf ( valstr, "%.1f", fsize );
	XmTextFieldSetString ( _arsiz_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _arSize = fsize;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arhszTxtCb ( Widget wid, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * msc_arhszTxtCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arhszTxtCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget				Widget ID		*
 *	clnt	XtPointer			Not used		*
 *	*cbs	XmTextVerifyCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	char	*valstr;
	int	ival, isize;
	float	fval;

/*---------------------------------------------------------------------*/

/*
 *	Confirm there is an event.  If not, this text has already
 *	been set up.
 */
	if  ( !cbs->event )  return;

/*
 *	If the value on the corresponding slider is different, set the
 *	slider value accordingly.
 */
	XmScaleGetValue ( _arhsz_scale, &ival );
	valstr = XmTextFieldGetString ( _arhsz_text );
	sscanf ( valstr, "%f", &fval );
	XtFree ( valstr );
	isize = (int) (fval * FACT_SIZE);

	if  ( MIN_SIZE <= fval && fval <= MAX_SIZE )  {
	    if  ( isize != ival )  {
		XmScaleSetValue ( _arhsz_scale, isize );
		_arHdsz = fval;
	    }
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arhszSclCb ( Widget wid, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * msc_arhszSclCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arhszSclCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		Not used		*
 *	*cbs		XmScaleCallbackStruct	Call back structure	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 ***********************************************************************/
{

	float	fsize;
	char	valstr[5];

/*---------------------------------------------------------------------*/

	fsize = (float)cbs->value / FACT_SIZE;

	sprintf ( valstr, "%.1f", fsize );
	XmTextFieldSetString ( _arhsz_text, valstr );

	if  ( cbs->reason == XmCR_VALUE_CHANGED )  {
	    _arHdsz = fsize;
	}

}

/*=====================================================================*/
/* ARGSUSED */
void msc_arrwCtlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_arrwCtlBtnCb							*
 *									*
 * This callback function pops a dialog to allow the user to edit the	*
 * arrow attributes, then sets the values in the global structure.	*
 *									*
 * void msc_arrwCtlBtnCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget ID			*
 *	which		long		Which display item		*
 *	cbs		XtPointer	Call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/00	Created					*
 * F. J. Yen/NCEP	 6/04	Added arrw.ityp				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	switch ( which )  {

/*
 *	    OK button: Set the global variables.
 */
	    case 0:
		    _types[_arIndex].arrw.iwid = _arWidth;
		    _types[_arIndex].arrw.size = _arSize;
		    _types[_arIndex].arrw.hdsz = _arHdsz;
		    _types[_arIndex].arrw.ityp = _arItyp;
		    break;

/*
 *	    Cancel button: Do nothing.
 */
	    case 1:
		    if ( _types[_arIndex].arrw.ityp == 5 ) {
			XtSetSensitive ( _arhsz_text, FALSE );
	    		XtSetSensitive ( _arhsz_scale, FALSE );
		    }
		    else {
			if ( _types[_arIndex].arrw.ityp != 2 ) {
			    XtSetSensitive ( _arhsz_text, TRUE );
	    		    XtSetSensitive ( _arhsz_scale, TRUE );
			}
		    }
		    break;

	}
	XtUnmanageChild ( _mscArrwW );
	XtDestroyWidget ( _mscArrwW );
	_mscArrwW = (Widget) NULL;
}
/*=====================================================================*/
/* ARGSUSED */
void msc_typewindCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * msc_typewindCb							*
 *									*
 * This is the callback function for type of wind toggle buttons.	*
 *									*
 * void msc_typewindCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *	wid		Widget		calling widget ID		*
 *	which		long		which type of wind vector	*
 *	cbs		XtPointer	call back structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	 6/04	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

/*
 *	Set global type of wind vector.
 */
	_arItyp = (int)which + 3;
	if ( _arItyp == 5 ) {
	    XtSetSensitive ( _arhsz_text, FALSE );
	    XtSetSensitive ( _arhsz_scale, FALSE );
	}
	else {
	    XtSetSensitive ( _arhsz_text, TRUE );
	    XtSetSensitive ( _arhsz_scale, TRUE );
	}
}

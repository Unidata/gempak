#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "ctbcmn.h"

#define MAX_SIZE_SCALE	 10
#define MIN_SIZE_SCALE	 1 
#define MAX_WIDTH_SCALE  10
#define MIN_WIDTH_SCALE  1
#define IMP_CHOICE       -99
#define	VOLCANO	 	201


static Widget	_symb_dlgW;
static Widget	_symb_colrW;
static Widget	_symb_sizeScaleW;
static Widget	_symb_sizeTxtW;
static Widget	_symb_widthScaleW;
static Widget	_symb_widthTxtW;

static Widget	_symb_loLaBbW;
static Widget	_symb_loLaCtrBbW;
static Widget	_symb_longLabelW;
static Widget	_symb_longTxtW;
static Widget	_symb_latiLabelW;
static Widget	_symb_latiTxtW;
static Widget	_symb_loLaPlacW;
static Widget	_symb_unRedoW;


static Widget	  _symb_ctlForm;
static WidgetList _symb_ctlBtns;

static Widget   _labelReminder_formW;
static Widget   _labelReminder_textW;

static Widget	_label_formW;
static Widget	_label_menuW, _label_menuW2;
static Widget	_label_toggW, _label_toggW2;
static Widget	_label_submenuW;
static Widget	_label_optW;
static Widget	_label_pbW[GRP_MXELE];

static Widget	_group_typeW;
static WidgetList _group_buttonW;

Widget		_clearRcW;
WidgetList	_clearBtnW;

static Widget	_volc_list, _volcTxtW;

static char	_labelFlag, _labelColorFlag;
static char	_labelName[MAX_TEXT];

static float	_symbSize;
static int	_symbWidth;
static int 	_symbColr;

static int      _numGrp   = 0;
static int      _groupTyp = IMP_CHOICE;
static int      _curGrpIdx= IMP_CHOICE;

static int      _objId    = IMP_CHOICE; /* A local copy of cur. objId */

static float	_Longitude;
static float	_Latitude;

static char	_vgType;
static int	_subTyp;
static int	_clearType;   /* initial value by table setting */

static int	_volcIdx;

static Boolean  _symbolUnRedoFlag;
static char	_symbolAttrStr[GRP_MXINFO];

/*
 *  private callback functions
 */
void pgsymb_colorCb 	( Widget, XtPointer, XmPushButtonCallbackStruct *cbs );
void pgsymb_clearCb 	( Widget, XtPointer, XtPointer );
void pgsymb_sizeScaleCb ( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_sizeTxtCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_widthScaleCb( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_widthTxtCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_labelToggCb ( Widget, XtPointer, XmAnyCallbackStruct *cbs );
void pgsymb_labelPbCb 	( Widget, long, XtPointer ); 
void pgsymb_longTxtCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_latiTxtCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgsymb_latLonPlacCb( Widget, XtPointer, XtPointer ); 
void pgsymb_unRedoCb 	( Widget, XtPointer, XtPointer );
void pgsymb_grpTypCb    ( Widget, long, XtPointer );
void pgsymb_menuTextCb  ( Widget, long, XtPointer );

/*
 *  private functions
 */
void pgsymb_setLabValue ( int which );
void pgsymb_initType    ( VG_DBStruct *el );


/************************************************************************
 * nmap_pgsymb.c							*
 *									*
 * This module creates and displays the symbols attibute editing box.	*
 * It also contains the callbacks for the box.				*
 *									*
 * CONTENTS:								*
 * pgsymb_create()	create symbol attribute editing window          *
 * pgsymb_createVolLst() create the volcano list window			*
 * pgsymb_popup()       pop up symbol attribute editing window          *
 * pgsymb_popdown()     pop down symbol attribute editing window        *
 * pgsymb_saveAttr()	save the new symbol color, size, and width	*
 * pgsymb_setAttr()	sets the symbol color, size, and width		*
 * pgsymb_setLatLon()	sets the symbol long/latitude position		*
 * pgsymb_setLabFlag()	sets the label flag				*
 * pgsymb_setLabItems() set label items based on the group type         *
 * pgsymb_setUnRedo()	sets the undo/redo button on symbol editor	*
 * pgsymb_setLabValue()	sets the label value				*
 *									*
 * pgsymb_getColor()	gets the symbol color				*
 * pgsymb_getSize()	gets the symbol size				*
 * pgsymb_getWidth()	gets the symbol width				*
 * pgsymb_getLabValue   gets the label value for text editor		*
 * pgsymb_getLabFlag()  gets the label flag				*
 * pgsymb_getLabColorFlag()  gets the label color flag                  *
 * pgsymb_getXxYyCoord	gets the symbol coordinate position		*
 * pgsymb_getGrptyp()	returns the current group type			*
 * pgsymb_getObjId()	gets the symbol object Id			*
 * 									*
 * pgsymb_isUp()	checks if attribute edit window is active	*
 *									*
 * pgsymb_colorCb()	callback for color widget			*
 * pgsymb_clearCb()	callback for clear widgets			*
 * pgsymb_sizeScaleCb() callback for size scale 			*
 * pgsymb_sizeTxtCb()	callback for size text widget			*
 * pgsymb_widthScaleCb()callback for width scale 			*
 * pgsymb_widthTxtCb()	callback for width text widget			*
 * pgsymb_labelToggCb   callback for label toggle button		*
 * pgsymb_labelPbCb()	callback for label type 			*
 * pgsymb_longTxtCb()	callback for longitude text widget		*
 * pgsymb_latiTxtCb()	callback for latiitude text widget		*
 * pgsymb_latLonPlacCb	callback for place symbol button widget		*
 * pgsymb_unRedoCb()	callback for undo/redo button widget		*
 * pgsymb_grpTypCb()    callback for group type option menu             *
 * pgsymb_initType()    initialize the group type                       *
 * pgsymb_updtGrpMenu() update the group menu				*
 * pgsymb_menuTextCb()	callback for the volcano list menu		* 
 ***********************************************************************/

/*=====================================================================*/

void pgsymb_create ( Widget parent )
/************************************************************************
 * pgsymb_create							*
 *									*
 * This function creates the symbol editing window.			*
 *									*
 * void pgsymb_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		03/98						*
 * W. Li/EAI		03/98	added "Apply", "Cancel" buttons		*
 * W. Li/EAI		02/99	added label type selections		*
 * W. Li/EAI		03/99	cleaned up				*
 * W. Li/EAI		03/99	added clear types and longitude/latitude*
 * W. Li/EAI		03/99	added longitude/latitude editing	*
 * W. Li/EAI		03/99	removed scale from long/lat editing	*
 * W. Li/EAI		04/99	modif. MIN_SIZE_SCALE, MIN_WIDTH_SCALE  *
 *							for Mark	*
 * W. Li/EAI		05/99	added group type in symbol editor	*
 * H. Zeng/EAI  	02/00   did minor changes to the appearance     *
 * H. Zeng/EAI  	04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI  	06/00   added use SYMBOL color toggle           *
 * H. Zeng/EAI  	07/00   added label reminder text widget        *
 * H. Zeng/EAI  	12/00   added _symbolUnRedoFlag                 *
 * E. Safford/GSC	12/00	fix compile warnings & rename grp defs  *
 * E. Safford/GSC	01/01	use Nmap resource file for window loc   *
 * M. Li/SAIC		10/01	increase MIN_SIZE_SCALE, MIN_WIDTH_SCALE*
 * E. Safford/SAIC	11/01	add pgutls_checkNumDataCb to txt widgets*
 * E. Safford/SAIC	12/01	pgutls_checkNumDataCb -> _vrfyNumericCb *
 * R. Tian/SAIC         01/02   removed numeric check for lati/longTxtCb*
 * J. Wu/SAIC           05/02   replace vrfyNumericCb with vrfyPosFltCb	*
 * M. Li/SAIC		12/02	Radio box -> check box			*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
Widget		bb[4], clearlbl, pane, pulldown, form[6];
XmString	xmstr;
int		grptyp, iret;
long		ii;
char            *btnstr[] = {"Apply", "Cancel"};
char		*clea_type[] = {"On", "Off"}; 
char            *names = NULL;  
Widget		label_form, label_menubar;
Widget		type_menu, group_form, labelReminder_label;
Arg             args[10];
Cardinal	nn;
char	        cc[10], grpnam[64];
/*---------------------------------------------------------------------*/

	/* 
	 * create a Form Dialog -- "Symbol Edit" 
	 */

	_symb_dlgW = XmCreateFormDialog ( parent, "symbol_edit",
				        NULL, 0 );

	xmstr = XmStringCreateLocalized("Symbol Attributes");
	XtVaSetValues(_symb_dlgW,
		XmNdefaultPosition,		False,
		XmNnoResize,			True,
		XmNdialogTitle, 		xmstr,
        	XmNautoUnmanage,		FALSE,
        	NULL);
	XmStringFree(xmstr);


	pane  = XtVaCreateManagedWidget ("pane",
		xmPanedWindowWidgetClass,	_symb_dlgW,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     3,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL);

	for (ii=0; ii <=5; ii++) {
	    form[ii] = XtVaCreateWidget ("form", 
		    xmFormWidgetClass, 	   	pane, 
                    XmNmarginHeight,            0,
                    XmNmarginWidth,             0,
		    NULL);
	}

	/*
	 * create Bulletin board for symbol color, size, width inputs.
	 */   

	bb[0] = XtVaCreateWidget( "pg_symbs_bb0",
                xmBulletinBoardWidgetClass,	form[0],
		XmNheight,			50,
		XmNwidth,			250,
                NULL);

	_clearRcW  =  XtVaCreateWidget ("_clearRowColW",
		xmRowColumnWidgetClass,		bb[0], 
        	XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			2,
		XmNradioBehavior,       	False,
		XmNtraversalOn,         	False,
		XmNx,                      	70,
		XmNy,                      	10,
		NULL);

	bb[1] = XtVaCreateWidget( "pg_symbs_bb1",
                xmBulletinBoardWidgetClass,	form[1],
		XmNheight,			110,
		XmNwidth,			250,
                NULL);

	_symb_loLaBbW = form[4];

	bb[2] = XtVaCreateWidget( "pg_symbs_bb2",
        	xmBulletinBoardWidgetClass,	_symb_loLaBbW,
		XmNheight,			100,
		XmNwidth,			250,
        	NULL);

	 _symb_loLaCtrBbW = form[5];

	bb[3] = XtVaCreateWidget( "pg_symbs_bb3",
        	xmBulletinBoardWidgetClass,	_symb_loLaCtrBbW,
		XmNheight,			50,
		XmNwidth,			250,
        	NULL);
	/*
	 * create clear label and radio buttons 
	 */   
   

	clearlbl = XtVaCreateManagedWidget ("Clear:",
		xmLabelGadgetClass,        	bb[0],
		XmNx,                      	10,
		XmNy,                      	15,
                NULL);
	XtManageChild (clearlbl);
    
	nn = XtNumber(clea_type);

	_clearBtnW = (WidgetList) XtMalloc( (size_t)nn * sizeof(Widget));

	for (ii=0; ii<(int)nn; ii++){
	    _clearBtnW[ii] = XtVaCreateManagedWidget (clea_type[ii],
		    xmToggleButtonGadgetClass,	_clearRcW,
		    NULL);	
	    XtAddCallback(_clearBtnW[ii], XmNvalueChangedCallback,
		    (XtCallbackProc)pgsymb_clearCb, (XtPointer)ii );
	}
	XtManageChild(_clearRcW);

	/*
	 * create color input
	 */   
	_symb_colrW = XtVaCreateManagedWidget(" ",
                xmPushButtonWidgetClass,  	bb[0],
                XmNwidth,                 	25,
                XmNheight,                 	20,
                XmNx,                     	210,
                XmNy,                      	15,
                NULL);

	XtAddCallback(_symb_colrW, XmNactivateCallback,
		      (XtCallbackProc)pgsymb_colorCb, NULL);


	/*	
	 * create  symbol size scale and text widget
	 */

	_symb_sizeScaleW = (Widget)XmCreateScale(bb[1], 
			    "Symbol_size", NULL, 0);
	xmstr = XmStringCreateLocalized("Size");
 	XtVaSetValues(_symb_sizeScaleW ,
                XmNorientation,			XmHORIZONTAL,
                XmNmaximum,			MAX_SIZE_SCALE * 10,
                XmNminimum, 			MIN_SIZE_SCALE ,
		XmNprocessingDirection,		XmMAX_ON_RIGHT,
                XmNvalue, 			10,
		XmNscaleHeight, 		20,
		XmNscaleMultiple, 		5,
                XmNshowValue, 			False,
		XmNtitleString, 		xmstr,
                XmNwidth,              		170,
		XmNx,                  		10,
		XmNy,                   	10,
		NULL);
	XmStringFree(xmstr);
	XtAddCallback(_symb_sizeScaleW, XmNdragCallback, 
                      (XtCallbackProc)pgsymb_sizeScaleCb, NULL);
	XtAddCallback(_symb_sizeScaleW, XmNvalueChangedCallback, 
                      (XtCallbackProc)pgsymb_sizeScaleCb, NULL);	
	XtManageChild(_symb_sizeScaleW);

	_symb_sizeTxtW = (Widget)XtVaCreateManagedWidget("symbol_size",
                xmTextFieldWidgetClass,		bb[1],
                XmNcolumns,			4,
                XmNvalue,			"1",
                XmNcursorPositionVisible,	True,
 		XmNx,				190,
		XmNy,                   	10,               
                NULL);
	XtAddCallback(_symb_sizeTxtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);
	XtAddCallback(_symb_sizeTxtW, XmNvalueChangedCallback, 
                      (XtCallbackProc)pgsymb_sizeTxtCb, NULL);
	/*
	 * create  symbol width scale and text widget
	 */

	_symb_widthScaleW = (Widget)XmCreateScale(bb[1], 
			     "Symbol_width", NULL, 0);
	xmstr = XmStringCreateLocalized("Width");
 	XtVaSetValues(_symb_widthScaleW ,
                XmNorientation,			XmHORIZONTAL,
                XmNmaximum,			MAX_WIDTH_SCALE,
                XmNminimum, 			MIN_WIDTH_SCALE,
		XmNprocessingDirection,		XmMAX_ON_RIGHT,
                XmNvalue, 			1,
		XmNscaleHeight, 		20,
		XmNscaleMultiple, 		1,
                XmNshowValue, 			False,
		XmNtitleString, 		xmstr,
                XmNwidth,              		170,
		XmNx,                  		10,
		XmNy,                   	60,
		NULL);
	XmStringFree(xmstr);
	XtAddCallback(_symb_widthScaleW, XmNdragCallback,
		      (XtCallbackProc)pgsymb_widthScaleCb, NULL);
	XtAddCallback(_symb_widthScaleW, XmNvalueChangedCallback, 
                      (XtCallbackProc)pgsymb_widthScaleCb, NULL);
	XtManageChild(_symb_widthScaleW);

	_symb_widthTxtW   = (Widget) XtVaCreateManagedWidget("symbol_width",
                xmTextFieldWidgetClass,		bb[1],
                XmNcolumns,			4,
                XmNvalue,			"1",
                XmNcursorPositionVisible,	True,
 		XmNx,				190,
		XmNy,                   	60,               
                NULL);
	XtAddCallback(_symb_widthTxtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);
	XtAddCallback(_symb_widthTxtW, XmNvalueChangedCallback, 
                      (XtCallbackProc)pgsymb_widthTxtCb, NULL);

	 /*	
	  * create  symbol Longitude label and text widget
	  */

	 _symb_longLabelW = (Widget)XmCreateLabel(bb[2], 
			     "Longitude", NULL, 0);
	
	 XtVaSetValues(_symb_longLabelW ,
        	 XmNorientation,		XmHORIZONTAL,
		 XmNx,                  	10,
		 XmNy,                   	60,
		 NULL);
	 XtManageChild(_symb_longLabelW);

	 _symb_longTxtW = (Widget)XtVaCreateManagedWidget("Longitude",
        	 xmTextFieldWidgetClass,	bb[2],
        	 XmNcolumns,			6,
        	 XmNvalue,			"",
        	 XmNcursorPositionVisible,	True,
 		 XmNx,				150,
		 XmNy,                   	60,               
        	 NULL);

	 XtAddCallback(_symb_longTxtW, XmNvalueChangedCallback, 
                       (XtCallbackProc)pgsymb_longTxtCb, NULL);
	 /*
	  * create  symbol Latitude scale and text widget
	  */

	 _symb_latiLabelW = (Widget)XmCreateLabel(bb[2], 
			      "Latitude", NULL, 0);
	 
	 XtVaSetValues(_symb_latiLabelW ,
        	 XmNorientation,		XmHORIZONTAL,
		 XmNx,                  	10,
		 XmNy,                   	10,
		 NULL);
	 XtManageChild(_symb_latiLabelW);

	 _symb_latiTxtW   = (Widget) XtVaCreateManagedWidget("Latitude",
        	 xmTextFieldWidgetClass,	bb[2],
        	 XmNcolumns,			6,
        	 XmNvalue,			"",
        	 XmNcursorPositionVisible,	True,
 		 XmNx,				150,
		 XmNy,                   	10,               
        	 NULL);

	 XtAddCallback(_symb_latiTxtW, XmNvalueChangedCallback, 
        	       (XtCallbackProc)pgsymb_latiTxtCb, NULL);
	/*
	 * create place symbol button
	 */

	 _symb_loLaPlacW = XtVaCreateManagedWidget("Place Symbol",
        	 xmPushButtonWidgetClass,  	bb[3],
        	 XmNwidth,                 	110,
        	 XmNheight,                 	30,
        	 XmNx,                     	10,
        	 XmNy,                      	10,
        	 NULL);

	 XtAddCallback(_symb_loLaPlacW, XmNactivateCallback,
		       (XtCallbackProc)pgsymb_latLonPlacCb, NULL);
	/*
	 * create undo/redo button
	 */

	 _symb_unRedoW = XtVaCreateManagedWidget("Undo",
        	 xmPushButtonWidgetClass,  	bb[3],
        	 XmNwidth,                 	110,
        	 XmNheight,                 	30,
        	 XmNx,                     	135,
        	 XmNy,                      	10,
        	 NULL);

	 XtAddCallback(_symb_unRedoW, XmNactivateCallback,
		       (XtCallbackProc)pgsymb_unRedoCb, NULL);

         _symbolUnRedoFlag = FALSE;


	 XtManageChild (bb[0]);
	 XtManageChild (bb[1]);
	 XtManageChild (bb[2]);
	 XtManageChild (bb[3]);
	 XtManageChild (form[0]);
	 XtManageChild (form[1]);
	 XtManageChild (_symb_loLaBbW);
	 XtManageChild (_symb_loLaCtrBbW);

         /*
          * Create label reminder
          */
         _labelReminder_formW = form[2];
         labelReminder_label  = 
             (Widget)XtVaCreateManagedWidget ("Label:  ",
		     xmLabelGadgetClass,     _labelReminder_formW,
                     XmNtopAttachment,       XmATTACH_FORM,
                     XmNtopOffset,           10,
		     XmNleftAttachment,	     XmATTACH_FORM,
                     XmNleftOffset,          10,
		     NULL);

         nn = 0;
         XtSetArg(args[nn], XmNrows,                         1); nn++;
         XtSetArg(args[nn], XmNcolumns,                     13); nn++;
         XtSetArg(args[nn], XmNcursorPositionVisible,    False); nn++;
         XtSetArg(args[nn], XmNresizeHeight,              True); nn++;
         XtSetArg(args[nn], XmNeditable,                 False); nn++;
         XtSetArg(args[nn], XmNeditMode,     XmMULTI_LINE_EDIT); nn++; 

         _labelReminder_textW =  
             (Widget)XmCreateScrolledText(_labelReminder_formW, 
                                       "Label", args, nn);

         XtVaSetValues( XtParent(_labelReminder_textW),
		XmNleftAttachment,	 XmATTACH_WIDGET,
		XmNleftWidget,           labelReminder_label,
		NULL);

         XtManageChild(_labelReminder_textW);


	/*
	 * create label widget input
	 */
	_label_formW = form[3];

        type_menu  = XtVaCreateManagedWidget ("type_menu",
	     xmRowColumnWidgetClass,	_label_formW,
	     XmNorientation,		XmVERTICAL,
	     XmNpacking,		XmPACK_TIGHT,
	     XmNnumColumns,		1,
	     NULL);

	label_form = (Widget)XtVaCreateManagedWidget("_label_formW",
	     xmFormWidgetClass,		type_menu,
	     NULL);

	_label_menuW = XtVaCreateManagedWidget ("_lin_label_menu",
	     xmRowColumnWidgetClass,	label_form, 
             XmNtopAttachment,          XmATTACH_FORM,
	     XmNorientation, 		XmHORIZONTAL,
	     NULL);

	_label_toggW = XtVaCreateManagedWidget("  ",
	     xmToggleButtonGadgetClass,	_label_menuW,
             XmNtraversalOn,            FALSE,
	     NULL);

	XtAddCallback(_label_toggW, XmNvalueChangedCallback, 
		      (XtCallbackProc)pgsymb_labelToggCb, (XtPointer)0 );

	XtVaCreateManagedWidget ("Label:",
	     xmLabelGadgetClass,		_label_menuW,
	     NULL); 

	_label_submenuW = XtVaCreateManagedWidget("_lin_lab_submenu",
	     xmRowColumnWidgetClass,	_label_menuW,
	     NULL);

	label_menubar = XmCreatePulldownMenu (_label_submenuW, 
					      "Label", NULL, 0);

	_label_optW = XmCreateOptionMenu (_label_submenuW, 
					      "label", NULL, 0);

	for (ii=0; ii < GRP_MXELE; ii++) {
	    sprintf (cc, "xxxx");
            xmstr = XmStringCreateLocalized (cc);
            _label_pbW[ii] = XtVaCreateManagedWidget(cc,
		 xmPushButtonWidgetClass,	    label_menubar,
		 NULL);
	    XtVaSetValues(_label_pbW[ii],
		XmNlabelString, 		xmstr,
		XmNalignment,		XmALIGNMENT_CENTER,
		NULL);
            XmStringFree (xmstr);
            XtAddCallback(_label_pbW[ii], XmNactivateCallback,
			  (XtCallbackProc)pgsymb_labelPbCb, (XtPointer) ii);
	}

	XtVaSetValues (_label_optW, 
	    XmNsubMenuId,			label_menubar,
	    XmNmenuHistory,			_label_pbW[0], 
	    NULL);

	XtManageChild (_label_optW);

        /*
         * create use SYMBOL color toggle
         */
	_label_menuW2 = XtVaCreateManagedWidget ("label_color_menu",
	     xmRowColumnWidgetClass,	label_form, 
             XmNtopAttachment,          XmATTACH_WIDGET,
             XmNtopWidget,              _label_menuW,
	     XmNorientation, 		XmHORIZONTAL,
	     NULL);

	_label_toggW2 = XtVaCreateManagedWidget("  ",
	     xmToggleButtonGadgetClass,	_label_menuW2,
             XmNtraversalOn,            FALSE,
	     NULL);

	XtAddCallback(_label_toggW2, XmNvalueChangedCallback, 
		      (XtCallbackProc)pgsymb_labelToggCb, (XtPointer)1 );

	XtVaCreateManagedWidget ("use SYMBOL color",
	     xmLabelGadgetClass,		_label_menuW2,
	     NULL); 

	XtManageChild (_label_formW);

	/*
	 * Create group type buttons
	 */
	group_form = (Widget)XtVaCreateManagedWidget("_group_formW",
	    xmFormWidgetClass,			type_menu,
	    NULL);

        /*
         * Build group type option menu.
         */
        ces_gtggrps(&_numGrp, &names, &iret);   
        if ( names != NULL )  free (names);

        pulldown = XmCreatePulldownMenu(group_form, "menuW", NULL, 0);
        _group_typeW = XmCreateOptionMenu(group_form, "option_menu", NULL, 0 ); 
        _group_buttonW = (WidgetList)XtMalloc((size_t)_numGrp * sizeof(Widget));

        for( ii = 0; ii < _numGrp; ii++ ) { 
             grptyp = ces_gtgmsid ( ii );
             ces_gtgnam (grptyp, grpnam, &iret);
             _group_buttonW[ii] = XtVaCreateManagedWidget(grpnam,
                  xmPushButtonWidgetClass,		pulldown,
                  NULL);

	     XtAddCallback (_group_buttonW[ii], XmNactivateCallback, 
		       (XtCallbackProc)pgsymb_grpTypCb, (XtPointer)ii);

        }

        xmstr = XmStringCreateLocalized("Group Type");
        XtVaSetValues(_group_typeW,
	    XmNsubMenuId, 	pulldown,
	    XmNlabelString, 	xmstr, 
	    NULL );
        XmStringFree(xmstr);

        XtManageChild(_group_typeW);


	/*
 	 * create Bulletin board for Apply and Cancel inputs.
 	 */   

	_symb_ctlForm =(Widget)XtVaCreateManagedWidget 
		("_symb_ctlformW",
		xmBulletinBoardWidgetClass, 	pane,
		XmNheight,			50,
		XmNwidth,			240,
        	NULL);

	/*
 	 * create  editing Apply, Cancel input
 	 */

	_symb_ctlBtns = (WidgetList)XtMalloc(
			 XtNumber(btnstr) * sizeof(Widget));
	NxmCtlBtn_create (_symb_ctlForm, 1, "symb_ctlBtns", 
		XtNumber(btnstr), btnstr, NULL, _symb_ctlBtns);
	XtManageChild(pane);

        /*
         * Initialize member variables
         */
	_labelFlag = 0;
        _labelColorFlag = 0;


	return;

}

/*=====================================================================*/

void pgsymb_createVolLst ( Widget parent )
/************************************************************************
 * pgsymb_createVolLst                                                  *
 *                                                                      *
 * This function creates the volcano list window.                       *
 *                                                                      *
 * void pgsymb_createVolLst ( parent )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          Parent widget                   *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/02                                           *
 * H. Zeng/XTRIA        10/02    modified for NxmVolcano.c              *
 ***********************************************************************/
{
Widget          pane, volcform, label;
int             loff = 5, toff = 5, info_type;
/*---------------------------------------------------------------------*/

        /*
         * create a Form Dialog for the volcano list
         */

        _volc_list = XmCreateFormDialog ( parent, "Volcano_list",
                                        NULL, 0 );

        XtVaSetValues(_volc_list,
                XmNdefaultPosition,             False,
                XmNnoResize,                    True,
                XmNautoUnmanage,                FALSE,
                NULL);


        pane  = XtVaCreateManagedWidget ("pane",
                xmPanedWindowWidgetClass,       _volc_list,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     3,
                XmNsashWidth,                   1,
                XmNsashHeight,                  1,
                NULL);


        /*
         * Create the volcano list menu
         */
        volcform = XtVaCreateWidget("form",
                            xmFormWidgetClass,  pane,
                            XmNnumColumns,      1,
                            XmNorientation,     XmHORIZONTAL,
                            XmNradioBehavior,   FALSE,
                            XmNpacking,         XmPACK_TIGHT,
                            NULL );
        /*
         * create label
         */
        label = XtVaCreateManagedWidget ("tmlabel",
                                     xmLabelWidgetClass,   volcform,
                                     XmNtopAttachment,     XmATTACH_FORM,
                                     XmNtopOffset,         toff,
                                     XmNleftAttachment,    XmATTACH_FORM,
                                     XmNleftOffset,        toff,
                                     NULL);
        NxmLabel_setStr(label, "Volcano: ");

        /*
         * create text field
         */
        _volcTxtW = XtVaCreateManagedWidget ("tmtext",
                                     xmTextFieldWidgetClass, volcform,
                                     XmNcolumns,             32,
                                     XmNtopAttachment,       XmATTACH_WIDGET,
                                     XmNtopWidget,           label,
                                     XmNtopOffset,           loff,
				     XmNleftAttachment,      XmATTACH_FORM,
                                     XmNleftOffset,          toff,
				     XmNbottomAttachment,    XmATTACH_FORM,
                                     XmNbottomOffset,        toff,
                                     NULL);

        _volcIdx = -1;
        XtVaSetValues (_volcTxtW, 
                       XmNvalue,                   "\0", 
                       XmNeditable,                TRUE,
                       XmNcursorPositionVisible,   TRUE,
                       NULL );

        info_type = 0;
        XtVaSetValues (_volcTxtW,  XmNuserData, info_type, NULL);

        /*
         * create menu bar
         */
        NxmVolcano_menuCreate(volcform, _volcTxtW, 
				(XtCallbackProc)pgsymb_menuTextCb);
                     
       
        XtManageChild(volcform);
        XtManageChild(pane);

}

/*=====================================================================*/

void pgsymb_popup ( int subtyp, int elmid, int show_ctl,
		    XtCallbackProc callback, char *label_ptr,
		    Boolean init_grp )
/************************************************************************
 * pgsymb_popup								*
 *									*
 * This function shows a VG symbol attribute  box.			*
 *									*
 * void pgsymb_popup (subtyp, elmid, show_ctl, callback, label_ptr, 	*
 *							init_grp)	*
 *									*
 * Input parameters:							*
 *	subtyp		int	subtype					*
 *	elmid		int	element ID				*
 *	show_ctl 	int	show control buttons flag		*
 *	callback	XtCallbackProc	callback for Apply/Cancel btns	*
 *      label_ptr       char*   pointer to the possible label string    *
 *	init_grp	Boolean	re-initialize the group menu		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	03/98							*
 * W. Li/EAI		03/98	add "Apply", "Cancel" buttons & callback*
 * F. J. Yen/NCEP	04/98	Replaced with new ces function names	*
 * W. Li/EAI	02/99	added label types				*
 * W. Li/EAI	03/99	added clear types				*
 * W. Li/EAI	03/99	added lat/lon position editting			*
 * W. Li/EAI	03/99	removed long/latitude initial setting		*
 * W. Li/EAI	05/99	added group type in symbol editor		*
 * H. Zeng/EAI  04/00   modified for new grptyp.tbl                     *
 * H. Zeng/EAI  06/00   added label color toggle initialization         *
 * H. Zeng/EAI  07/00   added a new para. label_ptr                     *
 * H. Zeng/EAI  02/01   added call to pggrpw_initType()                 *
 * H. Zeng/EAI  03/01   modified to check group type id                 *
 * E. Safford/GSC	03/01	add init_grp param, mod edit setup      *
 * M. Li/SAIC	10/01	Added CLASS_MARKER				*
 * H. Zeng/EAI  10/01   revised for new GROUP functionality             *
 * E. Safford/SAIC	12/01   wipe text fields B4 assigning values	*
 * M. Li/SAIC		04/02	Added pglabel_setLabelPending		*
 * M. Li/SAIC		05/02   Checked for the active Grouping		*
 * M. Li/SAIC		06/02   add check for the volcano symbol	*
 * J. Wu/SAIC		08/02   set label type to text			*
 * H. Zeng/XTRIA        10/02   added call to pgsymb_menuTextCb()       *
 * J. Wu/SAIC		05/03   reset VOLCANO list to default when popup*
 * E. Safford/SAIC	05/05	use and free up volStr			*
 ***********************************************************************/
{
VG_DBStruct 	el;	
int		iret, oper_id, group_type;
long		ii;
char		txtstr[10], *volStr;
/*---------------------------------------------------------------------*/

	if (XtIsManaged ( _symb_dlgW ) )
	    XtUnmanageChild ( _symb_dlgW );

	pglabel_setLabelPending (False);

       	/*
         * Popup/reset the Volcano List window only when drawing a new
	 * volcano but reset it to default after each drawing. For editting
	 * an existing volcano symbol, do not pop up Volcano List window.
         */
       	if ( subtyp == VOLCANO && !show_ctl ) {
	    XtManageChild(_volc_list);
	    _labelFlag = TRUE;	    

            pgsymb_menuTextCb ( NULL, 0, NULL ); 
	}


	if (show_ctl) {
	    oper_id = pgpalw_getCurOperId();
	    XtManageChild (_symb_ctlForm);
	    XtUnmanageChild (_label_formW);
	    XtUnmanageChild (_symb_loLaCtrBbW);

	    if ( oper_id == FUNC_MULTISEL) {
		XtUnmanageChild (_symb_loLaBbW);
	    }
	    else if(oper_id == FUNC_SELECT){
		XtManageChild (_symb_loLaBbW);
	    }
	    
	    if (callback) {
		for (ii=0; ii< 2; ii++) {
		    XtRemoveAllCallbacks (_symb_ctlBtns[ii], 
				          XmNactivateCallback);
		    XtAddCallback (_symb_ctlBtns[ii],
			XmNactivateCallback, (XtCallbackProc)callback, (XtPointer)ii);
	        }
            }
        } 
	else {
	    XtUnmanageChild (_symb_ctlForm);
	    XtManageChild (_symb_loLaBbW);
	    XtManageChild (_symb_loLaCtrBbW);

            /*
             * If GROUP is active, or current class is combo symbo, disable
             * "LABEL" capability.
             */
	    if ( pgpalw_getCurClassId() != CLASS_COMSYM &&
		 pgpalw_getCurClassId() != CLASS_MARKER ) {
		 XtManageChild (_label_formW);
	    }
	    else { 
                XmToggleButtonSetState(_label_toggW, FALSE, TRUE);
		XtUnmanageChild (_label_formW);
	    }
	}

	/* 
	 * save parameter to local variable
	 */

	_vgType  = (char)elmid;


	/* 
	 * set class and vg_type
	 */

	el.hdr.vg_class = (char)CLASS_SYMBOLS;
	el.hdr.vg_type  = _vgType;
	_subTyp = subtyp;

	/* 
	 * Get attributes from setting table
	 */

	ces_get(_subTyp, &el, &iret);

	_symbSize  = el.elem.sym.info.size;
	_symbWidth = el.elem.sym.info.width;
	_symbColr  = el.hdr.maj_col;


        volStr = XmTextGetString( _volcTxtW );

	if ( subtyp == VOLCANO && ( strlen( volStr ) > (size_t)0 )){
	    XtSetSensitive(_symb_loLaPlacW, TRUE);
	}
	else {
	    XtSetSensitive(_symb_loLaPlacW, FALSE);
	}
        XtFree( volStr );

	/*
	 * set clear buttons
	 */
 
	if (_symbWidth >= 800) {
	    XmToggleButtonGadgetSetState( _clearBtnW[0], TRUE,  FALSE);
	    XmToggleButtonGadgetSetState( _clearBtnW[1], FALSE, FALSE);
	    _clearType = 800;
	}
	else {
	    XmToggleButtonGadgetSetState( _clearBtnW[0], FALSE, FALSE);
	    XmToggleButtonGadgetSetState( _clearBtnW[1], TRUE,  FALSE);
	    _clearType = 0;
	}


	/*
	 * set symbol size
	 */
	sprintf(txtstr, "%3.1f", _symbSize);
	XmTextSetString(_symb_sizeTxtW, "");
	XmTextSetString(_symb_sizeTxtW, txtstr);
	XmScaleSetValue(_symb_sizeScaleW, (int) (_symbSize * 10.0F));

	/*
	 * set symbol  width
	 */
	sprintf(txtstr, "%i", (_symbWidth % 800));
	XmTextSetString(_symb_widthTxtW, "");
	XmTextSetString(_symb_widthTxtW, txtstr);
	XmScaleSetValue(_symb_widthScaleW, (_symbWidth % 800));

	/*
	 * set symbol color
	 */
	XtVaSetValues(_symb_colrW,
		XmNbackground, 		NxmColrP_getColorPixel(_symbColr),
                XmNtopShadowColor, 	NxmColrP_getColorPixel(_symbColr),
                XmNbottomShadowColor, 	NxmColrP_getColorPixel(_symbColr),
		NULL);


        /*
         * If there is a label string associated with the symbol,
         * show it on the attribute window.
         */
        if( label_ptr != NULL ) {
           XtManageChild(_labelReminder_formW);
           XmTextSetString (_labelReminder_textW, label_ptr);
      
        }
        else {
	   if( XtIsManaged(_labelReminder_formW) ) {
             XtUnmanageChild(_labelReminder_formW);
           }
        } 

	XtManageChild ( _symb_dlgW );

       /*
	* set group type and label items. 
	*/
       if( init_grp ) {
          pgsymb_initType( &el );
          group_type = _groupTyp;
          pgsymb_setLabItems(group_type);
       }

       XmToggleButtonGadgetSetState (_label_toggW2, (int)_labelColorFlag, TRUE);
       XmToggleButtonGadgetSetState (_label_toggW, (int)_labelFlag, TRUE);
       XtSetSensitive(_label_submenuW, (int)_labelFlag);
       XtSetSensitive(_label_menuW2, (int)_labelFlag);
    	if ( pgpalw_isGrpActv() ) {
       	    XtSetSensitive(_group_typeW, False);
    	}
    	else {
       	    XtSetSensitive(_group_typeW, (int)_labelFlag);
    	}

       /*
	* set undo/redo button 
	*/
       XtSetSensitive(_symb_unRedoW, FALSE);

       /*
	* set label type to text. 
	*/
       pglabel_setLabType( 1 );

}

/*=====================================================================*/

void pgsymb_popdown ( void )
/************************************************************************
 * pgsymb_popdown							*
 *									*
 * This function unmanages the symbol attribute editing window		*
 *									*
 * void pgsymb_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * W. Li/EAi	03/98							*
 * M. Li/SAIC	06/02	Popdown the volcano list window			*
 ***********************************************************************/
{
	NxmClrW_popdown();
	if (XtIsManaged(_symb_dlgW) ) {
	    XtUnmanageChild(_symb_dlgW);
	}

	if (XtIsManaged(_volc_list) ) {
            XtUnmanageChild(_volc_list);
        }

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_colorCb ( Widget w, XtPointer clnt, 
			XmPushButtonCallbackStruct *cbs )
/************************************************************************
 * pgsymb_colorCb                                                       *
 *                                                                      *
 * Callback for color button widget					*
 *                                                                      *
 * void pgsymb_colorCb (w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w			Widget                  Widget ID               *
 *   clnt        XtPointer               color                   *
 *   *cbs         XmPushButtonCallbackStruct   callback struct    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/98							*
 ***********************************************************************/
{
	NxmClrW_popup (w, (XtPointer)&_symbColr, (XtPointer)cbs);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_clearCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsymb_clearCb                                                      	*
 *                                                                      *
 * Callback for clear button widget.                            	*
 *                                                                      *
 * void pgsymb_clearCb( w, clnt, cbs)        	        *
 *									*
 * Input parameters:                                                    *
 *   w		    Widget	    	Widget ID			*
 *   clnt    XtPointer	    	button ID			*
 *   cbs      XtPointer	    	not used			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 * M. Li/SAIC	12/02	Radio_box -> check_box				*
 ***********************************************************************/
{  
    int which_button;
/*---------------------------------------------------------------------*/

    which_button = (long)clnt;

    if (which_button == 0){
        _clearType = 800;
	XmToggleButtonGadgetSetState( _clearBtnW[1], FALSE, FALSE);
        XmToggleButtonGadgetSetState( _clearBtnW[0], TRUE, FALSE);
    }
    else if (which_button == 1){
        _clearType = 0;
	XmToggleButtonGadgetSetState( _clearBtnW[0], FALSE, FALSE);
        XmToggleButtonGadgetSetState( _clearBtnW[1], TRUE, FALSE);
    }
    pgsymb_saveAttr();

}
/*=====================================================================*/
/* ARGSUSED */
void pgsymb_sizeScaleCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_sizeScaleCb							*
 *                                                                      *
 * Callback for symbol size scale widget.                               *
 *                                                                      *
 * void pgsymb_sizeScaleCb( w, clnt, cbs)                  *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	Widget ID                       *
 *   clnt    XtPointer       	not used                        *
 *   *cbs     XmScaleCallbackStruct callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI         3/98						*
 * E. Safford/SAIC	12/01	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
float	symbsize_f;   
char	txtstr[10];
/*---------------------------------------------------------------------*/

    symbsize_f = (float)(cbs->value)/10.0F;
    sprintf(txtstr, "%3.1f", symbsize_f);
    XmTextSetString(_symb_sizeTxtW, "");
    XmTextSetString(_symb_sizeTxtW, txtstr);
    _symbSize = symbsize_f;

    if (cbs->reason == XmCR_VALUE_CHANGED)
	pgsymb_saveAttr();

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_sizeTxtCb ( Widget w, XtPointer clnt, 
				XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_sizeTxtCb                                                     *
 *                                                                      *
 * Callback for symbol size text widget.                                *
 *                                                                      *
 * void pgsymb_sizeTxtCb( w, clnt, cbs)                    *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI         	 3/98						*
 * F. J. Yen/NCEP	 4/98	Replaced with new ces function name	*
 ***********************************************************************/
{
char		*s;
int		slval;
float		symbsize;
/*---------------------------------------------------------------------*/

	if (!cbs->event)
	return;

	s = XmTextGetString(_symb_sizeTxtW);
	symbsize = (float)atof(s);
	XtFree(s);


	/* 
	 *if the value on corresponding slider is different, 
         * set the sliders value accordingly.
	 */

	XmScaleGetValue(_symb_sizeScaleW, &slval);


	if ( (symbsize >= ((float)MIN_SIZE_SCALE/10.0F)) && 
	     (symbsize <= (float)MAX_SIZE_SCALE )) {

            if ( (int) (symbsize * 10.0F) != slval) {
                XmScaleSetValue(_symb_sizeScaleW, (int) (symbsize * 10.0F));
            }
	    _symbSize = symbsize;
	    pgsymb_saveAttr();
	}
}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_widthScaleCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_widthScaleCb							*
 *                                                                      *
 * Callback for symbol width scale widget.				*
 *                                                                      *
 * void pgsymb_widthScaleCb( w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI         3/98						*
 ***********************************************************************/
{
char txtstr[10];
/*---------------------------------------------------------------------*/

    sprintf(txtstr, "%i", cbs->value);
    XmTextSetString(_symb_widthTxtW, txtstr);

    _symbWidth = cbs->value;
    if (cbs->reason == XmCR_VALUE_CHANGED)
	pgsymb_saveAttr();
}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_widthTxtCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_widthTxtCb							*
 *                                                                      *
 * Callback for symbol width text widget.				*
 *                                                                      *
 * void pgsymb_widthTxtCb( w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		 3/98						*
 * F. J. Yen/NCEP	 4/98	Replaces with new ces function names	*
 ***********************************************************************/
{
char        	*s;
int         	slval;
int		symbwidth;
/*---------------------------------------------------------------------*/

	if (!cbs->event) 
	return;

	s = XmTextGetString(_symb_widthTxtW);
	symbwidth = (atoi(s));
	XtFree(s);

	/* 
	 *if the value on corresponding slider is different, 
	 *set the sliders value accordingly.
	 */

	XmScaleGetValue(_symb_widthScaleW, &slval);

        if ((symbwidth >= MIN_WIDTH_SCALE) && (symbwidth <= MAX_WIDTH_SCALE)){

            if ((int)(symbwidth) != slval){
                XmScaleSetValue(_symb_widthScaleW, (int) (symbwidth));
            }       
	    _symbWidth= symbwidth;
	    pgsymb_saveAttr();
        }

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_labelToggCb ( Widget w, XtPointer clnt, 
					XmAnyCallbackStruct *cbs )
/************************************************************************
 * pgsymb_labelToggCb                                                   *
 *                                                                      *
 * Callback for label toggle button widget.                             *
 *                                                                      *
 * void pgsymb_labelToggCb( w, clnt, cbs)                  *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmAnyCallbackStruct  callback struct                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	02/99							*
 * W. Li/EAI	05/99	added group type in symbol editor		*
 * H. Zeng/EAI  04/00   modified for new grptyp.tbl                     *
 * H. Zeng/EAI  06/00   added label color toggle callback               *
 * M. Li/SAIC	05/02   Check for the active Grouping			*
 ***********************************************************************/
{
     int      which_toggle;
     Boolean  btnval;
/*---------------------------------------------------------------------*/

    /* check to see if the button is up or down and take appropriate
     * actions.
     */
    XtVaGetValues(w, XmNset, &btnval, NULL);
    which_toggle = (long)clnt;

    if( which_toggle == 0 ) {
      _labelFlag = (char) ((btnval) ? 1 : 0);
      
      XtSetSensitive(_label_submenuW, (int)_labelFlag);
      XtSetSensitive(_label_menuW2, (int)_labelFlag);
      if ( pgpalw_isGrpActv() ) {
          XtSetSensitive(_group_typeW, False);
      }
      else {
          XtSetSensitive(_group_typeW, (int)_labelFlag);
      }
    }
    else {
      _labelColorFlag = (char) ((btnval) ? 1 : 0);

    }

}
/*=====================================================================*/
/* ARGSUSED */
void pgsymb_labelPbCb ( Widget w, long clnt, XtPointer cbs ) 
/************************************************************************
 * pgsymb_labelPbCb							*
 *									*
 * This function is the callback function of symbol label widget.	*
 *									*
 * void pgsymb_labelPbCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	w		Widget			widget ID		*
 *	clnt	int			clnt data		*
 *	cbs	XmAnyCallbackStruct	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	        02/99    initial coding				*
 * H. Zeng/EAI          04/00    modified for new grptyp.tbl            *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pgsymb_setLabValue ((int)clnt);
}

/*=====================================================================*/

void pgsymb_saveAttr ( void )
/************************************************************************
 * pgsymb_saveAttr							*
 *									*
 * Stores the new Attributes set by call back functions			*
 *									*
 * void pgsymb_saveAttr ()						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		03/98						*
 * F. J. Yen/NCEP	04/98	Replaced with new ces function names	*
 * W. Li/EAI		03/99	Added latitude/longitude and clear type	*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()			*
 * E. Safford/SAIC	02/02	load group type into el for ces_set 	*
 ***********************************************************************/
{
VG_DBStruct el;
int ier;
/*---------------------------------------------------------------------*/

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_class = (char)CLASS_SYMBOLS;
    el.hdr.vg_type  = _vgType;
    ces_get(_subTyp, &el, &ier);
    el.hdr.maj_col  = _symbColr;
    el.hdr.min_col  = _symbColr;
    el.elem.sym.info.size = _symbSize;
    el.elem.sym.info.width = _symbWidth % 800 + _clearType;


    if (_Latitude >= -90.0F && _Latitude <= 90.0F && 
        _Longitude >= -180.0F && _Longitude <= 180.0F && 
	!G_DIFF(_Latitude, 0.0F) && !G_DIFF(_Longitude, 0.0F) ) {

        el.elem.sym.data.latlon[0] = _Latitude;
        el.elem.sym.data.latlon[1] = _Longitude;
        XtSetSensitive(_symb_loLaPlacW, TRUE);
    }
    else {
        XtSetSensitive(_symb_loLaPlacW, FALSE);
    }

    if ( _groupTyp >= 0 ) {
	el.hdr.grptyp = (char)_groupTyp;
    }

    ces_set(_subTyp, &el, &ier);

}

/*=====================================================================*/
 
void pgsymb_setAttr ( int colr, float size, int width )
/************************************************************************
 * pgsymb_setAttr                                                       *
 *                                                                      *
 * This function sets the symbol color, size, and width			*
 *                                                                      *
 * void pgsymb_setAttr ( colr, size, width )			        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *	colr		int		color value			*
 *	size  		float		size value			*
 *	width		int		width value			*
 *                                                                      *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		03/98						*
 * W. Li/EAI		03/99	table setting clear type (>800 clear on)*
 * E. Safford/SAIC	12/01	wipe txt fields to assist verification  *
 ***********************************************************************/
{
char      str[10];
/*---------------------------------------------------------------------*/

	_symbColr   = colr;
	_symbSize   = size;
	_symbWidth  = width;

	if (_symbWidth >= 800) {
	    XmToggleButtonGadgetSetState( _clearBtnW[0], TRUE,  FALSE);
	    XmToggleButtonGadgetSetState( _clearBtnW[1], FALSE, FALSE);
	    _clearType = 800;
	}
	else {
	    XmToggleButtonGadgetSetState( _clearBtnW[0], FALSE, FALSE);
	    XmToggleButtonGadgetSetState( _clearBtnW[1], TRUE,  FALSE);
	    _clearType = 0;
	}

	XtVaSetValues(_symb_colrW,
	    XmNbackground,		NxmColrP_getColorPixel(_symbColr),
            XmNtopShadowColor,		NxmColrP_getColorPixel(_symbColr),
            XmNbottomShadowColor,	NxmColrP_getColorPixel(_symbColr),
	    NULL);

        if ((size >= ((float)MIN_SIZE_SCALE/10.0F)) && 
	    (size <= (float)MAX_SIZE_SCALE)) {
	    sprintf (str, "%3.1f", size);
            XmTextSetString(_symb_sizeTxtW, "");
            XmTextSetString(_symb_sizeTxtW, str);
            XmScaleSetValue(_symb_sizeScaleW, (int) (size * 10.0F));
	}
        if (((width % 800) >= MIN_WIDTH_SCALE) && 
	    ((width % 800)<= MAX_WIDTH_SCALE)) {
	    sprintf (str, "%i", (width % 800));
            XmTextSetString(_symb_widthTxtW, "");
            XmTextSetString(_symb_widthTxtW, str);
            XmScaleSetValue(_symb_widthScaleW, (width % 800));
	}

	pgsymb_saveAttr ();


}

/*=====================================================================*/

void pgsymb_getColor ( int *colr )
/************************************************************************
 * pgsymb_getColor							*
 *									*
 * This function gets symble color. 					*
 *									*
 * pgsymb_getColor( colr )						*
 *									*
 * Output parameters:							*
 * 	*colr	int	symble color					*
 * 									*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	 	10/98						*
 ***********************************************************************/
{
	*colr = _symbColr; 
}

/*=====================================================================*/

void pgsymb_setLabFlag ( Boolean lab_flag )
/************************************************************************
 * pgsymb_setLabFlag							*
 *									*
 * set the label flag 							*
 *									*
 * void pgsymb_setLabFlag (lab_flag)					*
 *									*
 * Input parameters:							*
 *	lab_flag	Boolean	lab_flag				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99	initial coding				*
 * W. Li/EAI	        05/99	added group type in symbol editor	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * M. Li/SAIC   05/02   Check for the active Grouping                   *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    _labelFlag = lab_flag;

    XtSetSensitive(_label_submenuW, (int)_labelFlag);
    if ( pgpalw_isGrpActv() ) {
        XtSetSensitive(_group_typeW, False);
    }
    else {
        XtSetSensitive(_group_typeW, (int)_labelFlag);
    }

}

/*=====================================================================*/

void pgsymb_setLabItems ( int group_type )
/************************************************************************
 * pgsymb_setLabItems							*
 *									*
 * set the label item strings based on the group type and choose the    *
 * default label. 							*
 *									*
 * void pgsymb_setLabItems (group_type)					*
 *									*
 * Input parameters:							*
 *	group_type	int	group type				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI  	04/00   initial coding                          *
 * H. Zeng/EAI  	04/00   added _groupTyp&_objId variable         *
 * E. Safford/GSC	12/00	rename grp defines, fix comp warnings   *
 * H. Zeng/EAI          03/01   modified to use ces_gtglbls()           *
 * E. Safford/SAIC	04/02	handle empty label string		*
 * H. Zeng/EAI          04/02   modified to use cur_grptyp              *
 ***********************************************************************/
{
int         ii, which_label, obj, nlbl = 0, iret;
char        *ptr, lbls[256];
XmString    xmstr;
static int  cur_grptyp = IMP_CHOICE;
/*---------------------------------------------------------------------*/

    obj = pgpalw_getCurObjId(); 

    if(group_type != cur_grptyp || obj != _objId) {
        cur_grptyp = group_type;
        _objId = obj;

        /*
         * Set label item strings based on selected group type
         */
        ces_gtglbls(group_type, &nlbl, lbls, &iret);
        if ( iret < 0 ) {
 	    strcpy (lbls, "Other;");
	    nlbl = 1;
        }

        ii = 0;
        ptr = strtok(lbls, ";");
        while ( ptr != (char *)NULL && ii < nlbl ) {
            xmstr = XmStringCreateLocalized (ptr);
	    XtVaSetValues(_label_pbW[ii],
	      XmNlabelString, 		xmstr,
	      XmNalignment,		XmALIGNMENT_CENTER,
	      NULL);
    	    XtManageChild(_label_pbW[ii]);
            XmStringFree (xmstr);

	    ptr = strtok(NULL, ";" );
            ii++;

        }

        for (ii = nlbl; ii < GRP_MXELE; ii++) {
    	    XtUnmanageChild(_label_pbW[ii]);
        }


        /*
         * Set default label item
         */
        if(group_type == 8) {

            /*
             * When the group type is LABEL
             */
            which_label = 12;
        }
        else {
            which_label = 0;
        }


        pgsymb_setLabValue (which_label);

    }/* the end of if */    
}

/*=====================================================================*/

Boolean pgsymb_isUp ( void )
/************************************************************************
 * pgsymb_isUp								*
 *									*
 * This function returns true if the atribute edit window is active or	*
 * false if it is not.							*
 *									*
 * Boolean pgsymb_isUp ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgsymb_isUp	Boolean	TRUE  -- attribute edit window is active	*
 *	       		FALSE -- attribute edit window is not active	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	11/98	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged(_symb_dlgW));
}

/*=====================================================================*/

void pgsymb_setLabValue ( int which )
/************************************************************************
 * pgsymb_setLabValue							*
 *									*
 * put the selected label name into the text string 			*
 *									*
 * void pgsymb_setLabValue (which)					*
 *									*
 * Input parameters:							*
 *	which		int	which label				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * W. Li/EAI		03/99	changed value setting to XtVaGetValues	*
 * W. Li/EAI		06/99	use text value to set label value	*
 * E. Safford/GSC	08/99	fix error w/ _labelName and *text	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * E. Safford/SAIC	07/04	add call to ces_gtginfo			*
 ***********************************************************************/
{
    XmString	xmstr;
    char	*text;
    int		ier;
/*---------------------------------------------------------------------*/

    XtVaSetValues (_label_optW, 
	XmNmenuHistory,			_label_pbW[which], 
	NULL);

    XtVaGetValues(_label_pbW[which],
	    XmNlabelString, 		&xmstr,
	    NULL);    

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text);
    XmStringFree(xmstr); 


    if ( strcmp(text, "Other")!=0 ){
        sprintf(_labelName, "%s", text);
    }
    else {
	sprintf(_labelName, "%s", "");
    }

    ces_gtginfo( _groupTyp, _labelName, _symbolAttrStr, &ier );

    XtFree(text);


}

/*=====================================================================*/

Boolean pgsymb_getLabFlag ( void )
/************************************************************************
 * pgsymb_getLabFlag							*
 *									*
 * This function returns a boolean value specifying whether the lines	*
 * labelFlag is TRUE or FALSE. 						*
 *									*
 * Boolean pgsymb_getLabFlag( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgsymb_getLabFlag Boolean  True -- use flage, False -- do not use  	*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	06/99							*
 ***********************************************************************/
{
    return( _labelFlag );

}
/*=====================================================================*/

Boolean pgsymb_getLabColorFlag ( void )
/************************************************************************
 * pgsymb_getLabColorFlag						*
 *									*
 * This function returns a boolean value specifying whether the label   *
 * color flag is TRUE or FALSE. 					*
 *									*
 * Boolean pgsymb_getLabColorFlag()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgsymb_getLabColorFlag Boolean  True -- use flage,                   *
 *                                 False -- do not use  	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	   06/00    initial coding				*
 ***********************************************************************/
{
    return( _labelColorFlag );

}
/*=====================================================================*/

void pgsymb_getLabValue ( char *label )
/************************************************************************
 * pgsymb_getLabValue							*
 *									*
 * This function gets the label text value 				*
 *									*
 * void pgsymb_getLabValue ( label)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*label		char		* label value			*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * M. Li/SAIC		06/02	set the volcano text			*
 * M. Li/SAIC		06/02	Modified the format of volcano lat/lon	*
 * H. Zeng/XTRIA        10/02   modified for NxmVolcano.c               *
 ***********************************************************************/
{
    char        *ptext;
    char        sn[2], ew[2], name[33];
    float 	vlat, vlon;
/*---------------------------------------------------------------------*/
    if (_subTyp == VOLCANO) {

        if ( _volcIdx == -1 ) {
             ptext = XmTextFieldGetString( _volcTxtW );
             sprintf(_labelName, "%s", ptext); 
             XtFree(ptext);
        }
        else {

             NxmVolcano_getInfo(_volcIdx, &vlat, &vlon, name);

	     if ( vlat > 0.0F ) {
	         strcpy(sn, "N");
	     }
	     else if ( G_DIFF(vlat, 0.0F) ) {
                 strcpy(sn, " ");
             }
	     else {
	         strcpy(sn, "S");
	         vlat = - vlat;
	     }

	     if ( vlon > 0.0F ) {
	         strcpy(ew, "E");
	     }
	     else if ( G_DIFF(vlon, 0.0F) ) {
                 strcpy(ew, " ");
             }
	     else {
	         strcpy(ew, "W");
	         vlon = - vlon;
	     }

             sprintf(_labelName, "%s\n%2.1f%s %3.1f%s", name, vlat, sn, 
                     vlon, ew); 

        }

    }

    strcpy(label, _labelName);

}

/*=====================================================================*/

void pgsymb_getAttrStr ( char *info )
/************************************************************************
 * pgsymb_getAttrStr							*
 *									*
 * This function gets a copy of _symbAttrStr string.			*
 *									*
 * void pgline_getAttrStr (info)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*info		char		copy of _symbAttrStr		*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	07/04	initial coding				*
 ***********************************************************************/
{
    sprintf(info, "%s", _symbolAttrStr);
}



/*=====================================================================*/
/* ARGSUSED */
void pgsymb_longTxtCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_longTxtCb                                                     *
 *                                                                      *
 * Callback for symbol longitude text widget.				*
 *                                                                      *
 * void pgsymb_longTxtCb( w, clnt, cbs)                    *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI         	 3/98						*
 * W. Li/EAI         	 3/99	added text field value check		*
 * J. Wu/SAIC         	 8/01	eliminate compiler warnings		*
 * R. Tian/SAIC          1/02   accept degrees:minutes format           *
 ***********************************************************************/
{
char		*s;
long		position;
double		tmp_long;
int		iret;

/*---------------------------------------------------------------------*/

    if (!cbs->event)
    return;

    s = XmTextGetString(_symb_longTxtW);

    position = XmTextGetLastPosition(_symb_longTxtW);

    if (((int)s[position -1] >= 48 && (int)s[position -1] <= 57) || 
	(int)s[position -1] == 46 || (int)s[position -1] == 43 ||
	(int)s[position -1] == 45 || (int)s[position -1] == 58) {

	cst_ctod(s, &tmp_long, &iret);
	_Longitude = (float)tmp_long;
        pgsymb_saveAttr();

    }
    else {
	XmTextFieldReplace (_symb_longTxtW, position, (position-1), '\0');
    }

    XtFree(s);

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_latiTxtCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgsymb_latiTxtCb							*
 *                                                                      *
 * Callback for symbol latitude text widget.				*
 *                                                                      *
 * void pgsymb_latiTxtCb( w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer       not used                            *
 *   *cbs     XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		 3/98						*
 * W. Li/EAI         	 3/99	added text field value check		*
 * J. Wu/SAIC         	 8/01	eliminate compilor warnings		*
 * R. Tian/SAIC          1/02   accept degrees:minutes format           *
 ***********************************************************************/
{
char        	*s;
long         	position;
double		tmp_lati;
int		iret;

/*---------------------------------------------------------------------*/

    if (!cbs->event) 
    return;

    s = XmTextGetString(_symb_latiTxtW);

    position = XmTextGetLastPosition(_symb_latiTxtW);

    if (((int)s[position -1] >= 48 && (int)s[position -1] <= 57) || 
	(int)s[position -1] == 46 || (int)s[position -1] == 43 ||
	(int)s[position -1] == 45 || (int)s[position -1] == 58) {

        cst_ctod(s, &tmp_lati, &iret);
        _Latitude = (float)tmp_lati;
	pgsymb_saveAttr();

    }
    else {
	XmTextFieldReplace (_symb_latiTxtW, position, (position-1), '\0');
    }

    XtFree(s);

}


/*=====================================================================*/

void pgsymb_setLatLon ( float latitude, float longitude )
/************************************************************************
 * pgsymb_setLatLon                                                  	*
 *                                                                      *
 * sets the symbol long/latitude position				*
 *                                                                      *
 * void pgsymb_setLatLon(latitude, longitude)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   latitude		float		latitude			*
 *   longitude		float		longitude			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 * E. Safford/SAIC	12/01	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
char	long_txtstr[10], lati_txtstr[10];
/*---------------------------------------------------------------------*/
    if ((longitude >= -180.00F) && (longitude <= 180.00F) &&
	(latitude  >= -90.00F ) && (latitude  <= 90.00F )) {

	sprintf(long_txtstr, "%3.2f", longitude);
	sprintf(lati_txtstr, "%3.2f", latitude);

	XmTextSetString(_symb_longTxtW, ""); 
	XmTextSetString(_symb_latiTxtW, "");

	XmTextSetString(_symb_longTxtW, long_txtstr); 
	XmTextSetString(_symb_latiTxtW, lati_txtstr);

	_Longitude = longitude;	
	_Latitude = latitude;

	XtSetSensitive(_symb_loLaPlacW, FALSE);
    }	
}

/*=====================================================================*/

void pgsymb_getXxYyCoord ( float *xx, float *yy )
/************************************************************************
 * pgsymb_getXxYyCoord                                                  *
 *                                                                      *
 * gets the symbol coordinate position					*
 *                                                                      *
 * void pgsymb_getXxYyCoord(xx, yy)					*
 *                                                                      *
 * Output parameters:                                                   *
 *   	*xx		float		x coodinate			*
 *   	*yy		float		y coodinate			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 * M. Li/GSC	1/00	Used string variables in gtrans			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 ***********************************************************************/
{
 float	rx[2], ry[2], lat_input[2], lont_input[2];
 int	np, ier;
/*---------------------------------------------------------------------*/
    np =1;
    lat_input[0]= _Latitude;
    lont_input[0]=_Longitude;
    gtrans (sys_M, sys_D, &np, lat_input, lont_input, rx, ry, 
            &ier, strlen(sys_M), strlen(sys_D) );

    *xx = rx[0];
    *yy = ry[0];
}

/*=====================================================================*/

char pgsymb_getGrptyp ( void ) 
/************************************************************************
 * pgsymb_getGrptyp                                                     *
 *                                                                      *
 * returns the current group type 					*
 *                                                                      *
 * char pgsymb_getGrptyp( )      					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 * Return:								*
 *	pgsymb_getGrptyp    char	current group type		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC	04/01						*
 ***********************************************************************/
{
char	grptyp;
/*---------------------------------------------------------------------*/

    grptyp = (char)(( pgsymb_getLabFlag() ) ? _groupTyp : 0);
    return ( grptyp );

}

/*=====================================================================*/

int pgsymb_getObjId ( void )
/************************************************************************
 * pgsymb_getObjId                                                      *
 *                                                                      *
 * returns the current object Id in the symbol class.			*
 *                                                                      *
 * char pgsymb_getObjId ( )                                             *
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return:                                                              *
 *      pgsymb_getObjId     int         current object Id		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  M. Li/SAIC		06/02						*
 ***********************************************************************/
{

    return ( _objId );

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_latLonPlacCb ( Widget w, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgsymb_latLonPlacCb							*
 *                                                                      *
 * Callback for place symbol button widget				*
 *                                                                      *
 * void pgsymb_latLonPlacCb(w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w			Widget                  Widget ID               *
 *   clnt        XtPointer               color                   *
 *   cbs         XtPointer   callback struct    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 * M. Li/GSC	 1/00	Used string variables in gtrans			*
 * H. Zeng/EAI  11/00   changed for the new undo design                 *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI  12/00   modified for the undo new design                *
 * M. Li/SAIC	06/02	Go directly to text placement after symbol place*
 * M. Li/SAIC	06/02	Specify group type and group number		*
 * J. Wu/SAIC	08/02	allow placing a symbol as a line's label	*
 ***********************************************************************/
{	
int	location, grpnum, np, class, obj, lab_type, ier;
float	xx[MAXPTS], yy[MAXPTS], latitude[MAXPTS], longitude[MAXPTS];
char    grptyp;
/*---------------------------------------------------------------------*/

    latitude[0] = _Latitude;
    longitude[0]= _Longitude;

    /*
     *  Get the label type to check if this symbol is placed as
     *  a label for a line or just a regular symbol.
     */
    lab_type = pglabel_getLabType();

    np =1;
    gtrans (sys_M, sys_D, &np, latitude, longitude, xx, yy, 
    	    &ier, strlen(sys_M), strlen(sys_D) );

    if ( lab_type == 1 ) {
	grptyp = (char)pgsymb_getGrptyp();    
        crg_ggnxt(grptyp, &grpnum, &ier);
    }
    else {
	pgnew_getGrpInfo(&grptyp, &grpnum);;
    }

    pgvgf_save(grptyp, grpnum, np, latitude, longitude,
			                &location, &ier );

    class = pgpalw_getCurClassId();
    obj   = pgpalw_getCurObjId();
    
    /*
     *  Save old information only for a regular symbol.
     */
    if (  _labelFlag && ( lab_type == 1 ) ) {
        pglabel_saveOldInfor(class, obj);
    }

    if (ier == 0) {
	pgundo_newStep();
	pgundo_storeThisLoc (location, UNDO_ADD, &ier);
        pgundo_endStep();
    }

    /*
     *  Label a regular symbol with text if required.
     */
    if ( _labelFlag && ( lab_type == 1 ) ) {
	pgnew_setGrpInfo(grptyp, grpnum);	
    	pglabel_setLabelPending(TRUE);
	pglabel_txtpopup();    
    }

    
    /*
     *  If the symbol is used to label a line, pop up the line
     *  attribute window for next drawing. Should reset the label
     *  type to text for normal symbol drawing. The label type
     *  will be reset to symbol in pglabel_symbpopup().
     */    
    if ( lab_type == 2 ) {	       
	
	if ( !pggrpw_isUp() ) {
	    pghdlb_deselectAll();
  	}
	
	pgsymb_popdown();
	pglabel_popupOldWin();  
    	pglabel_setLabelPending(FALSE);
    	pglabel_setLabType(1);    
    }

    XtSetSensitive(_symb_loLaPlacW, FALSE);

}


/*=====================================================================*/

void pgsymb_setUnRedo ( Boolean un_redo )
/************************************************************************
 * pgsymb_setUnRedo							*
 *									*
 * sets the undo/redo button sensitivity  				*
 *									*
 * void pgsymb_setUnRedo (un_redo)					*
 *									*
 * Input parameters:							*
 *	un_redo		Boolean	undo/redo sensitivity flag		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	03/99							*
 * W. Li/EAI	03/99	"Undo" -> "Undo symbol"				*
 * H. Zeng/EAI  12/00   Removed managing of _symb_unRedoW               *
 * J. Wu/GSC	05/01	free XmString					*
 * K. Tyle/UAlbany 11/10 Increase dimension size of cc			*
 ***********************************************************************/
{
XmString	xmstr;
char		cc[24];
/*---------------------------------------------------------------------*/
    XtSetSensitive(_symb_unRedoW, (int)un_redo);
    sprintf (cc, "%s", "Undo Symbol");
    xmstr = XmStringCreateLocalized (cc);    
	XtVaSetValues(_symb_unRedoW,
	XmNlabelString, 		xmstr,
	NULL);
    XmStringFree ( xmstr );
    _symbolUnRedoFlag = FALSE;

}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_unRedoCb ( Widget w, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgsymb_unRedoCb                                                  	*
 *                                                                      *
 * Callback for color button widget					*
 *                                                                      *
 * void pgsymb_unRedoCb (w, clnt, cbs)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   w			Widget                  	Widget ID	*
 *   clnt        XtPointer               	color		*
 *   cbs         XtPointer	callback struct	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 * W. Li/EAI	03/99	"Redo" -> "Redo symbol"				*
 * H. Zeng/EAI  12/00   modified for undo redesign                      *
 * K. Tyle/UAlbany 11/10 Increase dimension size of cc			*
 ***********************************************************************/
{
int		class, obj;
XmString	xmstr;
char		*undo_redo[] = {"Undo Symbol", "Redo Symbol"};
char		cc[24];
/*---------------------------------------------------------------------*/

    class = pgpalw_getCurClassId();
    obj   = pgpalw_getCurObjId();

    if( !_symbolUnRedoFlag ) {
       pgundo_undo();
       pgpalw_setCurBtns (FUNC_UNDO, class, obj);

       sprintf (cc, "%s", undo_redo[1]);
       xmstr = XmStringCreateLocalized (cc);
       XtVaSetValues(_symb_unRedoW,
                XmNlabelString, 		xmstr,
                XmNwidth,                 	110,
	        XmNheight,                 	30,
	        NULL);
       XmStringFree(xmstr);
       XtSetSensitive(_symb_loLaPlacW, TRUE);

    }
    else {
       pgundo_redo();
       pgpalw_setCurBtns (FUNC_REDO, class, obj);

       sprintf (cc, "%s", undo_redo[0]);
       xmstr = XmStringCreateLocalized (cc);
       XtVaSetValues(_symb_unRedoW,
                XmNlabelString, 		xmstr,
                XmNwidth,                 	110,
	        XmNheight,                 	30,
	        NULL);
       XmStringFree(xmstr);
       XtSetSensitive(_symb_loLaPlacW, FALSE);

    }

    _symbolUnRedoFlag = (char)!_symbolUnRedoFlag;


}

/*=====================================================================*/
/* ARGSUSED */
void pgsymb_grpTypCb ( Widget w,  long which, XtPointer call )
/************************************************************************
 * pgsymb_grpTypCb							*
 *									*
 * Callback function for group type option menu.			*
 *									*
 * void pgsymb_grpTypCb (w, which, data )				*
 *									*
 * Input parameters:							*
 *	w	Widget		option button widget ID			*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          04/02   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _curGrpIdx = (int)which;
    _groupTyp = ces_gtgmsid (_curGrpIdx);     
    pgsymb_setLabItems(_groupTyp);   

}

/*=====================================================================*/

void pgsymb_initType ( VG_DBStruct *el )
/************************************************************************
 * pgsymb_initType							*
 *									*
 * This function initiates group type menu option based on grptyp field *
 * of el.                                                               *
 *									*
 * void pgsymb_initType ( el )				                *
 *									*
 * Input parameters:							*
 *	el	VG_DBStruct*	       pointer to vg element		*
 *									*
 * Output parameters:						 	*
 *		NONE						        *
 * Return parameters:							*
 *		NONE						        *
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		05/99						*
 * E. Safford/GSC	06/99	set OBJ_CNTR to group type label	*
 * E. Safford/GSC	06/99	set all symbols to group type label	*
 * D.W.Plummer/NCEP	01/01	added SPLN6/20/21 to get OUTLOOK	*
 * M. Li/GSC            01/01   added CLASS_PRODUCTS                    *
 * D.W.Plummer/NCEP     01/01   replaced hardwired grptyp specification *
 * M. Li/GSC		01/01	Other -> OUTLOOK for outlook		*
 * H. Zeng/EAI          02/01   changed para. for new setting table     *
 * H. Zeng/EAI          03/01   rewrote for new group type table        *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * H. Zeng/EAI          04/02   rewrote from pggrpw_initType()          *
 * M. Li/SAIC		05/02   use active group type if GROUP is active*
 * H. Zeng/XTRIA	03/03   checked layer default group type        *
 ***********************************************************************/
{
int	 cur_layer, def_grp, ier, type_id = 0;
Boolean  layer_flag;
/*---------------------------------------------------------------------*/

    /*
     * Check if there is a default group for the current layer.
     */
    cur_layer  = pglayer_getCurLayer();
    def_grp    = pglayer_getDefGrp(cur_layer);
    layer_flag = ces_getflag(_subTyp, el, &ier);

    /*
     * Initialize the group type based on a preference order.
     */ 
    if ( pgpalw_isGrpActv() ) {

    	/*
     	 * If currently GROUP is active, use active group type.
     	 */
        _groupTyp  = pggrpw_getGrpType ();
	_curGrpIdx = ces_gtgavid ((char)_groupTyp);	
    }
    else if ( pgpalw_isLayerActv() &&
              def_grp != NON_GRPID && 
              ier == 0             && 
              layer_flag              ) { 

             _groupTyp  = def_grp;
             _curGrpIdx = ces_gtgavid ((char)_groupTyp);
    }
    else {

    	type_id   = (int)el->hdr.grptyp;

    	if(type_id != NON_GRPID) {
           _groupTyp = type_id;
           _curGrpIdx = ces_gtgavid ((char)_groupTyp);
    	}
    	else {
           _curGrpIdx = 0;
           _groupTyp  = ces_gtgmsid (_curGrpIdx);
	}
    }


    XtVaSetValues (_group_typeW,
		XmNmenuHistory,		_group_buttonW[_curGrpIdx], 
		NULL);

}

/*=====================================================================*/

void pgsymb_updtGrpMenu ( int index )
/************************************************************************
 * pgsymb_updtGrpMenu                                                   *
 *                                                                      *
 * This function updates the group type menu based on the input group   *
 * type index.                                                          *
 *                                                                      *
 * void pgsymb_updtGrpMenu ( index )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      index   int             group type index                        *
 *                                                                      *
 * Output parameters:                                                   *
 *              NONE                                                    *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           05/02                                           *
 ***********************************************************************/
{

    XtVaSetValues (_group_typeW,
                XmNmenuHistory,         _group_buttonW[index],
                NULL);
    pgsymb_grpTypCb (NULL, index, NULL);
}

/*=====================================================================*/

/* ARGSUSED */
void pgsymb_menuTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgsymb_menuTextCb                                                    *
 *                                                                      *
 * Callback function for Volcano list menu buttons.                     *
 *                                                                      *
 * void pgsymb_menuTextCb (wid, which, call)                            *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long             which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/02                                           *
 * H. Zeng/XTRIA        10/02    modified for NxmVolcano.c              *
 ***********************************************************************/
{
    float   lat, lon;
    char    name[33];
/*---------------------------------------------------------------------*/

    _volcIdx = (int)which - 1;
  	   
    switch (which) {

           case 0:
		XtVaSetValues (_volcTxtW, 
                               XmNvalue,                   "\0", 
                               XmNeditable,                TRUE,
                               XmNcursorPositionVisible,   TRUE,
                               NULL );

	        XmTextSetString(_symb_longTxtW, "\0"); 
	        XmTextSetString(_symb_latiTxtW, "\0");
	        XtSetSensitive(_symb_loLaPlacW, FALSE);

                break;

           default:

                NxmVolcano_getInfo( _volcIdx, &lat, &lon, name );
                XtVaSetValues (_volcTxtW, 
                               XmNvalue,                   name, 
                               XmNeditable,                FALSE,
                               XmNcursorPositionVisible,   FALSE,
                               NULL );

    	        if (_subTyp == VOLCANO) {
	            pgsymb_setLatLon(lat, lon);
	            XtSetSensitive(_symb_loLaPlacW, TRUE);
	        }
                
                break;

    }

}

/*=====================================================================*/

void pgsymb_unmanageLabForm ( void )
/************************************************************************
 * pgsymb_unmangeLabForm						*
 *									*
 * This function unmanages unmanage the label form to disable labeling  *
 * a symbol with text.							*
 *									*
 * void pgsymb_unmangeLabForm ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          07/02   initial coding                          *
 ***********************************************************************/
{
    if ( XtIsManaged(_label_formW) ) {
        XtUnmanageChild ( _label_formW );
    }
}

/*=====================================================================*/

void pgsymb_getSize ( float *size )
/************************************************************************
 * pgsymb_getSize							*
 *									*
 * This function gets symbol's size. 					*
 *									*
 * pgsymb_getSize ( size )						*
 *									*
 * Input parameters:							*
 *	NONE								*
 *									*
 * Output parameters:							*
 * 	*size		float		Symbol size			*
 * 									*
 * Return value:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	08/02		initail coding			*
 ***********************************************************************/
{
	*size = _symbSize; 
}

/*=====================================================================*/

void pgsymb_getWidth ( int *width )
/************************************************************************
 * pgsymb_getWidth							*
 *									*
 * This function gets symbol's width. 					*
 *									*
 * pgsymb_getWidth( width )						*
 *									*
 * Input parameters:							*
 *	NONE								*
 *									*
 * Output parameters:							*
 * 	*width		int		Symbol width			*
 * 									*
 * Return value:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	08/02		initial coding			*
 ***********************************************************************/
{
	*width = _symbWidth; 
}

/*=====================================================================*/

#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"

#define  ICON_DIR       "$NAWIPS/icons/nmap"
#define  BOX_DIR        "$NAWIPS/icons/box"
#define  MCLOUD_TBL	"mcloud.tbl"
#define  MAX_DIR_SCALE	360
#define	 NUMB_BTS	7

#define	 MAXMCLDITEM	15

#define  BOX            0
#define  SIZE           1
#define  FONT           2
#define  STYLE          3
#define  ALIGN          4
#define  TURB           5
#define	 ICNG		6
#define  TEXT           7
#define  COLOR          8
#define  ROTN           9

static Widget		_txtAttribW,	_textW;
static Widget		_txtRowCol;

static Widget           _editMenuRc;
static WidgetList       _editMenuForms;

static Widget		_fontMenu,	_turbMenu,	_sizeMenu; 
static Widget		_styleMenu,	_boxMenu,	_alignMenu;
static Widget		_icngMenu;

static Widget		_colrForm,	_colrW;
static Widget		_rotnSldW,	_rotnTxtW,	_rotnRowColW;
static Widget      	_rotnTyp;

static Widget		_colrLbl;
static Widget		_rotnLbl;
static WidgetList  	_rotnTypBtn;

static WidgetList	_fontWid,	_alignWid,	_sizeWid;
static WidgetList	_styleWid,	_turbWid,	_boxWid;
static WidgetList	_icngWid;

static Widget		_ctlForm;
static WidgetList	_ctlBtns;

static Widget		_textEdit;
static Widget		_colrEdit;
static Widget		_rotnEdit;

static WidgetList 	_attrbEdit;	/* box, size, font, style, just, turb, icng */
static WidgetList	_labName;	/* box, size, font, style, just, turb, icng */

static Widget		_mcldTypeForm;	 /* midlevel cloud type */
static Widget		_mcldAmountForm; /* midlevel cloud amount */
static Widget		_mcldTstormForm; /* midlevel t'storm type/level */
static Widget		_mcldTurbForm;   /* midlevel turb type/level */
static Widget		_mcldIcngForm;  /* midlevel icing type/level */

static Widget		_mcldTurbMenu;
static Widget		_mcldIcngMenu;
static WidgetList	_mcldTurbWid;
static WidgetList	_mcldIcngWid;

static Widget		_mcldTurbLvlTxtW;
static Widget		_mcldIcngLvlTxtW;
static Widget		_mcldTstrmLvlTxtW;

static struct incInfo   _incMcldType[MAXMCLDITEM];    /* midlevel cloud type */
static struct incInfo   _incMcldAmount[MAXMCLDITEM];  /* midlevel cloud code */
static struct incInfo   _incMcldTstorm[MAXMCLDITEM];  /* midlevel T'storm type */

static int   		_numMcldType;    /* number of midlevel cloud types */
static int   		_numMcldAmount;  /* number of midlevel cloud codes */
static int   		_numMcldTstorm;  /* number of midlevel T'storm types */

static int		_mcldTurbSym	= 4;
static int		_mcldIcngSym	= 5;

static char		_mcldTurbLvlTxt[32] = "";
static char		_mcldIcngLvlTxt[32] = "";
static char		_mcldTstrmLvlTxt[32] = "";

static int		_txtFont	= 0;
static int		_txtTurb	= 0;
static int		_txtIcng	= 0;

static int		_txtStyle	= 0;
static int		_txtColor	= 31;	
static float		_txtFontSz	= 1.0F;
static int		_txtAlign	= 0;	/* -1, 0, 1 */
static int		_txtWidth	= 1;
static int		_txtBox		= 0;

static int		_vgType		= SPTX_ELM;
static int		_sptxType	= 0;
static Boolean		_cesFlag;
static Boolean		_ghostFlag	= FALSE;
static Boolean		_initFlag	= FALSE;

static Boolean		_editFlags[10];	
static Boolean		_isLabel        = FALSE;

static int		_rotnSetShown;
static int		_rotnType	= 1;    /* 1= screen; 2= north */

static int		_currObjId	= OBJ_TEXTGEN;
static int		_currTxtGen	= 0;	/* general text type */
static int		_currTxtFzl	= 6;	/* freez level text type */
static int		_currTxtTurb	= 9;	/* turbulence text type */
static int		_currTxtCld	= 8;	/* cloud top text type */
static int		_currTxtIcng	= 12;   /* icing text type    */
static int		_currTxtMcld	= 15;   /* midlevel cloud text */

void			(*_updateGhostText) (VG_DBStruct *el);
XtCallbackProc		_exitCb;

/*
 *	Private Functions -- midlevel cloud GUI creation
 */
Widget  pgtxt_crtCheckBox ( Widget parent, char *labelstr, int row_spc, 
			   int opt_spc, int ncol, int nopt,
			   struct incInfo *incstruct,
			   XtCallbackProc callback);

Widget pgtxt_crtOptMenu ( Widget parent, char *labelstr, char *dir,
			   char  **itemicon, char **itemtext, 
			   int nitems, Widget *menuw, WidgetList menuitem,
			   char *levelstr, Widget *leveltext, int toff,
			   XtCallbackProc menucallback, 
			   XtCallbackProc lvlcallback );

/*
 *	Private Functions -- query
 */
int     pgtxt_getTxtType ( void );
Boolean pgtxt_isThisLabel ( void );
void	pgtxt_getMcldTxt ( char *text );

/*
 *	Private Callback functions
 */
void pgtxt_alignCb	( Widget, XtPointer, XtPointer );
void pgtxt_attrbEditCb	( Widget, XtPointer, XtPointer );
void pgtxt_boxCb	( Widget, XtPointer, XtPointer );
void pgtxt_fontCb	( Widget, XtPointer, XtPointer );
void pgtxt_rotnSldCb	( Widget, XtPointer, XtPointer );
void pgtxt_rotnTxtCb	( Widget, XtPointer, XtPointer );
void pgtxt_rotntypeCb	( Widget, XtPointer, XtPointer );
void pgtxt_sizeCb	( Widget, XtPointer, XtPointer );
void pgtxt_styleCb	( Widget, XtPointer, XtPointer );
void pgtxt_textCb	( Widget, XtPointer, XtPointer );
void pgtxt_turbSymCb	( Widget, XtPointer, XtPointer );
void pgtxt_turbHiLoCb	( Widget, XtPointer, XtPointer );
void pgtxt_icngSymCb	( Widget, XtPointer, XtPointer );
void pgtxt_mcldTypeCb	( Widget, long, XtPointer );
void pgtxt_mcldAmountCb ( Widget, long, XtPointer );
void pgtxt_mcldTstormCb ( Widget, long, XtPointer );
void pgtxt_mcldTurbSymCb ( Widget, XtPointer, XtPointer );
void pgtxt_mcldIcngSymCb ( Widget, XtPointer, XtPointer );
void pgtxt_mcldTurbLvlCb ( Widget, XtPointer, XtPointer );
void pgtxt_mcldIcngLvlCb ( Widget, XtPointer, XtPointer );
void pgtxt_mcldTstrmLvlCb( Widget, XtPointer, XtPointer );
static void pgtxt_vrfyTxtInput ( Widget, XtPointer, XtPointer );

/*
 *	Private Intialization functions
 */
void pgtxt_initAttrEdit ( Boolean edit );
void pgtxt_loadMcloudTbl ( int *nmctype, int *nmccode, int *ntstype );
void pgtxt_initMcld ( txt_attrib text_a ); 
void pgtxt_clearMcldSel ( void );


/************************************************************************
 * pgtxt.c								*
 *									*
 * This file contains the subroutines necessary to build and operate	*
 * the text edit popup to edit the attributes for a specific type of 	*
 * text or special text for product generation.				*
 *									*
 * CONTENTS:								*
 *	pgtxt_create		- creates the text edit window		*
 *	pgtxt_popup		- manages the text edit window		*
 *      pgtxt_editPopup		- start text window for attrib editing	*
 *	pgtxt_popdown		- unmanages the text edit window	*
 *	pgtxt_setGhostFlag	- sets the text ghosting flag       	*
 *	pgtxt_crtCheckBox	- creates check boxes for MCLOUD input	*
 *	pgtxt_pgtxt_crtOptMenu	- creates option menu for MCLOUD input	*
 *									*
 *	pgtxt_getAttr		- returns the current attribute values	*
 *	pgtxt_getTxtW		- returns the text input widget ID	*
 *	pgtxt_setRotn		- sets the attribute values       	*
 *	pgtxt_getRotnType	- get rotation type			*
 *	pgtxt_isUp		- return status of text edit window	*
 *	pgtxt_setEditFlag	- sets the text attribute edit flag	*
 *	pgtxt_getEditFlag	- gets the text attribute edit flag	*
 *	pgtxt_decodeFont	-decode the vgf font value		*
 *    	pgtxt_resetFocus	-redirect keyboard focus to main window	*
 *    	pgtxt_setLabelValue	-sets the label value into text filed	*
 *	pgtxt_getMcldTxt	- gets the MCLOUD text string		*
 *	pgtxt_loadMcloudTbl	- loads "mcloud.tbl" 			*
 *	pgtxt_clearMcldSel	- clears MCLOUD edit window		*
 *									*
 *	pgtxt_updateGstTxt	- sets the new box for ghosting       	*
 *									*
 *	pgtxt_getTxtType	- returns the current text type		*
 *	pgtxt_fillElement	- fills a VG_DBStruct element		*
 *									*
 *	pgtxt_rotnSldCb		- callback for rotation slider		*
 *	pgtxt_rotnTxtCb		- callback for rotation text widget	*
 *	pgtxt_turbSymCb		- callback for turbulence option button	*
 *	pgtxt_turbHiLoCb	- callback for turb. high/low button	*
 *	pgtxt_icngSymCb		- callback for icing option button	*
 *	pgtxt_alignCb		- callback for alignment option button	*
 *	pgtxt_sizeCb		- callback for the size option button	*
 *	pgtxt_styleCb		- callback for the style option button	*
 *	pgtxt_boxCb		- callback for the box option button	*
 *	pgtxt_fontCb		- callback for the font option button	*
 *	pgtxt_textCb		- callback for the text window		*
 *	pgtxt_rotntypeCb	- callback for the rotation type	*
 *	pgtxt_attrbEditCb	_ callback for the attribute edit 	*
 *	pgtxt_mcldTypeCb	_ callback for midlevel cloud type 	*	
 *	pgtxt_mcldAmountCb	_ callback for midlevel cloud amount	*	
 *	pgtxt_mcldTstormCb	_ callback for midlevel t'storm	type	*	
 *	pgtxt_mcldTurbSymCb	_ callback for midlevel turb type	*	
 *	pgtxt_mcldIcngSymCb	_ callback for midlevel icing type	*	
 *	pgtxt_mcldTurbLvlCb	_ callback for midlevel turb level	*	
 *	pgtxt_mcldIcngLvlCb	_ callback for midlevel icing level	*	
 *	pgtxt_mcldTstrmLvlCb	_ callback for midlevel t'storm	level	*	
 *									*
 *	pgtxt_initFonts		- initialization for font attribute	*
 *	pgtxt_initAlign		- initialization for alignment attribute*
 *	pgtxt_initRotn		- initialization for rotation attribute	*
 *	pgtxt_initSize		- initialization for font size attrib.	*
 *	pgtxt_initStyle		- initialization for font style attrib.	*
 *	pgtxt_initBox		- initialization for box attribute	*
 *	pgtxt_initText		- initialization for text		*
 *	pgtxt_initColor		- initialization for color attribute	*
 *	pgtxt_initAttrEdit	- initialization for text edit window	*
 *	pgtxt_initMcld		- initialization for MCLOUD edit window	*
 *	pgtxt_clearMcldSel	- clears MCLOUD edit window		*
 *	pgtxt_vrfyTxtInput 	- verify text input			*
 **									*
 * Log:									*
 * E. Safford/GSC	10/97	Initial coding.  Moved generic text	*
 *				 edit functions from NxmPlaceText.c,	*
 *				 renamed them, and cleaned them up	*
 * E. Safford/GSC	10/97	Moved color palette calls to NxmClrW.c	*
 * W. Li/EAI	08/98	Added rotation type				*
 * W. Li/EAI	11/98	moved number editor to nmap_pgnumb.c		*
 * W. Li/EAI	12/98	Added text multi_attributes edit		*
 * W. Li/EAI	01/99	Moved and renamed NxmTxtA.c  --> nmap_pgtxt.c	*
 *			NxmTxtA_XXXX --> pgtxt_XXXX			*
 * W. Li/EAI    01/99   Moved BOX,.., ROTN from Nxm.h --> nmap_pgedit   *
 * W. Li/EAI	02/99	Added label value and fixed toggle button prob.	*
 ***********************************************************************/


/*=====================================================================*/

void pgtxt_create ( Widget parent_w )
/************************************************************************
 * pgtxt_create								*
 *									*
 * This function creates the text dialog box for the Edit Text function	*
 *									*
 * void pgtxt_create (parent_w)						*
 *									*
 * Input parameters:							*
 *	parent_w	Widget	Parent widget				*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	 3/97	Initial coding				*
 * E. Safford/GSC 	 5/97	Added multi-line functionality		*
 * E. Safford/GSC 	 6/97	Added 2 more color palettes,		*
 *				software text, sliders on		*
 *				size & rotation.			*
 * E. Safford/GSC 	 7/97	Added turbulence widgets		*
 * E. Safford/GSC 	 8/97	Removed unused widgets			*
 * S. Wang/GSC     	 9/97	replace colrw_*() functions		*
 *				with NxmColrP_*()			*
 * E. Safford/GSC 	 9/97	Removed grP param			*
 * E. Safford/GSC	10/97	Renamed and cleaned up			*
 * E. Safford/GSC	10/97	Replaced linear menu builds with	*
 *				widget arrays and looping builds	*
 * E. Safford/GSC	10/97	Fixed memory and style errors		*
 * E. Safford/GSC	12/97	Added apply/cancel buttons for editing	*
 * S. Law/GSC		03/98	Changed turbulence label to pixmaps	*
 * S. Law/GSC		05/98	Added box pulldown menu			*
 * E. Safford/GSC	06/98	changed box buttons to bitmaps        	*
 * C. Lin/EAI		08/98	modified to use ctb_fszXXX        	*
 * C. Lin/EAI		08/98	use _txtFont for init setup (AWC)  	*
 * W. Li/EAI		08/98	Added rotation type			*
 * W. Li/EAI		10/98	Changed  rotation increment 10 -> 5	*
 * W. Li/EAI		11/98	Added increment/decrement		*
 * W. Li/EAI		11/98	moved number editor to nmap_pgnumb.c	*
 * W. Li/EAI		12/98	Added text multi_attributes edit	*
 * W. Li/EAI		01/98	Added Filled under line			*
 * H. Zeng/EAI          02/00   Did minor changes to appearance         *
 * E. Safford/GSC	01/01	use Nmap resource file for window loc   *
 * H. Zeng/EAI          01/01   modified resource for toggle buttons    *
 * M. Li/SAIC		10/01	Added icing object			*
 * E. Safford/SAIC	11/01	correct icing icon names                *
 * T. Piper/SAIC	11/01	freed menu_forms, menu_rowcol, 		*
 *					edit_menuForms, edit_forms	*
 * M. Li/SAIC		12/01	Changed TURB path			*
 * T. Piper/SAIC	 4/02	Added XmNmaxLength to text widget	*
 * J. Wu/SAIC           05/02   verify input to attribute "rotation"	*
 * T. Lee/SAIC		 8/02	added overline and filled overline	*
 * T. Piper/SAIC	12/02	radio box -> check Box			*
 * H. Zeng/XTRIA        12/02   removed edit_menu_rc and edit_menuForms *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget      form,		pane, 	  	slid_radiForm;
    Widget      menu_bar_fonts, menu_bar_align, menu_bar_size;
    Widget      menu_bar_turb,  menu_bar_style, menu_bar_box;
    Widget	menu_bar_icng;
    Widget	rotn_editForm, 	rotn_txtForm;
    Widget	slid_radiRC;
    WidgetList	menu_forms, menu_rowcol, edit_forms;
    
    Arg	   	args[10];
    int    	iret, jj, nfnt;
    int    	ncol, nrow, nrow1, toff_1, toff_2;
    XmString  	null_string, title_string;
    long	ii, ignore, nn;
    Pixel	fg, bg;
    Pixmap	turb_pxm, box_pxm, icng_pxm;
    char	filename[256], warning[200], fsznam[20];

    char	*align[] = {"    Left    ", "   Center  ", "    Right  "};
    char	*style[] = {"Regular", "Italic", "Bold", "B_Italic"};

    char	*rotn_type[] = {"screen", "north"}; 
    char        *box_xbm[] = {"box00.xbm", "box01.xbm", "box02.xbm", 
			      "box03.xbm", "box04.xbm", "box05.xbm",
			      "box06.xbm", "box07.xbm", "box08.xbm",
			      "box09.xbm"};
    /* box_msg count must match box_xbm count */
    char	*box_msg[] = {"None", "MinTrop", "MaxTrop", 
			      "Bounded", "B-Filled", "Filled",
			      "U_lined", "Filled_U_lined",
			      "O_lined", "Filled_O_lined"};
			
    char	*fonts[] = {"Courier", "Helvetica ", "Times", 
				"soft ware", "SOFT W."};
    char	*turb_xbm[]  = {"turb00.xbm", "turb01.xbm", "turb02.xbm", 
				"turb03.xbm", "turb04.xbm", "turb46.xbm", 
				"turb05.xbm", "turb06.xbm", "turb67.xbm", 
				"turb07.xbm", "turb08.xbm"};
    char        *icng_xbm[]  = {"ice00.xbm", "ice01.xbm", "ice02.xbm",
				"ice03.xbm", "ice04.xbm", "ice05.xbm",
				"ice06.xbm", "ice07.xbm", "ice08.xbm"};
 
    /* turb_msg count must match turb_xbm count */
    char	*turb_msg[]  = {"  0  ", "  1  ", "  2  ", "  3  ", "  4  ", 
				" 4/6 ", "  5  ", "  6  ", " 6/7 ", 
				"  7  ", "  8  "};
    char	*icng_msg[]  = {"  0  ", "  1  ", "  2  ", "  3  ", "  4  ",
                                "  5  ", "  6  ", "  7  ", "  8  "};

    char        *btnstr[] = {"Apply", "Cancel"};
    char	*lab_name[] = {"Box:",  "Size:", "Font:", "Style:",
			       "Just:", "Turb:", "Icng:"};

/*---------------------------------------------------------------------*/

    /*
     * read font size table
     */
    ctb_fszrd(&iret);

    _txtAttribW = XmCreateFormDialog(parent_w, "text_edit", NULL, 0);

    null_string = XmStringCreateLocalized ("");

    title_string = XmStringCreateLocalized ("Text Attributes");

    XtVaSetValues(_txtAttribW, 
		  XmNnoResize,          True, 
		  XmNdefaultPosition,	False,
		  XmNdialogTitle,	title_string,
		  XmNautoUnmanage,	False, 	
		  NULL);
    XmStringFree (title_string);

    form    =  XtVaCreateWidget ("form",
		xmFormWidgetClass,          	_txtAttribW,
	  	NULL);	

    pane    =  XtVaCreateWidget ("pane",
		xmPanedWindowWidgetClass,   	form,
		XmNsashWidth,	 	    	1,
		XmNsashHeight,		    	1,	
		NULL);	
    /*
     * creat a rowcol for text filed input
     */

    _txtRowCol =  XtVaCreateWidget ("_txtRowCol", 
  		xmRowColumnWidgetClass,	    	pane, 
		XmNpacking,		    	XmPACK_TIGHT,
		XmNnumColumns,		    	3, 
		XmNorientation,		    	XmHORIZONTAL,
		XmNisAligned,		    	TRUE,
		NULL);

    _textEdit = XtVaCreateManagedWidget (" ",
		xmToggleButtonGadgetClass,	_txtRowCol,
                XmNtraversalOn,                 False,
		NULL);

    XtAddCallback(_textEdit, XmNvalueChangedCallback,
                  (XtCallbackProc)pgtxt_attrbEditCb, NULL);
    XtVaCreateManagedWidget("Text",
		    xmLabelGadgetClass,	_txtRowCol,
		    NULL);

    nn = 0;
    XtSetArg(args[nn], XmNrows,                         2); nn++;
    XtSetArg(args[nn], XmNcolumns,                     18); nn++;
    XtSetArg(args[nn], XmNcursorPositionVisible,     True); nn++;
    XtSetArg(args[nn], XmNresizeHeight,              True); nn++;
    XtSetArg(args[nn], XmNeditable,                  True); nn++;
    XtSetArg(args[nn], XmNeditMode,     XmMULTI_LINE_EDIT); nn++; 
    XtSetArg(args[nn], XmNmaxLength,		 MAX_TEXT); nn++;

    _textW   =  XmCreateScrolledText(_txtRowCol, "Text", args, nn);
    XtAddCallback(_textW, XmNvalueChangedCallback, 
                  (XtCallbackProc)pgtxt_textCb, NULL);

    XtManageChild(_textW);
 

    /******* Beginning of midlevel cloud input area ****************/
 
    /*
     * load the midlevel cloud setting table
     */        
    pgtxt_loadMcloudTbl ( &_numMcldType, 
                          &_numMcldAmount,
			  &_numMcldTstorm );
        
    /*
     * creat a form for selecting midlevel cloud types
     */            
    _mcldTypeForm = pgtxt_crtCheckBox ( pane, 
		"Cloud Type:", 0, 10, 4, _numMcldType,
		_incMcldType, (XtCallbackProc)pgtxt_mcldTypeCb );
 	
    /*
     * creat a form for selecting midlevel cloud amount
     */                
    _mcldAmountForm = pgtxt_crtCheckBox ( pane, 
		"Cloud Amount:", 0, 1, 4, _numMcldAmount,
		_incMcldAmount, (XtCallbackProc)pgtxt_mcldAmountCb );
                
    /*
     * creat a form for inputting midlevel turbulance type & level
     */        
    jj = XtNumber ( turb_xbm );    
    _mcldTurbWid = (WidgetList) XtMalloc( jj * sizeof(Widget));
    _mcldTurbForm = pgtxt_crtOptMenu ( pane, "Turb Type:", ICON_DIR, 
		turb_xbm, turb_msg, (int)jj, &_mcldTurbMenu, _mcldTurbWid,
		"Top/Base:", &_mcldTurbLvlTxtW, 50,
		pgtxt_mcldTurbSymCb, pgtxt_mcldTurbLvlCb );

    /*
     * creat a form for inputting midlevel icing type & level
     */        
    jj = XtNumber ( icng_xbm );        
    _mcldIcngWid = (WidgetList) XtMalloc( jj * sizeof(Widget));
    _mcldIcngForm = pgtxt_crtOptMenu ( pane, "Icing Type:", ICON_DIR,
		icng_xbm, icng_msg, (int)jj, &_mcldIcngMenu, _mcldIcngWid,
		"Top/Base:", &_mcldIcngLvlTxtW, 50,
		pgtxt_mcldIcngSymCb, pgtxt_mcldIcngLvlCb );
    
    /*
     * creat a form for selecting midlevel thunderstorm type & level
     */        
    ncol = 4;
    toff_1 = 0;
    nrow1 = _numMcldTstorm / ncol;
    nrow = ( _numMcldTstorm % ncol ) ? ( nrow1 + 1 ): nrow1;
    toff_2 = 25 + toff_1 + nrow * 30;    
    _mcldTstormForm = pgtxt_crtCheckBox ( pane, 
		"Thunderstorm Type:", toff_1, 0, ncol, _numMcldTstorm,
		_incMcldTstorm, (XtCallbackProc)pgtxt_mcldTstormCb );
    pgtxt_crtOptMenu ( _mcldTstormForm, NULL, ICON_DIR, 
		turb_xbm, turb_msg, 0, NULL, NULL,
		"Top/Base:", &_mcldTstrmLvlTxtW, toff_2,
		 NULL, pgtxt_mcldTstrmLvlCb );

    /******* End of midlevel cloud input area ************************/

    /*
     * creat a rolcol for editing and menus
     */

    _editMenuRc =  XtVaCreateManagedWidget ("edit_menuRowCol", 
		xmRowColumnWidgetClass,	        pane, 
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     0,
		XmNpacking,		    	XmPACK_TIGHT,
		XmNnumColumns,			1, 
		XmNorientation,			XmVERTICAL,
		XmNisAligned,			TRUE,
		NULL);


    /*
     * creat "NUMB_BTS" forms for editing and menu forms
     * creat "NUMB_BTS" of forms for editing 
     * creat "NUMB_BTS" of forms for menu
     */

    nn = NUMB_BTS;

    _editMenuForms = (WidgetList) XtMalloc( nn * sizeof(Widget));
    edit_forms     = (WidgetList) XtMalloc( nn * sizeof(Widget));
    menu_forms     = (WidgetList) XtMalloc( nn * sizeof(Widget)); 
    menu_rowcol    = (WidgetList) XtMalloc( nn * sizeof(Widget)); 

    ii = 0;
    _editMenuForms[ii] = XtVaCreateManagedWidget ("edit_form",
		xmFormWidgetClass,          	_editMenuRc,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
		NULL);

    edit_forms[ii] = XtVaCreateManagedWidget ("edit_forms",
		xmFormWidgetClass,          	_editMenuForms[ii], 
		XmNleftAttachment,          	XmATTACH_FORM,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
	  	NULL); 

    menu_forms[ii] =  XtVaCreateManagedWidget ("menu_forms",
		xmFormWidgetClass,          	_editMenuForms[ii],
		XmNleftAttachment,          	XmATTACH_WIDGET,
		XmNleftWidget,			edit_forms[ii],
		XmNrightAttachment,	    	XmATTACH_FORM,
		XmNrightOffset,			5,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
	  	NULL);	

    for (ii=1; ii<nn; ii++){

	_editMenuForms[ii] = XtVaCreateManagedWidget ("edit_form",
		xmFormWidgetClass,          	_editMenuRc,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
		NULL);

 	edit_forms[ii] = XtVaCreateManagedWidget ("edit_forms",
		xmFormWidgetClass,          	_editMenuForms[ii], 
		XmNleftAttachment,          	XmATTACH_FORM,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
	  	NULL); 

	menu_forms[ii] =  XtVaCreateManagedWidget ("menu_forms",
		xmFormWidgetClass,          	_editMenuForms[ii],
		XmNleftAttachment,          	XmATTACH_WIDGET,
		XmNleftWidget,			edit_forms[ii],
		XmNrightAttachment,	    	XmATTACH_FORM,
		XmNrightOffset,			5,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
	  	NULL);	
    }

    /*
     * creat number (NUMB_BTS) toggle buttons on each editing 
     * form for editing selection
     */

    nn = NUMB_BTS;

    _attrbEdit = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){

	_attrbEdit[ii] = XtVaCreateManagedWidget (" ",
		xmToggleButtonGadgetClass,	edit_forms[ii],
                XmNtraversalOn,                 False,
		XmNtopAttachment,          	XmATTACH_FORM,
		NULL);

        XtAddCallback(_attrbEdit[ii], XmNvalueChangedCallback,
                      (XtCallbackProc)pgtxt_attrbEditCb, (XtPointer)(ii));
    }

    XtFree((XtPointer)edit_forms);

    /*
     *  creat labels and RowColums for  Box, Size, Font, Style, 
     *  Just, Turb, and Icng  menu
     */    

    nn = XtNumber(lab_name);
    _labName = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){

	if ( ii == TURB ) {
	    _labName[ii]  =  XtVaCreateManagedWidget (lab_name[ii],
		xmLabelWidgetClass,		menu_forms[ii],
		XmNleftAttachment,      	XmATTACH_FORM,
		XmNleftOffset,          	10,
		XmNtopAttachment,      		XmATTACH_FORM,
		XmNtopOffset,          		5,
		NULL);
	}
	else {
	    _labName[ii]  =  XtVaCreateManagedWidget (lab_name[ii],
                xmLabelGadgetClass,             menu_forms[ii],
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  10,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNtopOffset,                   5,
                NULL);
	}

	menu_rowcol[ii] = XtVaCreateManagedWidget ("menu_rowcol",
                xmRowColumnWidgetClass,     	menu_forms[ii], 
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0, 
                XmNpacking,                 	XmPACK_COLUMN,
                XmNnumColumns,              	1,
                XmNorientation,             	XmHORIZONTAL,
                XmNisAligned,               	TRUE,
                XmNentryAlignment,          	XmALIGNMENT_END,
		XmNleftAttachment,	    	XmATTACH_FORM,
		XmNleftOffset,              	70,
		XmNrightAttachment,	    	XmATTACH_FORM,
		XmNrightOffset,             	5,
	  	NULL);  
    }


    /*
     *  Box Menu
     */


    menu_bar_box  = XmCreatePulldownMenu (menu_rowcol[BOX],"Box",NULL,0);
    _boxMenu = XmCreateOptionMenu (menu_rowcol[BOX], "boxMenu", NULL, 0);
    XtVaSetValues (_boxMenu,
		XmNlabelString, 		null_string,	
		NULL);

    jj = XtNumber(box_xbm);
    _boxWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    XtVaGetValues (pane,
                XmNforeground, 			&fg,
                XmNbackground, 			&bg,
                NULL);

    for (ii=0; ii<jj; ii++) {
	cfl_inqr(box_xbm[ii], BOX_DIR, &ignore, filename, &iret );

        box_pxm = XmGetPixmap(XtScreen(parent_w),
                                  filename, fg, bg );

        if ( box_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
            sprintf( warning, "cannot load pixmap file %s",
                    filename );
            NxmWarn_show(parent_w, warning);
            _boxWid[ii] = XtVaCreateManagedWidget
                (box_msg[ii],
                 xmPushButtonWidgetClass,       menu_bar_box,
                 NULL);
        }
        else {
            _boxWid[ii] = XtVaCreateManagedWidget
                (box_msg[ii],
                 xmPushButtonWidgetClass,       menu_bar_box,
                 XmNlabelType,                  XmPIXMAP,
                 XmNlabelPixmap,                box_pxm,
                 NULL);
        }


        XtAddCallback (_boxWid[ii], XmNactivateCallback,
		       (XtCallbackProc)pgtxt_boxCb, (XtPointer)ii);
    }

    XtVaSetValues (_boxMenu,
		   XmNsubMenuId,	menu_bar_box,
		   XmNmenuHistory,	_boxWid[0], 
		   NULL);
    XtManageChild(_boxMenu);


    /*
     *  Size Menu
     */
 
    menu_bar_size = XmCreatePulldownMenu(menu_rowcol[SIZE],"Size",NULL,0);
    _sizeMenu = XmCreateOptionMenu(menu_rowcol[SIZE],"sizeMenu", NULL, 0);
    XtVaSetValues (_sizeMenu,
		XmNlabelString,		    null_string,
		NULL);

    ctb_fszqn( &nfnt, &iret);
    _sizeWid  = (WidgetList)XtMalloc((size_t)nfnt * sizeof(Widget));

    for (ii=0; ii<nfnt; ii++) {
	ctb_fsznam((int)ii, fsznam, &iret);
        _sizeWid[ii] = XtVaCreateManagedWidget(fsznam,
                xmPushButtonWidgetClass,    menu_bar_size,
		NULL);
        XtAddCallback(_sizeWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtxt_sizeCb, (XtPointer)ii );
    }

    XtVaSetValues (_sizeMenu, 
		XmNsubMenuId, 		    menu_bar_size,
         	XmNmenuHistory, 	    _sizeWid[2], 
		NULL);
    XtManageChild(_sizeMenu);


    /*
     *  Font Menu
     */


    menu_bar_fonts = XmCreatePulldownMenu(menu_rowcol[FONT],"Fonts",NULL,0);
    _fontMenu = XmCreateOptionMenu (menu_rowcol[FONT], "fontMenu", NULL, 0);
    XtVaSetValues (_fontMenu,
		XmNlabelString,		    null_string,	
		NULL);

    jj = XtNumber(fonts);
    _fontWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        title_string = XmStringCreateLocalized (fonts[ii]);
        _fontWid[ii] = XtVaCreateManagedWidget(fonts[ii],
                xmPushButtonWidgetClass,    menu_bar_fonts,
		XmNlabelString, 	    title_string, 
		NULL);
        XmStringFree (title_string);
        XtAddCallback(_fontWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtxt_fontCb, (XtPointer)ii );
    }

    XtVaSetValues (_fontMenu, 
		XmNsubMenuId, 		    menu_bar_fonts,
         	XmNmenuHistory, 	    _fontWid[_txtFont], 
		NULL);
    XtManageChild(_fontMenu);


    /*
     *  Style Menu
     */

    menu_bar_style = XmCreatePulldownMenu(menu_rowcol[STYLE],"Style",NULL, 0);
    _styleMenu = XmCreateOptionMenu(menu_rowcol[STYLE], "styleMenu", NULL, 0);
    XtVaSetValues (_styleMenu,
		XmNlabelString,		    null_string,	
		NULL);

    jj = XtNumber(style);
    _styleWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        title_string = XmStringCreateLocalized (style[ii]);
        _styleWid[ii] = XtVaCreateManagedWidget(style[ii],
                xmPushButtonWidgetClass,    menu_bar_style,
		XmNlabelString, 	    title_string, 
		NULL);
        XmStringFree (title_string);
        XtAddCallback(_styleWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtxt_styleCb, (XtPointer)ii);
    }

    XtVaSetValues (_styleMenu, 
		XmNsubMenuId, 		    menu_bar_style,
         	XmNmenuHistory, 	    _styleWid[0], 
		NULL);
    XtManageChild(_styleMenu);


    /*
     *  Alignment Menu
     */

    menu_bar_align = XmCreatePulldownMenu(menu_rowcol[ALIGN],"Align",NULL, 0);
    _alignMenu = XmCreateOptionMenu(menu_rowcol[ALIGN], "alignMenu", NULL, 0);
    XtVaSetValues (_alignMenu,
		XmNlabelString,		    null_string,
		NULL);

    jj = XtNumber(align);
    _alignWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        title_string = XmStringCreateLocalized (align[ii]);
        _alignWid[ii] = XtVaCreateManagedWidget(align[ii],
                xmPushButtonWidgetClass,    	menu_bar_align,
		XmNlabelString,             	title_string, 
		NULL);
        XmStringFree (title_string);
        XtAddCallback(_alignWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtxt_alignCb, (XtPointer)(ii - 1));
    }

    XtVaSetValues (_alignMenu, 
		XmNsubMenuId, 		    	menu_bar_align,
         	XmNmenuHistory, 	    	_alignWid[1], 
		NULL);
    XtManageChild(_alignMenu);

    /*
     *  Turbulence Menu
     */

    title_string = XmStringCreateLocalized ("H_turb");
    XtUnmanageChild (_labName[TURB]); 
    _labName[TURB] = 
	XtVaCreateManagedWidget	("hilo",
		xmPushButtonWidgetClass,	menu_forms[TURB],	
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,              	5,
		XmNtopAttachment,      		XmATTACH_FORM,
		XmNtopOffset,          		5,
		XmNlabelString,          	title_string,
		NULL);
    XmStringFree (title_string);
    XtFree((XtPointer)menu_forms);
    XtAddCallback(_labName[TURB], XmNactivateCallback,
		  (XtCallbackProc)pgtxt_turbHiLoCb, (XtPointer) NULL);

    menu_bar_turb = XmCreatePulldownMenu(menu_rowcol[TURB],"Turb",NULL,0); 
    _turbMenu = XmCreateOptionMenu (menu_rowcol[TURB],"turbMenu",NULL, 0); 
    XtVaSetValues (_turbMenu,
		XmNlabelString,		    null_string,
		NULL);

    jj = XtNumber(turb_xbm);
    _turbWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
	cfl_inqr(turb_xbm[ii], ICON_DIR, &ignore, filename, &iret );

	turb_pxm = XmGetPixmap(XtScreen(parent_w),
				  filename, fg, bg );

	if ( turb_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
	    sprintf( warning, "cannot load pixmap file %s",
		    filename );
	    NxmWarn_show(parent_w, warning);
	    _turbWid[ii] = XtVaCreateManagedWidget
		(turb_msg[ii], 
		 xmPushButtonWidgetClass,	menu_bar_turb,
		 NULL);
	}
	else {
	    _turbWid[ii] = XtVaCreateManagedWidget
		(turb_msg[ii],
		 xmPushButtonWidgetClass,	menu_bar_turb,
		 XmNlabelType,			XmPIXMAP,
		 XmNlabelPixmap,		turb_pxm,
		 NULL);
	}
        XtAddCallback(_turbWid[ii], XmNactivateCallback,
		      (XtCallbackProc)pgtxt_turbSymCb, (XtPointer) ii);
    }

    XtVaSetValues (_turbMenu, 
		XmNsubMenuId, 		    menu_bar_turb,
         	XmNmenuHistory, 	    _turbWid[0], 
		NULL);
    XtManageChild(_turbMenu);
    
     
    /*
     * Icing Widget
     */
    menu_bar_icng = XmCreatePulldownMenu(menu_rowcol[ICNG],"Icng",NULL,0);
    _icngMenu = XmCreateOptionMenu (menu_rowcol[ICNG],"icngMenu",NULL, 0);
    XtVaSetValues (_icngMenu,
                XmNlabelString,             null_string,
                NULL);
    XtFree((XtPointer)menu_rowcol);
    jj = XtNumber(icng_xbm);
    _icngWid  = (WidgetList)XtMalloc(jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
        cfl_inqr(icng_xbm[ii], ICON_DIR, &ignore, filename, &iret );

        icng_pxm = XmGetPixmap(XtScreen(parent_w),
                                  filename, fg, bg );

        if ( icng_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
            sprintf( warning, "cannot load pixmap file %s",
                    filename );
            NxmWarn_show(parent_w, warning);
            _icngWid[ii] = XtVaCreateManagedWidget
                (icng_msg[ii],
                 xmPushButtonWidgetClass,       menu_bar_icng,
                 NULL);
        }
        else {
            _icngWid[ii] = XtVaCreateManagedWidget
                (icng_msg[ii],
                 xmPushButtonWidgetClass,       menu_bar_icng,
                 XmNlabelType,                  XmPIXMAP,
                 XmNlabelPixmap,                icng_pxm,
                 NULL);
        }
        XtAddCallback(_icngWid[ii], XmNactivateCallback,
                      (XtCallbackProc)pgtxt_icngSymCb, (XtPointer) ii);
    }

    XtVaSetValues (_icngMenu,
                XmNsubMenuId,               menu_bar_icng,
                XmNmenuHistory,             _icngWid[0],
                NULL);

    XtManageChild (_icngMenu);


    /*
     *  Color Widget
     */
    _colrForm = XtVaCreateManagedWidget ("_colrForm",
                xmFormWidgetClass,          pane,
		NULL);

    _colrEdit = XtVaCreateManagedWidget (" ",
		xmToggleButtonGadgetClass,  _colrForm,
                XmNtraversalOn,             False,
		XmNleftAttachment,	    XmATTACH_FORM,
		XmNleftOffset,		    3,
		NULL);
    XtAddCallback(_colrEdit, XmNvalueChangedCallback,
                  (XtCallbackProc)pgtxt_attrbEditCb, NULL);

    XtAddCallback(_colrEdit, XmNvalueChangedCallback,
                  (XtCallbackProc)pgtxt_attrbEditCb, NULL);
    _colrLbl =  XtVaCreateManagedWidget ("Color:",
		xmLabelGadgetClass,	    _colrForm,
		XmNleftAttachment,	    XmATTACH_FORM,
		XmNleftOffset,		    70,
		XmNtopAttachment,	    XmATTACH_FORM,
		XmNtopOffset,		    3,
		NULL);


    _colrW   =  XtVaCreateManagedWidget ("        ",
		xmPushButtonWidgetClass,    _colrForm, 
		XmNbackground,		    NxmColrP_getColorPixel(_txtColor),
		XmNtopShadowColor,	    NxmColrP_getColorPixel(_txtColor),
		XmNbottomShadowColor,	    NxmColrP_getColorPixel(_txtColor),
  		XmNresize,		    FALSE,
		XmNleftAttachment,	    XmATTACH_FORM,
		XmNleftOffset,		    125,
		NULL);	
    XtAddCallback(_colrW, XmNactivateCallback, 
		  (XtCallbackProc)NxmClrW_popup, (XtPointer)(&_txtColor));

    /*
     *  Rotation widgets
     */

    _rotnRowColW = XtVaCreateManagedWidget("_rotnRowColW",
                xmRowColumnWidgetClass,		pane,
                XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			3,
		NULL);

    rotn_editForm = XtVaCreateManagedWidget ("rotn_editForm",
		xmFormWidgetClass,	    _rotnRowColW,
		NULL);

    _rotnEdit = XtVaCreateManagedWidget (" ",
		xmToggleButtonGadgetClass,	rotn_editForm,
                XmNtraversalOn,                 False,
		NULL);
    XtAddCallback(_rotnEdit, XmNvalueChangedCallback,
                  (XtCallbackProc)pgtxt_attrbEditCb, (XtPointer)ii);

    /*
     *    Rotation Slider
     */
    slid_radiForm = XtVaCreateManagedWidget ("slid_radiForm",
		xmFormWidgetClass,	    _rotnRowColW,
		NULL);
    slid_radiRC = XtVaCreateManagedWidget("slid_radiRC",
                xmRowColumnWidgetClass,		slid_radiForm,
                XmNorientation,			XmVERTICAL,
		XmNpacking,			XmPACK_TIGHT,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     0,
                NULL);

    _rotnLbl =  XtVaCreateManagedWidget ("Rotation:",
		xmLabelGadgetClass,	    slid_radiRC,
		NULL);

    title_string = XmStringCreateLocalized ("");
    _rotnSldW = XtVaCreateManagedWidget ("rotation",
		xmScaleWidgetClass,	    slid_radiRC,
		XmNorientation, 	    XmHORIZONTAL,
		XmNmaximum, 		    MAX_ROTN_SCALE,
		XmNminimum,		    0,
		XmNscaleMultiple,	    5,
		XmNshowValue, 		    False,
		XmNtitleString,		    title_string,
         	XmNwidth,                   125,
		NULL);
    XmStringFree (title_string);


    _rotnTyp = XtVaCreateManagedWidget("_rotnTyp",
                xmRowColumnWidgetClass,		slid_radiRC,
                XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			2,
		XmNradioBehavior,               False,
                NULL);


    nn = XtNumber(rotn_type);
    _rotnTypBtn = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
	_rotnTypBtn[ii] = XtVaCreateManagedWidget (rotn_type[ii],
			xmToggleButtonGadgetClass,	_rotnTyp,
                        XmNtraversalOn,                 False,
			NULL);
	XtAddCallback(_rotnTypBtn[ii], XmNvalueChangedCallback,
		      (XtCallbackProc)pgtxt_rotntypeCb, (XtPointer)ii );
	}

    XmToggleButtonGadgetSetState( _rotnTypBtn[0], TRUE, FALSE);
    XmToggleButtonGadgetSetState( _rotnTypBtn[1], FALSE, FALSE);

    rotn_txtForm  = (Widget)XtVaCreateManagedWidget( "rotn_txtForm",
                xmFormWidgetClass,              _rotnRowColW,
                NULL);
	
    _rotnTxtW = XtVaCreateManagedWidget ("_rotnTxtW",
		xmTextFieldWidgetClass,	    rotn_txtForm,
                XmNtopAttachment,           XmATTACH_FORM,
                XmNtopOffset,               12,
		XmNcolumns,		    3,
		XmNvalue, 		    "0",
		XmNcursorPositionVisible,   True,
		NULL);

    /*
     *  Apply & Cancel buttons
     */
    _ctlForm  = (Widget)XtVaCreateManagedWidget( "_txt_ctl_formW",
                xmFormWidgetClass,              pane,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNrightAttachment,             XmATTACH_FORM,
                NULL);

    _ctlBtns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));
    NxmCtlBtn_create (_ctlForm, 1, "ctlBtns", XtNumber(btnstr), btnstr,
                                                            NULL, _ctlBtns);


    XtAddCallback(_rotnSldW, XmNdragCallback,         pgtxt_rotnSldCb,  NULL);
    XtAddCallback(_rotnSldW, XmNvalueChangedCallback, pgtxt_rotnSldCb,  NULL);
    XtAddCallback(_rotnTxtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback(_rotnTxtW, XmNvalueChangedCallback, pgtxt_rotnTxtCb,  NULL);

    XtManageChild(_txtRowCol);
    XtManageChild(pane); 
    XtManageChild(form); 
    XmStringFree (null_string);
    
}

/*=====================================================================*/

void pgtxt_popup ( void )
/************************************************************************
 * pgtxt_popup                                                          *
 *                                                                      *
 * This function initiates the text edit popup.				*
 *                                                                      *
 * void  pgtxt_popup ( )                     	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        6/97	Initial coding				*
 * E. Safford/GSC        7/97	Reorganize and add starts for all 	*
 *				special text types			*
 * E. Safford/GSC       10/97   Moved to pgtxt_init.c, and modified to  *
 * 				call pgtxt_popup.			*
 * E. Safford/GSC       10/97   Modified for new nxmlib interface       *
 * W. Li/EAI		12/97	Enable color value reset		*
 * E. Safford/GSC       12/97   Modify default (center) alignment value *
 * C. Lin/EAI           02/98   rename,add auto activate text input     *
 * W. Li/EAI		04/98	Add OBJ_SPTEXTUD			*
 * S. Law/GSC		05/98	Added changes from drwids.h and updated	*
 *				call to NxmTxtA_popup			*
 * E. Safford/GSC	12/98	add resetFocus to text popup call	*
 * W. Li/EAI		12/98	Added attr_edit in NxmTxtA_setEditPopup	*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * E. Safford/GSC	06/99	reset _isLabel to false          	*
 * J. Wu/SAIC		08/01	add braces for union initilization 	*
 * M. Li/SAIC		10/01	added OBJ_TEXTICNG			*
 ***********************************************************************/
{
    int		obj;
    Widget	txtw, drawingw;

    txt_attrib	font  = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	align = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	rotn  = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	size  = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	color = {TRUE, TRUE,   TRUE, {31} };
    txt_attrib	turb  = {TRUE, FALSE,  TRUE, {0} };
    txt_attrib  icng  = {FALSE, FALSE,  TRUE, {0} };
    txt_attrib	style = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	box   = {TRUE, TRUE,   TRUE, {0} };
    txt_attrib	text  = {TRUE, TRUE,   FALSE, {0} };
/*---------------------------------------------------------------------*/

    obj = pgpalw_getCurObjId();
    
    switch(obj) {
      case OBJ_TEXTGEN:
	font.value._i = 3;
	break;
  
      case OBJ_TEXTFZL:
	font.value._i = 0;
	box.is_on = FALSE;
	box.value._i = 6;
	break;
 
      case OBJ_TEXTCLD:
	font.value._i = 0;
	box.is_on = FALSE;
	box.value._i = 8;
	break;
 
      case OBJ_TEXTTURB:
	font.value._i = 0;
	rotn.is_on = FALSE;
	rotn.reset_value = FALSE;
	rotn.value._i = 0;
	turb.is_on = TRUE;
	box.is_on = FALSE;
    	box.value._i = 7;		
	break;

      case OBJ_TEXTICNG:
        font.value._i = 0;
        rotn.is_on = FALSE;
        rotn.reset_value = FALSE;
        rotn.value._i = 0;
        box.is_on = FALSE;
        box.value._i = 12;
	turb.is_shown = FALSE;
	icng.is_shown = TRUE;
	icng.is_on = TRUE;
        break;

      case OBJ_TEXTMCLOUD:
        font.value._i = 0;
        rotn.is_on = FALSE;
        rotn.reset_value = FALSE;
        rotn.value._i = 0;
        box.is_on = FALSE;
        box.value._i = 15;
        break;

    }

    pgtxt_popupstart (1, SPTX_ELM, obj, font, align, rotn, size, 
		   color, turb, icng, style, box, text, FALSE, FALSE, 
		   (XtCallbackProc)NULL, (XtCallbackProc)pgtxt_resetFocus);

    /*
     * activate the text input except for midlevel cloud text
     */
    
    if ( obj != OBJ_TEXTMCLOUD ) {
        txtw     = (Widget)pgtxt_getTxtW();

        drawingw = (Widget)mcanvw_getDrawingW();

        XtSetKeyboardFocus(drawingw, txtw);
    }
    	
    _isLabel = FALSE;

}

/*=====================================================================*/

void pgtxt_popupstart (
int		use_ces,
int		vg_type,
int		obj_id,
txt_attrib	font_a,
txt_attrib	align_a,
txt_attrib	rotn_a,
txt_attrib	size_a,
txt_attrib	color_a,
txt_attrib	turb_a,
txt_attrib      icng_a,
txt_attrib	style_a,
txt_attrib	box_a,
txt_attrib	text_a,
int    		show_ctl,
Boolean		attr_edit,
XtCallbackProc	callback,
XtCallbackProc	exit_callback )
/************************************************************************
 * pgtxt_popupstart                                                     *
 *									*
 * This function initiates the place text popup and turns on the	*
 * color palettes for the desired type of text.				*
 *									*
 * The input values in txt_attrib setting for each text attribute will  *
 * be used to manage/unmanage, make sensitive/insensitive  and set the  *
 * initial value for each text attribute button.			*
 *									*
 * void pgtxt_popupstart (use_ces, vg_type, obj_id, font_a, align_a,	*
 *			  rotn_a, size_a, color_a, turb_a, icng_a, 	*
 *			  style_a, box_a, text_a, show_ctl, attr_edit, 	*
 *			  callback, exit_callback)			*
 *									*
 * Input Parameters:							*
 *	use_ces		int	flag telling whether to use ces		*
 *				 0 - use input values, _cesFlag = F	*
 *				 1 - use ces values,   _cesFlag = T	*
 *				 2 - use input values, _cesFlag = T	*
 *	vg_type		int	the vg_type for the current object	*
 *	obj_id		int	the the current object id		*
 *	font_a		txt_attrib	font attributes			*
 *					 .value._i valid range 0 .. 4	*
 *	align_a		txt_attrib	alignment attributes		*
 *					 .value._i valid range -1 .. 1 	*
 *	rotn_a		txt_attrib	rotation attributes		*
 *					 .value._i valid range 0 .. 360	*
 *	size_a		txt_attrib	font size attributes		*
 *					 .value._i valid range 0 .. 4	*
 *	color_a		txt_attrib	color attributes		*
 *	turb_a		txt_attrib	turbulence attributes		*
 *					 .value._i valid range 0 .. 9	*
 *	icng_a		txt_attrib      icing attributes           	*
 *                                       .value._i valid range 0 .. 12  *
 *	style_a		txt_attrib	style attributes		*
 *	box_a		txt_attrib	box attributes			*
 *	text_a		txt_attrib	text attributes			*
 *      show_ctl	int	show control buttons flag		*
 *	attr_edit	Boolean	multiple selection attribute editing	*
 *      callback	XtCallbackProc callback for Apply/Cancel btns	*
 *	exit_callback 	XtCallbackProc callback for window exit		*
 *									*
 * Output Parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	10/97	Initial coding - from NxmStartTextWin	*
 * E. Safford/GSC	12/97	add show_ctl flag & callback		*
 * S. Law/GSC		05/98	Added use_ces flag and box pulldown	*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * C. Lin/EAI		08/98	modify to use ctb_fszXXX		*
 * C. Lin/EAI		08/98	add assignment to _txtWidth (AWC)	*
 * W. Li/EAI		08/98	Added rotation type			*
 * E. Safford/GSC	12/98	added exit_callback			*
 * W. Li/EAI		01/99	added attr_edit for multi-select edit	*
 * M. Li/SAIC		11/01	Added TEXTICNG & pgtxt_initIcng		*
 * J. Wu/SAIC		03/03	add TEXTMCLOUD & pgtxt_initMcld		*
 * J. Wu/SAIC		03/03	pop up proper window for multi-editing	*
 * J. Wu/SAIC		08/03	switch between TURB/ICNG properly	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    int		iret, multi_sel, mixed_sel;
    long	ii;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
    pgtxt_popdown();
    _currObjId = obj_id;

    if (use_ces == 1) {
	_sptxType = pgtxt_getTxtType ();

	box_a.value._i = _sptxType;

	el.hdr.vg_class = CLASS_TEXT;
	_vgType = SPTX_ELM;
	el.hdr.vg_type  = (char)_vgType;
	el.elem.spt.info.sptxtyp = _sptxType;

	ces_get (_sptxType, &el, &iret);

	strcpy(text_a.value._c, el.elem.spt.text);
	font_a.value._i = el.elem.spt.info.itxfn % 10;
	font_a.value._i = (el.elem.spt.info.ithw == SOFTWARE) ?
	    font_a.value._i + 2 : font_a.value._i - 1;
	align_a.value._i = el.elem.spt.info.ialign;

	if (el.elem.spt.info.rotn >= 1000.0F ){
	    _rotnType = 2;
	    XmToggleButtonGadgetSetState( _rotnTypBtn[0], FALSE, FALSE);
	    XmToggleButtonGadgetSetState( _rotnTypBtn[1], TRUE, FALSE);
	}
	else {
	    _rotnType = 1;
	    XmToggleButtonGadgetSetState( _rotnTypBtn[0], TRUE, FALSE);
	    XmToggleButtonGadgetSetState( _rotnTypBtn[1], FALSE, FALSE);
	}
	rotn_a.value._i = (int)(el.elem.spt.info.rotn);

	ii = (int)(el.elem.spt.info.sztext*100.0F);
	ctb_fszfnd(el.elem.spt.info.sztext, &(size_a.value._i), &iret);

	color_a.value._i = el.hdr.maj_col;
	if (obj_id == OBJ_TEXTTURB) {
	    turb_a.value._i  = el.elem.spt.info.turbsym;
	}
	else if (obj_id == OBJ_TEXTICNG) {
	    icng_a.value._i  = el.elem.spt.info.turbsym;
	}
	style_a.value._i = el.elem.spt.info.itxfn / 10;
	_txtWidth	 = el.elem.spt.info.iwidth;
    }
    else {
	_vgType = vg_type;
	_sptxType = box_a.value._i;
    }

    _cesFlag = (Boolean)((use_ces == 0) ? FALSE : TRUE);
    

    /* 
     * initFonts must remain before initRotn 
     */

    _initFlag = TRUE;
    pgtxt_initFonts (font_a);
    pgtxt_initAlign (align_a);
    pgtxt_initRotn  (rotn_a);
    pgtxt_initSize  (size_a);
    pgtxt_initColor (color_a);
    pgtxt_initTurb  (turb_a);
    pgtxt_initIcng  (icng_a);
    pgtxt_initStyle (style_a);
    pgtxt_initBox   (box_a);
    pgtxt_initText (text_a);   
    
    if (show_ctl) {
	XtManageChild (_ctlForm);
	if (callback) {
	    for (ii=0; ii<2; ii++) {
		XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
		XtAddCallback (_ctlBtns[ii], XmNactivateCallback,
						callback, (XtPointer)ii);
	    }
	}
    }
    else
	XtUnmanageChild (_ctlForm);

    pgtxt_initAttrEdit(attr_edit);

    /*
     *  Manage/Unmanage proper widgets for midlevel cloud input
     */
    XtUnmanageChild ( _editMenuRc );
    
    if ( _currObjId == OBJ_TEXTMCLOUD ) {        
	XtUnmanageChild ( _txtRowCol );
        XtUnmanageChild ( _editMenuForms [BOX] );
        XtUnmanageChild ( _editMenuForms [TURB] );
        XtUnmanageChild ( _editMenuForms [ICNG] );
	XtUnmanageChild ( _rotnRowColW );       

	XtManageChild ( _mcldTypeForm );   
        XtManageChild ( _mcldAmountForm );   
        XtManageChild ( _mcldTurbForm );   
        XtManageChild ( _mcldIcngForm );   
        XtManageChild ( _mcldTstormForm );   
        
	pgtxt_initMcld ( text_a );   
    }
    else {
        XtManageChild ( _txtRowCol );
	XtManageChild ( _editMenuForms [BOX] );
        
	if ( _currObjId == OBJ_TEXTICNG ) {
	    XtManageChild ( _editMenuForms [ICNG] );    
	}
	else {
	    XtManageChild ( _editMenuForms [TURB] );    	
	}
	
	XtManageChild ( _rotnRowColW );               
	
	XtUnmanageChild ( _mcldTypeForm );   
        XtUnmanageChild ( _mcldAmountForm );   
        XtUnmanageChild ( _mcldTurbForm );   
        XtUnmanageChild ( _mcldIcngForm );   
        XtUnmanageChild ( _mcldTstormForm );   
    }
    
    /*
     *  Manage/Unmanage proper widgets when multi-selecting
     *  text elements for editting.
     */
    if ( pgpalw_getCurOperId() == FUNC_MULTISEL &&
         pgpalw_getCurClassId() == CLASS_TEXT ) {
        pgtxt_getSelMcloud ( &multi_sel, &mixed_sel );        
        if ( multi_sel || mixed_sel) {
	    XtUnmanageChild ( _mcldTypeForm );   
            XtUnmanageChild ( _mcldAmountForm );   
            XtUnmanageChild ( _mcldTurbForm );   
            XtUnmanageChild ( _mcldIcngForm );   
            XtUnmanageChild ( _mcldTstormForm );   	
	}
	
	if ( mixed_sel ) {	    
	    XtUnmanageChild ( _txtRowCol );
            XtUnmanageChild ( _editMenuForms [BOX] );
            XtUnmanageChild ( _editMenuForms [TURB] );
	    XtUnmanageChild ( _rotnRowColW );       	
	}
    }    
    
    XtManageChild ( _editMenuRc );
    
    XtManageChild  (_txtAttribW);

    _initFlag = FALSE;
    pgtxt_updateGstTxt ();

    if (exit_callback) {
        _exitCb = exit_callback;
    }
    
}

/*=====================================================================*/

void pgtxt_editPopup ( VG_DBStruct *elmnt, Boolean attrb_edit, 
					XtCallbackProc callback )
/************************************************************************
 * pgtxt_editPopup                                                      *
 *                                                                      *
 * This function initiates the text edit popup, and popup the window.	*
 *                                                                      *
 * void pgtxt_editPopup (elmnt, attrb_edit, callback)			*
 *                                                                      *
 * Input parameters:                                                    *
 *   *elmnt		VG_DBStruct	text element to be edited	*
 *   attrb_edit		Boolean						*
 *   callback		XtCallbackProc	callback function for ctl btns  *
 *									*
 * Output parameters:							*
 *   none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       12/97	Initial coding copied pgtxt_init	*
 * C. Lin/EAI           02/98	rename and auto activate text input	*
 * S. Law/GSC		05/98	Updated call to NxmTxtA_popup		*
 * C. Lin/EAI           08/98	modified to use ctb_fszXXX		*
 * E. Safford/GSC	12/98	add resetFocus to text popup call	*
 * W. Li/EAI		12/98	Added attr_edit in NxmTxtA_setEditPopup	*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * J. Wu/SAIC		08/01	add braces for union initilization 	*
 * M. Li/SAIC           10/01   added OBJ_TEXTICNG                      *
 * J. Wu/SAIC		03/03	add OBJ_TEXTMCLOUD 			*
 * J. Wu/SAIC		03/03	set no rotation for OBJ_TEXTMCLOUD 	*
 ***********************************************************************/
{
    int		obj, font_style, font_type, subtype, iret;
    Widget	txtw, drawingw;
    txt_attrib	font  = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	align = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	rotn  = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	size  = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	color = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	turb  = {TRUE, FALSE, FALSE, {0} };
    txt_attrib  icng  = {FALSE, FALSE, FALSE, {0} };
    txt_attrib	style = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	box   = {TRUE, TRUE, TRUE, {0} };
    txt_attrib	text  = {TRUE, TRUE, TRUE, {0} };
/*---------------------------------------------------------------------*/

    color.value._i = elmnt->hdr.maj_col;

    switch(elmnt->hdr.vg_type) {

      case TEXTC_ELM:
      case TEXT_ELM:
	pgtxt_decodeFont (elmnt->elem.txt.info.itxfn, elmnt->elem.txt.info.ithw,
			  &font_type, &font_style);

	font.value._i  = font_type; 
	align.value._i = elmnt->elem.txt.info.ialign - 2;
	rotn.value._i  = (int)elmnt->elem.txt.info.rotn;

	ctb_fszfnd(elmnt->elem.txt.info.sztext, &(size.value._i), &iret);

	style.value._i = font_style; 
	sprintf (text.value._c, "%s", elmnt->elem.txt.text);

	if (elmnt->elem.txt.info.ithw == HARDWARE)
	    rotn.is_on = rotn.reset_value = FALSE;

	obj = OBJ_TEXTGEN;
	box.is_on = FALSE;
	break;

      case SPTX_ELM:
	pgtxt_decodeFont (elmnt->elem.spt.info.itxfn, elmnt->elem.spt.info.ithw,
			  &font_type, &font_style);

	font.value._i  = font_type;
	style.value._i = font_style; 

	align.value._i = elmnt->elem.spt.info.ialign;
	rotn.value._i  = (int)elmnt->elem.spt.info.rotn;

	if (elmnt->elem.spt.info.ithw == HARDWARE)
	    rotn.is_on = rotn.reset_value = FALSE;

	ctb_fszfnd(elmnt->elem.spt.info.sztext, &(size.value._i), &iret);

	sprintf (text.value._c, "%s", elmnt->elem.spt.text);

	/* 
	 *  Activate turbulence pulldown if special text type of 7 or 9
	 */
	subtype = elmnt->elem.spt.info.sptxtyp;
	box.value._i = subtype;
	switch (subtype) {
	  case 6:
	    box.is_on = FALSE;
	    obj = OBJ_TEXTFZL;
	    break;
	  case 8:
	    box.is_on = FALSE;
	    obj = OBJ_TEXTCLD;
	    break;
	  case 7:
	  case 9:
	    turb.value._i = elmnt->elem.spt.info.turbsym;
	    turb.is_on = TRUE;
	    turb.reset_value = TRUE;
	    box.is_on = FALSE;
	    rotn.is_on = rotn.reset_value = FALSE;
	    obj = OBJ_TEXTTURB;
	    break;
          case 12:
            icng.value._i = elmnt->elem.spt.info.turbsym;
            icng.is_on = TRUE;
            icng.reset_value = TRUE;
	    icng.is_shown = TRUE;
	    turb.is_shown = FALSE;
            rotn.is_on = rotn.reset_value = FALSE;
            box.is_on = FALSE;
            obj = OBJ_TEXTICNG;
            break;
          case 15:  /* midlevel cloud text */
	    obj = OBJ_TEXTMCLOUD;
            rotn.is_on = rotn.reset_value = FALSE;
            break;
	  default:
	    obj = OBJ_TEXTGEN;
	    break;
	}

	break;
    }

    pgpalw_setCurBtns (-1, -1, obj);

    pgtxt_popupstart (2, (int)elmnt->hdr.vg_type, obj, font, align, rotn, size, 
		   color, turb, icng, style, box, text, TRUE, attrb_edit, (XtCallbackProc)callback,
		   (XtCallbackProc)pgtxt_resetFocus);

    /*
     * activate the text input for non-MCLOUD text objects
     */
    if ( elmnt->elem.spt.info.sptxtyp != 15 ) {
        txtw     = (Widget)pgtxt_getTxtW();
        drawingw = (Widget)mcanvw_getDrawingW();
        XtSetKeyboardFocus(drawingw, txtw);
    }
    
}

/*=====================================================================*/

void pgtxt_setGhostFlag ( Boolean flag, void (*update)(VG_DBStruct *) )
/************************************************************************
 * pgtxt_setGhostFlag							*
 *									*
 * This function sets the flag for ghosting.				*
 *									*
 * void pgtxt_setGhostFlag (flag, update)				*
 *									*
 * Input Parameters:							*
 *	flag		Boolean		To set or not to set		*
 *	*update()  	void	ghost update function (NULL if !flag)	*
 *									*
 * Output Parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC	07/98	Initial coding					*
 ***********************************************************************/
{
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (_ghostFlag) {
	el.hdr.vg_class = 0;
	_updateGhostText (&el);		/* using the function */
    }

    _ghostFlag = flag;

    if (_ghostFlag) {
	_updateGhostText = update;	/* setting the function */
    }
}

/*=====================================================================*/

void pgtxt_getAttr ( textattrib_t *attribs, char *text )
/************************************************************************
 * pgtxt_getAttr							*
 *									*
 * This routine returns the current value of the text field and		*
 * attributes.								*
 *									*
 * void pgtxt_getAttr  ( attribs, text )				*
 *									*
 * Input parameters:							*
 *		NONE							*
 *									*
 * Output parameters:							*
 *	*attribs	textattrib_t	text attribute structure	*
 *	*text		char		text string			*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	 9/97	Initial coding				*
 * E. Safford/GSC	10/97	renamed and cleaned up			*
 * C. Lin/EAI		03/98	allow bold in software fonts		*
 * W. Li/EAI		08/98	Added rotation type			*
 * M. Li/SAIC		11/01	Added _sptxType == 12			*
 * J. Wu/SAIC		04/02	set attribs->turb default as 0		*
 * J. Wu/SAIC		03/03	set attribs->frotn to 0 for MCLOUD	*
 ***********************************************************************/
{
    char        *crotn;
/*---------------------------------------------------------------------*/

    pgtxt_getTextValue (TRUE, text);
  
    /*
     *  Get rotation
     */
    if (_sptxType == 7 || _sptxType == 9 || _txtFont < 3 ||
        _sptxType == 12 || _sptxType == 15 ) {
	attribs->frotn = 0.0F;
    }
    else {

	XtVaGetValues (_rotnTxtW, XmNvalue, &crotn, NULL);

	if (_rotnType == 1)
	    attribs -> frotn = (float)atof(crotn);
	if (_rotnType == 2) 
	    attribs -> frotn = (float)atof(crotn) + 1000.00F;
	XtFree (crotn);
    }

    /*
     *  convert font to the approprate GEMPAK value (font values of 3 & 4
     *  indicate a text item with a software font).  Valid gempak font values
     *  start at 1 instead of 0, hence the gemfont assignment with a +1 below.
     */
    if (_txtFont > 2) {
        attribs->gemfont   = _txtFont - 2 + (10 * _txtStyle);
	attribs->fontithw  = SOFTWARE;
    }
    else {
        attribs->gemfont   = _txtFont + 1 + (10 * _txtStyle);
	attribs->fontithw  = HARDWARE;
    }
 
    attribs->sptxtyp	= _sptxType;

    attribs->fsize	= _txtFontSz;
    attribs->colr	= _txtColor;
    
    if (_sptxType == 12) {
        attribs->turb	= _txtIcng;
    }
    else if (_sptxType == 7 || _sptxType == 9) {
	attribs->turb   = _txtTurb;
    }
    else {
	attribs->turb   = 0;    
    }

    attribs->fontstyle	= _txtStyle;
    attribs->align	= _txtAlign;
    attribs->iwidth	= _txtWidth;
 
}

/*=====================================================================*/

Widget pgtxt_getTxtW ( void )
/************************************************************************
 * pgtxt_getTxtW 							*
 *                                                                      *
 * This function returns the text input widget ID.			*
 *                                                                      *
 * Widget pgtxt_getTxtW (void)	 					*
 *                                                                      *
 * Input Parameters:							*
 *     None								*
 *									*
 * Output Parameters:							*
 * pgtxt_getTxtW	Widget		Text input widget ID		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI 		02/98						*
 ***********************************************************************/
{

	return (_textW);

}

/*=====================================================================*/

void pgtxt_setRotn ( int new_dir )
/************************************************************************
 * pgtxt_setRotn                                                      	*
 *                                                                      *
 * This function sets the rotation value.                               *
 *                                                                      *
 * void pgtxt_setRotn ( new_dir )                                     	*
 *                                                                      *
 * Input Parameters:                                                    *
 *	new_dir		int	new direction                           *
 *                                                                      *
 * Output Parameters:                                                   *
 *     None                                                             *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/98                                           *
 * W. Li/EAI		10/98	Added north relative 			*
 * W. Li/EAI		10/98	Changed rotation increment 10 -> 5	*
 ***********************************************************************/
{
int	round_val;
char	txtstr[5];
/*---------------------------------------------------------------------*/
    if ( new_dir >= 1000 )
	new_dir  = (new_dir - 1000)%360;

    if ( -1 * MAX_DIR_SCALE <= new_dir && 
				new_dir <=  MAX_DIR_SCALE) {
	round_val = ( (new_dir % 5) < 3) ? new_dir - (new_dir % 5) :
				           new_dir + (5 - new_dir % 5);

	sprintf (txtstr, "%i", round_val);
	XmTextSetString (_rotnTxtW, txtstr);
    }    
}
/*=====================================================================*/

void pgtxt_getRotnType ( int *rotatype )
/************************************************************************
 * pgtxt_getRotnType                                                  	*
 *                                                                      *
 * This function gets the rotation type value.                          *
 *                                                                      *
 * void pgtxt_getRotnType ( rotatype )                                	*
 *                                                                      *
 * Input Parameters:                                                    *
 *	*rotatype		int totation type 	                *
 *                                                                      *
 * Output Parameters:                                                   *
 *     None                                                             *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	08/98	Added rotation type				*
 ***********************************************************************/
{
    *rotatype = _rotnType;
}

/*=====================================================================*/

Boolean pgtxt_isUp ( void )
/************************************************************************
 * pgtxt_isUp                                                         	*
 *                                                                      *
 * This function returns the current status of the text attribute 	*
 * window.  								*
 *                                                                      *
 * Boolean pgtxt_isUp ( )		                                *
 *                                                                      *
 * Input Parameters:                                                    *
 * Output Parameters:                                                   *
 * pgtxt_isUp	Boolean		True if attribute window is active	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/98	initial coding				*
 * W. Li/EAI		01/99	NxmTxtA_isUp --> pgtxt_isUp		*
 ***********************************************************************/
{
    return ( XtIsManaged(_txtAttribW) );
}

/*=====================================================================*/

void pgtxt_updateGstTxt ( void )
/************************************************************************
 * pgtxt_updateGstTxt							*
 *									*
 * This function updates the box for ghosting.				*
 *									*
 * void pgtxt_updateGstTxt ()						*
 *									*
 * Input Parameters:							*
 * Output Parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC	07/98	Initial coding					*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 ***********************************************************************/
{
    VG_DBStruct el;
    int		np, ier;
    float	srx, sry;
/*---------------------------------------------------------------------*/

    np = 1;

    if (_ghostFlag && !_initFlag) {

	pgtxt_fillElement (_sptxType, &el);
	srx = sry = 0.0F;

	if (el.hdr.vg_type == SPTX_ELM) {
	    pgtxt_getTextValue (FALSE, el.elem.spt.text);
	    gtrans (sys_D, sys_M, &np, &srx, &sry, 
		    &el.elem.spt.info.lat, &el.elem.spt.info.lon, 
		    &ier, strlen(sys_D), strlen(sys_M));
	}
	else {
	    pgtxt_getTextValue (FALSE, el.elem.txt.text);
	    gtrans (sys_D, sys_M, &np, &srx, &sry, 
		    &el.elem.txt.info.lat, &el.elem.txt.info.lon, 
		    &ier, strlen(sys_D), strlen(sys_M));
	}
	_updateGhostText (&el);
 
    }
}
/*=====================================================================*/

int pgtxt_getTxtType ( void )
/************************************************************************
 * pgtxt_getTxtType							*
 *									*
 * This function returns the current text type.				*
 *									*
 * int pgtxt_getTxtType (void)						*
 *									*
 * Input Parameters:							*
 * Output Parameters:							*
 * pgtxt_getTxtType	int	current text type			*
 **									*
 * Log:									*
 * S. Law/GSC	02/98	Initial coding					*
 * M. Li/SAIC	11/01	Added OBJ_TEXTICNG				*
 * J. Wu/SAIC	02/03	add OBJ_TEXTMCLOUD				*
 ***********************************************************************/
{
    int type;
/*---------------------------------------------------------------------*/
    switch (_currObjId) {
      case OBJ_TEXTFZL:
	type = _currTxtFzl;
	break;
      case OBJ_TEXTTURB:
	type = _currTxtTurb;
	break;
      case OBJ_TEXTICNG:
        type = _currTxtIcng;
        break;
      case OBJ_TEXTCLD:
	type = _currTxtCld;
	break;
      case OBJ_TEXTMCLOUD:
	type = _currTxtMcld;
	break;
      default:
	type = _currTxtGen;
	break;
    }

    return (type);
}

/*=====================================================================*/

void pgtxt_fillElement ( int sptype, VG_DBStruct *el )
/************************************************************************
 * pgtxt_fillElement							*
 *									*
 * This function fills the VG_DBStruct.					*
 *									*
 * void pgtxt_fillElement (sptype, el)					*
 *									*
 * Input Parameters:							*
 *	sptype	int		current special type to use		*
 *									*
 * Output Parameters:							*
 *	*el	VG_DBStruct	structure to fill			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC	06/98	Moved from pgtxt_popdown			*
 * W. Li/EAI	07/98	Added txt_attrib's text value setting		*
 * C. Lin/EAI	08/98	(_txtWidth-2) -> _txtWidth			*
 * W. Li/EAI	08/98	Added rotation type				*
 * J. Wu/SAIC	04/02	set el->elem.spt.info.turbsym default as 0	*
 * J. Wu/SAIC	03/03	get text for midlevel cloud			*
 * J. Wu/SAIC	03/03	set rotation to 0 for midlevel cloud text	*
 ***********************************************************************/
{
    char	*crotn, *txt;
/*---------------------------------------------------------------------*/
	if (_vgType == SPTX_ELM) {
	    if (_txtFont > 2) {
		el->elem.spt.info.itxfn = _txtFont - 2 + (10 * _txtStyle);
		el->elem.spt.info.ithw  = SOFTWARE;
	    }
	    else {
		el->elem.spt.info.itxfn = _txtFont + 1 + (10 * _txtStyle);
		el->elem.spt.info.ithw  = HARDWARE;
	    }

	    el->hdr.vg_class = CLASS_TEXT;
	    el->hdr.vg_type  = (char)_vgType;
	    el->hdr.maj_col = _txtColor;
	    el->elem.spt.info.sptxtyp	= sptype;
	    el->elem.spt.info.ialign	= _txtAlign; 
	    el->elem.spt.info.iwidth	= _txtWidth; 
	    el->elem.spt.info.sztext	= _txtFontSz;

	    if (sptype == 12) {
	        el->elem.spt.info.turbsym = _txtIcng;
	    }
	    else if (sptype == 7 || sptype == 9) {
		el->elem.spt.info.turbsym = _txtTurb;
	    }
	    else {
		el->elem.spt.info.turbsym = 0;	    
	    }

	    el->elem.spt.info.offset_x	= 0;
	    el->elem.spt.info.offset_y	= 0;
            if ( _sptxType == 15 ) { /* midlevel cloud text */
                pgtxt_getMcldTxt ( el->elem.spt.text ); 
	    }
	    else {	    
	        XtVaGetValues (_textW, XmNvalue, &txt, NULL);
	        strcpy(el->elem.spt.text, txt);
	        XtFree (txt);
            }
	    
	    if (sptype == 7 || sptype == 9 || sptype == 12 ||
	        _txtFont < 3 || sptype == 15 ) {
		el->elem.spt.info.rotn = 0.0F;
	    }
	    else {
		XtVaGetValues (_rotnTxtW, XmNvalue, &crotn, NULL);
		if (_rotnType == 1)
	   	    el->elem.spt.info.rotn = (float)atof(crotn);
		if (_rotnType == 2) 
		    el->elem.spt.info.rotn = (float)atof(crotn) + 1000.00F;
		XtFree (crotn);
	    }
	}
	else {
	    if (_txtFont > 2) {
		el->elem.txt.info.itxfn = _txtFont - 2 + (10 * _txtStyle);
		el->elem.txt.info.ithw  = SOFTWARE;
	    }
	    else {
		el->elem.txt.info.itxfn = _txtFont + 1 + (10 * _txtStyle);
		el->elem.txt.info.ithw  = HARDWARE;
	    }

	    el->hdr.vg_class = CLASS_TEXT;
	    el->hdr.vg_type  = (char)_vgType;
	    el->hdr.maj_col = _txtColor;
	    el->elem.txt.info.ialign   = _txtAlign; 
	    el->elem.txt.info.iwidth   = _txtWidth; 
	    el->elem.txt.info.sztext   = _txtFontSz;
	    el->elem.txt.info.offset_x	= 0;
	    el->elem.txt.info.offset_y	= 0;

	    XtVaGetValues (_textW, XmNvalue, &txt, NULL);
	    strcpy(el->elem.txt.text, txt);
	    XtFree (txt);

	    XtVaGetValues (_rotnTxtW, XmNvalue, &crotn, NULL);
	    if (_rotnType == 1)
	        el->elem.txt.info.rotn = (float)atof(crotn);
	    if (_rotnType == 2)
	        el->elem.txt.info.rotn = (float)atof(crotn) + 1000.00F;
	    XtFree (crotn);
	}
}


/*=====================================================================*/

Boolean pgtxt_isThisLabel ( void )
/************************************************************************
 * pgtxt_isThisLabel                                                   	*
 *                                                                      *
 * This function returns the value of _isLabel, TRUE if the text box is *
 * up for a label operation, or FALSE if not.                           *
 *                                                                      *
 * Boolean pgtxt_isThisLabel ()			                      	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pgtxt_isThisLabel	Boolean	T - text box up                         *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/99   Initial  coding                  	*
 ***********************************************************************/
{
    return (_isLabel);
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_rotnSldCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_rotnSldCb                                                    	*
 *                                                                      *
 * This function is the call back for user draw actions on the rotation *
 * slider.                                                              *
 *                                                                      *
 * void pgtxt_rotnSldCb ( w, clnt, call )                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        6/97   Initial  coding                  	*
 * E. Safford/GSC       10/97   renamed  and cleaned up          	*
 * W. Li/EAI		10/98	Changed  rotation increment 10 -> 5	*
 ***********************************************************************/
{
    int round_val;
    char txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;

/*----------------------------------------------------------------------*/
    round_val = ((cbs->value % 5) < 3) ? cbs->value - (cbs->value%5) :
                                          cbs->value + (5 - cbs->value%5);

    sprintf(txtstr, "%i", round_val);
    XmTextSetString(_rotnTxtW, txtstr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_rotnTxtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_rotnTxtCb	                                                *
 *                                                                      *
 * This function is the call back for user modification to the rotation *
 * text field.                                                          *
 *                                                                      *
 * pgtxt_rotnTxtCb ( w, clnt, call )                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer       Button press event record       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        6/97   Initial coding                  	*
 * E. Safford/GSC	 9/97	Removed grP 				*
 * E. Safford/GSC	10/97	renamed and cleaned up			*
 * S. Law/GSC		07/98	Added call to _updateGstTxt		*
 * W. Li/EAI		01/99	NxmTxtA_rotnTxtCb --> pgtxt_rotnTxtCb	*
 ***********************************************************************/
{
    char  *s;
    int   slval, itxval;
/*---------------------------------------------------------------------*/
 
    XtVaGetValues (_rotnTxtW, XmNvalue, &s, NULL);
 
    /*  if the value on corresponding slider is different, set the sliders
     *  value accordingly.
     */
    XmScaleGetValue(_rotnSldW, &slval);
 
    itxval = atoi(s);

    if ( (itxval >= 0) && (itxval <= MAX_ROTN_SCALE) && (itxval != slval)) {
        XmScaleSetValue(_rotnSldW, itxval);
    }
 
    XtFree (s);

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_turbSymCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_turbSymCb	                                                *
 *                                                                      *
 * This function is the call back to handle the entered values in the   *
 * turbulence text widget.                                              *
 *                                                                      *
 * void pgtxt_turbSymCb ( w, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        7/97           Initial coding                  *
 * E. Safford/GSC	 8/97		Fixed iturb assignments for 2   *
 *					  symbols			*
 * E. Safford/GSC        9/97           Removed unused turb combinations*
 *                                        and added 4/6                 *
 * E. Safford/GSC       10/97           renamed and cleaned up          *
 * S. Law/GSC		07/98		Added call to _updateGstTxt	*
 ***********************************************************************/
{
    if ((long)clnt == 0)
        _txtTurb = 0;
    else if ((long)clnt == 1)
        _txtTurb = 1;
    else if ((long)clnt == 2)
        _txtTurb = 2;
    else if ((long)clnt == 3)
        _txtTurb = 3;
    else if ((long)clnt == 4)
        _txtTurb = 4;
    else if ((long)clnt == 5)
        _txtTurb = 56;
    else if ((long)clnt == 6)
        _txtTurb = 5;
    else if ((long)clnt == 7)
        _txtTurb = 6;
    else if ((long)clnt == 8)
        _txtTurb = 77;
    else if ((long)clnt == 9)
        _txtTurb = 7;
    else
        _txtTurb = 8;

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_turbHiLoCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_turbHiLoCb							*
 *									*
 * This function is the call back to the high/low turbulence button	*
 *									*
 * void pgtxt_turbHiLoCb ( w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	w		Widget		Parent widget			*
 *	clnt	XtPointer	State information record	*
 *	call	XtPointer					*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/98	Initial coding				*
 * S. Law/GSC		07/98	Added call to _updateGstTxt		*
 * W. Li/EAI		12/98	Changed _turbLbl -> _lblName[TURB]	*
 ***********************************************************************/
{
    XmString  	label;
/*---------------------------------------------------------------------*/

    XtUnmanageChild (_labName[TURB]); 
    if (_currTxtTurb == 7) {
	_currTxtTurb = 9;
	label = XmStringCreateLocalized ("H_turb");
    }

    else {
	_currTxtTurb = 7;
	label = XmStringCreateLocalized ("L_turb");
    }

    _sptxType = _currTxtTurb;
    XtVaSetValues (_labName[TURB], XmNlabelString, label, NULL);
    XmStringFree (label);

    XtManageChild (_labName[TURB]); 

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_icngSymCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_icngSymCb                                                      *
 *                                                                      *
 * This function is the call back to handle the entered values in the   *
 * icning text widget.                                              	*
 *                                                                      *
 * void pgtxt_icngSymCb ( w, clnt, call )                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call       XtPointer                                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		11/01		Initial coding                  *
 ***********************************************************************/
{
    _txtIcng = (long)clnt;

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_alignCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_alignCb		                                        *
 *                                                                      *
 * This function is the call back to update the align value.            *
 *                                                                      *
 * void pgtxt_alignCb ( w, clnt, call )	               	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        8/97           Initial coding                  *
 * E. Safford/GSC       10/97           renamed and cleaned up          *
 * S. Law/GSC		07/98		Added call to _updateGstTxt	*
 ***********************************************************************/
{
    _txtAlign = (long)clnt;

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_sizeCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_sizeCb	                                                	*
 *                                                                      *
 * This function is the call back for the size menu button.             *
 *                                                                      *
 * void pgtxt_sizeCb ( w, clnt, call )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        8/97           Initial coding                  *
 * E. Safford/GSC       10/97           renamed and cleaned up          *
 * S. Law/GSC		07/98		Added call to _updateGstTxt	*
 * C. Lin/EAI		08/98		rewrite to use ctb_fszXXX	*
 ***********************************************************************/
{
int which, iret;
/*---------------------------------------------------------------------*/

    which = (long)clnt;

    ctb_fszval(which, &_txtFontSz, &iret);

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_styleCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_styleCb	                                                *
 *                                                                      *
 * This function is the call back for the style button.                 *
 *                                                                      *
 * void pgtxt_styleCb ( w, clnt, call )                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        8/97           Initial coding                  *
 * E. Safford/GSC       10/97           renamed and cleaned up          *
 * S. Law/GSC		07/98		Added call to _updateGstTxt	*
 ***********************************************************************/
{
    _txtStyle = (long)clnt; 

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_boxCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_boxCb								*
 *									*
 * This function is the call back for the box button.			*
 *									*
 * void pgtxt_boxCb (w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	w		Widget		Parent widget			*
 *	clnt	XtPointer	State information record	*
 *	call	XtPointer					*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	Copied from NxmTxtA_styleCb		*
 * S. Law/GSC		06/98	Added calls to _fillElement,ces_set	*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * S. Law/GSC		07/98	Added call to _updateGstTxt		*
 * C. Lin/EAI		08/98	Modified to use ctb_fszXXX		*
 * W. Li/EAI		12/98	Added multiple selection attribute edit	*
 * W. Li/EAI		01/98	Added Filled under line			*
 * W. Li/EAI		01/99	Added Box Change edit			*
 * E. Safford/GSC	06/00	remove reset on _textW			*
 * E. Safford/GSC	06/00	condtionally remove reset on _textW	*
 * J. Wu/SAIC		08/01	remove embeded '\0' in format		*
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG			*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()                	*
 * T. Lee/SAIC		 8/02	add overline and filled overline	*
 * J. Wu/SAIC		03/03	set no rotation for OBJ_TEXTMCLOUD	*
 ***********************************************************************/
{
    int		ii, iret, font_test, old_type = _sptxType;
    char	degrees[4];
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
    _txtBox = (long)clnt; 

    switch(_txtBox){
	case 0:
	case 1:
	case 2:
	case 3: 
	case 4:
	case 5:
	    _sptxType = _txtBox;
	    break;
	case 6:
	    _sptxType = 10;  /* underline */
	    break;
	case 7:
	    _sptxType = 11;  /* Filled underline */
	    break;
	case 8:
	    _sptxType = 13;  /* overline */
	    break;
	case 9:
	    _sptxType = 14;  /* Filled overline */
	    break;
	default:
	    _sptxType =_currTxtGen; 
	    break;
    }

    _currTxtGen = _sptxType;

    if (old_type != _sptxType && _cesFlag == TRUE) {

	pgutls_initHdr ( &(el.hdr) );

	pgtxt_fillElement (old_type, &el);
	ces_set (old_type, &el, &iret);

	el.hdr.vg_class = CLASS_TEXT;
	_vgType = SPTX_ELM;
	el.hdr.vg_type  = (char)_vgType;
	el.elem.spt.info.sptxtyp = _sptxType;
	ces_get (_sptxType, &el, &iret);

	if (strlen(el.elem.spt.text) > (size_t)0) {
	    XmTextSetString (_textW, el.elem.spt.text);
	}

	if (!XtIsManaged(_textEdit)) {
	    _txtFont = el.elem.spt.info.itxfn % 10;
	    _txtFont = (el.elem.spt.info.ithw == SOFTWARE) ?
		_txtFont + 2 : _txtFont - 1;

	    XtVaSetValues (_fontMenu, 
			   XmNmenuHistory, 	_fontWid[_txtFont], 
			   NULL);
	}

	/*
	 * rotation is not sensitive if hardware font or OBJ_TEXTTURB/
	 * OBJ_TEXTICNG/OBJ_TEXTMCLOUD
	 */
	if ((_rotnSetShown) && (_currObjId != OBJ_TEXTTURB &&
				_currObjId != OBJ_TEXTICNG &&
				_currObjId != OBJ_TEXTMCLOUD ) ){
	    font_test = (_txtFont > 2);

	    if (!XtIsSensitive(_attrbEdit[BOX])){ /*  no in edit condition */ 
		XtSetSensitive (_rotnTxtW, font_test);
		XtSetSensitive (_rotnSldW, font_test);
		XtSetSensitive (_rotnTyp,  font_test);
                XtSetSensitive (_rotnLbl,  font_test);
	    }
	    else{ /* in edit condition */
		XtSetSensitive (_rotnEdit, font_test);
		XtSetSensitive (_rotnLbl,  font_test);
		if (font_test==0){
		    XmToggleButtonGadgetSetState(_rotnEdit,FALSE,FALSE);
		    XtSetSensitive (_rotnTxtW, font_test);
		    XtSetSensitive (_rotnSldW, font_test);
		    XtSetSensitive (_rotnTyp,  font_test);
                    XtSetSensitive (_rotnLbl,  font_test);
		}
	   }
	}

	if (!XtIsManaged(_textEdit)) {
	    if (font_test) {
	    ii = (int) el.elem.spt.info.rotn;
            if ((ii < 360) && (ii > 0)) {
        	sprintf (degrees, "%d", ii);
	    }
            else {
        	strcpy (degrees, "0");
	    }
            XmTextSetString(_rotnTxtW,  degrees);
	    }

	    _txtAlign = el.elem.spt.info.ialign;
	    XtVaSetValues (_alignMenu, XmNmenuHistory, 
			   _alignWid[_txtAlign + 1], NULL);

	    _txtFontSz = el.elem.spt.info.sztext;
	    ctb_fszfnd(_txtFontSz, &ii, &iret);
            XtVaSetValues (_sizeMenu, 
			   XmNmenuHistory, _sizeWid[ii], 
			   NULL);

  	    _txtColor = el.hdr.maj_col;

            XtVaSetValues (_colrW,
			   XmNbackground,	NxmColrP_getColorPixel(_txtColor),
			   XmNtopShadowColor,	NxmColrP_getColorPixel(_txtColor),
			   XmNbottomShadowColor,NxmColrP_getColorPixel(_txtColor),
			   NULL);	
  
	    _txtStyle = el.elem.spt.info.itxfn / 10;
            XtVaSetValues (_styleMenu, XmNmenuHistory, 
			   _styleWid[_txtStyle], NULL);
	}

    }

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_fontCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_fontCb	                                                	*
 *                                                                      *
 * This function is the call back to update the font value.             *
 *                                                                      *
 * void pgtxt_fontCb ( w, clnt, call )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        8/97   Initial coding                  	*
 * E. Safford/GSC       10/97   renamed and cleaned up          	*
 * S. Law/GSC		05/98	Added check for hardware font		*
 * S. Law/GSC		07/98	Added call to _updateGstTxt		*
 * W. Li/EAI		12/98	Added multiple selection attribute edit	*
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG			*
 * J. Wu/SAIC		03/03	set no rotation for OBJ_TEXTMCLOUD	*
 ***********************************************************************/
{
    int		font_test;
/*---------------------------------------------------------------------*/

    _txtFont = (long)clnt;

    /*
     * rotation is not sensitive if hardware font or OBJ_TEXTTURB/
     * OBJ_TEXTICNG/OBJ_TEXTMCLOUD  
     */
    if ((_rotnSetShown) && (_currObjId != OBJ_TEXTTURB &&
			    _currObjId != OBJ_TEXTICNG &&
			    _currObjId != OBJ_TEXTMCLOUD ) ) {
	font_test = (_txtFont > 2);

	if (!XtIsSensitive(_attrbEdit[FONT])){ /* no in edit condition */
	    XtSetSensitive (_rotnTxtW, font_test);
	    XtSetSensitive (_rotnSldW, font_test);
	    XtSetSensitive (_rotnTyp,  font_test);
            XtSetSensitive (_rotnLbl,  font_test);
	}
	else{ /* in edit condition */
	    XtSetSensitive (_rotnEdit, font_test);
	    XtSetSensitive (_rotnLbl, font_test);
	    if (font_test == 0){
		XmToggleButtonGadgetSetState(_rotnEdit,FALSE,FALSE);
		XtSetSensitive (_rotnTxtW, font_test);
		XtSetSensitive (_rotnSldW, font_test);
		XtSetSensitive (_rotnTyp,  font_test);
                XtSetSensitive (_rotnLbl,  font_test);
	    }
	}
    }

    pgtxt_updateGstTxt ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_textCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_textCb								*
 *									*
 * This function is the callback for the text window.  It currently	*
 * only checks for C-backspace events					*
 *									*
 * void pgtxt_textCb (wid, clnt, call)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget				*
 *	clnt	XtPointer	State information record	*
 *	call	XtPointer	Button press event record	*
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/98	Initial coding				*
 * S. Law/GSC		07/98	Added call to _updateGstTxt		*
 * S. Jacobs/NCEP       12/98   Fixed cast of NULL for LINUX            *
 ***********************************************************************/
{
    XmTextVerifyCallbackStruct *cbs;
    XKeyEvent *kevent;
    Modifiers mr;
    KeySym kr;
    Display *dsp;
/*---------------------------------------------------------------------*/
    cbs = (XmTextVerifyCallbackStruct *) call;
    kevent = (XKeyEvent *) cbs->event;

    if (kevent != (XKeyEvent *) NULL && kevent->type == KeyPress) {
	dsp = XtDisplay (_textW);
        XtTranslateKey (dsp, kevent->keycode, (Modifiers)NULL, &mr, &kr);
	if ((kr == (KeySym)XK_BackSpace) && (kevent->state & ControlMask)) {
	    XmTextSetString (_textW, "\0");
	}
    }

    pgtxt_updateGstTxt ();

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_rotntypeCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_rotntypeCb	                                                *
 *                                                                      *
 * This function is the callback for the rotation type radio box.	*
 *                                                                      *
 * void pgtxt_rotntypeCb ( w, clnt, call)	                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer	Button press event record	*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		08/98	Initial coding				*
 * T. Piper/SAIC	12/02	radio box -> check box			*
 ***********************************************************************/
{
int which;
/*---------------------------------------------------------------------*/

    which = (long)clnt;
    if (which == 0) {
	_rotnType = 1;
	XmToggleButtonGadgetSetState( _rotnTypBtn[1], FALSE, FALSE);
	XmToggleButtonGadgetSetState( _rotnTypBtn[0], TRUE, FALSE);
    }
    else if (which == 1) {
	_rotnType = 2;
	XmToggleButtonGadgetSetState( _rotnTypBtn[0], FALSE, FALSE);
	XmToggleButtonGadgetSetState( _rotnTypBtn[1], TRUE, FALSE);
    }
    pgtxt_updateGstTxt();
}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_attrbEditCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgtxt_attrbEditCb	                                                *
 *                                                                      *
 * This function is the call back for the attribute toggle button	*
 *                                                                      *
 * void  pgtxt_attrbEditCb( w, clnt, call)	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XmAnyCallbackStruct				*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	12/98	Initial coding					*
 * M. Li/SAIC	11/01	Added NUMB_BTS					*
 ***********************************************************************/
{
int	data;
Boolean btnval;
/*---------------------------------------------------------------------*/
    data = (long)clnt;

    /* 
     *check if the button is up or down and take appropriate actions
     */

    if (w == _textEdit){
	XtVaGetValues(_textEdit, XmNset, &btnval, NULL);
	pgtxt_setEditFlag(TEXT, (Boolean)(((btnval) ? 1 : 0)) );
        XtSetSensitive(_textW, (int)_editFlags[TEXT]);
    }
    else if (w == _colrEdit){
	XtVaGetValues(_colrEdit, XmNset, &btnval, NULL);
	pgtxt_setEditFlag(COLOR, (Boolean)(((btnval) ? 1 : 0)) );
        XtSetSensitive(_colrW, (int)_editFlags[COLOR]);
    }
    else if (w == _rotnEdit){ 
	XtVaGetValues(_rotnEdit, XmNset, &btnval, NULL);
	pgtxt_setEditFlag(ROTN, (Boolean)(((btnval) ? 1 : 0)) );
	XtSetSensitive(_rotnSldW, (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnTyp,  (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnTxtW, (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnLbl,  (int)_editFlags[ROTN]);
    }
    else {
	if ((data >= 0) && (data < NUMB_BTS)){

	    /* 
	     * set box, size, font, style, just, turb or icng edit button 
	     */

            XtVaGetValues(_attrbEdit[(data)], XmNset, &btnval, NULL);
	    pgtxt_setEditFlag(data, (Boolean)(((btnval) ? 1 : 0)) );
	}
	XtSetSensitive(_boxMenu,   (int)_editFlags[BOX]);
	XtSetSensitive(_sizeMenu,  (int)_editFlags[SIZE]);
	XtSetSensitive(_fontMenu,  (int)_editFlags[FONT]);
	XtSetSensitive(_styleMenu, (int)_editFlags[STYLE]);
	XtSetSensitive(_alignMenu, (int)_editFlags[ALIGN]);
	XtSetSensitive(_turbMenu,  (int)_editFlags[TURB]);
	XtSetSensitive(_icngMenu,  (int)_editFlags[ICNG]);
    }

}

/*=====================================================================*/

void pgtxt_initFonts ( txt_attrib font_a ) 
/************************************************************************
 * pgtxt_initFonts	                                                *
 *                                                                      *
 * This function sets the font options for the text edit window.        *
 *									*
 * void pgtxt_initFonts (font_a)            	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   font_a	txt_attrib     	font attributes				*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97	Initial coding                  	*
 * E. Safford/GSC       12/97   Minor cleanup			        *
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 ***********************************************************************/
{
    if (!(font_a.is_shown)) {
        XtUnmanageChild (_fontMenu);
	XtUnmanageChild (_labName[FONT]);
	return;
    }

    XtManageChild (_fontMenu);
    XtManageChild (_labName[FONT]);

    XtSetSensitive (_fontMenu, font_a.is_on);
    XtSetSensitive (_labName[FONT],  font_a.is_on); 
    XtSetSensitive (_attrbEdit[FONT], font_a.is_on);

    if (font_a.is_on) { 
	/*
	 *   Set font catagory and turn on menu options   
	 */
	XtSetSensitive (_fontWid[0], TRUE);
	XtSetSensitive (_fontWid[1], TRUE);
	XtSetSensitive (_fontWid[2], TRUE);
	XtSetSensitive (_fontWid[3], TRUE);
	XtSetSensitive (_fontWid[4], TRUE);

	/*
	 *	Set font value and current font menu selection
	 */
	if ((font_a.reset_value) && (font_a.is_shown)) {

            _txtFont = font_a.value._i;
	    XtVaSetValues (_fontMenu, 
			   XmNmenuHistory, 	_fontWid[font_a.value._i], 
			   NULL);
	}
    }
}

/*=====================================================================*/

void pgtxt_initAlign ( txt_attrib align_a ) 
/************************************************************************
 * pgtxt_initAlign	                                                *
 *                                                                      *
 * This function sets the alignment options for the text edit window.   *
 *									*
 * void pgtxt_initAlign ( align_a )         	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   align_a	txt_attrib     	alignment attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97   Initial coding  	                *
 * E. Safford/GSC	12/97	Modified range of _txtAlign to 1 .. 3	*
 * S. Law/GSC		06/98	Modified range of _txtAlign to -1 .. 1	*
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 ***********************************************************************/
{
    if (!(align_a.is_shown)) {
        XtUnmanageChild (_alignMenu);
	XtUnmanageChild (_labName[ALIGN]);
	return;
    }

    XtManageChild (_alignMenu);
    XtManageChild (_labName[ALIGN]);

    XtSetSensitive (_alignMenu, align_a.is_on);
    XtSetSensitive (_labName[ALIGN],  align_a.is_on); 
    XtSetSensitive (_attrbEdit[ALIGN], align_a.is_on);

    if ((align_a.is_shown) && (align_a.reset_value)) {
	_txtAlign = align_a.value._i;
	XtVaSetValues (_alignMenu, XmNmenuHistory, 
		       _alignWid[_txtAlign + 1], NULL);
    }
}

/*=====================================================================*/

void pgtxt_initRotn ( txt_attrib rotn_a ) 
/************************************************************************
 * pgtxt_initRotn 	                                                *
 *                                                                      *
 * This function sets the rotation options for the text edit window.	*
 *									*
 * void pgtxt_initRotn  ( rotn_a )           	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   rotn_a	txt_attrib     	rotation attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97   Initial coding            	      	*
 * S. Law/GSC		05/98	Added hardware font check		*
 * W. Li/EAI		08/98	Added rotation type			*
 * W. Li/EAI		10/98	Changed  rotation increment  10 -> 5	*
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 * J. Wu/SAIC		08/01	remove embeded '\0' in format		*
 * M. Li/SAIC           10/01   Added OBJ_TEXTICNG                      *
 * J. Wu/SAIC		03/03	set no rotation for OBJ_TEXTMCLOUD	*
 ***********************************************************************/
{
    char	degrees[4]; 
    int		rotn;
/*---------------------------------------------------------------------*/
    if (!(rotn_a.is_shown)) {
        XtUnmanageChild (_rotnRowColW);
	return;
    }

    XtManageChild (_rotnRowColW);
    /* 
     * rotation is not sensitive if hardware font or OBJ_TEXTTURB/
     * OBJ_TEXTICNG/OBJ_TEXTMCLOUD
     */
    _rotnSetShown = rotn_a.is_shown;
    if ( (_rotnSetShown) && (_currObjId != OBJ_TEXTTURB &&
			    _currObjId != OBJ_TEXTICNG && 
			    _currObjId != OBJ_TEXTMCLOUD ) ) {
	rotn_a.is_on = (_txtFont > 2);
    }

    if ((rotn_a.is_shown) && (rotn_a.reset_value)) {
	if (rotn_a.value._i >= 1000) {
	    _rotnType = 2;
	    XmToggleButtonGadgetSetState( _rotnTypBtn[0], FALSE, FALSE);
	    XmToggleButtonGadgetSetState( _rotnTypBtn[1], TRUE, FALSE);

	    rotn = (int)(rotn_a.value._i-1000)%360;
	    rotn = ((rotn % 5) < 3) ? rotn - (rotn%5) : rotn + (5 - rotn%5);

	    sprintf (degrees, "%d", rotn);
	}
	else {		
	    _rotnType = 1;
	    XmToggleButtonGadgetSetState( _rotnTypBtn[0], TRUE, FALSE);
	    XmToggleButtonGadgetSetState( _rotnTypBtn[1], FALSE, FALSE);

            if (rotn_a.value._i >= 0 ){
		rotn = (int)(rotn_a.value._i)%360;
		rotn = ((rotn % 5) < 3) ? rotn - (rotn%5) : rotn + (5 - rotn%5);
                sprintf (degrees, "%d", rotn);
	    }
	}
        XmTextSetString(_rotnTxtW,  degrees);
    }

    XtSetSensitive (_rotnTxtW,  rotn_a.is_on);
    XtSetSensitive (_rotnSldW,  rotn_a.is_on);
    XtSetSensitive (_rotnTyp,   rotn_a.is_on);
    XtSetSensitive (_rotnLbl,   rotn_a.is_on);
    XtSetSensitive (_rotnEdit,  rotn_a.is_on);
}

/*=====================================================================*/

void pgtxt_initSize ( txt_attrib size_a ) 
/************************************************************************
 * pgtxt_initSize 	                                                *
 *                                                                      *
 * This function sets the size options for the text edit window.	*
 *									*
 * void pgtxt_initSize  ( size_a )          	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   size_a	txt_attrib     	rotation attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97           Initial coding                  *
 * C. Lin/EAI       	08/98           Modified to use ctb_fszXXX      *
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 ***********************************************************************/
{
int iret;
/*---------------------------------------------------------------------*/

    if (!(size_a.is_shown)) {
        XtUnmanageChild (_sizeMenu);
        XtUnmanageChild (_labName[SIZE]);
	return;
    }

    XtManageChild (_sizeMenu);
    XtManageChild (_labName[SIZE]);

    XtSetSensitive (_sizeMenu,  size_a.is_on); 
    XtSetSensitive (_labName[SIZE],   size_a.is_on); 
    XtSetSensitive (_attrbEdit[SIZE],   size_a.is_on); 

    if ((size_a.is_shown) && (size_a.reset_value)) {
        XtVaSetValues (_sizeMenu, 
		XmNmenuHistory, 	_sizeWid[size_a.value._i], 
		NULL);
	ctb_fszval(size_a.value._i, &_txtFontSz, &iret);
    } 
}

/*=====================================================================*/

void pgtxt_initTurb ( txt_attrib turb_a ) 
/************************************************************************
 * pgtxt_initTurb 	                                                *
 *                                                                      *
 * This function sets the turbulence options for the text edit window.  *
 *									*
 * void pgtxt_initTurb  ( turb_a )         	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   turb_a	txt_attrib     	rotation attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97   Initial coding                  	*
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 * H. Zeng/XTRIA        12/02   modified to use _editMenuForms Widget   *
 * J. Wu/SAIC       	03/03   resetting turb menu history correctly	*
 * J. Wu/SAIC       	05/03   remap menu wids with correct index	*
 ***********************************************************************/
{
    XmString  	label;
    int		turbsym;
/*---------------------------------------------------------------------*/

    if (!(turb_a.is_shown)) {
        XtUnmanageChild (_editMenuRc);
        XtUnmanageChild (_editMenuForms[TURB]); 
        XtManageChild (_editMenuRc);
	return;
    }

    if (_sptxType == 7 && _currTxtTurb != 7) {
	_currTxtTurb = 7;
	label = XmStringCreateLocalized ("L_turb");
	XtVaSetValues (_labName[TURB], XmNlabelString, label, NULL);
	XmStringFree (label);
    }

    if (_sptxType == 9 && _currTxtTurb != 9) {
	_currTxtTurb = 9;
	label = XmStringCreateLocalized ("H_turb");
	XtVaSetValues (_labName[TURB], XmNlabelString, label, NULL);
	XmStringFree (label);
    }

    XtUnmanageChild (_editMenuRc);
    XtManageChild (_editMenuForms[TURB]); 
    XtManageChild (_editMenuRc);

    XtSetSensitive (_turbMenu,  turb_a.is_on);
    XtSetSensitive (_labName[TURB],   turb_a.is_on); 
    XtSetSensitive (_attrbEdit[TURB],   turb_a.is_on);  

    if ((turb_a.is_shown) && (turb_a.reset_value) && 
			(turb_a.value._i >= 0) ) {
        _txtTurb = turb_a.value._i;
        
	turbsym = _txtTurb;
	
        if ( turbsym == 56 )
            turbsym = 5;
        else if (  turbsym== 5 || turbsym == 6 )
            turbsym++;
        else if ( turbsym == 77 )
            turbsym = 8;
        else if ( turbsym == 7 || turbsym == 8 )
            turbsym += 2;
	
        XtVaSetValues (_turbMenu, XmNmenuHistory, _turbWid[turbsym], NULL);
	    
    }	
}

/*=====================================================================*/

void pgtxt_initIcng ( txt_attrib icng_a )
/************************************************************************
 * pgtxt_initIcng                                                       *
 *                                                                      *
 * This function sets the Icing options for the text edit window.       *
 *                                                                      *
 * void pgtxt_initIcng  ( icng_a )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *   icng_a     txt_attrib      rotation attributes                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           11/01   Initial coding                          *
 * H. Zeng/XTRIA        12/02   modified to use _editMenuForms Widget   *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (!(icng_a.is_shown)) {
        XtUnmanageChild (_editMenuRc);
        XtUnmanageChild (_editMenuForms[ICNG]); 
        XtManageChild (_editMenuRc);
        return;
    }

    XtUnmanageChild (_editMenuRc);
    XtManageChild (_editMenuForms[ICNG]); 
    XtManageChild (_editMenuRc);

    XtSetSensitive (_icngMenu,  icng_a.is_on);
    XtSetSensitive (_labName[ICNG],   icng_a.is_on);
    XtSetSensitive (_attrbEdit[ICNG],   icng_a.is_on);

    if ((icng_a.is_shown) && (icng_a.reset_value) &&
                        (icng_a.value._i >= 0) && (icng_a.value._i <= 10)) {
        _txtIcng = icng_a.value._i;

        XtVaSetValues (_icngMenu, XmNmenuHistory, _icngWid[_txtIcng], NULL);
    }
}

/*=====================================================================*/

void pgtxt_initStyle ( txt_attrib style_a ) 
/************************************************************************
 * pgtxt_initStyle 	                                                *
 *                                                                      *
 * This function sets the style options for the text edit window.  	*
 *									*
 * void pgtxt_initStyle  ( style_a )         	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   style_a	txt_attrib     	rotation attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97   Initial coding                          *
 * E. Safford/GSC       04/98   fix to check reset_value                *
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 ***********************************************************************/
{
    if (!(style_a.is_shown)) {
        XtUnmanageChild (_styleMenu);
        XtUnmanageChild (_labName[STYLE]);
	return;
    }

    if (style_a.reset_value) {
        XtVaSetValues (_styleMenu, XmNmenuHistory, 
		       _styleWid[style_a.value._i], NULL);
        _txtStyle = style_a.value._i;
    }

    XtManageChild (_styleMenu);
    XtManageChild (_labName[STYLE]);

    XtSetSensitive (_styleMenu,  style_a.is_on);
    XtSetSensitive (_labName[STYLE],   style_a.is_on); 
    XtSetSensitive (_attrbEdit[STYLE],  style_a.is_on);
}

/*=====================================================================*/

void pgtxt_initBox ( txt_attrib box_a ) 
/************************************************************************
 * pgtxt_initBox							*
 *									*
 * This function sets the box options for the text edit window.		*
 *									*
 * void pgtxt_initBox (box_a)						*
 *									*
 * Input parameters:							*
 *	box_a	txt_attrib		box options 			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	Copied from NxmTxtA_initStyle		*
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 * W. Li/EAI		01/98	Added Filled under line			*
 * H. Zeng/EAI          03/01   Updated _currTxtGen value               *
 * M. Li/SAIC		06/02	Added pgsymb_getObjId			*
 * T. Lee/SAIC		08/02	Added overline and filled overline	*
 * J. Wu/SAIC		08/02	set outline box for VOLCANO symbol only	*
 * m.gamazaychikov/saic 11/02	changed VOLCANO _txtbox to 4		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    if (!(box_a.is_shown)) {
        XtUnmanageChild (_boxMenu);
        XtUnmanageChild (_labName[BOX]);
	return;
    }

    if (box_a.reset_value) {
	_sptxType = box_a.value._i;
	if (_currObjId == OBJ_TEXTGEN) {
            _currTxtGen = _sptxType;
	    switch(_sptxType){
		case 10:
		    _txtBox = 6;  /* underline */
		    break;
		case 11:
		    _txtBox = 7;  /* Filled underline */
		    break;
		case 13:
		    _txtBox = 8;  /* overline */ 
		    break;
		case 14:
		    _txtBox = 9;  /* Filled overline */ 
		    break;
		default:
		    _txtBox = _sptxType; 
		    break;
	    }

	    /*
	     *  Use an outline box to place the text label for a
	     *  VOLCANO symbol. 
	     */
	    if ( pgsymb_getLabFlag() &&
	         ( pglabel_getOldObject() == OBJ_WXSYM201 ) &&
	         ( pgsymb_getObjId() == OBJ_WXSYM201 ) )  {
	        _txtBox = 4;
		_sptxType = _txtBox;
	    }

            XtVaSetValues (_boxMenu, XmNmenuHistory, 
		           _boxWid[_txtBox], NULL);
        }
    }
    else {
	_sptxType = pgtxt_getTxtType ();
    }

    XtManageChild (_boxMenu);
    XtManageChild (_labName[BOX]);

    XtSetSensitive (_boxMenu,  box_a.is_on);
    XtSetSensitive (_labName[BOX],   box_a.is_on);
    XtSetSensitive (_attrbEdit[BOX],  box_a.is_on); 


}

/*=====================================================================*/

void pgtxt_initText ( txt_attrib text_a ) 
/************************************************************************
 * pgtxt_initText 	                                                *
 *                                                                      *
 * This function controls the starting string in the text field.	*
 *									*
 * void pgtxt_initText  ( text_a )          	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   text_a	txt_attrib     	text attributes				*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97	Initial coding                  	*
 * E. Safford/GSC       12/97   Simplified _textW setup	 	        *
 * C. Lin/EAI           02/98   Set the insertion point at the end	*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * W. Li/EAI		11/98	Added number editor			*
 * W. Li/EAI		11/98	moved number editor to nmap_pgnumb.c	*
 * W. Li/EAI		12/98	removed _textLbl and added _textEdit	*
 * S. Jacobs/NCEP	12/00	Changed CR to LF in the text string	*
 * E. Safford/SAIC	12/01	wipe text to assist in verification	*
 ***********************************************************************/
{
    Cardinal	jj;
/*---------------------------------------------------------------------*/
        
    if (!(text_a.is_shown)) {
        XtUnmanageChild (_textW);
     
	return;
    }

    XtManageChild (_textW);

    if (text_a.is_shown){

	/*
	 * Change CR to LF for display in the widget
	 */
	for ( jj = 0; jj < strlen(text_a.value._c); jj++ ) {
	    if  ( text_a.value._c[jj] == 13 )  {
		text_a.value._c[jj] = '\n';
	    }
	}

	XmTextSetString (_textW, "");
	XmTextSetString (_textW, text_a.value._c);
	XmTextSetInsertionPosition( _textW, 
		XmTextGetLastPosition (_textW) );
    }

    XtManageChild  (_txtRowCol); 
    XtManageChild  (_colrForm);
    XtManageChild  (_rotnRowColW);   

    XtSetSensitive (_textW,	text_a.is_on);
    XtSetSensitive (_textEdit,	text_a.is_on);
}

/*=====================================================================*/

void pgtxt_initColor ( txt_attrib color_a ) 
/************************************************************************
 * pgtxt_initColor	                                                *
 *                                                                      *
 * This function sets the turbulence options for the text edit window.  *
 *									*
 * void pgtxt_initColor ( color_a )         	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   color_a	txt_attrib     	color attributes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97   Initial coding                  	*
 * E. Safford/GSC	10/97	Added initial color setting		*
 * E. Safford/GSC	04/98	modified initial color setting		*
 * W. Li/EAI		12/98	Added multi-selection attribute edit	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    if (!(color_a.is_shown)) {
        XtUnmanageChild (_colrW);
        XtUnmanageChild (_colrLbl);
        XtUnmanageChild (_colrForm);
	return;
    }

    XtManageChild (_colrW);
    XtManageChild (_colrLbl);
    XtManageChild (_colrForm);


    if ((color_a.reset_value) && 
	((color_a.value._i >= 0) && (color_a.value._i <= 31))) {
        _txtColor = color_a.value._i;
    }

    if (color_a.is_shown) {

        XtVaSetValues (_colrW,
		       XmNbackground,		NxmColrP_getColorPixel(_txtColor),
		       XmNtopShadowColor,	NxmColrP_getColorPixel(_txtColor),
		       XmNbottomShadowColor,	NxmColrP_getColorPixel(_txtColor),
		       NULL);	
    }

    XtSetSensitive (_colrW,     color_a.is_on);
    XtSetSensitive (_colrLbl,   color_a.is_on); 
    XtSetSensitive (_colrEdit,  color_a.is_on); 
}

/*=====================================================================*/

void pgtxt_initAttrEdit ( Boolean edit )
/************************************************************************
 * pgtxt_initAttrEdit	                                        	*
 *                                                                      *
 * This function set toggle buttons of edit window 			*
 *                                                                      *
 * void pgtxt_initAttrEdit ( edit)	                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *  	edit	Boolean		 of edit buttons			*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		12/98						*
 * H. Zeng/EAI          01/01     added unmanaging of _txtRowCol        *
 * M. Li/SAIC		11/01     Added NUMB_BTS			*	
 ***********************************************************************/
{
    int         ii, obj;
/*---------------------------------------------------------------------*/

    XmToggleButtonGadgetSetState( _textEdit, FALSE, FALSE);
    XmToggleButtonGadgetSetState( _colrEdit, FALSE, FALSE);
    XmToggleButtonGadgetSetState( _rotnEdit, FALSE, FALSE);

    for (ii=0; ii<NUMB_BTS; ii++)
        XmToggleButtonGadgetSetState( _attrbEdit[ii], FALSE, FALSE);

    if (edit){
        if( XtIsManaged(_txtRowCol) ) {
           XtUnmanageChild(_txtRowCol);
        }
	XtManageChild(_textEdit);
        XtManageChild(_txtRowCol);

	XtManageChild(_colrEdit);
	XtManageChild(_rotnEdit);
	for (ii=0; ii<NUMB_BTS-1; ii++)
	    XtManageChild(_attrbEdit[ii]);

	obj = pgpalw_getCurObjId();
	if (obj == OBJ_TEXTICNG) {
	    XtUnmanageChild(_attrbEdit[TURB]);
	    XtManageChild(_attrbEdit[ICNG]);
	}
	else {
	    XtUnmanageChild(_attrbEdit[ICNG]);
	}

    	/*initiates editFlag to FALSE */

    	for (ii = 0; ii <10; ii++)
	    _editFlags[ii] = FALSE;  

	XtSetSensitive(_textW,	   (int)_editFlags[TEXT]);
	XtSetSensitive(_boxMenu,   (int)_editFlags[BOX]);
	XtSetSensitive(_sizeMenu,  (int)_editFlags[SIZE]);
	XtSetSensitive(_fontMenu,  (int)_editFlags[FONT]);
	XtSetSensitive(_styleMenu, (int)_editFlags[STYLE]);
	XtSetSensitive(_alignMenu, (int)_editFlags[ALIGN]);
	XtSetSensitive(_turbMenu,  (int)_editFlags[TURB]);
	XtSetSensitive(_icngMenu,  (int)_editFlags[ICNG]);
	XtSetSensitive(_colrW,     (int)_editFlags[COLOR]);
	XtSetSensitive(_rotnSldW,  (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnTyp,   (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnTxtW,  (int)_editFlags[ROTN]);
	XtSetSensitive(_rotnLbl,   (int)_editFlags[ROTN]);

    }
    else{
	XtSetSensitive(_textEdit, FALSE);
	XtSetSensitive(_colrEdit, FALSE);
	XtSetSensitive(_rotnEdit, FALSE);
	XtSetSensitive(_attrbEdit[BOX], FALSE);
	XtSetSensitive(_attrbEdit[SIZE], FALSE);
	XtSetSensitive(_attrbEdit[FONT], FALSE);
	XtSetSensitive(_attrbEdit[STYLE], FALSE);
	XtSetSensitive(_attrbEdit[ALIGN], FALSE);
	XtSetSensitive(_attrbEdit[TURB], FALSE);
	XtSetSensitive(_attrbEdit[ICNG], FALSE);

        if( XtIsManaged(_txtRowCol) ) {
           XtUnmanageChild(_txtRowCol);
        }
	XtUnmanageChild(_textEdit);
        XtManageChild(_txtRowCol);

	XtUnmanageChild(_colrEdit);
	XtUnmanageChild(_rotnEdit);
	for (ii=0; ii<NUMB_BTS; ii++)
	    XtUnmanageChild(_attrbEdit[ii]);
    }
}

/*=====================================================================*/

void pgtxt_getTextValue ( int warnflag, char *text )
/************************************************************************
 * pgtxt_getTextValue							*
 *									*
 * This routine returns the current value of the text field.		*
 *									*
 * void pgtxt_getTextValue (warnflag, text)				*
 *									*
 * Input parameters:							*
 *	warnflag	int		warning flag			*
 *									*
 * Output parameters:							*
 *	*text		char		text string			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	Moved from NxmTxtA_getAttr		*
 * S. Law/GSC		07/98	Added warning flag			*
 * T. Piper/SAIC	12/01	Fixed ABR; return if string len = 0	*
 * J. Wu/SAIC		02/03	retrieve midlevel cloud text (type 15)	*
 * S. Jacobs/NCEP	10/09	Check boundary conditions for string len*
 ***********************************************************************/
{
    char        *txt;
    char        ltxt[MAX_TEXT], message[256];
    int         jj, iret;
    unsigned int lead, trail;
/*---------------------------------------------------------------------*/

    if ( _sptxType == 15 ) {
        pgtxt_getMcldTxt ( text ); 
        return;
    }
    else {
        XtVaGetValues (_textW, XmNvalue, &txt, NULL);
    }
    
    trail = strlen(txt);
    if (trail > 0) {

        /*
         *  Strip any leading and trailing carriage 
	 *  returns ('\n' characters).
         */
        lead = 0;
        while (lead < strlen(txt)+1 && txt[lead] == '\n') {
            lead++;
        }
        while (trail > 0 && txt[trail - 1] == '\n') {
            trail--;
        }
        jj = 0;
        for (lead=lead; lead < trail; lead++) {
            ltxt[jj] = txt[lead];
            jj++;
        }

        ltxt[jj] = '\0';

	/*
	 *  If selected font is SOFTWARE (all upper case) convert 
	 *  string to all upper case.
	 */ 
        if (_txtFont == 4) {
            cst_lcuc (ltxt, text, &iret);
        }
        else {
            strcpy (text, ltxt);
        }  
   } 
    else {
	text[0] = '\0';
        if ( warnflag) {
            sprintf (message, "No text entered\n");
            NxmWarn_show (_txtAttribW, message);
        }
    }  

    XtFree (txt);
}
/*=====================================================================*/

void pgtxt_setLabelValue ( char *text )
/************************************************************************
 * pgtxt_setLabelValue							*
 *									*
 * This routine sets the current label value into the text field.	*
 *									*
 * void pgtxt_setLabelValue (text)					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*text		char		* text string			*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * E. Safford/GSC	06/99	set _isLabel to true	          	*
 ***********************************************************************/
{ 
    XmTextSetString (_textW, text);
    XmTextSetInsertionPosition( _textW, 
	    XmTextGetLastPosition (_textW) );
    _isLabel = TRUE;
}


/*=====================================================================*/

void pgtxt_setEditFlag ( int which_flag, Boolean flag_valu )
/************************************************************************
 * pgtxt_setEditFlag	                                        	*
 *                                                                      *
 * This function set multiple selection attribute edit flags		*
 *                                                                      *
 * void pgtxt_setEditFlag (which_flag, flag_valu )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *	which_flag	int						*
 *	flag_valu	Boolean						*
 *  									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		12/98						*
 ***********************************************************************/
{
    int         which, value;
/*---------------------------------------------------------------------*/
    which = which_flag;
    value = (int)flag_valu;
    _editFlags[which] = (Boolean)value;
}

/*=====================================================================*/

void pgtxt_getEditFlag ( Boolean edit_flags[11] )
/************************************************************************
 * pgtxt_getEditFlag	                                        	*
 *                                                                      *
 * This function get multiple selection attribute edit flags		*
 *                                                                      *
 * void pgtxt_getEditFlag ( edit_flags )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *	edit_flags[11]	Boolean	multiple selection attribute edit flags	*
 *  									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		12/98						*
 * M. Li/SAIC		11/01	Expanded the dimension from 9 to 10	*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii=0; ii<10; ii++){
	edit_flags[ii] = _editFlags[ii];
    } 
}


/*=====================================================================*/

void pgtxt_popdown (void)
/************************************************************************
 * pgtxt_popdown							*
 *									*
 * This function unmanages the text popup window			*
 *									*
 * void pgtxt_popdown (void)						*
 *									*
 * Input Parameters:							*
 *		None							*
 *									*
 * Output Parameters:							*
 *		None							*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	 8/97	Initial coding				*
 * E. Safford/GSC	10/97	renamed					*
 * S. Law/GSC		06/98	Added ces_set				*
 * S. Law/GSC		06/98	Moved part NxmTxtA_fillElement		*
 * E. Safford/GSC       12/98   add call to resetFocus                  *
 * E. Safford/SAIC	12/01	mv ces_set in if; add pgutls_initHdr    *
 *				 add pgtxt_setGhostFlag 		*
 ***********************************************************************/
{
    int		iret;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    NxmClrW_popdown();
    if (XtIsManaged (_txtAttribW)) {
	XtUnmanageChild (_txtAttribW);
            if (_exitCb) {
                _exitCb(NULL, NULL, NULL);
            }

        if (_cesFlag) {
	    pgutls_initHdr ( &(el.hdr) );
	    pgtxt_fillElement (_sptxType, &el);
	    ces_set (_sptxType, &el, &iret);
        }

	pgtxt_setGhostFlag (FALSE, NULL);
    }
}

/*=====================================================================*/

void pgtxt_decodeFont ( int font_val, int font_class, int *font_type, 
							int *font_style )
/************************************************************************
 * pgtxt_decodeFont                                                     *
 *                                                                      *
 * This function decodes the font_val, separating the font type         *
 * (hardware or software) from the style (plain, bold, italic, etc).	*
 *                                                                      *
 * void pgtxt_decodeFont ( font_val, font_class, font_type, font_style )*
 *                                                                      *
 * Input parameters:                                                    *
 *   font_val		int	font value (as stored in vgf file)	*
 *   font_class		int	1 = software font, 2 = hardware font	*
 *									*
 * Output parameters:							*
 *   *font_type		int	courier, helvetica, times, or software	*
 *   *font_style	int	plain, bold, italic, or bold-italic	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       12/97	Initial coding 				*
 ***********************************************************************/
{
    *font_style = font_val/10;
    *font_type  = font_val - ((font_val/10) * 10) - 1;

    /*
     *  NxmTxt module displays the type of font in a value range of 0 - 4.  
     *  Software fonts are values 3 and 4 in this scheme.
     */
    if (font_class == SOFTWARE) 
	*font_type = *font_type + 3;
}


/*=====================================================================*/

void pgtxt_resetFocus ( void )
/************************************************************************
 * pgtxt_resetFocus                                                     *
 *                                                                      *
 * This function resets the keyboard focus to the main drawing window.  *
 *                                                                      *
 * void pgtxt_resetFocus ( )                                 		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:							*
 * 	None                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       12/97	Initial coding 				*
 ***********************************************************************/
{  
Widget drawingw;	

/*---------------------------------------------------------------------*/

    drawingw = (Widget)mcanvw_getDrawingW();
    XtSetKeyboardFocus(drawingw, NULL);
}

/*=====================================================================*/

Widget pgtxt_crtCheckBox ( Widget parent, char *labelstr, int row_spc,
			   int opt_spc, int ncol, int nopt,
			   struct incInfo *incstruct, 
			   XtCallbackProc callback )
/************************************************************************
 * pgtxt_crtCheckBox							*
 *									*
 * This function creates a labeled check box selection for options.	*
 *									*
 * Widget pgtxt_crtCheckBox ( parent, labelstr, row_spc, opt_spc, ncol, *
 *			      nopt, incstruct, callback )		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*lablestr	char	label for the selection area		*
 *	row_spc		int	spacing between the rows 		*
 *	opt_spc		int	spacing between the options		*
 *	ncol		int	number of buttons per row		*
 *	nopt		int	number of options			*
 *	callback	XtCallbackProc	callback function		*
 *									*
 * Input/Output parameters:						*
 *	*incstruct	struct incInfo	include information structure	*
 *									*
 * Return parameters:							*
 * pgtxt_crtCheckBox	Widget	Widget ID of a form container widget 	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/03	initial coding				*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	form, label, top_wid, left_wid;
    int		top_spc, left_spc;
    long	ii;
/*---------------------------------------------------------------------*/

    /*
     * create a form container 
     */
    form = XtVaCreateWidget ( "form", xmFormWidgetClass, parent, NULL );

    /*
     * create label 
     */
    label = XtVaCreateManagedWidget( "label",
			xmLabelWidgetClass,	form,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopPosition,		2,
			NULL );
    NxmLabel_setStr ( label, labelstr );

    /*
     * create option check box 
     */
    if ( row_spc < 0 )  row_spc = 5;
    if ( opt_spc < 0 )  opt_spc = 5;
           
    for ( ii = 0; ii < nopt; ii++ ) {		
        incstruct[ii].wid = XtVaCreateWidget ( incstruct[ii].name, 
			xmToggleButtonWidgetClass,	form,
			XmNuserData,			incstruct,
			NULL );  	
        
	top_wid = label;
	top_spc = 2;
	if ( ii >= ncol )  {
	    top_wid = incstruct[ii-ncol].wid;	    
	    top_spc = row_spc;
	} 
	
	left_wid = form;
	left_spc = 2;
	if ( ii % ncol ) {
	    left_wid = incstruct[ii-1].wid;
	    left_spc = opt_spc;
	}
        
	XtVaSetValues ( incstruct[ii].wid,
			XmNtopAttachment,	XmATTACH_WIDGET,
			XmNtopWidget,		top_wid,
			XmNtopOffset,		top_spc,
			XmNleftAttachment,	XmATTACH_WIDGET,
			XmNleftWidget,		left_wid,
			XmNleftOffset,		left_spc,
			NULL );  	

	XtAddCallback ( incstruct[ii].wid, XmNvalueChangedCallback,
			(XtCallbackProc)callback,
			(XtPointer) ii );
        
	XtManageChild ( incstruct[ii].wid );    
    }
   
    XtManageChild (form);

    return (form);
}

/*=====================================================================*/

Widget pgtxt_crtOptMenu ( Widget parent, char *labelstr, char *dir,
			   char  **itemicon, char **itemtext, 
			   int nitems, Widget *menuwid, WidgetList menuitem,
			   char *levelstr, Widget *leveltext, int toff,
			   XtCallbackProc menucallback,
			   XtCallbackProc lvlcallback )
/************************************************************************
 * pgtxt_crtOptMenu							*
 *									*
 * This function creates a labeled pulldown menu and a text input box.	*
 *									*
 * Widget pgtxt_crtOptMenu ( parent, labelstr, dir, itemcion, itemtext,	*
 *			     nitems, menuitem, levelstr, leveltext,	*
 *			     menucallback, lvlcallback )		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label for the menu selection 		*
 *	*dir		char	directory of the icon bitmaps  		*
 *	**itemicon	char	icon bitmap file names  		*
 *	**itemtext	char	icon text message  			*
 *	nitems		int	number of menu items			*
 *	toff		int	offset of text box from top of the form	*
 *	*levelstr	char	label for the text input box 		*
 *	menucallback	XtCallbackProc	callback for menu items		*
 *	lvlcallback	XtCallbackProc	callback for level text input	*
 *									*
 * Output parameters:							*
 *	*menuwid	Widget		widget to hold menu items	*
 *	menuitem	WidgetList	widgets created for menu items	*
 *	*leveltext	Widget		widget created for level input	*
 *									*
 * Return parameters:							*
 * pgtxt_crtOptMenu	Widget	Widget ID of a form container widget 	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC        02/03   initial coding 				*
 * J. Wu/SAIC        03/03   limit the max text length to 16 chars	*
 * J. Wu/SAIC        05/03   free itemwid to avoid memory leak 		*
 * J. Wu/SAIC        08/03   verify top/base input 			*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	form, label, rowcol, menu_bar, menu;
    Widget	level, level_txtwid;
    WidgetList	itemwid;
    XmString	null_string;
    Pixel	fg, bg;
    Pixmap	itempxm;
    int		iret;
    long	ii, ignore;
    char	filename[256], warning[200];
    
/*---------------------------------------------------------------------*/

    /*
     * create a container form 
     */    
    form =  XtVaCreateWidget ( "menu_form",
		xmFormWidgetClass,          	parent,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
	  	NULL );	
    
    /*
     *  creat label and rowcolumn for menu
     */            
    if ( toff <= 0 )  toff = 5;
    
    if ( labelstr != NULL && nitems > 0) { 
	
        label = XtVaCreateManagedWidget( "label",
		xmLabelWidgetClass,	form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		10,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNleftOffset,          0,
		NULL );
        NxmLabel_setStr ( label, labelstr );

        rowcol = XtVaCreateManagedWidget ( "menu_rowcol",
                xmRowColumnWidgetClass,     	form, 
                XmNpacking,                 	XmPACK_TIGHT,
		XmNspacing,			0,
		XmNtopAttachment,	    	XmATTACH_FORM,
		XmNtopOffset,             	0,
		XmNleftAttachment,	    	XmATTACH_WIDGET,
		XmNleftWidget,	    		label,
		XmNleftOffset,              	5,
	  	NULL );
		  
        /*
         * create menu 
         */          
        menu_bar  = XmCreatePulldownMenu ( rowcol, "menu_bar", NULL, 0 );
        menu = XmCreateOptionMenu ( rowcol, "menu", NULL, 0);
	
	null_string = XmStringCreateLocalized ( "" );
        XtVaSetValues ( menu,
		XmNlabelString, 	null_string,	
		NULL );
        XmStringFree ( null_string );
        
	*menuwid = menu;
    
        itemwid = (WidgetList) XtMalloc ( (size_t)nitems * sizeof(Widget) );

        XtVaGetValues ( parent,
                XmNforeground, 		&fg,
                XmNbackground, 		&bg,
                NULL );

        for ( ii = 0; ii < nitems; ii++ ) {
	    cfl_inqr ( itemicon[ii], dir, &ignore, filename, &iret );

            itempxm = XmGetPixmap ( XtScreen(parent), filename, fg, bg );

            if ( itempxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
                sprintf ( warning, "cannot load pixmap file %s",
                        filename );
                NxmWarn_show ( parent, warning );
                itemwid[ii] = XtVaCreateManagedWidget ( itemtext[ii],
		    xmPushButtonWidgetClass,       menu_bar,
		    NULL );
            }
            else {
	        itemwid[ii] = XtVaCreateManagedWidget ( itemtext[ii],
		    xmPushButtonWidgetClass,       menu_bar,
		    XmNlabelType,                  XmPIXMAP,
		    XmNlabelPixmap,                itempxm,
		    NULL);
            }

            XtAddCallback ( itemwid[ii], XmNactivateCallback,
		       (XtCallbackProc)menucallback, (XtPointer)ii );
	    
	    menuitem[ii] = itemwid[ii];
                        
	}
        
	XtVaSetValues ( menu,
		    XmNsubMenuId,	menu_bar,
		    XmNmenuHistory,	menuitem[0], 
		    NULL );
    
        XtManageChild ( menu );    
	
	XtFree ( (XtPointer)itemwid );	
    
    }

    /*
     * create top/base text input
     */
    if ( levelstr != NULL ) {
        level = XtVaCreateManagedWidget( "level",
		xmLabelWidgetClass,	form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		toff,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNleftOffset,          0,
		NULL );
        NxmLabel_setStr ( level, levelstr );
	 
        level_txtwid  = (Widget) XtVaCreateManagedWidget( "level_text",
		xmTextFieldWidgetClass,		form,
        	XmNcolumns,			12,
        	XmNvalue,			"",
        	XmNcursorPositionVisible,	True,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			toff,
		XmNleftAttachment,      	XmATTACH_WIDGET,
		XmNleftWidget,      		level,
		XmNleftOffset,          	10,
  		XmNmaxLength,			16, 
        	NULL );

        XtAddCallback ( level_txtwid, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgtxt_vrfyTxtInput, NULL );
        
	XtAddCallback ( level_txtwid, XmNvalueChangedCallback, 
        	    (XtCallbackProc)lvlcallback, NULL );
        
	if ( leveltext != NULL ) {
	    *leveltext = level_txtwid;
	}
    
    }
    
    XtManageChild ( form );
   
    return ( form );
}

/*=====================================================================*/

void pgtxt_loadMcloudTbl ( int *nmctype, int *nmccode, int *ntstype )
/************************************************************************
 * pgtxt_loadMcloudTbl                                        		*
 *                                                                      *
 * This function loads the content of "mcloud.tbl" into structure. 	*
 *                                                                      *
 * void pgtxt_loadMcloudTbl ( nmctype, nmccode, ntstype )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                      	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nmctype     int          number of midlevel cloud types	*
 *      *nmccode     int          number of midlevel cloud amount codes	*
 *      *ntstype     int          number of midlevel T'storm types	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC        02/03   initial coding 				*
 ***********************************************************************/
{
    FILE    *fp;
    int     ii, kk, nr, itemcnt, ier;
    char    buffer[256];
    char    itemName[16], tmpItem[16], *itemToken, itemSep[] = ";";
/*---------------------------------------------------------------------*/
    	    
    /*
     *  Initialize the midlevel cloud setting structure.
     */
    for ( ii = 0; ii < MAXMCLDITEM; ii++ ) {
        _incMcldType[ii].include = False;   
        _incMcldType[ii].wid = NULL;   
        strcpy ( _incMcldType[ii].name, "" );
        
        _incMcldAmount[ii].include = False;   
        _incMcldAmount[ii].wid = NULL;   
	strcpy ( _incMcldAmount[ii].name, "" );

        _incMcldTstorm[ii].include = False;   
        _incMcldTstorm[ii].wid = NULL;   
	strcpy ( _incMcldTstorm[ii].name, "" );
    }

    
    /*
     *  Open the midlevel cloud settings table.
     */
    fp = (FILE *) cfl_tbop ( MCLOUD_TBL, "pgen", &ier);
    if ( fp == NULL  ||  ier != 0 )  return;

    cfl_tbnr ( fp, &nr, &ier );
    if ( ier != 0 || nr == 0 )  return;
    
    /*
     *  Read the table & load into the appropriate structure.
     */
    ii = 0;
    while ( ii < nr ) {

        cfl_trln( fp, 256, buffer, &ier );
        if ( ier != 0 ) { 
	    break;
        }
	
	itemToken = strtok ( buffer, itemSep );
	
	itemcnt = 0;
	while ( itemToken != NULL && itemcnt < MAXMCLDITEM ) {
	    /* 
	     *  Chop off the leading/ending space in the token. 
	     */                		    		    		    
	    cst_ldsp ( itemToken, itemToken, &kk, &ier );
	    cst_lstr ( itemToken, &kk, &ier );
	    cst_ncpy ( tmpItem, itemToken, kk, &ier );
                    		    
	    /* 
	     *  convert to all upper case & load into structure. 
	     */                		    		    		    
	    cst_lcuc ( tmpItem, itemName, &ier );
            
	    if ( ii == 0 ) {
                strcpy ( _incMcldType[itemcnt].name, itemName );
            }
	    else if ( ii == 1 ) {
	        strcpy ( _incMcldAmount[itemcnt].name, itemName );
            }
	    else if ( ii == 2 ) {
	        strcpy ( _incMcldTstorm[itemcnt].name, itemName );	    
	    }   
            
	    itemcnt++;	    
	    itemToken = strtok ( NULL, itemSep );
        }
	
	if ( ii == 0 ) {
            *nmctype = itemcnt;
        }
	else if ( ii == 1 ) {
	    *nmccode = itemcnt;
        }
	else if ( ii == 2 ) {
	    *ntstype = itemcnt;	    
	}   
    
        ii++;
    }

    cfl_clos ( fp, &ier );

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldTypeCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtxt_mcldTypeCb                                         		*
 *                                                                      *
 * Callback function for midlevel cloud type check boxes.		*
 *                                                                      *
 * void pgtxt_mcldTypeCb ( wid, which, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      none                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC        02/03   initial coding 				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    _incMcldType[which].include = (Boolean)(( _incMcldType[which].include ) ?
    			False : True);
        
    pgtxt_updateGstTxt ();

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldAmountCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtxt_mcldAmountCb                                         		*
 *                                                                      *
 * Callback function for midlevel cloud amount check boxes.		*
 *                                                                      *
 * void pgtxt_mcldAmountCb ( wid, which, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      none                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC        02/03   initial coding 				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
        
    _incMcldAmount[which].include = (Boolean)(( _incMcldAmount[which].include ) ?
    			False : True);

    pgtxt_updateGstTxt ();

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldTstormCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgtxt_mcldTstormCb                                         		*
 *                                                                      *
 * Callback function for midlevel thunderstorm type check boxes.	*
 *                                                                      *
 * void pgtxt_mcldTstormCb ( wid, which, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      none                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC        02/03   initial coding 				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
        
    _incMcldTstorm[which].include = (Boolean)(( _incMcldTstorm[which].include ) ?
    			False : True);

    pgtxt_updateGstTxt ();

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldTurbSymCb ( Widget w, XtPointer clnt,
					XtPointer call )
/************************************************************************
 * pgtxt_mcldTurbSymCb	                                                *
 *                                                                      *
 * Callback for handle the midlevel turbulence symbol menu.	       	*
 *                                                                      *
 * void pgtxt_mcldTurbSymCb ( w, clnt, call )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	copy for pgtxt_turbSymCb()		*
 ***********************************************************************/
{    
    int which;
/*---------------------------------------------------------------------*/
    
    which = (long)clnt;
    
    if ( which >= 0 && which <= 4  )
        _mcldTurbSym = which;    
    else if ( which == 5 )
        _mcldTurbSym = 56;
    else if ( which == 6)
        _mcldTurbSym = 5;
    else if ( which == 7 )
        _mcldTurbSym = 6;
    else if ( which == 8 )
        _mcldTurbSym = 77;
    else if ( which == 9 )
        _mcldTurbSym = 7;
    else
        _mcldTurbSym = 8;

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldIcngSymCb ( Widget w, XtPointer clnt,
					XtPointer call )
/************************************************************************
 * pgtxt_mcldIcngSymCb	                                                *
 *                                                                      *
 * Callback for the midlevel icing symbol menu.		   		*
 *                                                                      *
 * void pgtxt_mcldIcngSymCb ( w, clnt, call )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *	call	XtPointer					*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	copy from pgtxt_icngSymCb()		*
 ***********************************************************************/
{        
/*---------------------------------------------------------------------*/
    
    _mcldIcngSym = (long)clnt;

}

/*=====================================================================*/
/* ARGSUSED */
void pgtxt_mcldTurbLvlCb ( Widget w, XtPointer clnt,
					XtPointer call )
/************************************************************************
 * pgtxt_mcldTurbLvlCb	                                                *
 *                                                                      *
 * Callback for the midlevel turbulance top/base level text field.	*
 *                                                                      *
 * pgtxt_mcldTurbLvlCb ( w, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer       Button press event record       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	initial coding				*
 ***********************************************************************/
{
    char    *ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( w, XmNvalue, &ptext, NULL );

    if ( ptext ) {
        strcpy ( _mcldTurbLvlTxt, ptext ); 
	XtFree (ptext);
    }
    
}

/*=====================================================================*/

/* ARGSUSED */
void pgtxt_mcldIcngLvlCb ( Widget w, XtPointer clnt,
					XtPointer call )
/************************************************************************
 * pgtxt_mcldIcngLvlCb	                                                *
 *                                                                      *
 * Callback for the midlevel icing top/base level text. 		*
 *                                                                      *
 * pgtxt_mcldIcngLvlCb ( w, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer       Button press event record       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	initial coding				*
 ***********************************************************************/
{
    char    *ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( w, XmNvalue, &ptext, NULL );

    if ( ptext ) {
        strcpy ( _mcldIcngLvlTxt, ptext ); 
	XtFree (ptext);
    }
}

/*=====================================================================*/

/* ARGSUSED */
void pgtxt_mcldTstrmLvlCb ( Widget w, XtPointer clnt,
					XtPointer call )
/************************************************************************
 * pgtxt_mcldTstrmLvlCb	                                                *
 *                                                                      *
 * Callback for the midlevel thunderstorm top/base level text.		*
 *                                                                      *
 * pgtxt_mcldTstrmLvlCb ( w, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt     XtPointer       State information record        *
 *      call	XtPointer       Button press event record       *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	initial coding				*
 ***********************************************************************/
{
    char    *ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( w, XmNvalue, &ptext, NULL );

    if ( ptext ) {
        strcpy ( _mcldTstrmLvlTxt, ptext ); 
	XtFree (ptext);
    }

    pgtxt_updateGstTxt ();
    
}

/*=====================================================================*/

void pgtxt_getMcldTxt ( char *text )
/************************************************************************
 * pgtxt_getMcldTxt                                                	*
 *                                                                      *
 * Gets all inputs from the midlevel cloud selection area and encodes	*
 * them into a single string, using '|' as seperator.			*
 *                                                                      *
 * pgtxt_getMcldTxt ( text )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *text		char	Midlevel cloud information string	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/03	initial coding				*
 ***********************************************************************/
{
    int		ii, selected;
    char	infoSep[] = "|", itemSep[] = ";", tmp[8], ntext[MAX_TEXT];
/*---------------------------------------------------------------------*/
    
    strcpy ( ntext, "");

    /*
     *  collect midlevel cloud types
     */
    selected = FALSE;
    for ( ii = 0; ii < _numMcldType; ii++ ) {        	    	
	if ( _incMcldType[ii].include ) {
	    strcat ( ntext, _incMcldType[ii].name );
	    strcat ( ntext, itemSep );
            selected = TRUE;
	}
    }
    
    if ( selected )  ntext[ strlen(ntext) - 1 ] = '\0';
    strcat ( ntext, infoSep );
    
    /*
     *  collect midlevel cloud amount codes
     */
    selected = FALSE;
    for ( ii = 0; ii < _numMcldAmount; ii++ ) {	
	if ( _incMcldAmount[ii].include ) {
	    strcat ( ntext, _incMcldAmount[ii].name );
	    strcat ( ntext, itemSep );
            selected = TRUE;
	}
    }

    if ( selected )  ntext[ strlen(ntext) - 1 ] = '\0';
    strcat ( ntext, infoSep );

    /*
     *  collect midlevel icing/turbulance code, top/base level
     *	Note: icing code/level should go before turbulance for
     *        correct display sequence. 
     */
    sprintf( tmp, "%d", _mcldIcngSym );
    strcat ( ntext, tmp );
    strcat ( ntext, infoSep );
    
    strcat ( ntext, _mcldIcngLvlTxt );
    strcat ( ntext, infoSep );
    
    sprintf( tmp, "%d", _mcldTurbSym );
    strcat ( ntext, tmp );
    strcat ( ntext, infoSep );
    
    strcat ( ntext, _mcldTurbLvlTxt );
    strcat ( ntext, infoSep );

    /*
     *  collect midlevel thunderstorm type & its top/base level
     */
    selected = FALSE;
    for ( ii = 0; ii < _numMcldTstorm; ii++ ) {	
	if ( _incMcldTstorm[ii].include ) {
	    strcat ( ntext, _incMcldTstorm[ii].name );
	    strcat ( ntext, itemSep );
            selected = TRUE;
	}
    }

    if ( selected )  ntext[ strlen(ntext) - 1 ] = '\0';
    strcat ( ntext, infoSep );
    
    strcat ( ntext, _mcldTstrmLvlTxt );

    /*
     *  return the text
     */
    strcpy ( text, ntext );
    
}

/*=====================================================================*/

void pgtxt_initMcld ( txt_attrib text_a ) 
/************************************************************************
 * pgtxt_initMcld 	                                                *
 *                                                                      *
 * Fills the midlevel cloud fields with a selected MCLOUD text element.	*
 *									*
 * void pgtxt_initMcld  ( text_a )          	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   text_a	txt_attrib     	text attributes				*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/03	initial coding				*
 * J. Wu/SAIC		03/03	reset _mcldTurbSym/_mcldIcngSym		*
 * m.gamazaychikov/SAIC	10/03	set the initial settings in pulldown	*
 *				menues for icing and turbulence to 	*
 *				_mcldIcngSym and _mcldTurbSym 		*
 ***********************************************************************/
{
    int		ii, ier, turbsym;
    int		itemcnt, numcld, mclditem = 8, strsiz = 256;
    char	**txtarry, *token, itemSep[] = ";";
/*---------------------------------------------------------------------*/
    
    if ( strlen ( text_a.value._c ) == (size_t)0 ) {
        XtVaSetValues ( _mcldTurbMenu, XmNmenuHistory,
                        _mcldTurbWid[_mcldTurbSym], NULL );
        XtVaSetValues ( _mcldIcngMenu, XmNmenuHistory, 
                        _mcldIcngWid[_mcldIcngSym], NULL );
        return;
    }
  
    /*
     *  turn off all previous selections
     */ 
    pgtxt_clearMcldSel ();
                	    
    /*
     *  decode the text
     */ 
    txtarry= (char **) malloc ( (size_t)mclditem * sizeof(char *) );
    
    for( ii = 0; ii < mclditem; ii++ ) {
	txtarry[ii] = (char *) malloc ( (size_t)strsiz * sizeof(char) );
    }
    
    cst_clst( text_a.value._c, '|', "", mclditem, strsiz, txtarry,
		    &numcld, &ier );	

    /*
     *  reset the midlevel cloud type selections
     */     
    token = strtok ( txtarry[0], itemSep );
    itemcnt = 0;
    while ( token != NULL && itemcnt < _numMcldType ) {
	itemcnt++;	    
	for ( ii = 0; ii < _numMcldType; ii++ ) {	    
	    if ( strcmp( token, _incMcldType[ii].name ) == 0 ) {
		XmToggleButtonSetState ( _incMcldType[ii].wid, True, True );	        
		break;
	    }	
	}
		
	token = strtok ( NULL, itemSep );
    }

    /*
     *  reset the midlevel cloud distribution code selections
     */     
    token = strtok ( txtarry[1], itemSep );
    itemcnt = 0;
    while ( token != NULL && itemcnt < _numMcldAmount ) {
	itemcnt++;	    
	for ( ii = 0; ii < _numMcldAmount; ii++ ) {	    
	    if ( strcmp( token, _incMcldAmount[ii].name ) == 0 ) {
		XmToggleButtonSetState ( _incMcldAmount[ii].wid, True, True );	        
		break;
	    }	
	}
		
	token = strtok ( NULL, itemSep );
    }
    
    /*
     *  reset the midlevel turbulance/icing types
     */     
    _mcldTurbSym = atoi ( txtarry[4] );
    turbsym = _mcldTurbSym; 
    
    if ( turbsym == 56 )
        turbsym = 5;
    else if (  turbsym== 5 || turbsym == 6 )
        turbsym++;
    else if ( turbsym == 77 )
        turbsym = 8;
    else if ( turbsym == 7 )
        turbsym = 9;
    else if ( turbsym == 8 )
        turbsym = 10;

    XtVaSetValues ( _mcldTurbMenu,
		   XmNmenuHistory,	_mcldTurbWid[turbsym], 
		   NULL );
     
    _mcldIcngSym = atoi ( txtarry[2] );    
    XtVaSetValues ( _mcldIcngMenu,
		   XmNmenuHistory,	_mcldIcngWid[_mcldIcngSym], 
		   NULL );

    /*
     *  reset the midlevel turbulance/icing/thunderstorm level
     */     
    XmTextSetString ( _mcldTurbLvlTxtW, txtarry[5] );
    XmTextSetString ( _mcldIcngLvlTxtW, txtarry[3] );
    XmTextSetString ( _mcldTstrmLvlTxtW, txtarry[7] );

    /*
     *  reset the midlevel thunderstorm selections
     */     
    token = strtok ( txtarry[6], itemSep );
    itemcnt = 0;
    while ( token != NULL && itemcnt < _numMcldTstorm ) {
	itemcnt++;	    
	for ( ii = 0; ii < _numMcldTstorm; ii++ ) {	    
	    if ( strcmp( token, _incMcldTstorm[ii].name ) == 0 ) {
		XmToggleButtonSetState ( _incMcldTstorm[ii].wid, True, True );	        
		break;
	    }	
	}
		
	token = strtok ( NULL, itemSep );
    }
    		
    /*
     *  clean up
     */     
    for( ii = 0; ii < mclditem; ii++ )
	free( txtarry[ii] );
	
    if ( txtarry )  free( (char **) txtarry );
          
}

/*=====================================================================*/

void pgtxt_clearMcldSel ( void )
/************************************************************************
 * pgtxt_clearMcldSel                                                	*
 *                                                                      *
 * This function clears all midlevel cloud text fields.			*
 *                                                                      *
 * pgtxt_clearMcldSel ( void )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/03	initial coding				*
 ***********************************************************************/
{
    int    ii;
/*---------------------------------------------------------------------*/
    
    for ( ii = 0; ii < _numMcldType; ii++ ) {
	if ( XmToggleButtonGetState ( _incMcldType[ii].wid ) ) {
	    XmToggleButtonSetState ( _incMcldType[ii].wid, False, True );	        
	}
    }

    for ( ii = 0; ii < _numMcldAmount; ii++ ) {
	if ( XmToggleButtonGetState ( _incMcldAmount[ii].wid ) ) {
	    XmToggleButtonSetState ( _incMcldAmount[ii].wid, False, True );	        
	}
    }

    for ( ii = 0; ii < _numMcldTstorm; ii++ ) {
	if ( XmToggleButtonGetState ( _incMcldTstorm[ii].wid ) ) {
	    XmToggleButtonSetState ( _incMcldTstorm[ii].wid, False, True );	        
	}
    }

    XmTextSetString ( _mcldTurbLvlTxtW, "" );
    XmTextSetString ( _mcldIcngLvlTxtW, "" );
    XmTextSetString ( _mcldTstrmLvlTxtW, "" );
    XtVaSetValues ( _mcldTurbMenu, XmNmenuHistory, _mcldTurbWid[0], NULL );
    XtVaSetValues ( _mcldIcngMenu, XmNmenuHistory, _mcldIcngWid[0], NULL );

}

/*=====================================================================*/

void pgtxt_getSelMcloud ( int *multi_sel, int *mixed_sel )
/************************************************************************
 * pgtxt_getSelMcloud                                                	*
 *                                                                      *
 * Checks if there are more than one selected MCLOUD text elements in	*
 * WORK_FILE and no other types of text elements are selected. Also	*
 * check if MCLOUD text elements and other types of text elements	*
 * are selected together. 						*
 *                                                                      *
 * pgtxt_getSelMcloud ( multi_sel, mixed_sel )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *multi_sel	int	TRUE/FALSE - more than one MCLOUD text	*
 *                              selected & no other text types selected	*
 *      *mixed_sel	int	TRUE/FALSE - both MCLOUD & other text 	*
 *                              types selected				*
 *                                                                  	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/03	initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int		ii, location, num_mcld, num_other, ier;
    char	sel_flag;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    ii = 0;
    
    /*
     *  Loop through the range array to find if more than one MCLOUD
     *  objects are selected & if other types of text objects are
     *  also selected.
     */
    num_mcld = 0;
    num_other = 0;
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {
	 crg_gsel ( ii, &sel_flag, &ier ); 
	 if ( sel_flag ) {
	     crg_goffset ( ii, &location, &ier );	    
	     cvg_rdrec ( cvg_getworkfile(), location, &el, &ier );
	       
	     if ( el.hdr.vg_type == SPTX_ELM ) {
	         
		 if ( el.elem.spt.info.sptxtyp == 15 )
		     num_mcld++;
		 else 
		     num_other++;
	     }
	 }
	 
	 if ( num_mcld > 1 && num_other > 0 )  break;
	 
    }
    
    /*
     *  If more than one MCLOUD elements are selected and no there
     *  types of text are selected, set "multi_sel" to TRUE
     */
    *multi_sel = FALSE;
    if ( num_mcld > 1 && num_other == 0 )  
        *multi_sel = TRUE;

    /*
     *  If at least one MCLOUD element is selected and at least
     *  one other text objects are selected, set "mixed_sel" to TRUE
     */
    *mixed_sel = FALSE;
    if ( num_other > 0 && num_mcld > 0 )  
        *mixed_sel = TRUE;
           
}

/*=====================================================================*/
/* ARGSUSED */
static void pgtxt_vrfyTxtInput ( Widget text_w, XtPointer clnt, 
			         XtPointer call ) 
/************************************************************************
 * pgtxt_vrfyTxtInput							*
 *									*
 * This is an internal function used to verify the contents of a text	*
 * widget in Middle Level Cloud text as the user is entering data.	*
 * It examines the string entered and verifies that it contains only	*
 * digits, white space, 'X', 'x' and one '/'. Any other characters will *
 * force the "doit" flag of the cbs struct be set to "FALSE". This	*
 * signals the text widget to NOT display the data and SOUND the alarm	*
 * bell (a single beep). Note that the bell may be switched off by	*
 * setting the text widget's XmNaudibleWarning value ("TRUE" by default)*
 *									*
 * static void pgutls_vrfyTxtInput ( text_w, clnt, call )	*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt	XtPointer Widget's event data (not used here)   *
 *	call	XtPointer callback structure			*
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/03	modify from pgutls_vrfyNumeric		*
 ***********************************************************************/
{
    int 	ii, type, ier;
    char	*text;
    Boolean	reject, bsh_found = False;

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL ) {
	return;
    }


    /*
     *  Get the current contents of the text widget and check for '/'
     */
    XtVaGetValues (text_w, XmNvalue, &text, NULL);
    if ( strchr (text, '/') != NULL ) {
	bsh_found = True;
    }


    /*
     *  Check through each character. 
     */
    reject = False;
    for ( ii = 0; ii < cbs->text->length; ii++ ) {

	/*
	 * cst_alnum returns type == 2 if the char is a digit
	 */
	cst_alnm ( cbs->text->ptr[ii], &type, &ier );

	if ( type != 2 ) {
	    
	    reject = True;

	    /*  
	     *  Check for 'X', 'x' and '/'
	     */
	    if ( isspace ( cbs->text->ptr[ii] ) ||
	         cbs->text->ptr[ii] == 'X' || 
	         cbs->text->ptr[ii] == 'x' ) {
		reject = False;
            }
	    else if ( cbs->text->ptr[ii] == '/' && ( !bsh_found ) )  {
		reject = False;		
		bsh_found = True;	    
	    }	 	    	    
	}
	
	/*
	 *  The doit flag of the callback structure is used to signal
	 *  to X to not allow the typed character to be entered into the
	 *  text widget.
	 */
	if ( reject ) {
	    cbs->doit = FALSE;
	    break;
	}	
    }
}

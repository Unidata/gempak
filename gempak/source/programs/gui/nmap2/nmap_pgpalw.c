#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "Nxm.h"
#include "hints.h"
#include "nmap_mainw.h"		/* MAPW_NAME, MAINW_NAME */


#define CTRLBTN_TBL     "ctrlbtn.tbl"
#define OPERBTN_TBL	"funcbtn.tbl"
#define CLASSBTN_TBL	"classbtn.tbl"
#define ICON_DIR	"$NAWIPS/icons/nmap" 

#define MAX_FNAME_SZ	20
#define NONE_CURRENT	0

#define ICON_FG		"black"
#define ICON_BG		"bisque1"
#define ICON_HILITE	"blue"
#define ICON_WDTH	24	
#define ICON_HGHT	24	

#define PXM_REG		 0
#define PXM_REV		 1

#define LOT_OPER	 0
#define LOT_CLASS	 1
#define LOT_OBJ		 2

#define BUTTON_TYPE	xmPushButtonWidgetClass

/*
 * palinfo_t  - palette infomation data structure 
 *	The different pallette options (operation/function, class, and
 *	object) are found in two different sources.  The first is drwids.h,
 *	which is static and is reflected by the constants FUNC_*, CLASS_*,
 *	and OBJ_*.  The second source are the button tables (funcbtn.tbl, 
 *	classbtn.tbl, and the various object button tables), each of which
 *	is user changable to determine the order of the buttons within each
 *	palette.  All references in palinfo_t are static ones.  The dynamic 
 *	references only appear in pgpalw_create.  At that point, the 
 *	relationship between the static and dynamic is found and the button
 *	widgets are created in the user-defined order.  After that, the 
 *	dynamic references are no longer needed and not mentioned again.
 */

typedef struct {
    Widget	wid;
    char	name[MAX_FNAME_SZ];
    Pixmap	pxms[2];	/* regular and reversed pixmaps */
} button_t;

typedef struct {
    button_t	oper[MAX_OPERS];
    button_t	class[MAX_CLASSES];
    button_t	obj[MAX_OBJECTS];	/* holds objects in order by class */

/* total # of buttons */
    int         nctrls;               /* number of control buttons */
    int         nopers;               /* number of operations in palette */
    int         nclasses;	      /* number of classes in the palette */
    int         nobjs[MAX_CLASSES];   /* number of objects in each class */

/* current selected button index */
    int         cur_oper;               /* current operation index */
    int         cur_class;              /* current class index */
    int         cur_obj;   		/* current object index */
} palinfo_t;

static Widget 		_paletteW;      /* Widget id for popup palete panel */
static Widget 		_oper0_rcW, _oper1_rcW; 
static Widget 		_oper0_frameW, _oper1_frameW;
static Widget 		_class_rcW;     /* Widget id for class rowcol */
static Widget 		_class_frameW;
static Widget 		_objpalsW[MAX_CLASSES];
static Widget 		_modeFrameW;
static Widget 		_modeBoxW;
static Widget 		_prevOperWid;   /* previous oper selection */

static palinfo_t 	_palInfo;	/* holds palette information */

static char		_palMode;
static Boolean          _grpActv = FALSE;

static Boolean		_exitPGEN = FALSE; /* if the "EXIT" is initiated */

static Pixel		_iconTShadow;
static Pixel		_iconBShadow;

static Pixmap   _layerPxms[4];          /* 2 sets of layer regular and
                                           reversed pixmaps */

static Boolean          _layerActv = FALSE; /* if the layer is activated */
static Boolean          _GFAp_Actv = FALSE; /* if the GFAp window is available */

/* 
 *  private callback functions 
 */
static void pgpalw_cancelExit (	 Widget, XtPointer, XtPointer);
static void pgpalw_classCb (	 Widget, XtPointer, XtPointer);
static void pgpalw_deleteAllCb ( Widget, XtPointer, XtPointer);
static void pgpalw_endGrpShowEh( Widget, XtPointer, XEvent*, Boolean*);
static void pgpalw_operCancelCb (Widget, XtPointer, XtPointer);
static void pgpalw_restConfirmCb(Widget, XtPointer, XtPointer);
static void pgpalw_setModeCb (	 Widget, XtPointer, XtPointer);
static void pgpalw_ungroupAllCb (Widget, XtPointer, XtPointer);

/* 
 *  private functions 
 */
static void	pgpalw_getBtnLocation (int type, int class, 
						int start, int *palids);
static void 	pgpalw_manageObjPal ( int classnum );
static Boolean 	pgpalw_modifyType ( void );
static void	pgpalw_setButtonState( int type, int button );
static void	pgpalw_setCurClass ( Widget wid );


/************************************************************************
 * nmap_pgpalw.c							*
 *									*
 * This module defines the tool palettes used in product generation	*
 *									*
 * CONTENTS:								*
 *  pgpalw_create()	      creates palette				*
 *  pgpalw_popup()	      popup the palette window			*
 *  pgpalw_popdown()  	      pop down the palette window		*
 *									*
 *  pgpalw_setupOper()	      set the operation button action		*
 *  pgpalw_classPopdown	      popdown the object palette for each class	*
 *  pgpalw_setCurBtns	      set current operation, class, and obj btns*
 *  pgpalw_setPrevOper()      reset to the previous oper selection      *
 *  pgpalw_exitCheck()	      called for "exit" button action		*
 *  pgpalw_exit()	      called for "OK" for "EXIT" action		*
 *  pgpalw_cancelExit()       called for "Cancel" for "EXIT" action     *
 *  pgpalw_setBtnSntv()	      Func for setting any button's sensitivity *
 *  pgpalw_snstvUndo()	      Func for setting undo/redo sensitivity	*
 *  pgpalw_dsplyObjPal()      Func for displaying object palete		*
 *  pgpalw_rfrshObjPal()      Func for refreshing object palete		*
 *  pgpalw_unmanageObjPal()   Unmanages an object palette		*
 *  pgpalw_deleteAll()        Delete all records and clear drawing area	*
 *  pgpalw_rmDefVGF ()	      erases .DEFAULT.vgf file in the system    *
 *									*
 *  pgpalw_isUp()	      query if the tool palette is up		*
 *  pgpalw_getCurClassName()  get the current class name string	        *
 *  pgpalw_getCurOperId()     get the current operation ID (FUNC_XXXX)  *
 *  pgpalw_getOperWid()	      get the selected oper widget id		*
 *  pgpalw_getPrevOperWid()   get the previous oper Widget id           *
 *  pgpalw_getObjWid()	      get the selected object widget id		*
 *  pgpalw_getCurClassId()    get the current class ID (CLASS_XXXX)	*
 *  pgpalw_getCurObjId()      get the current object ID (OBJ_XXXX)	*
 *  pgpalw_getMode()	      get the mode (OBJ/GRP)               	*
 *									*
 *  pgpalw_operCb()	      Callback for operation button palette	*
 *  pgpalw_classCb()	      Callback for Class Button			*
 *  pgpalw_objCb()	      Callback for object button palette	*
 *									*
 *  pgpalw_deleteAllCb()      Callback for delete all confirmation    	*
 *  pgpalw_ungroupAllCb()     Ungroup all grouped elements		*
 *  pgpalw_endGrpShowCb()     Toggle off display of groups of elements	*
 *  pgpalw_restConfirmCb()    Confirms the restore operation		*
 *  pgpalw_operCancelCb()     Cancels an operation			*
 *  pgpalw_setModeCb()        set the _palMode for the OBJ/GRP selection*
 *									*
 *  pgpalw_manageObjPal()     Manages an object palette			*
 *  pgpalw_setCurOper()	      set the current operation			*
 *  pgpalw_setCurClass()      set the current class			*
 *  pgpalw_setCurObj()	      set the current object			*
 *  pgpalw_modifyType()	      checks if callback is to modify type	*
 *  pgpalw_getBtnLocation()   finds position of buttons on palette	*
 *  pgpalw_setButtonState()   sets the buttons state and highlights	*
 *  pgpalw_undo()	      undoes the last saved operation		*
 *  pgpalw_redo()	      redoes the last saved operation		*
 *  pgpalw_refresh    	      refresh the current pgen elements		*
 *  pgpalw_actvGrp()          makes GROUP process active                *
 *  pgpalw_inactvGrp()        makes GROUP process inactive              *
 *  pgpalw_isGrpActv()        query if GROUP process is active          *
 *  pgpalw_actvLayer()        makes LAYER process active                *
 *  pgpalw_inactvLayer()      makes LAYER process inactive              *
 *  pgpalw_isLayerActv()      query if layering process is active	*
 *  pgpalw_setExitPGEN()      sets the _exitPGEN flag			*
 *  pgpalw_isExitPGEN()	      checks _exitPGEN flag is True or false	*
 *  pgpalw_getObjID()         gets obj. ID asscicated with obj. name.	*
 ***********************************************************************/

/*=====================================================================*/

void pgpalw_create ( int *iret )
/************************************************************************
 * pgpalw_create							*
 *									*
 * Creates the palette used in product generation.			*
 *									*
 * void pgpalw_create(iret)						*
 *									*
 * Output parameters:							*
 *	iret		int *	Return code				*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/97	Created					*
 * E. Safford/GSC	 4/97   Added Text window setup code		*
 * E. Wehner/Eai	 5/97	Split into submenus for Classes/objects	*
 * D. Keiser/GSC	 6/97	Added symbol drawing icons, add maxbtn	*
 *				to cpggetmnuid calling sequence		*
 * D. Keiser/GSC	 6/97	Added delete all func to palette	*
 * E. Wehner/EAi	 6/97	Added wind icons to palette		*
 * D. Keiser/GSC	 6/97	Added flip func and volcano icon	*
 * E. Safford/GSC	 6/97   Added specil text types 1 & 2 icons	*
 * D. Plummer/NCEP	 7/97	Renamed county table to county_sort.tbl	*
 * E. Wehner/EAi	 7/97	Add grouping dialog			*
 * E. Wehner/EAi	 7/97	Add rotation				*
 * E. Safford/GSC	 7/97	Added special text types 3-9		*
 * E. Wehner/EAi	 7/97	Added gui for lines settings		*
 * D.W.Plummer/NCEP	 7/97	Added dashed lines & filled arrow	*
 * D.W.Plummer/NCEP	 7/97	Look for .xbm files in cwd first	*
 * D. Keiser/GSC	 7/97	Change parameter passed to cpg_getset	*
 * E. Wehner/EAi	 7/97	Added dialog for fronts and lines	*
 * D. Keiser/GSC	 8/97	Eliminate file dialog widget from GrInfo*
 * E. Wehner/EAi	 8/97	Remove all widgets from GrInfo and WBC	*
 * E. Wehner/EAi	 9/97	Initialize range records (crg_init)	*
 * E. Safford/GSC	 9/97   Replace all grP->cmd with cmd routines	*
 * E. Wehner/EAi	 9/97	Kill grinfo record			*
 * C. Lin/EAi		10/97	rename from NxmPaletteCr, major cleanup	*
 * E. Safford/GSC	10/97   Modified to use new Nxm routine for text*
 * C. Lin/EAi		10/97   fix calling sequence to pgfrtw_create()	*
 * C. Lin/EAi		10/97   modified for watch,add pgwbxw_create()	*
 * C. Lin/EAi		11/97   change popup title name 		*
 * E. Safford/GSC	11/97   Use new color picker routine		*
 * E. Safford/GSC	11/97   add modify operation			*
 * C. Lin/EAI		12/97   add save/save_as, restore  operation	*
 * C. Lin/EAI		12/97   change icon size parm from 32 to 24	*
 * E. Safford/GSC	01/98   add undo operation			*
 * C. Lin/EAI		02/98   change icon names, add text labeling	*
 * E. Safford/GSC	02/98	add partial delete operation		*
 * W. Li/EAI		03/98	add symbol size, width, color editing	*
 * E. Safford/GSC	03/98	remove unselect & add multiselect	*
 * W. Li/EAI		03/98	Added new symbols			*
 * E. Safford/GSC	03/98	Add class all				*
 * W. Li/EAI		04/98	Add specil text types 10 icons		*
 * E. Safford/GSC	04/98	Add ungroup function			*
 * C. Lin/EAI		04/98	Add combo-symbol and special lines	*
 * F. J. Yen/NCEP	04/98	Updated with new ces function names	*
 * S. Law/GSC		04/98	Add copy function			*
 * C. Lin/EAI		04/98	Add LABEL function and OUTLOOK obj	*
 * W. Li/EAI		04/98	Add darr and hash in CLASS_WINDS	*
 * D.W.Plummer/NCEP	 5/98	Add SFCPRG				*
 * S. Law/GSC		05/98	Mades changes to _palInfo structure	*
 * C. Lin/EAI           06/98   Add more dashlines: dsline2-9           *
 * F. J. Yen/NCEP       06/98   Added QPF obj				*
 * E. Safford/GSC	06/98	add default value for _labelTxtW 	*
 * W. Li/EAI		07/98	Add pwtstorm in  CLASS_SYMBOLS		*
 * D.W.Plummer/NCEP	 8/98	Added HCNTRK obj			*
 * D.W.Plummer/NCEP	 8/98	Added GGCNTR obj			*
 * G. Krueger/EAI	08/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
 * S. Law/GSC		08/98	Add FUNC_DELPOINT			*
 * F. J. Yen/NCEP	09/98	Added XRAINF, SPLN20, SPLN21 obj	*
 * S. Law/GSC		08/98	Add FUNC_CONNECT			*
 * S. Law/GSC		08/98	Add FUNC_UNGRPALL			*
 * E. Safford/GSC	09/98	Add OBJ/GRP modes			*
 * G. Krueger/EAI	10/98	Add OBJ_FLAME				*
 * W. Li/EAI		11/98	Add FUNC_NUMB_EDIT			*
 * S. Law/GSC		11/98	Seperated CLASS_WATCH / CLASS_PRODUCTS	*
 * W. Li/EAI		12/98	Add pgnumb_xxxx				*
 * E. Safford/GSC	12/98	rename FUNC_NUMB_EDIT to FUNC_INC_DEC   *
 * S. Law/GSC		11/98	Changed OBJ_SHOWBOX to OBJ_WATCHFMT	*
 * W. Li/EAI		01/99	NxmTxtA_create --> pgtxt_create		*
 * G. Krueger/EAI	01/99	Add OBJ_XCROSS and OBJ_LOWX		*
 * F. J. Yen/NCEP	01/99	Added WXD obj				*
 * E. Safford/GSC	01/99	added FUNC_SHOW_GRPS                    *
 * S. Jacobs/NCEP	 2/99	Added OBJ_SQUALL			*
 * S. Jacobs/NCEP	 2/99	Changed/Added combo symbols		*
 * S. Jacobs/NCEP	 3/99	Added special line: SPLN22		*
 * S. Law/GSC		04/99	changed row/cols to radio behavior	*
 * W. Li/EAI		05/99	Added FUNC_DEL_OBJ			*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		05/99	added CLASS_SIGMETS			*
 * G. Krueger/EAI	08/99	N & SH trop storm specials		*
 * S. Law/GSC		08/99	added remaining SIGMETs			*
 * H. Zeng/EAI          09/99   added pgofmt_create()                   *
 * H. Zeng/EAI          10/99   added pggfmt_create()                   *
 * H. Zeng/EAI          11/99   modified for Motif 2.1                  *
 * S. Law/GSC		11/99	added call to pgwsmw_create		*
 * T. Piper/GSC		12/99	added OBJ_HAZE				*
 * E. Safford/GSC       12/99   added OBJ_WATCHLN                       *
 * S. Law/GSC		12/99	added OBJ_WTCHSTAT			*
 * S. Law/GSC		02/00	Added CCF				*
 * S. Law/GSC		03/00	Added CCF product			*
 * M. Li/GSC		04/00	Added pgtrk_create			*
 * A. Hardy/GSC		05/00   Added pgwcnsl_xxxxx			*
 * S. Law/GSC		08/00	Changed to use two pixmaps for all btns	*
 * H. Zeng/EAI          12/00   modified for undo redesign              *
 * H. Zeng/EAI          01/01   Added file control function panel       *
 * H. Zeng/EAI          03/01   Added call to ces_gtrtbl()              *
 * S. Jacobs/NCEP	 3/01	Added OBJ_TRPTFNT			*
 * E. Safford/GSC       04/01   arrange palette btns horizontally       *
 * E. Safford/GSC       04/01   alter palette btns for linux 2.2        *
 * H. Zeng/EAI          05/01   Added FUNC_CHNG_GRPS                    *
 * S. Jacobs/NCEP	 9/01	Added OBJ_NUCLEAR			*
 * J. Wu/SAIC		10/01	add OBJ_SPLN23 - double line		*
 * M. Li/SAIC		10/01	Added Marker class and objects		*
 * J. Wu/SAIC		10/01	add OBJ_KINKLN1, 2 - kink arrow lines	*
 * H. Zeng/EAI          10/01   revised GROUP functionality             *
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG			*
 * J. Wu/SAIC		11/01	remove unnecessary crg_init call	*
 * E. Safford/SAIC	11/01	Add all symbols                    	*
 * H. Zeng/EAI          01/02   Added LAYER button                      *
 * H. Zeng/EAI          03/02   Modified attrib. of Layer Button        *
 * H. Zeng/EAI		03/02	modified to use pggrpch_create()	*
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * T. Lee/SAIC		04/02	Added OPEN_PROD button			*
 * J. Wu/SAIC		05/02	pass class ID to load proper obj. pal. 	*
 * M. Li/SAIC		06/02	Added pgsymb_createVolLst		*
 * J. Wu/SAIC		10/02	add FUNC_EXTRAP 			*
 * J. Wu/SAIC		11/02	add CLASS_LIST	 			*
 * R. Tian/SAIC    	01/03   add True flag to NxmBxmBtn_create(Multi)*
 * J. Wu/SAIC		02/03	add OBJ_TEXTMCLOUD - mid level cloud	*
 * m.gamazaychikov/SAIC 04/03   added special symbols 42 thru 49        *
 * m.gamazaychikov/SAIC 04/03   added combo symbol number 28		*
 * T. Piper/SAIC	05/03	removed XAllocNamedColor 		*
 * H. Zeng/XTRIA	07/03   added volcano and ash cloud elements	*
 * J. Wu/SAIC		10/03	add CLASS_MET/OBJ_JET			*
 * H. Zeng/XTRIA	10/03	added more ash cloud stuff		*
 * E. Safford/SAIC	11/03	add FUNC_SAVE_ALL			*
 * J. Wu/SAIC		11/03	add pgjet_create()			*
 * E. Safford/SAIC	11/03	add FUNC_SMEAR				*
 * A. Hardy/NCEP	12/03   added special symbol 50 		*
 * J. Wu/SAIC		02/04	add pggfaw_create()			*
 * J. Wu/SAIC		03/04	add OBJ_NCONSIG under CLASS_MET		*
 * B. Yin/SAIC		03/04   add pgtca_create()			*
 * H. Zeng/XTRIA	03/04	allowed N-column btn display		*
 * J. Wu/SAIC		04/04	add FUNC_INTERP 			*
 * A. Hardy/NCEP	 4/04   added OBJ_LISTWBCMZ under CLASS_LIST	*
 * B. Yin/SAIC           4/04   Changed tca's parent to the toplevel    *
 * J. Wu/SAIC		06/04	add OBJ_GFA under CLASS_MET		*
 * E. Safford/SAIC	06/04	add checks on N-column setup		*
 * J. Wu/SAIC		07/04	add FUNC_FILTER 			*
 * J. Wu/SAIC		07/04	add smear control window		*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET & OBJ_NCONSIG		*
 * B. Yin/SAIC		12/04	add AIRMET				*
 * J. Wu/SAIC		03/05	add FUNC_SHOW_NONGRP			*
 * S. Gilbert/NCEP	 6/05	add OBJ_SPLN26 - ZZZZZZ line		*
 * H. Zeng/SAIC		06/06   Initialized _objpalsW			*
 * H. Zeng/SAIC		07/06	add FUNC_DISTANCE			*
 * E. Safford/SAIC	03/07	add FUNC_FROM, OBJ_AIRMET_P, OBJ_GFA_P	*
 * E. Safford/SAIC	03/07   call pggfawp_create()			*
 * E. Safford/SAIC	05/07   call pgairmetp_create()			*
 * J. Wu/SAIC		09/07	add pgfrom_create()			*
 * S. Jacobs/NCEP	04/08	Added OBJ_DSHLN10 - dotted line		*
 * S. Jacobs/NCEP	 5/09	Added OBJ_SPSYM51 thru OBJ_SPSYM56	*
 * L. Hinson/AWC        09/09   Reordered call to pgcycle_create() to   *
 *                              precede call to pggfaw_create()         *
 * X. Guo/CWS		01/10   Add FUNC_ADDPOINT			*
 ***********************************************************************/
{
    Widget		canvas, control_label, oper_label; 
    Widget              class_label, obj_label;
    int			ii, jj, q, curr, curr2, rows, ier, ier2;
    long                ignore;
    char		tbname[256], iconfile[256], mwname[40];
    char		alt_iconfile[256], alt_layer2_icon[256];
    char		*nameptr, col_tag[25], col_val[15];
    struct bxmInfo	bxm_info[2];
    struct pxmBuf	pxm_buff[2];
    int			ids[MAX_OBJECTS], class_ids[MAX_OBJECTS]; 
    int			icon_ncolumn = 3;
    XmString		object, group;
    Boolean		useDayCycle = G_FALSE;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  In order to use the airmet' (airmet prime) GUI and the select
 *  Day/Cycle GUI the day/cycle selection value has to be set to TRUE 
 *  in the prefs.tbl.  
 */
    if( pgcycle_useCycle() ) {
	useDayCycle = G_TRUE;
    }

/* load up icon names  for functions */
    strcpy(_palInfo.oper[NONE_CURRENT].name,	"pgen");
    strcpy(_palInfo.oper[FUNC_CLOSVGF].name,	"exit");
    strcpy(_palInfo.oper[FUNC_OPEN_VGF].name,	"open");
    strcpy(_palInfo.oper[FUNC_SAVE_VGF].name,	"save");
    strcpy(_palInfo.oper[FUNC_SELECT].name,	"select");
    strcpy(_palInfo.oper[FUNC_MOVE].name,	"move");
    strcpy(_palInfo.oper[FUNC_COPY].name,	"copy");
    strcpy(_palInfo.oper[FUNC_DELETE].name,	"delete");
    strcpy(_palInfo.oper[FUNC_REFRESH].name,	"refresh");
    strcpy(_palInfo.oper[FUNC_DELALL].name,	"delall");
    strcpy(_palInfo.oper[FUNC_FLIP].name,	"flip");
    strcpy(_palInfo.oper[FUNC_GROUP].name,	"group");
    strcpy(_palInfo.oper[FUNC_ROTATE].name,	"rotate");
    strcpy(_palInfo.oper[FUNC_MODIFY].name,	"modify");
    strcpy(_palInfo.oper[FUNC_SAVE_AS].name,	"saveas");
    strcpy(_palInfo.oper[FUNC_RESTORE].name,	"restore");
    strcpy(_palInfo.oper[FUNC_UNDO].name,	"undo");
    strcpy(_palInfo.oper[FUNC_PARTDELETE].name,	"partdel");
    strcpy(_palInfo.oper[FUNC_DELPOINT].name,	"delpoint");
    strcpy(_palInfo.oper[FUNC_MULTISEL].name,	"multisel");
    strcpy(_palInfo.oper[FUNC_UNGROUP].name,	"ungroup");
    strcpy(_palInfo.oper[FUNC_UNGRPALL].name,	"ungrpall");
    strcpy(_palInfo.oper[FUNC_CONNECT].name,	"connect");
    strcpy(_palInfo.oper[FUNC_INC_DEC].name,	"inc_dec");
    strcpy(_palInfo.oper[FUNC_SHOW_GRPS].name,  "showgrps");
    strcpy(_palInfo.oper[FUNC_DEL_OBJ].name,	"del_obj");
    strcpy(_palInfo.oper[FUNC_REDO].name,       "redo");
    strcpy(_palInfo.oper[FUNC_CHNG_GRPS].name,	"chnggrps");
    strcpy(_palInfo.oper[FUNC_LAYER].name,      "layer");
    strcpy(_palInfo.oper[FUNC_OPEN_PROD].name,  "openprod");
    strcpy(_palInfo.oper[FUNC_EXTRAP].name,	"extrap");
    strcpy(_palInfo.oper[FUNC_SAVE_ALL].name,   "saveall");
    strcpy(_palInfo.oper[FUNC_SMEAR].name,	"smear"  );
    strcpy(_palInfo.oper[FUNC_BLANK].name,	"blank"  );
    strcpy(_palInfo.oper[FUNC_INTERP].name,	"interp" );
    strcpy(_palInfo.oper[FUNC_FILTER].name,	"filter" );
    strcpy(_palInfo.oper[FUNC_SHOW_NONGRP].name,"shownongrp" );
    strcpy(_palInfo.oper[FUNC_DISTANCE].name,	"distance" );
    strcpy(_palInfo.oper[FUNC_FROM].name,	"from" );
    if( useDayCycle ) {
        strcpy(_palInfo.oper[FUNC_CYCLE].name,	"cycle" );
    }
    strcpy(_palInfo.oper[FUNC_ADDPOINT].name,   "addpoint");

/* load up icon names  for classes */
    strcpy(_palInfo.class[NONE_CURRENT].name,	"class");
    strcpy(_palInfo.class[CLASS_FRONTS].name,	"front");
    strcpy(_palInfo.class[CLASS_TEXT].name,	"text");
    strcpy(_palInfo.class[CLASS_SYMBOLS].name,	"symbol");
    strcpy(_palInfo.class[CLASS_LINES].name,	"line");
    strcpy(_palInfo.class[CLASS_WATCHES].name,	"watch");
    strcpy(_palInfo.class[CLASS_PRODUCTS].name,	"prod");
    strcpy(_palInfo.class[CLASS_WINDS].name,	"vector");
    strcpy(_palInfo.class[CLASS_ANY].name,	"any");
    strcpy(_palInfo.class[CLASS_COMSYM].name,	"combsymb");
    strcpy(_palInfo.class[CLASS_TRACKS].name,	"track");
    strcpy(_palInfo.class[CLASS_SIGMETS].name,	"sigmet");
    strcpy(_palInfo.class[CLASS_CIRCLE].name,	"circle");
    strcpy(_palInfo.class[CLASS_MARKER].name,   "marker");
    strcpy(_palInfo.class[CLASS_LIST].name,     "list");
    strcpy(_palInfo.class[CLASS_MET].name,	"met");
    strcpy(_palInfo.class[CLASS_BLANK].name,	"blank");

/* load up icon names  for objects */
    strcpy(_palInfo.obj[NONE_CURRENT].name,	"obj");

/* fronts */
    strcpy(_palInfo.obj[OBJ_COLDFNT].name,	"coldfnt");
    strcpy(_palInfo.obj[OBJ_WKCOLDFNT].name,	"cold_forming");
    strcpy(_palInfo.obj[OBJ_DIFCOLDFNT].name,	"cold_diss");
    strcpy(_palInfo.obj[OBJ_WARMFNT].name,	"warmfnt");
    strcpy(_palInfo.obj[OBJ_WKWARMFNT].name,	"warm_forming");
    strcpy(_palInfo.obj[OBJ_DIFWARMFNT].name,	"warm_diss");
    strcpy(_palInfo.obj[OBJ_STATFNT].name,	"statfnt");
    strcpy(_palInfo.obj[OBJ_WKSTATFNT].name,	"stat_forming");
    strcpy(_palInfo.obj[OBJ_DIFSTATFNT].name,	"stat_diss");
    strcpy(_palInfo.obj[OBJ_OCCLFNT].name,	"occlfnt");
    strcpy(_palInfo.obj[OBJ_WKOCCLFNT].name,	"occl_forming");
    strcpy(_palInfo.obj[OBJ_DIFOCCLFNT].name,	"occl_diss");
    strcpy(_palInfo.obj[OBJ_DRYFNT].name,	"dryline");
    strcpy(_palInfo.obj[OBJ_TROFFNT].name,	"troffnt");
    strcpy(_palInfo.obj[OBJ_TRPTFNT].name,	"trptfnt");
    strcpy(_palInfo.obj[OBJ_SQUALL].name,	"squall");

/* lines */
    strcpy(_palInfo.obj[OBJ_CNTR].name,		"cntr");
    strcpy(_palInfo.obj[OBJ_DSHLINE].name,	"dshline");
    strcpy(_palInfo.obj[OBJ_DSHLN2].name,       "dsline2");
    strcpy(_palInfo.obj[OBJ_DSHLN3].name,       "dsline3");
    strcpy(_palInfo.obj[OBJ_DSHLN4].name,       "dsline4");
    strcpy(_palInfo.obj[OBJ_DSHLN5].name,       "dsline5");
    strcpy(_palInfo.obj[OBJ_DSHLN6].name,       "dsline6");
    strcpy(_palInfo.obj[OBJ_DSHLN7].name,       "dsline7");
    strcpy(_palInfo.obj[OBJ_DSHLN8].name,       "dsline8");
    strcpy(_palInfo.obj[OBJ_DSHLN9].name,       "dsline9");
    strcpy(_palInfo.obj[OBJ_DSHLN10].name,      "dsline10");
    strcpy(_palInfo.obj[OBJ_SPLN1].name,	"spln1");
    strcpy(_palInfo.obj[OBJ_SPLN2].name,	"spln2");
    strcpy(_palInfo.obj[OBJ_SPLN3].name,	"spln3");
    strcpy(_palInfo.obj[OBJ_SPLN4].name,	"spln4");
    strcpy(_palInfo.obj[OBJ_SPLN5].name,	"spln5");
    strcpy(_palInfo.obj[OBJ_SPLN6].name,	"spln6");
    strcpy(_palInfo.obj[OBJ_SPLN7].name,	"spln7");
    strcpy(_palInfo.obj[OBJ_SPLN8].name,	"spln8");
    strcpy(_palInfo.obj[OBJ_SPLN9].name,	"spln9");
    strcpy(_palInfo.obj[OBJ_SPLN10].name,	"spln10");
    strcpy(_palInfo.obj[OBJ_SPLN11].name,	"spln11");
    strcpy(_palInfo.obj[OBJ_SPLN12].name,	"spln12");
    strcpy(_palInfo.obj[OBJ_SPLN13].name,	"spln13");
    strcpy(_palInfo.obj[OBJ_SPLN14].name,	"spln14");
    strcpy(_palInfo.obj[OBJ_SPLN15].name,	"spln15");
    strcpy(_palInfo.obj[OBJ_SPLN16].name,	"spln16");
    strcpy(_palInfo.obj[OBJ_SPLN17].name,	"spln17");
    strcpy(_palInfo.obj[OBJ_SPLN18].name,	"spln18");
    strcpy(_palInfo.obj[OBJ_SPLN19].name,	"spln19");
    strcpy(_palInfo.obj[OBJ_SPLN20].name,	"spln20");
    strcpy(_palInfo.obj[OBJ_SPLN21].name,	"spln21");
    strcpy(_palInfo.obj[OBJ_SPLN22].name,	"spln22");
    strcpy(_palInfo.obj[OBJ_SPLN23].name,	"spln23");
    strcpy(_palInfo.obj[OBJ_KINKLN1].name,	"kinkln1");
    strcpy(_palInfo.obj[OBJ_KINKLN2].name,	"kinkln2");
    strcpy(_palInfo.obj[OBJ_SPLN26].name,	"spln26");

/* products */
    strcpy(_palInfo.obj[OBJ_WATCHFMT].name,	"wtchfmt");
    strcpy(_palInfo.obj[OBJ_OUTLOOK].name,	"outlook");
    strcpy(_palInfo.obj[OBJ_SFCPRG].name,	"sfcprg");
    strcpy(_palInfo.obj[OBJ_QPF].name,		"qpf");
    strcpy(_palInfo.obj[OBJ_HCNTRK].name,	"hcntrak");
    strcpy(_palInfo.obj[OBJ_GGCNTR].name,	"ggcntr");
    strcpy(_palInfo.obj[OBJ_XRAINF].name,	"xrainf");
    strcpy(_palInfo.obj[OBJ_WXD].name,		"wxd");
    strcpy(_palInfo.obj[OBJ_WTCHSTAT].name,	"wtchstat");
    strcpy(_palInfo.obj[OBJ_CCFPRD].name,	"ccfprd");
    strcpy(_palInfo.obj[OBJ_WTCHCNL].name,	"wtchcnl");
    strcpy(_palInfo.obj[OBJ_AIRMET].name,	"airmet");

/*
 *  In order to use the airmet' (airmet prime) GUI the day/cycle
 *  selection has to be set to TRUE in the prefs.tbl.  Otherwise
 *  the airmet' GUI won't have a valid cycle setting to work with.
 */
    if( useDayCycle ) {
        strcpy(_palInfo.obj[OBJ_AIRMET_P].name,	"airmet_p");
    }

/* text */
    strcpy(_palInfo.obj[OBJ_TEXTGEN].name,	"textgen");
    strcpy(_palInfo.obj[OBJ_TEXTFZL].name,	"textfrzlvl");
    strcpy(_palInfo.obj[OBJ_TEXTTURB].name,	"textturb");
    strcpy(_palInfo.obj[OBJ_TEXTCLD].name,	"textcloud");
    strcpy(_palInfo.obj[OBJ_TEXTICNG].name,     "texticng");
    strcpy(_palInfo.obj[OBJ_TEXTMCLOUD].name,   "textmcloud");

/* vectors */
    strcpy(_palInfo.obj[OBJ_WINDBARB].name,	"windbarb");
    strcpy(_palInfo.obj[OBJ_WINDARRW].name,	"windarrw");
    strcpy(_palInfo.obj[OBJ_WINDDARR].name,	"darr");
    strcpy(_palInfo.obj[OBJ_WINDHASH].name,	"hash");

/* SIGMETs */
    strcpy(_palInfo.obj[OBJ_SIGAIRM].name,	"sigairm");
    strcpy(_palInfo.obj[OBJ_SIGCONV].name,	"sigconv");
    strcpy(_palInfo.obj[OBJ_SIGINTL].name,	"sigintl");
    strcpy(_palInfo.obj[OBJ_SIGNCON].name,	"signcon");
    strcpy(_palInfo.obj[OBJ_SIGOUTL].name,	"sigoutl");
    strcpy(_palInfo.obj[OBJ_SIGCCF].name,	"sigccf");
    strcpy(_palInfo.obj[OBJ_SIGVOL].name,	"sigvol");
    strcpy(_palInfo.obj[OBJ_SIGVAC].name,	"sigvac");

/* watches */
    strcpy(_palInfo.obj[OBJ_WBCOUNTY].name,	"wbcnty");
    strcpy(_palInfo.obj[OBJ_WBPARALL].name,	"wbprll");
    strcpy(_palInfo.obj[OBJ_WATCHLN ].name,     "wtchln");

/* tracks */
    strcpy(_palInfo.obj[OBJ_TRKSTORM].name,	"trkstorm");

/* circles */
    strcpy(_palInfo.obj[OBJ_CIRSOL].name,	"cirsolid");

/* markers */
    strcpy(_palInfo.obj[OBJ_MARK1].name,       	"marker1");
    strcpy(_palInfo.obj[OBJ_MARK2].name,        "marker2");
    strcpy(_palInfo.obj[OBJ_MARK3].name,        "marker3");
    strcpy(_palInfo.obj[OBJ_MARK4].name,        "marker4");
    strcpy(_palInfo.obj[OBJ_MARK5].name,        "marker5");
    strcpy(_palInfo.obj[OBJ_MARK6].name,        "marker6");
    strcpy(_palInfo.obj[OBJ_MARK7].name,        "marker7");
    strcpy(_palInfo.obj[OBJ_MARK8].name,        "marker8");
    strcpy(_palInfo.obj[OBJ_MARK9].name,        "marker9");
    strcpy(_palInfo.obj[OBJ_MARK10].name,       "marker10");
    strcpy(_palInfo.obj[OBJ_MARK11].name,       "marker11");
    strcpy(_palInfo.obj[OBJ_MARK12].name,       "marker12");
    strcpy(_palInfo.obj[OBJ_MARK13].name,       "marker13");
    strcpy(_palInfo.obj[OBJ_MARK14].name,       "marker14");
    strcpy(_palInfo.obj[OBJ_MARK15].name,       "marker15");
    strcpy(_palInfo.obj[OBJ_MARK16].name,       "marker16");
    strcpy(_palInfo.obj[OBJ_MARK17].name,       "marker17");
    strcpy(_palInfo.obj[OBJ_MARK18].name,       "marker18");
    strcpy(_palInfo.obj[OBJ_MARK19].name,       "marker19");
    strcpy(_palInfo.obj[OBJ_MARK20].name,       "marker20");
    strcpy(_palInfo.obj[OBJ_MARK21].name,       "marker21");
    strcpy(_palInfo.obj[OBJ_MARK22].name,       "marker22");

/* cloud type symbols */
    strcpy(_palInfo.obj[OBJ_CLOUD01].name,	"cloud01");
    strcpy(_palInfo.obj[OBJ_CLOUD02].name,	"cloud02");
    strcpy(_palInfo.obj[OBJ_CLOUD03].name,	"cloud03");
    strcpy(_palInfo.obj[OBJ_CLOUD04].name,	"cloud04");
    strcpy(_palInfo.obj[OBJ_CLOUD05].name,	"cloud05");
    strcpy(_palInfo.obj[OBJ_CLOUD06].name,	"cloud06");
    strcpy(_palInfo.obj[OBJ_CLOUD07].name,	"cloud07");
    strcpy(_palInfo.obj[OBJ_CLOUD08].name,	"cloud08");
    strcpy(_palInfo.obj[OBJ_CLOUD09].name,	"cloud09");
    strcpy(_palInfo.obj[OBJ_CLOUD11].name,	"cloud11");
    strcpy(_palInfo.obj[OBJ_CLOUD12].name,	"cloud12");
    strcpy(_palInfo.obj[OBJ_CLOUD13].name,	"cloud13");
    strcpy(_palInfo.obj[OBJ_CLOUD14].name,	"cloud14");
    strcpy(_palInfo.obj[OBJ_CLOUD15].name,	"cloud15");
    strcpy(_palInfo.obj[OBJ_CLOUD16].name,	"cloud16");
    strcpy(_palInfo.obj[OBJ_CLOUD17].name,	"cloud17");
    strcpy(_palInfo.obj[OBJ_CLOUD18].name,	"cloud18");
    strcpy(_palInfo.obj[OBJ_CLOUD19].name,	"cloud19");
    strcpy(_palInfo.obj[OBJ_CLOUD21].name,	"cloud21");
    strcpy(_palInfo.obj[OBJ_CLOUD22].name,	"cloud22");
    strcpy(_palInfo.obj[OBJ_CLOUD23].name,	"cloud23");
    strcpy(_palInfo.obj[OBJ_CLOUD24].name,	"cloud24");
    strcpy(_palInfo.obj[OBJ_CLOUD25].name,	"cloud25");
    strcpy(_palInfo.obj[OBJ_CLOUD26].name,	"cloud26");
    strcpy(_palInfo.obj[OBJ_CLOUD27].name,	"cloud27");
    strcpy(_palInfo.obj[OBJ_CLOUD28].name,	"cloud28");
    strcpy(_palInfo.obj[OBJ_CLOUD29].name,	"cloud29");

/* past weather symbols */
    strcpy(_palInfo.obj[OBJ_PSTWX03].name,	"pstwx03");
    strcpy(_palInfo.obj[OBJ_PSTWX04].name,	"pstwx04");
    strcpy(_palInfo.obj[OBJ_PSTWX05].name,	"pstwx05");
    strcpy(_palInfo.obj[OBJ_PSTWX06].name,	"pstwx06");
    strcpy(_palInfo.obj[OBJ_PSTWX07].name,	"pstwx07");
    strcpy(_palInfo.obj[OBJ_PSTWX08].name,	"pstwx08");
    strcpy(_palInfo.obj[OBJ_PSTWX09].name,	"pstwx09");

/* pressure tendency symbols */
    strcpy(_palInfo.obj[OBJ_PTEND00].name,	"ptend00");
    strcpy(_palInfo.obj[OBJ_PTEND01].name,	"ptend01");
    strcpy(_palInfo.obj[OBJ_PTEND02].name,	"ptend02");
    strcpy(_palInfo.obj[OBJ_PTEND03].name,	"ptend03");
    strcpy(_palInfo.obj[OBJ_PTEND04].name,	"ptend04");
    strcpy(_palInfo.obj[OBJ_PTEND05].name,	"ptend05");
    strcpy(_palInfo.obj[OBJ_PTEND06].name,	"ptend06");
    strcpy(_palInfo.obj[OBJ_PTEND07].name,	"ptend07");
    strcpy(_palInfo.obj[OBJ_PTEND08].name,	"ptend08");

/* sky cover symbols */
    strcpy(_palInfo.obj[OBJ_SKY00].name,	"sky00");
    strcpy(_palInfo.obj[OBJ_SKY01].name,	"sky01");
    strcpy(_palInfo.obj[OBJ_SKY02].name,	"sky02");
    strcpy(_palInfo.obj[OBJ_SKY03].name,	"sky03");
    strcpy(_palInfo.obj[OBJ_SKY04].name,	"sky04");
    strcpy(_palInfo.obj[OBJ_SKY05].name,	"sky05");
    strcpy(_palInfo.obj[OBJ_SKY06].name,	"sky06");
    strcpy(_palInfo.obj[OBJ_SKY07].name,	"sky07");
    strcpy(_palInfo.obj[OBJ_SKY08].name,	"sky08");
    strcpy(_palInfo.obj[OBJ_SKY09].name,	"sky09");
    strcpy(_palInfo.obj[OBJ_SKY10].name,	"sky10");

/* icing symbols */
    strcpy(_palInfo.obj[OBJ_ICE00].name,	"ice00");
    strcpy(_palInfo.obj[OBJ_ICE01].name,	"ice01");
    strcpy(_palInfo.obj[OBJ_ICE02].name,	"ice02");
    strcpy(_palInfo.obj[OBJ_ICE03].name,	"ice03");
    strcpy(_palInfo.obj[OBJ_ICE04].name,	"ice04");
    strcpy(_palInfo.obj[OBJ_ICE05].name,	"ice05");
    strcpy(_palInfo.obj[OBJ_ICE06].name,	"ice06");
    strcpy(_palInfo.obj[OBJ_ICE07].name,	"ice07");
    strcpy(_palInfo.obj[OBJ_ICE08].name,	"ice08");
    strcpy(_palInfo.obj[OBJ_ICE09].name,	"ice09");
    strcpy(_palInfo.obj[OBJ_ICE10].name,	"ice10");

/* special symbols */
    strcpy(_palInfo.obj[OBJ_SPSYM00].name,	"spsym00");
    strcpy(_palInfo.obj[OBJ_SPSYM01].name,	"spsym01");
    strcpy(_palInfo.obj[OBJ_SPSYM02].name,	"spsym02");
    strcpy(_palInfo.obj[OBJ_SPSYM03].name,	"spsym03");
    strcpy(_palInfo.obj[OBJ_SPSYM04].name,	"spsym04");
    strcpy(_palInfo.obj[OBJ_SPSYM05].name,	"spsym05");
    strcpy(_palInfo.obj[OBJ_SPSYM06].name,	"spsym06");
    strcpy(_palInfo.obj[OBJ_SPSYM07].name,	"spsym07");
    strcpy(_palInfo.obj[OBJ_SPSYM08].name,	"spsym08");
    strcpy(_palInfo.obj[OBJ_SPSYM09].name,	"spsym09");
    strcpy(_palInfo.obj[OBJ_SPSYM10].name,	"spsym10");
    strcpy(_palInfo.obj[OBJ_SPSYM11].name,	"spsym11");
    strcpy(_palInfo.obj[OBJ_SPSYM12].name,	"spsym12");
    strcpy(_palInfo.obj[OBJ_SPSYM13].name,	"spsym13");
    strcpy(_palInfo.obj[OBJ_SPSYM14].name,	"spsym14");
    strcpy(_palInfo.obj[OBJ_SPSYM15].name,	"spsym15");
    strcpy(_palInfo.obj[OBJ_SPSYM16].name,	"spsym16");
    strcpy(_palInfo.obj[OBJ_SPSYM17].name,	"spsym17");
    strcpy(_palInfo.obj[OBJ_SPSYM18].name,	"spsym18");
    strcpy(_palInfo.obj[OBJ_SPSYM19].name,	"spsym19");
    strcpy(_palInfo.obj[OBJ_SPSYM20].name,	"spsym20");
    strcpy(_palInfo.obj[OBJ_SPSYM21].name,	"spsym21");
    strcpy(_palInfo.obj[OBJ_SPSYM22].name,	"spsym22");
    strcpy(_palInfo.obj[OBJ_SPSYM23].name,	"spsym23");
    strcpy(_palInfo.obj[OBJ_SPSYM24].name,	"spsym24");
    strcpy(_palInfo.obj[OBJ_SPSYM25].name,	"spsym25");
    strcpy(_palInfo.obj[OBJ_SPSYM26].name,	"spsym26");
    strcpy(_palInfo.obj[OBJ_SPSYM27].name,	"spsym27");
    strcpy(_palInfo.obj[OBJ_SPSYM28].name,	"spsym28");
    strcpy(_palInfo.obj[OBJ_SPSYM29].name,	"spsym29");
    strcpy(_palInfo.obj[OBJ_SPSYM30].name,	"spsym30");
    strcpy(_palInfo.obj[OBJ_SPSYM31].name,	"spsym31");
    strcpy(_palInfo.obj[OBJ_SPSYM32].name,	"spsym32");
    strcpy(_palInfo.obj[OBJ_SPSYM33].name,	"spsym33");
    strcpy(_palInfo.obj[OBJ_SPSYM34].name,	"spsym34");
    strcpy(_palInfo.obj[OBJ_SPSYM35].name,	"spsym35");
    strcpy(_palInfo.obj[OBJ_SPSYM36].name,	"spsym36");
    strcpy(_palInfo.obj[OBJ_SPSYM37].name,	"spsym37");
    strcpy(_palInfo.obj[OBJ_SPSYM38].name,	"spsym38");
    strcpy(_palInfo.obj[OBJ_SPSYM39].name,	"spsym39");
    strcpy(_palInfo.obj[OBJ_SPSYM40].name,	"spsym40");
    strcpy(_palInfo.obj[OBJ_SPSYM41].name,	"spsym41");
    strcpy(_palInfo.obj[OBJ_SPSYM42].name,      "spsym42");
    strcpy(_palInfo.obj[OBJ_SPSYM43].name,      "spsym43");
    strcpy(_palInfo.obj[OBJ_SPSYM44].name,      "spsym44");
    strcpy(_palInfo.obj[OBJ_SPSYM45].name,      "spsym45");
    strcpy(_palInfo.obj[OBJ_SPSYM46].name,      "spsym46");
    strcpy(_palInfo.obj[OBJ_SPSYM47].name,      "spsym47");
    strcpy(_palInfo.obj[OBJ_SPSYM48].name,      "spsym48");
    strcpy(_palInfo.obj[OBJ_SPSYM49].name,      "spsym49");
    strcpy(_palInfo.obj[OBJ_SPSYM50].name,      "spsym50");
    strcpy(_palInfo.obj[OBJ_SPSYM51].name,      "spsym51");
    strcpy(_palInfo.obj[OBJ_SPSYM52].name,      "spsym52");
    strcpy(_palInfo.obj[OBJ_SPSYM53].name,      "spsym53");
    strcpy(_palInfo.obj[OBJ_SPSYM54].name,      "spsym54");
    strcpy(_palInfo.obj[OBJ_SPSYM55].name,      "spsym55");
    strcpy(_palInfo.obj[OBJ_SPSYM56].name,      "spsym56");

/* turbulence symbols */
    strcpy(_palInfo.obj[OBJ_TURB00].name,	"turb00");
    strcpy(_palInfo.obj[OBJ_TURB01].name,	"turb01");
    strcpy(_palInfo.obj[OBJ_TURB02].name,	"turb02");
    strcpy(_palInfo.obj[OBJ_TURB03].name,	"turb03");
    strcpy(_palInfo.obj[OBJ_TURB04].name,	"turb04");
    strcpy(_palInfo.obj[OBJ_TURB05].name,	"turb05");
    strcpy(_palInfo.obj[OBJ_TURB06].name,	"turb06");
    strcpy(_palInfo.obj[OBJ_TURB07].name,	"turb07");
    strcpy(_palInfo.obj[OBJ_TURB08].name,	"turb08");
    
/* combo-symbols */
    strcpy(_palInfo.obj[OBJ_CSYMB01].name,	"csymb01");
    strcpy(_palInfo.obj[OBJ_CSYMB02].name,	"csymb02");
    strcpy(_palInfo.obj[OBJ_CSYMB03].name,	"csymb03");
    strcpy(_palInfo.obj[OBJ_CSYMB04].name,	"csymb04");
    strcpy(_palInfo.obj[OBJ_CSYMB05].name,	"csymb05");
    strcpy(_palInfo.obj[OBJ_CSYMB06].name,	"csymb06");
    strcpy(_palInfo.obj[OBJ_CSYMB07].name,	"csymb07");
    strcpy(_palInfo.obj[OBJ_CSYMB08].name,	"csymb08");
    strcpy(_palInfo.obj[OBJ_CSYMB09].name,	"csymb09");
    strcpy(_palInfo.obj[OBJ_CSYMB10].name,	"csymb10");
    strcpy(_palInfo.obj[OBJ_CSYMB11].name,	"csymb11");
    strcpy(_palInfo.obj[OBJ_CSYMB12].name,	"csymb12");
    strcpy(_palInfo.obj[OBJ_CSYMB13].name,	"csymb13");
    strcpy(_palInfo.obj[OBJ_CSYMB14].name,	"csymb14");
    strcpy(_palInfo.obj[OBJ_CSYMB15].name,	"csymb15");
    strcpy(_palInfo.obj[OBJ_CSYMB16].name,	"csymb16");
    strcpy(_palInfo.obj[OBJ_CSYMB17].name,	"csymb17");
    strcpy(_palInfo.obj[OBJ_CSYMB18].name,	"csymb18");
    strcpy(_palInfo.obj[OBJ_CSYMB19].name,	"csymb19");
    strcpy(_palInfo.obj[OBJ_CSYMB20].name,	"csymb20");
    strcpy(_palInfo.obj[OBJ_CSYMB21].name,	"csymb21");
    strcpy(_palInfo.obj[OBJ_CSYMB22].name,	"csymb22");
    strcpy(_palInfo.obj[OBJ_CSYMB23].name,	"csymb23");
    strcpy(_palInfo.obj[OBJ_CSYMB24].name,	"csymb24");
    strcpy(_palInfo.obj[OBJ_CSYMB25].name,	"csymb25");
    strcpy(_palInfo.obj[OBJ_CSYMB26].name,	"csymb26");
    strcpy(_palInfo.obj[OBJ_CSYMB27].name,	"csymb27");
    strcpy(_palInfo.obj[OBJ_CSYMB28].name,	"csymb28");

/* weather symbols */
    strcpy(_palInfo.obj[OBJ_WXSYM000].name,	"wxsym000");
    strcpy(_palInfo.obj[OBJ_WXSYM001].name,	"wxsym001");
    strcpy(_palInfo.obj[OBJ_WXSYM002].name,	"wxsym002");
    strcpy(_palInfo.obj[OBJ_WXSYM003].name,	"wxsym003");
    strcpy(_palInfo.obj[OBJ_WXSYM004].name,	"wxsym004");
    strcpy(_palInfo.obj[OBJ_WXSYM005].name,	"wxsym005");
    strcpy(_palInfo.obj[OBJ_WXSYM006].name,	"wxsym006");
    strcpy(_palInfo.obj[OBJ_WXSYM007].name,	"wxsym007");
    strcpy(_palInfo.obj[OBJ_WXSYM008].name,	"wxsym008");
    strcpy(_palInfo.obj[OBJ_WXSYM009].name,	"wxsym009");
    strcpy(_palInfo.obj[OBJ_WXSYM010].name,	"wxsym010");
    strcpy(_palInfo.obj[OBJ_WXSYM011].name,	"wxsym011");
    strcpy(_palInfo.obj[OBJ_WXSYM012].name,	"wxsym012");
    strcpy(_palInfo.obj[OBJ_WXSYM013].name,	"wxsym013");
    strcpy(_palInfo.obj[OBJ_WXSYM014].name,	"wxsym014");
    strcpy(_palInfo.obj[OBJ_WXSYM015].name,	"wxsym015");
    strcpy(_palInfo.obj[OBJ_WXSYM016].name,	"wxsym016");
    strcpy(_palInfo.obj[OBJ_WXSYM017].name,	"wxsym017");
    strcpy(_palInfo.obj[OBJ_WXSYM018].name,	"wxsym018");
    strcpy(_palInfo.obj[OBJ_WXSYM019].name,	"wxsym019");
    strcpy(_palInfo.obj[OBJ_WXSYM020].name,	"wxsym020");
    strcpy(_palInfo.obj[OBJ_WXSYM021].name,	"wxsym021");
    strcpy(_palInfo.obj[OBJ_WXSYM022].name,	"wxsym022");
    strcpy(_palInfo.obj[OBJ_WXSYM023].name,	"wxsym023");
    strcpy(_palInfo.obj[OBJ_WXSYM024].name,	"wxsym024");
    strcpy(_palInfo.obj[OBJ_WXSYM025].name,	"wxsym025");
    strcpy(_palInfo.obj[OBJ_WXSYM026].name,	"wxsym026");
    strcpy(_palInfo.obj[OBJ_WXSYM027].name,	"wxsym027");
    strcpy(_palInfo.obj[OBJ_WXSYM028].name,	"wxsym028");
    strcpy(_palInfo.obj[OBJ_WXSYM029].name,	"wxsym029");
    strcpy(_palInfo.obj[OBJ_WXSYM030].name,	"wxsym030");
    strcpy(_palInfo.obj[OBJ_WXSYM031].name,	"wxsym031");
    strcpy(_palInfo.obj[OBJ_WXSYM032].name,	"wxsym032");
    strcpy(_palInfo.obj[OBJ_WXSYM033].name,	"wxsym033");
    strcpy(_palInfo.obj[OBJ_WXSYM034].name,	"wxsym034");
    strcpy(_palInfo.obj[OBJ_WXSYM035].name,	"wxsym035");
    strcpy(_palInfo.obj[OBJ_WXSYM036].name,	"wxsym036");
    strcpy(_palInfo.obj[OBJ_WXSYM037].name,	"wxsym037");
    strcpy(_palInfo.obj[OBJ_WXSYM038].name,	"wxsym038");
    strcpy(_palInfo.obj[OBJ_WXSYM039].name,	"wxsym039");
    strcpy(_palInfo.obj[OBJ_WXSYM040].name,	"wxsym040");
    strcpy(_palInfo.obj[OBJ_WXSYM041].name,	"wxsym041");
    strcpy(_palInfo.obj[OBJ_WXSYM042].name,	"wxsym042");
    strcpy(_palInfo.obj[OBJ_WXSYM043].name,	"wxsym043");
    strcpy(_palInfo.obj[OBJ_WXSYM044].name,	"wxsym044");
    strcpy(_palInfo.obj[OBJ_WXSYM045].name,	"wxsym045");
    strcpy(_palInfo.obj[OBJ_WXSYM046].name,	"wxsym046");
    strcpy(_palInfo.obj[OBJ_WXSYM047].name,	"wxsym047");
    strcpy(_palInfo.obj[OBJ_WXSYM048].name,	"wxsym048");
    strcpy(_palInfo.obj[OBJ_WXSYM049].name,	"wxsym049");
    strcpy(_palInfo.obj[OBJ_WXSYM050].name,	"wxsym050");
    strcpy(_palInfo.obj[OBJ_WXSYM051].name,	"wxsym051");
    strcpy(_palInfo.obj[OBJ_WXSYM052].name,	"wxsym052");
    strcpy(_palInfo.obj[OBJ_WXSYM053].name,	"wxsym053");
    strcpy(_palInfo.obj[OBJ_WXSYM054].name,	"wxsym054");
    strcpy(_palInfo.obj[OBJ_WXSYM055].name,	"wxsym055");
    strcpy(_palInfo.obj[OBJ_WXSYM056].name,	"wxsym056");
    strcpy(_palInfo.obj[OBJ_WXSYM057].name,	"wxsym057");
    strcpy(_palInfo.obj[OBJ_WXSYM058].name,	"wxsym058");
    strcpy(_palInfo.obj[OBJ_WXSYM059].name,	"wxsym059");
    strcpy(_palInfo.obj[OBJ_WXSYM060].name,	"wxsym060");
    strcpy(_palInfo.obj[OBJ_WXSYM061].name,	"wxsym061");
    strcpy(_palInfo.obj[OBJ_WXSYM062].name,	"wxsym062");
    strcpy(_palInfo.obj[OBJ_WXSYM063].name,	"wxsym063");
    strcpy(_palInfo.obj[OBJ_WXSYM064].name,	"wxsym064");
    strcpy(_palInfo.obj[OBJ_WXSYM065].name,	"wxsym065");
    strcpy(_palInfo.obj[OBJ_WXSYM066].name,	"wxsym066");
    strcpy(_palInfo.obj[OBJ_WXSYM067].name,	"wxsym067");
    strcpy(_palInfo.obj[OBJ_WXSYM068].name,	"wxsym068");
    strcpy(_palInfo.obj[OBJ_WXSYM069].name,	"wxsym069");
    strcpy(_palInfo.obj[OBJ_WXSYM070].name,	"wxsym070");
    strcpy(_palInfo.obj[OBJ_WXSYM071].name,	"wxsym071");
    strcpy(_palInfo.obj[OBJ_WXSYM072].name,	"wxsym072");
    strcpy(_palInfo.obj[OBJ_WXSYM073].name,	"wxsym073");
    strcpy(_palInfo.obj[OBJ_WXSYM074].name,	"wxsym074");
    strcpy(_palInfo.obj[OBJ_WXSYM075].name,	"wxsym075");
    strcpy(_palInfo.obj[OBJ_WXSYM076].name,	"wxsym076");
    strcpy(_palInfo.obj[OBJ_WXSYM077].name,	"wxsym077");
    strcpy(_palInfo.obj[OBJ_WXSYM078].name,	"wxsym078");
    strcpy(_palInfo.obj[OBJ_WXSYM079].name,	"wxsym079");
    strcpy(_palInfo.obj[OBJ_WXSYM080].name,	"wxsym080");
    strcpy(_palInfo.obj[OBJ_WXSYM081].name,	"wxsym081");
    strcpy(_palInfo.obj[OBJ_WXSYM082].name,	"wxsym082");
    strcpy(_palInfo.obj[OBJ_WXSYM083].name,	"wxsym083");
    strcpy(_palInfo.obj[OBJ_WXSYM084].name,	"wxsym084");
    strcpy(_palInfo.obj[OBJ_WXSYM085].name,	"wxsym085");
    strcpy(_palInfo.obj[OBJ_WXSYM086].name,	"wxsym086");
    strcpy(_palInfo.obj[OBJ_WXSYM087].name,	"wxsym087");
    strcpy(_palInfo.obj[OBJ_WXSYM088].name,	"wxsym088");
    strcpy(_palInfo.obj[OBJ_WXSYM089].name,	"wxsym089");
    strcpy(_palInfo.obj[OBJ_WXSYM090].name,	"wxsym090");
    strcpy(_palInfo.obj[OBJ_WXSYM091].name,	"wxsym091");
    strcpy(_palInfo.obj[OBJ_WXSYM092].name,	"wxsym092");
    strcpy(_palInfo.obj[OBJ_WXSYM093].name,	"wxsym093");
    strcpy(_palInfo.obj[OBJ_WXSYM094].name,	"wxsym094");
    strcpy(_palInfo.obj[OBJ_WXSYM095].name,	"wxsym095");
    strcpy(_palInfo.obj[OBJ_WXSYM096].name,	"wxsym096");
    strcpy(_palInfo.obj[OBJ_WXSYM097].name,	"wxsym097");
    strcpy(_palInfo.obj[OBJ_WXSYM098].name,	"wxsym098");
    strcpy(_palInfo.obj[OBJ_WXSYM099].name,	"wxsym099");
    strcpy(_palInfo.obj[OBJ_WXSYM103].name,	"wxsym103");
    strcpy(_palInfo.obj[OBJ_WXSYM104].name,	"wxsym104");
    strcpy(_palInfo.obj[OBJ_WXSYM105].name,	"wxsym105");
    strcpy(_palInfo.obj[OBJ_WXSYM107].name,	"wxsym107");
    strcpy(_palInfo.obj[OBJ_WXSYM201].name,	"wxsym201");
    strcpy(_palInfo.obj[OBJ_WXSYM202].name,	"wxsym202");
    strcpy(_palInfo.obj[OBJ_WXSYM203].name,	"wxsym203");

/* lists */
    strcpy(_palInfo.obj[OBJ_LISTCOUNTY].name,	"list_county");
    strcpy(_palInfo.obj[OBJ_LISTZONE].name,	"list_zone");
    strcpy(_palInfo.obj[OBJ_LISTWFO].name,	"list_wfo");
    strcpy(_palInfo.obj[OBJ_LISTSTATE].name,	"list_state");
    strcpy(_palInfo.obj[OBJ_LISTWBCMZ].name,	"list_mzcnty");

/* met */
    strcpy(_palInfo.obj[OBJ_JET].name,		"jet");
    strcpy(_palInfo.obj[OBJ_GFA].name,		"gfa");
    strcpy(_palInfo.obj[OBJ_GFA_P].name,	"gfa_p");

/* tca */
    strcpy(_palInfo.obj[OBJ_TCA].name,          "tca");

/* blank */
    strcpy(_palInfo.obj[OBJ_BLANK].name,	"blank");

/*
 * create palette window 
 */
    canvas = (Widget)mcanvw_getDrawingW();
    _paletteW = XmCreateFormDialog(canvas, 
			"pgpalette", NULL, 0);
    XtVaSetValues(_paletteW,
		  XmNnoResize, 	 True, 
		  XmNmarginWidth,  0,
		  XmNmarginHeight, 0,
		  NULL );
    XtVaSetValues(XtParent(_paletteW),
		  XmNtitle, 	 "pgpalette", 
		  NULL );

/*
 * Create file control function panel.
 */
    control_label = XmCreateLabel(_paletteW, "Controls:", NULL, 0);
    XtManageChild(control_label);
    XtVaSetValues(control_label, 
		  XmNtopAttachment, XmATTACH_FORM, 
		  NULL);

    _oper0_frameW = XmCreateFrame( _paletteW, "func0_frame", 
				 NULL, 0);
    XtVaSetValues(_oper0_frameW,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget,     control_label,
		  NULL);

/* 
 * set up the pixmap information
 */
    strcpy ( bxm_info[0].fgcolor, ICON_FG ); 
    strcpy ( bxm_info[0].bgcolor, ICON_BG ); 

    bxm_info[0].insens_bits = NULL;
    bxm_info[1].insens_bits = NULL; 

/* 
 * get PGEN_ICON_NCOLUMN info and set icon_ncolumn variable.
 */
    strcpy ( col_tag, "PGEN_ICON_NCOLUMN" );
    ctb_pfstr( col_tag, col_val, &ier);
    if( ier >= 0 ) {
    icon_ncolumn = atoi ( col_val );
    }
    else {
       icon_ncolumn = 3;
    }

/* 
 * load the file control function palette ids... 
 *  NOTE:  this also loads _palInfo.nctrls, so call this before using
 *  the _palInfo.nctrls value.  Clean this side effect up at some point! 
 */
    pgpalw_getBtnLocation (3, 0, 1, ids);


/* create a row column widget for the operation buttons */
    _oper0_rcW = XmCreateRowColumn(_oper0_frameW,
			"rowcol", NULL, 0);

    rows = 1;
    if ( _palInfo.nctrls > 1 ) {
        rows = (int)_palInfo.nctrls / icon_ncolumn; 
	if ( _palInfo.nctrls % icon_ncolumn ) {  
	    rows++;
	}
    } 

    XtVaSetValues(_oper0_rcW,
		  XmNpacking,		XmPACK_COLUMN,
		  XmNorientation,	XmHORIZONTAL,
    		  XmNnumColumns,	rows,
  		  XmNspacing,		1,
		  NULL);

/* 
 *  load the file control function buttons 
 */
    for (ii=1;  ii <= _palInfo.nctrls; ii++) {
	curr = ids[ii];
	nameptr =  _palInfo.oper[curr].name;

/* 
 * create all buttons pixmaps with inverted colors
 */    
	sprintf (iconfile, "%s.xbm", nameptr);
	cfl_inqr (iconfile, ICON_DIR, &ignore, alt_iconfile, &ier2);

	bxm_info[0].sens_bits = alt_iconfile;
	bxm_info[1].sens_bits = alt_iconfile;

	strcpy (bxm_info[1].fgcolor, ICON_BG); 
	strcpy (bxm_info[1].bgcolor, ICON_FG); 

/*
 * For LAYER button, we have 2 sets of pixmaps to choose.
 */
        if ( curr == FUNC_LAYER ) {
           strcpy (iconfile, "layer2.xbm");
           cfl_inqr (iconfile, ICON_DIR, &ignore, alt_layer2_icon, &ier2);
           bxm_info[0].insens_bits = alt_layer2_icon;
           bxm_info[1].insens_bits = alt_layer2_icon;
        }
        else {
           bxm_info[0].insens_bits = NULL;
           bxm_info[1].insens_bits = NULL;
        }

	_palInfo.oper[curr].wid = 
	    (Widget) NxmBxmBtn_createMulti (_oper0_rcW, 
					    nameptr, BUTTON_TYPE, 
					    ICON_WDTH, ICON_HGHT, 
					    bxm_info, 2, 
					    nameptr, True, pgpalw_operCb, 
					    NULL, pxm_buff); 

	_palInfo.oper[curr].pxms[0] = pxm_buff[0].snstv;
	_palInfo.oper[curr].pxms[1] = pxm_buff[1].snstv;

/*
 * For LAYER button, save 2 sets of pixmaps into a local global.
 */
        if ( curr == FUNC_LAYER ) {
           _layerPxms[0] = pxm_buff[0].snstv;
           _layerPxms[1] = pxm_buff[1].snstv;
           _layerPxms[2] = pxm_buff[0].insnstv;
           _layerPxms[3] = pxm_buff[1].insnstv;

	   XtVaSetValues (_palInfo.oper[curr].wid,
			  XmNlabelType,	    XmPIXMAP,
			  XmNlabelInsensitivePixmap,
                                            XmUNSPECIFIED_PIXMAP,
		          NULL);
        }

/*
 * For BLANK button, make it insensitive.
 */
        if ( curr == FUNC_BLANK ) {

	   XtSetSensitive (_palInfo.oper[curr].wid, FALSE);
        }

	XtVaSetValues (_palInfo.oper[curr].wid,
		       XmNshadowThickness,	1, 
		       XmNmarginLeft,		0,
		       XmNmarginWidth,		2,
		       XmNmarginHeight,		1,
                       XmNtraversalOn,          False,
		       NULL);
    } /* the end of for loop */

/* 
 * Create drawing action panel. 
 */
    oper_label = XmCreateLabel(_paletteW, 
			       "Actions:", NULL, 0);
    XtManageChild(oper_label);
    XtVaSetValues(oper_label, 
		  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget,     _oper0_frameW,
		  NULL);

/*
 *  create the frame for object/group selection
 */
    _modeFrameW = XmCreateFrame ( _paletteW, "mode_frame", NULL, 0);

    XtVaSetValues(_modeFrameW,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget,     oper_label,
                  NULL);

/* create the radio box for obj/grp selection */
    object = XmStringCreateLocalized ("obj");
    group  = XmStringCreateLocalized ("grp");

    _palMode = 0;
    _modeBoxW = XmVaCreateSimpleRadioBox (_modeFrameW, "mode_box",
        (int)_palMode, pgpalw_setModeCb,
        XmVaRADIOBUTTON, object, NULL, NULL, NULL,
        XmVaRADIOBUTTON, group,  NULL, NULL, NULL,
        XmNorientation, XmHORIZONTAL,
        XmNtraversalOn, FALSE,
        NULL);

    XmStringFree (object);
    XmStringFree (group);

/*
 * create the frame for drawing action button selection.
 */
    _oper1_frameW = XmCreateFrame( _paletteW, "func1_frame", 
				 NULL, 0);
    XtVaSetValues(_oper1_frameW,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget,     _modeFrameW,
		  NULL);

/* 
 * set up the pixmap information and load the operations 
 *  NOTE:  this also loads _palInfo.nopers, so call this before using
 *  the _palInfo.nctrls value.  Clean this side effect up at some point! 
 */
    strcpy ( bxm_info[0].fgcolor, ICON_FG ); 
    strcpy ( bxm_info[0].bgcolor, ICON_BG ); 

    bxm_info[0].insens_bits = NULL;
    bxm_info[1].insens_bits = NULL; 

/* 
 * load the drawing action palette ids... 
 */
    pgpalw_getBtnLocation (0, 0, 1, ids);

/* 
 * create a row column widget for the drawing action buttons 
 */
    rows = 1;
    if ( _palInfo.nopers > 1 ) {
        rows = (int)_palInfo.nopers / icon_ncolumn; 
	if ( _palInfo.nopers % icon_ncolumn ) {  
	    rows++;
	}
    } 

    _oper1_rcW = XmCreateRowColumn(_oper1_frameW,
			"rowcol", NULL, 0);
    XtVaSetValues(_oper1_rcW,
		  XmNpacking,		XmPACK_COLUMN,
		  XmNorientation,	XmHORIZONTAL,
		  XmNadjustLast,	FALSE,
		  XmNnumColumns,	rows,
		  XmNspacing,		1,
		  NULL);

/* 
 *  load the drawing action buttons 
 */
    for (ii=1;  ii <= (_palInfo.nopers); ii++) {
	curr = ids[ii];
	nameptr =  _palInfo.oper[curr].name;

/* 
 * create all buttons pixmaps with inverted colors
 */
	sprintf (iconfile, "%s.xbm", nameptr);
	cfl_inqr (iconfile, ICON_DIR, &ignore, alt_iconfile, iret);

	bxm_info[0].sens_bits = alt_iconfile;
	bxm_info[1].sens_bits = alt_iconfile;

	strcpy (bxm_info[1].fgcolor, ICON_BG); 
	strcpy (bxm_info[1].bgcolor, ICON_FG); 
	
	_palInfo.oper[curr].wid = 
	    (Widget) NxmBxmBtn_createMulti (_oper1_rcW, 
					    nameptr, BUTTON_TYPE, 
					    ICON_WDTH, ICON_HGHT, 
					    bxm_info, 2, 
					    nameptr, True, pgpalw_operCb, 
					    NULL, pxm_buff); 

	_palInfo.oper[curr].pxms[0] = pxm_buff[0].snstv;
	_palInfo.oper[curr].pxms[1] = pxm_buff[1].snstv;

/*
 * For BLANK button, make it insensitive.
 */
        if ( curr == FUNC_BLANK ) {

	   XtSetSensitive (_palInfo.oper[curr].wid, FALSE);
        }

	XtVaSetValues (_palInfo.oper[curr].wid,
		       XmNshadowThickness,	1, 
		       XmNmarginLeft,		0,
		       XmNmarginWidth,		2,
		       XmNmarginHeight,		1,
                       XmNtraversalOn,          FALSE,
		       NULL);

    }

/* 
 * create class panel 
 */
    class_label = XmCreateLabel(_paletteW, "Classes:", NULL, 0);
    XtManageChild(class_label);
    XtVaSetValues(class_label, 
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget,     _oper1_frameW, 
		  NULL);

    _class_frameW = XmCreateFrame( _paletteW, "class_frame", NULL, 0);
    XtVaSetValues(_class_frameW,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget,     class_label,
		  NULL);

/*
 * create object button panel
 */
    obj_label = XmCreateLabel(_paletteW, "Objects:", NULL, 0);
    XtManageChild(obj_label);
    XtVaSetValues(obj_label, 
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget,     _class_frameW, 
		NULL);

/* 
 * load the classes palette ids 
 *  NOTE:  this also loads _palInfo.nclasses, so call this before using
 *  the _palInfo.nctrls value.  Clean this side effect up at some point! 
 */
    pgpalw_getBtnLocation (1, 0, 1, class_ids);

/* 
 * create a row column widget for the class buttons 
 */
    rows = 1;
    if ( _palInfo.nclasses > 1 ) {
        rows = (int)_palInfo.nclasses / icon_ncolumn; 
	if ( _palInfo.nclasses % icon_ncolumn ) {  
	    rows++;
	}
    } 

    _class_rcW = XmCreateRowColumn(_class_frameW,
			"rowcol2", NULL, 0);
    XtVaSetValues(_class_rcW,
		  XmNpacking,		XmPACK_COLUMN,
		  XmNorientation,	XmHORIZONTAL,
		  XmNadjustLast,	FALSE,
		  XmNnumColumns,	rows,
		  NULL);

/*
 * load the class buttons
 */
    for (ii=1; ii< (_palInfo.nclasses+1); ii++) {
	curr = class_ids[ii];
	nameptr = _palInfo.class[curr].name;
	sprintf(iconfile, "%s.xbm", nameptr);
	cfl_inqr (iconfile, ICON_DIR, &ignore, alt_iconfile, &ier2);

	bxm_info[0].sens_bits = alt_iconfile;
	bxm_info[1].sens_bits = alt_iconfile;

	_palInfo.class[curr].wid = 
	    (Widget) NxmBxmBtn_createMulti (_class_rcW, 
					    nameptr, BUTTON_TYPE, 
					    ICON_WDTH, ICON_HGHT, 
					    bxm_info, 2, 
					    nameptr, True, pgpalw_classCb, 
					    NULL, pxm_buff); 

	_palInfo.class[curr].pxms[0] = pxm_buff[0].snstv;
	_palInfo.class[curr].pxms[1] = pxm_buff[1].snstv;

/*
 * For BLANK button, make it insensitive.
 */
        if ( curr == CLASS_BLANK ) {

	   XtSetSensitive (_palInfo.class[curr].wid, FALSE);
        }

	XtVaSetValues ( _palInfo.class[curr].wid,
		       XmNshadowThickness,	1, 
		       XmNmarginLeft,		0,
		       XmNmarginWidth,		2,
		       XmNmarginHeight,		1,
                       XmNtraversalOn,          FALSE,
		       NULL);
    }

/*
 * Initialize _objpalsW
 */
    for ( ii = 0; ii < MAX_CLASSES; ii++ )  _objpalsW[ii] = NULL;

/*
 * load the object buttons for each class
 */
    for (ii = 1, jj = 1; ii < (_palInfo.nclasses+1); ii++) {

	curr = class_ids[ii];
	nameptr = _palInfo.class[curr].name;
	strcpy(mwname, nameptr); 

        sprintf(tbname, "%sbtn.tbl", nameptr);
        pgpalw_getBtnLocation (2, curr, jj, ids);

/* 
 * create a row column widget for the object buttons 
 */
        rows = 1;
        if ( _palInfo.nobjs[curr] > 1 ) {
	    rows = (int)_palInfo.nobjs[curr] / icon_ncolumn; 
	    if ( _palInfo.nobjs[curr] % icon_ncolumn ) {  
	        rows++;
	    }
        } 

        _objpalsW[curr] = XmCreateRowColumn(_paletteW, mwname, NULL, 0);

        XtVaSetValues(_objpalsW[curr], 
		      XmNpacking,		XmPACK_COLUMN,
		      XmNorientation,		XmHORIZONTAL,
		      XmNadjustLast,		False,
		      XmNnumColumns,		rows,
		      XmNtopAttachment,		XmATTACH_WIDGET,
		      XmNtopWidget,		obj_label,
		      XmNspacing,		1,
		      NULL);


        for (q=0; q < _palInfo.nobjs[curr]; q++, jj++) { 

	    curr2 = ids[jj];
	    nameptr = _palInfo.obj[curr2].name;
	    sprintf(iconfile, "%s.xbm", nameptr);
	    cfl_inqr (iconfile, ICON_DIR, &ignore, alt_iconfile, &ier2);

	    bxm_info[0].sens_bits = alt_iconfile;
	    bxm_info[1].sens_bits = alt_iconfile;

	    _palInfo.obj[curr2].wid = 
		(Widget) NxmBxmBtn_createMulti (_objpalsW[curr], 
						nameptr, BUTTON_TYPE, 
						ICON_WDTH, ICON_HGHT, 
						bxm_info, 2, 
						nameptr, True, pgpalw_objCb, 
						NULL, pxm_buff); 

	    _palInfo.obj[curr2].pxms[0] = pxm_buff[0].snstv;
	    _palInfo.obj[curr2].pxms[1] = pxm_buff[1].snstv;

/*
 * For BLANK button, make it insensitive.
 */
            if ( curr2 == OBJ_BLANK ) {

	       XtSetSensitive (_palInfo.obj[curr2].wid, FALSE);
            }

	    XtVaSetValues (_palInfo.obj[curr2].wid,
			   XmNshadowThickness,	1, 
			   XmNmarginLeft,	0,
			   XmNmarginWidth,	2,
			   XmNmarginHeight,	1,
                           XmNtraversalOn,      FALSE,
			   NULL);

   	    XtManageChild (_palInfo.obj[curr2].wid);

	    if( curr == CLASS_MET && curr2 == OBJ_GFA_P ) {
		_GFAp_Actv = True;
	    }
	}
    }

/* create a graphics context */
    pggst_initGc();

/*
 * load the group type information.
 * Note that this should be done before calling ces_rtbl().
 */
    ces_gtrtbl( &ier );

/* 
 * load the graphics settings information.
 */
    ces_rtbl(&ier);

    pghdlb_deselectAll ();

/* 
 * Create the sub forms that are part of the dialogs needed
 * for product generation...
 */
    pgtxt_create(canvas);
    NxmClrW_create(canvas);

    pgfilw_create(canvas);
    pgwndw_create(canvas);

/*
 * keep pggrpw_create() before pgline_create()
 * pgfrtw_create() and pgsymb_create()
 */
    pggrpw_create(canvas);
    pggrpch_create(canvas);
    pgline_create(canvas);
    pgcirc_create(canvas);
    pgfrtw_create(canvas);
    pgwbxw_create(canvas);
    pgwsmw_create(canvas);
    pgprd_create(canvas);
    pgsymb_create(canvas);
    pgsymb_createVolLst(canvas);
    pgnumb_create(canvas);
    pgtrkw_create(canvas);
    pgsigw_create(canvas);
    pgvolw_create(canvas);
    pgvolw_editCreate(canvas);
    pgvacw_create(canvas);
    pgofmt_create(canvas);
    pggfmt_create(canvas);
    pgccfw_create(canvas);
    pgccfp_create(canvas);
    pgtrk_create(canvas);
    pgwcnsl_create(canvas);
    pglayrw_create(canvas);
    pglpfw_create(canvas);
    pgextrap_create(canvas);
    pglist_create(canvas);
    pgjet_create(canvas);
    if( pgcycle_useCycle() ) {
        pgcycle_create( canvas );
    }
    pggfaw_create(canvas);
    pggfawp_create(canvas);
    pgtca_create(XtParent(mcanvw_getDrawingW()));
    pginterp_create(canvas);
    pgfilterw_create(canvas);
    pgsmear_create(canvas);
    pgfrom_create(canvas);
    pgairmet_create(canvas);
    pgairmetp_create(canvas);
    pgdist_create(canvas);
    
/*
 * initialize the current selection
 */
    _palInfo.cur_oper  = NONE_CURRENT;
    _palInfo.cur_class = NONE_CURRENT;
    _palInfo.cur_obj   = NONE_CURRENT;

    pgpalw_setBtnSntv (FUNC_SAVE_ALL, FALSE);    

    XtVaGetValues (_palInfo.oper[1].wid,
		   XmNtopShadowColor,	&_iconTShadow,
		   XmNbottomShadowColor,&_iconBShadow, 
		   NULL);

    return;
}

/*=====================================================================*/

void pgpalw_popup ( void )
/************************************************************************
 * pgpalw_popup								*
 *									*
 * Displays  the palette used in product generation.			*
 *                                                                      *
 * void pgpalw_popup()							*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi	10/96	Created 				*
 * E. Wehner/EAi	04/97	Cleanup 				*
 * E. Wehner/Eai	05/97	Split into submenus for classes/objs	*
 * E. Wehner/EAi	05/97	Save frames when loading PGen		*
 * E. Wehner/EAi	07/97	Check file header on .DEFAULT		*
 * E. Wehner/EAi	09/97	Kill grinfo record			*
 * C. Lin/EAI		10/97	rename from NxmPaletteSh, major cleanup *
 * E. Safford/GSC	02/98	add undo initalization			*
 * C. Lin/EAI		04/98	set _pgenFlag to True			*
 * E. Safford/GSC	05/98	update undo				*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * W. Li/EAI		08/98	Removed object value setting		*
 * E. Safford/GSC	09/98	manage _mode related widgets		*
 * E. Safford/GSC	10/98	exit pg when no WORK_FILE		*
 * E. Safford/GSC	12/98	update to new xwindow refresh design	*
 * S. Law/GSC		01/99	added call to pgwbxw_setWlst		*
 * E. Safford/GSC	03/99	lock down loop/zoom/roam during popup   *
 * S. Law/GSC		04/99	added calls to pgpalw__setButtonState	*
 * S. Jacobs/NCEP	 9/99	Open the WORK_FILE before the palette	*
 * E. Safford/GSC	12/99	xpgsvfrm -> xpgsvlp			*
 * E. Safford/GSC	03/00	add call to xpgsetpg			*
 * S. Law/GSC		05/00	removed "VGF:" from mesg string		*
 * E. Safford/GSC	06/00	add check if mapw_isUp			*
 * S. Law/GSC		06/00	removed loop parameter from xpgsvlp	*
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 * A. Hardy/GSC         01/01   Changed ifp from int to FILE            *
 * J. Wu/SAIC           02/02   reset layer information           	*
 * M. Li/SAIC		04/02	Added pglabel_setLabelPending		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * T. Piper/SAIC	09/06	Changed wrtflg to TRUE on cvg_open	*
 * E. Safford/SAIC	07/07   retitle window with default day/cycle	*
 ***********************************************************************/
{
    int 	numfrm, curpxm, ier;
    char	grp[4], mesg[128];
    Widget	draw_w;
    FILE        *ifp;
/*---------------------------------------------------------------------*/
/*
 * Initialize the range records & reset layer information.
 */
    crg_init(&ier);
    pglayer_init();

/*
 * Initialize the auto placement interface from cvg
 */
    cvg_initplace(&ier);

/*
 * Initialize the label pending
 */
    pglabel_setLabelPending(False);

/* 
 * Attempt to open the VGF file and close it again.  If it doesn't
 * exist or is the wrong version, handle it.
 */
    cvg_open(cvg_getworkfile(), TRUE, &ifp, &ier);
    if (ier < 0) {
/*
 *  can't find or create a WORK_FILE, exit here... 
 */
        NxmErr_update();

        draw_w = (Widget)mcanvw_getDrawingW();
        sprintf(mesg, 
              "Unable to create  %s  file -- no write permission", cvg_getworkfile());
	NxmWarn_show (draw_w, mesg); 

	return;
    }
    else {
	cvg_clos((FILE *)ifp, &ier);
    }

    mbtnw_zoomSensitive(False);
    loopw_sensitive(False);
    roamw_sensitive(False);

/*
 * initialize the current selection
 */
    pgpalw_setButtonState (LOT_OPER,  NONE_CURRENT);
    pgpalw_setButtonState (LOT_CLASS, NONE_CURRENT);
    pgpalw_setButtonState (LOT_OBJ,   NONE_CURRENT);

    pgwbxw_setWlst (-99, False);

    pgpalw_unmanageObjPal();

    strcpy(grp, "NXM");

    XtManageChild(_paletteW);
    XtManageChild(_modeFrameW);
    XtManageChild(_modeBoxW);
    XtManageChild(_oper0_frameW);
    XtManageChild(_oper1_frameW);
    XtManageChild(_oper0_rcW);
    XtManageChild(_oper1_rcW);
    XtManageChild(_class_frameW);
    XtManageChild(_class_rcW);
   
    pgundo_initUndo ();

/* set to do the load on all frames */
    xpgsetpg (TRUE);
    xqcpxm( &numfrm, &curpxm);

    xpgsvlp (&ier);

    xg2pxm(&curpxm, &ier);

    sprintf(mesg, " ");
    mbotw_pgfileSet(mesg);

    mbotw_enabledSet();
    pgpalw_setCurBtns(0, CLASS_ANY, 0);

    if (!mapw_isUp()) {
	mbtnw_zoomSensitive(True);
	loopw_sensitive(True);
	roamw_sensitive(True);
    }

    if( pgcycle_useCycle() ) {
	pgcycle_showCycleTime( &ier );
    }
}

/*=====================================================================*/

void pgpalw_popdown ( void )
/************************************************************************
 * pgpalw_popdown							*
 *									*
 * Pop down the palette window used in product generation.		*
 *                                                                      *
 * void pgpalw_popdown()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi    	10/96  	Created                                 *
 * C. Lin/EAI       	10/97	rename from NxmPaletteUnmanage		*
 * C. Lin/EAI       	09/98	add pgpalw_classPopdown & pggrpw_popdown*
 * E. Safford/GSC	03/00	add call to xpgsetpg			*
 * H. Zeng/EAI          05/01   added pggrpw_popdownGrpChngW            *
 * H. Zeng/EAI          10/01   removed pgprpw_popdown                  *
 * H. Zeng/EAI		03/02	modified to use pggrpch_popdown()	*
 * E. Safford/SAIC	07/07	use pgcycle_removeCycleTime() as needed *
 ***********************************************************************/
{

    if ( XtIsManaged(_paletteW) ) {
    	XtUnmanageChild(_paletteW);
    }
    pgpalw_classPopdown();
    pggrpch_popdown();
    xpgsetpg (FALSE);

    if( pgcycle_useCycle() ) {
	pgcycle_removeCycleTime( );
    }
}

/*=====================================================================*/

void pgpalw_setupOper ( void )
/************************************************************************
 * pgpalw_setupOper							*
 *									*
 * This function handles the state transition for going into the 	*
 * selected operation.							*
 *									*
 * void pgpalw_setupOper ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	Condensed from individual functions	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * C. Lin/EAI	        08/98	Hilight the operation button for ANY	*
 * C. Lin/EAI	        08/98	Add class ANY with FUNC_ROTATE 		*
 * C. Lin/EAI	        08/98	reorganize, add "ADD" state   		*
 * S. Law/GSC		08/98	Added FUNC_DELPOINT			*
 * S. Law/GSC		09/98	Added FUNC_CONNECT			*
 * G. Krueger/EAI	09/98	Correct MOUSESET hints			*
 * E. Safford/GSC	10/98	add disarmPress and end dynamics	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		11/98	add FUNC_NUMB_EDIT			*
 * S. Law/GSC		11/98	Seperated CLASS_WATCH / CLASS_PRODUCTS	*
 * E. Safford/GSC	11/98	add multi-select by drag		*
 * S. Law/GSC		11/98	Stopped deselect during FUNC_GROUP	*
 * W. Li/EAI		12/98	add CLASS_ANY for number editor		*
 * W. LI/EAI		05/99	add FUNC_DEL_OBJ			*
 * E. Safford/GSC	06/99	mod FUNC_DEL_OBJ hint messages		*
 * S. Law/GSC		05/99	added CLASS_SIGMETS			*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          05/00   added pgnumb_popup()                    *
 * E. Safford/GSC       05/00   removed pgnumb_popup()                  *
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * M. Li/SAIC		04/02	remove FUNC_SELECT while group is active*
 * J. Wu/SAIC		10/03	add CLASS_MET				*
 * E. Safford/SAIC	12/03	add FUNC_SMEAR				*
 * E. Safford/SAIC	12/03	remove mouse hints for smear setup	*
 * J. Wu/SAIC		12/03	keep EXTRAP window up for FUNC_EXTRAP	*
 * M. Li/SAIC		12/03	Added a check for FUNC_INC_DEC		*
 * M. Li/SAIC		01/04	Removed pgnumb_updateBtn		*
 * J. Wu/SAIC		10/03	adjust FUNC_SMEAR on CLASS_MET		*
 * J. Wu/SAIC		04/04	add FUNC_INTERP				*
 * E. Safford/SAIC	07/04	multi-select mode uses pgasel module    *
 * J. Wu/SAIC		03/05	add FUNC_SHOW_NONGRP			*
 * H. Zeng/SAIC		10/06	removed call to pgpalw_setCurBtns()	*
 * E. Safford/SAIC	03/07	add no-op conditions for FUNC_FROM	*
 * J. Wu/SAIC		09/07	add pgfrom_selectElmEh()		*
 * X. Guo/CWS		01/10   add FUNC_ADDPOINT			*
 ***********************************************************************/
{
    int		operation, class;
    Boolean     no_op;
/*---------------------------------------------------------------------*/

    mcanvw_disarmDynamic();    
    mcanvw_setDynActFlag(False); 

/*
 * Check if the operation is valid
 */
    operation	= pgpalw_getCurOperId();
    class	= pgpalw_getCurClassId();
    pgpalw_setCurObj ((Widget) NULL);

    if ( operation != FUNC_GROUP && !pgpalw_isGrpActv() && 
         operation != FUNC_INC_DEC) {
        pghdlb_deselectAll();
    }

    if ( operation == NONE_CURRENT || operation == FUNC_SHOW_GRPS ||
         operation == FUNC_SHOW_NONGRP ) {
	    no_op = TRUE;
    }
    else {
	if ((operation == FUNC_FLIP ||
	     operation == FUNC_MODIFY ||
	     operation == FUNC_PARTDELETE ||
	     operation == FUNC_DELPOINT ||
	     operation == FUNC_CONNECT ||
	     operation == FUNC_ADDPOINT)
             &&
             (class != CLASS_LINES &&
              class != CLASS_FRONTS &&
              class != CLASS_SIGMETS &&
	      class != CLASS_MET &&
              class != CLASS_ANY)) {
	    no_op = TRUE;
	}
	else if (operation == FUNC_MULTISEL && 
		 (class == CLASS_WATCHES ||
		  class == CLASS_PRODUCTS)) {
	    no_op = TRUE;
	}
	else if (operation == FUNC_ROTATE &&
             (class != CLASS_TEXT &&
              class != CLASS_WINDS &&
              class != CLASS_ANY)) {
	    no_op = TRUE;
	}
	else if (operation == FUNC_INC_DEC && 
  		 (class != CLASS_TEXT && class != CLASS_ANY)) { 
	    no_op = TRUE;
	}
	else if (operation == FUNC_DEL_OBJ && class == CLASS_PRODUCTS ) {
	    no_op = TRUE;
	}
	else if ( (operation == FUNC_SMEAR || operation == FUNC_INTERP ||
		   operation == FUNC_FROM ) &&
		(class != CLASS_LINES &&
		 class != CLASS_SIGMETS &&
		 class != CLASS_MET &&		 
		 class != CLASS_ANY)) {
	    no_op = TRUE;
	} 
	else {
	    no_op = FALSE;
	}
    }

    if ( no_op ) {
	mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
    }
    else {

/*
 * Arm the location function 
 */
	if ( operation == FUNC_MULTISEL || 
	     operation == FUNC_GROUP	||
	    (operation == FUNC_INC_DEC && pgpalw_getMode() == TYPE_OBJ) ) {

/*
 * start multi-select mode
 */
	    pgasel_start( &pgmsel_singlePtSel, 
	    		  &pgmsel_multiPtSel, 
	    		  &pgmsel_endMultiSel );
        }
	else if ( operation == FUNC_SMEAR ) {
	    mcanvw_setPressFunc( (XtEventHandler)&pgsmear_selectElmEh, CURS_DEFAULT);
	    pgsmear_startSmear();
        }
	else if ( operation == FUNC_FROM ) {
	    mcanvw_setPressFunc( (XtEventHandler)&pgfrom_selectElmEh, CURS_DEFAULT);
	    pgfrom_startSmear();
        }
	else if ( operation == FUNC_INTERP ) {
	    pginterp_popup();	    
	    mcanvw_setPressFunc( (XtEventHandler)&pginterp_selectElmEh, CURS_DEFAULT);
	    pginterp_startInterp();
        }
        else if ( pgpalw_isGrpActv() ) { 
	    mcanvw_setPressFunc((XtEventHandler)&pgevt_selectElmEh, CURS_DEFAULT);            
        }     
	else {
	    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
	}

	if ( operation != NONE_CURRENT ) {
	    mbotw_actionSet( pgpalw_getCurOperName() );
	}
	else {
	    mbotw_actionSet( ACHINT_ADD );
	}

	mbotw_classSet( pgpalw_getCurClassName() );


	if (class != NONE_CURRENT && operation != FUNC_UNDO) {
	    if (operation == FUNC_MULTISEL || operation == FUNC_GROUP) {
		mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_NOACTION);
	    }
	    else if (operation == FUNC_DEL_OBJ) {
		if (class == CLASS_ANY) {
		    mbotw_mouseSet(LMHINT_CLASSSELECT, MMHINT_EXIT);
		}
		else {
		    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
		}
	    }
	    else if ( operation == FUNC_SELECT) {
	        if ( pgpalw_isGrpActv() ) {
		    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
                }
                else {
		    mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
	        }
	    }
	    else if ( operation == FUNC_EXTRAP ) {		
		pgextrap_popup ();
		mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
	    }	    
	    else {
		mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
	    }
	}
    }
}

/*=====================================================================*/

void pgpalw_classPopdown ( void )
/************************************************************************
 * pgpalw_classPopdown							*
 *									*
 * This pops down the object palette for each class			*
 *									*
 * void pgpalw_classPopdown()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	initial copying				*
 * C. Lin/EAI		09/98	remove pggrpw_popdown			*
 * W. Li/EAI		12/98	added pgnumb_popdown			*
 * S. Law/GSC		12/98	added pgwlst_popdown			*
 * S. Law/GSC		01/99	added pgwfmt_popdown			*
 * W. Li/EAI		01/99	renamed NxmTxtA_XXX --> pgtxt_XXX	*
 * S. Law/GSC		05/99	added pgtrkw_popdown			*
 * E. Safford/GSC	06/99	mod handling of FUNC_DEL_OBJ		*
 * S. Law/GSC		07/99	added pgsigw_popdown and pgsigf_popdown	*
 * S. Law/GSC		08/99	removed pgsigf_popdown			*
 * H. Zeng/EAI          09/99   added pgofmt_popdown()                  *
 * H. Zeng/EAI          10/99   added pggfmt_popdown()                  *
 * S. Law/GSC		05/99	added pgwsmw_popdown			*
 * S. Law/GSC		02/00	added pgccfw_popdown			*
 * S. Law/GSC		03/00	added pgccfp_popdown			*
 * M. Li/GSC		04/00	added pgtrk_popdown			*
 * A. Hardy/GSC		05/00   added pgwcnsl_popdown			*
 * H. Zeng/EAI          11/01   added pgwfmt_popdownWCC                 *
 * E. Safford/SAIC	12/01	add if *_isUp checks to each call       *
 * T. Lee/SAIC		04/02	added pglpfw_popdown			*
 * M. Li/SAIC		05/02	added pgtrk_isUp			*
 * E. Safford/SAIC	06/02	move check pgwfmt_popdownWCC		*
 * E. Safford/SAIC      06/02   added pgwfmt_popdownWCL                 *
 * J. Wu/SAIC		10/02	add pgextrap_popdown			*
 * J. Wu/SAIC		11/02	add pglist_popdown			*
 * R. Tian/SAIC		01/03	add pgpalw_manageObjPal			*
 * S. Jacobs/NCEP	 3/03	Add check for pgpalw_isUp before	*
 *					pgpalw_manageObjPal		*
 * H. Zeng/XTRIA	07/03   added pgvolw_popdown			*
 * H. Zeng/XTRIA	08/03	modified pgvolw_editPopdown()		*
 * H. Zeng/XTRIA	09/03   added pgvacw_popdown()			*
 * J. Wu/SAIC		11/03	add pgjet_popdown			*
 * J. Wu/SAIC		11/03	add pggfaw_popdown			*
 * B. Yin/SAIC		03/04	added pgtca_popdown			*
 * J. Wu/SAIC		04/04	add pginterp_popdown			*
 * J. Wu/SAIC		07/04	add pgsmear_popdown			*
 * B. Yin/SAIC		12/04	add pgairmet_popdown			*
 * H. Zeng/SAIC		07/06	add pgdist_popdown			*
 * H. Zeng/SAIC		04/07	added call to pgofmt_bulkProcessEnd	*
 * E. Safford/SAIC	04/07	add pggfawp_popdown			*
 * J. Wu/SAIC		09/07	add pgfrom_popdown			*
 ***********************************************************************/
{
/*
 * Before pop down any popup, make sure the "pgplette" is managed 
 * properly so that it can refresh itself.
 */
    if ( pgpalw_isUp() ) pgpalw_manageObjPal(_palInfo.cur_class);

    if ( pgwndw_isUp() ) pgwndw_popdown(); 
    if ( pgline_isUp() ) pgline_popdown();
    if ( pgcirc_isUp() ) pgcirc_popdown();
    if ( pgfrtw_isUp() ) pgfrtw_popdown();
    if ( pgwbxw_isUp() ) pgwbxw_popdown();

    if ( pgwlst_isUp() ) {
        pgwfmt_popdownWCL();
        pgwfmt_popdownWCC();
        pgwlst_popdown();
    }

    if ( pgwfmt_isUp() ) pgwfmt_popdown();

    if ( pgofmt_isUp() ) {
        pgofmt_bulkProcessEnd(); 
	pgofmt_popdown();
    }

    if ( pggfmt_isUp() ) pggfmt_popdown();
    if ( pgfilw_isUp() ) pgfilw_popdown();
    if ( pglpfw_isUp() ) pglpfw_popdown();
    if ( pgsymb_isUp() ) pgsymb_popdown();
    if ( pgnumb_isUp() ) pgnumb_popdown();
    if ( pgtxt_isUp()  ) pgtxt_popdown();
    if ( pgtrkw_isUp() ) pgtrkw_popdown();
    if ( pgsigw_isUp() ) pgsigw_popdown();
    if ( pgvolw_isUp() ) pgvolw_popdown();

/*
 * Don't put if ( pgvolw_editIsUp() ) below.
 */
    pgvolw_editPopdown();

    if ( pgvacw_isUp() ) pgvacw_popdown();
    if ( pgwsmw_isUp() ) pgwsmw_popdown();
    if ( pgccfw_isUp() ) pgccfw_popdown();
    if ( pgccfp_isUp() ) pgccfp_popdown();
    if ( pgtrk_isUp() )  pgtrk_popdown();
    if ( pgwcnsl_isUp()) pgwcnsl_popdown();
    if ( pgextrap_isUp() && pgpalw_getCurOperId() != FUNC_EXTRAP ) 
        pgextrap_popdown();
    if ( pglist_isUp()) pglist_popdown();
    if ( pgjet_isUp()) pgjet_popdown();
    if ( pggfaw_isUp()) pggfaw_popdown();
    if ( pggfawp_isUp()) pggfawp_popdown();
    if ( pgtca_isUp()) pgtca_popdown();
    if ( pginterp_isUp() && pgpalw_getCurOperId() != FUNC_INTERP )
        pginterp_popdown();
    if ( pgsmear_isUp())  pgsmear_popdown();
    if ( pgfrom_isUp())  pgfrom_popdown();
    if ( pgairmet_isUp()) pgairmet_popdown();
    if ( pgdist_isUp())   pgdist_popdown();
 
}

/*=====================================================================*/

void pgpalw_setCurBtns ( int oper, int class, int obj )
/************************************************************************
 * pgpalw_setCurBtns							*
 *									*
 * Sets the current operation, class, and object buttons.		*
 *									*
 * void pgpalw_setCurBtns (oper, class, obj)				*
 *									*
 * Input parameters:							*
 *	oper		int	current operation			*
 *	class		int	current class				*
 *	obj		int	current object				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	initial coding				*
 * G. Krueger/EAI	05/99	Added parameter declaration		*
 ***********************************************************************/
{

    if (oper == 0) {
	pgpalw_setCurOper ((Widget) NULL);
    }
    else if (oper > 0) {
	pgpalw_setCurOper (_palInfo.oper[oper].wid);
    }

    if (class == 0) {
	pgpalw_setCurClass ((Widget) NULL);
    }
    else if (class > 0) {
	pgpalw_setCurClass (_palInfo.class[class].wid);
    }

    if (obj == 0) {
	pgpalw_setCurObj ((Widget) NULL);
    }
    else if (obj > 0) {
	pgpalw_setCurObj (_palInfo.obj[obj].wid);
    }
}

/*=====================================================================*/

void pgpalw_setPrevOper ( void )
/************************************************************************
 * pgpalw_setPrevOper							*
 *									*
 * This function reset the previous oper selection.		        *
 *									*
 * void pgpalw_setPrevOper ( )						*
 *									*
 * Input  parameters:                                                   *
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/01   initial coding                          *
 * J. Wu/SAIC           03/02   check pgpalw_isUp() before reset        *
 ***********************************************************************/
{
   Widget      prev_wid;
/*---------------------------------------------------------------------*/
/*
 * Reset to previous oper selection.
 */
   if ( pgpalw_isUp() ) {
       prev_wid = (Widget)pgpalw_getPrevOperWid();
       pgpalw_setCurOper(prev_wid);
       pgpalw_setupOper();
   }
}

/*=====================================================================*/
/* ARGSUSED */
void pgpalw_exit ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_exit								*
 *									*
 * This function handles the exits from product generation mode. 	*
 *									*
 * void pgpalw_exit (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *      wid             Widget          widget ID                       *
 *      clnt		XtPointer       not used                       	*
 *      cbs            XtPointer       callback struct                 *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC         2/97                                           *
 * E. Wehner/EAi         4/97    Refresh when exiting                   *
 * C. Lin/EAi            7/97    Show current pixmap when exiting       *
 * E. Wehner/EAi         8/97   Remove watch by county variables        *
 * C. Lin/EAi            7/97    Call xputpxms()                        *
 * E. Wehner/EAi         9/97   Remove graphics info record             *
 * C. Lin/EAI           10/97	rename from NxmDrawExitCb, cleanup	*
 * C. Lin/EAI           12/97	add call to cvgrndef()			*
 * E. Safford/GSC	01/98	add call to pgfilw_clearFileName()      *
 * S. Law/GSC		04/98	add call to pghdlb_deselectAll()	*
 * E. Safford/GSC	05/98	update undo                             *
 * E. Safford/GSC	09/98	clean up                                *
 * E. Safford/GSC	12/98	fix refresh curpxm problem              *
 * W. Li/EAI		05/99	added call to mbotw_disabledSet()	*
 * E. Safford/GSC 	10/99	enable loop  buttons on exit		*
 * E. Safford/GSC	12/99	xputpxms -> xpgrestlp			*
 * E. Safford/GSC	03/00	remove crg_init (moved to pgpalw_popup) *
 * S. Law/GSC		05/00	removed mbtnw_ calls			*
 * S. Law/GSC		06/00	removed loop parameter from xpgrestlp	*
 * H. Zeng/EAI          09/00   added pgfilw_clearSaveFlag()            *
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * E. Safford/SAIC	08/01	add xmloop_switchLoop() to fix 463:P2	*
 * T. Piper/SAIC	10/01	corrected callback parameter list	*
 * E. Safford/SAIC	11/01	fix burn-in w/ mapw up (trk# 53)     	*
 * J. Wu/SAIC	   	12/01   remove uncessary xpgrfrsh()		*
 * H. Zeng/EAI          02/02   added LAYER active check                *
 * E. Safford/SAIC	03/02	use pglayrw_exit to exit layering	*
 * J. Wu/SAIC	   	03/02   add check to _exitPGEN flag		*
 * J. Wu/SAIC	   	03/02   reset changes_made in single-layer mode	*
 * H. Zeng/XTRIA        12/02   used xpxm2win() to refix 463:P2         *
 * T. Piper/SAIC	07/03	added XtDisplay and XtWindow		*
 * J. Wu/SAIC		07/04	use pgfilterw_popdown to exit filtering	*
 ***********************************************************************/
{
    Cardinal	width, height;
    Widget	draw_w;
    int		ier;
    char	wname[256];
/*---------------------------------------------------------------------*/
/*
 * If _exitPGEN is TRUE, exit and reset. Otherwise, do not exit.
 */
    if ( _exitPGEN ) {
       pgpalw_setExitPGEN ( FALSE );
    }
    else {
	 return;
    }

/*
 * If LAYER is active, inactivate it.
 */
    if ( pglayrw_isUp() ) {
	pglayrw_exit();
    }
    else { /* Reset flags for single layer mode. */
        pglayer_setChngMade ( 0, FALSE );    
    }

/*
 * If FILTER is active, pop it down.
 */
    if ( pgfilterw_isUp() ) {
	pgfilterw_popdown();
    }

/*
 * Unmanage the palette and other popup windows
 */
    pgpalw_popdown();

    if (mapw_isUp()) {
        strcpy(wname, MCANVW_NAME);
        gslwin(wname, &ier, strlen(wname));
        if ( ier != 0 ) {
            NxmErr_update();
        }
    }

/*
 * Query the size of the pixmap window to copy.
 */
    draw_w = (Widget)mcanvw_getDrawingW();
    XtVaGetValues(draw_w, XmNwidth, &width, XmNheight, &height, NULL);
    XClearArea(XtDisplay(draw_w), XtWindow(draw_w), 0, 0, width, height, False);

/*
 * Copy each of the saved frames to the current window.
 */
    xpgrestlp ();

/*
 *  Order refresh (redisplay) of current pixmap
 */
    xpxm2win (-1, -1, &ier);

/*
 * clear the active element record
 */
    pgactv_clearActv();

/*
 * Clear and reset the selected elms, range record, and WORK_FILE
 */
    pghdlb_deselectAll ();
    cvg_rndef();

    pgfilw_clearFileName();
    pgfilw_showFileName();
    pgfilw_clearSaveFlag();

    mbotw_disabledSet();

    if (mapw_isUp()) {
        strcpy(wname, MAPW_NAME);
        gslwin(wname, &ier, strlen(wname));
        if ( ier != 0 ) {
            NxmErr_update();
	}
    }        
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_cancelExit ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_cancelExit							*
 *									*
 * This is the callback function for "Cancel" button on the pgen exit   *
 * confirmation window.                                                 *
 *									*
 * static void pgpalw_cancelExit (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *      wid             Widget          widget ID                       *
 *      clnt		XtPointer       not used                       	*
 *      cbs            XtPointer       callback struct                 *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/01   initial coding                          *
 * T. Piper/SAIC	10/01	corrected callback parameter list	*
 * J. Wu/SAIC		02/02	pop down exit window			*
 * J. Wu/SAIC		03/02	reset _exitPGEN flag			*
 * E. Safford/SAIC	03/02	use pgpalw_setupOper, not setPrevOper	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
   
   pgpalw_setExitPGEN ( FALSE );
   
/*
 * Reset oper selection.
 */
   pgpalw_setupOper();
}

/*=====================================================================*/

void pgpalw_exitCheck ( Widget w )
/************************************************************************
 * pgpalw_exitCheck                                             	*
 *                                                                      *
 * Callback function for EXIT button on the product generation window	*
 *                                                                      *
 * void pgpalw_exitCheck(w)                   				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w       Widget     parent widget ID                                 *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *									*
 * W.Li/EAI		05/99						*
 * H. Zeng/EAI  	02/01   added new para. to NxmExit_create()     *
 * J. Wu/SAIC   	02/02   redesign with layering & save capability*
 * J. Wu/SAIC   	03/02   unify with exiting from layering	*
 * E. Safford/SAIC	03/02	move to SELECT in case of cancel	*
 ***********************************************************************/
{
    pgpalw_setExitPGEN ( TRUE );

    pgpalw_setCurBtns (FUNC_SELECT, -1, -1);


    if ( pglayer_getChngLayer (0) < 0 ) {	
	NxmExit_create(w, "Exit Confirmation", 
                       "OK to EXIT from product generation?",
		       pgpalw_exit, pgpalw_cancelExit );          
    }
    else {
	pglayrxt_popup ();
    }
}

/*=====================================================================*/

void pgpalw_setBtnSntv ( int btn_id, Boolean btn_snstv )
/************************************************************************
 * pgpalw_setBtnSntv                                                    *
 *                                                                      *
 * This function makes the selected button sensitive/insensitve         *
 *                                                                      *
 * void pgpalw_setBtnSntv (btn_id, btn_snstv)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	btn_id		int             button id                       *
 *	btn_snstv	Boolean         True  = button sensitive        *
 *                                      False = button insensitive      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       09/98   initial coding                          *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/XTRIA        12/02   fixed the UNDO btn expansion bug.       *
 * T. Piper/SAIC	04/05	removed XmVERSION check			*
 * S. Jacobs/NCEP	 3/06	Add check on widget before change snstv	*
 ***********************************************************************/
{
    Boolean   ori_state;
/*---------------------------------------------------------------------*/

    if  ( _palInfo.oper[btn_id].wid )  {
    ori_state = (Boolean)(XtIsSensitive(_palInfo.oper[btn_id].wid) ? TRUE : FALSE);

    if ( ori_state == btn_snstv ) {
         return;
    }

    XtSetSensitive (_palInfo.oper[btn_id].wid, (int)btn_snstv);
    }
}

/*=====================================================================*/

void pgpalw_dsplyObjPal ( char vg_class )
/************************************************************************
 * pgpalw_dsplyObjPal							*
 *									*
 * This function starts the appropriate object palette for a given      *
 * vg_class value.  The widgets are ordered by position within          *
 * classfuncbtn.tbl, while the vg_class of elements comes from drwids.h *
 *									*
 * void pgpalw_dsplyObjPal (vg_class)					*
 *									*
 * Input parameters:							*
 *    vg_class 		char	class value defined in drwids.h        	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	04/98	initial coding                          *
 ***********************************************************************/
{
    if (vg_class <= MAX_CLASSES) {
	if (vg_class != CLASS_ANY)
            pgpalw_manageObjPal((int)vg_class);
	_palInfo.cur_class = (int)vg_class;
    }
}

/*=====================================================================*/

void pgpalw_rfrshObjPal ( void )
/************************************************************************
 * pgpalw_rfrshObjPal                                                   *
 *                                                                      *
 * This function refreshes the object palette by unmanagaging and       *
 * managing it.                                                         *
 *                                                                      *
 * void pgpalw_rfrshObjPal ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/98   initial coding                          *
 * C. Lin/EAI           08/98   add palette window checking             *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/XTRIA	12/03   added call to XmUpdateDisplay()		*
 ***********************************************************************/
{
    if ( pgpalw_isUp() ) {

        pgpalw_dsplyObjPal ((char)_palInfo.cur_class);
        XmUpdateDisplay(_paletteW);
    }
}

/*=====================================================================*/

void pgpalw_unmanageObjPal ( void )
/************************************************************************
 * pgpalw_unmanageObjPal						*
 *									*
 * Undisplays  the palette used in product generation.			*
 *                                                                      *
 * void pgpalw_unmanageObjPal( )					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 * 			None						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi    5/97   	Created                                 *
 * E. Wehner/EAi    9/97	Kill grinfo record			*
 * C. Lin/EAI      10/97	rename NxmObjPalUnmanage, minor clean up*
 * C. Lin/EAI      08/98	add update display			*
 * H. Zeng/SAIC	   06/06	deleted update display			*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    for (ii = 1; ii< MAX_CLASSES; ii++) {
        if (_objpalsW[ii] && XtIsManaged(_objpalsW[ii])) {
	    XtUnmanageChild(_objpalsW[ii]);
	}
    }
}

/*=====================================================================*/

void pgpalw_deleteAll ( void )
/************************************************************************
 * pgpalw_deleteAll  							*
 *									*
 * This function marks all records in the VG file as deleted and clears *
 * the drawing area of VG elements.					*
 *									*
 * void pgpalw_deleteAll ( )                       			*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * E. Safford/GSC	02/00	moved functionality from _deleteAllCb 	*
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC		11/01	remove redundant geplot calls		*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC	   	12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	   	12/01   remove cvg_load()			*
 * J. Wu/SAIC	   	02/02   redraw elements & remove crg_init()	*
 * E. Safford/SAIC	04/02	param change crg_clearLayer()		*
 * T. Piper/SAIC	07/03	added XtDisplay and XtWindow		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int		ier, cur_layer;
    Cardinal	width, height;
    Widget	draw_w;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer ();

/*
 * Mark all VG records as deleted.
 */
    pgdel_deletAll( &ier );

/*
 * Clear the range records for current layer.
 */
    pghdlb_deselectAll ();
    crg_clearLayer ( cur_layer, &ier );    
   
/*
 * Clear the screen and refresh pixmaps.
 */
    draw_w = (Widget)mcanvw_getDrawingW();
    XtVaGetValues(draw_w,
                XmNwidth,  &width,
                XmNheight, &height,
                NULL);

    XClearArea(XtDisplay(draw_w), XtWindow(draw_w),
                0,0, width, height, False);

    xpgrestlp ();

    cvg_redraw ( cvg_getworkfile(), &ier); /* Redraw elem. on other layers */
    xpgrfrsh();
    
    crg_rebuild ();
    
}

/*=====================================================================*/

void pgpalw_rmDefVGF ( void )
/************************************************************************
 * pgpalw_rmDefVGF 							*
 *									*
 * This function save the current .DEFAULT.vgf file as .DEFAULT.vgf.save*
 * erases .DEFAULT.vgf file in the system and clears the drawing area   *
 * of VG elements.							*
 *									*
 * void pgpalw_rmDefVGF ( )                       			*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	04/03   initial coding				*
 * T. Piper/SAIC	07/03	added XtDisplay and XtWindow		*
 ***********************************************************************/
{
    int		ier;
    Widget	draw_w;
    Cardinal	width, height;
/*---------------------------------------------------------------------*/

    pgactv_clearActv();  
    pghdlb_deselectAll ();
    cvg_rndef ();
    crg_init  ( &ier );

/*
 * Clear the screen and refresh pixmaps.
 */
    draw_w = (Widget)mcanvw_getDrawingW();
    XtVaGetValues(draw_w,
                  XmNwidth,  &width,
                  XmNheight, &height,
                  NULL);

    XClearArea(XtDisplay(draw_w), XtWindow(draw_w),
               0,0, width, height, False);

    xpgrestlp ();
    
}

/*=====================================================================*/

Boolean pgpalw_isUp ( void )
/************************************************************************
 * pgpalw_isUp								*
 *									*
 * Returns a boolean value regarding whether the palette is up		*
 * (managed) or down (unmanaged).					*
 *                                                                      *
 * Boolean pgpalw_isUp( )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	None.								*
 *                                                                      *
 * Output parameters:                                                   *
 *  none								*
 *									*
 * Return parameters:							*
 *	pgpalw_isUp		Boolean		Prod Generation status	*
 *						  1 = Palette managed.	*
 *						  0 = Palette unmanaged	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi    9/97   	Created                                 *
 * C. Lin/EAI      10/97   	rename from NxmPaletteUp                *
 * G. Krueger/EAI   5/99	Cleanup prologue			*
 ***********************************************************************/
{
    return (XtIsManaged(_paletteW) );
}

/*=====================================================================*/

char *pgpalw_getCurOperName ( void )
/************************************************************************
 * pgpalw_getCurOperName						*
 *									*
 * Returns a string containing the name of the current active		*
 * operation.								*
 *									*
 * char* pgpalw_getCurOperName()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *	*pgpalw_getCurOperName	char	current operation name		*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    return( _palInfo.oper[pgpalw_getCurOperId()].name);
}

/*=====================================================================*/

char *pgpalw_getCurClassName ( void )
/************************************************************************
 * pgpalw_getCurClassName						*
 *                                                                      *
 * Returns a string containing the name of the current active class.	*
 *                                                                      *
 * char* pgpalw_getCurClassName()                                       *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  			NONE						*
 *                                                                      *
 * Return parameters:                                                   *
 * 	*pgpalw_getCurClassName	char	current class name		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAi		10/97	based on NxmPaletteClnam		*
 * C. Lin/EAi		11/97	bug fix					*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    return( _palInfo.class[pgpalw_getCurClassId()].name);
}

/*=====================================================================*/

int pgpalw_getCurOperId ( void )
/************************************************************************
 * pgpalw_getCurOperId                                                  *
 *                                                                      *
 * This function gets the FUNC_XXXX ID of the current selected 		*
 * operation.  								*
 *                                                                      *
 * int pgpalw_getCurOperId ()                                          	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 * Return value:                                                        *
 * 	pgpalw_getCurOperId	int	current oper ID (FUNC_XXXX )	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI        10/97                   				*
 * C. Lin/EAI        08/98   modified to be able to return NONE_CURRENT *
 * G. Krueger/EAI    05/99   Cleanup prologue				*
 ***********************************************************************/
{
    if (_palInfo.cur_oper >= 0 )
	return (_palInfo.cur_oper);
    else 
	return ( FUNC_SELECT );
}

/*=====================================================================*/

Widget pgpalw_getOperWid ( int id )
/************************************************************************
 * pgpalw_getOperWid            	                                *
 *                                                                      *
 * This function gets the Widget ID of the selected operation.		*
 *                                                                      *
 * Widget pgpalw_getOperWid ( id )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *	id		int	Operation id				*
 *                                                                      *
 * Output parameters:                                                   *
 *				NONE					*
 * Return value:                                                        *
 * 	pgpalw_getOperWid	Widget	Oper widget id.      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/SAIC       04/02   initial coding                             *
 ***********************************************************************/
{
    return ( _palInfo.oper[id].wid);
}
/*=====================================================================*/

Widget pgpalw_getPrevOperWid ( void )
/************************************************************************
 * pgpalw_getPrevOperWid                                                *
 *                                                                      *
 * This function gets the Widget ID of the previous selected operation. *
 *                                                                      *
 * Widget pgpalw_getPrevOperWid ()                                      *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 * Return value:                                                        *
 * 	pgpalw_getPrevOperWid	Widget	prev. oper widget id.           *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI       02/01   initial coding                             *
 ***********************************************************************/
{
    return ( _prevOperWid );
}

/*=====================================================================*/

Widget pgpalw_getObjWid ( int id )
/************************************************************************
 * pgpalw_getObjWid							*
 *                                                                      *
 * This function gets the Widget ID of the selected object.		*
 *                                                                      *
 * Widget pgpalw_getObjWid ( id )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	id		int	Object id				*
 *                                                                      *
 * Output parameters:                                                   *
 *				NONE					*
 * Return value:                                                        *
 * 	pgpalw_getObjWid	Widget	Object widget id      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC	     04/07	initial coding				*
 ***********************************************************************/
{
    return ( _palInfo.obj[id].wid);
}

/*=====================================================================*/

int pgpalw_getCurClassId ( void )
/************************************************************************
 * pgpalw_getCurClassId                                                 *
 *                                                                      *
 * This function gets the CLASS_XXXX ID of the current selected class.  *
 *                                                                      *
 * int pgpalw_getCurClassId()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      		NONE                   				*
 * Return value:                                                        *
 * 	pgpalw_getCurClassId	int	current class ID (CLASS_XXXX)	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI        10/97                   				*
 * G. Krueger/EAI    05/99   Cleanup prologue				*
 ***********************************************************************/
{
    if (_palInfo.cur_class > 0)
	return (_palInfo.cur_class);
    else 
	return ( NONE_CURRENT );
}

/*=====================================================================*/

int pgpalw_getCurObjId ( void )
/************************************************************************
 * pgpalw_getCurObjId							*
 *									*
 * This function gets the OBJ_XXXX ID of the current selected object.	*
 *									*
 * int pgpalw_getCurObjId()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return value:							*
 *	pgpalw_getCurObjId	int	current object ID (OBJ_XXXX ID)	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * S. Law/GSC		05/98	Updated _palInfo structure		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    if (_palInfo.cur_obj > 0) {
	return (_palInfo.cur_obj);
    }
    else 
	return ( NONE_CURRENT );
}

/*=====================================================================*/

char pgpalw_getMode ( void )
/************************************************************************
 * pgpalw_getMode                                                       *
 *                                                                      *
 * This function returns the mode except if FUNC_GROUP or UNGROUP is    *
 * selected, in which case it always returns TYPE_GRP.                  *
 *                                                                      *
 * char pgpalw_getMode()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      NONE                                                            *
 *                                                                      *
 * Return value:                                                        *
 *	pgpalw_getMode	char		current value of _palType	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       09/98   initial coding                          *
 * E. Safford/GSC	04/99	fix irix6 compile warning		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
char	ret_val;
/*---------------------------------------------------------------------*/

    if (_palMode == TYPE_GRP) {
        ret_val = _palMode;
    }
    else {
        switch (pgpalw_getCurOperId()) {
            case FUNC_GROUP:
            case FUNC_UNGROUP:
                ret_val = TYPE_GRP;
                break;

            default:
                ret_val = _palMode;
                break;
        }
    }
    return (ret_val);
}

/*=====================================================================*/
/* ARGSUSED */
void pgpalw_operCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_operCb							*
 *									*
 * Callback function for function buttons on the drawing palette.	*
 *									*
 * void pgpalw_operCb( w, clnt, cbs )					*
 *									*
 * Input parameters:							*
 *	w		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAI	04/97		Created				*
 * E. Wehner/EAi	05/97	Added command structure to grinfo	*
 * S. Jacobs/NCEP	06/97	Added write flag of TRUE to cvg_load	*
 * D. Keiser/GSC	06/97	Change MAX_DRAWBTN to MAX_OPERS		*
 * D. Keiser/GSC	06/97	Added FUNC_DELALL			*
 * E. Wehner/EAI	06/97	Handle wind barb form			*
 * D. Keiser/GSC	06/97	Added FUNC_FLIP				*
 * E. Safford/GSC	06/97	Handle special text and s/w text	*
 * E. Wehner/EAi	07/97	Handle grouping dialog			*
 * E. Wehner/EAi	07/97	Added rotate element			*
 * E. Safford/GSC	07/97	Added last 7 special text types		*
 * E. Wehner/EAi	07/97	Added lines setting dialog		*
 * E. Wehner/EAi	07/97	Added front setting dialog		*
 * D. Keiser/GSC	08/97	Make file selection dialog generic	*
 * C. Lin/EAI	 	08/97	Call xputpxms()				*
 * E. Safford/GSC	09/97	Replaced grp->cmd with cmd_ routines    *
 * E. Wehner/EAi	09/97	Remove graphics infor record		*
 * C. Lin/EAI	 	10/97	Major cleanup 				*
 * C. Lin/EAI	 	10/97	add pgwbxw_popdown() 			*
 * E. Safford/GSC	11/97	add MODIFY function                     *
 * C. Lin/EAI		12/97	add SAVE/SAVE_AS, RESTORE functions     *
 * E. Safford/GSC	01/98   add UNDO function                       *
 * E. Safford/GSC	01/98   add PARTIAL DELETE function             *
 * E. Safford/GSC	03/98   add MULTIPLE SELECT remove UNSELECT     *
 * C. Lin/EAI		04/98   modify for new grouping function     	*
 * E. Safford/GSC	04/98	Add function ungroup			*
 * C. Lin/EAI		04/98   add LABEL function     			*
 * S. Law/GSC		05/98	Changed to use pgpalw_setupOper		*
 * S. Law/GSC		05/98	Changed to use pgpalw_classPopdown	*
 * E. Safford/GSC	06/98	remove check on _labelTxtW		*
 * E. Safford/GSC	06/98	set GROUP to activate locateElmCb	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * S. Law/GSC		08/98	Added FUNC_DELPOINT			*
 * C. Lin/EAI	        09/98	modify the logic in GROUP/UNGROUP	*
 * S. Law/GSC		09/98	Added FUNC_CONNECT			*
 * C. Lin/EAI           09/98   unmange obj menu for GROUP/UNGROUP      *
 * S. Law/GSC		09/98	Added FUNC_UNGRPALL			*
 * E. Saffford/GSC	09/98	disallow labeling of COMSYMs		*
 * E. Saffford/GSC	09/98	clean up                    		*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		11/98	add FUNC_NUMB_EDIT			*
 * W. Li/EAI		12/98	Set CLASS_TEXT as number edit default	*
 * E. Safford/GSC	12/98	clean up REFRESH			*
 * E. Safford/GSC	12/98	modify startup conditions on INC_DEC	*
 * E. Safford/GSC	01/99	add FUNC_SHOW_GRPS                  	*
 * W. Li/EAI		02/99	fixed a label problem			*
 * E. Safford/GSC	04/99	fix call to NxmConfirm_show         	*
 * W. LI/EAI		05/99	add FUNC_DEL_OBJ			*
 * W. Li/EAI		05/99	add button setting for FUNC_DEL_OBJ	*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * E. Safford/GSC	12/99	xputpxms -> xpgrestlp			*
 * S. Law/GSC		05/00	pgfilw_svConfirmCb -> _saveAcceptCb	*
 * S. Law/GSC		06/00	removed loop parameter from xpgrestlp	*
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * H. Zeng/EAI          02/01   reset to prev. oper. selection for file *
 *                              ctrl buttons                            *
 * H. Zeng/EAI          05/01   added FUNC_CHNG_GRPS                    *
 * E. Safford/SAIC	09/01   mv refresh code to pgpalw_refresh()	*
 * H. Zeng/EAI          10/01   revised group functionality             *
 * H. Zeng/EAI          02/02   added FUNC_LAYER callback               *
 * J. Wu/SAIC           03/02   add layer with pglayrw_addLayer()	*
 * H. Zeng/EAI          03/02   added call to pgevt_unsetOper()         *
 * H. Zeng/EAI		03/02	modified to use pggrpch_popup()		*
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * T. Lee/SAIC		04/02	added FUNC_OPEN_PROD; removed message	*
 * M. Li/SAIC		04/02	Removed multiselect while group is up	*
 * T. Lee/SAIC		05/02	Layer window remained up when OPEN PROD	*
 * J. Wu/SAIC           10/02   add FUNC_EXTRAP				*
 * J. Wu/SAIC           12/02   add param into pgpalw_actvLayer()	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	11/03	add FUNC_SMEAR				*
 * J. Wu/SAIC           04/04   add FUNC_INTERP				*
 * E. Safford/SAIC	07/04	modified MULTI_SELECT to use pgasel     *
 * J. Wu/SAIC           07/04   add FUNC_FILTER				*
 * J. Wu/SAIC           03/05   add FUNC_SHOW_NONGRP			*
 * H. Zeng/SAIC		06/06   removed pgpalw_unmanageObjPal		*
 * H. Zeng/SAIC		07/06   add FUNC_DISTANCE			*
 * B. Yin/SAIC		11/06	reset to previous oper after refrersh	* 
 * E. Safford/SAIC	03/07	add FUNC_FROM				*
 * J. Wu/SAIC		09/07	add actual functionality to FUNC_FROM	*
 * X. Guo/CWS		01/10   add FUNC_ADDPOINT			*
 ***********************************************************************/
{
    int		ier, operation, class;
    char	mesg[128];
/*---------------------------------------------------------------------*/
/*
 * set the current operation based on the selected operation 
 * widget ID 
 */
    pgpalw_setCurOper (w);

/*
 * switch through the function retrieved from the button click 
 * and take appropriate action.
 */
    operation = pgpalw_getCurOperId();
    class     = pgpalw_getCurClassId();

/*
 * Unmanage unneeded forms 
 */
    pgpalw_classPopdown ();
    pggrpch_popdown();


    if ( operation != FUNC_GROUP && !pgpalw_isGrpActv() ) {
        pghdlb_deselectAll();
    }

    mcanvw_disarmDynamic();
    
    if ( class <= 0 ) mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
    switch(operation) 
    {
	case NONE_CURRENT:
	    break;

	case FUNC_MULTISEL:
	    pgpalw_setupOper ();
	    break;

	case FUNC_SELECT:	/* SELECT (MODIFY VERTEX) */
	case FUNC_DELETE:	/* DELETE */
	case FUNC_MOVE:		/* MOVE */
	case FUNC_COPY:		/* COPY */
	case FUNC_ROTATE:	/* ROTATE */
	case FUNC_FLIP:		/* FLIP */
	case FUNC_MODIFY:	/* MODIFY */
	case FUNC_PARTDELETE:	/* PARTIAL DELETE */
	case FUNC_DELPOINT:	/* DELETE POINT */
	case FUNC_CONNECT:	/* CONNECT */
        case FUNC_ADDPOINT:     /* ADD POINT */

/*
 * if a class has been previously selected, enter the state
 */
	    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	    if (class >= 0) {
		pgpalw_setupOper ();
	    }
	    else {
	        pgpalw_unmanageObjPal ();
	    }

	    break;

	case FUNC_GROUP:

	    if ( !pgpalw_isGrpActv() ) {
	        pgpalw_setCurBtns (-1, CLASS_ANY, -1);
	        pgpalw_unmanageObjPal(); 

	        pgpalw_actvGrp();
  
            }

            pgpalw_setupOper();
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_EXIT);

	    break;

	case FUNC_UNGROUP:	
	 
	    pgpalw_setCurBtns (-1, CLASS_ANY, -1);
	    pgpalw_unmanageObjPal();
	    pgpalw_setupOper ();
	    
	    break;

	case FUNC_LAYER:

	    if ( !pglayrw_isUp() ) {

	        pgpalw_actvLayer( True ); 
                pgevt_unsetOper( TRUE ); 
                
	    }
	    else {

	        pglayrw_addLayer();  	        
	    }

	    break;

	case FUNC_INC_DEC:	/* increment/decrement numbers*/
  	        pgpalw_setCurBtns (-1, CLASS_TEXT, -1);
  		pgpalw_setupOper ();
                pgpalw_manageObjPal (_palInfo.cur_class); 
		if (_palMode == TYPE_OBJ) {
		    pgnumb_popup(); 
		}
    	    break;

	case FUNC_CLOSVGF:	/* EXIT BUTTON */
	    pgpalw_exitCheck(w);

	    break;

	case FUNC_SAVE_AS:	/* SAVE_AS BUTTON */
	    mbotw_mouseSet( LMHINT_NOACTION, MMHINT_NOACTION);
	    pgfilw_popup( FUNC_SAVE_VGF );

	    break;
        case FUNC_SAVE_ALL:     /* SAVE ALL BUTTON */
	    pgfilw_startSaveAll();
	    break;
	case FUNC_SAVE_VGF:	/* SAVE VGF BUTTON */
	    mbotw_mouseSet( LMHINT_NOACTION, MMHINT_NOACTION);
	    if (pgfilw_isFileSaved()) {
	        pgfilw_save();
	    }
	    else { 
	        pgfilw_popup( FUNC_SAVE_VGF );
	    }
	    break;

	case FUNC_OPEN_VGF:	/* OPEN VGF BUTTON */
	    mbotw_mouseSet( LMHINT_NOACTION, MMHINT_NOACTION);
	    pgfilw_popup( FUNC_OPEN_VGF );

	    break;

	case FUNC_RESTORE:	/* RESTORE BUTTON */

	    mbotw_mouseClear();

/*
 * Confirm restore action with user.
 */
    	    sprintf ( mesg,
             "Are you sure you want to restore from the %s.save file ?",
             	cvg_getworkfile());
     	    NxmConfirm_show(_paletteW, mesg, pgpalw_restConfirmCb,
			    pgpalw_operCancelCb, NULL, &ier);
	    break;

	case FUNC_REFRESH:	/* REFRESH */

	    pgpalw_refresh();

/*
 * Reset to previous oper selection.
 */
            pgpalw_setPrevOper();

 	    break;

	case FUNC_OPEN_PROD:     /* OPEN PROD BUTTON */

            mbotw_mouseSet( LMHINT_NOACTION, MMHINT_NOACTION);
            pglpfw_popup();

            break;

	case FUNC_DELALL: 	/* DELETE ALL */

	    mbotw_mouseClear();

	    NxmConfirm_show( _paletteW,
			 "Are you sure you want to delete all?",
			 (XtCallbackProc)pgpalw_deleteAllCb, 
                         &pgpalw_operCancelCb,
			 NULL, &ier);
	    break;

	case FUNC_DEL_OBJ: 	/* DELETE BY OBJECT */

	    mbotw_mouseClear();
	    pgpalw_setCurBtns (operation,-1, -1);
            pgpalw_setupOper ();
            pgpalw_setButtonState (LOT_OPER, operation);

	    if (class > 0 && class != CLASS_ANY) {
	        mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	    }
	    else {
	        mbotw_mouseSet(LMHINT_CLASSSELECT, MMHINT_EXIT);
	    }
	    break;

	case FUNC_UNGRPALL: 	/* UNGROUP ALL */

	    mbotw_mouseClear();

	    NxmConfirm_show( _paletteW,
			 "Are you sure you want to ungroup all?",
			 (XtCallbackProc)pgpalw_ungroupAllCb, 
                         &pgpalw_operCancelCb,
			 NULL, &ier);
	    break;

	case FUNC_UNDO:       	/* UNDO */

/*
 * If GROUP process is active, deselect the active 
 * group first.
 */
            if ( pgpalw_isGrpActv() ) {
                 pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                           pggrpw_getGrpNum()   );  
            }

  	    pgundo_undo();  

/*
 * If GROUP process is active, select the active 
 * group again.
 */
            if ( pgpalw_isGrpActv() ) {
                 pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                         pggrpw_getGrpNum()   );  
            }

    	    pgpalw_setPrevOper ( );

	    break;

	case FUNC_REDO:       	/* REDO */

/*
 * If GROUP process is active, deselect the active 
 * group first.
 */
            if ( pgpalw_isGrpActv() ) {
                 pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                           pggrpw_getGrpNum()   );  
            }

  	    pgundo_redo();  

/*
 * If GROUP process is active, select the active 
 * group again.
 */
            if ( pgpalw_isGrpActv() ) {
                 pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                         pggrpw_getGrpNum()   );  
            }
 
    	    pgpalw_setPrevOper ( );

	    break;

	case FUNC_SHOW_GRPS:	/* SHOW ALL GROUPS */
	case FUNC_SHOW_NONGRP:	/* SHOW ALL UNGROUPED ELEMENTS */
	    mbotw_mouseSet( LMHINT_NOACTION, MMHINT_DONE);
	    pgpalw_setCurBtns (-1, CLASS_ANY, -1);
	    pgpalw_unmanageObjPal();

	    if ( operation == FUNC_SHOW_GRPS ) {
	        pghdlb_selectAll(GRP_ELMS);
	    }
	    else {
	        pghdlb_selectAll(NON_GRP_ELMS);	    
	    }
	    
	    pghdlb_displayAllSel();
	    mcanvw_setPressFunc((XtEventHandler)&pgpalw_endGrpShowEh, CURS_DEFAULT);

	    break;

	case FUNC_CHNG_GRPS:	/* CHANGE GROUPS */
	    mbotw_mouseClear();
            pggrpch_popup();

	    break;

	case FUNC_EXTRAP:	/* EXTRAPOLATION */
	    pgpalw_setCurBtns (-1, CLASS_ANY, -1);
	    pgpalw_unmanageObjPal(); 
            pgpalw_setupOper();
	    
	    pgextrap_popup();

	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);	    
	    
	    break;
       
        case FUNC_SMEAR:	/* SMEAR */
	    mcanvw_setPressFunc( (XtEventHandler)&pgsmear_selectElmEh, CURS_DEFAULT);
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_CANCEL);
	    pgsmear_startSmear();
	    break;

        case FUNC_INTERP:	/* INTERPOLATION */	    
	    pginterp_popup();
	    mcanvw_setPressFunc( (XtEventHandler)&pginterp_selectElmEh, CURS_DEFAULT);
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_CANCEL);
	    pginterp_startInterp();
	    break;

        case FUNC_FILTER:	/* DISPLAY FILTER */	    
	    pgfilterw_popup();
            pgevt_unsetOper( TRUE ); 

	    break;

	case FUNC_DISTANCE:	/* DISTANCE DISPLAY */
	    mbotw_mouseClear();
	    pgdist_popup();

	    break;

        case FUNC_FROM:		/* FROM */
	    mcanvw_setPressFunc( (XtEventHandler)&pgfrom_selectElmEh, CURS_DEFAULT);
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_CANCEL);
	    pgfrom_startSmear();
	    break;

        case FUNC_CYCLE:	/* DAY/CYCLE selection GUI */
	    pgcycle_popup();
	    break;

	default:
	    break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_classCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_classCb							*
 *									*
 * Callback function for class buttons on the drawing palette.		*
 *									*
 * static void pgpalw_classCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt     	XtPointer	not used			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAI	 4/97	Created					*
 * E. Wehner/EAI	 6/97	Handle wind barb form			*
 * D. Keiser/GSC	 6/97	Added FUNC_FLIP				*
 * E. Safford/GSC	 9/97	Replaced grp->cmd with cmd_ routines	*
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Minor cleanup				*
 * C. Lin/EAI	        10/97	add pgwbxw_popdown()			*
 * E. Safford/GSC	11/97	Add FUNC_MODIFY                 	*
 * E. Safford/GSC	03/98	Add FUNC_MULTISEL               	*
 * S. Law/GSC		04/98	Add FUNC_COPY				*
 * S. Law/GSC		05/98	Changed to use pgpalw_setupOper		*
 * S. Law/GSC		05/98	Changed to use _classPopdown		*
 * E. Safford/GSC	09/98	clean up                  		*
 * S. Law/GSC		11/98	stopped group popdown if FUNC_GROUP	*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/EAI          09/01   revised GROUP functionality             *
 * H. Zeng/EAI		03/02	modified to use pggrpch_popdown()	*
 * H. Zeng/SAIC		06/06   removed pgpalw_unmanageObjPal		*
 ***********************************************************************/
{
   int	 cur_class;
/*---------------------------------------------------------------------*/

    pgpalw_setCurClass(wid);

/* 
 * also, disarm all mouse callbacks
 */
    mcanvw_disarmDynamic();

/*
 * ... and unmanage unneeded forms 
 */
    pgpalw_classPopdown ();

    if (pgpalw_getCurOperId() != FUNC_CHNG_GRPS) {
	pggrpch_popdown();
    }

/*
 * For certain classes, end GROUP process if it is active.
 */
    cur_class = pgpalw_getCurClassId();
    if ( cur_class == CLASS_WATCHES  ||
         cur_class == CLASS_TRACKS   ||
         cur_class == CLASS_SIGMETS  ||
         cur_class == CLASS_PRODUCTS    ) {

         if ( pgpalw_isGrpActv() ) {
              pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                        pggrpw_getGrpNum()   );
              pgpalw_inactvGrp();
         }
    }

    pgpalw_manageObjPal (_palInfo.cur_class);
 
    pgpalw_setupOper();

}

/*=====================================================================*/
/* ARGSUSED */
void pgpalw_objCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_objCb								*
 *									*
 * Callback function for push buttons on the drawing palette.		*
 *									*
 * static void pgpalw_objCb ( w, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	w		Widget		widget ID			*
 *	clnt		XtPointer					*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAI	10/96	Created					*
 * D. Keiser/GSC	02/97	Remove callback to info box,		*
 *				add call to ss_ltim for wnum		*
 * E. Wehner/EAI	02/97	Replace ss_ltim w/ cfdate		*
 * D. Keiser/GSC	03/97	Add contour element type		*
 * E. Wehner/EAi	04/97	Changed mouse callbacks			*
 * E. Wehner/Eai	05/97	Split commands out to addbtn		*
 * E. Wehner/EAi	08/97	Remove watch by county vbls.		*
 * C. Lin/EAI           10/97	rename, clean up, move NxmAddBtn() here	*
 * C. Lin/EAI           10/97	add watch_box and county_list  		*
 * E. Safford/GSC	12/97	update param list to popups     	*
 * F. Yen/NCEP		01/98	Updated calls for crg library cleanup	*
 * W. Li/EAI		02/98	added call to pgobj_getId.		*
 * S. Law/GSC		03/98	added call to pgpalw_modifyType		*
 * W. Li/EAI            03/98   modify for symbol editing window        *
 * E. Safford/GSC	03/98	added CLASS_ANY, removed unused FUNCs   *
 * C. Lin/EAI	        04/98	modify for combo-symbol and lines	*
 * C. Lin/EAI	        04/98	modify for LABEL func and OUTLOOK obj 	*
 * W. Li/EAI		04/98	Add call to pgobj_getId			*
 * W. Li/EAI            04/98   Removed stroke from CLASS_LINE          *
 * F. Yen/NCEP          05/98   Removed pip stroke from CLASS_FRONTS    *
 * D.W.Plummer/NCEP	05/98	Add SFCPRG and OUTLOOK			*
 * C. Lin/EAI	        06/98	modify for more dashed lines 		*
 * F. J. Yen/NCEP	06/98	Added QPF obj                		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	08/98	mod dynamic flag for text & wind ghosts *
 * D.W.Plummer/NCEP	 8/98	Added HCNTRK obj			*
 * D.W.Plummer/NCEP	 8/98	Added GGCNTR obj			*
 * F. J. Yen/NCEP	09/98	Added XRAINF obj			*
 * E. Safford/GSC	09/98	move modifyType ahead of LABEL check    *
 * C. Lin/EAI		09/98	remove pgfrtw_updateSmth()    		*
 * C. Lin/EAI		09/98	add close popup windows    		*
 * E. Safford/GSC	09/98	clean up                                *
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		11/98	Seperated CLASS_WATCH / CLASS_PRODUCTS	*
 * S. Law/GSC		01/99	made changes to force choosing shape	*
 * W. Li/EAI		01/99	NxmTxtA_setGhostFlag -->		*
 *						pgtxt_setGhostFlag	*
 * F. J. Yen/NCEP	01/99	Added WXD obj				*
 * S. Law/GSC		03/99	removed call to pgline_updateChar	*
 * S. Law/GSC		04/99	added call to pgpalw__setButtonState	*
 * W. LI/EAI		05/99	add FUNC_DEL_OBJ			*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * W. LI/EAI		05/99	add call to pggrpw_initType		*
 * W. LI/EAI		06/99	disabled FUNC_DEL_OBJ for group mode	*
 * S. Law/GSC		07/99	added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	changed pgsigw_popup calling		*
 * S. Law/GSC		09/99	changed pgsigw_popup calling		*
 * H. Zeng/EAI          09/99   added call to pgofmt_popup()            *
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          10/99   added call to pggfmt_popup()            *
 * S. Law/GSC		12/99	added OBJ_WTCHSTAT			*
 * S. Law/GSC		02/00	Added CCF				*
 * S. Law/GSC		03/00	Added CCF product			*
 * M. Li/GSC		04/00	Added case HCNTRK			*
 * A. Hardy/GSC		05/00	Added WTCHCNL				*
 * H. Zeng/EAI          07/00   added a new para. to pg****_popup() for *
 *                              front, line, symbol and circle          *
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 * H. Zeng/EAI          08/00   changed to use pgpalw_setButtonState()  *
 * H. Zeng/EAI          09/00   fixed a bug of FUNC_LABEL               *
 * E. Safford/GSC	01/01	add param to pgline_popup		*
 * M. Li/GSC		01/01	Added pggrpw_initType to PRODUCT	*
 * M. Li/GSC		01/01	Added pggrpw_initOlk to PRODUCT		*
 * H. Zeng/EAI          03/01   Removed calls to pggrpw_initType()      *
 * E. Safford/GSC	03/01	add param to pgline_popup, pgfrtw, symb	*
 * H. Zeng/EAI          05/01   added  pggrpw_popdownGrpChngW           *
 * M. Li/SAIC		10/01	added class marker			*
 * H. Zeng/EAI          10/01   revised GROUP functionality             *
 * E. Safford/SAIC	03/02	rm pggrpw_initOlk from OUTLOOK case     *
 * H. Zeng/EAI		03/02	modified to use pggrpch_popdown()	*
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * M. Li/SAIC           04/02   Added call to pgpalw_isGrpActv          *
 * J. Wu/SAIC           11/02   add CLASS_LIST			        *
 * J. Wu/SAIC		12/02	remove pgnew_setArmDynamic fm CLASS_LIST*
 * H. Zeng/XTRIA	01/03   modified arguments to pgwbxw_popup      *
 * H. Zeng/XTRIA	07/03	added OBJ_SIGVOL and OBJ_SIGVAC		*
 * H. Zeng/XTRIA	08/03	removed ghost lines for OBJ_SIGVOL	*
 * J. Wu/SAIC		10/03	add CLASS_MET				*
 * H. Zeng/XTRIA	10/03   added more ash cloud stuff		*
 * J. Wu/SAIC		02/04	add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC          03/04   added CLASS_MET -> TCA_ELM              *
 * B. Yin/SAIC          04/04   modified tca_popup calling sequence     *
 * H. Zeng/SAIC         11/04   changed para. list for pgwbxw_popup     *
 * B. Yin/SAIC          12/04   added CLASS_PRODUCTS -> AIRMET		*
 * E. Safford/SAIC	03/07	add OBJ_AIRMET_P, OBJ_GFA_P		*
 ***********************************************************************/
{
    int		ii, ier, oper;
    char	mesg[128];
    int		class, obj, elmid, gemtyp, subtyp;
/*---------------------------------------------------------------------*/

    pgpalw_setCurObj(w);
    
/*
 * If GROUP is active, disable "Modify Type" capability.
 */
    if ( !pgpalw_isGrpActv() ) {   
	if (pgpalw_modifyType () == TRUE)  {
	    return;
	}
    }
    
    oper =  pgpalw_getCurOperId();  

    if ( oper != FUNC_DEL_OBJ) {
	pgpalw_setButtonState (LOT_OPER, NONE_CURRENT);
    }
    
/*
 * disarm other states if applicable.
 */
    mcanvw_disarmDynamic();
    
/* 
 * if an element is selected, deselect it.
 * if the GROUP is active, let it remain selected.
 */
    if ( !pgpalw_isGrpActv() ) {
	pghdlb_deselectAll();
    }
    
/* 
 * close all popups 
 */
    pgpalw_classPopdown();
    pggrpch_popdown();
    
/* 
 * if we are trying to add an element and we already have the max
 * number of drawables drawn, just report an error and don't allow the 
 * add to occur.
 */
    crg_newinx(&ii, &ier);
    if (ier < 0) {
	strcpy(mesg, 
	       "Unable to add anymore elements.  Delete something first \n");
	NxmWarn_show(_paletteW, mesg);
	mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
	return;
    }
    
    class = pgpalw_getCurClassId();
    obj   = pgpalw_getCurObjId();
    
    if (oper == FUNC_DEL_OBJ && class != CLASS_PRODUCTS && 
	class != CLASS_ANY && (pgpalw_getMode() == TYPE_OBJ)) {
	pgpalw_setButtonState (LOT_OPER, oper);
	pgdelobj_deletStart(w, class, obj);
    }
    else{

/*
 * If the current operation is not FUNC_GROUP, change it to
 * FUNC_SELECT.
 */
	if ( pgpalw_isGrpActv() ) {
	    pgpalw_setButtonState (LOT_OPER, FUNC_GROUP);
	}
	else {
	    pgpalw_setButtonState (LOT_OPER, FUNC_SELECT);
	}

	switch ( class ) {
	  case CLASS_TEXT:
	    pgnew_setArmDynamic();
	    mcanvw_setDynActFlag(FALSE);
	    pgtxt_setGhostFlag (TRUE, pggst_setText);
	    pgtxt_popup ();

	    break;

	  case CLASS_SYMBOLS:
	  case CLASS_COMSYM:
   	  case CLASS_MARKER:
	    pgobj_getId (class, obj, &elmid, &gemtyp, &subtyp);
	    pgsymb_popup (subtyp, elmid, FALSE, NULL, NULL, TRUE);
	    pgnew_setArmDynamic();
	    mcanvw_setDynActFlag(FALSE);

	    break;
	    
	  case CLASS_WINDS:
	    pgobj_getId (class, obj, &elmid, &gemtyp, &subtyp);
	    pgwndw_popup(obj, FALSE, NULL);
	    mcanvw_setDynActFlag(FALSE);

	    break;

	  case CLASS_FRONTS:
	    pgobj_getId ( class, obj, &elmid, &gemtyp, &subtyp);
	    pgfrtw_popup(gemtyp, TRUE, TRUE, FALSE, NULL, NULL, TRUE);
	    pgnew_setArmDynamic();

	    break;

	  case CLASS_LINES:
	    pgobj_getId (class, obj, &elmid, &gemtyp, &subtyp);
	    if ( elmid == LINE_ELM ) {
		pgline_popup(gemtyp, elmid, TRUE, FALSE, FALSE, FALSE, 
                             NULL, NULL, 0, TRUE );
	    }
	    else {
		if ( gemtyp == 24 || gemtyp == 25 ) { /* kink line */
		    pgline_popup(gemtyp, elmid, TRUE, TRUE, TRUE, FALSE, 
                             NULL, NULL, 0, TRUE );
	    }
		else {
		    pgline_popup(gemtyp, elmid, TRUE, TRUE, FALSE, FALSE, 
                             NULL, NULL, 0, TRUE );
	        }
	    }
	    pgnew_setArmDynamic();

	    break;

	  case CLASS_CIRCLE:
	    pgobj_getId (class, obj, &elmid, &gemtyp, &subtyp);
	    pgnew_setArmDynamic();
	    mcanvw_setDynActFlag(FALSE);
	    pgcirc_setGhostFlag (TRUE, pggst_setCircle);
	    pgcirc_popup(gemtyp, elmid, TRUE, FALSE, NULL, NULL);

	    break;

	  case CLASS_WATCHES:
	    switch (_palInfo.cur_obj) {

	        case OBJ_WBCOUNTY:
		case OBJ_WBPARALL:
	            subtyp = (_palInfo.cur_obj == OBJ_WBCOUNTY) ? WBC : PGRAM;
		    pgwbxw_popup (subtyp, -1, NONE, -1, -1.0F, -1, 
		                  0, -1, FALSE, NULL);

	            if (subtyp == WBC) {
		        pgnew_setArmDynamic();
		        mbotw_mouseSet(LMHINT_START, MMHINT_DONE);
	            }
	            else {
		        mcanvw_setPressFunc ((XtEventHandler)&pgwbxw_waitEh, CURS_DEFAULT);
	            }
	            break;

	        case OBJ_WATCHLN:
	            pgpalw_setButtonState (LOT_OPER, FUNC_SELECT);
                    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
                    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
                    break;
	    }
	    break;

	  case CLASS_TRACKS:
	    pgtrkw_popup (TRKSTORM_ELM, FALSE, NULL);

	    pgnew_setArmDynamic();

	    break;

	  case CLASS_SIGMETS:

	    if ( obj == OBJ_SIGVOL ) {

		pgvolw_popup ();
		mbotw_actionSet(ACHINT_ADD);
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_TOSELECTOPER);
                mcanvw_setDropFunc( (XtEventHandler)&pgnew_dummyDrop, CURS_DEFAULT );
            }
	    else if ( obj == OBJ_SIGVAC ) {

		pgvacw_popup ( NULL );
	        pgnew_setArmDynamic();
	    }
	    else if (obj == OBJ_SIGCCF) {

		pgccfw_popup (NULL, NULL);
		pgnew_setArmDynamic();
	    }
	    else {

		pgsigw_popup (NULL, NULL);
	        pgnew_setArmDynamic();
	    }

	    break;

	  case CLASS_PRODUCTS:
            switch(obj) {
	      case  OBJ_SFCPRG :
	      case  OBJ_QPF    :
	      case  OBJ_XRAINF :
	      case  OBJ_WXD    :
		mbotw_actionClear();
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		pgprd_popup();
                break;
	      case  OBJ_CCFPRD :
		mbotw_actionClear();
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		pgccfp_popup();
                break;
              case  OBJ_HCNTRK :
                mbotw_actionClear();
                mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
                pgtrk_popup();
                break;
	      case  OBJ_WATCHFMT :
		mbotw_actionClear();
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		pgwfmt_popup();
		pgpalw_manageObjPal (CLASS_WATCHES);

		pgpalw_setCurOper (_palInfo.oper[FUNC_SELECT].wid);
		pgpalw_setupOper();
		mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
                break;

	      case  OBJ_WTCHCNL :
 	        mbotw_actionClear();
                mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
                pgwcnsl_popup();
                pgpalw_manageObjPal (CLASS_WATCHES);

                pgpalw_setCurOper (_palInfo.oper[FUNC_SELECT].wid);
                pgpalw_setupOper();
                mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
                break;

	      case  OBJ_WTCHSTAT :
		mbotw_actionClear ();
		mbotw_mouseSet (LMHINT_NOACTION, MMHINT_NOACTION);
		pgwsmw_popup (NULL, NULL);
		pgpalw_manageObjPal (CLASS_WATCHES);

		pgpalw_setCurOper (_palInfo.oper[FUNC_SELECT].wid);
		pgpalw_setupOper();
		mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
                break;
	      case  OBJ_OUTLOOK :
		mbotw_actionClear();
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		pgofmt_popup();
                break;
              case OBJ_GGCNTR :
		mbotw_actionClear();
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		pggfmt_popup();
                break; 
              case OBJ_AIRMET :
		pgairmet_popup();
                break; 
              case OBJ_AIRMET_P :
		pgairmetp_popup();
                break; 
	    } /* end of switch(obj) */

	    break;

	  case CLASS_LIST:
/*
 *  Dynamic press functions are set up in pglist_popup()
 *  based on the selection of list-creating mode.
 */ 
	    pgobj_getId ( class, obj, &elmid, &gemtyp, &subtyp );
	    pglist_popup ( subtyp, FALSE, NULL );
	    
	    mcanvw_setDynActFlag(FALSE);
	    
	    break;

	  case CLASS_MET:	    

	    switch( obj ) {

	      case OBJ_JET:
	        pgnew_setArmDynamic();
	        break;
              case OBJ_GFA:
		pggfaw_popup ( NULL, NULL );
	        pgnew_setArmDynamic();
		break;
	      case OBJ_GFA_P:
		pggfawp_popup ( NULL, NULL );
	        pgnew_setArmDynamic();
	        break;
              case OBJ_TCA:
                pgtca_popup( NULL );
		break;
            }
	    	    
	    break;

	  case CLASS_ANY:
	  
	  default:
	    break;
	}
    }	/* else */
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_deleteAllCb ( Widget w, XtPointer clnt, 
							XtPointer cbs )
/************************************************************************
 * pgpalw_deleteAllCb							*
 *									*
 * This is the callback function which marks all records in the VG file	*
 * as deleted and clears the drawing area of VG elements.		*
 *									*
 * static void pgpalw_deleteAllCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback			*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	06/97						*
 * D. Keiser/GSC	06/97	Change name from drw_delall		*
 * C. Lin/EAI		08/97	Call xputpxms()				*
 * E. Wehner/EAi	08/97	Changed call to cvg_load		*
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI		10/97	rename from drw_dallcb, minor cleanup	*
 *				call crg_init				*
 * E. Safford/GSC	01/98	add clear vgf file name			*
 * E. Safford/GSC	03/98	add pgevt_initUndo call			*
 * S. Law/GSC		04/98	added pghdlb_deselectAll		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	add undo via eraseAll to delete_all	*
 * E. Safford/GSC	10/98	reset vgf file name on delete all  	*
 * W. Li/EAI		10/98	renamed pgdel_eraseAll->pgdel_deletAll	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC	12/99	xputpxms -> xpgrestlp			*
 * S. Law/GSC		06/00	removed loop parameter from xpgrestlp	*
 * E. Safford/GSC	11/00	don't clear vg file name		*
 * H. Zeng/EAI          11/00   removed pgundo_storeFileName()          *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * H. Zeng/EAI          02/01   removed call to pgpalw_setCurOper()     *
 * E. Safford/GSC	02/01	moved functionality to pgpalw_deleteAll *
 ***********************************************************************/
{
    pgpalw_deleteAll();
    pgpalw_setPrevOper();
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_ungroupAllCb ( Widget wid, XtPointer clnt, 
							XtPointer cbs )
/************************************************************************
 * pgpalw_ungroupAllCb							*
 *									*
 * This is the callback function which ungroups all grouped elements.	*
 *									*
 * static void pgpalw_ungroupAllCb (wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	cbs		XtPointer	callback			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	Initial coding				*
 * E. Safford/GSC	09/98	fix ungroup all problem			*
 * G. Krueger/EAI	09/98	Correct MOUSESET hints			*
 * E. Safford/GSC	09/98	clean up               			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * E. Safford/SAIC	08/01	reset to select upon ungroup & clean up *
 ***********************************************************************/
{
    pghdlb_selectAll (2);

    pgevt_ungroup ();
    pghdlb_deselectAll ();

    pgpalw_setCurOper(_palInfo.oper[FUNC_SELECT].wid);    
    pgpalw_setupOper();
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_endGrpShowEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgpalw_endGrpShowEh							*
 *									*
 * This is the callback function which deselects all groups or selected	*
 * elements and then resets pgen palette back to FUNC_SELECT.         	*
 *									*
 * static void pgpalw_endGrpShowEh (wid, clnt, event, ctdr)		*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data         	*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	01/99	initial coding         			*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/EAI          05/02   added call to pgpalw_setCurBtns()       *
 * J. Wu/SAIC           03/05   rename to endGrpShowCb()		*
 ***********************************************************************/
{

    if (event->xbutton.button == Button2) {
  	pghdlb_deselectAll();

/*
 * Reset pgen palette back to FUNC_SELECT.
 */
        pgpalw_setCurBtns ( FUNC_SELECT, -1, -1 );
        pgpalw_setupOper();

	mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_restConfirmCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_restConfirmCb							*
 *									*
 * This function confirms the restore operation. 			*
 *									*
 * static void pgpalw_restConfirmCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		Widget that activated callback		*
 *	clnt	XtPointer	Pointer to client data			*
 *	cbs	XtPointer	callback struct				*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		12/97						*
 * E. Safford/GSC	01/98	Added crg_init to clear range		*
 * S. Law/GSC		04/98	Added pghdlb_deselectAll		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	01/00   use xpgrestlp instead of xpgrest	*
 * S. Law/GSC		06/00	removed loop parameter from xpgrestlp	*
 * H. Zeng/EAI          12/00   modified for the new undo design        *
 * H. Zeng/EAI          02/01   reset to the previous oper selection    *
 * J. Wu/SAIC		11/01	remove redundant geplot call		*
 * J. Wu/SAIC	   	12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC		02/02	set changes_made to TRUE on curr. layer	*
 * E. Safford/SAIC	04/06	rm cvg_load(), use pgpalw_refresh()	*
 * B. Yin/SAIC		11/06	reset to previous oper after refrersh	* 
 ***********************************************************************/
{
    int		ier = 0, curLayer = 0;
    char   	fname[128], newpath[LLPATH], work_file[ LLPATH ];
    struct 	stat buf;
/*---------------------------------------------------------------------*/

    strcpy( work_file, cvg_getworkfile() );

/*
 *  deselect all elements, wipe the range records
 */
    pghdlb_deselectAll();
    crg_init( &ier );

/*
 *  Copy the ./[ work_file ].save file to the work_file
 */
    css_envr ( work_file, newpath, &ier );
    sprintf(fname, "%s.save", newpath);
    if ( stat(fname, &buf) == 0 ) {
        cvg_cp(fname, work_file, G_TRUE, &ier);
    }
    
/*
 *  Construct new range records, call refresh
 *  to display the file contents.
 */
    curLayer = pglayer_getCurLayer();
    crg_build( work_file, &curLayer, &ier );

    pgpalw_refresh();    

/*
 * Reset to previous oper selection.
 */
    pgpalw_setPrevOper();
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_operCancelCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_operCancelCb							*
 *									*
 * This function is the callback when a user presses cancel from a 	*
 * confirm operation popup.						*
 *									*
 * static void pgpalw_operCancelCb (w, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	w	 Widget 	 Widget that activated callback 	*
 *	clnt	 XtPointer	 Pointer to client data 		*
 *	cbs	 XtPointer	 callback struct			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI	06/98						*
 * S. Law/GSC		09/98	Combined various CancelCb functions	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * H. Zeng/EAI          02/01   reset to the previous oper selection    *
 ***********************************************************************/
{
/*
 * Reset to previous oper selection.
 */
    pgpalw_setPrevOper ( );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgpalw_setModeCb ( Widget widget, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpalw_setModeCb                                                     *
 *                                                                      *
 * This function updates the value of _palType and toggles the          *
 * appropriate function buttons on/off.                                 *
 *                                                                      *
 * static void pgpalw_setModeCb (widget, clnt, cbs)                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      widget   Widget          Widget that activated callback         *
 *      clnt	XtPointer       Pointer to client data                	*
 *      cbs	XtPointer       callback struct                        	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       09/98   initial coding                          *
 * E. Safford/GSC       12/98   add numb_popup when in INC/DEC          *
 * H. Zeng/SAIC		06/06   removed pgpalw_unmanageObjPal		*
 ***********************************************************************/
{
int     which = (long)clnt;
XmToggleButtonCallbackStruct *state =
                                   (XmToggleButtonCallbackStruct *)cbs;
/*---------------------------------------------------------------------*/

    _palMode = (Boolean)((state->set)?  which : 0);

/*
 * deselct everything
 */
    pghdlb_deselectAll();

/* 
 * also, disarm all mouse callbacks
 */
    mcanvw_disarmDynamic();

/*
 * ... and unmanage unneeded forms 
 */
    pgpalw_classPopdown ();
    pgpalw_manageObjPal (_palInfo.cur_class);

    pgpalw_setupOper();

    if ( _palInfo.cur_class == CLASS_TEXT && 
         _palInfo.cur_oper == FUNC_INC_DEC && _palMode == TYPE_OBJ ) {
	pgnumb_popup(); 
    }
}

/*=====================================================================*/

static void pgpalw_manageObjPal ( int classnum )
/************************************************************************
 * pgpalw_manageObjPal							*
 *									*
 * Displays  the requested object palette.				*
 *                                                                      *
 * static void pgpalw_manageObjPal(classnum)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  classnum	Int	 	Which class number palette to be shown	*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi	 5/97	Created					*
 * E. Wehner/EAi	 9/97	Kill grinfo record			*
 * C. Lin/EAI		10/97	rename NxmObjPalManage, minor cleanup	*
 * S. Law/GSC		05/98	Added call to pgpalw_setCurClass	*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * T. Piper/SAIC	02/06	Re-done for AWIPS problem w/bluecurve	*
 ***********************************************************************/
{
#ifdef AWIPS
    Dimension height;
    XtUnmanageChild(_paletteW);
#endif
    pgpalw_unmanageObjPal();
    pgpalw_setCurClass (_palInfo.class[classnum].wid);
    XtManageChild(_objpalsW[classnum]);
#ifdef AWIPS
    XtVaGetValues(_paletteW, XmNheight, &height, NULL);
    XtVaSetValues(pgpalette, XmNheight, height, NULL);
    XtManageChild(_paletteW);
    XmUpdateDisplay(_paletteW);
#endif
}

/*=====================================================================*/

void pgpalw_setCurOper ( Widget wid )
/************************************************************************
 * pgpalw_setCurOper							*
 *									*
 * This function identifies the numeric value of the user		*
 * selected operation button, or zero if no change was made to the	*
 * operation buttons.							*
 *									*
 * void pgpalw_setCurOper (wid)						*
 *									*
 * Input parameters:                                                    *
 *	wid		Widget	User selected Operation button		*
 *									*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97	modified from cmd_idFunc()		*
 * S. Law/GSC		05/98	added highlighting			*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * S. Law/GSC		04/99	added calls to pgpalw__setButtonState	*
 * W. Li/EAI		05/99	added resetting	for buttons		*
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 * H. Zeng/EAI          02/01   modified to save current oper Widget    *
 ***********************************************************************/
{
    int		nn, id, cur_oper;
/*---------------------------------------------------------------------*/
/*
 * Save the current oper Widget into _prevOperWid.
 */
    id = pgpalw_getCurOperId();
    _prevOperWid = _palInfo.oper[id].wid;

/*
 * Set the current oper widget.
 */
    nn = 1;
    while ((nn < MAX_OPERS) && (wid != _palInfo.oper[nn].wid)) {
        nn++;
    }

    if (wid == (Widget) NULL || nn >= MAX_OPERS) {
	nn = FUNC_SELECT;
    }

    pgpalw_setButtonState (LOT_OPER, nn);

    if (_palInfo.cur_oper != NONE_CURRENT) {
	mbotw_actionSet( pgpalw_getCurOperName() );
    }

/*
 * If GROUP is active, it remains active only for certain 
 * operations.
 */
    cur_oper = pgpalw_getCurOperId();
    if (cur_oper != FUNC_SELECT && cur_oper != FUNC_MULTISEL &&
        cur_oper != FUNC_UNDO   && cur_oper != FUNC_REDO     &&
        cur_oper != FUNC_GROUP  && pgpalw_isGrpActv()          ) {

        pgpalw_inactvGrp();
    }
}

/*=====================================================================*/

static void pgpalw_setCurClass ( Widget wid )
/************************************************************************
 * pgpalw_setCurClass							*
 *									*
 * This function identifies the user selected class based on the input	*
 * widget ID.								*
 *									*
 * static void pgpalw_setCurClass (wid)					*
 *									*
 * Input parameters:							*
 *	wid		Widget	User selected button			*
 *									*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97	modified from cmd_idClass()		*
 * S. Law/GSC		05/98	added highlighting			*
 * G. Krueger/EAI	06/98   Uniform status hints			*
 * C. Lin/EAI		08/98	modified to improve the efficiency	*
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		01/99	Added Inc/decrement for CLASS_ANY	*
 * E. Safford/GSC	02/99	correct INC_DEC & TEXT and cleanup      *
 * S. Law/GSC		04/99	added calls to pgpalw__setButtonState	*
 * W. LI/EAI		05/99	added FUNC_DEL_OBJ			*
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 ***********************************************************************/
{
    int		nn, class, oper;
/*---------------------------------------------------------------------*/
/*
 * find the current CLASS based on the widget
 */
    nn = 1;
    while ((nn < MAX_CLASSES) && (wid != _palInfo.class[nn].wid)) {
	nn++;
    }

    if (wid == (Widget) NULL || nn > MAX_CLASSES) {
	nn = NONE_CURRENT;
	mbotw_classClear();
    }

/*
 * set the current class
 */
    pgpalw_setButtonState (LOT_CLASS, nn);

    if (_palInfo.cur_class != NONE_CURRENT) {
	class = pgpalw_getCurClassId();
	oper  = pgpalw_getCurOperId();

	mbotw_classSet( pgpalw_getCurClassName() );

	if ( oper == FUNC_SELECT  && class > 0 ) {
	    mbotw_mouseSet(LMHINT_CLASSSELECT, MMHINT_NOACTION);
	}
	else if ( oper == FUNC_DEL_OBJ && class > 0) {
	    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_NOACTION);
	}
	else if ( oper == FUNC_INC_DEC && 
		(class == CLASS_ANY || class == CLASS_TEXT ) ) {
	    pgnumb_popup();
	}
    }
}

/*=====================================================================*/

void pgpalw_setCurObj ( Widget wid )
/************************************************************************
 * pgpalw_setCurObj							*
 *									*
 * This function identifies the user selected object based on the input	*
 * widget ID.								*
 *									*
 * void pgpalw_setCurObj (wid)						*
 *									*
 * Input parameters:							*
 *	wid		Widget	User selected button			*
 *									*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97	modified from cmd_idObj()		*
 * S. Law/GSC		05/98	added highlighting			*
 * C. Lin/EAI		08/98	modify to improve the efficiency	*
 * S. Law/GSC		04/99	added calls to pgpalw__setButtonState	*
 * S. Law/GSC		08/00	pgpalw_setButtonState parameters change	*
 ***********************************************************************/
{
    int		nn;
/*---------------------------------------------------------------------*/
/*
 * find the current object based on the object widget ID
 */
    nn = 1;
    while ((nn < MAX_OBJECTS) && (wid != _palInfo.obj[nn].wid)) {
        nn++;
    }

    if (wid == (Widget) NULL || nn >= MAX_OBJECTS) {
	nn = NONE_CURRENT;
    }

/*
 * set the current object widget
 */
    pgpalw_setButtonState (LOT_OBJ, nn);
}

/*=====================================================================*/

static Boolean pgpalw_modifyType ( void )
/************************************************************************
 * pgpalw_modifyType							*
 *									*
 * Checks to see if the current function is FUNC_MODIFY or FUNC_SELECT	*
 * and if the current class is CLASS_FRONTS or CLASS_LINES.  If true,	*
 * the appropiate function is called to change the current element to	*
 * to the new type.							*
 *									*
 * static Boolean pgpalw_modifyType()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return value:							*
 *	pgpalw_modifyType	static Boolean	Modify conditions were	*
 *						  TRUE = met		*
 *						  FALSE = unmet		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/98	Created					*
 * S. Law/GSC		04/98	Add check if any element is selected	*
 * E. Safford/GSC	09/98   mod check on pgevt_changeType		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/SAIC		04/04	added FUNC_MULTISEL			*
 ***********************************************************************/
{
int 	curr_id, num_selected;
/*---------------------------------------------------------------------*/

    curr_id	 = pgpalw_getCurOperId ();
    num_selected = pghdlb_elemSelected ();

    if (!(curr_id == FUNC_MODIFY || curr_id == FUNC_SELECT ||
	  curr_id == FUNC_MULTISEL) || num_selected == 0)  {
	return (FALSE);
    }

    switch (pgpalw_getCurClassId ()) {
      case CLASS_FRONTS:
      case CLASS_LINES:
  	if (pgevt_changeType (pgpalw_getCurObjId ()) != 0) 
	    return (FALSE);
	break;
      default:
	return (FALSE);
    }

    return (TRUE);
}

/*=====================================================================*/

static void pgpalw_getBtnLocation ( int type, int class, int start, int *palids )
/************************************************************************
 * pgpalw_getBtnLocation						*
 *									*
 * Finds the user defined positions of the buttons on the palette	*
 *									*
 * static void pgpalw_getBtnLocation (type, class, start, palids)	*
 *									*
 * Input parameters:							*
 *	type		int	operation = 0, class = 1, object = 2	*
 *                              file control = 3                        *
 *	class		int	class value (needed for objects)	*
 *	start		int	starting value (1, except for objects)	*
 *									*
 * Output parameters:							*
 *	*palids		int	location of button on palette		*
 *									*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/98	Adapted from cpg_getmnuid		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/EAI          02/01   Added a new type                        *
 * T. Piper/SAIC	12/01	Close file				*
 ***********************************************************************/
{
    FILE        *fp;
    int 	ier, matched, master_btnid, found_palid, *nbtns, maxbtns;
    char	temp[120], content[120];
    button_t	*buttons;
/*---------------------------------------------------------------------*/

    switch (type) {
      case 0:
	strcpy (temp, OPERBTN_TBL);
	nbtns	= &_palInfo.nopers;
	maxbtns	= MAX_OPERS;
	buttons	= _palInfo.oper;
	break;
      case 1:
	strcpy (temp, CLASSBTN_TBL);
	nbtns	= &_palInfo.nclasses;
	maxbtns	= MAX_CLASSES;
	buttons	= _palInfo.class;
	break;
      case 2:
	sprintf (temp, "%sbtn.tbl", _palInfo.class[class].name);
	nbtns	= &_palInfo.nobjs[class];
	maxbtns	= MAX_OBJECTS;
	buttons	= _palInfo.obj;
	break;
      case 3:
	strcpy (temp, CTRLBTN_TBL);
	nbtns	= &_palInfo.nctrls;
	maxbtns	= MAX_OPERS;
	buttons	= _palInfo.oper;
	break;
    }

    fp = cfl_tbop (temp, "pgen", &ier);
    if ( fp == NULL || ier != 0 ) {
	return;
    }

    strcpy(temp, "BEGIN");

/*
 * search for BEGINning
 */
    matched = 0;
    while (!matched) {
        cfl_trln (fp, sizeof (content), content, &ier);
        if (ier != G_NORMAL) {
	    cfl_clos(fp, &ier );
            return;
        }
        else if (strncmp (temp, content, strlen(content)) == 0) {
	    strcpy(temp, "END");
	    matched = 1;
	}
    }

/*
 *  load paletteids.
 */
    *nbtns = start;
    while (TRUE) {
/*
 * check for END
 */
	cfl_trln (fp, sizeof (content), content, &ier);
	if (ier != G_NORMAL ||
	    (strncmp (temp, content, strlen (content))) == 0) {
	    *nbtns = *nbtns - start;
	    cfl_clos(fp, &ier);
	    return;
	}

/* 
 * Determine which palette id has the icon
 * name that matches this line of text....
 */
	found_palid = FALSE;
	master_btnid = 1;
	while ((!found_palid) && (master_btnid <= maxbtns)) {
	    if ( strcmp(content, buttons[master_btnid].name) == 0 ) {
		found_palid = TRUE;
		palids[*nbtns] = master_btnid;
		(*nbtns)++;
	    }
	    else {
	 	master_btnid ++;
	    }
	}
    }
/* 
 * This point will never be reached.  Don't put important code here.
 */
}

/*=====================================================================*/

static void pgpalw_setButtonState ( int type, int button )
/************************************************************************
 * pgpalw_setButtonState						*
 *									*
 * Sets the buttons state and the appropiate pixmap			*
 *									*
 * static void pgpalw_setButtonState (type, button)			*
 *									*
 * Input parameters:							*
 *	type		int	LOT_OPER, LOT_CLASS, LOT_OBJ		*
 *	button		int	the button index			*
 *									*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/99	initial coding				*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/EAI          11/99   modified for Motif 2.1                  *
 * S. Law/GSC		08/00	rewrote to use reversed color pixmaps	*
 ***********************************************************************/
{
    int		curr;
/*---------------------------------------------------------------------*/

    switch (type) {
      case LOT_OPER:
	curr = _palInfo.cur_oper;

	if (button < 0 || MAX_OPERS <= button || curr == button) return;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.oper[curr].wid,
			   XmNlabelPixmap, _palInfo.oper[curr].pxms[PXM_REG],
			   NULL);
	}

	curr = _palInfo.cur_oper = button;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.oper[curr].wid,
			   XmNlabelPixmap, _palInfo.oper[curr].pxms[PXM_REV],
			   NULL);
	}

	break;

      case LOT_CLASS:
	curr = _palInfo.cur_class;

	if (button < 0 || MAX_CLASSES <= button || curr == button) return;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.class[curr].wid,
			   XmNlabelPixmap, _palInfo.class[curr].pxms[PXM_REG],
			   NULL);
	}

	curr = _palInfo.cur_class = button;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.class[curr].wid,
			   XmNlabelPixmap, _palInfo.class[curr].pxms[PXM_REV],
			   NULL);
	}

	break;

      case LOT_OBJ:
	curr = _palInfo.cur_obj;

	if (button < 0 || MAX_OBJECTS <= button || curr == button) return;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.obj[curr].wid,
			   XmNlabelPixmap, _palInfo.obj[curr].pxms[PXM_REG],
			   NULL);
	}

	curr = _palInfo.cur_obj = button;

	if (curr != NONE_CURRENT) {
	    XtVaSetValues (_palInfo.obj[curr].wid,
			   XmNlabelPixmap, _palInfo.obj[curr].pxms[PXM_REV],
			   NULL);
	}

	break;
    }
}

/*=====================================================================*/

void pgpalw_undo ( void )
/************************************************************************
 * pgpalw_undo								*
 *									*
 * Undoes the last saved operation.  Used for the undo hotkey.		*
 *									*
 * void pgpalw_undo ( )							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		04/01	create					*
 * J. Wu/GSC		05/01	check if Product Generation is up	*
 * E. Safford/SAIC	01/04	reroute event through operCb		*
 ***********************************************************************/
{
    if ( pgpalw_isUp() && pgundo_undoIsActv() ) {
	pgpalw_operCb( _palInfo.oper[FUNC_UNDO].wid, NULL, NULL );
    }
}
	
/*=====================================================================*/

void pgpalw_redo ( void )
/************************************************************************
 * pgpalw_redo								*
 *									*
 * Redoes the last saved operation.  Used for the redo hotkey.		*
 *									*
 * void pgpalw_redo ( )							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		04/01	create					*
 * J. Wu/GSC		05/01	check if Product Generation is up	*
 * E. Safford/SAIC	01/04	reroute event through operCb		*
 ***********************************************************************/
{
    if ( pgpalw_isUp() && pgundo_redoIsActv() ) {
	pgpalw_operCb( _palInfo.oper[FUNC_REDO].wid, NULL, NULL );
    }
}

/*=====================================================================*/

void pgpalw_refresh ( void )
/************************************************************************
 * pgpalw_refresh    							*
 *									*
 * This function refeshes the drawn pgen elements and signals for a     *
 * reload of all frames in the loop.  The current pixmap is cleared and *
 * restored from the master copy, then cvg_load is called to redraw the *
 * pgen elements.  The xw driver will then reload the pgen elements for *
 * all the remaining pixmaps in the loop.  This reload will take place  *
 * the next time they are displayed.					*
 *									*
 *									* 
 *									*
 * void pgpalw_refresh ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return value:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	09/01	extract refresh functionality from      *
 *					pgpalw_operCb			*
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC	   	12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	   	12/01   replace cvg_reload() with cvg_redraw()	*
 * T. Piper/SAIC	07/03	added XtDisplay and XtWindow		*
 * H. Zeng/SAIC		08/06	added call to pgdist_stop()		*
 * B. Yin/SAIC		11/06	removed call to pgpalw_setPrevOper()	*
 ***********************************************************************/
{
Widget		canvas;
Cardinal	width, height;
int		ier;
/*---------------------------------------------------------------------*/
/*
 * First destroy any overriding widget on the main window.
 */
    pgdist_stop ();

/* 
 * clear current display 
 */
    canvas = (Widget)mcanvw_getDrawingW();
    XtVaGetValues(canvas,
                   XmNwidth,  		&width,
                   XmNheight, 		&height,
                   NULL);

    XClearArea(XtDisplay(canvas), XtWindow(canvas), 0,0, width, height, False);

/*
 * for each frame, copy from master pixmap to displayable 
 * pixmaps in gemwindow.
 */
    xpgrestlp();

/*
 * tell xw driver to refresh the vector graphics
 * for all pixmaps in the loop.
 */
    xpgrfrsh();
    crg_rebuild();

/* 
 * load and plot the vg elements current frame.
 */
    cvg_redraw( NULL, &ier );
    geplot(&ier);
}

/*=====================================================================*/

void pgpalw_actvGrp ( void )
/************************************************************************
 * pgpalw_actvGrp                                                       *
 *                                                                      *
 * This function sets the _grpActv flag.                        	*
 *                                                                      *
 * void pgpalw_actvGrp ()                                               *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        08/01						*
 ***********************************************************************/
{
    _grpActv = TRUE;
    pggrpw_popup();
    pggrpw_startGrp();
}

/*=====================================================================*/

void pgpalw_inactvGrp ( void )
/************************************************************************
 * pgpalw_inactvGrp                                                     *
 *                                                                      *
 * This function resets the _grpActv flag.                        	*
 *                                                                      *
 * void pgpalw_inactvGrp ()                                             *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        08/01						*
 ***********************************************************************/
{
    _grpActv = FALSE;
    pggrpw_popdown();
}

/*=====================================================================*/

Boolean pgpalw_isGrpActv ( void )
/************************************************************************
 * pgpalw_isGrpActv                                                     *
 *                                                                      *
 * This function retrieves the _grpActv flag.                        	*
 *                                                                      *
 * Boolean pgpalw_isGrpActv ()                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *  pgpalw_isGrpActv	Boolean		value of _grpActv	        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        08/01						*
 ***********************************************************************/
{
    return( _grpActv );
}

/*=====================================================================*/

void pgpalw_actvLayer ( Boolean popup )
/************************************************************************
 * pgpalw_actvLayer                                                     *
 *                                                                      *
 * This function sets the "Add Layer" pixmap for LAYER button.          *
 *                                                                      *
 * void pgpalw_actvLayer ( popup )                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *     popup		Boolean		True  - pop up layer window	*
 *  					Flase - do not pop up  		*
 *                                                    			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        01/02						*
 * H. Zeng/EAI          02/02	fixed a bug on irix5&irix6		*
 * J. Wu/SAIC           12/02	pop up layer window only when desired	*
 * J. Wu/SAIC           12/02	add _actvLayer flag			*
 * S. Jacobs/NCEP	 3/06	Add check on widget before change pixmap*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    _layerActv = True;  /* activate the layering flag */
    
    for (ii=1;  ii <= MAX_OPERS; ii++) {
     
/*
 * Change pixmap for LAYER button.
 */
        if ( ii == FUNC_LAYER ) {

	   _palInfo.oper[ii].pxms[0] = _layerPxms[2];
	   _palInfo.oper[ii].pxms[1] = _layerPxms[3];

	   if  ( _palInfo.oper[ii].wid )  {
	   XtVaSetValues (_palInfo.oper[ii].wid,
		          XmNlabelPixmap, _palInfo.oper[ii].pxms[PXM_REV],
		          NULL);
	   }
           break;
        }
    }
    
    if ( popup ) pglayrw_popup();

    pgpalw_setBtnSntv (FUNC_SAVE_ALL, TRUE);    
}

/*=====================================================================*/

void pgpalw_inactvLayer ( void )
/************************************************************************
 * pgpalw_inactvLayer                                                   *
 *                                                                      *
 * This function resets the "Start Layer" pixmap for LAYER button.      *
 *                                                                      *
 * void pgpalw_inactvLayer ()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	        01/02						*
 * H. Zeng/EAI          02/02    fixed a bug on irix5&irix6             *
 * J. Wu/SAIC           12/02	 add _actvLayer flag			*
 * S. Jacobs/NCEP	 3/06	Add check on widget before change pixmap*
 ***********************************************************************/
{
    int		ii, cur_oper;
/*---------------------------------------------------------------------*/

    _layerActv = False;  /* deactivate the layering flag */    
    
    for (ii=1;  ii <= MAX_OPERS; ii++) {
     
/*
 * Change pixmap for LAYER button.
 */
        if ( ii == FUNC_LAYER ) {

	   _palInfo.oper[ii].pxms[0] = _layerPxms[0];
	   _palInfo.oper[ii].pxms[1] = _layerPxms[1];

           cur_oper = pgpalw_getCurOperId();

	   if  ( _palInfo.oper[ii].wid )  {
		if ( cur_oper == FUNC_LAYER ) {
		    XtVaSetValues (_palInfo.oper[ii].wid,
		             XmNlabelPixmap, _palInfo.oper[ii].pxms[PXM_REV],
		             NULL);
		}
		else {
		    XtVaSetValues (_palInfo.oper[ii].wid,
		             XmNlabelPixmap, _palInfo.oper[ii].pxms[PXM_REG],
		             NULL);
		}
	    }
            break;
	}
    }
    pgpalw_setBtnSntv (FUNC_SAVE_ALL, FALSE);    
}

/*=====================================================================*/

void pgpalw_setExitPGEN ( Boolean flag )
/************************************************************************
 * pgpalw_setExitPGEN							*
 *									*
 * Sets the _exitPGEN flag.						*
 *									*
 * void pgpalw_setExitPGEN ( flag )					*
 *									*
 * Input parameters:							*
 *	flag		Boolean		True = Exit PGEN		*
 *					False = Do not exit PGEN	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 ***********************************************************************/
{
    _exitPGEN = flag;
}

/*=====================================================================*/

Boolean pgpalw_isExitPGEN ( void )
/************************************************************************
 * pgpalw_isExitPGEN							*
 *									*
 * Returns the _exitPGEN flag.						*
 *									*
 * Boolean pgpalw_isExitPGEN (void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	pgpalw_isExitPGEN	Boolean	True/False 			*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 ***********************************************************************/
{
    return _exitPGEN;
}

/*=====================================================================*/

int pgpalw_getObjID ( char *obj_name )
/************************************************************************
 * pgpalw_getObjID							*
 *									*
 * This function gets the object ID asscicated with the object name.	*
 *									*
 * int pgpalw_getObjID ( obj_name )					*
 *									*
 * Input parameters:							*
 *	*obj_name		char	Object name			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	pgpalw_getObjID()	int	Object's ID			*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/02	initial coding				*
 ***********************************************************************/
{
    int    ii, obj_id;
/*---------------------------------------------------------------------*/
    
    obj_id = -1;
    
    for ( ii = 0; ii < MAX_OBJECTS; ii++ ) {
        if ( strcmp(obj_name, _palInfo.obj[ii].name) == 0 ) {
	    obj_id = ii;	    	    
	    break;
	} 
    }
    return obj_id;    
}

/*=====================================================================*/

Boolean pgpalw_isLayerActv ( void )
/************************************************************************
 * pgpalw_isLayerActv                                               	*
 *                                                                      *
 * This function retrieves the _layerActv flag.                        	*
 *                                                                      *
 * Boolean pgpalw_isLayerActv ()                               		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *  pgpalw_isLayerActv	Boolean		value of _layerActv	        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC	        12/02						*
 ***********************************************************************/
{
    return ( _layerActv );
}

/*=====================================================================*/

Boolean pgpalw_isGFAp_Actv ( void )
/************************************************************************
 * pgpalw_isGFA_P_Actv                                              	*
 *                                                                      *
 * This function retrieves the _GFAp_Actv flag.                        	*
 *                                                                      *
 * Boolean pgpalw_isGFAp_Actv ()                               		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *  pgpalw_isGFAp_Actv	Boolean		value of _GFAp_Actv	        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      03/07	initial coding				*
 ***********************************************************************/
{
    return ( _GFAp_Actv );
}

/*=====================================================================*/

void pgpalw_setClassMet( void )
/************************************************************************
 * pgpalw_setClassMet                                              	*
 *                                                                      *
 * This function sets the default CLASS to MET.                        	*
 *                                                                      *
 * void pgpalw_setClassMet ()                               		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      04/08	initial coding				*
 ***********************************************************************/
{
    if ( _palInfo.cur_class != CLASS_MET ) {

       pgpalw_setCurClass( _palInfo.class[ CLASS_MET ].wid );
       pgpalw_manageObjPal( _palInfo.cur_class );
       pgpalw_setupOper();

    }
}

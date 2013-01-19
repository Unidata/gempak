#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmapprm.h"
#include "nmap_data.h"
#include "nmap_gtbl.h"
#include "nmap_stbl.h"
#define VGFTBL_GLOBAL
#include "vgftbl.h"
#include <Xm/Separator.h>

#define MAX_DIR_LAYER	 12	/* maximum directory layers */
#define GRID_LAYER	  6 	/* # of grid layers */
#define MAX_LAYER_LEN	 40	/* maximum string length for each layer */
#define ENS_LAYER	  4 	/* # of ensemble layers */

#define MAX_MISC_STR	 20	/* maximum string length for misc table */
#define MAX_NMISC	  5	/* maximum number of items in misc table */
#define MAXALIAS	150	/* max number of parameter aliases */

#define DATA_SOURCE	  0
#define DATA_CATEGORY	  1
#define VGF_EXTENT	".vgf"

#define VISIBLE_ITEM0     20	/* # of visible items in layer 2 */
#define VISIBLE_ITEM      20	/* # of visible items in other layers */

#define GRID_CAT_TBL	  "grid_cat.tbl"	/* Grid category table   */ 
#define MODEL_GROUP_TBL	  "model_group.tbl"	/* Model group table   */ 
#define NUMCYC		   4	/* Number of cycles for selection */ 

#define TOPOFFSET	  15

typedef struct {
    Widget	titlew;			/* title widget of the layer */
    Widget	framew;			/* frame/(form for MOTIF1.1) widget */
    Widget	listw;			/* list widget of the layer */
    char	title[MAX_LAYER_LEN];	/* title string of the layer */
    char	dirpath[MAX_STRLEN];	/* directory path of the layer */
}dslwLayer_t;

typedef struct {
	int cat;
	int grid;
	int group;
	int prod;
	char cycle[20];
}grdProd_t;	   /* structure to record selected grid product */

typedef struct {
    int	 ngrd;
    char catnam[MAX_LAYER_LEN];
    char gridnam[MAXALIAS][MAX_LAYER_LEN]; 
} gridcat_t;

typedef struct {
    Widget      aliasw;         /* alias                                */
    Widget	arroww;		/* flag to show ensemble members	*/
    Widget	firstw;		/* flag to first model			*/
    Widget	weightw[NUMCYC];	/* wieght of alias/member		*/
    Widget	cycleListw[NUMCYC]; /* cycles to be selected		*/
} ensel_t;

typedef struct {
    char  alias[MAX_LAYER_LEN]; 	/* alias name */
    int   ncycle;			/* number of available cycles */
    dattm_t cycles[NUMCYC];	
    int   nmember;			/* number of members in this alias */
    char  member[MAXALIAS][MAX_LAYER_LEN]; /* ensemble member names	  */
    ensel_t  my_alias;
    Widget   member_form;
    ensel_t  *my_member;	
}ensInfo_t;

static gridcat_t   *_gridCat;   /* grid catetory table structure */
static gridtbl_t   _gridTbl;	/* grid table structure */
static gridtbl_t   _ensTbl;	/* grid table structure */
static grdProd_t   _selectGrdProd;	/* grid table structure */
static grdProd_t   _selectEnsProd;	/* grid table structure */
static modgrp_t    _modgrp;		/* Model group structure */

static ensInfo_t  *_ensdata;
static int	   _aliasNum;	/* Number of alias in the table	*/
static Widget      _enselpopW; 
static Widget      _showSelect, _modelList; 
static char	   _cycIdx[40];

static Widget     _dslpopW;       /* data selection popup window */
static Widget     _applyButton;   /* apply button widget ID */

static char	  _topData; /* indicates whether the toplevel is a category */  
static char	  _dsrcList[FILE_FULLSZ];   /* list of data sources for the category */

static char	  _selectMosCycle[20];	   /* MOS data cycle */
static char	  _firstSelMdl[MAXALIAS][MAX_LAYER_LEN];  /* First selected model */

static char       _dirPath[FILE_FULLSZ];   /* data file full path */
static char       _dsName[FILE_FULLSZ];    /* data source name for data bar */
static char       _gdDirNam[FILE_FULLSZ];    /* data source name for data bar */

static int         _currCatg;
static int         _validCat;	/* number of valid grid categories  */

static dslwLayer_t _dslwLayers[MAX_DIR_LAYER]; /* each layer of data selection */
static int	  _sourceType;

static char	  _mscUsrTbl[MAX_NMISC][MAX_MISC_STR];
static int	  _mscNitems;

static Boolean	  _imageIsUp;
static int	  _imagePos;

static int      _arrowDir       = XmARROW_DOWN;

/*
 * These are the data categories that are presented to the user.  The order
 * matches the order of the CAT_* defines in gemprm.h.  Change the _catgStr
 * contents ONLY if the CAT_* defines change.
 *
 * NONE is added as a place holder, since the defines (CAT_) start with one.
 *
 * MAX_CATG needs to remain equal to the number in this list
 */
static char	  *_catgStr[] = {"NONE", "IMAGE", "SURF_OBS", "SURF_FCST", 
				 "UAIR_OBS", "UAIR_FCST", 
				 "GRID", "VGF", "MISC", "ENSEMBLES"};
#define MAX_CATG	10
/*
 *  This holds the last path used for each category 
 */
static char	  _storedPath[MAX_CATG][FILE_FULLSZ];

/*
 * Private function.
 */
static void dslw_readGdCat( void );

/*
 *  private functions -- callback
 */
void dslw_ctlBtnCb ( Widget, long, XtPointer );
void dslw_listCb (   Widget, long, XmListCallbackStruct* );
void dslw_ensMemberCb ( Widget wid, long which, XtPointer cbs );
void dslw_ensSelCtlBtnCb ( Widget wid, long which, XtPointer cbs );
void dslw_showSelCb ( Widget wid, long which, XtPointer cbs );
void dslw_firstCb ( Widget wid, long which, XtPointer cbs );

/*
 *  private functions -- action
 */
int dslw_checkVGF ( void );	/* never called? */
int dslw_getDataCatg ( char   *data_name );
void dslw_getVGFuser (int iusr, char *usrtitle, char *fname );/* never called? */
void dslw_layerSet ( int layer );
void dslw_setSelect ( char *dspath );
static Boolean dslw_topLayerOk ( const char *layer );
void dslw_getEnsDt ( void );
Widget dslw_ensSelCreate ( Widget parent );
static void dslw_cycSelCreate ( Widget pane, long ens_idx, char *model, dattm_t ecycles[], Boolean hasMember, ensel_t *ensw );
void dslw_getEnsSel ( char *mdlist, int *iret );
void dslw_setFirst (int mod, int mem,  char *list1, char *list2 );
void dslw_clearAll ( void );
void dslw_setEnsSel ( void );

/************************************************************************
 * nmap_dslw.c								*
 *									*
 * This module defines the date source selection popup widget for nmap.	* 
 *									*
 * CONTENTS:                                                            *
 *  dslw_readTable()	read auxiliary tables for data selection.	*
 *  dslw_create()	creates the data source selection popup.	*
 *  dslw_popup()	pop up the data source selection window.	*
 *  dslw_popdown()	close the data source selection window.		*
 *									*
 *  dslw_isUp()		check if data source selection window is up.	*
 *  dslw_getGrdResfile() get grid restore file name.			*
 *  dslw_getVGFpathByTitle() get VGF directory by user title.		*
 *  dslw_getFrstMod()	get the first selected alias			*
 *  dslw_getEnsResfile() get ensembles restore file name.               *
 *  dslw_getModList()	get the ensemble model list			*
 *									*
 *  dslw_listCb()	callback for each list selection layer.		*
 *  dslw_ctlBtnCb()	callback for control buttns at bottom.		*
 *									*
 *  dslw_layerSet()	function to set the specified layer.		*
 *  dslw_getDataCatg()	gets the data category number			*
 *  dslw_getVGFuser()	get VGF user information. 			*
 *  dslw_checkVGF()	check whether there is any VGF user		*
 *  dslw_setSelect()	sets the directory selections			*
 *  dslw_validSrc()	confirms existence of the data selection path 	*
 *  dslw_topLayerOk()	validates the contents of the layer string	*
 *  dslw_getGrdCycle    Get GRID data cycle information              	*
 ***********************************************************************/

/*=====================================================================*/

static void dslw_readGdCat( void )
/************************************************************************
 * dslw_readGdCat							*
 *                                                                      *
 * This function reads the grid category table for data selection.	*
 *                                                                      *
 * void dslw_dslw_readGdCat( )                             		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/07						*
 * S. Jacobs/NCEP	 9/09	Increased MAXALIAS from 50 to 150	*
 ***********************************************************************/
{
int	ii, jj, kk, ng, ier, nn, nline, ipos;
char    *ptr, record[512], buffer[512], grid[MAX_LAYER_LEN];
char	tmpstr[MAX_LAYER_LEN], tmpcat[MAX_LAYER_LEN], missgd[MAXALIAS][MAX_LAYER_LEN];
Boolean is_in;
FILE    *ftbl;
/*---------------------------------------------------------------------*/
    _validCat = 0;

    ftbl = cfl_tbop( GRID_CAT_TBL, "nmap", &ier);
    if ( ftbl == NULL ) return;

/*
 * Get number of grid categories.
 */
    nline = 0;
    while ( !feof(ftbl) ) {
        cfl_trln(ftbl, 512, record, &ier);
        if ( ier == 0 )  nline++;
    }

    if ( nline != 0 ) {
	G_MALLOC ( _gridCat, gridcat_t, (nline+2), "_gridCat" );
    }
    else {
	fclose ( ftbl );
        return;
    }

    rewind ( ftbl );
    
/*
 * Read grid names for each category.
 */
    nn = 0;
    ng = 0;
    while ( nn < nline ) {
	cfl_trln(ftbl, 512, record, &ier);

  	if ( ier == 0 ) {
	    strcpy(tmpcat, "");
	    strcpy(buffer, "");
	    sscanf(record, "%s %s", tmpcat, buffer);

	    _gridCat[ng].ngrd = 0;
	    ptr = strtok(buffer, ";");
	    while (ptr) {
		strcpy(grid, ptr);

/* 
 * Check for invalid grid name.
 */
		for (ii = 0; ii < _gridTbl.ngrids; ii++) {
		    cst_nocc(_gridTbl.grids[ii]->grdnam, ':', 1, 0, &ipos, &ier);
	
		    if ( ier == 0 ) {
			cst_ncpy(tmpstr, _gridTbl.grids[ii]->grdnam, ipos, &ier);
		    }
		    else {
			strcpy(tmpstr, _gridTbl.grids[ii]->grdnam);
		    }

		    if ( strcmp(grid, tmpstr) == 0 ) {
			strcpy( _gridCat[ng].gridnam[_gridCat[ng].ngrd], _gridTbl.grids[ii]->grdnam );
			_gridCat[ng].ngrd++;
		    }
                }

		ptr = strtok(NULL, ";");
	    }

	    if ( _gridCat[ng].ngrd > 0 ) {
	 	strcpy ( _gridCat[ng].catnam, tmpcat );
		ng++;
	    }
	}

	nn++;
    }

    if ( ng == 0 ) return;

    _validCat = ng;

/*
 * Find mising grids.
 */
    nn = 0;
    for ( ii = 0; ii < _gridTbl.ngrids; ii++) {
	is_in = False;

	for ( jj = 0; jj < _validCat; jj++ ) {
	    for ( kk = 0; kk < _gridCat[jj].ngrd; kk++ ) {
		if ( strcmp(_gridTbl.grids[ii]->grdnam, _gridCat[jj].gridnam[kk]) == 0 ) {
		    is_in = True;
		    break;	
		}
	    }

	    if ( is_in ) break;
	}		

        if ( !is_in ) {
	    strcpy(missgd[nn], _gridTbl.grids[ii]->grdnam);
	    nn++;
	    if ( nn > MAXALIAS ) break;
	} 
    } 

   if ( nn <= 0 ) return;

/*
 * Create "Others" category using missing grids. If "Others" already
 * exists in the table, append missing grids onto "Others".
 */
    is_in = False;
    for (  jj = 0; jj < _validCat; jj++ ) {
	strcpy ( tmpstr, _gridCat[jj].catnam );
	cst_uclc ( tmpstr, tmpstr, &ier );
	
  	if ( strcmp (tmpstr, "others") == 0 || strcmp (tmpstr, "other") == 0 ) {
	    for ( kk = 0; kk < nn; kk++ ) {
		strcpy (_gridCat[jj].gridnam[kk+_gridCat[jj].ngrd], missgd[kk]);
	    } 	
	    _gridCat[jj].ngrd += nn;
	    is_in = True;

	    break;
	}
    } 


    if ( !is_in ) {
	strcpy( _gridCat[_validCat].catnam, "Others" );
	_gridCat[_validCat].ngrd = nn;

        for ( ii = 0; ii < nn; ii++ ) {
	    strcpy(_gridCat[_validCat].gridnam[ii], missgd[ii]);
        }
	_validCat++;
    }
}

/*=====================================================================*/
void dslw_readTable ( char *dsrcnam, char *tblname )
/************************************************************************
 * dslw_readTable                                                    	*
 *                                                                      *
 * This function reads the auxiliary table for data selection (defined  *
 * in dsrc.nmap table) when needed.   					*
 *                                                                      *
 * void dslw_readTable ( dsrcnam, tblname )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *	*dsrcnam	char	data source name 			*
 *	*tblname	char	table name				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      5/97                            			*
 * C. Lin/EAI      2/98		add FFG                            	*
 * C. Lin/EAI      3/98		remove station model tables             *
 * I. Durham/GSC   5/98		changed underscore decl. to an include	*
 * S. Law/GSC		10/99	added call to msc_readUsrTbl		*
 * S. Law/GSC		10/99	changed to use _catgStr			*
 * S. Jacobs/NCEP	12/99	Removed call to msc_readUsrTbl		*
 * S. Jacobs/NCEP	 3/00	Renamed header file nmap_vtbl to vgftbl	*
 * T. Piper/SAIC	 2/02	Fixed memory leak - made vgftbl global	*
 * M. Li/SAIC            2/07   Added dslw_readGdCat                    *
 * M. Li/SAIC            2/08   Added gtbl_readEnsTbl                   *
 ***********************************************************************/
{
    char	msgstr[100];
    int		iret;
/*---------------------------------------------------------------------*/

    iret = 0;
    if ( strcmp(dsrcnam, _catgStr[CAT_GRD]) == 0 ) { 
	gtbl_init(tblname, &_gridTbl, &iret);
	strcpy(msgstr, _catgStr[CAT_GRD]);
	dslw_readGdCat();
        gtbl_readEnsTbl(MODEL_GROUP_TBL, &_modgrp, &_ensTbl, &iret );
    }
    else if ( strcmp(dsrcnam, _catgStr[CAT_VGF]) == 0 ) {
	vtbl_readUsrTbl (tblname, &iret);
	strcpy(msgstr, _catgStr[CAT_VGF]);
    }


    if ( iret != 0 ) {
	printf("Fatal Error: error in reading %s table %s\n",
	       msgstr, tblname);
	exit(2);
    }
}

/*=====================================================================*/

Widget dslw_create ( Widget parent )
/************************************************************************
 * dslw_create                                                          *
 *                                                                      *
 * This function creates the data source editting window.               *
 *                                                                      *
 * Widget dslw_create(parent)                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * parent	Widget   parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * dslw_create	Widget   Widget ID for data selection popup window      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * C. Lin/EAI      	01/97   modify to make _dslwTbl[] table driven  *
 * C. Lin/EAI      	03/97   remove dslw.nmap table;			*
 *			  	remove data category toggle selection   *
 * G. Krueger/EAI  	10/97   Removed unused NxmFrameLabel declaration*
 * G. Krueger/EAI  	10/97   NxmControlBtn->NxmCtlBtn_create 	*
 * C. Lin/EAI      	06/98   add model cycle initialization        	*
 * S. Jacobs/NCEP  	11/98   Added MOS cycle initialization		*
 * E. Safford/GSC	12/98	add XmVERSION to REVISION check         *
 * S. Law/GSC		10/99	added image category check		*
 * S. Law/GSC		10/99	changed to use _catgStr			*
 * S. Law/GSC		12/99	made fix to _imagePos value		*
 * S. Law/GSC		07/00	initialize _storedPath array		*
 * T. Piper/GSC		07/01	freed btnw				*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 * T. Piper/SAIC	10/05	declared ii long			*
 * T. Piper/SAIC	03/06	modified for bluecurve bug		*
 * M. Li/SAIC		03/08	Added ENSEMBLES				*
 * M. Li/SAIC		05/08	Added ENSEMBLES	picker			*
 ***********************************************************************/
{
    int		jj, kk, mm, nn, count;
    long	ii;
    Widget	pane, form, button; 
    char	*btnstr[] = {"Accept", "Close"}, dsrclist[FILE_FULLSZ];
    XmStringTable xmstr;
#ifdef AWIPS
    Widget      dslwrcw;
#endif
/*---------------------------------------------------------------------*/

    _dslpopW = XmCreateFormDialog(parent, "dslw_popup",	NULL, 0);
    XtVaSetValues(_dslpopW, 
    		XmNnoResize, 			True, 
		XmNdialogStyle,			XmDIALOG_APPLICATION_MODAL,
		NULL);

    XtVaSetValues(XtParent(_dslpopW), XmNtitle, "Data Source", NULL);

    pane = XtVaCreateWidget("_paneW",
			    xmPanedWindowWidgetClass,	_dslpopW,
			    XmNorientation,		XmHORIZONTAL,
			    XmNsashWidth,		1,
			    XmNsashHeight,		1,
			    NULL);

    dslw_ensSelCreate ( pane );


#ifdef AWIPS
    dslwrcw = XtVaCreateManagedWidget("dslwrc",
			xmRowColumnWidgetClass,		pane,
			XmNnumColumns,			3,
			XmNpacking,			XmPACK_COLUMN,
			NULL);

/*
 * create the layers
 */
    for (ii = 0; ii < MAX_DIR_LAYER; ii ++) {

	_dslwLayers[ii].framew = XtVaCreateWidget("frame",
				xmFrameWidgetClass, dslwrcw,
				XmNshadowType,	XmSHADOW_ETCHED_OUT,
				NULL);
#else
/* 
 * create the layers
 */
    for (ii = 0; ii < MAX_DIR_LAYER; ii ++) {

        _dslwLayers[ii].framew = XtVaCreateWidget("frame",
                                xmFrameWidgetClass, pane,
                                XmNshadowType,  XmSHADOW_ETCHED_OUT,
                                NULL);
#endif

	_dslwLayers[ii].titlew = 
	    XtVaCreateManagedWidget("",
				    xmLabelGadgetClass, _dslwLayers[ii].framew,
				    XmNchildType,	XmFRAME_TITLE_CHILD,
				    XmNchildVerticalAlignment, 
				    XmALIGNMENT_WIDGET_TOP,
				    NULL);

/*
 * create scrolled list for displaying the contents of the
 * directory.
 */
	_dslwLayers[ii].listw = XmCreateScrolledList(_dslwLayers[ii].framew, 
						    "list", NULL, 0);
#ifdef AWIPS
	XtVaSetValues(XtParent(_dslwLayers[ii].listw),
			XmNheight,      200,
                        XmNwidth,       250,
			NULL);
#endif
	XtVaSetValues(_dslwLayers[ii].listw,
		      XmNselectionPolicy,  XmSINGLE_SELECT,
		      XmNscrollingPolicy,  XmAUTOMATIC,
		      NULL);

	XtManageChild(_dslwLayers[ii].listw);

	XtAddCallback(_dslwLayers[ii].listw, 
		      XmNsingleSelectionCallback,
		      (XtCallbackProc)dslw_listCb, (XtPointer)ii);

#ifdef AWIPS
        XtManageChild(_dslwLayers[ii].framew);
#endif
}

    _imagePos = -1;
    _imageIsUp = FALSE;
    nn = XtNumber (_catgStr);
    mm = nn - 1;	/* skips over NONE */
    xmstr = (XmStringTable) XtMalloc ((size_t)mm * sizeof(XmString *));
    for (ii = 1, jj = 0; ii < nn; ii++) { 
	kk = (int)ii;
   	ctb_dtlist (&kk, dsrclist, &count); 
	if (count > 0 || ii == CAT_ENS ) {
	    xmstr[jj] = XmStringCreateLocalized (_catgStr[ii]);
	    jj++;

	    if (strcmp (_catgStr[ii], _catgStr[CAT_IMG]) == 0) {
		_imagePos  = jj;
		_imageIsUp = TRUE;
	    }
	}
    }

    XtVaSetValues(_dslwLayers[0].listw,
		  XmNitems,		xmstr,
		  XmNitemCount,		jj,
		  XmNvisibleItemCount,	25,
		  NULL);

/*
 * Set the title for the first pane
 */
    NxmLabel_setStr (_dslwLayers[0].titlew, "DATA TYPES");
    XtManageChild   (_dslwLayers[0].titlew);


#ifndef AWIPS
    XtManageChild(_dslwLayers[0].framew);
#endif
    for (ii = 0; ii < jj; ii++) {
	XmStringFree(xmstr[ii]);
    }
    XtFree((XtPointer)xmstr);

    for (ii = 0; ii < MAX_CATG; ii++) {
	strcpy (_storedPath[ii], "");
    }

/*
 * create the control buttons
 */
    nn = XtNumber (btnstr);
    form = XtVaCreateWidget("form",
                            xmFormWidgetClass, pane,
			    XmNfractionBase,   (nn * 100),
			    XmNmarginHeight,   20,
                            NULL                       ); 

    for ( ii = 0; ii < nn; ii++ )  {

	button = XtVaCreateManagedWidget ( btnstr[ii], 
			xmPushButtonWidgetClass, form,
			XmNheight,               25,
			XmNwidth,                100,
			XmNleftAttachment,	 XmATTACH_FORM,
                        XmNtopAttachment,        XmATTACH_POSITION,
			XmNtopPosition,          (ii * 100 + 40),
			XmNbottomAttachment,     XmATTACH_POSITION,
			XmNbottomPosition,       ((ii + 1) * 100 - 40),
			NULL );
   
        if ( ii == 0 )  _applyButton = button; 

	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)dslw_ctlBtnCb, (XtPointer)ii );
    }

    XtManageChild(form);
    XtManageChild(pane);

    _selectGrdProd.cycle[0] = '\0';
    _selectMosCycle[0] = '\0';

    return(_dslpopW);
}

/*=====================================================================*/

void dslw_popup ( int dcatg, char *path )
/************************************************************************
 * dslw_popup								*
 *									*
 * This function pops up the data selection popup window.		*
 *									*
 * void dslw_popup (dcatg, path, xx, yy )				*
 *									*
 * Input parameters:							*
 *	dcatg	int 		category of data                	*
 *      path    char*           path to data                            *
 *	xx	Position	upper left corner x coord.		*
 *	yy	Position	upper left corner y coord.		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/96						*
 * C. Lin/EAI		01/97	modify to make _dslwTbl[] table driven	*
 * C. Lin/EAI		03/97	major change to remove data cat. select	*
 * C. Lin/EAI		04/97	automatic position the popup selection	*
 * C. Lin/EAI		06/97	add VGF data type			*
 * C. Lin/EAI		12/97	modified to deal with IMAGE/RAD...	*
 * C. Lin/EAI		04/98	use dsrc name and selmode		*
 * C. Lin/EAI		06/98	remove strtok, add model cycle process	*
 * C. Lin/EAI		08/98	fix potential memory leak(S.DANZE/AWC)	*
 * S. Law/GSC		12/98	added domflag parameter			*
 * S. Jacobs/NCEP	 5/99	Added WTCH_WARN data type		*
 * S. Law/GSC		10/99	added image category check		*
 * S. Jacobs/NCEP	10/99	Added nindex to checks for SAT and RAD	*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop   	*
 * S. Law/GSC		11/99	updated to use new data table		*
 * H. Zeng/EAI          06/00   changed passed-in parameters            *
 * S. Law/GSC		01/00	moved menu setup to dslw_setSelect and	*
 *				changed image add/delete location	*
 * E. Safford/GSC	06/01	remove dslw_getDataCatg()               *
 * M. Li/SAIC           02/07   different inputs for dslw_setSelect     *
 * H. Zeng/SAIC		07/07	added codes to set the window position	*
 ***********************************************************************/
{
    char	dspath[FILE_FULLSZ], dsrclist[FILE_FULLSZ];
    char        rootdata[FILE_FULLSZ], currpath[FILE_FULLSZ];
    char	*ptr, *catg;
    int		ii, ier, loop, nindex;
    Widget	top_w;
    Position	pos_x, pos_y;
    Boolean	list_image = FALSE;
    XmString	xmstr;
/*---------------------------------------------------------------------*/
/*
 * set the window position 
 */
    top_w = mainw_getToplevel ();
    XtVaGetValues (top_w, XmNx, &pos_x, XmNy, &pos_y, NULL);

    XtVaSetValues(_dslpopW,
		  XmNdefaultPosition,	False,
		  XmNx,			pos_x,
		  XmNy,			(pos_y+98),
		  NULL);

    _currCatg = dcatg;
    strcpy(currpath, path);
    path = NULL;

    loop = loop_getCurLoop ();
    list_image = (Boolean)(( ! ( ( dataw_isSatSelect (loop, &nindex) ) ||
		       ( dataw_isRadSelect (loop, &nindex)))));

    strcpy(dspath, currpath);
    catg = strtok (dspath, "/");

    if (_currCatg >0 && strcmp (catg, _catgStr[CAT_IMG]) == 0) {
	list_image = TRUE;
    }

    if (_imagePos != -1) {
	if (list_image && !_imageIsUp) {
	    xmstr = XmStringCreateLocalized (_catgStr[CAT_IMG]);
	    XmListAddItem (_dslwLayers[0].listw, xmstr, _imagePos);
	    XmStringFree (xmstr);

	    _imageIsUp = TRUE;
	}
	else if (!list_image && _imageIsUp) {
	    XmListDeletePos (_dslwLayers[0].listw, _imagePos);

	    _imageIsUp = FALSE;
	}
    }

/*
 *   New Data Source
 */
    if (_currCatg <= 0) {
        _sourceType = NEW_SOURCE;
	XmListDeselectAllItems (_dslwLayers[0].listw);

	for (ii = 1; ii < MAX_DIR_LAYER; ii++) {
#ifdef AWIPS
	    NxmLabel_setStr(_dslwLayers[ii].titlew, "");
	    XtUnmanageChild(_dslwLayers[ii].listw);
#else
	    XtUnmanageChild(_dslwLayers[ii].framew);
#endif
	}
        XtSetSensitive (_applyButton, FALSE);
    }
    else {

/*
 *  Modify a previously selected source 
 */        
        _sourceType = MOD_SOURCE;

	strcpy (dspath, currpath);

  	_currCatg = dcatg; 

        ctb_dtlist (&_currCatg, dsrclist, &ier); 

/*
 * find out whether the top level is a category
 */
        _topData = (char)((strcmp(dsrclist, "NONE") == 0) ? 
	    DATA_SOURCE : DATA_CATEGORY);

/*
 * pass the data source list through layer 1
 */
  	if ( _topData == DATA_CATEGORY ) {
	    strcpy(_dsrcList, dsrclist);
	}
	strcpy(_dslwLayers[1].title, catg);

	if ( _currCatg != CAT_VGF ) {
	    ctb_dtpath ("VGF", rootdata, &ier);
	    css_envr(rootdata, _dslwLayers[1].dirpath, &ier);
	}
	else {
	    ptr = strchr(currpath, '/');
	    strcpy(_dslwLayers[1].dirpath, ptr+1);
	}

	for (ii = 1; ii < MAX_DIR_LAYER; ii++) {
#ifdef AWIPS
            NxmLabel_setStr(_dslwLayers[ii].titlew, "");
            XtUnmanageChild(_dslwLayers[ii].listw);
#else
            XtUnmanageChild(_dslwLayers[ii].framew);
#endif
	}

	if ( _currCatg == CAT_GRD && _validCat > 0 )
            dslw_setSelect (_gdDirNam);
	else
	    dslw_setSelect (currpath);
    }
    XtManageChild(_dslpopW);
}

/*=====================================================================*/

void dslw_popdown ( void )
/************************************************************************
 * dslw_popdown								*
 *									*
 * This function unmanages the data selection window.			*
 *									*
 * void dslw_popdown()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		01/97						*
 * S. Law/GSC		07/00	added check if popup is managed		*
 * M. Li/SAIC		05/08	popdown ensemble selection window	*
 ***********************************************************************/
{
    if (XtIsManaged(_dslpopW)) {
	XtUnmanageChild(_dslpopW);
    }

    if ( dslw_ensSelIsUp() ) dslw_ensSelPopdown();
}

/*=====================================================================*/

Boolean dslw_isUp ( void )
/************************************************************************
 * dslw_isUp                                                            *
 *                                                                      *
 * This function checks if the data source selection window is up.      *
 *                                                                      *
 * Boolean dslw_isUp()                                               	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * dslw_isUp	Boolean          True -- up,    False -- down           *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      01/97 						*
 ***********************************************************************/
{
    return (XtIsManaged(_dslpopW));
}

/*=====================================================================*/

void dslw_getGrdResfile ( char *dsname, char *resfile )
/************************************************************************
 * dslw_getGrdResfile                                                   *
 *                                                                      *
 * Function to get the restore file name based on the grid selection   *
 * path shown on the data bar.   					*
 *                                                                      *
 * dslw_getGrdResfile ( dsname, resfile )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      *dsname     char       grid selection path name                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *resfile    char       restore file for the specified grid     *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            04/97                                          *
 * C. Lin/EAI            06/98   add cycle infomation                   *
 * S. Law/GSC		11/99	updated to use new data table		*
 ***********************************************************************/
{
    int		ii, grd, grp, prd, index, ret;
    char	buffer[FILE_FULLSZ], *ptr, *prdname, *prdpath;

/*---------------------------------------------------------------------*/

    resfile[0] = '\0';

    grd = -1;
    grp = -1;
    prd = -1;
    strcpy(buffer, dsname);

/*
 * search for grid
 */
    ptr = strtok(buffer, "/"); /* discard the pre-string */
    ptr = strtok(NULL, "/");
    for (ii = 0; ii < _gridTbl.ngrids; ii++) {
	if (strcmp(ptr, _gridTbl.grids[ii]->grdnam) == 0) {
	    grd = ii;
	    break;
	}
    }
    if (grd == -1) return;

/*
 * skip cycle
 */
    ptr = strtok(NULL, "/");

/*
 * search for group
 */
    ptr = strtok(NULL, "/");

    for (ii = 0; ii < _gridTbl.grids[grd]->ngroups; ii++) {
	if (strcmp(ptr, _gridTbl.grids[grd]->groups[ii]->grpnam) == 0) {
	    grp = ii;
	    break;
	}
    }
    if (grp == -1) return;

/*
 * search for product
 */
    ptr = strtok(NULL, "/");

    for (ii = 0; ii < _gridTbl.grids[grd]->groups[grp]->nprods; ii++){
	index = _gridTbl.grids[grd]->groups[grp]->iprod[ii];
	ret = gtbl_getProd(index, &prdname, &prdpath);
	if (ret == 0 && strcmp (ptr, prdname) == 0) {
	    prd = ii;
	    break;
	}
    }
    if (prd == -1) return;

/*
 * set the data directory string
 */
    strcpy(resfile, prdpath);
}

/*=====================================================================*/

void dslw_getEnsResfile ( char *dsname, char *resfile )
/************************************************************************
 * dslw_getEnsResfile                                                   *
 *                                                                      *
 * Function to get the ensemble restore file name based on the model 	*
 * selection path shown on the data bar.   				*
 *                                                                      *
 * dslw_getEnsResfile ( dsname, resfile )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      *dsname     char       model selection path name                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *resfile    char       restore file for specified model group   *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/08						*
 ***********************************************************************/
{
    int		ii, grd, grp, prd, index, ret;
    char	buffer[FILE_FULLSZ], *ptr, *prdname, *prdpath;

/*---------------------------------------------------------------------*/

    resfile[0] = '\0';

    grd = -1;
    grp = -1;
    prd = -1;
    strcpy(buffer, dsname);

/*
 * search for grid
 */
    ptr = strtok(buffer, "/"); /* discard the pre-string */
    ptr = strtok(NULL, "/");
    for (ii = 0; ii < _ensTbl.ngrids; ii++) {
	if (strcmp(ptr, _ensTbl.grids[ii]->grdnam) == 0) {
	    grd = ii;
	    break;
	}
    }
    if (grd == -1) return;

/*
 * search for group
 */
    ptr = strtok(NULL, "/");

    for (ii = 0; ii < _ensTbl.grids[grd]->ngroups; ii++) {
	if (strcmp(ptr, _ensTbl.grids[grd]->groups[ii]->grpnam) == 0) {
	    grp = ii;
	    break;
	}
    }
    if (grp == -1) return;

/*
 * search for product
 */
    ptr = strtok(NULL, "/");

    for (ii = 0; ii < _ensTbl.grids[grd]->groups[grp]->nprods; ii++){
	index = _ensTbl.grids[grd]->groups[grp]->iprod[ii];
	ret = gtbl_getEnsProd(index, &prdname, &prdpath);
	if (ret == 0 && strcmp (ptr, prdname) == 0) {
	    prd = ii;
	    break;
	}
    }
    if (prd == -1) return;

/*
 * set the data directory string
 */
    strcpy(resfile, prdpath);
}

/*=====================================================================*/

void dslw_getModList ( char *dsname, char *mlist, char *cycIdx )
/************************************************************************
 * dslw_getModList                                                   	*
 *                                                                      *
 * Function to get the ensemble model list string based on the model 	*
 * selection path shown on the data bar.   				*
 *                                                                      *
 * dslw_getModList( dsname,  mlist, cycIdx )                       	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dsname     char       	model selection path name         	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*mlist	    char	model list				*
 *	*cycIdx	    char 	cycle index string			*	
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		03/08						*
 * M. Li/SAIC		05/08	modlist -> selModlst			*
 ***********************************************************************/
{
    int		ii;
    char	buffer[FILE_FULLSZ], *ptr;

/*---------------------------------------------------------------------*/

    mlist[0]  = '\0';
    cycIdx[0]  = '\0';

    strcpy(buffer, dsname);

    ptr = strtok(buffer, "/"); /* discard the pre-string */
    ptr = strtok(NULL, "/");
    for (ii = 0; ii < _modgrp.numgrp; ii++) {
	if (strcmp(ptr, _modgrp.modgrp[ii]) == 0) {
	    strcpy ( mlist, _modgrp.selModLst[ii] );
	    break;
	}
    }

    if ( strlen(_cycIdx) > 0 ) strcpy ( cycIdx, _cycIdx );
}

/*=====================================================================*/

void dslw_getFrstMod ( char *dsname, char *falias )
/************************************************************************
 * dslw_getFrstMod                                                 	*
 *                                                                      *
 * Function to get the first alias based on the model 			*
 * selection path shown on the data bar.   				*
 *                                                                      *
 * dslw_getFrstMod( dsname, falias )                       		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dsname     char       	model selection path name         	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*falias	    char	first selected model in model list	*	
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		03/08						*
 * M. Li/SAIC		05/08	Get falias from GUI selection		*
 ***********************************************************************/
{
    int		ii, grd, ipos1, ipos2, lens, ier;
    char	buffer[FILE_FULLSZ], *ptr, *ptr1, tmpstr[LLMXLN];
    char	mlist[LLMXLN], str1[80];

/*---------------------------------------------------------------------*/

    falias[0] = '\0';

    grd = -1;
    strcpy(buffer, dsname);

/*
 * search for model group 
 */
    ptr = strtok(buffer, "/"); /* discard the pre-string */
    ptr = strtok(NULL, "/");
    for (ii = 0; ii < _modgrp.numgrp; ii++) {
	if (strcmp(ptr, _modgrp.modgrp[ii]) == 0) {
	    if ( _firstSelMdl[ii][0] != CHNULL ) {
		strcpy ( falias, _firstSelMdl[ii] );
		return;
	    } else {
	        strcpy ( mlist, _modgrp.selModLst[ii] );
	        grd = ii;
	    }
	    break;
	}
    }
    if (grd == -1) return;

/*
 * get the first alias from the model list string
 */
    cst_nocc ( mlist, '{', 1, 0, &ipos1, &ier );
    cst_nocc ( mlist, '}', 1, 0, &ipos2, &ier );
    cst_ncpy ( tmpstr, mlist+ipos1+1, ipos2-ipos1-1, &ier );

    if ( strchr (tmpstr, ',') == NULL &&  strchr (tmpstr, '|') == NULL ) {
	strcpy ( str1, tmpstr );	
    } 
    else if ( strchr (tmpstr, ',') == NULL &&  strchr (tmpstr, '|') != NULL ) {
	ptr = strtok ( tmpstr, "|" );
	strcpy ( str1, ptr );
    }
    else if ( strchr (tmpstr, ',') != NULL ) {
	ptr = strtok ( tmpstr, "," );
 	ptr1 = strtok ( tmpstr, "|" );
	if ( ptr1 != NULL )
	    strcpy ( str1, ptr1 );
	else
	    strcpy ( str1, ptr );
    } 

    cst_rmbl ( str1, str1, &lens, &ier );
    cst_nocc ( str1, '%', 1, 0, &ipos1, &ier );
    if ( ier == 0 ) {
	strcpy ( falias, str1+ipos1+1 );
    }
    else {
	strcpy ( falias, str1 );
    }

    if ( strlen(falias) < 3 ) return;

}

/*=====================================================================*/

void dslw_getVGFpathByTitle ( char *usrtitle, char *path )
/************************************************************************
 * dslw_getVGFPathByTitle                                               *
 *                                                                      *
 * This routine will return the VGF file directory path for the         *
 * user specified by its title.                                         *
 *                                                                      *
 * void dslw_getVGFpathByTitle ( usrtitle, path )                       *
 *                                                                      *
 * Input parameters:                                                    *
 *      *usrtitle        char            user title name                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *path            char            VGF data directory path        *
 *                                                                      *
 * Return code:                                                         *
 *              index to the default user                               *
 *                                      -1 - error                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            5/97                                           *
 * S. Law/GSC		01/00	changed _vgfUsrTbl to be a pointer	*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/

    path[0] = '\0';

    for (ii = 0; ii < _vgfUsrTbl.nitems; ii++) {
	if (strcmp(usrtitle, _vgfUsrTbl.items[ii].title) == 0 ){
	    strcpy(path, _vgfUsrTbl.items[ii].usrpath);
	    break;
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void dslw_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_ctlBtnCb							*
 *									*
 * Callback function for control buttons on the bottom.			*
 *									*
 * void dslw_ctlBtnCb (wid, which, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/96						*
 * C. Lin/EAI		01/97	add Model case				*
 * C. Lin/EAI		02/97	close window before setting time	*
 * C. Lin/EAI		04/97	modified for taking out data category	*
 *				selections data type			*
 * C. Lin/EAI		05/97	special treatment for VGF when close	*
 * G. Krueger/EAI	09/97	Changed NxmWarning -> NxmWarn_show	*
 * C. Lin/EAI		04/98	use dsrc name and selmode		*
 * S. Law/GSC		12/98	added call to dataw_resetFrame		*
 * E. Safford/GSC	06/99	update for new data handling scheme	*
 * H. Zeng/EAI          06/00   changed dataw_setDataSrc() parameters   *
 * S. Law/GSC		07/00	save the path				*
 * E. Safford/GSC	12/00	inactivate the Accept button to prevent *
 *				   duplicate data set loads		*
 * M. Li/SAIC		05/08	popdown ensemble selection window	*
 ***********************************************************************/
{

    switch( which ) {

      case 0:			/* Accept */

        XtSetSensitive (_applyButton, FALSE);
  	dataw_setDataSrc(_sourceType, _currCatg, _dsName);
	strcpy (_storedPath[_currCatg], _dsName);

	break;

      case 1:			/* Cancel (close window) */

	break;

    }
    XtUnmanageChild(_dslpopW);
    if ( dslw_ensSelIsUp() ) dslw_ensSelPopdown();
}

/*=====================================================================*/

void dslw_listCb ( Widget wid, long layer, XmListCallbackStruct *cbs )
/************************************************************************
 * dslw_listCb                                                          *
 *                                                                      *
 * Callback function for the scrolled list selection.                   *
 *                                                                      *
 * void dslw_listCb ( wid, layer, cbs )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid	Widget			The widget ID			*
 *      layer	long             	The layer number		*
 *      *cbs	XmListCallbackStruct	callback data structure		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            06/96                                          *
 * C. Lin/EAI            01/97	add _propagateMode for MODEL            *
 * C. Lin/EAI            02/97	modify code to use MODEL table          *
 * C. Lin/EAI            04/97	modified for new data selection scheme  *
 * C. Lin/EAI            05/97	add case SHIP  				*
 * C. Lin/EAI            05/97	add case VGF  				*
 * C. Lin/EAI            12/97	modifed to add RADAR  			*
 * C. Lin/EAI            02/98	modifed to add FFG  			*
 * C. Lin/EAI            04/98	use dsrc name and selmode  		*
 * C. Lin/EAI            06/98	replace free with XtFree (S.Danz/AWC)  	*
 * C. Lin/EAI            06/98	add MODEL cycle info  			*
 * S. Jacobs/NCEP	 11/98	Added MOS data selection type		*
 * S. Jacobs/NCEP	  5/99	Added WTCH_WARN data type		*
 * S. Law/GSC		10/99	_WWN -> _MSC and updated how it works	*
 * S. Law/GSC		11/99	updated to use new data table		*
 * S. Law/GSC		01/00	changed _vgfUsrTbl to be a pointer	*
 * H. Zeng/EAI          06/00   changed to use _currCatg                *
 * S. Law/GSC		07/00	added call to dslw_setSelect		*
 * M. Li/SAIC           02/07   Add additional layer for GRID           *
 * M. Li/SAIC           05/08   popup ensemble selection window		* 
 * m.gamazaychikov/CWS	01/10	add handlng of AWIPS II db stored images*
 ***********************************************************************/
{
    int		ii, nselect, next_layer, cmp_layer, ier, position;
    char	pname[FILE_FULLSZ], *name, *ptr, dsrclist[FILE_FULLSZ]; 
    struct stat	buf;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, XmNselectedItemCount, &nselect, NULL);
    if ( nselect == 0 ) return;

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &name);

    if (layer == 0) {
	_currCatg = dslw_getDataCatg (name);

   	ctb_dtlist (&_currCatg, dsrclist, &ier); 

  	if ( strcmp(dsrclist, "NONE") == 0 ) { 
	    _topData = DATA_SOURCE;
	}
	else {
	    _topData = DATA_CATEGORY;
	}


	if ( _topData == DATA_CATEGORY ) {
	    strcpy(_dsrcList, dsrclist);
	}

	next_layer = (int)layer + 1;
	strcpy(_dslwLayers[next_layer].title, name);

    }
    else {
  	if (layer == 1 && _topData == DATA_CATEGORY) {

	    ctb_dtpath (name, pname, &ier);

	    if (_currCatg != CAT_VGF) { 
		css_envr(pname, _dslwLayers[layer].dirpath, &ier);
  	    }
	    else {
		strcpy(_dslwLayers[layer].dirpath, pname);
  	    }
	}
	
	position = cbs->item_position-1;
  	switch( _currCatg ) {

  	  case CAT_SFF:  /* MOS: station model + model times */
  	  case CAT_SNF:
  
	    if  ( layer == 2 ) {
		strcpy ( _selectMosCycle, name );
		ptr  = strchr ( _selectMosCycle, '_' );
		*ptr = '/';
	    }

	    next_layer = (int)layer + 1;
	    strcpy(_dslwLayers[next_layer].title, name);

	    break;
  
  	  case CAT_IMG:  	/* directory driven */

	    if ( _topData == DATA_CATEGORY && layer == 1 ) {
		next_layer = (int)layer + 1;
		strcpy(_dslwLayers[next_layer].title, name);
		strcpy(_dslwLayers[next_layer].dirpath, 
		       _dslwLayers[layer].dirpath);
	    }
	    else {
		sprintf(pname, "%s/%s", _dslwLayers[layer].dirpath, name);
/*
 *              Filesystem storage case
 */
                if ( strstr (_dslwLayers[layer].dirpath, "A2DB") == NULL ) {
		   ier = stat(pname, &buf);
		   if ( ier < 0) { 
		      XtFree(name);
		      return;
		   }

/*
 * if it is directory, set the next level
 */
  		   if ( S_ISDIR(buf.st_mode) ) {
		      next_layer = (int)layer + 1;
		      strcpy(_dslwLayers[next_layer].title, name);
		      strcpy(_dslwLayers[next_layer].dirpath, pname);
		  }
	        }
/*
 *              AWIPS II database storage case
 */
                else {
                    next_layer = (int)layer + 1;
                    strcpy(_dslwLayers[next_layer].title, name);
                    strcpy(_dslwLayers[next_layer].dirpath, pname);
	        }

	    }
	    break;

	  case CAT_SFC:	/* table driven */
	  case CAT_SND:
	    next_layer = (int)layer + 1;
	    strcpy(_dslwLayers[next_layer].title, name);

	    break;


	  case CAT_GRD:  /* GRID table driven */

	    cmp_layer = (_validCat == 0) ? (layer + 1) : layer;

	    switch( cmp_layer ) {
	      case 1:
                if ( _validCat > 0 )_selectGrdProd.cat = position;
                break;

	      case 2:
		if ( _validCat > 0 ) {
		    for (ii = 0; ii < _gridTbl.ngrids; ii++) { 
                        if (strcmp(name, _gridTbl.grids[ii]->grdnam) == 0) {
			    _selectGrdProd.grid = ii;
			    break;
		        }
                    }
		}
		else {
		    _selectGrdProd.grid = position;
		}
		break;

	      case 3:
		strcpy(_selectGrdProd.cycle, name);
		ptr = strchr(_selectGrdProd.cycle, '_');
		*ptr = '/';
		break;

	      case 4:
		_selectGrdProd.group = position;
		break;

	      case 5:
		_selectGrdProd.prod = position;
		break;
	    }

	    next_layer = (int)layer + 1;

	    strcpy(_dslwLayers[next_layer].title, name);

	    break;

	  case CAT_ENS:  /* ENSEMBLES table driven */
	    switch( layer ) {
	      case 1:
		if ( dslw_ensSelIsUp() ) dslw_ensSelPopdown();
		_selectEnsProd.grid = position;
		dslw_ensSelPopup();
		break;

	      case 2:
		_selectEnsProd.group = position;
		if ( dslw_ensSelIsUp() ) dslw_ensSelPopdown();
		break;

	      case 3:
		_selectEnsProd.prod = position;
		break;
	    }

	    next_layer = (int)layer + 1;

	    strcpy(_dslwLayers[next_layer].title, name);

	    break;

  	  case CAT_VGF:	/* VGF table + directory */
  
	    next_layer = (int)layer + 1;

	    strcpy(_dslwLayers[next_layer].title, name);

	    if ( layer == 1 ) {
		strcpy(_dslwLayers[next_layer].dirpath, 
		       _vgfUsrTbl.items[position].usrpath);
	    }

	    break;

   	  case CAT_MSC:	/* watch/warning */

	    next_layer = (int)layer + 1;
	    strcpy (_dslwLayers[next_layer].title, name);
  	    break;
	}
    }
  
    for (ii = next_layer; ii < MAX_DIR_LAYER; ii++) {
#ifdef AWIPS
            NxmLabel_setStr(_dslwLayers[ii].titlew, "");
            XtUnmanageChild(_dslwLayers[ii].listw);
#else
            XtUnmanageChild(_dslwLayers[ii].framew);
#endif
    }

    dslw_layerSet(next_layer);

    if (layer == 0 && strlen (_storedPath[_currCatg]) > (size_t)0) {
	if ( _currCatg == CAT_GRD && _validCat > 0 ) 
	    dslw_setSelect (_gdDirNam);
	else
	    dslw_setSelect (_storedPath[_currCatg]);
    }
    XtFree(name);
}

/*=====================================================================*/

void dslw_layerSet ( int layer )
/************************************************************************
 * dslw_layerSet                                                        *
 *                                                                      *
 * This function sets the title and the scrolled list for displaying    *
 * directory contents.                                                  *
 *                                                                      *
 * dslw_layerSet(layer)                                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      layer    int             The layer number                       *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            12/95                                          *
 * E. Wehner/EAi         01/97  Added parameter to cfl_rdir             *
 * C. Lin/EAI            01/97	add _propagateMode for MODEL            *
 * C. Lin/EAI            02/97	add constant VISIBLE_ITEMS              *
 * C. Lin/EAI            02/97	process MODEL table              	*
 * C. Lin/EAI            04/97	modified for new data selection scheme  *
 * C. Lin/EAI            05/97	add case SHIP  				*
 * C. Lin/EAI            06/97	add case VGF  				*
 * C. Lin/EAI            07/97	check currrent directory for VGF layer 0*
 * G. Krueger/EAI	 09/97	Changed NxmWarning -> NxmWarn_show	*
 * G. Krueger/EAI	 10/97	NxmSetLabel->NxmLabel_setStr		*
 * G. Krueger/EAI	 10/97	CST_xLST: Removed RSPTB; Add str limit	*
 * C. Lin/EAI            12/97	add RAD					*
 * C. Lin/EAI            02/98	add FFG					*
 * C. Lin/EAI            03/98	modified to use station model table	*
 * C. Lin/EAI            04/98	modified to use selmode			*
 * C. Lin/EAI            06/98	add model cycle				*
 * C. Lin/EAI            06/98	check visible>0 before set VISIBLE	*
 * C. Lin/EAI            08/98  fixed potential memory leak with dsrcstr*
 * S. Jacobs/NCEP	 11/98	Added MOS data selection type		*
 * S. Jacobs/NCEP	  5/99	Added WTCH_WARN data type		*
 * S. Law/GSC		10/99	_WWN -> _MSC and updated how it works	*
 * S. Law/GSC		11/99	updated to use new data table		*
 * S. Law/GSC		01/00	changed _vgfUsrTbl to be a pointer	*
 * H. Zeng/EAI          06/00   changed to use _currCatg                *
 * Chiz/Unidata		12/00	increased gdclst size 1024->4096	*
 * Chiz/Unidata          2/01   increased gdclst size 4096->12000       *
 * S. Jacobs/NCEP	 1/01	Changed call to ctb_pllist		*
 * S. Jacobs/NCEP	 1/01	Reverse order for cycle times		*
 * E. Safford/GSC	06/01   init nn=0 and no free on xmstr if nn=0  *
 * B. Yin/SAIC		05/05	allocate memory for tmptim 		*
 * T. Piper/SAIC	07/06	Add st_null after gd_gcyc		*
 * M. Li/SAIC           02/07   Add additional layer for GRID           *
 * T. Piper/SAIC	10/07	Add if test before freeing dnamelist	*
 * M. Li/SAIC           07/08   Correct ensemble layer setting		* 
 ***********************************************************************/
{
    char	dsrclist[FILE_FULLSZ], msg[MAX_STRLEN];
    int		ii, i0, jj, nn, nsdir, visible, def_vis, grd, grd0, grp, prd, cat;
    int		lens, index, ipos1, ipos2, iret, cmp_layer, max_layer;
    char	*prdname, *prdpath, **dsrcstr, tmpstr[16];
    char	pmlist[MAXALIAS][16], grdname[20], gdclst[12000];
    char	moslst[1024];
    dattm_t	*tmptim;
    XmStringTable xmstr;
    struct dirent **dnamelist=NULL;
/*---------------------------------------------------------------------*/

    nn = 0;

    if (layer >= MAX_DIR_LAYER){
	sprintf(msg, "Error: the directory layer exceeds max = %d\n",
		MAX_DIR_LAYER);
	NxmWarn_show(_dslpopW, msg);
	return;
    }

    if ( layer == 1 && _topData == DATA_CATEGORY ) {

	strcpy(dsrclist, _dsrcList); 
	dsrcstr = (char **) malloc(sizeof(char *) * MAXDSRC_OF_DCAT);
	for (ii = 0; ii < MAXDSRC_OF_DCAT; ii++)
	    dsrcstr[ii] = (char *) malloc(40);

	cst_clst(dsrclist, ';', " ", MAXDSRC_OF_DCAT, 40,
		 dsrcstr, &nn, &iret );

	xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));

	for (ii = 0; ii < nn; ii++) {
	    xmstr[ii] = XmStringCreateLocalized(dsrcstr[ii]);
	}

/*
 * free the strings
 */
	for (ii = 0; ii <  MAXDSRC_OF_DCAT; ii++) {
	    free(dsrcstr[ii]);
	}
	free(dsrcstr);

    }
    else {

	XtSetSensitive(_applyButton, False);

  	switch( _currCatg ) {

  	  case CAT_SFC: 	/* station model driven */
  	  case CAT_SND:

	    if (layer == 3) {

/* 
 * process last layer 
 */ 
  		strcpy(_dirPath, _dslwLayers[1].dirpath);  
		strcpy(_dsName, _dslwLayers[1].title);  

		for (ii = 2; ii <= layer; ii++) {
		    strcat(_dsName, "/");  
		    strcat(_dsName, _dslwLayers[ii].title);  
		}

		XtSetSensitive(_applyButton, True);

		return;
	    }

	    ctb_pllist(_dslwLayers[layer].title, MAXALIAS,
	    		pmlist, &nn, &iret);

	    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
	    for (ii = 0; ii < nn; ii++) { 
		if ( strcmp(pmlist[ii], _dslwLayers[layer].title) == 0 ) {
		    xmstr[ii] = XmStringCreateLocalized("standard");
		}
		else {
		    cst_uclc(pmlist[ii], tmpstr, &iret);
		    xmstr[ii] = XmStringCreateLocalized(tmpstr);
		}
	    }

	    break;

	  case CAT_SFF: 	/* MOS: station model + model times */
	  case CAT_SNF:

  	    if ( layer == 4 ) {

/* 
 * process last layer 
 */ 
  		strcpy(_dirPath, _dslwLayers[1].dirpath);  
		strcpy(_dsName, _dslwLayers[1].title);  

		for(ii = 2; ii <= layer; ii++) {
		    strcat(_dsName, "/");  
		    strcat(_dsName, _dslwLayers[ii].title);  
		}

		XtSetSensitive(_applyButton, True);

		return;
	    }
	    else if  ( layer == 2 )  {

/*
 * process cycle times
 */
  		strcpy(grdname, _dslwLayers[layer].title);
		gd_gcyc(grdname,";", &nn, moslst, &iret,
			strlen(grdname), 1, sizeof(moslst) );
		st_null(moslst, moslst, &lens, &iret, sizeof(moslst), sizeof(moslst));
		cst_rmbl(moslst, moslst, &lens, &iret);

		G_MALLOC ( tmptim, dattm_t, nn, "dslw_layerSet() tmptim" );

		ipos1 = 0;
		for  ( ii = 0; ii < nn; ii++ )  {
		    cst_nocc ( moslst, ';', ii+1, 1, &ipos2, &iret );
		    if  ( iret != 0 )  ipos2 = (int)strlen(moslst) + 1;
		    cst_ncpy ( tmptim[ii], &moslst[ipos1],
		    	       ipos2-ipos1, &iret );
		    cst_rpst ( tmptim[ii], "/", "_", tmptim[ii], &iret );
		    ipos1 = ipos2 + 1;
		}

		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = nn-1, jj = 0; ii >= 0; ii--, jj++) { 
		    xmstr[jj] = XmStringCreateLocalized ( tmptim[ii] );
		}
		G_FREE ( tmptim, dattm_t );
	    }
	    else {
		ctb_pllist(_dslwLayers[2].title, MAXALIAS,
			   pmlist, &nn, &iret);

		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = 0; ii < nn; ii++) { 

		    if (strcmp(pmlist[ii], _dslwLayers[2].title) == 0) {
			xmstr[ii] = XmStringCreateLocalized("standard");
		    }
		    else {
			cst_uclc(pmlist[ii], tmpstr, &iret);
			xmstr[ii] = XmStringCreateLocalized(tmpstr);
		    }
		}
	    }

	    break;

	  case CAT_IMG: 	/* directory driven */

/*
 * get directory contents, if file and dir co-exist, 
 * ignore the file.
 */
  	    nn = cfl_rdir(1, _dslwLayers[layer].dirpath, NULL,
			  &dnamelist, &nsdir);

            NxmErr_update();

	    if (nn == 0) { 

/* 
 * process last layer 
 */ 
  		strcpy(_dirPath, _dslwLayers[layer].dirpath);  
		strcpy(_dsName, _dslwLayers[1].title);  

		for (ii = 2; ii <= layer; ii++) {
		    strcat(_dsName, "/");  
		    strcat(_dsName, _dslwLayers[ii].title);  
		}

		XtSetSensitive(_applyButton, True);
	    }

	    if (nn <= 0) return;

/*
 * set the scrolled list
 */
  	    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
	    for (ii = 0; ii < nn; ii++) {
		xmstr[ii] = XmStringCreateLocalized(dnamelist[ii]->d_name);
		free(dnamelist[ii]);
	    }
	    if ( dnamelist != NULL )  free(dnamelist);

	    break;
  
	  case CAT_GRD:	/* grid table driven */

	    max_layer = (_validCat == 0) ? (GRID_LAYER - 1) : GRID_LAYER;
	    i0 = (_validCat == 0) ? 2 : 3;
  	    if ( layer == max_layer ) { 

/* 
 * process last layer 
 */ 
  		grd = _selectGrdProd.grid;
		grp = _selectGrdProd.group;
		prd = _selectGrdProd.prod;
		index = _gridTbl.grids[grd]->groups[grp]->iprod[prd];

		if (gtbl_getProd(index, &prdname, &prdpath) == 0) {

		    strcpy(_dirPath, prdpath); 
		    strcpy(_dsName, _dslwLayers[1].title);  

		    for (ii = i0; ii <= layer; ii++) {
			strcat(_dsName, "/");  
			strcat(_dsName, _dslwLayers[ii].title);  
		    }

		    if ( _validCat > 0 ) {
		        strcpy(_gdDirNam, _dslwLayers[1].title);  
		        for (ii = 2; ii <= layer; ii++) {
                            strcat(_gdDirNam, "/"); 
                            strcat(_gdDirNam, _dslwLayers[ii].title);
                        } 
		    }
		    XtSetSensitive(_applyButton, True);
		}
		return;
	    }

/*
 * set the scrolled list
 */
	    cmp_layer = (_validCat == 0) ? (layer + 1) : layer;
  	    switch( cmp_layer ) {

	      case 1:

		if ( _validCat > 0 ) {
		    nn = _validCat;
		    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		    for (ii = 0; ii < nn; ii++) {
                        xmstr[ii] = 
                            XmStringCreateLocalized(_gridCat[ii].catnam);
                    }
		}
		break;

	      case 2:
		if ( _validCat == 0 ) {
		    nn = _gridTbl.ngrids;
		    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		    for (ii = 0; ii < nn; ii++) { 
		        xmstr[ii] = 
			    XmStringCreateLocalized(_gridTbl.grids[ii]->grdnam);
		    }
		}
		else {
		    cat = _selectGrdProd.cat;
                    nn =  _gridCat[cat].ngrd; 
                    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
                    for (ii = 0; ii < nn; ii++) { 
                        xmstr[ii] = 
                            XmStringCreateLocalized(_gridCat[cat].gridnam[ii]);
                    }
		}
		break;

	      case 3:
		strcpy(grdname, _dslwLayers[layer].title);
		gd_gcyc(grdname,";", &nn, gdclst, &iret,
			strlen(grdname), 1, sizeof(gdclst) );
		st_null(gdclst, gdclst, &lens, &iret, sizeof(gdclst), sizeof(gdclst));
		cst_rmbl(gdclst, gdclst, &lens, &iret);

		G_MALLOC ( tmptim, dattm_t, nn, "dslw_layerSet() tmptim" );

		ipos1 = 0;
		for  ( ii = 0; ii < nn; ii++ )  {
		    cst_nocc ( gdclst, ';', ii+1, 1, &ipos2, &iret );
		    if  ( iret != 0 )  ipos2 = (int)strlen(gdclst) + 1;
		    cst_ncpy ( tmptim[ii], &gdclst[ipos1],
		    	       ipos2-ipos1, &iret );
		    cst_rpst ( tmptim[ii], "/", "_", tmptim[ii], &iret );
		    ipos1 = ipos2 + 1;
		}

		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = nn-1, jj = 0; ii >= 0; ii--, jj++) { 
		    xmstr[jj] = XmStringCreateLocalized ( tmptim[ii] );
		}

		G_FREE ( tmptim, dattm_t );
		break;

	      case 4:
		grd = _selectGrdProd.grid; 
		nn  = _gridTbl.grids[grd]->ngroups;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn *sizeof (XmString *));
		for (ii = 0; ii < nn; ii ++) { 
		    xmstr[ii] = XmStringCreateLocalized
			(_gridTbl.grids[grd]->groups[ii]->grpnam);
		}
		break;

	      case 5:
		grd = _selectGrdProd.grid;
		grp = _selectGrdProd.group;
		nn  = _gridTbl.grids[grd]->groups[grp]->nprods;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = 0; ii < nn; ii++) { 
		    index = _gridTbl.grids[grd]->groups[grp]->iprod[ii];
		    gtbl_getProd(index, &prdname, &prdpath);
		    xmstr[ii] = XmStringCreateLocalized( prdname );
		}
	    }

	    break;

	  case CAT_ENS:	/* ENSEMBLES table driven */

	    grd0 = _selectEnsProd.grid;
  	    grd  = -1;		
            for ( ii = 0; ii < _ensTbl.ngrids; ii++ ) {
                if (strcmp(_modgrp.modgrp[grd0], _ensTbl.grids[ii]->grdnam) == 0) {
                    grd = ii;
                    break;
                }
            }
	    grp = _selectEnsProd.group;
	    prd = _selectEnsProd.prod;


  	    if ( layer == ENS_LAYER ) { 

/* 
 * process last layer 
 */ 
		index = _ensTbl.grids[grd]->groups[grp]->iprod[prd];

		if (gtbl_getEnsProd(index, &prdname, &prdpath) == 0) {

		    strcpy(_dirPath, prdpath); 
		    strcpy(_dsName, _dslwLayers[1].title);  


		    for (ii = 2; ii <= layer; ii++) {
			strcat(_dsName, "/");  
			strcat(_dsName, _dslwLayers[ii].title);  
		    }
		    XtSetSensitive(_applyButton, True);
		}
		return;
	    }

/*
 * set the scrolled list
 */
  	    switch( layer ) {

	      case 1:
		nn = _modgrp.numgrp;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = 0; ii < nn; ii++) {
		    xmstr[ii] = XmStringCreateLocalized(_modgrp.modgrp[ii]);
		}

		break;

	      case 2:
		if ( grd >= 0 ) {
		    nn  = _ensTbl.grids[grd]->ngroups;
		    xmstr = (XmStringTable) XtMalloc ((size_t)nn *sizeof (XmString *));
		    for (ii = 0; ii < nn; ii ++) { 
		        xmstr[ii] = XmStringCreateLocalized
			    (_ensTbl.grids[grd]->groups[ii]->grpnam);
		    }
		}
		break;

	      case 3:
		nn  = _ensTbl.grids[grd]->groups[grp]->nprods;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = 0; ii < nn; ii++) { 
		    index = _ensTbl.grids[grd]->groups[grp]->iprod[ii];
		    gtbl_getEnsProd(index, &prdname, &prdpath);
		    xmstr[ii] = XmStringCreateLocalized( prdname );
		}
	    }

	    break;

  	  case CAT_VGF:	/* VGF table + directory */

  	    if ( layer == 3 ) { /* last layer */

/* 
 * process last layer 
 */ 
  		strcpy(_dsName, _dslwLayers[1].title);  

		for (ii = 2; ii <= layer; ii++) {
		    strcat(_dsName, "/");  
		    strcat(_dsName, _dslwLayers[ii].title);  
		}
		strcpy(_dirPath, _dsName);  

		XtSetSensitive(_applyButton, True);

		return;

	    }

	    switch( layer ) {

	      case 1:
		vtbl_checkCurDir();
		nn = _vgfUsrTbl.nitems;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));
		for (ii = 0; ii < nn; ii ++) { 
		    xmstr[ii] = XmStringCreateLocalized
			(_vgfUsrTbl.items[ii].title);
		}

		break;

	      case 2:

/*
 *  get ".vgf" files, 
 */
  		nn = cfl_rdir(0, _dslwLayers[layer].dirpath, 
			      VGF_EXTENT, &dnamelist,&nsdir);
		if (nn > 0) {
/*
 *  set the scrolled list
 */
  		    xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof(XmString *));
		    for (ii = 0; ii < nn; ii++) { 
			xmstr[ii] = XmStringCreateLocalized
			    (dnamelist[ii]->d_name);
			free(dnamelist[ii]);
		    }
		    if ( dnamelist != NULL )  free(dnamelist);
		}
		else {
		    sprintf(msg, "No VGF file in directory: %s",
			    _dslwLayers[layer].dirpath);
		    NxmWarn_show(_dslpopW, msg);
		    return;
		}
	    }

	    break;

	  case CAT_MSC:	/* watch/warning */
	    if (layer == 1) {
		nn = _mscNitems;
		xmstr = (XmStringTable) XtMalloc ((size_t)nn * sizeof (XmString *));

		for (ii = 0; ii < nn; ii++) {
		    xmstr[ii] = XmStringCreateLocalized(_mscUsrTbl[ii]);
		}
	    }
	    else {
 		strcpy(_dirPath, _dslwLayers[1].dirpath);  
    	        strcpy(_dsName, _dslwLayers[1].title);  

		strcat(_dsName, "/");
		strcat(_dsName, _dslwLayers[2].title);  


	    XtSetSensitive(_applyButton, True);

	    return;
	    }

	    break;
	} 
    }

/*
 * set the title  
 */
    NxmLabel_setStr (_dslwLayers[layer].titlew, 
		     _dslwLayers[layer].title);
    XtManageChild (_dslwLayers[layer].titlew);

/*
 * set up the list for this layer
 */
    if (layer == 1) {
	def_vis = VISIBLE_ITEM0;
    }
    else {
	def_vis = VISIBLE_ITEM;
    }

    if (nn < def_vis) {
	visible = nn;
    }
    else {
	visible = def_vis;
    }

    XtVaSetValues(_dslwLayers[layer].listw,
		  XmNitems,            xmstr,
		  XmNitemCount,        nn,
		  NULL);

    if (visible > 0) {
	XtVaSetValues(_dslwLayers[layer].listw,
		      XmNvisibleItemCount, visible,
		      NULL);
    }

    for (ii = 0; ii < nn; ii++) {
	XmStringFree(xmstr[ii]);
    }
    if (nn > 0) {
        XtFree((XtPointer)xmstr); 
    }

/*
 * clean up the selections
 */
    XmListDeselectAllItems(_dslwLayers[layer].listw);
    XmListSetPos(_dslwLayers[layer].listw, 1);

#ifdef AWIPS
    XtManageChild(_dslwLayers[layer].listw);
#else
    XtManageChild(_dslwLayers[layer].framew);
#endif
}

/*=====================================================================*/

int dslw_getDataCatg ( char *data_name )
/************************************************************************
 * dslw_getDataCatg  	                                                *
 *                                                                      *
 * This function gets data category number that corresponds to the      *
 * data_name.								*
 *                                                                      *
 * int dslw_getDataCatg ( data_name )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * 	*data_name	char	data category (source) name             *
 *                                                                      *
 * Output parameters:                                                   *
 * dslw_getDataCatg	int	data category number or 		*
 *					CAT_NIL if not matched		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/99	initial coding                    	*
 * S. Law/GSC		11/99	moved dcatg_getDataCatg -> dslw_getD...	*
 * S. Law/GSC		11/99	rewrote to use _catgStr			*
 ***********************************************************************/
{
    int			catg;
    unsigned int	ii;
/*---------------------------------------------------------------------*/

    catg = CAT_NIL; 

    for (ii = 0; ii < XtNumber (_catgStr); ii++) {
	if (strcmp (data_name, _catgStr[ii]) == 0) {
	    catg = (int)ii;
	    break;
	}
    }

    return (catg);
}

/*=====================================================================*/

void dslw_getVGFuser ( int iusr, char *usrtitle, char *fname )
/************************************************************************
 * dslw_getVGFuser                                                   	*
 *                                                                      *
 * Function to get the VGF user information based on the index to the   *
 * VGF table structure. It returns the user title and the latest VGF    *
 * file in the user directory.   					*
 *                                                                      *
 * dslw_getVGFuser ( iusr, usrtitle, fname )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *       iusr     int        index to the user table              	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *usrtitle   char       user title name				*
 *      *fname      char       latest VGF file in that directory     	*
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            05/97                                          *
 * S. Law/GSC		01/00	changed _vgfUsrTbl to be a pointer	*
 ***********************************************************************/
{
    char	vgfname[FILE_FULLSZ];

/*---------------------------------------------------------------------*/

    usrtitle[0] = '\0';
    fname[0] = '\0';

    if (iusr < 0 || iusr >= _vgfUsrTbl.nitems) return;

    strcpy(usrtitle, _vgfUsrTbl.items[iusr].title);

    vgf_getLatestFile(_vgfUsrTbl.items[iusr].usrpath, vgfname);

    if (vgfname[0] != '\0') strcpy(fname, vgfname);

}

/*=====================================================================*/

int dslw_checkVGF ( void )
/************************************************************************
 * dslw_checkVGF                                                        *
 *                                                                      *
 * This function first checks if there is any valid VGF user.		*
 *                                                                      *
 * int dslw_checkVGF()                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * dslw_checkVGF	int    1 = there is VGF file, 0 = no VGF file   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           6/97                                            *
 * S. Law/GSC		01/00	changed _vgfUsrTbl to be a pointer	*
 ***********************************************************************/
{
    if (_vgfUsrTbl.nitems) {
	return (1);
    }
    else {
	return (0);
    }
}

/*=====================================================================*/

void dslw_getMosCycle ( char *cycle )
/************************************************************************
 * dslw_getMosCycle                                                   	*
 *                                                                      *
 * Function to get the cycle for the  file name based on the MOS	*
 * selection path shown on the data bar.   				*
 *                                                                      *
 * dslw_getMosCycle ( cycle )                               		*
 *                                                                      *
 * Input parameters:                                                    *
 *		NONE							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *cycle    char       MOS cycle info     			*
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	11/98	Created					*
 ***********************************************************************/
{
    cycle[0] = '\0';
    if (_selectMosCycle[0] != '\0') strcpy (cycle, _selectMosCycle);
}

/*=====================================================================*/

void dslw_setSelect ( char *dspath )
/************************************************************************
 * dslw_setSelect							*
 *									*
 * This function selects the various directories in _dslpopW based on	*
 * dspath.								*
 *									*
 * void dslw_setSelect(dspath)						*
 *									*
 * Input parameters:							*
 *	*dspath		char	current path				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/00	moved from dslw_popup			*
 * E. Safford/GSC	06/01	remove extraneous dslw_layerSet(1) call *
 ***********************************************************************/
{
    char	**items;
    int		ii, nn, nitems, ier;
    XmString	xmstr;
    static Boolean on_hold = FALSE;
/*---------------------------------------------------------------------*/

    if (on_hold) return;
    on_hold = TRUE;

    items = (char **) malloc (sizeof (char *) * MAX_DIR_LAYER);
    for (ii = 0; ii < MAX_DIR_LAYER; ii++) {
	items[ii] = (char *) malloc (MAX_LAYER_LEN);
    }

    cst_clst (dspath, '/', " ", MAX_DIR_LAYER, MAX_LAYER_LEN, 
	      items, &nn, &ier);
    for (ii = 0; ii < nn; ii++) {
	if (strcmp (items[ii], "*") != 0) {
	    xmstr = XmStringCreateLocalized (items[ii]);
	    XmListSelectItem (_dslwLayers[ii].listw, xmstr, TRUE);
	    XmListSetBottomItem (_dslwLayers[ii].listw, xmstr);
	    XmStringFree (xmstr);
	}
	else { /* for grid cycle data */

	    XtVaGetValues (_dslwLayers[ii].listw,
			   XmNitemCount, &nitems,
			   NULL);
	    if (nitems > 0) {
		XmListSelectPos (_dslwLayers[ii].listw, nitems, TRUE);
		XmListSetBottomPos (_dslwLayers[ii].listw, nitems);
	    }
	}
    } 

    for (ii = 0; ii < MAX_DIR_LAYER; ii++) {
	free(items[ii]);
    }
    free (items);
    on_hold = FALSE;
}

/*=====================================================================*/

Boolean dslw_validSrc ( int dcatg, char *dspath )
/************************************************************************
 * dslw_validSrc							*
 *									*
 * This function confirms the data selection path as valid (TRUE) or 	*
 * invalid (FALSE).  Note that a NULL string is not considered valid. 	*
 *									*
 * Boolean dslw_validSrc (dcatg, dspath )				*
 *									*
 * Input parameters:							*
 *	dcatg		int	data category number			*
 *	*dspath		char	current path				*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	dslw_validSrc	Boolean	TRUE = valid, FALSE = invalid path	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/01	initial coding                          *
 * E. Safford/GSC	08/01	fix misc validation			*
 * E. Safford/GSC	10/01	fix img validation			*
 * M. Li/SAIC		02/08	added ensemble case			*
 ***********************************************************************/
{
    Boolean	src_ok = TRUE;

    char	*pstart, **layer, **dsrcstr, path[FILE_FULLSZ] = "\0";
    char	dummy[FILE_FULLSZ] = "\0", dsrclist[FILE_FULLSZ];

    char	actual_path[FILE_FULLSZ] = "\0";
    char	res_file[FILE_FULLSZ] = "\0";

    char	file[MAX_LAYER_LEN],  alias[MAX_LAYER_LEN];
    char	type[MAX_LAYER_LEN],  tmpstr[MAX_LAYER_LEN];

    char	pmlist[MAXALIAS][16], dt_path[MAX_LAYER_LEN];

    int		nn, n_layer, ii, iret; 
    long	flen;
/*---------------------------------------------------------------------*/

    if (strlen(dspath) <= (size_t)0) {
 	src_ok = FALSE;	
	return (src_ok);
    }

    n_layer = 0;

    layer = (char **) malloc (sizeof (char *) * MAX_DIR_LAYER);
    for (ii = 0; ii < MAX_DIR_LAYER; ii++) {
        layer[ii] = (char *) malloc (MAX_LAYER_LEN);
    }

/*
 *  Split the dspath string into individual layers
 */
    cst_clst (dspath, '/', " ", MAX_DIR_LAYER, MAX_LAYER_LEN, 
    					layer, &n_layer, &iret);
    if (iret < 0) {
        src_ok = FALSE;
    }

/*
 *  verify data catagory is valid
 */
    if ( src_ok ) {
        src_ok = dslw_topLayerOk (layer[0]);
    }
   

    if (src_ok) {

	switch (dcatg) {

	    case (CAT_VGF):
	        strcpy ( path, layer[1] );
	        strcpy ( file, layer[2] );
	        
/*
 *  Translate the vg directory into an actual path
 */
                dslw_getVGFpathByTitle(path, actual_path);

/*
 *  Verify the vg file exists
 */
	        if (strlen(actual_path) > (size_t)0) {

	            strcat (actual_path, "/");
	            strcat (actual_path, file);

	            cfl_inqr (actual_path, NULL, &flen, dummy, &iret); 
	            if (iret < 0) {
	                src_ok = FALSE;
	            }
	        }
  	        else {
	            src_ok = FALSE;
	        }

	        break;


	    case (CAT_GRD):

/*
 *  Validate grid source by checking for a restore file.
 */
	        dslw_getGrdResfile (dspath, res_file);

	        if (strlen(res_file) <= (size_t)0 ) {
	            src_ok = FALSE; 
	        }

	        break;

            case (CAT_ENS):

/*
 *  Validate ensemble source by checking for a restore file.
 */
                dslw_getEnsResfile (dspath, res_file);

                if (strlen(res_file) <= (size_t)0 ) {
                    src_ok = FALSE;
                }

                break;

	    case (CAT_SFC) :
	    case (CAT_SFF) :
	    case (CAT_SND) :
	    case (CAT_SNF) :
		strcpy (alias, layer[1]);
        
/*
 *  Get the type (standard, simple, etc).  For the MOS
 *  types, skip over the cycle time to find the type.
 */
		if (dcatg == CAT_SFF || dcatg == CAT_SNF) {
		    strcpy (tmpstr, layer[3]);
		}
		else {
		    strcpy (tmpstr, layer[2]);
		}

/*
 *  If type = standard, then use the alias as the type.
 *  In either case, convert type to upper case.
 */
		if ( strcmp(tmpstr, "standard") == 0 ) {
		    strcpy (tmpstr, alias);
		}
		cst_lcuc(tmpstr, type, &iret);

		ctb_dtlist (&dcatg, dsrclist, &iret);
		if ( strstr(dsrclist, alias) == NULL ) {
		    src_ok = FALSE;
		}

/*
 *  If the alias == "STANDARD", this is stored in the 
 *  table as the alias value itself.  Thus if alias has
 *  checked out ok, we don't need to check the type. 
 */
		if ( src_ok ) {

	            ctb_pllist(alias, MAXALIAS, pmlist, &nn, &iret);
		    src_ok = FALSE;

		    for (ii=0; ii<nn; ii++) {
		        if ( strcmp(pmlist[ii], type) == 0 ) {
			    src_ok = TRUE;
			    break;
			}
		    }
		}

		break;


	    case (CAT_MSC) :
		strcpy (alias, layer[1]);
		ctb_dtlist (&dcatg, dsrclist, &iret);

  		dsrcstr= (char **) malloc(sizeof(char *) * MAXDSRC_OF_DCAT);
		for (ii=0; ii< MAXDSRC_OF_DCAT; ii++)
		    dsrcstr[ii] = (char *)malloc(MAX_LAYER_LEN);

		cst_clst (dsrclist, ';', " ", MAXDSRC_OF_DCAT, MAX_LAYER_LEN, 
				dsrcstr, &nn, &iret); 
	
		src_ok = FALSE;	
	        for (ii=0; ii < nn; ii++) {
		    if ( strcmp(dsrcstr[ii], alias) == 0 ) {  
			src_ok = TRUE;
		        break;
		    }
		}
 
		for (ii=0; ii < MAXDSRC_OF_DCAT; ii++) {
		    free(dsrcstr[ii]);
		}
		free(dsrcstr);

		break;


	    case (CAT_IMG) :

		pstart = strstr(dspath, "SAT");
		if (pstart != NULL) {
		    strcpy (alias, "SAT");
		}
		else {
		    pstart = strstr(dspath, "RAD");
		    strcpy (alias, "RAD");
		}

/*
 *  Get the path to the top of the directory tree
 *  from the datatype.tbl using the alias name.
 */
		ctb_dtpath (alias, dt_path, &iret);		

/*
 *  Split off the SAT or IMG, replace it with the
 *  dt_path, and then check the resulting path.
 */
		strcpy (tmpstr, pstart);
                pstart = strchr (tmpstr, '/');

		strcpy (path, dt_path);
		strcat (path, pstart); 

	        cfl_inqr (path, NULL, &flen, dummy, &iret); 
		if (iret < 0) {
		    src_ok = FALSE;
		}

		break;


	    default:
		src_ok = FALSE;
		break;

        }
    }
 
/*
 *  Free up all allocated memory
 */    
    for (ii = 0; ii < MAX_DIR_LAYER; ii++) {
	free(layer[ii]);
    }
    free (layer);

    return (src_ok);
}

/*=====================================================================*/

static Boolean dslw_topLayerOk ( const char *layer )
/************************************************************************
 * dslw_topLayerOK							*
 *									*
 * This function validates the contents of the layer string by ensuring	*
 * it matches one of the top level layer strings.  False is returned if	*
 * no match is found.  Valid top layers are contained in the _catgStr[]	*
 * array.								*
 *									*
 * static Boolean dslw_topLayerOk(layer)				*
 *									*
 * Input parameters:							*
 *	*layer		const char	current path			*
 *									*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	dslw_topLayerOk static Boolean	TRUE = valid 			*
 *					FALSE = invalid top layer	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/01	initial coding                          *
 ***********************************************************************/
{
    int		ii;
    Boolean	layer_ok = FALSE;
/*---------------------------------------------------------------------*/

    for (ii=0; ii < MAX_CATG; ii++) {
        if ( strcmp( _catgStr[ii], layer) == 0 ) {
	    layer_ok = TRUE;
	    break;
	}
    }

    return (layer_ok);
}

/*=====================================================================*/

void dslw_getGrdCycle ( char *cycle )
/************************************************************************
 * dslw_getGrdCycle                                                     *
 *                                                                      *
 * Function to get the cycle for the  file name based on the MODEL      *
 * selection path shown on the data bar.                                *
 *                                                                      *
 * dslw_getGrdCycle ( cycle )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *              NONE                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *cycle    char        cycle info                                *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/03                                           *
 ***********************************************************************/
{
    cycle[0] = '\0';
    if ( _selectGrdProd.cycle != '\0' )
        strcpy ( cycle, _selectGrdProd.cycle );
}

/*=====================================================================*/

void dslw_getEnsDt ( void )
/************************************************************************
 * dslw_getEnsDt							*
 *                                                                      *
 * Function to to read data from table model_group.tbl and load into	* 
 * a structure for ensemble selection.					*
 *                                                                      *
 * dslw_getEnsDt ()							*	
 *                                                                      *
 * Input parameters:                                                    *
 *              NONE                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *              NONE                                                    *
 
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           04/08                                           *
 ***********************************************************************/
{
    int         ii, jj, kk, ij, mm, nn, ipos1, ipos2, lens, ier, iret;
    char        *ptr, tmpstr[LLMXLN];
    char        mlist[LLMXLN], str1[80], tmpalias[40], tmpalias1[40], buffer[40];
    char 	**tmplist, gdclst[12000];
    dattm_t     *tmptim ;
    Boolean	found;


/*---------------------------------------------------------------------*/

    
    tmplist = (char **) malloc(MAXALIAS * sizeof(char *));
    for (ii = 0; ii < MAXALIAS; ii++) {
 	tmplist[ii] = (char *) malloc(40);
    }

    if ( _modgrp.numgrp > 0 ) {
	 mm = 2;
	 G_MALLOC ( _ensdata, ensInfo_t, mm, "_ensdata" );
    }
    _aliasNum = 0;
    for (ii = 0; ii < _modgrp.numgrp; ii++) {
    	strcpy ( mlist, _modgrp.modlst[ii] );

	/*
	 * Remove '{' and '}'
 	 */
	cst_nocc ( mlist, '{', 1, 0, &ipos1, &ier );
    	cst_nocc ( mlist, '}', 1, 0, &ipos2, &ier );
    	cst_ncpy ( tmpstr, mlist+ipos1+1, ipos2-ipos1-1, &ier );

	/*
	 * Get alias list from the model list string
   	 */
	if ( strchr (tmpstr, ',') == NULL ) {
	    nn = 1;
	    strcpy ( tmplist[0], tmpstr );
	} else {
	    cst_clst ( tmpstr, ',', " ", MAXALIAS, 40, tmplist, &nn, &ier );	    
	}

	/*
	 * Retrive each alias or ensemble member, and load into the struture.
  	 */
        for ( jj = 0; jj < nn; jj++ ) {
	    if ( strchr (tmplist[jj], '|') == NULL ) {
        	strcpy ( str1, tmplist[jj] );
    	    }
    	    else {
		ptr = strtok ( tmplist[jj], "|" );
        	strcpy ( str1, ptr );
	    }
	
	    cst_rmbl ( str1, str1, &lens, &ier );
    	    cst_nocc ( str1, '%', 1, 0, &ipos1, &ier );
    	    if ( ier == 0 ) {
                strcpy ( tmpalias, str1+ipos1+1 );
    	    }
    	    else {
        	strcpy ( tmpalias, str1 );
    	    }

	    if ( strchr ( tmpalias, ':' ) == NULL ) {   /* is an alias */
		if ( _aliasNum <= 0 ) {
		    strcpy ( _ensdata[0].alias, tmpalias );
		    _aliasNum++;
		} else {
		    found = False;
		    for ( kk = 0; kk < _aliasNum; kk++ ) {
			if ( strcmp(_ensdata[kk].alias, tmpalias ) == 0 ) {
			    found = True;			
			    break;
			}
		    }
		    
		    if ( !found ) {
			if ( _aliasNum == mm ) {
			    mm++;
			    G_REALLOC ( _ensdata, ensInfo_t, mm, "_ensdata" );
			}
                    	strcpy ( _ensdata[_aliasNum].alias, tmpalias );
			_aliasNum++;
                    }  
		}
	    }
	    else { /* is a member */
		 strcpy ( buffer, tmpalias );
		 ptr = strtok ( buffer, ":" );
		 strcpy ( tmpalias1, ptr );

		 found = False;
                 for ( kk = 0; kk < _aliasNum; kk++ ) {
                    if ( strcmp(_ensdata[kk].alias, tmpalias1 ) == 0 ) {
                        found = True;                        
                        break;
                    }
                }

                if ( !found ) {
		    if ( _aliasNum == mm ) {
                        mm++; 
                        G_REALLOC ( _ensdata, ensInfo_t, mm, "_ensdata" );
                    }

                    strcpy ( _ensdata[_aliasNum].alias, tmpalias1 );
		    _ensdata[_aliasNum].nmember = 0;
		    strcpy ( _ensdata[_aliasNum].member[0], tmpalias );
		    (_ensdata[_aliasNum].nmember)++;
                    _aliasNum++;

                }
		else {
		    if ( _ensdata[kk].nmember <= 0 ) {
			strcpy ( _ensdata[kk].member[0], tmpalias );
			(_ensdata[kk].nmember)++;
		    } else {
			found = False;
			for ( ij = 0; ij < _ensdata[kk].nmember; ij++ ) {
			    if ( strcmp(_ensdata[kk].member[ij], tmpalias ) == 0 ) {
				found = True;
                        	break;
                    	    }
			}
			
			if ( !found ) {
			    strcpy ( _ensdata[kk].member[_ensdata[kk].nmember], tmpalias );
			    (_ensdata[kk].nmember)++;	
			}
		    }
		}
	    }
        } /* End of  for ( jj = 0; jj < nn; jj++ ) */

    } /* End of for loop */

     for (ii = 0; ii < MAXALIAS; ii++) {
        free (tmplist[ii]);
     }
     free ( tmplist );

    /*
     * Find the cycles for each alias
     */
    for ( ii = 0; ii < _aliasNum; ii++ ) {
 	gd_gcyc(_ensdata[ii].alias,";", &nn, gdclst, &iret,
             	 strlen(_ensdata[ii].alias), 1, sizeof(gdclst) );
        st_null(gdclst, gdclst, &lens, &iret, sizeof(gdclst), sizeof(gdclst));
        cst_rmbl(gdclst, gdclst, &lens, &iret);

        G_MALLOC ( tmptim, dattm_t, nn, "tmptim" );

        ipos1 = 0;
        for  ( jj = 0; jj < nn; jj++ )  {
            cst_nocc ( gdclst, ';', jj+1, 1, &ipos2, &iret );
            if  ( iret != 0 )  ipos2 = (int)strlen(gdclst) + 1;
            cst_ncpy ( tmptim[jj], &gdclst[ipos1], ipos2-ipos1, &iret );
            ipos1 = ipos2 + 1;
        }

	_ensdata[ii].ncycle = 0;
	for ( jj = 0; jj < NUMCYC; jj++ ) {
	    strcpy (_ensdata[ii].cycles[jj], "" );
	    kk = nn - 1 - jj;
	    if ( kk < 0 ) continue;
	    strcpy ( _ensdata[ii].cycles[jj], tmptim[kk] );
	    (_ensdata[ii].ncycle)++;
	}
	
    }

}

/*=====================================================================*/

Widget dslw_ensSelCreate ( Widget parent )
/************************************************************************
 * dslw_create                                                          *
 *                                                                      *
 * This function creates the ensemble selection  window.               	*
 *                                                                      *
 * Widget dslw_enselCreate(parent)                                      *
 *                                                                      *
 * Input parameters:                                                    *
 * parent       Widget   parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * dslw_enselCreate  Widget   Widget ID for ensemble selection window   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           04/08  	created					* 
 * M. Li/SAIC		07/08   Added header, first & alias buttons	*
 ***********************************************************************/
{
    int         jj, kk, nn;
    long	ii, alias_idx;
    Widget      attach, selPaneW, selRC;
    Widget	saveW, cntrPaneW, sep;
    Widget	headerPaneW, model_label, first_label, cycle_labelW[NUMCYC];
    WidgetList	btnw;
    char        *btnstr[] = {"Accept", "Clear All", "Cancel"};
    char        *cycle_label[] = {"Cycle1", "Cycle2", "Cycle3", "Cycle4"};
    Boolean	has_member;
/*---------------------------------------------------------------------*/
    dslw_getEnsDt();
    for ( ii = 0; ii < _modgrp.numgrp; ii++ ) { 
        _firstSelMdl[ii][0] = CHNULL;
    }

    _enselpopW = XmCreateFormDialog(parent, "ensel_popup", NULL, 0);
    XtVaSetValues(_enselpopW,
                XmNnoResize,                    True,
		XmNdefaultPosition, 		False,
                NULL);

    XtVaSetValues(XtParent(_enselpopW), XmNtitle, "Ensemble Selection Window", NULL);

    /*
     * Header form 
     */
    headerPaneW = XtVaCreateManagedWidget("cycle_pane",
                        xmFormWidgetClass,        _enselpopW,
                        XmNnoResize,              TRUE,
                        XmNautoUnmanage,          FALSE,
                        NULL );

    model_label = XtVaCreateManagedWidget ("         Models   ",
                xmLabelGadgetClass,             headerPaneW,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  30,
                NULL);

    first_label = XtVaCreateManagedWidget ("First",
                xmLabelGadgetClass,             headerPaneW,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  210,
                NULL);
     
    for ( ii = 0; ii < NUMCYC; ii++ ) {
        cycle_labelW[ii] = XtVaCreateManagedWidget (cycle_label[ii],
		xmLabelGadgetClass,             headerPaneW,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  320+ii*150,
                NULL);
    }

    sep = XtVaCreateManagedWidget ( "Separator",
		xmSeparatorGadgetClass,		headerPaneW,
		XmNtopAttachment,            	XmATTACH_WIDGET,
		XmNtopWidget,            	first_label,
		XmNleftAttachment,        	XmATTACH_FORM,
		NULL);

    /*
     * ENSEMBLE SELECTION FORM
     */
    selPaneW = XtVaCreateManagedWidget("selecton_pane",
                        xmFormWidgetClass,        _enselpopW,
			XmNtopAttachment,  	  XmATTACH_WIDGET,
			XmNtopWidget,             sep,
			XmNtopOffset,		  TOPOFFSET,
			XmNleftAttachment,        XmATTACH_FORM,
                        NULL );
    selRC = XtVaCreateManagedWidget ("cycle_RowColumn",
                        xmRowColumnWidgetClass,   selPaneW,
                        XmNorientation,           XmVERTICAL,
                        XmNtopAttachment,         XmATTACH_FORM,
                        XmNrightAttachment,       XmATTACH_FORM,
                        XmNleftAttachment,        XmATTACH_FORM,
                        NULL);

    for ( ii = 0; ii < _aliasNum; ii++ ) {
	has_member = ( _ensdata[ii].nmember > 0 ) ? True : False;
        dslw_cycSelCreate ( selRC, ii, _ensdata[ii].alias, _ensdata[ii].cycles, has_member, &(_ensdata[ii].my_alias) );

        alias_idx = (ii+1) * 1000;
    	XtAddCallback ( _ensdata[ii].my_alias.firstw, XmNvalueChangedCallback,
                        (XtCallbackProc)dslw_firstCb, (XtPointer)alias_idx );

	/*
	 * Grey out unavailable cycles.
	 */
	for ( jj = 0; jj < NUMCYC; jj++ ) {
	    if ( strlen(_ensdata[ii].cycles[jj]) < 10 ) {
		XtSetSensitive(_ensdata[ii].my_alias.cycleListw[jj], False);
		XtSetSensitive(_ensdata[ii].my_alias.weightw[jj], False);
		XtVaSetValues(_ensdata[ii].my_alias.cycleListw[jj],
		  	XmNbackground,  		NxmColrP_getColorPixel(8),
                  	NULL);

	    }
  	}
	
	/*
	 * Show members
	 */
	if ( has_member ) {
	    _ensdata[ii].member_form = XtVaCreateWidget ("member_RowColumn",
                        xmRowColumnWidgetClass,   selRC,
                        XmNorientation,           XmVERTICAL,
            		XmNleftAttachment,        XmATTACH_FORM,
            		XmNrightAttachment,       XmATTACH_FORM,
                        NULL);

	    G_MALLOC ( _ensdata[ii].my_member, ensel_t, _ensdata[ii].nmember, "my_member" );
	    for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
		dslw_cycSelCreate ( _ensdata[ii].member_form, ii, _ensdata[ii].member[jj], _ensdata[ii].cycles, False, &(_ensdata[ii].my_member[jj]) );
		alias_idx = (ii+1) * 1000 + jj + 1;
		XtAddCallback ( _ensdata[ii].my_member[jj].firstw, XmNvalueChangedCallback,
                        (XtCallbackProc)dslw_firstCb, (XtPointer)alias_idx );
	
		for ( kk = 0; kk < NUMCYC; kk++ ) {
            	    if ( strlen(_ensdata[ii].cycles[kk]) < 10 ) {
                	XtSetSensitive(_ensdata[ii].my_member[jj].cycleListw[kk], False);
		        XtSetSensitive(_ensdata[ii].my_member[jj].weightw[kk], False);
		 	XtVaSetValues(_ensdata[ii].my_member[jj].cycleListw[kk],
                        	XmNbackground,                  NxmColrP_getColorPixel(8),
                        	NULL);

            	    }
        	}

	    }

	    XtManageChild(_ensdata[ii].member_form);
	}
    }

    /*
     * CONTROL FORM 
     */
    attach = selPaneW;
    saveW = XtVaCreateManagedWidget("control_pane",
                        xmFormWidgetClass,        _enselpopW,
                        XmNnoResize,              TRUE,
                        XmNautoUnmanage,          FALSE,
			XmNtopAttachment,         XmATTACH_WIDGET,
                        XmNtopWidget,             attach,
			XmNtopOffset,		  TOPOFFSET,
                        XmNleftAttachment,        XmATTACH_FORM,
                        NULL );
     
    _showSelect = XtVaCreateManagedWidget ("Selected Models:",
                xmLabelGadgetClass,        	saveW,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  15,
                NULL);

    _modelList = (Widget) XtVaCreateManagedWidget("model_list",
                xmTextFieldWidgetClass,         saveW,
                XmNcolumns,                     80,
		XmNeditable,                    False,
                XmNcursorPositionVisible,       False,
                XmNtopAttachment,         	XmATTACH_FORM,
		XmNleftAttachment,        	XmATTACH_WIDGET,
	 	XmNleftWidget,            	_showSelect,	
		XmNleftOffset,                  10,	
                NULL);
     
    attach = saveW;
    cntrPaneW = XtVaCreateManagedWidget("control_pane",
                        xmRowColumnWidgetClass,   _enselpopW,
                        XmNtopAttachment,         XmATTACH_WIDGET,
                        XmNtopWidget,             attach,
			XmNtopOffset,		  TOPOFFSET,
			XmNleftAttachment,	  XmATTACH_FORM,
			XmNrightAttachment,       XmATTACH_FORM,
                        NULL );

    nn = XtNumber(btnstr);
    btnw = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(cntrPaneW, 1, "ensSel_ctlBtn", nn,
                     btnstr, (XtCallbackProc)dslw_ensSelCtlBtnCb, btnw); 

    return(_enselpopW);
}

/*=====================================================================*/

static void dslw_cycSelCreate ( Widget pane, long ens_idx, char *model, dattm_t ecycles[], Boolean hasMember, ensel_t *ensw ) 
/************************************************************************
 * dslw_create                                                          *
 *                                                                      *
 * This function creates the ensemble and cycle selection row.		*
 *                                                                      *
 * Widget dslw_enselCreate(parent)                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *	pane		Widget  parent pane				*
 *	ens_idx		long	ensemble index				*
 * 	*model		char	Model name				*
 *	ecycles[]	dattm_t cycles					*
 *	hasMember	Booelan Have member flag			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ensw		ensel_t	Ouput ensembel structure		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           04/08  	created					* 
 ***********************************************************************/
{
    int		ier;
    long        ii;
    char	dcyc[6];
    Widget      cycleRC, model_label, percent_label, arrowW, weiW[NUMCYC];
    Widget	cycleW[NUMCYC], first_btn;
/*---------------------------------------------------------------------*/

    cycleRC = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, pane, NULL );

    if ( hasMember ) {
    	arrowW = XtVaCreateManagedWidget ("arrow",
                                      xmArrowButtonWidgetClass, cycleRC,
                                      XmNarrowDirection,        _arrowDir, 
                                      XmNheight,                25,
                                      XmNwidth,                 25,
                                      XmNtopAttachment,         XmATTACH_FORM,
				      XmNleftAttachment,        XmATTACH_FORM,
				      XmNleftOffset,            15,
                                      NULL);
   	ensw->arroww = arrowW;
	XtAddCallback(ensw->arroww, XmNactivateCallback,
                  (XtCallbackProc)dslw_ensMemberCb, (XtPointer)ens_idx);
    }

    model_label = XtVaCreateManagedWidget (model,
                xmToggleButtonGadgetClass,             cycleRC,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  50,
                NULL);
    ensw->aliasw = model_label;
    XtAddCallback ( ensw->aliasw, XmNvalueChangedCallback,
                        (XtCallbackProc)dslw_showSelCb, (XtPointer)NULL );


    first_btn = XtVaCreateManagedWidget (" ",
                xmToggleButtonGadgetClass,      cycleRC,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  210,
                NULL);
    ensw->firstw = first_btn;
    XtAddCallback ( ensw->aliasw, XmNvalueChangedCallback,
                        (XtCallbackProc)dslw_showSelCb, (XtPointer)NULL );

    for ( ii = 0; ii < NUMCYC; ii++ ) {

    	weiW[ii] = (Widget) XtVaCreateManagedWidget("total_weight",
                xmTextFieldWidgetClass,         cycleRC,
                XmNcolumns,                     3,
                XmNcursorPositionVisible,       True,
		XmNtopAttachment,         XmATTACH_FORM,
		XmNleftAttachment,        XmATTACH_FORM,
                XmNleftOffset,            280+ii*150,
                NULL);

    	ensw->weightw[ii] = weiW[ii];

	XtAddCallback(ensw->weightw[ii], XmNmodifyVerifyCallback,
                   (XtCallbackProc) pgutls_vrfyPosIntBlkCb, NULL );
	XtAddCallback(ensw->weightw[ii], XmNvalueChangedCallback,
                   (XtCallbackProc)dslw_showSelCb, NULL);

    	percent_label = XtVaCreateManagedWidget ("%",
                xmLabelGadgetClass,             cycleRC,
                XmNtopAttachment,               XmATTACH_FORM,
		XmNleftAttachment,        	XmATTACH_WIDGET,
                XmNleftWidget,            	weiW[ii],
		XmNleftOffset,			0,
                NULL);
	cst_ncpy ( dcyc, ecycles[ii]+4, 5, &ier );
	cycleW[ii] = XtVaCreateManagedWidget (dcyc,
                xmToggleButtonGadgetClass,      cycleRC,
                XmNtopAttachment,               XmATTACH_FORM,
		XmNleftAttachment,        	XmATTACH_WIDGET,
		XmNleftWidget,			percent_label,
                XmNleftOffset,            	6,
                NULL);
	ensw->cycleListw[ii] = cycleW[ii];

	XtAddCallback ( ensw->cycleListw[ii], XmNvalueChangedCallback,
                        (XtCallbackProc)dslw_showSelCb, (XtPointer)NULL );
    }

}

/*=====================================================================*/
/* ARGSUSED */
void dslw_ensMemberCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_ensMemberCb                                                     *
 *                                                                      *
 * Callback function for control buttons on the bottom.                 *
 *                                                                      *
 * void dslw_ensMemberCb (wid, which, cbs)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      cbs     XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li	04/08	create						*
 ***********************************************************************/
{
    int ii;
    ii = (long)which;

    if ( _arrowDir == XmARROW_RIGHT ) {
        XtManageChild(_ensdata[ii].member_form);
        _arrowDir = XmARROW_DOWN;
    } else {
        XtUnmanageChild(_ensdata[ii].member_form);
        _arrowDir = XmARROW_RIGHT;
    }

    XtVaSetValues(_ensdata[ii].my_alias.arroww,
                XmNarrowDirection,      _arrowDir,
                NULL);


}

/*=====================================================================*/

void dslw_ensSelPopup ( void )
/************************************************************************
 * dslw_ensSelPopup                                                     *
 *                                                                      *
 * This function pops up the ensemble selection popup window.           *
 *                                                                      *
 * void dslw_ensSelPopup (dcatg, path, xx, yy )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           02/07   different inputs for dslw_setSelect     *
 * M. Li/SAIC           07/08   add dslw_setEnsSel			* 
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtManageChild(_enselpopW);
    dslw_setEnsSel();

}

/*=====================================================================*/

void dslw_ensSelPopdown ( void )
/************************************************************************
 * dslw_popup                                                           *
 *                                                                      *
 * This function pops up the data selection popup window.               *
 *                                                                      *
 * void dslw_popup (dcatg, path, xx, yy )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      dcatg   int             category of data                        *
 *      path    char*           path to data                            *
 *      xx      Position        upper left corner x coord.              *
 *      yy      Position        upper left corner y coord.              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           02/07   different inputs for dslw_setSelect     *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtUnmanageChild(_enselpopW);
}

Boolean dslw_ensSelIsUp ( void )
/************************************************************************
 * dslw_isUp                                                            *
 *                                                                      *
 * This function checks if the data source selection window is up.      *
 *                                                                      *
 * Boolean dslw_isUp()                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * dslw_isUp    Boolean          True -- up,    False -- down           *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      01/97                                                *
 ***********************************************************************/
{
    return (XtIsManaged(_enselpopW));
}

/*=====================================================================*/
/* ARGSUSED */
void dslw_ensSelCtlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_ensMemberCb                                                     *
 *                                                                      *
 * Callback function for control buttons on the bottom.                 *
 *                                                                      *
 * void dslw_ensMemberCb (wid, which, cbs)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      cbs     XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li	04/08	create						*
 * M. Li/SAIC	07/08	Move clearAll section to dslw_clearAll		*
 ***********************************************************************/
{
int ier;
char mlist[81];
/*---------------------------------------------------------------------*/

    switch( which ) {

      case 0:		/* Accept	*/
	dslw_getEnsSel(mlist, &ier);
	if ( ier == 0 ) {
	    strcpy ( _modgrp.selModLst[_selectEnsProd.grid], mlist );
	}
	
	dslw_ensSelPopdown();
	break;

      case 1:		/* Clear All	*/ 
	dslw_clearAll ();

    break;

      case 2:       	/* close window */
	dslw_ensSelPopdown();	

        break;

    }


}

/*=====================================================================*/

void dslw_showSelCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_showSelCb                                                     	*
 *                                                                      *
 * Callback function for save selected alias                 		*
 *                                                                      *
 * void dslw_showSelCb (wid, which, cbs)                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      cbs     XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li        04/08   create                                          *
 ***********************************************************************/ 
{
int	ier;
char	mlist[81];
/*---------------------------------------------------------------------*/
    XmTextFieldSetString( _modelList, ""); 
    dslw_getEnsSel(mlist, &ier);

    if ( ier == 0 ) {
	XmTextFieldSetString( _modelList, mlist);
    }

}
/*=====================================================================*/

void dslw_getEnsSel ( char *mdlist, int *iret )
/************************************************************************
 * dslw_getEnsSel                                                     	*
 *                                                                      *
 * Function to get the ensemble selection from the selection window	*
 *                                                                      *
 * void dslw_getEnsSel(mdlist)						*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *	*mdlist		char	Selected model list			*	
 *	*iret       	int     return code                             *
 *                               0 = normal                             *
 *                              -1 = invalid selection                  *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li        05/08   create                                          *
 * M. Li        07/08   Add first & alias buttons			*
 ***********************************************************************/ 
{
int     ii, kk, jj, ier, weight, i0, j0, ipos1, ipos2;
char    mlist[300], tmpmod[21], time[6], *text = NULL, cycstr[5], mlist0[300];
char	list1[300], tmpstr[300];
Boolean has_member, isFirst = True, is_in;
/*---------------------------------------------------------------------*/
        *iret = 0;

        mlist[0] = CHNULL;
        _cycIdx[0] = CHNULL;
  	i0 = -1;
	j0 = -1;
        for ( ii = 0; ii < _aliasNum; ii++ ) {

	    if ( XmToggleButtonGetState(_ensdata[ii].my_alias.firstw) ) {
		i0 = ii;
                strcpy (_firstSelMdl[_selectEnsProd.grid], _ensdata[ii].alias);
	    }
            tmpmod[0] = CHNULL;
            has_member = ( _ensdata[ii].nmember > 0 ) ? True : False;
            isFirst = (strlen(mlist) > 0) ? False : True;
	    is_in = False;

            for ( jj = 0; jj < NUMCYC; jj++ ) {
                tmpmod[0] = CHNULL;
                time[0]   = CHNULL;
                weight = 0;

                if (XmToggleButtonGetState(_ensdata[ii].my_alias.cycleListw[jj]) ) {
		    is_in = True;
                    sprintf( cycstr, "%d,%d", ii, jj );

                    text = XmTextGetString ( _ensdata[ii].my_alias.weightw[jj] );
                    weight = atoi ( text );
                    if ( weight > 0 && weight <= 100 ) {
                        strcpy(tmpmod, text);
                        strcat(tmpmod, "%");
                        strcat(tmpmod, _ensdata[ii].alias);
                    } else {
                        strcpy(tmpmod, _ensdata[ii].alias);
                    }

                    XtFree ( text );

                    cst_ncpy ( time, _ensdata[ii].cycles[jj]+4, 5, &ier );
                    sprintf (tmpmod, "%s|%s", tmpmod, time);
                    if (isFirst) {
                        strcpy (mlist, tmpmod);
                        isFirst = False;

                        strcpy ( _cycIdx, cycstr );
                    }
                    else {
                        strcat (mlist, ",");
                        strcat (mlist, tmpmod);

                        strcat ( _cycIdx, "," );
                        strcat ( _cycIdx, cycstr );
                    }
                }
            }

	    if ( !is_in && XmToggleButtonGetState(_ensdata[ii].my_alias.aliasw) ) {
		if (isFirst) {
                    strcpy (mlist, _ensdata[ii].alias);
                    isFirst = False;
                }
                else {
                    strcat (mlist, ",");
                    strcat (mlist, _ensdata[ii].alias);
                }
            }


            if ( has_member ) {
                for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
	       	    if ( XmToggleButtonGetState(_ensdata[ii].my_member[jj].firstw) ) {
                        strcpy (_firstSelMdl[_selectEnsProd.grid], _ensdata[ii].member[jj]);
			i0 = ii;
			j0 = jj;
		    }
                    tmpmod[0] = CHNULL;
		    is_in = False;

                    for ( kk = 0; kk < NUMCYC; kk++ ) {
                        tmpmod[0] = CHNULL;
                        time[0]   = CHNULL;
                        weight = 0;

                        if (XmToggleButtonGetState(_ensdata[ii].my_member[jj].cycleListw[kk]) ) {
			    is_in = True;
                            sprintf( cycstr, "%d,%d", ii, kk );

                            text = XmTextGetString ( _ensdata[ii].my_member[jj].weightw[kk] );
                            weight = atoi ( text );
                            if ( weight > 0 && weight <= 100 ) {
                                strcpy(tmpmod, text);
                                strcat(tmpmod, "%");
                                strcat(tmpmod, _ensdata[ii].member[jj]);
                            } else {
                                strcpy(tmpmod, _ensdata[ii].member[jj]);
                            }
                            XtFree ( text );

                            cst_ncpy ( time, _ensdata[ii].cycles[kk]+4, 5, &ier );
                            sprintf (tmpmod, "%s|%s", tmpmod, time);

                            if (isFirst) {
                                strcpy (mlist, tmpmod);
                                isFirst = False;

                                strcpy ( _cycIdx, cycstr );
                            }
                            else {
                                strcat (mlist, ",");
                                strcat (mlist, tmpmod);

                                strcat ( _cycIdx, "," );
                                strcat ( _cycIdx, cycstr );
                            }
                        }
                    }

		    if ( !is_in && XmToggleButtonGetState(_ensdata[ii].my_member[jj].aliasw) ) {
			if (isFirst) {
                    	    strcpy (mlist, _ensdata[ii].member[jj]);
                            isFirst = False;
                	}
                	else {
                    	    strcat (mlist, ",");
                    	    strcat (mlist, _ensdata[ii].member[jj]);
                	}
            	    }

                } /*  for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) */
            }  /* has_member */
        }
        if ( strlen(mlist) >= 3 && strlen(mlist) <= 80 ) {
            strcpy ( mlist0, "{" );
            strcat ( mlist0, mlist );
            strcat ( mlist0, "}" );
            mdlist[strlen(mlist0)] = '\0';
            _cycIdx[strlen(_cycIdx)] = '\0';

	    dslw_setFirst(i0, j0, mlist0, mlist0);

	    strcpy ( list1, _modgrp.selModLst[_selectEnsProd.grid]);
	    if ( strchr(list1, '+') ) {
	        cst_nocc ( list1, '{', 1, 0, &ipos1, &ier );
    	        cst_nocc ( list1, '}', 1, 0, &ipos2, &ier );
    	        cst_ncpy ( tmpstr, list1+ipos1, ipos2-ipos1+1, &ier );
		cst_rpst ( list1, tmpstr, mlist0, mdlist, &ier );
	    } else {
	        strcpy ( mdlist, mlist0 );
	    }
        }
        else {
            *iret = -1;
            strcpy ( mdlist, "");
            strcpy ( _cycIdx, "");
        }

}

/*=====================================================================*/

void dslw_getEnsCycle (int modIdx, int cycIdx,  char *cycle )
/************************************************************************
 * dslw_getEnsCycle                                                     *
 *                                                                      *
 * Function to get the cycle for the model based on the MODEL index     *
 * and cycle index.							*
 *                                                                      *
 * dslw_getEnsCycle ( cycle )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *	modIdx		int	model index				*
 *	cycIdx		int	cycle index				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *cycle    char        	cycle					* 
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           05/08                                           *
 ***********************************************************************/
{
    int	ier;

    cycle[0] = '\0';
    cst_ncpy ( cycle, (_ensdata[modIdx].cycles[cycIdx])+4, 5, &ier );
}

/*=====================================================================*/
void dslw_firstCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * dslw_firstCb                                                       	*
 *                                                                      *
 * Callback function for first model buttons                            *
 *                                                                      *
 * void dslw_firstCb (wid, which, cbs)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long            which button                            *
 *      cbs     XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li        07/08   create                                          *
 ***********************************************************************/
{
int i0, j0, ii, jj, imod, imem, ipos1, ipos2, ier;
char mlist1[300], mlist2[300], *text = NULL;
char list1[300], tmpstr[300];
Boolean is_member, has_member, btnval;
/*---------------------------------------------------------------------*/
    j0 = (long) which;
    i0 = (int) (j0 / 1000) - 1 ;
    imod = i0;
    imem = -1;

    is_member = False;
    if ( (j0 % 1000) != 0 ) {
        j0 = j0 - 1 - (i0+1) * 1000; 
        is_member = True;
   	imem = j0;
    }

    XtVaGetValues(wid, XmNset, &btnval, NULL);
    for ( ii = 0; ii < _aliasNum; ii++ ) {
            has_member = ( _ensdata[ii].nmember > 0 ) ? True : False;

            if (btnval && ii != i0) {       
                XmToggleButtonSetState(_ensdata[ii].my_alias.firstw, False, False);

		if (  has_member ) {
		    for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
                    	XmToggleButtonSetState(_ensdata[ii].my_member[jj].firstw, False, False);
                    }
		}    
	    }

	    if ( btnval && ii == i0 && has_member ) {
                if (  is_member ) {
		    XmToggleButtonSetState(_ensdata[ii].my_alias.firstw, False, False);
                    for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
                        if ( jj != j0 )
                            XmToggleButtonSetState(_ensdata[ii].my_member[jj].firstw, False, False);
                    }
            	}
	        else  {
		    for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
                        XmToggleButtonSetState(_ensdata[ii].my_member[jj].firstw, False, False);
                    }
	        }
	    }
    }

    text = XmTextGetString ( _modelList );
    strcpy ( list1, text );
    strcpy ( mlist1, text );
    XtFree ( text );

    if ( strchr(list1, '+') ) {
        cst_nocc ( list1, '{', 1, 0, &ipos1, &ier );
        cst_nocc ( list1, '}', 1, 0, &ipos2, &ier );
        cst_ncpy ( tmpstr, list1+ipos1, ipos2-ipos1+1, &ier );
    } else {
	strcpy ( tmpstr, list1 ); 
    }

    dslw_setFirst (imod, imem, tmpstr, mlist2);
    if ( strchr(list1, '+') ) {
	cst_rpst ( list1, tmpstr, mlist2, list1, &ier );
        strcpy ( mlist2, list1 );
    }

    if ( strcmp(mlist1, mlist2) != 0 ) {
        XmTextFieldSetString( _modelList, "" );
        XmTextFieldSetString( _modelList, mlist2 );
    }

}
/*=====================================================================*/
void dslw_setFirst (int mod, int mem,  char *list1, char *list2 )
/************************************************************************
 * dslw_setFirst                                                     	*
 *                                                                      *
 * Function to set the first model in the list.				* 
 *                                                                      *
 * dslw_setFirst (mod, mem, list1, list2 )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      mod          	int     model index                             *
 *      mem          	int     member index                            *
 *	*list1		char	model list				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *list2    char          model list output	                * 
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/08                                           *
 ***********************************************************************/
{
    int         ii, iflag = -1, nn, ipos1, ipos2, ier;
    int		intnum, intattr[20], itmp, jtmp;
    char        tmpstr[LLMXLN],  **clist, tmpmod[60], firstmod[60];
    
/*---------------------------------------------------------------------*/

    if ( mod < 0 ) {
	strcpy(list2, list1);
	return;
    }

    if ( mem < 0 ) 
	strcpy(firstmod, _ensdata[mod].alias);
    else
	strcpy(firstmod, _ensdata[mod].member[mem]);
     

    cst_nocc ( list1, '{', 1, 0, &ipos1, &ier );
    cst_nocc ( list1, '}', 1, 0, &ipos2, &ier );
    cst_ncpy ( tmpstr, list1+ipos1+1, ipos2-ipos1-1, &ier );

    clist = (char **) malloc(sizeof(char *) * 30);
    for (ii = 0; ii < 30; ii++)
        clist[ii] = (char *) malloc(40);

    cst_clst(tmpstr, ',', " ", 30, 40, clist, &nn, &ier );

    if ( nn <= 1 ) {
	strcpy(list2, list1);
        return;
    }

    for (ii = nn-1; ii >= 0; ii--) {
	cst_nocc ( clist[ii], '%', 1, 0, &ipos1, &ier );
	if ( ier == 0 ) {
	    if ( strncmp(clist[ii]+ipos1+1, firstmod, strlen(firstmod)) == 0 ) {
	        iflag = ii;
	        break;
	    }
	} else {
	    if ( strncmp(clist[ii], firstmod, strlen(firstmod))  == 0 ) {
                iflag = ii;
                break;
	    }
 	}
    }

    if ( iflag <= 0 ) {
	strcpy(list2, list1);
        return;
    }

    strcpy ( tmpmod, clist[0] );
    strcpy ( clist[0], clist[iflag] );
    strcpy ( clist[iflag], tmpmod ); 
    strcpy ( list2, "{" );
    strcat ( list2, clist[0] );
    for ( ii = 1; ii < nn; ii++) {
	strcat ( list2, "," );
	strcat ( list2, clist[ii] );
    }
    strcat ( list2, "}" );
    list2[strlen(list2)] = '\0';

    for (  ii = 0; ii < 30; ii++) {
	free(clist[ii]);
    }
    free(clist);
    
    if ( strchr(clist[0], '/') ) {
        cst_ilst ( _cycIdx, ',', -1, 20, intattr, &intnum, &ier );
        itmp = intattr[0];
        jtmp = intattr[1];
        intattr[0] = intattr[2*iflag];
        intattr[1] = intattr[2*iflag+1];
        intattr[2*iflag] = itmp;
        intattr[2*iflag+1] = jtmp;

        for ( ii = 0 ; ii < intnum; ii++ ) {
	    if ( ii == 0 )
	        sprintf ( _cycIdx, "%d", ii );
	    else
	        sprintf ( _cycIdx, ",%d", ii ); 
        }
        _cycIdx[strlen(_cycIdx)] = '\0';
    }

}

/*=====================================================================*/

void dslw_clearAll ( void )
/************************************************************************
 * dslw_clearAll							*
 *                                                                      *
 * Function to clear all buttons in ensemble selection window           *
 *                                                                      *
 * void dslw_clearAll                                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li        07/08   create                                          *
 ***********************************************************************/
{
int ii, kk, jj;
Boolean has_member;
/*---------------------------------------------------------------------*/

 	XmTextFieldSetString ( _modelList, "" ); 	

	for ( ii = 0; ii < _aliasNum; ii++ ) {
            has_member = ( _ensdata[ii].nmember > 0 ) ? True : False;

	    if (XmToggleButtonGetState(_ensdata[ii].my_alias.aliasw ))
		XmToggleButtonSetState(_ensdata[ii].my_alias.aliasw, False, False);
	    if (XmToggleButtonGetState(_ensdata[ii].my_alias.firstw ))        
                XmToggleButtonSetState(_ensdata[ii].my_alias.firstw, False, False);
 
            for ( jj = 0; jj < NUMCYC; jj++ ) {
 	        XmTextFieldSetString ( _ensdata[ii].my_alias.weightw[jj], "" ); 	

		if (XmToggleButtonGetState(_ensdata[ii].my_alias.cycleListw[jj]) ) {
                    XmToggleButtonSetState(_ensdata[ii].my_alias.cycleListw[jj], False, False);
                }
            }

            if ( has_member ) {
                for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {

		    if (XmToggleButtonGetState(_ensdata[ii].my_member[jj].aliasw ))
                	XmToggleButtonSetState(_ensdata[ii].my_member[jj].aliasw, False, False);
            	    if (XmToggleButtonGetState(_ensdata[ii].my_member[jj].firstw ))
                	XmToggleButtonSetState(_ensdata[ii].my_member[jj].firstw, False, False);
		    for ( kk = 0; kk < NUMCYC; kk++ ) {
            	    	XmTextFieldSetString ( _ensdata[ii].my_member[jj].weightw[kk], "" );

		    	if (XmToggleButtonGetState(_ensdata[ii].my_member[jj].cycleListw[kk]) ) {
                            XmToggleButtonSetState(_ensdata[ii].my_member[jj].cycleListw[kk], False, False);

		        }
		    }	
                }
            }
        }
}
/*=====================================================================*/

void dslw_setEnsSel ( void )
/************************************************************************
 * dslw_setEnsSel							*
 *                                                                      *
 * Function to pre-set ensemble selection window           		*
 *                                                                      *
 * void dslw_setEnsSel                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li        07/08   create                                          *
 ***********************************************************************/
{
    int         ii, jj, kk, nn, ipos1, ipos2, ier, ier1, ier2;
    char        mlist[300], tmpstr[300],  **clist, tmpmod[60];
    Boolean 	has_member, isFirst = True;

/*---------------------------------------------------------------------*/
    dslw_clearAll();

    strcpy ( mlist, _modgrp.selModLst[_selectEnsProd.grid] );
    cst_nocc ( mlist, '{', 1, 0, &ipos1, &ier );
    cst_nocc ( mlist, '}', 1, 0, &ipos2, &ier );
    cst_ncpy ( tmpstr, mlist+ipos1+1, ipos2-ipos1-1, &ier );

    clist = (char **) malloc(sizeof(char *) * 30);
    for (ii = 0; ii < 30; ii++)
        clist[ii] = (char *) malloc(40);

    cst_clst(tmpstr, ',', " ", 30, 40, clist, &nn, &ier );

    if ( nn <= 0 ) return;

    for (kk = 0; kk < nn; kk++) {
        cst_nocc ( clist[kk], '%', 1, 0, &ipos1, &ier1 );
        cst_nocc ( clist[kk], '|', 1, 0, &ipos2, &ier2 );
	tmpmod[0] = '\0';
        if ( ier1 == 0 && ier2 == 0 ) {
	    cst_ncpy ( tmpmod, clist[kk]+ipos1+1, ipos2-ipos1-1, &ier );
        } else if ( ier1 == 0 && ier2 != 0 ) { 
	    strcpy ( tmpmod,  clist[kk]+ipos1+1 );
	} else if ( ier1 != 0 && ier2 == 0 ) {
	    cst_ncpy ( tmpmod, clist[kk], ipos2, &ier );
	} else if ( ier1 != 0 && ier2 != 0 ) {
	    strcpy ( tmpmod,  clist[kk] );
	}

	if ( strlen(tmpmod) < 1 ) continue;

	for ( ii = 0; ii < _aliasNum; ii++ ) {
            has_member = ( _ensdata[ii].nmember > 0 ) ? True : False;
	
	    if ( strcmp(tmpmod,  _ensdata[ii].alias) == 0 ) {
		XmToggleButtonSetState(_ensdata[ii].my_alias.aliasw, True, False);
		if (isFirst) {
		    XmToggleButtonSetState(_ensdata[ii].my_alias.firstw, True, False);
		    isFirst = False;
		}
		break;
	    }

	    if (  has_member ) {
                for ( jj = 0; jj < _ensdata[ii].nmember; jj++ ) {
		    if ( strcmp(tmpmod,  _ensdata[ii].member[jj]) == 0 ) {
                	XmToggleButtonSetState(_ensdata[ii].my_member[jj].aliasw, True, False);
			if (isFirst) {
                    	    XmToggleButtonSetState(_ensdata[ii].my_member[jj].firstw, True, False);
                    	    isFirst = False;
                	}
                	break;
            	    }
	    	}
	    }
        }
    }

    for (  ii = 0; ii < 30; ii++) {
        free(clist[ii]);
    }
    free(clist);

    XmTextFieldSetString( _modelList, _modgrp.selModLst[_selectEnsProd.grid] );

}

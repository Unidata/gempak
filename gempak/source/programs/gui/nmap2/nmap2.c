#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmapprm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"

#define NMAP2	TRUE


struct _resrcs {
	Boolean verbose;        /* verbose mode */
};

static XrmOptionDescRec _nmapOpts[] = {
	{"-v", "verbose", XrmoptionNoArg, "True"},
};

static XtResource _nmapResources[] = {
	{"verbose", "Verbose", XmRBoolean, sizeof(Boolean),
	    XtOffsetOf(struct _resrcs, verbose),
		XmRImmediate, (String)False },
};


/*
 *  _appContext is referenced as an external variable within nmap_dataw.c
 */
XtAppContext	_appContext;


void addAction ( XtAppContext app );

static void hotkeyArrowUp ( Widget wdgt, XEvent *event, String *parms,
			    Cardinal *numParms );

static void hotkeyArrowDown ( Widget wdgt, XEvent *event, String *parms,
			    Cardinal *numParms );

/************************************************************************
 * nmap.c								*
 *									*
 * This module contains the main program of nmap. 			*
 *									*
 * CONTENTS:								*
 *      main()           main program of nmap.				*
 *      addAction()      add action table for nmap.			*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main									*
 *									*
 * Main program of nmap.						*
 *									*
 * main(argc, argv)							*
 *									*
 * Input parameters:							*
 *  argc	int	number of parameters of command line		*
 *  argv	char**	parameter array of command line			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/96  						*
 * C. Lin/EAI		01/97	add input option (verbose for gdpltb)	*
 * C. Lin/EAI		03/97	add NAWIPS version in the title string	*
 * C. Lin/EAI		04/97	add data source and category tables	*
 * D. Plummer/NCEP	07/97	add cds_init()				*
 * C. Lin/EAI		07/97	add map area/overlay table		*
 *				use new mapw module			*
 * C. Lin/EAI		08/97	add creating roam control window	*
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI		10/97	modified for product generation code	*
 *				cleanup and added NxmRes_check()	*
 * D. Plummer/NCEP	10/97	add clo_init()				*
 * C. Lin/EAI		03/98	add ctb_plrd() to read prmlst.tbl table	*
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * C. Lin/EAI		08/98	add ctb_fszrd() to read fontsz.tbl	*
 * E. Safford/GSC	01/99	move app to _appContext			*
 * S. Law/GSC		01/99	added call to seekw_create		*
 * H. Zeng/EAI		09/99	add mapw_setMapstrs() before		*
 *                              mapw_drawMap() to make sure map is on   *
 *                              initially                               *
 * H. Zeng/EAI		09/99	added call to cldhgtw_create()		*
 * E. Safford/GSC	10/99	initialize maps & loop info		*
 * S. Jacobs/NCEP	10/99	Added image_resetLut to NxmEnhw_create	*
 * E. Safford/GSC	10/99	return cloud height			*
 * E. Safford/GSC	10/99	add call to xsvflg			*
 * E. Safford/GSC	11/99	dsp_initLoops -> loop_initLoops		*
 * S. Law/GSC		11/99	changed to use new table		*
 * H. Zeng/EAI		02/00	added call to locfmtw_create()		*
 * E. Safford/GSC	04/00	draw default map in all loops		*
 * E. Safford/GSC	05/00	xsvflg -> xsetver			*
 * H. Zeng/EAI		05/00	added call to cursorw_create()		*
 * M. Li/GSC		02/01	added nmp_init				*
 * H. Zeng/EAI		05/01	added call to NxmBusy_setStopBtn()	*
 * J. Wu/SAIC		12/01	add call to pglayer_init()		*
 * M. Li/SAIC		12/01	add mapset_create			*
 * E. Safford/SAIC	02/02	move mapset_create() to mapw_create()	*
 * M. Li/SAIC		09/02	Added call to ctb_pfread		*
 * R. Tian/SAIC		11/02	add mbtnw_auto(Updt/Lock)Toggle		*
 * T. Piper/SAIC	05/03	replaced gmpk_init with NxmGmpkInit	*
 * T. Piper/SAIC	05/03	removed unnecessary xwcmn.h		*
 * M. Li/SAIC		12/03	load spf from command line		*
 * H. Zeng/XTRIA	01/04	added aodt_create()			*
 * B. Yin/SAIC	04/04	removed XtRealizeWidget ( toplevel )		*
 * E. Safford/SAIC	04/05	add call to NxmGif_create		*
 * M. Li/SAIC		06/05	add aodtw_readTable, remove aodtw_create*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
{
    Widget		toplevel;
    int			lflag, iret;
    String		resource_file="Nmap";
    char		wname[30], pname[40], spfile[MXFLSZ],
		       	lpfile[MXFLSZ], vgfile[MXFLSZ];
    struct _resrcs	res;

    Widget	draw_w;
    char        mesg1[] = "Unable to invoke PRODUCT GENERATION.\n\n"
			"1) Product generation already in use.\n"
			" or \n"
			"2) Cleanup of previous NMAP error required.\n"
			"    type <cleanvgf> in current directory\n";
    char        newpath[LLPATH];
    int		ier;
    struct stat	buf;

/*---------------------------------------------------------------------*/

/*
 *  Parse command line, stop if nmap2.hlp file is shown.
 */
    cmdln_parse ( argc, argv, &iret);
    if ( cmdln_getShowHelp() ) return ( 0 );

/*
 *  Read the font size table.  (which is?)
 */
    ctb_fszrd( &iret);

/*
 *  Read the data table.  (which is?)
 */
    ctb_dtrd (&iret);

/*
 *  Read auxiliary tables.
 */
    dslw_readTable ("GRID", "mod_res.tbl");
    dslw_readTable ("VGF", "vgf.nmap");
    aodtw_readTable ( &iret );

/*
 *  Initilize.
 */
    cds_init( &iret );
    clo_init( &iret );
    gd_init ( &iret );
    nim_init( &iret );
    nmp_init( &iret );
    pglayer_init( );
    
/*
 *  Read the locator information table.
 */
    locfmtw_readLocTbl( &iret );

/*
 *  Read the station model parameter tables.  (which are?)
 */
    ctb_plrd( &iret);
    ctb_pfread ( &iret );
    printf("Finished reading tables...\n");

/*
 *  Tell the xw driver this is nmap2, not nmap.
 */
    xsetver (NMAP2);

/*
 *  Create the top-level widget.
 */
    XtSetLanguageProc(NULL, NULL, NULL);
    toplevel = XtVaAppInitialize( &_appContext, resource_file, 
			       _nmapOpts, XtNumber(_nmapOpts),
			       &argc, argv, NULL, NULL );

/*
 *  Set the minimum width and height for the window.
 */
    XtVaSetValues( toplevel, 
		  XmNminWidth,  	625,
		  XmNminHeight, 	100,
		  XmNiconName,		DEFAULT_NMAP2_ICON_NAME,
		  NULL );
	
/*
 *  Check the resource file.  (which is?)
 */
    NxmRes_check(XtDisplay(toplevel), resource_file, NULL);

/*
 *  Display the nmap version in title string.
 */
    NxmVers_showTitle(toplevel);

    XtGetApplicationResources(toplevel, &res, _nmapResources,
			      XtNumber(_nmapResources), NULL, 0);

/*
 *  Set the verbose mode for gdpltb.
 */
    if (res.verbose) {
	lflag = 1;
    }
    else {
	lflag = 0;
    }

    strcpy(pname, "VERBOSE");
    gdpstt(pname, &lflag, &iret, strlen(pname));

/*
 *  Add the action table.  (which is?)
 */
    addAction(_appContext);

/*
 *  Initialize the GEMPAK variables.
 */
    if ( NxmGmpkInit(toplevel, 1, gmpk_init) != 0 ) {
	exit(1);
    }

/*
 *  Create the main window widgets.
 */
    mainw_create( toplevel );

/*
 *  Create the map selection popup.
 */
    mapw_create(toplevel);

/*
 *  Create the computational window.
 */
    ncw_create(toplevel);

/*
 *  Register the drawing area in the main window as a GEMPAK window.
 */
    mcanvw_rgstr();

/*
 *  Clear the main drawing window. 
 */
    strcpy(wname, MCANVW_NAME);
    gslwin(wname, &iret, strlen(wname));
    gclear( &iret );

/*
 *  Create the data popup.
 */
    dataw_create(toplevel); 

/*
 *  Create the seek popup.
 */
    seekw_create (toplevel);

/*
 *  Create the cloud height popup.
 */
    cldhgtw_create (toplevel); 

/*
 *  Create the roam control window.
 */
    roamw_create(toplevel);

/*
 *  Create the gif image window.
 */
    NxmGif_create("mainwin", toplevel); 

/*
 *  Create the locator format edit window popup.
 */
    locfmtw_create (toplevel);

/*
 *  Create the cursor edit window popup.
 */
    cursorw_create (toplevel);

/*
 *  Create the image enhancement window.
 */
    NxmEnhw_create(toplevel, mbotw_getFadeColor, image_resetLut);

    XmProcessTraversal(mcanvw_getDrawingW(), XmTRAVERSE_CURRENT);

    NxmErr_update();

/*
 *  Initialize the loop record structs.
 */
    loop_initLoops();

/*
 *  Draw the default map in each loop and set loop 1 in view.
 */
    NxmBusy_setStopBtn(0);
    dsp_loadAllLoops();
    NxmBusy_setStopBtn(1);

    mbtnw_setLoopOne();

/*
 *  Load an SP file to the data window if needed.   
 */
    if ( cmdln_getLoadSPF() ) {
	cmdln_getSPF ( spfile );
	if ( strlen ( spfile ) > (size_t)0 ) {
	    dataw_cmdLineLoadSPF ( spfile, cmdln_getAutoLoadData () );
	}
    }       

/*
 *  Open PGEN and load an VG file, if needed.
 */
    if ( cmdln_getLoadVGF() ) {
	cmdln_getVGF ( vgfile );
	if ( strlen ( vgfile ) > (size_t)0 ) {
	    css_envr ( cvg_getworkfile(), newpath, &ier );
	    if ( stat(newpath, &buf) != 0 ) {
		pgpalw_popup();
	    }
	    else {
		draw_w = (Widget)mcanvw_getDrawingW();
		NxmWarn_show(draw_w, mesg1);
	    }
	    pglayer_setFileName ( pglayer_getCurLayer(), vgfile );
	    pgfilw_openVGF ( FALSE, &ier );
	}
    }       

    cmdln_cleanUp ();

    XtAppMainLoop(_appContext);

    return (0);
}

/*=====================================================================*/

void addAction ( XtAppContext app )
/************************************************************************
 * addAction								*
 *									*
 * This function adds the action table.					*
 *									*
 * void addAction (app)							*
 *									*
 * Input parameters:							*
 *	app	XtAppContext	application context			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 7/95	add action table			*
 * C. Lin/EAI		 1/96	XtAddActions -> XtAppAddActions		*
 * S. Law/GSC		01/00	added loop set actions			*
 * E. Safford/GSC	05/00	added loop set actions 5 - 8		*
 * S. Jacobs/NCEP	 7/00	added fade reset action			*
 * J. Wu/GSC		04/01	added hide toggle action		*
 * H. Zeng/EAI          04/01   added roam control actions              *
 * J. Wu/GSC		04/01	added undo/redo actions			*
 * J. Wu/GSC		05/01	added image fade toggle action		*
 * E. Safford/SAIC	01/02	added mcanvw_catchCtrl			*
 * H. Zeng/XTRIA	03/04	added more loops			*
 * E. Safford/SAIC	04/04	added pgdel_keyboardDelete		*
 * B. Yin/SAIC		06/04	added pglayrw_setLayer, removed rock	*
 * J. Wu/SAIC		07/04	added pgfilterw_stepOneUp/Down		*
 * M. Li/SAIC		12/07	Added zoomw_czoom and zoomw_zzoom	*
 ***********************************************************************/
{

    static XtActionsRec actionsTable[] = {
	{"toggle_dataPopup",	(XtActionProc)dataw_toggle},
	{"loopw_back",		(XtActionProc)loopw_back},
	{"loopw_frwd",		(XtActionProc)loopw_frwd},
	{"loopw_lpfd",		(XtActionProc)loopw_lpfd},
	{"loopw_hide",		(XtActionProc)loopw_hide},
	{"mbotw_fadeReset",	(XtActionProc)mbotw_fadeReset},
	{"mbotw_fadeToggle",	(XtActionProc)mbotw_fadeToggle},
	{"mbtnw_setLoopOne",	(XtActionProc)mbtnw_setLoopOne},
	{"mbtnw_setLoopTwo",	(XtActionProc)mbtnw_setLoopTwo},
	{"mbtnw_setLoopThree",	(XtActionProc)mbtnw_setLoopThree},
	{"mbtnw_setLoopFour",	(XtActionProc)mbtnw_setLoopFour},
	{"mbtnw_setLoopFive",	(XtActionProc)mbtnw_setLoopFive},
	{"mbtnw_setLoopSix",	(XtActionProc)mbtnw_setLoopSix},
	{"mbtnw_setLoopSeven",	(XtActionProc)mbtnw_setLoopSeven},
	{"mbtnw_setLoopEight",	(XtActionProc)mbtnw_setLoopEight},
	{"mbtnw_setLoopNine",	(XtActionProc)mbtnw_setLoopNine},
	{"mbtnw_setLoopTen",	(XtActionProc)mbtnw_setLoopTen},
	{"mbtnw_setLoopEleven",	(XtActionProc)mbtnw_setLoopEleven},
	{"mbtnw_setLoopTwelve",	(XtActionProc)mbtnw_setLoopTwelve},
	{"mbtnw_setLoopThirteen", (XtActionProc)mbtnw_setLoopThirteen},
	{"mbtnw_setLoopFourteen", (XtActionProc)mbtnw_setLoopFourteen},
	{"mbtnw_setLoopFifteen",  (XtActionProc)mbtnw_setLoopFifteen},
	{"mbtnw_setLoopSixteen",  (XtActionProc)mbtnw_setLoopSixteen},
	{"mbtnw_autoUpdtToggle",(XtActionProc)mbtnw_autoUpdtToggle},
	{"mbtnw_autoLockToggle",(XtActionProc)mbtnw_autoLockToggle},
	{"roamw_arrowLeft",	(XtActionProc)roamw_arrowLeft},
	{"roamw_arrowRight",	(XtActionProc)roamw_arrowRight},
	{"hotkeyArrowUp",	(XtActionProc)hotkeyArrowUp},
	{"hotkeyArrowDown",	(XtActionProc)hotkeyArrowDown},
	{"pgpalw_undo",		(XtActionProc)pgpalw_undo},
	{"pgpalw_redo",		(XtActionProc)pgpalw_redo},
	{"mcanvw_catchCtrl",	(XtActionProc)mcanvw_catchCtrl},
	{"pgdel_keyboardDelete",(XtActionProc)pgdel_keyboardDelete},
        {"pglayrw_setLayer", 	(XtActionProc)pglayrw_setLayer},
	{"pgfilterw_stepOneUp",  (XtActionProc)pgfilterw_stepOneUp},
	{"pgfilterw_stepOneDown",(XtActionProc)pgfilterw_stepOneDown},
	{"zoomw_czoom",         (XtActionProc)zoomw_czoom},
	{"zoomw_zzoom",         (XtActionProc)zoomw_zzoom}
    };
/*---------------------------------------------------------------------*/

    XtAppAddActions(app, actionsTable, XtNumber(actionsTable));

}

/*=====================================================================*/

static void hotkeyArrowUp ( Widget wdgt, XEvent *event, String *parms, 
			    Cardinal *numParms )
/************************************************************************
 * hotkeyArrowUp                                                        *
 *                                                                      *
 * This function is the action for arrow key UP.            		*
 *                                                                      *
 * void hotkeyArrowUp ( wdgt, event, params, num_params )               *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt	Widget    widget that caused action to be called	*
 *  *event	XEvent    event that caused action to be called		*
 *  *params	String    pointer to list of strings specified as action*
 *                      	arguments (not used)                    *
 *  *num_params  Cardinal   number of strings (not used)                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		3/05	Created	                                *
 ***********************************************************************/
{

    int ier;
/*---------------------------------------------------------------------*/
    
    roamw_arrowUp ( wdgt, event, parms, numParms );

    if ( pgtca_isUp () ) pgtca_stepList ( UP_LIST, &ier ); 

}

/*=====================================================================*/

static void hotkeyArrowDown ( Widget wdgt, XEvent *event, String *parms, 
			      Cardinal *numParms )
/************************************************************************
 * hotkeyArrowDown                                                      *
 *                                                                      *
 * This function is the action for arrow key Down.            		*
 *                                                                      *
 * void hotkeyArrowDown ( wdgt, event, params, num_params )             *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt	Widget    widget that caused action to be called        *
 *  *event	XEvent    event that caused action to be called         *
 *  *params	String    pointer to list of strings specified as action*
 *                      	arguments (not used)                    *
 *  *num_params  Cardinal   number of strings (not used)                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		3/05	Created	                                *
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/

    roamw_arrowDown ( wdgt, event, parms, numParms );

    if ( pgtca_isUp () ) pgtca_stepList ( DOWN_LIST, &ier ); 

}

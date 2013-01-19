#include "nwx_cmn.h"
#include "nwx_gui.h"

#define RESOURCE_FILE "Nwx"


static void 	nwx_actionsAdd ( XtAppContext  app );

XtAppContext	_appContext;

/************************************************************************
 * nwxg_nwx.c								*
 *									*
 * This module creates the GUIs and starts the application loop.	*
 *									*
 * CONTENTS:								*
 *   nwx_guiStart()	create and start the GUIs			*
 *   nwx_actionsAdd()	Adds the action table.				*
 ***********************************************************************/

/*=====================================================================*/

int nwx_guiStart ( int argc, char *argv[], int *iret )
/************************************************************************
 * nwx_guiStart 							*
 *									*
 * This function will display an output text window and a graphical	*
 * selection window. The user may select from the data selection popup. *
 * The requested data will be used to construct a search string, and	*
 * data file names to search, and will display the text information in	*
 * the output text window.						*
 **									*
 * Log: 								*
 * S. Jacobs/NMC	 7/94						*
 * L. Williams/EAI	 6/95 remove menutop Widget			*
 * C. Lin/EAI		 7/95 add action table				*
 * C. Lin/EAI		 8/95 rewrite					*
 * D.Plummer/NCEP	12/95 added WATCHWARN (fosd type W) processing	*
 * D.Plummer/NCEP	 9/96 added "O" data type processing		*
 * D.Plummer/NCEP	 9/96 changed time selection from days to hours *
 * D.W.Plummer/NCEP	11/96 time selection from hours to days or hours*
 * S. Wang/GSC		03/97 change help file table name		*
 * G. Krueger/EAI	10/97 add NxmRes_check; improve headers 	*
 * G. Krueger/EAI	11/97 Renamed NxmHelp functions			*
 * A. Hardy/GSC          3/98 Added interface header file               *
 * S. Jacobs/NCEP	 3/99 Use NO_ZOOM instead G_FALSE		*
 * E. Safford/SAIC	10/01 Make help window 80 columns wide		*
 * T. Piper/SAIC	 6/02 Removed unused variable usrSelect.selct_by*
 * T. Piper/SAIC	 7/02 Removed interface.h, no longer needed	*
 * T. Piper/SAIC	 7/02 Reinstated the select by state option	*
 * T. Piper/SAIC	05/03 Replaced gmpk_init with NxmGmpkInit	*
 * T. Piper/SAIC	07/03 Added ip_svar for $MAPFIL			*
 * R. Tian/SAIC		11/03 Changed XtAppContext global and renamed 	* 
 * T. Piper/SAIC	01/04	Modified error message wording		*
 * T. Piper/SAIC	02/04	Changed name and location of help index	*
 * E. Safford/SAIC	05/04 	Add XtRealize on toplevel so X server   *
 *				 gets command line options		*
 * H. Zeng/SAIC		07/04  put title string on topshell window	*
 * T. Piper/SAIC	01/05	Added check on NxmGmpkInit		*
 * E. Safford/SAIC	12/07	code moved from main()			*
 ***********************************************************************/
{
Widget		toplevel, maptop;
char		mapfil[9]="$MAPFIL=";
/*---------------------------------------------------------------------*/

/*
 * Create the top level widget.
 */
	toplevel = XtAppInitialize( &_appContext, RESOURCE_FILE, NULL, 0,
			&argc, argv, NULL, NULL, 0 );

/*
 * check resource file
 */
	NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

/*
 * add action table
 */
	nwx_actionsAdd(_appContext);

/*
 * display version in title string
 */
	NxmVers_showTitle(toplevel);

/*
 * Create the map window.
 */
	maptop = mapw_create(toplevel);

/*
 * Realize the popup window.
 */
	XtRealizeWidget( toplevel );

/*
 * Create the text window.
 */
	txtw_create(toplevel);

/*
 * create the help popup widget
 */
	NxmHelp_create( maptop, "HelpDialog", "Help",
			"$GEMHLP/hlp/nwxIndex.hlp", 20, 80);

/*
 * initialize GEMPAK variables 
 */
	if ( NxmGmpkInit(toplevel, 2, NULL ) != 0 ) {
            exit(1);
        }
	ip_svar(mapfil, iret, strlen(mapfil));

/*
 * Register the map window in GEMPAK
 */
	*iret = mapw_rgstr( mapCanvW );
	if (*iret != 0) {
	    printf("NWX:  ERROR registering MAP window.\n");
	    exit(2);
	}

/*
 * Start main loop.
 */
	XtAppMainLoop( _appContext );

	return(0);
}

/*=====================================================================*/

static void nwx_actionsAdd ( XtAppContext app )
/************************************************************************
 * nwx_actionsAdd							*
 *									*
 * This function adds the action table. 				*
 *									*
 * static void nwx_actionsAdd(app)					*
 *									*
 * Input Parameters:							*
 *	app		XtAppContext					*
 **									*
 * Log: 								*
 * C. Lin/EAI		 7/95 add action table				*
 ***********************************************************************/
{
static XtActionsRec actionsTable[] = {
		{"toggle_dataPopup", (XtActionProc)dslw_toggle}, };

    XtAppAddActions(app, actionsTable, XtNumber(actionsTable));
}

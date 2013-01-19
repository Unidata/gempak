#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

#define  CREATE 

#include "interface.h"
#include "panel.h"

#define RESOURCE_FILE "Ntrans2"

void init_param ( void );
void add_actions ( XtAppContext	app );

/************************************************************************
 * ntrans.c								*
 *									*
 * This module is the main program module of NTRANS.			*
 *									*
 * CONTENTS:								*
 *									*
 *   main()		Main function.					*
 *   initparam()	Initializes the global variables in NTRANS.	*
 *   add_actions()	Adds the action table.				*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
*   main		             				   	*
*								   	*
*   Main program for NTRANS.	             			   	*
*								   	*
*   Log:							   	*
*   Chien Lin/EAI      02/93  					   	*
*   Chien Lin/EAI      09/93  change for ntrans1                   	*
*   C. Lin/EAI         09/94  add function checkNtransEnv()        	*
*   C. Lin/EAI         04/95  use GEMPAK device driver             	*
*   C. Lin/EAI         09/95  add G_ZEROCB check after xcaloc      	*
*   J. Cowie/COMET     11/95  gemplt.err ->gemplt      		   	*
*   C. Lin/EAI          1/96  use application context      	   	*
*   C. Lin/EAI          4/96  change call to xmotifw, replace      	*
*				gg_sdev with gg_motf      	   	*
*   S. Wang/GSC		1/97  general structure change 			*
*   C. Lin/EAI         10/97  call NxmRes_check()     			*
*   G. Krueger/EAI     10/97  add NxmVers_showTitle; improve headers	*
*   I. Durham/GSC	5/98  changed call for underscore		*
*   S. Jacobs/NCEP      8/98  Added open and close of log file		*
*   S. Jacobs/NCEP	3/02  Fixed LOGGING option			*
*   A. Hardy/NCEP	6/03  Added tmzn to CSS_DATE			*
* T. Piper/SAIC		 1/04	Removed NxmCheckEnv			*
* B. Yin/SAIC           03/04   changed css_date calling sequences      *
************************************************************************/
{
char    wname[20];
XtAppContext app;

#ifdef LOGGING
int	itype, iy, im, id, ih, in, is, ij, ier;
char	lognam[81], tmzn[4];
#endif

/*---------------------------------------------------------------------*/

        toplevel = XtAppInitialize( &app, RESOURCE_FILE, NULL, 0,
                        &argc, argv, NULL, NULL, 0 );

/*
 * check resource file
 */
	NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

/*
 * display version in title string
 */
	NxmVers_showTitle(toplevel);

	add_actions(app);

	XtVaSetValues( toplevel, XmNminWidth,  625,
				 XmNminHeight, 100,
				 NULL);
	
/*
 * initialize GEMPAK variables
 */
	if ( NxmGmpkInit(toplevel, 2, NULL ) != 0 ) {
	    exit(1);
	}

	create_main(toplevel);
    	XtRealizeWidget(toplevel);
	init_param();

	strcpy(wname, "ntrans");
	NxmGmpkRgstr(DrawingW, wname, graf_init);

#ifdef LOGGING

/*
 * Open log file.
 */
	itype = 0;
	css_date ( &itype, &iy, &im, &id, &ih, &in, &is, &ij, tmzn, &ier );
	sprintf ( lognam, ".NTRANS.LOG.%04d%02d%02d.%d",
		  iy, im, id, getpid() );
	logptr = cfl_aopn ( lognam, &ier );
#endif
		
	XmProcessTraversal(DrawingW, XmTRAVERSE_CURRENT);

	XtAppMainLoop(app);

#ifdef LOGGING
/*
 * Close log file.
 */
	cfl_clos ( logptr, &ier );
#endif

	return(0);

}         	/* end of main   */

/*=====================================================================*/

void init_param ( void )
/************************************************************************
*   init_param			        			   	*
*								   	*
*   This function initialize the global variables in ntrans	   	*
*								   	*
*   Log:							   	*
*   S. Wang/GSC		01/97	extract from old ntrans.c and add  	*
*				new variables			   	*
*   G. Krueger/EAI	10/97	improve headers				*
*   S. Jacobs/NCEP	10/97	Added oldtextalgn initialization	*
*   S. Jacobs/NCEP	 7/98	Changed init values for "old" attributes*
* T. Piper/SAIC		07/03	removed current_window, added xqclrs	*
* T. Piper/SAIC		01/04	removed temporary variable "colors"	*
************************************************************************/
{
int	colr, red, green, blue;
int	iret;
int     cbank=GraphCid, ncolors;

/*---------------------------------------------------------------------*/

        load_flag	= 0;

	GraphicsData.line_colr = 1;
	GraphicsData.fill_colr = 1;
	GraphicsData.textfont = 1;
	GraphicsData.oldtextfont = 0;
	GraphicsData.textsize = 1.0F;
	GraphicsData.oldtextsize = 0.0F;
	GraphicsData.textalgn = 1;
	GraphicsData.oldtextalgn = 0;

        PixmapData.current_pixmap        = 0;
	PixmapData.old_pixmap_no         = 1;
	PixmapData.pixmap_no             = 1;

        FrameNo               =  0;
        ViewFrame             =  0; /* display group */
	OpenModel	      =  0;

	Mpanel.columns 		= 1;
	Mpanel.rows 		= 1;
	Mpanel.selectCol 	= 0;
	Mpanel.selectRow 	= 0;

/*
 * set global pixel values and set background
 */
	xqclrs(&cbank, &ncolors, pixels, &iret);

        colr = 0;
        red = green = blue = 0;
        gscrgb(&colr, &red, &green, &blue, &iret);

        if ( iret != 0 ) {
                printf("ERROR:  gscrgb, iret = %d\n", iret);
                return;
        }
}

/*=====================================================================*/

void add_actions ( XtAppContext app )
/************************************************************************
*   add_actions			        			   	*
*								   	*
*   This function adds the action table.			   	*
*								   	*
*   Log:							   	*
*   G. Krueger/EAI	10/97	improve headers				*
************************************************************************/
{

static  XtActionsRec actionsTable[] = {
                {"ok_select",		(XtActionProc)ok_select},
                {"Search_Callback",	(XtActionProc)Search_Callback},
                {"setMode0",		(XtActionProc)setMode0},
                {"setMode1",		(XtActionProc)setMode1},
                {"AddGpList_Callback",	(XtActionProc)AddGpList_Callback},
                {"view_frame",		(XtActionProc)view_frame},
                {"save_group",		(XtActionProc)save_group},
                {"toggle_menuWindow",	(XtActionProc)toggle_menuWindow},
        };

        XtAppAddActions(app, actionsTable, XtNumber(actionsTable));

}

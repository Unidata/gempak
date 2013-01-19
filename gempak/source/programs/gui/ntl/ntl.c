#include "xwcmn.h"
#include "app.h"
#include "prog.h"
#include "Nxm.h"

#define	RESOURCE_FILE	"Ntop"

struct _resrcs Resrcs;

static XtResource resources[] = {

	{"graphClr", "GraphClr", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, graphClr), 
		XmRImmediate, (char *) GRAPH_COLORS },

	{"satClr", "SatClr", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, satClr), 
		XmRImmediate, (char *) SAT_COLORS },
 
	{"radClr", "RadClr", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, radClr), 
		XmRImmediate, (char *) RAD_COLORS },

	{"faxClr", "FaxClr", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, faxClr), 
		XmRImmediate, (char *) FAX_COLORS },

	{"appbgName", "AppbgName", XmRString, sizeof(char *),
	    XtOffsetOf(struct _resrcs, appbgName), 
		XmRImmediate, (char *) NULL },

	{"appbgRed", "AppbgRed", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, appbgRed), 
		XmRImmediate, (char *) 0 },

	{"appbgGreen", "AppbgGreen", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, appbgGreen), 
		XmRImmediate, (char *) 0 },

	{"appbgBlue", "AppbgBlue", XmRInt, sizeof(int),
	    XtOffsetOf(struct _resrcs, appbgBlue), 
		XmRImmediate, (char *) 0 },

	{"verbose", "Verbose", XmRBoolean, sizeof(Boolean),
	    XtOffsetOf(struct _resrcs, verbose), 
		XmRImmediate, (char *) False },

	{"help", "Help", XmRBoolean, sizeof(Boolean),
	    XtOffsetOf(struct _resrcs, help), 
		XmRImmediate, (char *) False },

};

static XrmOptionDescRec options[] = {
	/*
	 * not allow to change graphic colors for now
	 *
	 * {"-g", "graphClr", XrmoptionSepArg, NULL},
	 */

	{"-s", "satClr",   XrmoptionSepArg, NULL},
	{"-r", "radClr",   XrmoptionSepArg, NULL},
	{"-v", "verbose",  XrmoptionNoArg,  "True"},
	{"-h", "help",     XrmoptionNoArg,  "True"},
};

/************************************************************************
 * ntl.c                                                                *
 *                                                                      *
 * This is the main program of ntl. It displays an icon for each 	*
 *	application and manages the color pool shared among the 	*
 *	applications.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()   main program of ntl                     		*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * The main function of ntl.                       			*
 *                                                                      *
 * void main(argc, argv)                                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *      argc    int   # of command line arguments			*
 *      **argv  char  command line arguments				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *       int           the number of application button created         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chien Lin/EAI      04/94  					   	*
 * A. Chang/EAi       08/94	Patched to use external table	   	*
 *				for application programs	   	*
 * Chien Lin/EAI      03/95    Call XW to manage sharing colors   	*
 * Chien Lin/EAI      06/95    Reconstruct program   		   	*
 * Chien Lin/EAI      08/95    Add signal handler for SIGCHLD 	   	*
 *				Add command line option processing 	*
 * C. Lin/EAI         12/95    clean up the header                      *
 * C. Lin/EAI          1/96    bug fix for -r option                    *
 *  			       consistant options and explaination      *
 * C. Lin/EAI         11/96  set max/min height/width after realization *
 *			       set RowColumn attr to control position   *
 * C. Lin/EAI         12/96  add application background color resource  *
 * G. Krueger/EAI     10/97  Add NxmRes_check				*
 * C. Lin/EAI         11/97  colrMapInit->colr_init			*
 * R. Tian/SAIC       05/02  Added color resource for fax image		*
 * S. Jacobs/NCEP      9/02  Added check for depth to support 24 bit	*
 * S. Jacobs/NCEP     10/02  Removed check for depth			*
 * T. Piper/SAIC	01/04	Removed apptabCheck 			*
 * T. Piper/SAIC	07/04	Fixed color calc. with COLR_SCAL	*
 ***********************************************************************/
{
XtAppContext	apptxt;
Widget 	 	toplevel;
Widget	 	rc;		 /* RowColumn Widget to hold app icons */
Dimension 	maxh, maxw;    /* max height,width of top level window */
int             banks[4]; 	     /* # of colors for each bank */
int             ctotal;              /* total # of colors */
int             len, iret;

/*---------------------------------------------------------------------*/

	toplevel = XtAppInitialize(&apptxt, RESOURCE_FILE, 
			options, XtNumber(options), 
			&argc, argv, NULL, NULL, 0);

	/*
	 * check resource file
	 */
	NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

	XtGetApplicationResources(toplevel, &Resrcs, resources,
		XtNumber(resources), NULL, 0);

	/*
	 * validate the color components
	 */
	if ( Resrcs.appbgRed < 0 || Resrcs.appbgRed > 255 )
		Resrcs.appbgRed = 0;
	else
		Resrcs.appbgRed = ((Resrcs.appbgRed+1)*COLR_SCAL) - 1;

	if ( Resrcs.appbgGreen < 0 || Resrcs.appbgGreen > 255 )
		Resrcs.appbgGreen = 0;
	else
		Resrcs.appbgGreen = ((Resrcs.appbgGreen+1)*COLR_SCAL) - 1;

	if ( Resrcs.appbgBlue < 0 || Resrcs.appbgBlue > 255 )
		Resrcs.appbgBlue = 0;
	else
		Resrcs.appbgBlue = ((Resrcs.appbgBlue+1)*COLR_SCAL) - 1;

	if ( Resrcs.appbgName ) {
        	cst_lstr(Resrcs.appbgName, &len, &iret);
        	Resrcs.appbgName[len] = '\0';
	}

	if ( Resrcs.help ) {
		printf("\n\t Usage: ntl [-s N] [-r N] [-v] [-h]\n");

		/*
		 * not allow to change graphic colors for now
		 *
		 *printf("\n\t\t -g number of graphic colors\n");
		 */

		printf("\n\t\t -s number of satellite colors\n");
		printf("\n\t\t -r number of radar colors\n");
		printf("\n\t\t -v verbose mode\n");
		printf("\n\t\t -h print this usage\n\n");
		exit(0);
	}

	gemdisplay = XtDisplay(toplevel);
	if ( gemdisplay == NULL){
       		printf("Cannot open gemdisplay\n");
       		exit (1);
	}
	gemmap = DefaultColormap ((XtPointer)gemdisplay, 
		 DefaultScreen((XtPointer)gemdisplay) );

	/* 
	 * read table file and create application buttons 
	 */
	rc = XtVaCreateManagedWidget("tplevelRc",
		xmRowColumnWidgetClass, toplevel,
		XmNorientation,         XmVERTICAL,
		XmNmarginHeight, 	15,
		XmNmarginWidth, 	5,
		XmNpacking, 	        XmPACK_TIGHT,
		XmNspacing, 	        10,
		NULL);

	apps_bp = NULL;
	apptabRead( rc, &apps_bp );

	/*
    	 * Get colormap ID and initialize the color map 
	 */
	if ( Resrcs.graphClr < 0 )
		Resrcs.graphClr = 0;
	if ( Resrcs.satClr < 0 )
		Resrcs.satClr = 0;
	if ( Resrcs.radClr < 0 )
		Resrcs.radClr = 0;
	if ( Resrcs.faxClr < 0 )
		Resrcs.faxClr = 0;
	ctotal = Resrcs.graphClr + Resrcs.satClr + Resrcs.radClr + Resrcs.faxClr;
	if ( ctotal > MAX_TOTAL_COLOR ) {
	  printf("ERROR: total # of colors needed %d exceeds the max %d.\n",
		ctotal, MAX_TOTAL_COLOR);
	    exit(1);
		
	}

	printf("graphic, satellite, radar, fax -- %d %d %d %d\n", 
		Resrcs.graphClr, Resrcs.satClr, Resrcs.radClr, Resrcs.faxClr);

	banks[GraphCid] = Resrcs.graphClr;
	banks[SatCid] = Resrcs.satClr;
	banks[RadCid] = Resrcs.radClr;
	banks[FaxCid] = Resrcs.faxClr;

	colr_init( 4, banks, Resrcs.verbose);
	
	XtRealizeWidget(toplevel);

	/*
	 * Setup maximum height and width. 
	 */
	XtVaGetValues(toplevel,
		XmNwidth,  &maxw, 
		XmNheight, &maxh, 
		NULL);
	XtVaSetValues(toplevel,
		XmNmaxHeight, maxh, 
		XmNminHeight, maxh,
		XmNmaxWidth,  maxw,
		XmNminWidth,  maxw,
		NULL);
	/*
	 * setup signal handler to intercept the termination of
	 * child applications.
	 */
	if (signal(SIGCHLD, progWait) == SIG_ERR){
		printf("signal(SIGCHLD) error\n");
		exit(0);
	}

	XtAppMainLoop(apptxt);

	return(0);
}

/*=====================================================================*/

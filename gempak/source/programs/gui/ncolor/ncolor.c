#include "geminc.h"
#include "Nxm.h"


#define RESOURCE_FILE "Ncolor"

static Widget	_topRC, _colorBar;

void ncolor_btnCb ( Widget, XtPointer, XtPointer );
void ncolor_create ( Widget parent );

/************************************************************************
 * ncolor.c								*
 *									*
 * This module is the main program module of NCOLOR.			*
 *									*
 * CONTENTS:								*
 *									*
 *   main()		Main function.					*
 *   ncolor_create()	create ncolor window				*
 *   ncolor_btnCb()	callback for reset & exit buttons		*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc , char **argv )
/************************************************************************
 * main			             				   	*
 *								   	*
 * Main program for ncolor.	             			   	*
 *								   	*
 * main ( argc, argv )							*
 * Input paramaters:							*
 *  argc	int	number of input arguments			*
 *  **argv	char	input arguments					*
 * 									*
 * Output paramaters:							*
 *  NONE								*
 **									*
 * Log:								   	*
 * E. Safford/GSC	12/98	initial coding				*
 * T. Piper/SAIC	05/03	removed proto_ncolor.h			*
 * T. Piper/SAIC	07/03	removed unnecessary xwcmn.h		*
 ***********************************************************************/
{
Widget		top_level;
XtAppContext 	app;
Screen		*screen;
/*---------------------------------------------------------------------*/

    top_level = XtAppInitialize( &app, RESOURCE_FILE, NULL, 0,
                        &argc, argv, NULL, NULL, 0);

    screen = XtScreen(top_level);

    XtVaSetValues (top_level, 
		XmNminWidth,		625,
  		XmNx,			90,	
  		XmNy,		 	HeightOfScreen(screen) - 25,	
		NULL); 

/*
 * check resource file
 */
    NxmRes_check(XtDisplay(top_level), RESOURCE_FILE, NULL);

/*
 * initialize GEMPAK variables
 */
    if ( NxmGmpkInit(top_level, 2, NULL ) != 0 ) {
	exit(1);
    }

    ncolor_create(top_level); 
    XtRealizeWidget(top_level);

    NxmGmpkRgstr(top_level, "ncolor", NULL);
    XtAppMainLoop(app);

    return(0);
} 

/*=====================================================================*/

void ncolor_create ( Widget parent )
/************************************************************************
 * ncolor_create							*
 *									*
 * This module creates the ncolor window area.				*
 *									*
 * ncolor_create ( parent )						*
 *									*
 * Input parameters:							*
 *   parent	Widget		parent widget id			*
 *									*
 * Output parameters:							*
 *   None								*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	01/99	initial coding             		*
 * E. Safford/GSC	01/99	re-arrange ctl btns        		*
 * T. Piper/SAIC	12/01	freed ctl_btnsW				*
 ***********************************************************************/
{
Widget		top_form;
char        	*btnstr[] = {"Reset Colors", "Exit"};
WidgetList  	ctl_btnsW;
/*---------------------------------------------------------------------*/

    top_form =  XtCreateManagedWidget("toplevel_form",
		xmFormWidgetClass, 	parent,
		NULL,		   	0);

    _topRC   =  XtVaCreateManagedWidget("toplevel_rc",
    		xmRowColumnWidgetClass,	top_form,
		XmNorientation,		XmHORIZONTAL,
		XmNpacking,		XmPACK_TIGHT,
		XmNrows,		2,
		NULL);

/*
 *  Color bar and edit popup window
 */
    _colorBar = XtVaCreateManagedWidget("colrbar",
    		xmFrameWidgetClass, 		_topRC,  
		XmNpositionIndex,		0,
		NULL);

    NuiColorBarCreate(_colorBar, FALSE);
    NuiColorEditPopup(top_form);  

/*
 *  control buttons
 */
    ctl_btnsW = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget)); 
    NxmCtlBtn_create(_topRC, 1, "btn_rc", XtNumber(btnstr), btnstr, 
						ncolor_btnCb, ctl_btnsW); 
    
    XtFree((XtPointer)ctl_btnsW);
}


/*=====================================================================*/
/* ARGSUSED */
void ncolor_btnCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * ncolor_btnCb								*
 *									*
 * This is the callback for the reset and exit buttons.			*
 *									*
 * ncolor_btnCb ( w, clnt, call )					*
 *									*
 * Input parameters:							*
 *  w			Widget		widget ID       		*
 *  clnt		XtPointer 	client data			*
 *  call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 *   None								*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	01/99	initial coding             		*
 * E. Safford/GSC	01/99	modify refresh             		*
 ***********************************************************************/
{
int		which = (long)clnt;
Display		*dsply;
/*---------------------------------------------------------------------*/

    if (which) {	/* exit */

    	dsply = XtDisplay(w);
	XFlush(dsply);
	XtCloseDisplay(dsply);

	exit(0);
    }
    else {		/* reset colors */
	NuiColorBarReset(_colorBar);
    }
}

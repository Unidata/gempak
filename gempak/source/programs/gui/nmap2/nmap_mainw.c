#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"


#define	 COMPONENT_LEN		( 128 )

static Widget _topLevel = NULL;
static char   _spfName [ COMPONENT_LEN ];
static char   _dayCycle[ COMPONENT_LEN ];
/************************************************************************
 * nmap_mainw.c 							*
 *									*
 * This module creates the main window for nmap.			*
 *									*
 * CONTENTS:								*
 *	mainw_create()	    creates the main window.			*
 *	mainw_setIconName() sets the top level icon name		*
 *	mainw_setTitleName()sets the title for the top level window	*
 *	mainw_removeCycle() removes the cycle time fromt the title  	*
 *	mainw_removeSpf()   removes the spf name fromt the title  	*
 ***********************************************************************/

/*=====================================================================*/

Widget mainw_create ( Widget parent )
/************************************************************************
 * mainw_create 							*
 *									*
 * This function creates the main window.				*
 *									*
 * Widget mainw_create(parent)						*
 *									*
 * Input parameters:							*
 *  parent	 Widget  parent widget ID				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *	mainw_create	Widget	Widget ID of the pane window		*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	   04/96						*
 * S. Wang/GSC	   03/97	change help file table name		*
 * S. Wang/GSC	   03/97	add print popup 			*
 * S. Wang/GSC	   05/97	change NxmPrt_create interface		*
 * C. Lin/EAI	   07/97	change NxmPrt_create calliing sequence	*
 * E. Wehner/EAi   09/97	Remove graphics info record		*
 * C. Lin/EAI	   10/97	change NxmPaletteCr->pgpalw_create	*
 * G. Krueger/EAI  11/97	Renamed NxmHelp functions		*
 * E. Safford/GSC  09/99	set icon tips to off as default		*
 * S. Jacobs/NCEP  11/99	Changed dataw_prtFrame to dsp_print	*
 * E. Safford/GSC  06/01	put canvas on form, fixes 955 sz limit  *
 * E. Safford/GSC  06/01	fix problems with form resize           *
 * E. Safford/SAIC 10/01	make NxmHelp display 80 characters	*
 * T. Piper/SAIC	06/03	moved NxmBxmBtn_enableLabel to		*
 * 							mmenuw_create	*
 * T. Piper/SAIC	02/04	Changed name and location of help index	*
 * B. Yin/SAIC		04/04	Added code to realize toplevel widget   *
 * H. Zeng/SAIC		09/04	set _topLevel variable			*
 * E. Safford/SAIC	04/05	move NxmGif_create to nmap.c -- aix has *
 *				  an issue if it's created here		*
 * E. Safford/SAIC	07/07	initialize optional title components	*
 ***********************************************************************/
{
Widget 	pane, form, frame, sep, canvas;
Widget	main_form;
int	iret;
/*---------------------------------------------------------------------*/

/*
 * First set _topLevel variable.
 */
	_topLevel = parent; 

	main_form = XtVaCreateWidget("mainw_form",
			xmFormWidgetClass, 		parent,
			XmNfractionBase,		20,
			NULL);
	
/*
 * creates main pane widget
 */
	pane = XtVaCreateWidget("mainw_pane",
			xmPanedWindowWidgetClass, 	main_form,
			XmNleftAttachment,		XmATTACH_FORM,	
			XmNrightAttachment,		XmATTACH_FORM,	
			XmNsashWidth,		  	1,
			XmNsashHeight,		  	1,
			NULL, 0);

/*
 * create menu-bar
 */
	mmenuw_create(pane);

/*
 * creates main window buttons
 */
	mbtnw_create(pane);

/*
 * create canvas drawing area
 */
	form = XtVaCreateWidget("mainw_form",
			xmFormWidgetClass, 		main_form,
			XmNtopAttachment,		XmATTACH_WIDGET,
			XmNtopWidget,			pane,	
			XmNleftAttachment,		XmATTACH_FORM,
			XmNrightAttachment,		XmATTACH_FORM,
			XmNbottomAttachment,		XmATTACH_FORM,
			XmNfractionBase,		20,
			NULL);

	canvas = mcanvw_create(form);

	XtVaSetValues(canvas,
			XmNbottomAttachment,		XmATTACH_POSITION,
			XmNbottomPosition,		18,
			NULL);

	/*
	 * create a separator
	 */
	sep = XtVaCreateManagedWidget("mainw_separator",
			xmSeparatorWidgetClass, 	form,
			XmNleftAttachment,		XmATTACH_FORM,
			XmNrightAttachment,		XmATTACH_FORM,
			NULL);

	/*
	 * create bottom frame
	 */
	frame = mbotw_create(form);

	XtVaSetValues(sep,
			XmNbottomAttachment,  		XmATTACH_WIDGET,
			XmNbottomWidget,      		frame,
			XmNbottomOffset,      		3,
			NULL);

	XtVaSetValues(canvas,
			XmNbottomAttachment,  		XmATTACH_WIDGET,
			XmNbottomWidget,      		sep,
			XmNbottomOffset,      		3,
			NULL);


	XtManageChild(canvas);
	XtManageChild(form);
	XtManageChild(pane);

	XtManageChild(main_form);

	/*
	 * create Print popup
	 */
	NxmPrt_create("mainwin", parent, dsp_print);

	/*
	 * create HELP popup
	 */
	NxmHelp_create(canvas, "HelpDialog", "Help",
			"$GEMHLP/hlp/nmapIndex.hlp", 20, 80);
        
	XtRealizeWidget( parent );
	/*
	 * create PRODUCT GENERATION popup...
	 */
	pgpalw_create( &iret);

	/*
	 *  Initialize the optional title components.
	 */
	_spfName[0]  = '\0';
	_dayCycle[0] = '\0';

	return(pane);

}

/*=====================================================================*/

void mainw_setIconName ( char* iconName )
/************************************************************************
 * mainw_setIconName 							*
 *									*
 * This function sets the toplevel icon name.				*
 *									*
 * void mainw_setIconName(iconName)					*
 *									*
 * Input parameters:							*
 *  iconName	 char*          proposed icon name			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		 NONE							*
 *									*
 **									*
 * Log: 								*
 * H. Zeng/SAIC		09/04   initial coding				*
 * E. Safford/SAIC	07/07	use defined COMPONENT_LEN		*
 ***********************************************************************/
{
  static char  icon_name[ COMPONENT_LEN ];
  int	       ier;
/*---------------------------------------------------------------------*/

        if ( _topLevel != NULL ) {

          cst_ncpy ( icon_name, iconName, 120, &ier );
	  XtVaSetValues( _topLevel, XmNiconName, icon_name, NULL);
        }

}

/*=====================================================================*/

void mainw_setTitleName ( const char* spfName, const char *cycle )
/************************************************************************
 * mainw_setTitleName 							*
 *									*
 * This function adds the title addtion to the NMAP2 window title name.	*
 *									*
 * The nmap2 title format is:						*
 *									*
 *   "NMAP2 (Version) -- [spf file name ] -- [forecast day/cycle]"	*
 *									*
 * The spf file name is only used if an spf file has been loaded, and   *
 * the forecast day/cycle string will only be added if the prefs.tbl    *
 * file's USE_DAY_CYCLE switch is set to True and product generation is *
 * active.								*
 *									*
 * void mainw_setTitleName ( titleAdditon )				*
 *									*
 * Input parameters:							*
 *   spfName	 char*          spf file name				*
 *   cycle	 char*          forecast day/cycle time string 		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		 NONE							*
 *									*
 **									*
 * Log:                                                                 *
 * J. Wu/SAIC		03/05   initial coding				*
 * E. Safford/SAIC	07/07 	change input params to spfName & cycle	*
 ***********************************************************************/
{
    static char	*oldTitle, newTitle[256];

    int		nc, ier;
    Boolean	haveSpf = False, haveCycle = False;
/*---------------------------------------------------------------------*/

    if ( _topLevel != NULL ) {

	/*
	 *  Determine what inputs have been provided.
	 */
	if( spfName ) {
            cst_ncpy( _spfName, spfName, COMPONENT_LEN -1, &ier );
	    if( ier < 0 ) return;
        } 
	
	if( cycle ) {
            cst_ncpy( _dayCycle, cycle, COMPONENT_LEN -1, &ier );
	    if( ier < 0 ) return;
	}

	if( strlen( _spfName ) > 0 )  haveSpf = True;	
	if( strlen( _dayCycle ) > 0 )  haveCycle = True;	


	/*
	 *  Get the current title string.
	 */
        XtVaGetValues ( _topLevel, XmNtitle, &oldTitle, NULL );


	/*
	 *  Chop off the old title at the right ')' which is the end of
	 *  the version number in the title.
	 */
	cst_nocc ( oldTitle, ')', 1, 1, &nc, &ier );        	
	cst_ncpy ( newTitle, oldTitle, nc+1, &ier );
	       
	/*
	 *  Add the SPF file.
	 */ 
	if( haveSpf ) {
	    strcat ( newTitle, " -- " );
	    strcat ( newTitle, _spfName );
	}
	
	/*
	 *  Add the day/cycle string.
	 */	
	if( haveCycle ) {
	    strcat ( newTitle, " -- " );
	    strcat ( newTitle, _dayCycle );
	}

	/*
	 *  Display the new title.
	 */
	XtVaSetValues ( _topLevel, XmNtitle, newTitle, NULL );	
        
    }

}

/*=====================================================================*/

void mainw_removeCycle ( void )
/************************************************************************
 * mainw_removeCycle 							*
 *									*
 * This function removes the cycle time from the title.			*
 *									*
 * void mainw_removeCycle ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *		 NONE							*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding				*
 ***********************************************************************/
{
    _dayCycle[0] = '\0';
    mainw_setTitleName ( NULL, NULL );
}


/*=====================================================================*/

Widget mainw_getToplevel ( void )
/************************************************************************
 * mainw_getToplevel 							*
 *									*
 * This function gets toplevel widget.					*
 *									*
 * Widget mainw_getToplevel ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *		 NONE							*
 * Return parameters:							*
 *   mainw_getToplevel	 Widget          toplevel widget		*
 *									*
 **									*
 * Log:                                                                 *
 * H. Zeng/SAIC		 08/06   initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return ( _topLevel );

}

/*=====================================================================*/


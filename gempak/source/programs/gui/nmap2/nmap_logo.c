#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"
#include "nmap_data.h"
#include "Nxm.h"

#define LOGO_TABLE      "logo.tbl"

#define BLACK_WHITE	( 1 )
#define COLOR_MODE	( 2 )

#define THIS_LOOP	( 0 )
#define ALL_LOOP	( 1 )

#define WIDGET_OFFSET	 15	

int	_readTbl = FALSE;

int	_curLogoMode = 0;

int	_nlogo;

Boolean	_noLogo = TRUE;

static Widget	_loopPopup = (Widget) NULL;

typedef struct logo_t
{
    Boolean	logo_actv[MAX_LOGO];	/* logo status */
    int		csp_mode[MAX_LOGO];	/* logo color, size, position */
} logo_t;

typedef struct logo_tbl_t
{
    char	name[LOGOLEN];
    int		icmode;
    float	size;
    float	xpos;
    float	ypos;
} logo_tbl_t;

static logo_t		logos[MAX_LOOP];
static logo_tbl_t	*_logoTbl;

/*
 * Private Functions
 */
static void logo_loadCb ( Widget w, long which, XtPointer call );
static void logo_init ( void );
static void logo_readTable ( void );
static void logo_loadOneLoop ( int lp );

/************************************************************************
 * nmap_logo.c								*
 *									*
 * This module reads the logo table and provides the information to	*
 * other modules.							*
 *									*
 * CONTENTS:								*
 *	logo_getNames()		Get a list of logo mode names for GUI	*
 *	logo_getInfo()		Get the mode info for a given logo	*
 *	logo_getCurLogo()	Get a logo's current mode 		*
 *	logo_init()		Initialize the 'logos' structure	*
 *	logo_isLogoActv()	Check the active status for the logo	*
 *	logo_loadCb()		Callback for the logo selection popup	*
 *	logo_loadOneLoop()	Load logo & data for one loop		*
 *	logo_lpLogoActv()	Check if active logoes exist in a loop	*
 *	logo_readTable()	Read the logo table			*
 *	logo_selectCb()		Callback for the logo menu option	*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void logo_getNames ( int *num, char *logolist[LOGOLEN] )
/************************************************************************
 * logo_getNames							*
 *									*
 * This function returns a list of logo mode names from the logo table.	*
 *									*
 * void logo_getNames ( num, logolist )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*num			int	Number of table entries		*
 *	*logolist[LOGOLEN]	char	List of names of entries	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 ***********************************************************************/
{

    int		ii;

/*---------------------------------------------------------------------*/

    /*
     * If the table has not been read, read it.
     */
    if  ( ! _readTbl )  {
	logo_readTable ( );
    }

    /*
     * Set the number of entries and the list of names.
     * Note: add "OFF" as first option. 
     */
    *num = _nlogo + 1;

    logolist[0] = ( char * ) malloc( LOGOLEN * sizeof( char ) );	 
    strcpy ( logolist[0], "OFF" );
             
    for ( ii = 1; ii < *num; ii++ )  {
    
        logolist[ii] = ( char * ) malloc( LOGOLEN * sizeof( char ) );	 
	strcpy ( logolist[ii], _logoTbl[ii - 1].name );
    
    }

}

/*=====================================================================*/

void logo_getInfo ( int mode, char name[], float *size, 
					float *xn, float *yn, int *icm )
/************************************************************************
 * logo_getInfo								*
 *									*
 * This function returns the color/size/position inf. about a logo.	*
 *									*
 * void logo_getInfo ( mode, name, size, xn, yn, icm )			*
 *									*
 * Input parameters:							*
 *	mode		int	Mode number of the logo to return	*
 *									*
 * Output parameters:							*
 *	name[]		char	Name for the requested item		*
 *	*size		float	Size for the requested item		*
 *	*xn		float	X-normal for the requested item		*
 *	*yn		float	Y-normal for the requested item		*
 *	*icm		int	Color mode for the requested item	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 * T. Piper/SAIC	10/06	Cannot hardcode yn in table; retrieved	*
 *				via gqbnd				*
 ***********************************************************************/
{
    int		iret;
    float	xl, yb, xr, yt;
/*---------------------------------------------------------------------*/
/*
 * Get the information for this entry.
 */
    if ( mode == 0 ) {
    
        strcpy ( name, "" ); 
        *icm  = 0;
        *size = 0.0F;
        *xn   = 0.0F;
        *yn   = 0.0F;    
    
    }
    else {	/* From LOGO table */

	strcpy ( name, _logoTbl[mode-1].name );
        *icm  = _logoTbl[mode-1].icmode;
        *size = _logoTbl[mode-1].size;
        *xn   = _logoTbl[mode-1].xpos;
	if ( mode < 4  ) {
	    *yn   = _logoTbl[mode-1].ypos;
	}
	else {
	    gqbnd(sys_N, &xl, &yb, &xr, &yt, &iret, strlen(sys_N));    
	    *yn = 0.9 * yt;
	}
    }
}

/*=====================================================================*/

int logo_getCurLogo ( int lp, int logo )
/************************************************************************
 * logo_getCurLogo							*
 *									*
 * This function returns a given logo's color/size/position mode.	*
 *									*
 * int logo_getCurLogo ( lp, logo )					*
 *									*
 * Input parameters:							*
 *   	lp		int	loop number 			        *
 *   	logo		int	logo ID 			        *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	logo_getCurLogo		int	Current logo number		*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    return ( logos[lp].csp_mode[logo] );

}

/*=====================================================================*/

Boolean logo_isLogoActv ( int lp, int logo )
/************************************************************************
 * logo_isLogoActv							*
 *									*
 * This function check if a logo is active or not.			*
 *									*
 * Boolean logo_isLogoActv ( lp, logo )					*
 *									*
 * Input parameters:							*
 *   	lp		int	loop number 			        *
 *   	logo		int	logo ID 			        *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	logo_isLogoActv		Boolean		True if logo is active	*
 *						False if not active	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    return ( logos[lp].logo_actv[logo] && logos[lp].csp_mode[logo] ) ;

}

/*=====================================================================*/
/* ARGSUSED */
void logo_selectCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * logo_selectCb                                               		*
 *                                                                      *
 * Callback function for the LOGO cascade menu on the menubar of the	*
 * main window. 							*
 *                                                                      *
 * void logo_selectCb ( w, which, call )                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w       Widget	parent widget ID				*
 *  which   long	buttom ID                              		*
 *  call    XtPointer	not used					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/GSC  	 	4/01	Created					*
 * T. Piper/SAIC	12/01	freed btn_list				*
 * J. Wu/GSC  	 	05/02	pop up dialogue box only when necessary	*
 ***********************************************************************/
{
    int		ii, curLogo;
    Widget 	mock_parent, label, ctnl;
    WidgetList	btn_list;
    char	*buttons[] = {"This Loop", "All Loops"};

/*---------------------------------------------------------------------*/

    /*
     * Save off the current logo color/size/position mode.
     */
    _curLogoMode = (int)which;   

    
    /*
     * Pop down the dialogue box first if it exists.
     */
    mock_parent = (Widget) mcanvw_getDrawingW ();
    
    if ( _loopPopup != (Widget) NULL ) {

        XtDestroyWidget ( _loopPopup );    

    }
    
    /*
     * If the user intends to turn a logo "OFF" and it is not 
     * active in any loops at all, just ignore & return.
     */
    curLogo = mmenuw_getLogoName() - 1;
    if ( _curLogoMode == 0 ) {  /* OFF */
        for ( ii = 0; ii < MAX_LOOP; ii++ ) {
	    if ( logo_isLogoActv( ii, curLogo ) ) break;
	}
        
	if ( ii == MAX_LOOP ) return; 	
    }

    /*
     * Prompt for users' selection.
     */
    _loopPopup = XmCreateFormDialog (mock_parent, "Loop Selection", NULL, 0);

    label = XtVaCreateManagedWidget ("Apply to just this loop or all loops?", 
				     xmLabelWidgetClass, _loopPopup,
				     XmNtopAttachment,	XmATTACH_FORM,
				     XmNtopOffset,	WIDGET_OFFSET,
				     XmNleftAttachment,	XmATTACH_FORM,
				     XmNleftOffset,	WIDGET_OFFSET,
				     XmNrightAttachment,XmATTACH_FORM,
				     XmNrightOffset,	WIDGET_OFFSET,
				     NULL);
    ctnl = XtVaCreateManagedWidget ( "ctnl",
				     xmFormWidgetClass, _loopPopup,
				     XmNentryClass, xmToggleButtonWidgetClass,
				     XmNisHomogeneous,	True,
				     XmNradioBehavior,	True,
				     XmNorientation,	0,
				     XmNtopAttachment,	XmATTACH_WIDGET,
				     XmNtopWidget,	label,
				     XmNtopOffset,	WIDGET_OFFSET,
				     XmNleftAttachment,	XmATTACH_FORM,
				     XmNleftOffset,	WIDGET_OFFSET,
				     XmNrightAttachment,XmATTACH_FORM,
				     XmNrightOffset,	WIDGET_OFFSET,
				     XmNbottomAttachment,XmATTACH_FORM,
				     XmNbottomOffset,	WIDGET_OFFSET,
				     NULL );
				     
    btn_list = (WidgetList) XtMalloc( XtNumber(buttons)* sizeof(Widget) );
    NxmCtlBtn_create ( ctnl, 1, "ctnl_buttons", XtNumber(buttons),
    		       buttons, (XtCallbackProc)logo_loadCb, btn_list);
    XtFree((XtPointer)btn_list);

    XtManageChild (_loopPopup);

}

/*=====================================================================*/
/* ARGSUSED */
static void logo_loadCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * logo_loadCb                                               		*
 *                                                                      *
 * Callback function for the loop selection popup window.		*
 *                                                                      *
 * void logo_loadCb ( w, which, call )                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w       Widget	parent widget ID                                *
 *  which   long	button ID                              		*
 *  call    XtPointer	not used					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP  	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 * T. Piper/SAIC	09/06	Added xmloop_switchLoop and nmp_rstrproj*
 * T. Piper/SAIC	10/06	Moved loop_changeLoop to fix bug	*
 ***********************************************************************/
{

    int		_curLoop, ier, ii;
/*---------------------------------------------------------------------*/

    XtUnmanageChild (_loopPopup);
    XtDestroyWidget (_loopPopup);
    
    _loopPopup = (Widget)NULL;

    _curLoop = loop_getCurLoop( );

    dsp_setBusy( TRUE );
/*
 *  Apply user selection to either current loop or all loops.
 */    
    if ( which == THIS_LOOP )  {
        logo_loadOneLoop( _curLoop );
    }
    else {  /* All Loops */
	for ( ii = 0; ii < MAX_LOOP; ii++ ) {
	    xmloop_switchLoop(ii, FALSE);
	    nmp_rstrproj(ii, &ier );
	    logo_loadOneLoop( ii );
	}
    }	

    loop_changeLoop(_curLoop);  /*  Reset to current loop.  */
    
    dsp_setBusy( FALSE );

}

/*=====================================================================*/

static void logo_init ( void )
/************************************************************************
 * logo_init								*
 *									*
 * This function initializes the logos' settings.			*
 *									*
 * void logo_init ( )							*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	 	4/01	Created					*
 ***********************************************************************/
{
    int ii, jj;

/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < MAX_LOOP; ii++ ) {
        
	for  ( jj = 0; jj < MAX_LOGO; jj++ )  {

	    logos[ii].logo_actv[jj] = FALSE;
	    logos[ii].csp_mode[jj] = 0;
	    	    
	}
    
    }

}

/*=====================================================================*/

static void logo_readTable ( void )
/************************************************************************
 * logo_readTable							*
 *									*
 * This function reads the information from the logo table and stores	*
 * it globally in _logoTbl.						*
 *									*
 * void logo_readTable ( )						*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/00	Created					*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific *
 * T. Piper/SAIC	12/01	Close file				*
 ***********************************************************************/
{

    char	buffer[80], name[LOGOLEN];
    int		jj, bufsiz, ier, icm, tblsiz;
    float	size, xn, yn;

    FILE	*fp;

/*---------------------------------------------------------------------*/

    /*
     * Initialize the settings for each logo & loop.
     */    
    logo_init ( );

    /*
     * Set the entries from the table.
     */
    fp = cfl_tbop ( LOGO_TABLE, "nmap", &ier );

    if  ( ier == 0 )  {

	bufsiz = sizeof (buffer);

	/*
	 * Get the number of entries in the table.
	 */
	cfl_tbnr ( fp, &(_nlogo), &ier );

	/*
	 *  Allocate space for table.
	 */
	tblsiz = sizeof( struct logo_tbl_t );
	_logoTbl = ( logo_tbl_t * ) malloc( _nlogo * tblsiz );	 
	 
	/*
	 * Read each entry and load into the table structure. 
	 */
	for  ( jj = 0; jj < _nlogo; jj++ )  {

	    cfl_trln ( fp, bufsiz, buffer, &ier ); 

	    sscanf ( buffer,"%s %f %f %f %d",
		     name, &size, &xn, &yn, &icm );

	    strcpy ( _logoTbl[jj].name, name);
	    _logoTbl[jj].icmode = icm;
	    _logoTbl[jj].size = size;
	    _logoTbl[jj].xpos = xn;
	    _logoTbl[jj].ypos = yn;

	}
	cfl_clos(fp, &ier);
    }

    /*
     * Set the flag that the table has been read.
     */
    _readTbl = TRUE;

}

/*=====================================================================*/

Boolean logo_lpLogoActv ( int lp )
/************************************************************************
 * logo_lpLogoActv							*
 *									*
 * This function check if there are active logos in a loop.		*
 *									*
 * Boolean logo_lpLogoActv ( lp )					*
 *									*
 * Input parameters:							*
 *   	lp		int	loop number 			        *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	logo_lpLogoActv		Boolean		True if one logo active	*
 *						False if no active logo	*
 **									*
 * Log:									*
 * J. Wu/GSC	  	 4/01	Created					*
 ***********************************************************************/
{
    int ii;
    
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < MAX_LOGO; ii++ ) {
       
       if ( logos[lp].logo_actv[ii] && logos[lp].csp_mode[ii] ) {
       
           return TRUE;
       
       }
    
    }
    
    return FALSE;

}

/*=====================================================================*/

static void logo_loadOneLoop ( int lp )
/************************************************************************
 * logo_loadOneLoop							*
 *									*
 * This function add logo to one loop and reload data if necessary	*
 *									*
 * logo_loadOneLoop ( lp )						*
 *									*
 * Input parameters:							*
 *   	lp		int	loop number 			        *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	  	 4/01	Created					*
 * J. Wu/GSC	  	 5/01	use mmenuw_getLogoName()		*
 * E. Safford/GSC	06/01	if pgen actv, always reload loop	*
 ***********************************************************************/
{
    int		_curLogo, _oldMode, ier;

/*---------------------------------------------------------------------*/
/*
 *  Get the current selected logo code.
 */
    _curLogo = mmenuw_getLogoName( );
    
    _oldMode = logos[lp].csp_mode[_curLogo - 1];
/*
 *  If there is already an active logo in the loop and the user
 *  elects to change the mode (color/size/position) of the selected
 *  logo, then reload the loop and store logo information.
 */    
    if ( logo_lpLogoActv( lp ) || pgpalw_isUp() ) {

 	if ( _curLogoMode != _oldMode ) {

            logos[lp].logo_actv[_curLogo - 1] = TRUE;
            logos[lp].csp_mode[_curLogo - 1] = _curLogoMode;
	    
	    dsp_reloadLoop( lp, &ier );
	}
    }
/*
 *  If there is no active logoes in the loop and the user
 *  elects to add the selected logo, store logo information
 *  and add it, do not need to reload other data.
 */       
    else {
	    
	if ( _curLogoMode != 0 ) {

            logos[lp].logo_actv[_curLogo - 1] = TRUE;
            logos[lp].csp_mode[_curLogo - 1] = _curLogoMode;
	    dsp_addLogo( lp );
	}
    }
}

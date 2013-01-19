#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "ctbcmn.h"
#include "proto_xw.h"

#define	EXTRA	5.0F

#define MAX_UNIT_TYPES      3	/* Max. types of speed unit */
#define MAX_INTERV_TYPES   21	/* Max. number of duration options */

#define MIN_SPEED	  0.0F
#define MAX_SPEED	400.0F	/* miles per hour */

#define SPD_IN_KTS	    0	/* knots */
#define SPD_IN_MPH	    1	/* miles per hour */
#define SPD_IN_MPS	    2	/* meters per second */

#define MIN_DIRECTION	  0.0F
#define MAX_DIRECTION	360.0F

#define EXTRAP_BY_COPY	    0
#define EXTRAP_BY_MOVE	    1

static Widget		_extrap_dlgW;
static Widget		_spd_txtW;
static Widget		_spdunit_pbW[MAX_UNIT_TYPES];
static Widget		_dir_txtW;
static Widget		_dur_txtW;
static Widget		_dur_optW;
static Widget		_dur_pbW[MAX_INTERV_TYPES];
static Widget		_mvcp_optW[2];

static float		_spdValue = 30.0F;	/* default speed */
static int		_spdUnitIndx = 1;	/* knots */

static float		_dirValue = 270.0F;	/* default direction */

static int		_durIndx = 1;		/* Default to 00:30 */ 
static int		_durTime= 30;		/* default to 30 minutes */

static int		_extrapType = EXTRAP_BY_COPY;

static float    *_dcX, *_dcY;   /* Coordinates of the active element */
static int	_dcN;		/* Number of points in the active element */

/*
 *  private callback functions
 */
static void pgextrap_dirTxtCb  ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
static void pgextrap_dirVrfyCb ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
static void pgextrap_spdTxtCb  ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
static void pgextrap_spdVrfyCb ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
static void pgextrap_durTxtCb  ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
static void pgextrap_durOptCb  ( Widget, long, XtPointer );
static void pgextrap_spdUnitCb ( Widget, long, XtPointer );
static void pgextrap_mvcpOptCb ( Widget, long, XtPointer );

/*
 *  private functions
 */
static float pgextrap_getDisp ( void );
static void pgextrap_setDuration ( char *text );
static void pgextrap_actvExtrap ( void );
static void pgextrap_elExtrap ( void );
static void pgextrap_grpExtrap ( void );
static void pgextrap_wboxCalc ( void );
static void pgextrap_jetCalc ( VG_DBStruct *el );
static void pgextrap_gfaCalc ( VG_DBStruct *el );

/************************************************************************
 * nmap_pgextrap.c							*
 *									*
 * This module creates and displays the extrapolation box. It also	*
 * contains the callbacks for the box as well as functions to		*
 * extrapolate either a single object or a group of objects.		*
 *									*
 * CONTENTS:								*
 *  pgextrap_create()     create the extrapolation window		*
 *  pgextrap_popup()      pop up the extrapolation window		*
 *  pgextrap_popdown()    pop down the extrapolation window		*
 *  pgextrap_isUp()       querry whether the EXTRAP window is up    	*
 *  pgextrap_getDisp()    calculates the displacement value    		*
 *  pgextrap_getMode()    get EXTRAP mode (Copy or Move)     		*
 *  pgextrap_setDuration()set duration time from the given text		*
 *  pgextrap_actvExtrap() extrapolate the current active PEGN object	*
 *  pgextrap_elExtrap()	  extrapolate/save/display a single object	*
 *  pgextrap_grpExtrap()  extrapolate/save/display a group of objects	*
 *  pgextrap_wboxCalc()   calculate new watch box point/anchor		*
 *  pgextrap_jetCalc()    calculate jet's barb/hash locations		*
 *  pgextrap_gfaCalc()    calculate GFA's attribute box location	*
 *  pgextrap_start()	  start extrapolation on single or group object	*
 *									*
 *  pgextrap_spdTxtCb()   callback for speed text field 		*
 *  pgextrap_spdVrfyCb()  Limit speed input within MIN_ & MAX_SPEED	*
 *  pgextrap_spdUnitCb()  callback for speed unit menu options		*
 *  pgextrap_dirTxtCb()   callback for direction text field 		*
 *  pgextrap_dirVrfyCb()  Limit dir input within MIN_ & MAX_DIRECTION	*
 *  pgextrap_durTxtCb()   callback for duration text field		*
 *  pgextrap_durOptCb()   callback for duration menu options		*
 *  pgextrap_mvcpOptCb()  callback for Copy/Move radio button options 	*
 ***********************************************************************/

/*=====================================================================*/

void pgextrap_create ( Widget parent )
/************************************************************************
 * pgextrap_create							*
 *									*
 * This function creates the extrapolation window.			*
 *									*
 * void pgextrap_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget	paneW, rcW, lbl_rcW, txt_rcW, opt_rcW;
    Widget	spd_unit_pldW, spd_unit_optW;
    Widget	dur_pldW;
    Widget	mvcp_formW, mvcp_optW;    
    XmString	xmstr;
    int		dur_num, ier;
    long	ii, nn;
    float	prefs_val[4];
    char	*spd_unit_str[] = {"KTS", "MPH", "M/S"};
    char	**dur_str, cc[10];
    char	prefs[4][MAX_PREF_STR], dur_pref[MAX_PREF_STR];
    char	*mvcp_str[2] = {"Copy", "Move"};
    char	prefs_tag[4][MAX_PREF_STR] = { "EXTRAP_SPEED",
                  			       "EXTRAP_SPDUNIT",
					       "EXTRAP_DIRECTION",
					       "EXTRAP_DURATION" };
/*---------------------------------------------------------------------*/
    
    /*
     *  Create the extrapolation selection box.
     */
    _extrap_dlgW = XmCreateFormDialog ( parent, "extrap_popup",
				        NULL, 0 );
    XtVaSetValues ( _extrap_dlgW,
		    XmNnoResize,	TRUE,
		    XmNautoUnmanage,	FALSE,
		    NULL );

    xmstr = XmStringCreateLocalized ( "Extrapolation" );
    XtVaSetValues ( _extrap_dlgW, XmNdialogTitle, xmstr, NULL );
    XmStringFree ( xmstr );

    paneW  = XtVaCreateManagedWidget ( "pane",    
			xmPanedWindowWidgetClass,	_extrap_dlgW,
			XmNsashWidth,			1,
			XmNsashHeight,			1,
			NULL );

    /*
     * Create speed, direction, and duration inputs.
     */
    rcW = XtVaCreateWidget ( "Extrap_attribs",
			xmRowColumnWidgetClass, 	paneW,
			XmNorientation, 		XmHORIZONTAL,
			XmNradioAlwaysOne,		FALSE,
	 		XmNnumColumns,			3,
			NULL );

   
    /*
     * Labels  for speed, direction, and duration.
     */    
    lbl_rcW = XtVaCreateWidget ( "Extrap_attribs",
			xmRowColumnWidgetClass, 	rcW,
			XmNorientation, 		XmVERTICAL,
			XmNradioAlwaysOne,		FALSE,
	 		XmNnumColumns,			1,
                	XmNmarginHeight,		10, 
                	XmNspacing,         		18,
			NULL );
    XtVaCreateManagedWidget ( "Speed:",
			xmLabelGadgetClass,		lbl_rcW,
			NULL ); 
    
    XtVaCreateManagedWidget ( "Direction:",
			xmLabelGadgetClass,		lbl_rcW,
			NULL ); 

    XtVaCreateManagedWidget ( "Duration:",
			xmLabelGadgetClass,		lbl_rcW,
			NULL ); 

    /*
     *  Text fields for speed, direction, and duration.
     *  Use users' settings in "prefs.tbl" as defaults, if valid. 
     */    
    txt_rcW = XtVaCreateWidget( "Extrap_attribs",
			xmRowColumnWidgetClass, 	rcW,
			XmNorientation, 		XmVERTICAL,
			XmNradioAlwaysOne,		FALSE,
	 		XmNnumColumns,			1,
			NULL);

    for ( ii = 0; ii < 4; ii++ ) {
        ctb_pfstr ( prefs_tag[ii], prefs[ii], &ier );
        prefs_val[ii] = (float) atof ( prefs[ii] );
    }
    
    if ( MIN_SPEED <= prefs_val[0] && prefs_val[0] <= MAX_SPEED ) {
        _spdValue = prefs_val[0];
    }

    if ( SPD_IN_KTS <= (int)prefs_val[1] && (int)prefs_val[1] <= SPD_IN_MPS ) {
        _spdUnitIndx = (int)prefs_val[1];
    }

    if ( MIN_DIRECTION <= prefs_val[2] && prefs_val[2] <= MIN_DIRECTION ) {
        _dirValue = prefs_val[2];
    }
    
            
    sprintf ( cc, "%6.2f", _spdValue);   
    _spd_txtW = XtVaCreateManagedWidget ( "extrap_speed",
			xmTextFieldWidgetClass,		txt_rcW,
			XmNcolumns,	                8,
			XmNvalue,			cc,
			XmNcursorPositionVisible,	True,
			NULL );
    XtAddCallback ( _spd_txtW, XmNlosingFocusCallback, 
		       (XtCallbackProc)pgextrap_spdVrfyCb, NULL ); 
    XtAddCallback ( _spd_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgextrap_spdTxtCb, NULL );


    sprintf ( cc, "%6.2f", _dirValue);   
    _dir_txtW = XtVaCreateManagedWidget ( "extrap_dir",
			xmTextFieldWidgetClass,		txt_rcW,
			XmNcolumns,	                8,
			XmNvalue,			cc,
			XmNcursorPositionVisible,	True,
			NULL );
    XtAddCallback ( _dir_txtW, XmNlosingFocusCallback, 
		       (XtCallbackProc)pgextrap_dirVrfyCb, NULL ); 
    XtAddCallback ( _dir_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgextrap_dirTxtCb, NULL );


    _dur_txtW = XtVaCreateManagedWidget ( "extrap_dur",
			xmTextFieldWidgetClass,		txt_rcW,
			XmNcolumns,	                8,
			XmNcursorPositionVisible,	True,
			XmNmaxLength,			5,
			NULL );
    XtSetSensitive ( _dur_txtW, FALSE );    
    
    XtAddCallback ( _dur_txtW, XmNactivateCallback, 
		    (XtCallbackProc)pgextrap_durTxtCb, (XtPointer)NULL );
    XtAddCallback ( _dur_txtW, XmNlosingFocusCallback,
		   (XtCallbackProc)pgextrap_durTxtCb, (XtPointer)NULL );

    
    /*
     *  Options for speed unit.
     */    
    opt_rcW = XtVaCreateWidget( "Extrap_attribs",
			xmRowColumnWidgetClass, 	rcW,
			XmNorientation, 		XmVERTICAL,
			XmNradioAlwaysOne,		FALSE,
	 		XmNnumColumns,			1,
                	XmNmarginHeight,		2, 
                	XmNspacing,         		38,
			NULL);
    
    spd_unit_pldW = XmCreatePulldownMenu ( opt_rcW, "spd_unit_pld", NULL, 0 );
    spd_unit_optW = XmCreateOptionMenu ( opt_rcW, "spd_unit_opt", NULL, 0);

    nn = XtNumber ( spd_unit_str );
    for ( ii = 0; ii < nn; ii++ ) {
        xmstr = XmStringCreateLocalized ( spd_unit_str[ii] );
        _spdunit_pbW[ii] = XtVaCreateManagedWidget ( spd_unit_str[ii],
			xmPushButtonWidgetClass,	spd_unit_pldW,
			XmNlabelString,			xmstr,
			NULL);
        XmStringFree ( xmstr );
        XtAddCallback ( _spdunit_pbW[ii], XmNactivateCallback,
		        (XtCallbackProc)pgextrap_spdUnitCb, (XtPointer) ii );
    }
    
    xmstr = XmStringCreateLocalized ( "\0" );
    XtVaSetValues ( spd_unit_optW, 
			XmNlabelString,		xmstr,	
			XmNsubMenuId,		spd_unit_pldW,
			XmNmenuHistory,		_spdunit_pbW[_spdUnitIndx], 
			NULL );
    XmStringFree ( xmstr );

    XtManageChild ( spd_unit_optW );
    

    /*
     *  Options for duration periods from track table.
     */    
    dur_pldW = XmCreatePulldownMenu ( opt_rcW, "dur_pulldown", NULL, 0 );
    _dur_optW = XmCreateOptionMenu ( opt_rcW, "dur_opt", NULL, 0);

    ctb_trkqn ( &dur_num, &ier ); /* Get number of intervals */
    
    dur_str = (char **) malloc( (size_t)dur_num * sizeof( char *) ) ;
    for ( ii = 0; ii < dur_num; ii++ ) {
 	*(dur_str+ii) = (char *) malloc( 6 * sizeof( char ) );       
    }    
    
    ctb_trkitv ( dur_num, dur_str, &ier ); /* Get periods from table */
    
    for ( ii = 0; ii <= dur_num ; ii++ ) {
        if ( ii < dur_num ) {
	    sprintf ( cc, "%s", dur_str[ii] );
	} 
	else {
	    strcpy ( cc, "Other" );                 
        }
        	
        xmstr = XmStringCreateLocalized ( cc );        
        _dur_pbW[ii] = XtVaCreateManagedWidget ( cc,
			xmPushButtonWidgetClass,	dur_pldW,
			XmNlabelString,			xmstr,
			XmNalignment,			XmALIGNMENT_CENTER,
			NULL );
        XmStringFree ( xmstr );
        XtAddCallback ( _dur_pbW[ii], XmNactivateCallback,
		        (XtCallbackProc)pgextrap_durOptCb, (XtPointer) ii );
	
    }
        
    strcpy ( dur_pref, dur_str[_durIndx] );
    if ( strlen ( prefs[3] ) > (size_t)0 ) {  /*  Set duration pref. */
	
	_durIndx = dur_num;
        for ( ii = 0; ii < dur_num; ii++ ) {	
	    if ( strcmp ( prefs[3], dur_str[ii] ) == 0 ) {
	        _durIndx = ii;
	    }
	}
        
	pgextrap_setDuration ( prefs[3] ); 
        strcpy ( dur_pref, prefs[3] );
	
    }
    
    for ( ii = 0; ii < dur_num; ii++ ) {	
	free( *(dur_str+ii) );
    }
		    
    free( dur_str );		
             
    XtVaSetValues ( _dur_optW, 
			XmNsubMenuId,		dur_pldW,
			XmNmenuHistory,		_dur_pbW[_durIndx], 
			NULL );
    
    XmTextFieldSetString ( _dur_txtW, dur_pref );
    if ( _durIndx == dur_num ) {
        XtSetSensitive ( _dur_txtW, TRUE ); 
    }       
   
    XtManageChild ( _dur_optW );

    XtManageChild ( lbl_rcW );
    XtManageChild ( txt_rcW );
    XtManageChild ( opt_rcW );
    XtManageChild ( rcW );
   
    
    /*
     * Create copy/move radio options
     */
    mvcp_formW = XtVaCreateManagedWidget ( "mvcp_formW",
			xmFormWidgetClass,	paneW,
			NULL );
    mvcp_optW = XtVaCreateManagedWidget ( "mvcp_optW",
			xmRowColumnWidgetClass, mvcp_formW,
                	XmNpacking,             XmPACK_TIGHT,
                	XmNorientation,         XmHORIZONTAL,
			XmNradioBehavior,       True,
                	XmNtraversalOn,         False,
                	XmNmarginWidth,		50, 
                	XmNspacing,         	50,
               		NULL );

    for ( ii = 0; ii < 2; ii++ ) {
	_mvcp_optW[ii] = XtVaCreateManagedWidget ( mvcp_str[ii],
                	xmToggleButtonGadgetClass,	mvcp_optW,
                	NULL );
        XtAddCallback ( _mvcp_optW[ii], XmNarmCallback,
		        (XtCallbackProc)pgextrap_mvcpOptCb, (XtPointer) ii );
    }
    
    XmToggleButtonSetState ( _mvcp_optW[_extrapType], TRUE, TRUE );       

    return;
}

/*=====================================================================*/

void pgextrap_popup ( void )
/************************************************************************
 * pgextrap_popup							*
 *									*
 * This function pops up the extrapolation window.			*
 *									*
 * void pgextrap_popup ( void )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{

    if ( !XtIsManaged ( _extrap_dlgW ) ) {
	 XtManageChild ( _extrap_dlgW );
    }

}

/*=====================================================================*/

void pgextrap_popdown ( void )
/************************************************************************
 * pgextrap_popdown							*
 *									*
 * This function pops down the extrapolation window.			*
 *									*
 * void pgextrap_popdown ( void )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    if ( XtIsManaged ( _extrap_dlgW ) ) { 	 
	 XtUnmanageChild ( _extrap_dlgW );
    }    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_spdTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgextrap_spdTxtCb                                      		*
 *                                                                      *
 * Callback for speed text widget.                              	*
 *                                                                      *
 * static void pgextrap_spdTxtCb( w, clnt, cbs )		        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log: 								*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    char        *ss;
    float	spd;
/*---------------------------------------------------------------------*/

    if ( !cbs->event ) 
	return;

    ss = XmTextFieldGetString ( _spd_txtW );
    spd = (float) ( atof(ss) );
    XtFree ( ss );

    if ( MIN_SPEED <= spd && spd <= MAX_SPEED ) {
        _spdValue = spd;
    }

}
/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_spdVrfyCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgextrap_spdVrfyCb                                      		*
 *                                                                      *
 * Callback to verify the speed input. Invalid input will be ignored	* 
 * and the text field will be set back to the last valid input.		*
 *                                                                      *
 * static void pgextrap_spdVrfyCb( w, clnt, cbs )	        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log: 								*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    char        *ss, cc[10];
    float	spd;
/*---------------------------------------------------------------------*/

    ss = XmTextFieldGetString ( _spd_txtW );
    spd = (float) ( atof(ss) );
    XtFree ( ss );

    if ( spd < MIN_SPEED || spd > MAX_SPEED ) {
        sprintf ( cc, "%6.2f", _spdValue );   
	XmTextFieldSetString ( _spd_txtW, cc ); 
    }        
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_dirTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgextrap_dirTxtCb                                      		*
 *                                                                      *
 * Callback for direction text widget.                              	*
 *                                                                      *
 * static void pgextrap_dirTxtCb( w, clnt, cbs )		        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log: 								*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    char        *ss;
    float	dir;
/*---------------------------------------------------------------------*/

    if ( !cbs->event ) 
	return;

    ss = XmTextFieldGetString ( _dir_txtW );
    dir = (float) ( atof(ss) );
    XtFree ( ss );

    if ( MIN_DIRECTION <= dir && dir <= MAX_DIRECTION ) {
        _dirValue = dir;
    }
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_dirVrfyCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgextrap_dirVrfyCb                                      		*
 *                                                                      *
 * Callback to verify the direction input. Invalid input will be ignored* 
 * and the text field will be set back to the last valid input.		*
 *                                                                      *
 * static void pgextrap_dirVrfyCb( w, clnt, cbs )	        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log: 								*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    char        *ss, cc[10];
    float	dir;
/*---------------------------------------------------------------------*/

    ss = XmTextFieldGetString ( _dir_txtW );
    dir = (float) ( atof(ss) );
    XtFree ( ss );

    if ( dir < MIN_DIRECTION || dir > MAX_DIRECTION ) {
        sprintf ( cc, "%6.2f", _dirValue );   
	XmTextFieldSetString ( _dir_txtW, cc ); 
    }        
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_durTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgextrap_durTxtCb                                      		*
 *                                                                      *
 * Callback for duration text widget.                              	*
 *                                                                      *
 * static void pgextrap_durTxtCb( w, clnt, cbs )		        *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log: 								*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    char        *pstr, new_factor[10];
/*---------------------------------------------------------------------*/

    pstr = XmTextGetString ( _dur_txtW );
    strcpy ( new_factor, pstr );

    pgextrap_setDuration ( pstr );
    XtFree ( pstr );

    if ( XtIsSensitive(_dur_txtW) ) {
        if ( _durTime == 30 )    
      	    XmTextFieldSetString ( _dur_txtW, "00:30" );
	else
	    XmTextFieldSetString ( _dur_txtW, new_factor );
    }

}

/*=====================================================================*/

/* ARGSUSED */
static void pgextrap_spdUnitCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgextrap_spdUnitCb							*
 *									*
 * Callback function for speed unit option menu.			*
 *									*
 * static void pgextrap_spdUnitCb (w, which, call )			*
 *									*
 * Input parameters:							*
 *	w	Widget		option button widget ID			*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    _spdUnitIndx = (int)which;
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_durOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgextrap_durOptCb							*
 *									*
 * Callback function for duration option menu.				*
 *									*
 * static void pgextrap_durOptCb (w, which, call )			*
 *									*
 * Input parameters:							*
 *	w	Widget		option button widget ID			*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    XmString    xmstr;
    char        *text;

/*---------------------------------------------------------------------*/
    
    _durIndx = (int)which;
    XtVaSetValues ( _dur_optW,
			XmNmenuHistory,		_dur_pbW[_durIndx],
			NULL );

    XtVaGetValues ( _dur_pbW[_durIndx],
			XmNlabelString,		&xmstr,
			NULL );

    XmStringGetLtoR ( xmstr, XmFONTLIST_DEFAULT_TAG, &text );
    XmStringFree ( xmstr );
    
    if ( strcmp ( text, "Other" ) == 0 ) {   /* Other */
	XtSetSensitive ( _dur_txtW, TRUE );
	sprintf ( text, "%02d:%02d", _durTime/60, _durTime%60 );
	XmTextFieldSetString ( _dur_txtW, text);
    } 
    else {
	XmTextFieldSetString ( _dur_txtW, text ); 
	XtSetSensitive ( _dur_txtW, FALSE );
    }

    pgextrap_setDuration ( text );
    XtFree(text);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgextrap_mvcpOptCb ( Widget w, long clnt, XtPointer call )
/************************************************************************
 * pgextrap_mvcpOptCb							*
 *									*
 * Callback for move/copy radio box widget.				*
 *									*
 * static void pgextrap_mvcpOptCb ( w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	w		Widget			widget ID		*
 *	clnt		long			client data		*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    int         which;
/*---------------------------------------------------------------------*/

    which = (int)clnt;
            
    if ( which != _extrapType ) {
        _extrapType = which;
        XmToggleButtonSetState ( _mvcp_optW[which], TRUE, TRUE );       
    }
}

/*=====================================================================*/

static void pgextrap_setDuration ( char *text )
/************************************************************************
 * pgextrap_setDuration							*
 *                                                                      *
 * This function sets the duration time (unit: minute) from the given	*
 * duration text (format - HH:MM).					*
 *                                                                      *
 * static void pgextrap_setDuration ( text )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*text		char		input text			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/02   modify from pgtrkw_getInterv()		*
 ***********************************************************************/
{
    int		ii, ier, min, iarr[2];

/*---------------------------------------------------------------------*/
    
    cst_ilst ( text, ':', 0, 2, iarr, &ii, &ier );

    if ( ii == 2 ) {
	min = iarr[0] * 60 + iarr[1];
    } 
    else {
	min = iarr[0];
    }

    if ( min <= 0 ) {
        _durTime = 30;
    }  else {
        _durTime = min;
    }
} 

/*=====================================================================*/

Boolean pgextrap_isUp ( void )
/************************************************************************
 * pgextrap_isUp							*
 *									*
 * This function returns a boolean value specifying whether the extrap	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgextrap_isUp ()						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *   pgextrap_isUp()	Boolean		True -- up, 	False -- down 	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02   intial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged ( _extrap_dlgW ) );
}

/*=====================================================================*/

int pgextrap_getMode ( void )
/************************************************************************
 * pgextrap_getMode							*
 *									*
 * This function returns the extrapolation mode (copy or move)		*
 *									*
 * int pgextrap_getMode ( void )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *   pgextrap_getMode()		int	Extrapolation mode 		*
 *					0 - Extrapolate by copy		*
 *					1 - Extrapolate by move		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02   initial coding				*
 ***********************************************************************/
{
    return ( _extrapType );
}

/*=====================================================================*/

static float pgextrap_getDisp ( void )
/************************************************************************
 * pgextrap_getDisp							*
 *									*
 * This function calculates the displacement from speed and duration. 	*
 *									*
 * static float pgextrap_getDisp ( void )				*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *   pgextrap_getDisp()		float	Extrapolation displacement 	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02   initial coding				*
 ***********************************************************************/
{
    float    disp;
    
/*---------------------------------------------------------------------*/
        
    disp = _spdValue * (float)_durTime * 60.0F; /* Dispalcement in meters */
    
    if ( _spdUnitIndx == SPD_IN_KTS ) {
	disp *= NMH2MS;    
    }
    else if ( _spdUnitIndx == SPD_IN_MPH ) {
	disp *= SMH2MS;
    }
          
    return ( disp );
}

/*=====================================================================*/

void pgextrap_start ( char grptyp, int grpnum )
/************************************************************************
 * pgextrap_start                                                       *
 *                                                                      *
 * Function to complete EXTRAP action used by pgevt_extrapHdl.   	*
 *                                                                      *
 * void pgextrap_start ( grptyp, grpnum )         			*
 *                                                                      *
 * Input parameters:                                                    *
 * 	grptyp		char	group type of selected element		*
 *	grpnum		int	group number of selected element	*
 *									*
 * Output parameters:                                                   *
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    if ( grptyp && grpnum && 
       	  (grptyp == GRPTYP_COMSYM || pgpalw_getMode() == TYPE_GRP) ) {
	pgextrap_grpExtrap ();
    }
    else {
        pgextrap_elExtrap ();    
    } 
}

/*=====================================================================*/

static void pgextrap_actvExtrap ( void )
/************************************************************************
 *  pgextrap_actvExtrap                                                 *
 *                                                                      *
 * This function extrapolates the current active PGEN element.       	*
 *                                                                      *
 * static void pgextrap_actvExtrap ( void )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/02	initial coding				*
 ***********************************************************************/
{
    int		ii, ier;
    float	disp, dir;
    float	*tmplat, *tmplon, *newlat, *newlon;

/*---------------------------------------------------------------------*/
/* 
 *  Retrieve the displacement and direction.
 *  Note: the wind direction (counter clockwise from N) should be
 *  reversed clockwise to be used in clo_dltln().
 */
    disp = pgextrap_getDisp ();
    if ( _dirValue >= 180.0F ) {
        dir = _dirValue - 180.0F; 
    }
    else {
        dir = _dirValue + 180.0F;     
    }
    
/*
 *  Retrieve the coordinates for the current active element.
 */
    pgactv_getDevPts ( &_dcN, &_dcX, &_dcY );
    
    
    if  ( _dcN <= 0 ) return;
	
    tmplon = (float *) malloc ( sizeof(float) * (size_t)_dcN );
    tmplat = (float *) malloc ( sizeof(float) * (size_t)_dcN );
    newlat = (float *) malloc ( sizeof(float) * (size_t)_dcN );
    newlon = (float *) malloc ( sizeof(float) * (size_t)_dcN );
 
/*
 *  Compute the new coordinates - need to transfer from the
 *  device coordinates to map coordinates for extrapolation.
 *  Then convert back to device coordinates. 
 */
    gtrans ( sys_D, sys_M, &_dcN, _dcX, _dcY, tmplat, tmplon, 
		&ier, strlen(sys_D), strlen(sys_M) );
    
    for ( ii = 0; ii < _dcN; ii++) {
        clo_dltln ( &tmplat[ii], &tmplon[ii], &disp, &dir,
	            &newlat[ii], &newlon[ii], &ier );
    }
 
    gtrans ( sys_M, sys_D, &_dcN, newlat, newlon, _dcX, _dcY, 
    		&ier, strlen(sys_M), strlen(sys_D) );
		   
    free ( tmplat );
    free ( tmplon );
    free ( newlat );
    free ( newlon );
}

/*=====================================================================*/

static void pgextrap_elExtrap ( void )
/************************************************************************
 * pgextrap_elExtrap							*
 *									*
 * This function extrapolates a single element.				*
 *									*
 * static void pgextrap_elExtrap ( void )				*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02   initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		12/03   add extrapolation for jet's barb/hash	*
 * J. Wu/SAIC		02/04   extrapolate GFA's attr. box location	*
 * J. Wu/SAIC         	07/04   add filter param to crg_get		*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
    int         ier, location, new_loc, num, layer, el_layer, found;    
    int         update_crg;
    float       llx, lly, urx, ury, inf_bbox[4]; 
    float       o_llx, o_lly, o_urx, o_ury;
    VG_DBStruct el, del_el;
    filter_t	dsplyFilter;
        
/*---------------------------------------------------------------------*/
        
    update_crg = 0;

    pgundo_newStep ();
    
/*
 *  Retrieve the active element and deselect it.
 */    
    location = pgactv_getElmLoc ();
    cvg_rdrec ( cvg_getworkfile(), location, &el, &ier );

    crg_getinx ( location, &num, &ier );
    crg_get ( num, &el_layer, dsplyFilter, &o_llx, &o_lly, &o_urx, &o_ury, &ier );
    
    pghdlb_deselectEl ( location, FALSE );     
        
/*
 *  Extrapolate the active element.
 */    
    pgextrap_actvExtrap ();
            
    if ( el.hdr.vg_type == WBOX_ELM ) {
	pgwpts_setSnap ( TRUE );
        pgextrap_wboxCalc ();
    }
    
    if ( el.hdr.vg_type == JET_ELM ) {
        pgextrap_jetCalc ( &el );    
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        pgextrap_gfaCalc ( &el );    
    }
    
/*
 *  Delete the old element if extrapolate by "MOVE".
 */
    if ( pgextrap_getMode() == 1 ) {
/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
        cvg_rdrec ( cvg_getworkfile(), location, &del_el, &ier );
        cvg_checkplace(&del_el, 1, location, &found, inf_bbox, &ier);
        if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area passed in
 */
            o_llx = G_MIN(o_llx, inf_bbox[0]);
            o_lly = G_MIN(o_lly, inf_bbox[2]);
            o_urx = G_MAX(o_urx, inf_bbox[1]);
            o_ury = G_MAX(o_ury, inf_bbox[3]);
            update_crg = 1;
        }

        /*
         *  Free GFA block memory.
         */
        if ( del_el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &del_el );
        }

	cvg_delet ( cvg_getworkfile(), location, TRUE, &ier );
	crg_clear ( num, &ier );
        pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
    }
    
    
    /*
     *  Handle the watches.
     */    
    if ( el.hdr.vg_type == WBOX_ELM )  {

        pgwbxw_getAnchor ( 0, el.elem.wbx.info.w_a0id,
                        &el.elem.wbx.info.w_a0lt, &el.elem.wbx.info.w_a0ln,
                        &el.elem.wbx.info.w_a0dis, el.elem.wbx.info.w_a0dir,
                        &ier );

        pgwbxw_getAnchor ( 1, el.elem.wbx.info.w_a1id,
                        &el.elem.wbx.info.w_a1lt, &el.elem.wbx.info.w_a1ln,
                        &el.elem.wbx.info.w_a1dis, el.elem.wbx.info.w_a1dir,
                        &ier );

	/*
	 *  Wipe the county list
 	 */
 	el.elem.wbx.info.numcnty = 0;
    }
    

    /*
     *  Save the new element.
     */
    pgvgf_saveNewElm ( NULL, sys_D, &el, _dcN, _dcX, _dcY, FALSE, &new_loc,
    		&ier );
    pgundo_storeThisLoc ( new_loc, UNDO_ADD, &ier );
    pgundo_endStep ();

    /*
     *  Free GFA block memory.
     */
    if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }
   

    /*
     *  Build the range record for the new element and display it.
     */
    cvg_rdrec ( cvg_getworkfile(), new_loc, &el, &ier );
    layer = pglayer_getCurLayer ();
    crg_set ( &el, new_loc, layer, &ier );

    crg_getinx ( new_loc, &num, &ier );
    crg_get ( num, &el_layer, dsplyFilter, &llx, &lly, &urx, &ury, &ier );

    if ( o_llx < llx )
        llx = o_llx;
    if ( o_lly < lly )
        lly = o_lly;
    if ( o_urx > urx )
        urx = o_urx;
    if ( o_ury > ury )
        ury = o_ury;

    /*
     * Mark elements in placement that are effected by
     * the new element, and get the area of influence back
     */
    cvg_checkplace(&el, 0, new_loc, &found, inf_bbox, &ier);
    if (found > 0) {
        /*
         * Update the refresh extent if the area impacted by
         * placement was bigger than the area passed in
         */
        llx = G_MIN(llx, inf_bbox[0]);
        lly = G_MIN(lly, inf_bbox[2]);
        urx = G_MAX(urx, inf_bbox[1]);
        ury = G_MAX(ury, inf_bbox[3]);
        update_crg = 1;
    }
    
    xpgpaste ( llx, lly, urx, ury, &ier );
    cvg_rfrsh ( NULL, llx, lly, urx, ury, &ier );
    /*
     * If placement effected other areas, then we will
     * need to rebuild all the CRG records since we
     * don't know all the elements effected
     */
    if (update_crg) {
        crg_rebuild();
    }

    /*
     *  Free GFA block memory.
     */
    if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

}

/*=====================================================================*/

static void pgextrap_grpExtrap ( void )
/************************************************************************
 *  pgextrap_grpExtrap                                                  *
 *                                                                      *
 * This function extrapolates a group of elements.                 	*
 *                                                                      *
 * static void pgextrap_grpExtrap ( void )                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/02	initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		12/03   add extrapolation for jet's barb/hash	*
 * J. Wu/SAIC		02/04   extrapolate GFA's attr. box location	*
 * J. Wu/SAIC		10/04   free GFA block memory			* 
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
    int		location, ier, nelm, ii, *inxarry, layer;
    int		newnum, old_location, found, update_crg;
    float	llx, lly, urx, ury, inf_bbox[4];
    float	o_llx, o_lly, o_urx, o_ury;
    char	newtyp;
    VG_DBStruct	el, del_el;
/*---------------------------------------------------------------------*/

    old_location = pgactv_getElmLoc ();

    cvg_rdrec ( cvg_getworkfile(), old_location, &el, &ier );
    crg_ggnel ( el.hdr.grptyp, el.hdr.grpnum, &nelm, &ier );

    if ( nelm <= 0 )  return;

    inxarry = (int *) malloc ( (size_t)nelm * sizeof(int) );
    crg_gginx ( el.hdr.grptyp, el.hdr.grpnum, nelm, inxarry, &nelm, &ier );
        
    newtyp = el.hdr.grptyp;
    newnum = el.hdr.grpnum;
    crg_ggbnd ( newtyp, newnum, &o_llx, &o_urx, &o_ury, &o_lly, &ier );

    if ( pgextrap_getMode() == 0 )  /* Copy */
        crg_ggnxt ( el.hdr.grptyp, &newnum, &ier );
    
    pghdlb_deselectEl ( old_location, FALSE ); 

    /*
     * Free GFA memory
     */
    if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

    update_crg = 0;
    pgundo_newStep ();
    layer = pglayer_getCurLayer ();
    for ( ii = 0; ii < nelm; ii++ ) {

        crg_goffset ( inxarry[ii], &location, &ier );

        cvg_rdrec ( cvg_getworkfile(), location, &el, &ier );

	pgactv_setActvElm ( &el, location);
 
        /*
         *  Extrapolate the active element.
         */
	pgextrap_actvExtrap ();	   
        
        if ( el.hdr.vg_type == JET_ELM ) {
            pgextrap_jetCalc ( &el );    
        }
        else if ( el.hdr.vg_type == GFA_ELM ) {
            pgextrap_gfaCalc ( &el );    
        }
	
        /*
         *  Delete the old element if extrapolate by "MOVE".
         */
	if ( pgextrap_getMode() == 1 ) {	  
            /*
             * Mark elements in placement that are effected by
             * the delete, and get the area of influence back
             */
            cvg_rdrec ( cvg_getworkfile(), location, &del_el, &ier );
            cvg_checkplace(&del_el, 1, location, &found, inf_bbox, &ier);
            if (found > 0) {
                /*
                 * Update the refresh extent if the area impacted by
                 * placement was bigger than the area passed in
                 */
                o_llx = G_MIN(o_llx, inf_bbox[0]);
                o_lly = G_MIN(o_lly, inf_bbox[2]);
                o_urx = G_MAX(o_urx, inf_bbox[1]);
                o_ury = G_MAX(o_ury, inf_bbox[3]);
                update_crg = 1;
            }

            /*
             * Free GFA memory
             */
            if ( del_el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &del_el );
            }

	    cvg_delet ( cvg_getworkfile(), location, TRUE, &ier );
	    crg_clear ( inxarry[ii], &ier ); 
            pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
        }

        el.hdr.grptyp = newtyp;
        el.hdr.grpnum = newnum;

        /*
         *  Save the new element.
         */
        pgvgf_saveNewElm ( NULL, sys_D, &el, _dcN, _dcX, _dcY, FALSE, &location,
			&ier );
        pgundo_storeThisLoc ( location, UNDO_ADD, &ier );

        /*
          * Free GFA memory
          */
        if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
        
	cvg_rdrec ( cvg_getworkfile(), location, &el, &ier );

        /*
         * Mark elements in placement that are effected by
         * the new element, and get the area of influence back
         */
        cvg_checkplace(&el, 0, location, &found, inf_bbox, &ier);
        if (found > 0) {
            /*
             * Update the refresh extent if the area impacted by
             * placement was bigger than the area passed in
             */
            o_llx = G_MIN(o_llx, inf_bbox[0]);
            o_lly = G_MIN(o_lly, inf_bbox[2]);
            o_urx = G_MAX(o_urx, inf_bbox[1]);
            o_ury = G_MAX(o_ury, inf_bbox[3]);

            update_crg = 1;
        } else if (update_crg == 0) {
            crg_set ( &el, location, layer, &ier );
        }
        
        /*
          * Free GFA memory
          */
        if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }

     }  /* for */
    
    pgundo_endStep();

    crg_ggbnd ( newtyp, newnum, &llx, &urx, &ury, &lly, &ier ); 

    free ( inxarry );

    o_llx -= EXTRA;
    o_lly -= EXTRA;
    o_urx += EXTRA;
    o_ury += EXTRA;

    if ( o_llx < llx )
        llx = o_llx;
    if ( o_lly < lly )
        lly = o_lly;
    if ( o_urx > urx )
        urx = o_urx;
    if ( o_ury > ury )
        ury = o_ury;

    xpgpaste ( llx, lly, urx, ury, &ier );
    cvg_rfrsh ( NULL, llx, lly, urx, ury, &ier );
    /*
     * If placement effected other areas, then we will
     * need to rebuild all the CRG records since we
     * don't know all the elements effected
     */
    if (update_crg) {
        crg_rebuild();
    }

}

/*=====================================================================*/

static void pgextrap_wboxCalc ( void )
/************************************************************************
 * pgextrap_wboxCalc                                         		*
 *                                                                      *
 * Internal function for EXTRAP watch box point/anchor calculation.	*
 *                                                                      *
 * static void pgextrap_wboxCalc ( void )          			*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/02	modify from _pgmvcp_wboxCalc()		*
 * H. Zeng/XTRIA	01/03   modifed arguments to pgwbxw_getAttr	*
 ***********************************************************************/
{
    int           ii, np, cnty_colr, ier;
    signed char	  cnty_fill;
    int           nearest, wcolr, wstyle, wshape, mtype, mwidth;
    float	  rx[8], ry[8], rx2[8], ry2[8], tlat[8], tlon[8], msize;
/*---------------------------------------------------------------------*/

        /*
         * get the new corner points (by calculating anchor points)
         */
        pgwbxw_getAttr ( &wcolr, &wstyle, &wshape,
			 &mtype, &msize,  &mwidth,
			 &cnty_fill, &cnty_colr    );

        np = 8;

	for ( ii = 0; ii < np; ii++ )  {
	    rx[ii] = *(_dcX +ii);
	    ry[ii] = *(_dcY +ii);
	}
        
	gtrans ( sys_D, sys_M, &np, rx, ry, tlat, tlon, &ier,
                 strlen(sys_D), strlen(sys_M) );
	
	nearest = 0;
        pgwpts_get ( nearest, wshape, tlat, tlon, tlat, tlon, &ier );
        
	gtrans ( sys_M, sys_D, &np, tlat, tlon, rx2, ry2, &ier,
                 strlen(sys_M), strlen(sys_D) );

        /*
         * assign the new values
         */
        for (ii = 0; ii < np; ii++) {
            _dcX[ii] = rx2[ii];
            _dcY[ii] = ry2[ii];
        }
        
	_dcX[np+1] = _dcX[0];
        _dcY[np+1] = _dcY[0];
        _dcX[np+2] = ( _dcX[2] + _dcX[6] ) / 2.0F;
        _dcY[np+2] = ( _dcY[2] + _dcY[6] ) / 2.0F;
        _dcX[np+3] = _dcX[4];
        _dcY[np+3] = _dcY[4];

}

/*=====================================================================*/
static void pgextrap_jetCalc ( VG_DBStruct *el )
/************************************************************************
 * pgextrap_jetCalc                                                     *
 *                                                                      *
 * Internal function to EXTRAPolate jet's barbs/hashs.			*
 *                                                                      *
 * static void pgextrap_jetCalc ( el )                 			*
 *      *el		VG_DBStruct	Pointer to a jet element	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		12/03	initial coding				*
 ***********************************************************************/
{
    int		ii, ier;
    float	disp, dir, lat, lon;
/*---------------------------------------------------------------------*/
    
    /* 
     *  Retrieve the displacement and direction.
     *  Note: the wind direction (counter clockwise from N) should be
     *  reversed clockwise to be used in clo_dltln().
     */
    disp = pgextrap_getDisp ();
    if ( _dirValue >= 180.0F ) {
         dir = _dirValue - 180.0F; 
    }
    else {
         dir = _dirValue + 180.0F;     
    }

    /* 
     *  Extrapolate barb/hash.
     */
    if ( el->elem.jet.nbarb > 0 ) {	    
	 for ( ii = 0; ii < el->elem.jet.nbarb; ii++ ) {
	     clo_dltln ( &el->elem.jet.barb[ii].wnd.data.latlon[0],
		        &el->elem.jet.barb[ii].wnd.data.latlon[1],
		        &disp, &dir, &lat, &lon, &ier );
		
	     el->elem.jet.barb[ii].wnd.data.latlon[0] = lat;
	     el->elem.jet.barb[ii].wnd.data.latlon[1] = lon;	    
	 }
    }
        
    if ( el->elem.jet.nhash > 0 ) {
	for ( ii = 0; ii < el->elem.jet.nhash; ii++ ) {
            clo_dltln ( &el->elem.jet.hash[ii].wnd.data.latlon[0],
		        &el->elem.jet.hash[ii].wnd.data.latlon[1],
		        &disp, &dir, &lat, &lon, &ier );
		
	    el->elem.jet.hash[ii].wnd.data.latlon[0] = lat;
	    el->elem.jet.hash[ii].wnd.data.latlon[1] = lon;	
	}
    }
}

/*=====================================================================*/
static void pgextrap_gfaCalc ( VG_DBStruct *el )
/************************************************************************
 * pgextrap_gfaCalc                                                     *
 *                                                                      *
 * Internal function to EXTRAPolate gfa's attribute box location.	*
 *                                                                      *
 * static void pgextrap_gfaCalc ( el )                        		*
 *      *el		VG_DBStruct	Pointer to a gfa element	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		10/04	access GFA attr with cvg_get/setFld	*
 * J. Wu/SAIC		05/08	let attibute box arrow point to center	*
 ***********************************************************************/
{
    int		ier, one = 1;
    float	disp, dir, lat, lon, tlat, tlon, igr;
    char	value[32];
/*---------------------------------------------------------------------*/
    
    /* 
     *  Retrieve the displacement and direction.
     *  Note: the wind direction (counter clockwise from N) should be
     *  reversed clockwise to be used in clo_dltln().
     */
    disp = pgextrap_getDisp ();
    if ( _dirValue >= 180.0F ) {
         dir = _dirValue - 180.0F; 
    }
    else {
         dir = _dirValue + 180.0F;     
    }

    /* 
     *  Extrapolate attribute box location.
     */
    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    tlat = atof ( value );
    cvg_getFld ( el, TAG_GFA_LON, value, &ier );
    tlon = atof ( value );
    
    clo_dltln ( &tlat, &tlon, &disp, &dir, &lat, &lon, &ier );
		
    sprintf ( value, "%7.2f", lat );
    cvg_setFld ( el, TAG_GFA_LAT, value, &ier );
    
    sprintf ( value, "%7.2f", lon );
    cvg_setFld ( el, TAG_GFA_LON, value, &ier );
 
 
    /* 
     *  Let the attribute box point to the center.
     */
    cgr_centroid ( _dcX, _dcY, &_dcN, &tlat, &tlon, &igr, &ier );
            
    gtrans ( sys_D, sys_M, &one, &tlat, &tlon, &lat, &lon, 
		 &ier, strlen(sys_D), strlen(sys_M) );
    
    sprintf ( value, "%7.2f", lat );
    cvg_setFld ( el, TAG_GFA_ARROW_LAT, value, &ier );
    sprintf ( value, "%7.2f", lon );
    cvg_setFld ( el, TAG_GFA_ARROW_LON, value, &ier );

}

/*=====================================================================*/

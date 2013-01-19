#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h" 
#include "hints.h"
#include "Nxm.h"


#define DEFAULT_SMEAR_COLOR	( 4 )	/* Blue */
#define DEFAULT_OUTLOOK_COLOR 	( 8 )   /* Brown */	
#define TOP			( 0 )
#define BOTTOM			( 1 )
#define SPECIAL_INTERVAL	( 15 )	/* Mimimum interval in minute allowed 
					       for GFA forecast hour */
#define BAD_NUMBER_OF_POINTS    ( -28 )

#define TIE_DIST_IN_MAP    	( 0.01 ) /* Tie distance for two points in
						map coordinate */

enum states { SMEAR_ALL, SMEAR_TAG, SMEAR_NONE };	/* logical state of */
							/*  smear functions  */
static int _state;				

static Widget	_smearForm;
static Widget	_cntlForm;

static char 	_smearTag[32];
static char 	_smearHazardTyp[32];
static char 	_smearVgType  = GFA_ELM;
static int	_numElSelected  = 0;

static int	_smearColor   = DEFAULT_SMEAR_COLOR;
static int	_outlkColor = DEFAULT_OUTLOOK_COLOR;

static Boolean	_confirmEnabled = False;
/*Only have two smear algorithms now, use boolean to determine which algorithm to use for GFA smearing*/
static Boolean  _smearUnionOnly = False; 

/*
 * typedefs used for smear all processing
 */
struct GfaElm_t {
   int			fileLoc;
   char			fcstHr[50];
   struct GfaElm_t	*next;
};

struct GfaList_t {
   char			hazard[50];
   char			tag[50];
   struct GfaElm_t	*elm;
   struct GfaList_t	*next;
};


/*
 *  private callback functions
 */
static void pgsmear_ctlBtnCb	    ( Widget, long, XtPointer );
static void pgsmear_confirmOkCb     ( Widget, XtPointer, XtPointer );
static void pgsmear_confirmCancelCb ( Widget, XtPointer, XtPointer );


/*
 *  private functions
 */
static void pgsmear_multiSmear(    	int 	     	numEls, 
				   	VG_DBStruct	**el_in, 
			   		Boolean 	timeSmear,
			   		Boolean 	unionOnly,
                                   	int       	*numPts, 
				   	float     	xPts[], 
				   	float     	yPts[],
				   	char	     	*fcstHr, 
					char 		*snapshotHrs,
				   	char      	*top,
				   	char      	*bot,
					char		*topFzl,
					char		*botFzl,
				        char            *worstFreq, 
					char            *worstSev,
					char            *worstIssue,
					char            *IFRType,
                                        char            *MtnObscType,
					char		*sigmetRef,
				   	int       	*iret );

static void pgsmear_selectSmearEl( 	XEvent    	*event,
				   	int	     	*iret );

static void pgsmear_smearAll(	   	void );

static void pgsmear_makeSmearList( 	int 	     	layer, 
				   	struct GfaList_t **list ) ;

static void pgsmear_getElmLocs (    	struct GfaList_t *list,
				   	char		*fcstHr,
				   	int		*nElm,
				   	int		elmLoc[] );

static struct GfaList_t *pgsmear_newGfaListT( 		void );
static struct GfaElm_t *pgsmear_newGfaElmT(		void );

static void pgsmear_freeGfaList(   	struct GfaList_t *list );

static void pgsmear_popup ( 		void );
static void pgsmear_exit ( 		void );
static void pgsmear_deleteSmears(	void );

static void pgsmear_doSnap ( 		VG_DBStruct 	*el, 
					int 		numPts, 
					float 		*xPts, 
					float 		*yPts );
					
static void pgsmear_smearTwoHour ( 	struct GfaList_t	*list, 
					char 		*startHr,
					char 		*endHr,
			   		Boolean 	doSnap,
					int		color,
			   		VG_DBStruct	*el,
					int		*numPts,
			   		float		xPts[],
			   		float		yPts[] );

static Boolean pgsmear_isOutlkExist (	struct GfaList_t  *list,
				        int outlkHr );

static void pgsmear_getMinMaxFcstHr (	const char	*fcstHr1,
				 	const char	*fcstHr2,
					char		minFcstHr[], 
					char		maxFcstHr[], 
			   		int		*iret );
static void pgsmear_fcstHr2Min (	const char 	*fcstHr, 
					int 		*minu, 
					int 		*iret );


static int pgsmear_getSubType ( VG_DBStruct *el, int category );

static Boolean pgsmear_isFmtable ( char *hazIn );

/************************************************************************
 * nmap_pgsmear.c                                                       *
 *                                                                      *
 * This module controls the smear function.                           	*
 *                                                                      *
 * CONTENTS:                                                            *
 *   pgsmear_create()		Creates the smear control window	*
 *   pgsmear_popdown()		Unmanages the smear control window	*
 *   pgsmear_isUp()		queries whether the smear window is up	*
 *									*
 *   pgsmear_selectElmEh	Route all mouse clicks to state handler *
 *   pgsmear_startSmear		Initialize for smear         		*
 *   pgsmear_smear        	Perform the smear               	*
 *									*
 * private functions:							*
 *   pgsmear_popup()		Manages the popup window		*
 *   pgsmear_ctlBtnCb()		Callback for control buttons		*
 *   pgsmear_confirmOkCb()	Callback for confirmation OK button	*
 *   pgsmear_confirmCancelCb()	Callback for confirmation Cancel button	*
 *   pgsmear_multiSmear  	Smear N figures         		*
 *   pgsmear_selectSmearEl	Select a vg element for a smear 	*
 *   pgsmear_smearAll        	Perform the smear all               	*
 *   pgsmear_exit       	Exits smear mode and cleans up          *
 *   pgsmear_deleteSmears      	Deletes all pre-existing smears         *
 *   pgsmear_smearTwoHours      Smear elements between 2 forecast hours	*
 *   pgsmear_isOutlkExist	Check if an outlook can be produced 	*
 *   pgsmear_getMinMaxFcstHr	Gets max/min of two forecast hours  	*
 *   pgsmear_fcstHr2Min		Converts forecast hour string to minute	*
 ***********************************************************************/

/*=====================================================================*/

void pgsmear_create ( Widget parent )
/************************************************************************
 * pgsmear_create							*
 *									*
 * This function creates the smear control window.			*
 *									*
 * void pgsmear_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * E. Safford/SAIC	10/04	add Smear All and associated features	*
 * B. Yin/SAIC          11/04   removed variables set but not used      *
 * J. Wu/SAIC           12/04   remove outlookLbl      			*
 * T. Piper/SAIC	09/05	made ii and nn long			*
 * J. Wu/SAIC           06/06   Change "Smear" to "Smear Tag"      	*
 * X. Guo/CWS		01/10   query which smear algorithm to be used  *
 ***********************************************************************/
{
    int		ier;
    long	ii, nn;
    char	*ctlstrs[] = {"Smear All", "Smear Tag", "Cancel"};
    Widget	pane, attrform, button;
    Widget	smearColorPb, outlkColorPb;
    XmString	xmstr;    
    char 	cval[5];
/*---------------------------------------------------------------------*/
/*
 *  Create main dialog window.
 */
    _smearForm = XmCreateFormDialog ( parent, "pgsmear_popup", NULL, 0 );
    xmstr = XmStringCreateLocalized("Smear Control");

    XtVaSetValues ( _smearForm,
		XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		NULL );

    XmStringFree(xmstr);

/*
 *  Create pane area and attribute form and label.
 */
    pane = XtVaCreateManagedWidget ( "smear_pane",
		xmPanedWindowWidgetClass, 	_smearForm,
		XmNsashWidth,			1,
		XmNsashHeight,	 		1,
		XmNleftAttachment,  		XmATTACH_FORM,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL );

    attrform = XtVaCreateManagedWidget ( "smear_attrform",
		xmFormWidgetClass,		pane,
		NULL );
    
    
              XtVaCreateManagedWidget ( "Smeared Figure Attributes:",
		xmLabelWidgetClass,		attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			5,
		NULL ); 

/*
 *   The smear and outlook colors are obtained from the setting table,
 *   so the color picker is not needed anymore. But this has not been 
 *   cleared by AWC, so I keep the code and just unmanaged the widget.
 */
    XtUnmanageChild( attrform );
    
/*
 *  Smear Color
 */ 
    XtVaCreateManagedWidget ( "Color:",
		xmLabelWidgetClass,		attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			35,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			20,
		NULL ); 

    smearColorPb = XtVaCreateManagedWidget ( "",
		xmPushButtonWidgetClass,	attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			30,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			200,
		XmNwidth,			30,
		XmNheight,			25,
		NULL );

    XtVaSetValues ( smearColorPb,
		XmNbackground,		NxmColrP_getColorPixel ( _smearColor ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _smearColor ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _smearColor ),
		NULL );


    XtAddCallback ( smearColorPb, XmNactivateCallback, NxmClrW_popup, 
		    &_smearColor );
        
/*
 *  Outlook Color
 */ 
    XtVaCreateManagedWidget ( "Outlook Color:",
		xmLabelWidgetClass,		attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			65,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			20,
		NULL ); 

    outlkColorPb = XtVaCreateManagedWidget ( "",
		xmPushButtonWidgetClass,	attrform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			60,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			200,
		XmNwidth,			30,
		XmNheight,			25,
		NULL );

    XtVaSetValues ( outlkColorPb,
		XmNbackground,		NxmColrP_getColorPixel ( _outlkColor ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _outlkColor ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _outlkColor ),
		NULL );

    XtAddCallback ( outlkColorPb, XmNactivateCallback, NxmClrW_popup, 
		    &_outlkColor );

/*
 *  Control buttons
 */
    nn = XtNumber ( ctlstrs );
    _cntlForm = XtVaCreateManagedWidget ( "smear_cntl_form",
		xmFormWidgetClass,		pane,
		XmNfractionBase,		nn * 100,
		NULL );

    for ( ii = 0; ii < nn; ii++ ) {
	button = XtVaCreateManagedWidget ( ctlstrs[ii], 
		xmPushButtonWidgetClass,	_cntlForm, 
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		((ii * 100) + 10 ),
		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		(((ii + 1) * 100) - 10),
		NULL );

	XtAddCallback ( button, XmNactivateCallback,
		(XtCallbackProc)pgsmear_ctlBtnCb, (XtPointer) ii );
    }    
    
/*
 * Query the value of _confirmEnabled.
 */
    ctb_pfbool( "SMEAR_CONFIRM_POPUP", &_confirmEnabled, &ier );
/*
 * Query gfa smear algorithm
 */
    ctb_pfstr ( "GFA_SMEAR_ALGORITHM", cval, &ier );
    if ( ier == 0 && strcasecmp ( cval, "2" ) == 0 ) {
        _smearUnionOnly = True;
    }

}

/*=====================================================================*/

static void pgsmear_popup ( void )
/************************************************************************
 * pgsmear_popup							*
 *									*
 * This function manages the smear control window.			*
 *									*
 * static void pgsmear_popup ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * D.W.Plummer/NCEP	02/07	Add 'set' of computational window	*
 ***********************************************************************/
{
    if ( !XtIsManaged ( _smearForm ) ) {

/*
 * Set computational projection window for smear calculations.
 */
        ncw_set ();
        ncw_sproj ( "PREFS" );

/*
 * disable things that will cause conflicts
 */
	auto_stopAutoUpdt();
	mbtnw_zoomSensitive(FALSE);
	mbtnw_loopSetSensitive(FALSE);
	loopw_sensitive(FALSE); 
	roamw_disableMenu();
	mbtnw_CtlBtnsSetSensitive ( FALSE );

	XtManageChild ( _smearForm );
    }        
}

/*=====================================================================*/

void pgsmear_popdown ( void )
/************************************************************************
 * pgsmear_popdown							*
 *									*
 * This function unmanages the smear control window.			*
 *									*
 * void pgsmear_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * D.W.Plummer/NCEP	02/07	Add 'unset' of computational window	*
 ***********************************************************************/
{
int             cur_lp;
/*---------------------------------------------------------------------*/

   if ( XtIsManaged ( _smearForm ) ) {

	NxmClrW_popdown();
	XtUnmanageChild ( _smearForm );

/*
 * re-enable things that may have caused conflicts
 */
	auto_startAutoUpdt();
	mbtnw_zoomSensitive(TRUE);
	mbtnw_loopSetSensitive(TRUE);
	loopw_sensitive(TRUE); 
	cur_lp = loop_getCurLoop();
	roamw_setup(cur_lp, TRUE);
	mbtnw_CtlBtnsSetSensitive ( TRUE );

/*
 * Unset computational projection window back to main canvas.
 */
        ncw_unset ();
    }
}

/*=====================================================================*/

Boolean pgsmear_isUp ( void )
/************************************************************************
 * pgsmear_isUp								*
 *									*
 * Query whether the smear control window is managed or not.		*
 *									*
 * Boolean pgsmear_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	pgsmear_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_smearForm) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgsmear_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgsmear_ctlBtnCb							*
 *									*
 * Callback function for the msear control buttons.			*
 *									*
 * static void pgsmear_ctlBtnCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04	initial coding				*
 * J. Wu/SAIC		10/05	allow time smear and rubberband smear	*
 * J. Wu/SAIC		06/06	Add "Smear Tag" & confirmation		*
 * D.W.Plummer/NCEP	01/07	Use computational coord for calculation	*
 * D.W.Plummer/NCEP	02/07	rm comp coord for calc from this fxn	*
 ***********************************************************************/
{
    int		ier;
    char	message[128];    			       
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case SMEAR_ALL:		/* Smear All */
	_state = SMEAR_ALL;
		 
	if ( _confirmEnabled ) {
	    strcpy ( message, "This will delete and regenerate ALL smears.\n\n");
	    strcat ( message, "Ok to continue?\n ");
	    NxmConfirm_show( mcanvw_getDrawingW(), message, 
			 (XtCallbackProc)pgsmear_confirmOkCb, 
                         &pgsmear_confirmCancelCb,
			 NULL, &ier);	
        }
	else {
	    pgsmear_confirmOkCb ( (Widget)NULL, (XtPointer)NULL, 
	                          (XtPointer)NULL );
	}
	
	mbotw_mouseSet( LMHINT_NOACTION, MMHINT_CANCEL );
	
	break;

      case SMEAR_TAG:		/* Smear Tag */
	
	_state = SMEAR_TAG;	
	mbotw_mouseSet( LMHINT_SELECT, MMHINT_CANCEL );
		
	break;
    
      case SMEAR_NONE:		/* Cancel */
 	_state = SMEAR_NONE;       
	pgsmear_exit ();
	
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsmear_selectElmEh ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr ) 
/************************************************************************
 * pgsmear_selectElmEh                                                 	*
 *                                                                      *
 * This function selects elements for a "Smear Tag" action.             *
 *                                                			*
 * void pgsmear_selectElmEh ( w, clnt, event, ctdr )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid                       Widget                                *
 *      clnt             XtPointer                               	*
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/03	initial coding				*
 * J. Wu/SAIC		07/04	revise to add GUI control		*
 * J. Wu/SAIC		06/06	revise for "Smear Tag"			*
 ***********************************************************************/
{
   int		ier;
   char		message[128];
/*---------------------------------------------------------------------*/
   
    if ( event->xbutton.button == Button1 ) {
    
/*
 *   Actions:
 *       1)  The user clicks L on/near one of the GFA elements.
 *           Pop up the confirmation window with GFA's hazard type 
 *           and tag.
 *
 *       2)  The user clicks M, exit smear function.
 */
        if ( _state == SMEAR_TAG ) {
	    pgsmear_selectSmearEl( event, &ier );
       
            if ( ier == 0 ) {
                _numElSelected++;
            
	        if ( _confirmEnabled ) {
		    strcpy ( message, "This will smear ALL snapshots\n");
	            strcat ( message, "with hazard type ");
	            strcat ( message, _smearHazardTyp );
	            strcat ( message, " and tag " );
	            strcat ( message, _smearTag );
	            strcat ( message, ".\n\n" );	    
	            strcat ( message, "Ok to continue?\n ");	    
	    
	            NxmConfirm_show( mcanvw_getDrawingW(), message, 
			 (XtCallbackProc)pgsmear_confirmOkCb, 
                         &pgsmear_confirmCancelCb, NULL, &ier);	     
	            
		    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
		}
		else {
	            pgsmear_confirmOkCb ( (Widget)NULL, (XtPointer)NULL, 
	                                  (XtPointer)NULL );		
		}
            }
        }
    }    
    else if ( event->xbutton.button == Button2 ) {               
	pgsmear_exit ();	/* exit smear */
    }      
}

/*=====================================================================*/

void pgsmear_startSmear ( void )
/************************************************************************
 * pgsmear_startSmear                                                  	*
 *                                                                      *
 * This function does all the setup necessary to begin a smear.         * 
 *									*
 * void pgsmear_startSmear ( )         					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *    		`	None		            		 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/03	initial coding				*
 * E. Safford/SAIC	12/03	add mouse hint for correct state1 setup *
 * J. Wu/SAIC		03/04	add _smearSubTyp			*
 * J. Wu/SAIC		07/04	add GUI control 			*
 * E. Safford/SAIC	11/04	remove pgsmear_updateColor		*
 * E. Safford/SAIC	12/04   _smearHazardTyp is now a string		*	
 * J. Wu/SAIC		12/04	add _smearTag & _smearUpdtNum		*
 * E. Safford/SAIC	07/05	rm init of _smearUpdtNum		*
 * B. Yin/SAIC		11/05	rm GFA subtype				*
 * J. Wu/SAIC		06/06	revise for "Smear Tag" 			*
 ***********************************************************************/
{
   _smearHazardTyp[0] = '\0'; 
   _smearTag[0] = '\0'; 
   _numElSelected = 0;
   pgsmear_popup ();
   _state = SMEAR_NONE;
   mbotw_mouseSet( LMHINT_NOACTION, MMHINT_CANCEL );
}

/*=====================================================================*/

static void pgsmear_multiSmear( int numEls, VG_DBStruct **el_in, 
				Boolean	timeSmear, Boolean unionOnly,
				int *numPts, float xPts[], 
				float yPts[], char *fcstHr, 
				char *snapshotHrs,
				char *top, char *bot, 
				char *topFzl, char *botFzl,
				char *worstFreq, char *worstSev, 
				char *worstIssue, char *IFRType,
                                char *MtnObscType, char *sigmetRef,
				int *iret )
/************************************************************************
 * pgsmear_multiSmear                                                   *
 *                                                                      *
 * Smear N figures.  This will do either a rubberband smear or time	*
 * smear.  For rubberband smear, we smear figure 1 & 2 to get figure	*
 * 12, 	then smear figure 12 and figure 3  to get figure 123, and	*
 * so on.  For time smear, we smear figures 1 & 2, 2 & 3, etc, then	*
 * union the results of all the smears.					*
 *                                                                      *
 * Space is allocated for the output params *xPts and *yPts.  Calling	*
 * routine must free this up.						*
 *									*
 * static void pgsmear_multiSmear ( numIn, el_in, timeSmear,		*
 *			     	numPts, xPts, yPts, fcstHr,		*
 *			     	top, bot, topFzl, botFzl, worstFreq, 	*
 *				worstSev, worstIssue, sigmetRef, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	numEls			int	number of elements in 		*
 * 	**el_in			VG_DBStruct array of GFA snapshot (elms)*	
 *	timeSmear		Boolean	flag for time/rubberband smear	*
 *	unionOnly		Boolean	union instead of shrink	wrap	*
 * Output parameters:                                                   *
 *	*numPts			int	number of points in smear fig.  *
 *	*xPts			float	array of x coordinates in smear	*
 *	*yPts			float 	array of y coordinates in smear	*
 *	*fcstHr			char 	forcast hour for smeared GFA	*
 *      *snapshotHrs		char	';' delimited snapshot hrs	* 
 *	*top			char 	top for smeared GFA		*
 *	*bot			char 	bottom for smeared GFA		*
 *	*topFzl			char 	top of FZL for smeared GFA	*
 *	*botFzl			char	bottom of FZL for smeared GFA	*
 *	*worstFreq		char	worst case frequency		*
 *	*worstSev		char	worst case severity		*
 *	*worstIssue		char	worst case issue type		*
 *      *IFRType		char	cumulative IFR Type 		*
 *      *MtnObscType            char    cumulative Mtn Obscuration Type *
 *	*sigmetRef		char	sigmet reference string		*
 *	*iret			int	return code			*
 *					  0 = normal			*
 *					 -1 = unable to get el pts	* 
 *					 -2 = error on smear		*
 *					 -3 = less than 2 elements      *
 *					        selected		*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/03	initial coding				*
 * E. Safford/SAIC	12/03	add checks for bad ier values from cgr	*
 * E. Safford/SAIC	04/04	Combine forecast times for GFA_ELM	*
 * J. Wu/SAIC		07/04	Combine top/bottom/description for GFA	*
 * E. Safford/SAIC	08/04	change top and bot to char*		*
 * J. Wu/SAIC		09/04	remove check for GFA_GFA		*
 * B. Yin/SAIC		10/04	changed the smear algorithm		*
 * E. Safford/SAIC	10/04	add numEls & listEls[] params		*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_getFld()	*
 * B. Yin/SAIC          11/04   Removed gfa description field           *
 * E. Safford/SAIC	11/04	add snapshotHrs param & worst-case code *
 * H. Zeng/SAIC		12/04	used cgr_ordrccw()			*
 * E. Safford/SAIC	01/05	add sigmetRef param, use _combineSigStrs*
 * B. Yin/SAIC		06/05	call pgsmear_getWorstFL to get worst FLs*
 * H. Zeng/SAIC		09/05	rm call to pggfaw_combineSigStrs	*
 * J. Wu/SAIC		10/05	replace pggfaw_cmpSeverity with ctb_gfa**
 * J. Wu/SAIC		10/05	allow time smear and rubberband smear	*
 * B. Yin/SAIC		04/06	add topFzl and botFzl parm		*
 * E. Safford/SAIC	04/06	add worstIssue param			*
 * E. Safford/SAIC	04/06	add IFRType param			*
 * J. Wu/SAIC		05/06	add call to pgsmear_getMinMaxFcstHr	*
 * E. Safford/SAIC	05/06	fix fzlTop/Bot calculation bug		*
 * D.W.Plummer/NCEP	06/06	revert back to ORIGINAL smear algorithm	*
 * B. Yin/SAIC		07/06	remove call to ctb_gfaGetWorstIssTyp	*
 * J. Wu/SAIC		09/06	add call to ctb_gfaWorstFL()		*
 * B. Yin/SAIC		10/06	change input elements to pointers	*
 * J. Wu/SAIC		09/07	add "unionOnly" for "Join" action	*
 * B. Yin/SAIC		10/07	call cgr_polyint to check if two 	*
 *				polygons intersect.                     *
 * L. Hinson/AWC        09/09   add MtnObscType paramter  	        *
 *                              add call to ctb_gfapCombineMtnObscTypes *
 ***********************************************************************/
{
   int		maxTmpPts   = MAXPTS;
   int 		maxUnionPts = MAXPTS;
   int    	numMapPts   = 0;

   int	  	numResults, numTmp, numUnion, pts0, pts1;
   int		ii, jj, ier, intersect;

   float	*x0       = NULL, *y0       = NULL; 
   float	*x1       = NULL, *y1       = NULL;
   float  	*xResults = NULL, *yResults = NULL;
   float	*xTmp     = NULL, *yTmp     = NULL;
   float	*xUnion   = NULL, *yUnion   = NULL;
   float	*xMap1    = NULL, *yMap1    = NULL;
   float	*xMap2    = NULL, *yMap2    = NULL;   
   
   char		worstTop[32], worstBottom[32];
   char		worstBottomFZL[32], worstTopFZL[32];
   char		fzlTop1[32];
   char		fzlBottom1[32];
   char		value[32], hazard[32];
   char		freq0[32], freq1[32];
   char		sev0[32],  sev1[32];
   char		type0[ 265 ], type1[ 256 ];
   char		minFcsthr[5], maxFcsthr[5], tmpstr1[5], tmpstr2[5];

   Boolean	isGfaElm = False, isIFR = False, isMtnObsc = False;
   Boolean      shrinkWrap = False;
   VG_DBStruct	*elPtr0, *elPtr1;
/*---------------------------------------------------------------------*/

   *numPts        = 0;
   *iret          = 0;
   fcstHr[0]      = '\0';
   top[0]         = '\0';
   bot[0]         = '\0'; 
   snapshotHrs[0] = '\0';
   worstFreq[0]   = '\0';
   worstSev[0]    = '\0';
   topFzl[0]	  = '\0';
   botFzl[0]	  = '\0';
   worstIssue[0]  = '\0';
   IFRType[0]     = '\0';
   MtnObscType[0] = '\0';
   minFcsthr[0]	  = '\0';
   maxFcsthr[0]	  = '\0';

/*
 *   Need at least 2 figures selected to do a smear.  This check
 *   is just a backup sanity check -- calling routine should have
 *   already screened out single element "smears".
 */
   if ( numEls < 2 ) {
      *iret = -3;
      return;
   }

/* 
 *  load the points from element 0 into the el0 arrays.
 */
   elPtr0 = el_in[0];

   pts0 = cvg_gtnumpts( elPtr0, &ier );
   if ( ier < 0 ) {
      *iret = -1;
      return;
   }

   x0 = ( float * )malloc( pts0 * sizeof(float) );
   y0 = ( float * )malloc( pts0 * sizeof(float) );

   cvg_todev( elPtr0, &pts0, x0, y0, &ier );  
   if ( ier < 0 ) {
      *iret = -1;
      if ( x0 ) free( x0 );
      if ( y0 )	free( y0 );
      return;
   }

/*
 *  Retrieve forecst hours for GFA elements.
 */
   if ( elPtr0->hdr.vg_type == GFA_ELM ) {
       
       isGfaElm = True;
       
/*
 *  Several fields of the resulting smear are to be "worst-case".
 *  The top and bottom flight levels are to be maximum and minimum,
 *  respectively, and the Frequency and Severity fields are to be
 *  worst case as well.  Here we load the intial values for these
 *  fields, by taking the values from first element.  We'll then
 *  compare these values to all the other elements that are part 
 *  of the smear and take the worst cases of each for the smear.
 */
       cvg_getFld ( elPtr0, TAG_GFA_TOP, top, &ier );
       cvg_getFld ( elPtr0, TAG_GFA_BOTTOM, bot, &ier );

       cvg_getFld ( elPtr0, TAG_GFA_FZL_TOP, topFzl, &ier );
       cvg_getFld ( elPtr0, TAG_GFA_FZL_BOTTOM, botFzl, &ier );

       cvg_getFld ( elPtr0, TAG_GFA_AREATYPE, value, &ier );
       strcpy( hazard, value );

       if( strcasecmp( hazard, "IFR" ) == 0 ) {
	   isIFR = True;
           cvg_getFld( elPtr0, "<Type>", IFRType, &ier );
       }
       if( strcasecmp( hazard, "MT_OBSC" ) == 0 ) {
         isMtnObsc = True;
         cvg_getFld( elPtr0, "<Type>", MtnObscType, &ier );
       }

       cvg_getFld ( elPtr0, "<Frequency>", worstFreq, &ier );      
       cvg_getFld ( elPtr0, "<Severity>", worstSev, &ier );      
       cvg_getFld ( elPtr0, TAG_GFA_STATUS, worstIssue, &ier ); 
   

       cvg_getFld ( elPtr0, TAG_GFA_FCSTHR, value, &ier );
       if ( strlen ( value ) > (size_t)0 ) {
	   sprintf( snapshotHrs, "%s;", value );           
	   pgsmear_getMinMaxFcstHr ( value, value, minFcsthr, 
	                             maxFcsthr, &ier );
       }           
   }

/*
 *  Loop over all remaining selected elements
 */
   for ( ii=1; ii < numEls; ii++ ) {
      
/*
 *  Read the next selected element
 */
      elPtr1 = el_in[ii];

      pts1 = cvg_gtnumpts( elPtr1, &ier );
      if ( ier < 0 ) {
         *iret = -1;
         break;
      }

/*
 *  allocate space and load the coordinate points
 */
      x1 = ( float * )malloc( pts1 * sizeof(float) );
      y1 = ( float * )malloc( pts1 * sizeof(float) );
      cvg_todev( elPtr1, &pts1, x1, y1, &ier );  
      if ( ier < 0 ) {
	 *iret = -1;
         break;
      }

/*
 *  Reorder the coordinate arrays
 */
      cgr_ordrccw ( pts0, x0, y0, &ier );
      if ( ier < 0 ) {
	 *iret = -2;
         break;
      }
      cgr_ordrccw ( pts1, x1, y1, &ier );
      if ( ier < 0 ) {
	 *iret = -2;
         break;
      }

/*
 *  Retrieve forecst hours for GFA elements.
 */
      if ( isGfaElm ) {
  
/*
 *  Compare each snapshot's values to the worst-case.
 */  
         cvg_getFld ( elPtr1, TAG_GFA_TOP, value, &ier );

         cvg_getFld ( elPtr1, TAG_GFA_FZL_TOP, fzlTop1, &ier );
         cvg_getFld ( elPtr1, TAG_GFA_FZL_BOTTOM, fzlBottom1, &ier );

	 ctb_gfaWorstFL ( TOP, top, botFzl, topFzl, value, 
	 		      fzlBottom1, fzlTop1, worstTop, 
			      worstBottomFZL, worstTopFZL, &ier );
	 if ( ier == 0 ) {
	    strcpy ( top, worstTop );
	 }

         cvg_getFld ( elPtr1, TAG_GFA_BOTTOM, value, &ier );

	 ctb_gfaWorstFL ( BOTTOM, bot, botFzl, topFzl, value, 
	 		      fzlBottom1, fzlTop1, worstBottom, 
			      worstBottomFZL, worstTopFZL, &ier );
	 if ( ier == 0 ) {
	    strcpy ( bot, worstBottom );
	    strcpy ( topFzl, worstTopFZL );
	    strcpy ( botFzl, worstBottomFZL );
	 }


	 if( strlen( worstFreq ) > (size_t)0 ) {
	     strcpy( freq0, worstFreq );
             cvg_getFld ( elPtr1, "<Frequency>", freq1, &ier );      
             ctb_gfaCmpSeverity( hazard, "Frequency", 
	     				freq0, freq1, worstFreq, &ier ); 
         }

	 if( strlen( worstSev ) > (size_t)0 ) {
	     strcpy( sev0, worstSev );
             cvg_getFld ( elPtr1, "<Severity>", sev1, &ier );      
             ctb_gfaCmpSeverity( hazard, "Severity", 
	     				sev0, sev1, worstSev, &ier ); 
         }

	 if( isIFR ) {
	     cvg_getFld( elPtr1, "<Type>", type1, &ier );
	     strcpy( type0, IFRType );
	     ctb_gfaCombineIFRTypes( type0, type1, IFRType, &ier );
         }
         
         if( isMtnObsc ) {
             cvg_getFld( elPtr1, "<Type>", type1, &ier );
             strcpy( type0, MtnObscType );
             ctb_gfapCombineMtnObscTypes( type0, type1, MtnObscType, &ier );
         }

         cvg_getFld ( elPtr1, TAG_GFA_FCSTHR, value, &ier );
	 if ( strlen ( value ) > (size_t)0 ) {
	     
	     strcpy ( tmpstr1, minFcsthr );         
	     pgsmear_getMinMaxFcstHr ( value, tmpstr1, minFcsthr, 
	                               tmpstr2, &ier );
	     
	     strcpy ( tmpstr1, maxFcsthr );
	     pgsmear_getMinMaxFcstHr ( value, tmpstr1, tmpstr2, 
	                               maxFcsthr, &ier );

/*
 *  update or add the fcsthr to the snapshotHrs string
 */
             if( strlen( snapshotHrs ) > (size_t)0 ) {
                 strcat( snapshotHrs, value );
                 strcat( snapshotHrs, ";" );
             }
             else {
		 sprintf( snapshotHrs, "%s", value );
             }
	 }	 	 
         sigmetRef[0] = '\0';
      }
      
/*
 *  Smear the two element points.  If unionOnly is True and two polygons
 *  intersect, a union will be performed.  Otherwise, "shrink-wrap" is
 *  used.  Note that if two polygons do not touch each other,  the union
 *  returns the same input polygon that has less points.
 */
      xTmp = ( float * )malloc( maxTmpPts * sizeof(float) );
      yTmp = ( float * )malloc( maxTmpPts * sizeof(float) );

      cgr_init( &ier );
      
      shrinkWrap = False;
      if ( unionOnly ) {
          cgr_polyunion( &pts0, x0, y0, &pts1, x1, y1,
	 		 &maxTmpPts, &numTmp, xTmp, yTmp, &ier ); 

/*
 *  If two polygons do not intersect, use shrink wrap.
 */
	  cgr_polyint ( sys_D, &pts0, x0, y0, sys_D, &pts1, x1, y1,
                 &intersect, &ier );

          if ( ier < 0 || intersect == 0 ) {
              shrinkWrap = True;              
	  }
      }
      else {
          shrinkWrap = True;
      }
      
      if ( shrinkWrap ) {
          cgr_polysmear( "ORIGINAL", &pts0, x0, y0, &pts1, x1, y1, 
      		     &numMapPts, xMap1, yMap1, xMap2, yMap2, 
		     &maxTmpPts, &numTmp, xTmp, yTmp, &ier ); 
      }
      
      if ( ier < 0 ) {
	 *iret = -2;
         break;
      }
      cgr_done( &ier );

      cgr_ordrccw ( numTmp, xTmp, yTmp, &ier );
      if ( ier < 0 ) {
	 *iret = -2;
         break;
      }

/*
 *  If this is the 1st time through, copy the xTmp and yTmp into
 *  the Results arrays.  After the first time though we need to 
 *  union the Tmp figure with the Results figure.
 */
      if ( ii == 1 || !timeSmear ) { /* first time through or rubberband*/
         numResults = numTmp;

/*
 *  resize the results arrays and copy the tmp values into them
 */
         xResults = ( float * )malloc( numResults * sizeof(float) );
         yResults = ( float * )malloc( numResults * sizeof(float) );

         for (jj=0; jj<numResults;  jj++ ) {
            xResults[jj] = xTmp[jj];
            yResults[jj] = yTmp[jj];
         }
      }
      else {					/* union Tmp and Results */
         cgr_init( &ier );

/*
 *  do a union operation to combine Tmp and Results into Union
 */  
         xUnion = ( float * )malloc( maxUnionPts * sizeof(float) );
         yUnion = ( float * )malloc( maxUnionPts * sizeof(float) );

         cgr_init( &ier );
         cgr_polyunion( &numTmp, xTmp, yTmp, &numResults, xResults, yResults,
	 		&maxUnionPts, &numUnion, xUnion, yUnion, &ier ); 
         if ( ier < 0 ) {
	    *iret = -2;
            break;
         }
         cgr_done( &ier );

/*
 *  Redimension Result and copy Union into Result
 */  
         numResults = numUnion;
	 xResults   = ( float * )realloc( xResults, numResults * sizeof(float) );
	 yResults   = ( float * )realloc( yResults, numResults * sizeof(float) );

         for (jj=0; jj<numResults;  jj++ ) {
            xResults[jj] = xUnion[jj];
            yResults[jj] = yUnion[jj];
         }
         cgr_ordrccw ( numResults, xResults, yResults, &ier );
         if ( ier < 0 ) {
	    *iret = -2;
            break;
         }

/*
 *  Free Union
 */
	 if ( xUnion ) free( xUnion );
	 if ( yUnion ) free( yUnion );
	 numUnion = 0;
      }

/*
 *   Free Tmp 
 */
      if ( xTmp ) free( xTmp );
      if ( yTmp ) free( yTmp );
      numTmp = 0;

/*
 *  For time smear: Move x1, y1 -> x0, y0, free x1, y1
 *  For rubberband smear: Move the result -> x0, y0, free x1, y1
 */
      if ( ii < ( numEls - 1 ) ) {
          
	  if ( timeSmear ) {
	      pts0 = pts1;
	      x0   = ( float * )realloc( x0, pts0 * sizeof(float) );
	      y0   = ( float * )realloc( y0, pts0 * sizeof(float) );

              for (jj=0; jj<pts0;  jj++ ) {
                  x0[jj] = x1[jj];
                  y0[jj] = y1[jj];
              }	   
	  }
	  else { 
	      pts0 = numResults;
	      x0   = ( float * )realloc( x0, pts0 * sizeof(float) );
	      y0   = ( float * )realloc( y0, pts0 * sizeof(float) );

              for (jj=0; jj<pts0;  jj++ ) {
                  x0[jj] = xResults[jj];
                  y0[jj] = yResults[jj];
              }	 
	  }
	  
	  if ( x1 ) free( x1 );
	  if ( y1 ) free( y1 );
      }


   }  /* end for loop */


   if ( *iret >= 0 ) {

/*
 *  Finish Up -- allocate space for xPts and yPts
 *  and load the results into them.  Then free the results.
 */
      *numPts = numResults; 

      for ( ii=0; ii < *numPts; ii++ ) {
         xPts[ii] = xResults[ii];   
         yPts[ii] = yResults[ii];   
      }
      
/*
 *  Build forecast hour string for GFA element.
 */
      if ( isGfaElm ) {         
          sprintf ( fcstHr, "%s-%s", minFcsthr, maxFcsthr );
      }
   }  

   if ( xResults ) free( xResults ); 
   if ( yResults ) free( yResults );
   if ( x0       ) free( x0 );       
   if ( y0       ) free( y0 );
   if ( x1       ) free( x1 );
   if ( y1       ) free( y1 );
}

/*=====================================================================*/

static void pgsmear_selectSmearEl( XEvent *event, int *iret )
/************************************************************************
 * pgsmear_selectSmearEl                                                *
 *                                                                      *
 * Given a mouse click event, determine if it is near an element that   *
 * can be used in a smear function.					*
 *                                                                      *
 * void pgsmear_selectSmearEl ( event, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *event                  XEvent                                  *
 *                                                                      *
 * Output parameters:                                                   *
 *	&iret			int	return code			*
 *					  0 = element selected		*
 *					 -1 = no element selected	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/03	initial coding				*
 * E. Safford/SAIC	12/03	add check, post scan for correct el type*
 * J. Wu/SAIC		02/04	add GFA_ELM				*
 * J. Wu/SAIC		03/04	smear GFAs only with the same subtype	*
 * E. Safford/SAIC	04/04	correct two selection bugs		*
 * J. Wu/SAIC		07/04	smear GFAs with same subtype & area type*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_getFld()	*
 * E. Safford/SAIC	12/04	_smearHazardTyp is now string		*
 * J. Wu/SAIC		12/04	add _smearTag & _smearUpdtNum		*
 * E. Safford/SAIC	07/05	rm _smearUpdtNum              		*
 * B. Yin/SAIC		11/05	rm the check for GFA subtype		*
 * J. Wu/SAIC		06/06	Limit the selection to GFA ONLY		*
 ***********************************************************************/
{
   char		value[32];

   int		curLayer, class, ier, nearest, numSelected; 
   int		offsetToElem, xoff, yoff;
   float	xx,   yy;

   VG_DBStruct 	el;
/*---------------------------------------------------------------------*/

   ncw_unset ();
   
   *iret = -1;

   numSelected = pghdlb_elemSelected();
   if ( numSelected > 0 ) {
       ncw_set ();
       ncw_sproj ( "PREFS" );
       return;
   }
   
   curLayer    = pglayer_getCurLayer();
   
   xgtoff( &xoff, &yoff, &ier );
   xx = (float) ( event->xbutton.x + xoff );
   yy = (float) ( event->xbutton.y + yoff );

/*
 * Get the vg_class from the palette and make sure it's an allowable 
 * class for smear.  Put the class into the el for masking the cvg_scan.
 */
   class   = pgpalw_getCurClassId();
   if ( (class != CLASS_ANY) && (class != CLASS_MET) ) {
       ncw_set ();
       ncw_sproj ( "PREFS" );
       return;
   }
   
   el.hdr.vg_class = class; 

   cvg_scan( NULL, curLayer, (char) class, xx, yy, 0, 
      		&el, &offsetToElem, &nearest, &ier );

/*
 *  Kame sure it is of VG type GFA_ELM
 */
   if ( ier >= 0 && el.hdr.vg_type == _smearVgType ) {
	    
      pgactv_setActvElm( &el, offsetToElem );
      pghdlb_select( &el, offsetToElem );
         
      cvg_getFld ( &el, TAG_GFA_AREATYPE, value, &ier );	    
      strcpy( _smearHazardTyp, value );
	    
      cvg_getFld ( &el, TAG_GFA_TAG, value, &ier );	    
      strcpy( _smearTag, value );
      
 /*
  * Free GFA block memory
  */
      if ( el.hdr.vg_type == GFA_ELM ) {
          cvg_freeElPtr ( &el );
      }
      *iret = 0;
   }
   ncw_set ();
}

/*=====================================================================*/

void pgsmear_smear ( int		numSnapShots,
			   int 		snapShots[], 
			   Boolean 	makeSmearElm,
			   int		color,
			   int		numIn,
			   VG_DBStruct	*el_in,
			   Boolean 	timeSmear,
			   Boolean 	skipFzlvl,
			   Boolean 	useAllSnapShots,
			   Boolean 	reducePts,
			   Boolean 	unionOnly,
			   VG_DBStruct	*el,
			   int		*numPts,
			   float	xPts[],
			   float	yPts[])
/************************************************************************
 * pgsmear_smear	                                                *
 *                                                                      *
 * Perform the smear operation on the list of snapShots (snap shot   	*
 * file locations or an array of GFA elements ).			*
 *                                                                      *
 * void pgsmear_smear ( numSnapShots, snapShots, makeSmearElm, color, 	*
 *                      numIn, el_in, timeSmear, skipFzlvl, 		*
 *			useAllSnapShots, reducePts, el, 		*
 *			numPts, xPts, yPts )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	numSnapShots	int		number of snap shots file locs	*
 * 	snapShots 	int		array of snapshot file positions*	
 *	makeSmearElm	Boolean		flag to snap and display	*
 *	color		int		color of smear			*
 *	numIn		int		number of GFA snapshots	(elms)	*
 * 	el_in 		int		array of GFA snapshot (elms)	*	
 *	timeSmear	Boolean		flag for time/rubberband smear	*
 *	skipFzlvl	Boolean		flag to skip FZLVLs		*
 *	useAllSnapShots	Boolean		flag for using all snapshots	*
 *	reducePts	Boolean		flag for point reduction	*
 *	unionOnly	Boolean		union instead of shrink	wrap	*
 *									*
 * Output parameters:                                                   *
 *	*el		VG_DBStruct	resulting smear element		*
 *	*numPts		int		number of points in xPts/yPts	*
 *	xPts[]		float		array of x coords for smear el	*
 *	yPts[]		float		array of y coords for smear el	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/03	initial coding				*
 * E. Safford/SAIC	12/03	add err reporting, avoid undo_step() on *
 *				error					*
 * J. Wu/SAIC		03/04	correct parameter in er_wmsg() calling	*
 * J. Wu/SAIC		04/04	Combine forecast hours for GFA_ELM	*
 * E. Safford/SAIC	04/04	Position GFA_ELM text in center of smear*
 * E. Safford/SAIC	04/04	make sure smear elem isn't grouped      *
 * J. Wu/SAIC		07/04	combine GFA's color/top/bottom/descript	*
 * E. Safford/SAIC	08/04	make gfaTop and gfaBottom chars		*
 * J. Wu/SAIC		09/04	remove check for GFA_GFA		*
 * J. Wu/SAIC		09/04	Reduce points for smeared GFA elements	* 
 * E. Safford/SAIC	10/04	update for param change to multiSmear() *
 *				  and add params and generalize		*
 * J. Wu/SAIC           10/04   access GFA attr using cvg_get/setFld()  *
 * E. Safford/SAIC	11/04	set TAG_GFA_CATEGORY for smear		*
 * B. Yin/SAIC          11/04   Removed gfa description field           *
 * E. Safford/SAIC	11/04	update params for pgsmear_multiSmear	*
 * E. Safford/SAIC	12/04	_smearHazardTyp is now string		*
 * E. Safford/SAIC	01/05	add param for pgsmear_multiSmear	*
 * B. Yin/SAIC		01/05	add a call to snap			*
 * B. Yin/SAIC		06/05	change GFA_SMEAR to GFA_SYSTEM_SMEAR	*
 * J. Wu/SAIC		10/05	allow time smear and rubberband smear	*
 * B. Yin/SAIC		11/05	change GFA category to GFA subtype	*
 * J. Wu/SAIC		12/05	reduce points for single elm smear	*
 * B. Yin/SAIC		01/06	added a call to get GFA line width	*
 * J. Wu/SAIC		01/06	use size check option in cgr_reducePts	*
 * B. Yin/SAIC		04/06	Skip FZLVL				*
 * B. Yin/SAIC		04/06	add top and bottom of FZL		*
 * E. Safford/SAIC	04/06	get worst issue type from all snapshots	*
 * B. Yin/SAIC		07/06	add call to pgsmear_getWorstIssue	*
 * B. Yin/SAIC		07/06	change subtype (hazard and cat types) 	*
 * J. Wu/SAIC		09/06	add call to ctb_gfaWorstIssue		*
 * J. Wu/SAIC		10/06	modify cgr_reducePts			*
 * B. Yin/SAIC		10/06	remove canceled snapshots	 	*
 * J. Wu/SAIC		10/06	default reducepct & reduceDst to 0	*
 * B. Yin/SAIC		11/06	fix worst issue bug		 	*
 * J. Wu/SAIC		11/06	add an option to turn on/off point 	*
 *				reduction				*
 * J. Wu/SAIC		12/06	add "SMEAR_RED_INC_PCT" to pt reduction	*
 * E. Safford/SAIC	12/06	param change for af_useSnapshots	*
 * E. Safford/SAIC	12/06	add coord_sys to reduceopt string	*
 * J. Wu/SAIC		01/07	use float input for cgr_reducePts 	*
 * J. Wu/SAIC		06/07	add more options to be used in "CONNECT"*
 * J. Wu/SAIC		09/07	add parameter "unionOnly" for "Join"	*
 * J. Wu/SAIC		10/07	Position text box arrow	at the center	*
 * L. Hinson/AWC        09/09   Add Mtn Obscuration parameter           *
*************************************************************************/
{
   int		ii;
   int		ier            = 0;
   int		errorCode      = 0;
   int		tmpPts         = 0;
   int		newPts         = 0;
   int          onePt          = 1;
   int		nNoCancel      = 0, nCancel = 0;
   int		smearFlag      = 0;
   int		ptsReduce      = G_TRUE;
   char         gfaTop[4]      = "";
   char         gfaBottom[4]   = "";
   char		fzlTop[4]      = "";
   char		fzlBottom[4]   = "";
   char		sigmetRef[ MAX_SIG_REF ];

   char		fcstHr[10], cval[5], value[32], category[32];
   char		snapshotHrs[256];
   char		worstFreq[32], worstSev[32], worstIssue[ 32 ];
   char		tmpIssue[ 32 ];
   char		IFRType[ 256 ], MtnObscType[ 256 ], hazard[ 32 ];
   char		**issues;
   char		reduceopt[256], tmpopt[64];
   
   float	xtmp[MAXPTS], ytmp[MAXPTS];
   float	xNew[MAXPTS], yNew[MAXPTS];
   float	xCent = 0.0F, yCent = 0.0F, area = 0.0F;
   float	lat, lon, tval;
   float	reducePct      = 0.0F;
   float	reduceDst      = 0.0F;
   
   VG_DBStruct	*tmpEl, **elPtr, **elNoCancel, **elCancel;
/*---------------------------------------------------------------------*/
/*
 *  sanity check; we should have at least 1 element 
 *  selected and confirmed by this point.
 */
   if ( numSnapShots <= 0 && numIn <= 0 ) {
      return;
   }

/*
 *  Load the first selected element and use it as a template
 *  for the resulting smear element.  Everything about the
 *  new element is the same as the first element except the
 *  points.
 */
   if ( numIn <= 0 ) {
       G_MALLOC ( tmpEl, VG_DBStruct, numSnapShots, "pgsmear_smear: tmpEl" );
       for ( ii = 0; ii < numSnapShots; ii++ ) {
           cvg_rdrec( cvg_getworkfile(), snapShots[ii], &tmpEl[ii], &ier ); 
       }
   }
   else {
       tmpEl = &el_in[0];
       numSnapShots = numIn;
   }
   
   hazard[ 0] = '\0';
      
/*
 *  Skip FZLVL
 */
   if ( tmpEl[ 0 ].hdr.vg_type == GFA_ELM && skipFzlvl ) {

      cvg_getFld ( &tmpEl[ 0 ], TAG_GFA_AREATYPE, hazard, &ier );

      if ( strcasecmp( hazard, "FZLVL" ) == 0 ) {

         if ( numIn <= 0 ) {

            for( ii = 0; ii < numSnapShots; ii++ ) {

               cvg_freeElPtr( &tmpEl[ii] );
            }
   
            G_FREE ( tmpEl, VG_DBStruct );
         }
	 return;
      }
   }
   
   smearFlag = GFA_SYSTEM_SMEAR;
                
/*
 *  Get the worst issue type.
 */
   issues = (char **) malloc ( numSnapShots * sizeof( char * ) );

   for ( ii = 0;  ii < numSnapShots; ii++ ) {

       issues[ ii ] = (char (*) ) malloc ( sizeof( char ) * 32 );
       cvg_getFld ( &tmpEl[ ii ], TAG_GFA_STATUS, issues[ ii ], &ier );

       cvg_getFld ( &tmpEl[ ii ], TAG_GFA_FCSTHR, value, &ier );
       if ( atoi( value ) > 6 ) smearFlag = GFA_SYSTEM_OUTLOOK;

   }
		
   ctb_gfaWorstIssue ( numSnapShots, issues, worstIssue, &ier );
      
   for ( ii = 0;  ii < numSnapShots; ii++ ) {

       free ( issues[ ii ] );

   }
      
   free ( issues );
      
/*
 *  Build an array of element pointers needed by af_useSnapshots().
 */ 
   G_MALLOC( elPtr, VG_DBStruct*, numSnapShots, "pgsmear_smear: elPtr" );

   for ( ii = 0; ii < numSnapShots; ii++ ) {
  
       elPtr[ ii ] = &tmpEl[ ii ];

   }
   
/*
 *  Remove canceled snapshots - if required.
 */
   if ( useAllSnapShots ) {
       nNoCancel = numSnapShots;
       elNoCancel = elPtr;
   }
   else {
       af_useSnapshots( elPtr, numSnapShots, smearFlag, &elNoCancel,
      		    &nNoCancel, &elCancel, &nCancel, &ier );
   }

   el->elem.gfa.info.nblocks = elNoCancel[0]->elem.gfa.info.nblocks;

   G_MALLOC ( el->elem.gfa.info.blockPtr[ 0 ],
	      gfaBlock_t, elNoCancel[0]->elem.gfa.info.nblocks, 
	      "pgsmear_smear: el->blockPtr" );

   memcpy ( &(el->hdr), &(elNoCancel[0]->hdr), sizeof( VG_HdrStruct ) );

   memcpy ( el->elem.gfa.info.blockPtr[ 0 ], 
	    elNoCancel[0]->elem.gfa.info.blockPtr[ 0 ], 
	    elNoCancel[0]->elem.gfa.info.nblocks * STD_STRLEN * sizeof ( char ) );

   el->elem.gfa.info.npts= elNoCancel[0]->elem.gfa.info.npts;
   memcpy ( &(el->elem.gfa.latlon), &(elNoCancel[0]->elem.gfa.latlon), 
            elNoCancel[0]->elem.gfa.info.npts * 2 * sizeof( float ) );
   
/*
 *  Use the user's color pick. 
 */
   el->hdr.maj_col = color;
   el->hdr.min_col = color;
          
   if ( nNoCancel > 1 ) {

/*
 * smear the selected figures and create the new element
 */
      pgsmear_multiSmear( nNoCancel, elNoCancel, timeSmear, unionOnly,
      			  &newPts, xNew, yNew, fcstHr, snapshotHrs, gfaTop,
      			  gfaBottom, fzlTop, fzlBottom, worstFreq, worstSev, 
			  tmpIssue, IFRType, MtnObscType, sigmetRef, &ier );

      if ( el->hdr.vg_type == GFA_ELM ) {
	  	  
	  if ( strcasecmp ( gfaBottom, "FZL" ) == 0 ) {

	     cvg_setFld ( el, TAG_GFA_FZL_TOP, fzlTop, &ier );
	     cvg_setFld ( el, TAG_GFA_FZL_BOTTOM, fzlBottom, &ier );

	  }
	  else {

	    cvg_rmFld ( el, TAG_GFA_FZL_TOP, &ier );
	    cvg_rmFld ( el, TAG_GFA_FZL_BOTTOM, &ier );

	  }

	  cvg_setFld ( el, TAG_GFA_TOP, gfaTop, &ier );
	  cvg_setFld ( el, TAG_GFA_BOTTOM, gfaBottom, &ier );
	  cvg_setFld ( el, TAG_GFA_FCSTHR, fcstHr, &ier );
	  cvg_setFld ( el, TAG_GFA_SNAPSHOTHRS, snapshotHrs, &ier );

	  if( strlen( worstFreq ) > (size_t)0 ) {
	      cvg_setFld ( el, "<Frequency>", worstFreq, &ier );
          }
	  if( strlen( worstSev ) > (size_t)0 ) {
	      cvg_setFld ( el, "<Severity>", worstSev, &ier );
          }
	  
	  if( strlen( worstIssue ) > (size_t)0 ) {
              cvg_setFld ( el, TAG_GFA_STATUS, worstIssue, &ier );
          }

	  sprintf( category, "%d", pgsmear_getSubType( el, GFA_SYSTEM_SMEAR ) );
	  cvg_setFld ( el, TAG_GFA_SUBTYPE, category, &ier );
	  pggfaw_getLineWidth ( el );

/*
 *  For IFR elements the cumulative IFRType is the 
 *  smear/outlook's type.
 */
          if ( strcasecmp( hazard, "IFR" ) == 0 ) {
              cvg_setFld( el, "<Type>", IFRType, &ier );
	  }
/*
 *  For MT_OBSC elements the cumulative MtnObscType is the 
 *  smear/outlook's type.
 */          
          if ( strcasecmp( hazard, "MT_OBSC" ) == 0 ) {
            cvg_setFld( el, "<Type>", MtnObscType, &ier );
          }
      }
      
      if ( ier < 0 ) {

/*
 * Error encountered when trying to smear the figures.
 */
	 errorCode = ier;
	 er_wmsg( "PGEN", &errorCode, NULL, &ier, 4, 0 );
	 NxmErr_update();

	 return;
      }
   }
   else {

/*
 *  Only 1 element selected, create the new element using the 
 *  same points as the selected element.
 */
      cvg_todev( el, &newPts, xNew, yNew, &ier );
      
      cvg_getFld ( el, TAG_GFA_FCSTHR, fcstHr, &ier );
      cvg_setFld ( el, TAG_GFA_SNAPSHOTHRS, fcstHr, &ier );      

      if( strlen( worstIssue ) > (size_t)0 ) {

          cvg_setFld ( el, TAG_GFA_STATUS, worstIssue, &ier );

      }
   }
   
   G_FREE ( elNoCancel, VG_DBStruct* );   

   if ( !useAllSnapShots ) {
       G_FREE ( elCancel, VG_DBStruct* );
       G_FREE ( elPtr, VG_DBStruct* );
   }
   
/*
 * Reduce points based on the maximum size increase and distance
 * allowed in prefs.tbl.
 */
   ctb_pfstr ( "SMEAR_RED_INC_PCT", cval, &ier );
   if ( ier >= 0 ) {
       cst_crnm ( cval, &tval, &ier );
       if ( ier == 0 ) {
           reducePct = tval;
       }
   }

   ctb_pfstr ( "SMEAR_INCR_DST", cval, &ier );
   if ( ier >= 0 ) {
       cst_crnm ( cval, &tval, &ier );
       if ( ier == 0 ) {           
	   reduceDst = tval;
       }
   }

   ptsReduce = G_TRUE;
   ctb_pfstr ( "SMEAR_PTS_REDUCE", cval, &ier );
   if ( ier == 0 && strcasecmp ( cval, "FALSE" ) == 0 ) {
      ptsReduce = G_FALSE;
   }
   
   sprintf ( reduceopt, "<alg_choice>3</alg_choice>" );	  
   sprintf ( tmpopt, "<incr_pct>%10.2f</incr_pct>", reducePct );	  
   strcat ( reduceopt, tmpopt);
   sprintf ( tmpopt, "<incr_dst>%10.2f</incr_dst>", reduceDst );	  
   strcat ( reduceopt, tmpopt);
   sprintf ( tmpopt, "<coord_sys>%s</coord_sys>", sys_D );	  
   strcat ( reduceopt, tmpopt );
   
   strcat ( reduceopt, "<format_prefix>FROM</format_prefix>" );           
      
   cvg_getFld ( &tmpEl[ numSnapShots - 1 ], TAG_GFA_FCSTHR, fcstHr, &ier );
   if ( atoi( fcstHr ) > 6 ) {
       strcat ( reduceopt, "<format_prefix>BOUNDED BY</format_prefix>" );    
   }	  
   
   if ( ptsReduce == G_TRUE && reducePts ) {
       
       cgr_reducePts ( reduceopt, newPts, xNew, yNew, NULL,
	           &tmpPts, xtmp, ytmp, NULL, &ier );
	  	 
       if ( ier == 0 ) {
           newPts = tmpPts;
           for ( ii = 0; ii < tmpPts; ii++ ) {
	       xNew[ii] = xtmp[ii];
	       yNew[ii] = ytmp[ii];
           }
       }
   }

/*
 *  If we're working with a GFA element use cgr_centroid to 
 *  position the text in the center of the new (smeared) element.
 */
   if ( el->hdr.vg_type == GFA_ELM ) {
      cgr_centroid( xNew, yNew, &newPts, &xCent, &yCent, &area, &ier ); 

      if( ier >= 0 ) {
         gtrans( sys_D, sys_M, &onePt, &xCent, &yCent, &lat, &lon, &ier, 
      		 strlen(sys_D), strlen(sys_M) );
         if ( ier >= 0 ) {
	    sprintf ( value, "%7.2f", lat );
	    cvg_setFld ( el, TAG_GFA_LAT, value, &ier );
	    cvg_setFld ( el, TAG_GFA_ARROW_LAT, value, &ier );

	    sprintf ( value, "%7.2f", lon );
	    cvg_setFld ( el, TAG_GFA_LON, value, &ier );
	    cvg_setFld ( el, TAG_GFA_ARROW_LON, value, &ier );
         }
      }	

/*
 *  Set the category to Smear
 */
      sprintf( category, "%d", pgsmear_getSubType( el, GFA_SYSTEM_SMEAR ) );
      cvg_setFld ( el, TAG_GFA_SUBTYPE, category, &ier );
      pggfaw_getLineWidth ( el );

   }

/*
 *  Insure that the smear element is not grouped with any other element.
 */
   el->hdr.grptyp = 0;
   el->hdr.grpnum = 0;


   if( makeSmearElm ) {
       pgsmear_doSnap ( el, newPts, xNew, yNew );

   }

   for( ii=0; ii<newPts; ii++ ) {
      xPts[ ii ] = xNew[ ii ];
      yPts[ ii ] = yNew[ ii ];      
   }

   if ( numIn <= 0 ) {
       for( ii = 0; ii < numSnapShots; ii++ ) {
           cvg_freeElPtr( &tmpEl[ii] );
       }
   
       G_FREE ( tmpEl, VG_DBStruct );
   }
   
   *numPts = newPts;
}

/*=====================================================================*/

static void pgsmear_smearAll( void )
/************************************************************************
 * pgsmear_smearAll	                                                *
 *                                                                      *
 * Perform the smear all operation.                                     *
 *                                                                      *
 * static void pgsmear_smearAll ( void )				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 * E. Safford/SAIC	11/04	init nblocks on smearEl/outlkEl for aix *
 * E. Safford/SAIC	12/04	call psmear_deleteSmears and refresh 	*
 * E. Safford/SAIC	12/04	init numSmearPts, numOutlkPts		*
 * B. Yin/SAIC		 1/05	added a call to snap			*
 * J. Wu/SAIC		10/05	change GFA_OUTLOOK to GFA_SYSTEM_OUTLOOK*
 * J. Wu/SAIC		10/05	allow time smear and rubberband smear	*
 * B. Yin/SAIC		11/05	change GFA category to GFA subtype	*
 * J. Wu/SAIC		12/05	smear for every valid node in smear list*
 * B. Yin/SAIC		01/06	added a call to get GFA line width	*
 * E. Safford/SAIC	01/06	keep the 6hr snapshot in the outlook	*
 * J. Wu/SAIC		01/06	use size check option in cgr_reducePts	*
 * B. Yin/SAIC		04/06	skip FZLVL				*
 * B. Yin/SAIC		07/06	change subtype (hazard and cat types) 	*
 * J. Wu/SAIC		10/06	modify cgr_reducePts			*
 * J. Wu/SAIC		10/06	default reducePct & reduceDst to 0	*
 * J. Wu/SAIC		11/06	add an option to turn on/off point 	*
 *				reduction				*
 * J. Wu/SAIC		12/06	add "SMEAR_RED_INC_PCT" to pt reduction	*
 * J. Wu/SAIC		01/07	use float input for cgr_reducePts	*
 * J. Wu/SAIC		10/07	Position text box arrow	at the center	*
 * J. Wu/SAIC		10/07	Refresh in pgsmear_confirmOkCb instead	*
***********************************************************************/
{   
   int			curLayer, ier, ii, len, ha, hb;
   int			numSmearPts, numOutlkPts, jj, ptsReduce;

   int			numOutPts = 0;
   int			tmpPts = 0;
   int          	onePt       = 1;

   float		smearX[ MAXPTS ], smearY[ MAXPTS ];
   float		outlkX[ MAXPTS ], outlkY[ MAXPTS ];

   float		outX[ MAXPTS ], outY[ MAXPTS ];
   float		xtmp[ MAXPTS ], ytmp[ MAXPTS ];
   float		xCent = 0.0F, yCent = 0.0F, area = 0.0F;
   float		lat, lon;
   float		reducePct = 0.0F, reduceDst = 0.0F, tval;

   char		        cval[5], value[32], category[32];
   char			newSnapshotHrs[256] = "";
   char			*testPtr = NULL, *prevPtr = NULL;
   char			newFcstHrs[256];

   char			reduceopt[256], tmpopt[64];

   VG_DBStruct		smearEl, outlkEl;
   
   struct GfaList_t	*list = NULL, *listPtr = NULL;
/*---------------------------------------------------------------------*/

    curLayer = pglayer_getCurLayer();

    pgsmear_deleteSmears();

    pgsmear_makeSmearList( curLayer, &list );
        
    smearEl.hdr.vg_type = GFA_ELM;
    smearEl.elem.gfa.info.nblocks = 0;
    outlkEl.hdr.vg_type = GFA_ELM;
    outlkEl.elem.gfa.info.nblocks = 0;
       
    if ( list ) {

        listPtr = list;
	
	while ( listPtr != NULL ) {
	
/*
 *  Skip FZLVL
 */
	    if ( strcasecmp( listPtr->hazard, "FZLVL" ) == 0 ) {

	       listPtr = listPtr->next;
	       continue;

	    }
	     
	    numSmearPts  = 0;
            numOutlkPts  = 0;

/*
 *  get the smear points (snapshots from 0 - 6 hrs );
 */
            pgsmear_smearTwoHour ( listPtr, "0", "6", False, _smearColor, 
	                 &smearEl, &numSmearPts, smearX, smearY );
	    if( numSmearPts > 0 ) { 
	        pgsmear_doSnap ( &smearEl, numSmearPts, smearX, smearY );
            }
	    
/*
 * get the outlook points (snapshots from 6 - 12 hrs );
 */ 
            if ( pgsmear_isOutlkExist ( listPtr, 6 ) ) {
	        pgsmear_smearTwoHour ( listPtr, "6", "12", False, _outlkColor, 
	                      &outlkEl, &numOutlkPts, outlkX, outlkY );
	 
                sprintf( category, "%d", pgsmear_getSubType( &outlkEl, GFA_SYSTEM_OUTLOOK ) );
   	        cvg_setFld ( &outlkEl, TAG_GFA_SUBTYPE, category, &ier );
	  	pggfaw_getLineWidth ( &outlkEl );

                for( ii = 0; ii < numOutlkPts; ii++ ) {
                        outX[ ii ] = outlkX[ ii ];
                        outY[ ii ] = outlkY[ ii ];
	            }
	    
	        numOutPts = numOutlkPts;
        
/*
 * Reduce points based on the maximum size increase and
 * distance allowed in prefs.tbl.
 */
                ctb_pfstr ( "SMEAR_RED_INC_PCT", cval, &ier );
                if ( ier >= 0 ) {
		    cst_crnm ( cval, &tval, &ier );
		    if ( ier == 0 ) {
                        reducePct = tval;
                    }
		}

                ctb_pfstr ( "SMEAR_INCR_DST", cval, &ier );
                if ( ier >= 0 ) {
		    cst_crnm ( cval, &tval, &ier );
                    if ( ier == 0 ) {
                        reduceDst = tval;
                    }
		}

                ptsReduce = G_TRUE;
                ctb_pfstr ( "SMEAR_PTS_REDUCE", cval, &ier );
                if ( ier == 0 && strcasecmp ( cval, "FALSE" ) == 0 ) {
                    ptsReduce = G_FALSE;
                }
   
                sprintf ( reduceopt, "<alg_choice>3</alg_choice>" );	  
                sprintf ( tmpopt, "<incr_pct>%10.2f</incr_pct>", reducePct );	  
                strcat ( reduceopt, tmpopt);
                sprintf ( tmpopt, "<incr_dst>%10.2f</incr_dst>", reduceDst );	  
                strcat ( reduceopt, tmpopt);
	        
            	strcat ( reduceopt, "<format_prefix>BOUNDED BY</format_prefix>" );    
		
		if ( ptsReduce == G_TRUE ) { 
	            cgr_reducePts ( reduceopt, numOutPts, outX, outY, NULL,
	                        &tmpPts, xtmp, ytmp, NULL, &ier );
                }
		else {
		    tmpPts = numOutPts;
		    for ( jj = 0; jj < tmpPts; jj++ ) {
		        xtmp[ jj ] = outX[ jj ];
		        ytmp[ jj ] = outY[ jj ];
	            }
		}
		
/*
 *  Locate the text box in the middle of the Outlook
 */
                cgr_centroid( xtmp, ytmp, &tmpPts, &xCent, &yCent, &area, &ier ); 

                if ( ier >= 0 ) {
                    gtrans( sys_D, sys_M, &onePt, &xCent, &yCent, &lat, &lon, &ier, 
      		 			strlen(sys_D), strlen(sys_M) );
               
                    if ( ier >= 0 ) {
	                sprintf ( value, "%7.2f", lat );
	                cvg_setFld ( &outlkEl, TAG_GFA_LAT, value, &ier );
	                cvg_setFld ( &outlkEl, TAG_GFA_ARROW_LAT, value, &ier );
				        
	                sprintf ( value, "%7.2f", lon );
	                cvg_setFld ( &outlkEl, TAG_GFA_LON, value, &ier );
	                cvg_setFld ( &outlkEl, TAG_GFA_ARROW_LON, value, &ier );
                    }
                }

/*
 *  Correct the TAG_GFA_FCSTHR. Use the newSnapshotHrs to 
 *  set this - in format of "A-B", where A is the first hour
 *  listed in the newSnapshots and B is the last hour listed
 *  in the newSnapshots.  If there is only one hour listed in 
 *  the newSnapshots, set as 'A-A".
 */
	        len = strlen( newSnapshotHrs );
	        if ( len > 0 ) {
	            if ( newSnapshotHrs[len - 1] == ';' ) {
	                newSnapshotHrs[len - 1] = '\0';
	            }
	    
	            ha = atoi ( newSnapshotHrs );	    
	            hb = ha;

	            testPtr = strstr ( newSnapshotHrs, ";" );
	            while ( testPtr ) {	        
		        prevPtr = testPtr + 1;
		        testPtr = strstr ( prevPtr, ";" );
	            }
	    
	            if ( prevPtr ) {
		        hb = atoi ( prevPtr );	    
	            }
			    
	            sprintf ( newFcstHrs, "%d-%d", ha, hb );
	            cvg_setFld( &outlkEl, TAG_GFA_FCSTHR, newFcstHrs, &ier );
	        }
	
	        pgsmear_doSnap ( &outlkEl, tmpPts, xtmp, ytmp );
           
	   } /* outlook finished */ 

           cvg_freeElPtr( &smearEl );
           cvg_freeElPtr( &outlkEl );
                 	   
	   listPtr = listPtr->next;
	   
       } /* end of while loop */         
   } 

   pglayer_setChngMade( curLayer, TRUE );
   pgsmear_freeGfaList( list );

}

/*=====================================================================*/

static void pgsmear_getElmLocs( struct GfaList_t  *list,
				char		 *fcstHr,
				int		 *nElm,
				int		 elmLoc[] )
/************************************************************************
 * pgsmear_getElmLocs	                                                *
 *                                                                      *
 * Given a single list node, traverse its element nodes and return      *
 * arrays of the element locations (file positions) for snapshot        *
 * elements with matching forecat hour.      				*
 *                                                                      *
 * static void pgsmear_getElmLocs ( list, fcstHr, nElm, elmLoc )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*list		GfaList_t	element list			*	
 *	*fcstHr		Char		forecast hour to be matched	*	
 * Output parameters:                                                   *
 *	nElm		*int		number of elements found 	*
 *	elmLoc[]	int		file locations of elms found	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 * J. Wu/SAIC		10/05	revised to search for a given fcstHr	*
 * J. Wu/SAIC		05/06	search for fcstHr in format of "H:MM"	*
  ***********************************************************************/
{   
   int			elmHr, srchHr, ier1, ier2;
   struct GfaElm_t	*elmPtr;
/*---------------------------------------------------------------------*/
   
    *nElm = 0;
    
    if ( list != NULL ) {
        	
        elmPtr = list->elm;

/*
 *  Step through the elements.  If the forecast hour matches the 
 *  given forecast hour, save the file location and increment the counter.
 */
        while( elmPtr != NULL ) {
            pgsmear_fcstHr2Min ( elmPtr->fcstHr, &elmHr, &ier1 );
            pgsmear_fcstHr2Min ( fcstHr, &srchHr, &ier2 );
            
	    if ( ier1 == 0 && ier2 == 0 && elmHr == srchHr ) {
                elmLoc[ *nElm ] = elmPtr->fileLoc;
	        (*nElm)++;
            }

           elmPtr = elmPtr->next;
        }
    }
}

/*=====================================================================*/

static void pgsmear_makeSmearList( int layer, struct GfaList_t **list ) 
/************************************************************************
 * pgsmear_makeSmearList                                                *
 *                                                                      *
 * Create a list (tree actually) containing information about the GFA   *
 * elements on the current layer.  First get an array of file offsets 	*
 * for all GFA elements on this layer.  Then create the list using the  *
 * GfaList_t (head nodes) and GfaElm_t (leaf nodes) structures.  The    *
 * GfaElm_t (leaf) nodes will be sorted by forecast hour, lowest to     *
 * highest.  								*
 *                                                                      *
 * void pgsmear_makeSmearList ( )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	layer		int	pgen layer to process			*
 *									*
 * Output parameters:                                                   *
 *	**list		struct GfaList_t pointer to the head of the     *
 *					  list (tree).			*	
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 * E. Safford/SAIC	07/05	rm sequence from GfaList_t struct	*
 * J. Wu/SAIC		05/06	process snapshots with special hours	*
 * E. Safford/SAIC	05/06	don't include user drawn Smears & Otlks *
 * J. Wu/SAIC		06/06	revise for "Smear Tag"			*
 * B. Yin/SAIC		07/06	change subtype (hazard and cat types) 	*
 * E. Safford/SAIC	05/07	exclude GFA_FBBA_* elements from list	*
 *				 and fix memory leak			*
 * J. Wu/SAIC		06/08	Exclude FZLVLSs/non-formattable hazards	*
 ***********************************************************************/
{   
   struct GfaList_t 	*listHead = NULL, *listPtr, *prevList;
   struct GfaElm_t	*elmPtr, *prevElm, *newElm;
   VG_DBStruct		el;
   int			offsets[ MAX_EDITABLE_ELEMS ], noffsets = 0;
   int			iret, ii, newElmHr = -1, elmHr = -1, ier;
   int			subtype;
   Boolean		storedEl;
   char			value[32], tag[32], hazard[32];
/*---------------------------------------------------------------------*/

   listPtr = NULL;
   elmPtr  = NULL;

   crg_goffsets( CLASS_MET, GFA_ELM, layer, offsets, &noffsets );
   
   for( ii=0; ii < noffsets; ii++ ) {

      storedEl = FALSE;
      prevList = listPtr = listHead;

/*
 *  Read the next offset and create newElm from it.
 */
      cvg_rdrec( cvg_getworkfile(), offsets[ii], &el, &iret );

/*
 *  Do not put user generated Smears or Outlooks into the list
 */
      subtype = -1;
      cvg_getFld( &el, TAG_GFA_SUBTYPE, value, &ier );
      subtype = atoi( value ) - atoi( value )/10 * 10;

      if( subtype < 0 || subtype == GFA_USER_SMEAR || 
       		subtype == GFA_USER_OUTLOOK || 
		subtype == GFA_FBBA_AIRMET || 
		subtype == GFA_FBBA_OUTLOOK ){ 
          cvg_freeElPtr( &el );
          continue;
      }

/*
 *  Do not put FZLVLs and non-formatable hazards into the list
 */
      cvg_getFld ( &el, TAG_GFA_AREATYPE, value, &ier );
      if ( strcmp( value, "FZLVL" ) == 0 ||
           !pgsmear_isFmtable( value ) )  {
          
	  cvg_freeElPtr( &el );
          continue;
      }


/*
 *  For "Smear Tag", hazard type and tag should match those of the
 *  selected GFA's.
 */
      if ( _state == SMEAR_TAG && _numElSelected > 0 ) {
          cvg_getFld ( &el, TAG_GFA_AREATYPE, hazard, &ier );
          cvg_getFld ( &el, TAG_GFA_TAG, tag, &ier );
          	    
	  if ( strcmp( hazard, _smearHazardTyp ) != 0 || 
	       strcmp( tag, _smearTag ) != 0 ) {
              cvg_freeElPtr( &el );
              continue;
	  }
      }
                

      newElm = pgsmear_newGfaElmT();

      newElm->fileLoc   = offsets[ii];

      cvg_getFld ( &el, TAG_GFA_FCSTHR, value, &ier );
      if( (strlen( value ) > (size_t)0) && (ier >= 0) ) {
         strcpy( newElm->fcstHr, value );
      }
            
      pgsmear_fcstHr2Min ( value, &newElmHr, &ier );                        

/*
 *  Put the newElm into the element list
 */
      while ( !storedEl && listPtr != NULL ) {

	 cvg_getFld( &el, TAG_GFA_AREATYPE, value, &ier );
	 strcpy( hazard, value );

	 cvg_getFld( &el, TAG_GFA_TAG, value, &ier );
	 strcpy( tag, value );

	 if( (strcmp( listPtr->hazard, hazard) ==0) &&
	     (strcmp( listPtr->tag, tag) == 0) ) {

	    prevElm = elmPtr = listPtr->elm;

	    while( (elmPtr != NULL) && (!storedEl) ) { 

	       if( elmPtr != NULL )  {
                  pgsmear_fcstHr2Min ( elmPtr->fcstHr, &elmHr, &ier );                        
               }

  	       if( prevElm == NULL ) {	    	/* shouldn't ever hit this */
                  listPtr->elm = newElm;     	/*   but just in case ...  */
	          storedEl     = TRUE;
	       }
	       else if( newElmHr < elmHr ) {   	/* insert left of elm */
	       					
	          if( listPtr->elm == elmPtr ) {	/* at head of list */
		     listPtr->elm = newElm;
		     newElm->next = elmPtr;
                  }
		  else {			
                     prevElm->next = newElm;		/* in middle of list */
	             newElm->next  = elmPtr; 
		  }
	          storedEl = TRUE;
               }
	   
	       if( !storedEl ) {		/* advance elmPtr */
	          prevElm       = elmPtr;
	          elmPtr        = elmPtr->next;
               }

            }

	    if( !storedEl ) {			/* store newElm at end of list */
	       prevElm->next = newElm;
	       storedEl      = TRUE;
            }

         }				/* end if */
         else {
	    prevList = listPtr;			/* advance listPtr */
	    listPtr  = listPtr->next;
         }
      }

/*
 * No match, create a new list and add a new elm 
 */
      if( !storedEl ) {
         listPtr           = pgsmear_newGfaListT(); 
	 cvg_getFld( &el, TAG_GFA_AREATYPE, value, &ier );
	 strcpy( listPtr->hazard, value );

	 cvg_getFld( &el, TAG_GFA_TAG, value, &ier );
	 if( ier >= 0 ) {
	    strcpy( listPtr->tag, value );
         }

/*
 *  If listHead is still NULL it means this is the first list struct
 *  added.  Point the listHead at it so we have a handle to the list.
 */
	 if( listHead == NULL ) {
	    listHead = listPtr;
         } else {
	    prevList->next = listPtr;
         }

         listPtr->elm      = newElm;
      }

      cvg_freeElPtr( &el );
   }

   *list = listHead;   
}


/*=====================================================================*/

static struct GfaList_t *pgsmear_newGfaListT( void )
/************************************************************************
 * pgsmear_newGfaListT	                                                *
 *                                                                      *
 * Allocate space for a new GfaList_T structure.                        *
 *                                                                      *
 * void pgsmear_newGfaListT ( void )					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *		*struct GfaElm_t	pointer to new GfaList_t 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 * E. Safford/SAIC	07/05	rm sequence from GfaList_t struct	*
 ***********************************************************************/
{   
   struct GfaList_t	*newList;
/*---------------------------------------------------------------------*/
 
   newList = ( struct GfaList_t * )malloc( sizeof( struct GfaList_t ) ); 
   newList->elm       = NULL;
   newList->next      = NULL;
   newList->hazard[0] = '\0';
   newList->tag[0]    = '\0';

   return( newList ); 

}

/*=====================================================================*/

static struct GfaElm_t *pgsmear_newGfaElmT( void )
/************************************************************************
 * pgsmear_newGfaElmT	                                                *
 *                                                                      *
 * Allocate space for a new GfaElm_t structure.                         *
 *                                                                      *
 * void pgsmear_newGfaElmT ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *		*struct GfaElm_t	pointer to new GfaElm_t 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 ***********************************************************************/
{   
   struct GfaElm_t 	*newElm;
/*---------------------------------------------------------------------*/

   newElm = ( struct GfaElm_t * )malloc( sizeof( struct GfaElm_t ));
   newElm->next = NULL;
   newElm->fileLoc = -1;
   newElm->fcstHr[0] = '\0';

   return( newElm );
}

/*=====================================================================*/

static void pgsmear_freeGfaList( struct GfaList_t *list )
/************************************************************************
 * pgsmear_freeGfaList	                                                *
 *                                                                      *
 * Free the list and all associated elms.                               *
 *                                                                      *
 * void pgsmear_freeGfaList ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*list		struct GfaList_t	list of gfa elements	*
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 ***********************************************************************/
{   
   struct GfaElm_t *elmPtr = NULL, *nextElm = NULL;
   struct GfaList_t *listPtr = NULL, *nextList = NULL;
/*---------------------------------------------------------------------*/

   listPtr = list;

   if( listPtr ) elmPtr = listPtr->elm;

   while( listPtr != NULL ) {

      while( elmPtr ) {
         nextElm = elmPtr->next;
	 free( elmPtr );
	 elmPtr = nextElm; 
      }

      nextList = listPtr->next;
      free( listPtr );
      listPtr = nextList;

      if( listPtr ) {
         elmPtr = listPtr->elm;
      }
   }

   list = NULL;
}

/*=====================================================================*/

static void pgsmear_exit ( void )
/************************************************************************
 * pgsmear_exit                                                  	*
 *                                                                      *
 * This function does all the cleanup necessary to exit from a smear.   * 
 *									*
 * void pgsmear_exit ( )         					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *    			None		            		 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/04	initial coding				*
 ***********************************************************************/
{   
    pghdlb_deselectAll ();
    pgsmear_popdown ();
    pgpalw_setCurBtns ( FUNC_SELECT, -1, -1 );
    pgpalw_setupOper ();
}

/*=====================================================================*/

static void pgsmear_deleteSmears ( void )
/************************************************************************
 * pgsmear_deleteSmears                                          	*
 *                                                                      *
 * This function deletes all pre-existing smears on the current layer.  * 
 *									*
 * void pgsmear_deleteSmears ( )      					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *    			None		            		 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/04	initial coding				*
 * B. Yin/SAIC		06/05	change GFA_SMEAR to GFA_SYSTEM_SMEAR	*
 * J. Wu/SAIC		10/05	change GFA_OUTLOOK to GFA_SYSTEM_OUTLOOK*
 * B. Yin/SAIC		11/05	change GFA category to GFA subtype	*
 * J. Wu/SAIC		06/06	revise for "Smear Tag"			*
 * B. Yin/SAIC		07/06	change subtype (hazard and cat types) 	*
 * S.Danz/AWC		07/06	Include new parameter to cvg_delet	*
 * E. Safford/SAIC	10/06	rm user drawn smears/outlooks (8.87)	*
 ***********************************************************************/
{
   int          curLayer, ier, ii, cat, rangeIdx;   
   int		offsets[ MAX_EDITABLE_ELEMS ], noffsets = 0;
   char		value[32], areaType[32], tag[32];
   Boolean	delete;
   VG_DBStruct	el;
/*---------------------------------------------------------------------*/

   el.elem.gfa.info.nblocks = 0;
   curLayer    = pglayer_getCurLayer();

   crg_goffsets( CLASS_MET, GFA_ELM, curLayer, offsets, &noffsets );

   for( ii=0; ii<noffsets; ii++ ) {

      cvg_rdrec( cvg_getworkfile(), offsets[ii], &el, &ier );

      cvg_getFld ( &el, TAG_GFA_SUBTYPE, value, &ier );
      cat = atoi( value ) - atoi( value )/10 * 10;

/*
 *  If element is a smear or outlook, delete it and wipe range record
 */
      if ( cat == GFA_SYSTEM_SMEAR || cat == GFA_SYSTEM_OUTLOOK ||
           cat == GFA_USER_SMEAR   || cat == GFA_USER_OUTLOOK ) {
	 
          delete = True;
          
/*
 *  For "Smear Tag", only delete the system_generated smear and
 *  outlook with the same hazard type & tag as the selected GFA's.
 */
	  if ( _state == SMEAR_TAG && _numElSelected > 0 ) {
              cvg_getFld ( &el, TAG_GFA_AREATYPE, areaType, &ier );
              cvg_getFld ( &el, TAG_GFA_TAG, tag, &ier );
          	    
	      if ( strcmp( areaType, _smearHazardTyp) != 0 || 
	           strcmp( tag, _smearTag ) != 0 ) {
                   delete = False;
	      }
          }
	  
	  if ( delete ) {
	      cvg_delet( cvg_getworkfile(), offsets[ii], FALSE, &ier );

              crg_getinx( offsets[ii], &rangeIdx, &ier );
	      crg_clear( rangeIdx, &ier );
          }
      }

      cvg_freeElPtr( &el );
   }
}    

/*=====================================================================*/

void pgsmear_snapEl ( Boolean expandOnly, VG_DBStruct *const el, int *iret )
/************************************************************************
 * pgsmear_snapEl	                                          	*
 *                                                                      *
 * This function snaps points of the input GFA element to the nearest 	*
 * VOR points.  							* 
 *									*
 * void pgsmear_snapEl ( expandOnly, el, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *	expandOnly	Boolean		flag to expand GFA only		*
 *	*el		VG_DBStruct	input GFA element		*
 *									*
 * Output parameters:                                             	*
 *	*iret		int		return code    		 	*
 *					=  0	normal return		*
 *					= -1	not a GFA		*
 *					= -2	no smear or outlook	*
 *					= -3	less than 3 points	*
 *					= -4	no snap points found	*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		1/05	Created					*
 * B. Yin/SAIC		6/05	change GFA_SMEAR to GFA_SYSTEM_SMEAR &	*
 *				GFA_USER_SMEAR				*
 * J. Wu/SAIC		10/05	change GFA_OUTLOOK to GFA_SYSTEM_OUTLOOK*
 *                              and GFA_USER_OUTLOOK			*
 * B. Yin/SAIC		11/05	change GFA category to GFA subtype	*
 * B. Yin/SAIC		12/05	change pgsmear_findSnapPt to clo_snapPt	*
 * D.W.Plummer/NCEP	03/06	changes for sparse snap points		*
 * B. Yin/SAIC		04/06	keep the order of points for FZLVL	*
 * D.W.Plummer/NCEP     04/06   changes for kink processing             *
 * B. Yin/SAIC		07/06	change subtype (hazard and cat types) 	*
 * D.W.Plummer/NCEP     09/06   Add tolerance parm to clo_snapPt	*
 * J. Wu/SAIC		03/07	add de-clustering processing		*
 * J. Wu/SAIC		09/07	snap FBBA elements			*
 ***********************************************************************/
{
    int		ier, ii, gfaCat, np, nout;
    float	*newLat, *newLon, tolerance;
    char 	catStr[ 4 ], haz[ 32 ];
    Boolean	reorder, closed;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Check if the input el is a GFA
 */    
    if ( (int)el->hdr.vg_type != GFA_ELM || el->hdr.delete )  {

       *iret = -1;
       return;

    }

/*
 *  Check if the input GFA is smear or outlook or FBBA.
 */
    cvg_getFld ( el, TAG_GFA_SUBTYPE, catStr, &ier );
    gfaCat = atoi( catStr ) - atoi( catStr )/10 * 10;

    if ( ( gfaCat != GFA_USER_SMEAR ) &&
    	 ( gfaCat != GFA_SYSTEM_SMEAR ) &&
    	 ( gfaCat != GFA_USER_OUTLOOK ) &&
         ( gfaCat != GFA_SYSTEM_OUTLOOK ) &&
    	 ( gfaCat != GFA_FBBA_AIRMET ) &&
         ( gfaCat != GFA_FBBA_OUTLOOK ) ) {

       *iret = -2;
       return;

    }
	 
/*
 *  Check if the GFA points are less than 3
 */
    if ( el->elem.gfa.info.npts < 3 ) {

       *iret = -3;
       return;

    }
    
/*
 *  Check if el is FZLVL and if it is open.
 *
 *  All FZLVLs should not and do not to be reordered for 
 *  clo_snapPt to work correctly.  For Open FZLVLs,  even if
 *  the first and last point are within clustering distance,
 *  do not shift them (if it is closed,  the first point
 *  will be shifted to the end so the NEW first point will not
 *  be part of a cluster.
 */
    closed = (el->hdr.closed == 0) ? False : True; 
    
    cvg_getFld ( el, TAG_GFA_AREATYPE, haz, &ier ); 

    if ( strcasecmp ( haz, "FZLVL" ) == 0 ) {
        reorder = False;       
    }
    else {
        reorder = True; 
    }

/*
 *  Snap polygon - the polygon should come back without any
 *                 clustering points.
 */    
    tolerance = 0.0F;
    np = el->elem.gfa.info.npts;
    G_MALLOC ( newLat, float, 2*(np+1), "pgsmear_snapEl newLat " );
    G_MALLOC ( newLon, float, 2*(np+1), "pgsmear_snapEl newLon " );

    clo_snapPoly (  expandOnly, tolerance, reorder, closed,
		    np, el->elem.gfa.latlon, &el->elem.gfa.latlon[np], 
		    &nout, newLat, newLon, &ier );
    
    if ( ier < 0 ) {
        *iret = -4;
        return;        
    }
           
/*
 *  Copy the snapped points into the GFA
 */
    el->elem.gfa.info.npts = nout;
    for ( ii = 0; ii < el->elem.gfa.info.npts; ii++ ) {
	el->elem.gfa.latlon[ ii ] = newLat[ ii ];
	el->elem.gfa.latlon[ ii + nout ] = newLon[ ii ];
    }
    
    G_FREE ( newLat, float );
    G_FREE ( newLon, float );

}

/*=====================================================================*/

static void pgsmear_doSnap ( VG_DBStruct *el, int numPts,
					float *xPts, float *yPts )
/************************************************************************
 * pgsmear_doSnap	                                          	*
 *                                                                      *
 * This function puts the smear points into the template GFA element,	*
 * calls pgsmear_snapEl to snap and then save the GFA element.		*
 *									*
 * void pgsmear_doSnap ( el, numPts, xPts, yPts )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	template GFA element		*
 *	numPts		int		number of smear points		*
 *	xPts[]		float		x of smear points in sys_D	*
 *	yPts[]		float		y of smear points in sys_D	*
 *									*
 * Output parameters:                                             	*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		1/05	Created					*
 * B. Yin/SAIC		8/05	Fixed a bug that deletes smear points	*
 * J. Wu/SAIC		10/05	adjust fcstHr for single hour smear	*
 * D. Plummer/SIB	05/06	fix npts snap bug			*
 * S.Danz/AWC		08/06	New flag to pgvgf_saveNewElm to place el*
 * E. Safford/SAIC	06/07	check ier from saveNewElm, avoid ranging*
 *				 if failure on the write occured	*
 * J. Wu/SAIC		07/08	compare points in MAP coordinate in 	*
 *				stead of Device coordinate		*
 ***********************************************************************/
{
    int		ii, jj, npts, ier;
    int		curLayer = 0;
    int		loc 	 = 0;

    char	fcstHr[10], value[10], warnMsg[256];
    
    float	*xtmp, *ytmp, *lat, *lon;
    float	xDev[ MAXPTS ], yDev[ MAXPTS ];

    Boolean	repeat;
/*---------------------------------------------------------------------*/

    curLayer    = pglayer_getCurLayer(); 

    G_MALLOC ( xtmp, float, numPts, "pgsmear_doSnap xtmp" );
    G_MALLOC ( ytmp, float, numPts, "pgsmear_doSnap ytmp" );
    G_MALLOC ( lat, float, numPts, "pgsmear_doSnap lat" );
    G_MALLOC ( lon, float, numPts, "pgsmear_doSnap lon" );

/*
 *  Convert device (x,y) to (lat,lon)
 */
    gtrans ( sys_D, sys_M, &numPts, xPts, yPts, xtmp, ytmp, &ier,
    	     strlen(sys_D), strlen(sys_M) );

/*
 *  Remove repeated points
 */

    npts = 0;
    for ( ii = 0; ii < numPts; ii++ ) {

	repeat = False;

	for ( jj = ii + 1; jj < numPts; jj++ ) {

	    if ( ( fabs ( xtmp[ ii ] - xtmp[ jj ] ) < TIE_DIST_IN_MAP ) &&

	         ( fabs ( ytmp[ ii ] - ytmp[ jj ] ) < TIE_DIST_IN_MAP ) ) {

	       repeat = True;
	       break;

	    }
	}

	if ( !repeat ) {

	   lat[ npts ] = xtmp[ ii ];
	   lon[ npts ] = ytmp[ ii ];
	   npts++;

	}
    }


/* 
 * Put the points into the GFA
 */
    if ( npts > 0 ) {

       el->elem.gfa.info.npts = npts;
       for ( ii = 0; ii < npts; ii++ ) {

           el->elem.gfa.latlon[ ii ] 	      = lat[ ii ];
           el->elem.gfa.latlon[ ii + npts ]   = lon[ ii ];

       }
    }

/*
 *  Snap
 */
    pgsmear_snapEl ( True, el, &ier );
    
    npts = el->elem.gfa.info.npts;
    
    cvg_todev( el, &npts, xDev, yDev, &ier );

/*
 *  If there is only one forecast hour A, make it as "A-A".
 */
    cvg_getFld ( el, TAG_GFA_FCSTHR, fcstHr, &ier );
    if ( strlen( fcstHr ) > (size_t)0 &&
        strstr ( fcstHr, "-" ) == NULL ) {
	sprintf ( value, "%s-%s", fcstHr, fcstHr );
	cvg_setFld ( el, TAG_GFA_FCSTHR, value, &ier );
    }
    
/*
 *  Save the GFA element.
 */
    pgvgf_saveNewElm ( NULL, sys_M, el, 0, 
	               xDev, yDev, TRUE, &loc, &ier );

/*
 *  If new element was saved without error then build a range record
 *  and store this new element as an undo step.
 *
 *  If a -28 error (BAD_NUMBER_OF_POINTS) was received from the cvg routines 
 *  under pgvgf_saveNewElm then post a message to the user to warn them 
 *  which tag sequence failed to produce a valid smear.  Most commonly a 
 *  very small, single snapshot sequence is the cause.
 */
    if( ier >= 0 ) {

/*
 * Unset the computational canvas so that the user's
 * display projection is used to compute the range record.
 */
    ncw_unset ();

/*
 *  Set the range record for the new element and complete the undo step.
 */
    crg_set ( el, loc, curLayer, &ier );

/*
 * Set the computational canvas back so that future
 * smears are computed correctly.
 */
    ncw_set ();
    ncw_sproj ( "PREFS" );

    pgundo_newStep ();
    pgundo_storeThisLoc ( loc, UNDO_ADD, &ier );
    pgundo_endStep ();
    }
    else if ( ier == BAD_NUMBER_OF_POINTS ) {	/* too few points in resulting smear */
	cvg_getFld ( el, TAG_GFA_TAG, value, &ier );
	sprintf( warnMsg, "Unable to smear tag %s.\n It has too few points after smearing and/or snapping.\n", value );
	NxmWarn_show( mcanvw_getDrawingW(), warnMsg );
    }

    G_FREE ( xtmp, float );
    G_FREE ( ytmp, float );
    G_FREE ( lat, float );
    G_FREE ( lon, float );
}

/*=====================================================================*/

static void pgsmear_smearTwoHour (  struct GfaList_t	*list,
			   char		*startHr, 
			   char		*endHr, 
			   Boolean 	doSnap,
			   int		color,
			   VG_DBStruct	*el,
			   int		*numPts,
			   float	xPts[],
			   float	yPts[])
/************************************************************************
 * pgsmear_smearTwoHour	                                        	*
 *                                                                      *
 * Perform a full smear operation on the snapshots found between the 	*
 * starting and ending forecast hours.  First a rubberband smear is	*
 * performed on the snapshots found at each single forecast hour.  Then	*
 * a time smear is performed on the resulting array of smeared elements *
 * (Time smear means a rubberband smear	on any two elements adjacent in	*
 * forecast hours and then union all those intermediate smears) 	*
 *                                                                      *
 * static void pgsmear_smearTwoHours ( list, fcstHr, doSnap, color,	*
 *                           		el, numPts, xPts, yPts )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*list		GfaList_t	pointer to element list		*
 *	*startHr	char		forecast hour to start		*
 *	*endHr		char		forecast hour to end		*
 *	doSnap		Boolean		flag to snap and display	*
 *	color		int		color of smear			*
 *									*
 * Output parameters:                                                   *
 *	*el		VG_DBStruct	resulting smear element		*
 *	*numPts		int		number of points in xPts/yPts	*
 *	xPts[]		float		array of x coords for smear el	*
 *	yPts[]		float		array of y coords for smear el	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           10/05   initial coding				*
 * J. Wu/SAIC           12/05   adjust for single elm smear		*
 * B. Yin/SAIC		04/06	fixed a bug to free memory		*
 * J. Wu/SAIC           05/06   smear specials with fcst hour as "H:MM"	*
 * J. Wu/SAIC           06/07   add more options in pgsmear_smear()	*
 * J. Wu/SAIC		09/07	add "unionOnly" for into pgsmear_smear	*
 * X. Guo/CWS		01/10   add _smearUnionOnly to handle two 	*
 *				smear algorithms			*
*************************************************************************/
{
    int			ii, jj, ier, shour, ehour, numOut, numElm;
    int			elmLoc[ MAX_EDITABLE_ELEMS ];
    char		singleHour[10];
    float		*lat, *lon;
    Boolean		tmSmear;    
    
    VG_DBStruct		*elOut;
/*---------------------------------------------------------------------*/

    el->elem.gfa.info.nblocks = 0;
    *numPts = 0;
                   
    pgsmear_fcstHr2Min ( startHr, &shour, &ier );
    pgsmear_fcstHr2Min ( endHr,   &ehour, &ier );
    
    if ( shour == ehour ) {
        tmSmear = False;
    }
    else {
        tmSmear = True;    
    }
    
    if ( list ) {
            
/*
 *  Do rubberband smear on the elements at each forecast hour.
 */
	numOut = 0;
        elOut = NULL;
	for ( ii = shour; ii <= ehour; ii += SPECIAL_INTERVAL ) {

/*
 *  We will convert "fcstHr" string into number of minutes when
 *  doing comaprison in the search - so just passing in the total
 * number of minutes in format of "H:MM" will do the job.
 */
	    sprintf ( singleHour, "0:%d", ii );
        
	    numElm = 0; 
            for( jj = 0; jj < MAX_EDITABLE_ELEMS; jj++ ) {
	         elmLoc[ jj ] = -1;
            }
	
	    pgsmear_getElmLocs ( list, singleHour, &numElm, elmLoc );
	            	    
	    if ( numElm > 0 ) { 
                G_REALLOC ( elOut, VG_DBStruct, numOut+1, "pgsmear_smearTwoHours: elOut" );
	        elOut[ numOut ].elem.gfa.info.nblocks = 0; 
		 
		pgsmear_smear ( numElm, elmLoc, doSnap, color, 0, NULL, 
		            False, True, False, True, _smearUnionOnly,
			    &elOut[ numOut ], numPts, xPts, yPts );
                
    		G_MALLOC ( lat, float, *numPts, "pgsmear_smearTwoHours: lat" );
    		G_MALLOC ( lon, float, *numPts, "pgsmear_smearTwoHours: lon" );
				
		gtrans ( sys_D, sys_M, numPts, xPts, yPts, lat, lon, &ier,
    	                 strlen(sys_D), strlen(sys_M) );
		
                elOut[ numOut ].elem.gfa.info.npts = *numPts;

                for ( jj = 0; jj < *numPts; jj++ ) {

                    elOut[ numOut ].elem.gfa.latlon[ jj ] 	    = lat[ jj ];
                    elOut[ numOut ].elem.gfa.latlon[ jj + *numPts ] = lon[ jj ];

                }
    		
	        G_FREE ( lat, float );
    		G_FREE ( lon, float );				
		
	        numOut++;
            }
        }       
        
/*
 *  Do time smear on the array of smeared single hour elements.
 */
	if ( numOut > 0 ) {    
 	    pgsmear_smear ( 0, NULL, doSnap, color, numOut, elOut, 
		            tmSmear, True, False, True, _smearUnionOnly,
			    el, numPts, xPts, yPts );
        }
        
/*
 *  clean up.
 */
	for ( ii = 0; ii < numOut; ii++ ) {            
	    cvg_freeElPtr( &elOut[ ii ] );	    
	}
	
	G_FREE ( elOut, VG_DBStruct );
    }        
}

/*=====================================================================*/

static Boolean pgsmear_isOutlkExist ( struct GfaList_t  *list,
							int outlkHr )
/************************************************************************
 * pgsmear_isOutlkExist	                                                *
 *                                                                      *
 * Given a single list node, traverse its element nodes and check if    *
 * there is a snapshot's forecast hour greater than the given forecat   *
 * hour.    	  							*
 *                                                                      *
 * static Boolean pgsmear_isOutlkExist ( list, outlkHr )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*list		GfaList_t	element list			*	
 *	outlkHr		int		hour to separate smear/outlook	*	
 * Output parameters:                                                   *
 *	pgsmear_isOutlkExist()	Boolean	True/False 			*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/05	initial coding				*
  ***********************************************************************/
{   
    Boolean	outlkFound = False;	    
    struct GfaElm_t	*elmPtr;   
/*---------------------------------------------------------------------*/
      
    if ( list != NULL ) {

        elmPtr = list->elm;

/*
 *  Step through the elements.  If a snapshot's forecast hour 
 *  matches the given outlook hour, return with True.
 */
	while( elmPtr != NULL ) {
            if ( strchr( elmPtr->fcstHr, '-' ) == NULL &&
	         atoi ( elmPtr->fcstHr ) > outlkHr ) {
	        outlkFound = True;
		break;    
	    }

           elmPtr = elmPtr->next;
        }
    }
    return ( outlkFound );
}

/*=====================================================================*/

static void pgsmear_getMinMaxFcstHr ( 	 const char	*fcstHr1,
					const char	*fcstHr2,
					char 		minFcstHr[],
					char		maxFcstHr[], 
			   		int		*iret )
/************************************************************************
 * pgsmear_getMinMaxFcstHr	                                        *
 *                                                                      *
 * Compares two forecast hours and returns the min and max. 		*
 *                                                                      *
 * static void pgsmear_getMinMaxFcstHr ( fcstHr1, fcstHr2, minMaxFlag	* 
 *                                       minFcstHr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fcstHr1	char	first forecast hour for comparison	*	
 *	*fcstHr2	char	second forecast hour for comparison	*	
 *	minFcstHr[]	char	greater one of the inputs		*	
 *	maxFcstHr[]	char	smaller one of the inputs		*	
 *	iret		int	return value				*	
 *				     0 - normal				*
 *				    -1 - fcstHr1 or fcstHr is empty	*
 *									*	
 * Output parameters:                                                   *
 *	None								*	
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		05/06	initial coding				*
 * E. Safford/SAIC	05/06 	handle an input hour range 		*
  ***********************************************************************/
{   
    int		hour1, hour2, ier;
    char	tmp1[ 32 ], tmp2[ 32 ], tmpHr[ 32 ];
    char	*dashPtr, *fcstPtr;
/*---------------------------------------------------------------------*/
   
    *iret = 0;        

/*
 *  For any inputs sent in as N-N format, just use the first hour of the
 *  range;
 */
    dashPtr = NULL;
    dashPtr = strstr( fcstHr1, "-" );
    if( dashPtr ) {
	   strcpy( tmpHr, fcstHr1 );
           fcstPtr = strtok( tmpHr, "-" );
	   fcstPtr = strtok( NULL, "-" );
	   strcpy( tmp1, fcstPtr );
    }
    else {
        strcpy( tmp1, fcstHr1 );
    }

    dashPtr = NULL;
    dashPtr = strstr( fcstHr2, "-" );
    if( dashPtr ) {
	   strcpy( tmpHr, fcstHr2 );
           fcstPtr = strtok( tmpHr, "-" );
	   fcstPtr = strtok( NULL, "-" );
	   strcpy( tmp2, fcstPtr );
    }
    else {
        strcpy( tmp2, fcstHr2 );
    }

/*
 *  Convert forecast hours to minutes. If either input is 
 *  empty, return;
 */
    pgsmear_fcstHr2Min ( tmp1, &hour1, &ier );
    if ( ier != 0 ) {
        *iret = -1;
	return;   
    }
        
    pgsmear_fcstHr2Min ( tmp2, &hour2, &ier );
    if ( ier != 0 ) {
        *iret = -1;
	return;   
    }
       
/*
 *  Compare the two inputs to find greater/smaller one
 */
    if ( hour1 < hour2 ) {
        strcpy( minFcstHr, tmp1 );
        strcpy( maxFcstHr, tmp2 );
    }
    else if ( hour1 > hour2 ) {
	strcpy( minFcstHr, tmp2 );
        strcpy( maxFcstHr, tmp1 );    
    }
    else {
	if ( strchr ( tmp1, ':' ) ) { 
	    strcpy( minFcstHr, tmp2 );
            strcpy( maxFcstHr, tmp2 );        
        }
	else {
	    strcpy( minFcstHr, tmp1 );
            strcpy( maxFcstHr, tmp1 );        	
	}
    }
}

/*=====================================================================*/

static void pgsmear_fcstHr2Min ( const char *fcstHr, int *minu, int *iret )
/************************************************************************
 * pgsmear_fcstHr2Min	                                        	*
 *                                                                      *
 * Converts a forecast hour string into number of minutes. 		*
 *                                                                      *
 * static void pgsmear_fcstHr2Min ( fcstHr, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fcstHr		char	first forecast hour for comparison	*	
 *	*minu		int	number of minutes			*	
 *	iret		int	return value				*	
 *				     0 - normal				*
 *				    -1 - fcstHr is empty		*
 *									*	
 * Output parameters:                                                   *
 *	None								*	
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		05/06	initial coding				*
  ***********************************************************************/
{   
    char	*cptr, hour[32];
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
/*
 *  Return if input is empty
 */
    if ( !fcstHr || (int)strlen( fcstHr ) == 0 ) {
        *minu = 0;        
        *iret = -1;
	return;
    }
    
/*
 *  convert forecast hour to minutes
 */
    strcpy ( hour, fcstHr );
    cptr = strtok( hour, ":" );
    *minu = atoi ( cptr ) * 60;
    cptr = strtok ( NULL, ":" );	
    if ( cptr ) {
        *minu += atoi( cptr );
    }
}
   
/*=====================================================================*/
/* ARGSUSED */
static void pgsmear_confirmOkCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgsmear_confirmOkCb							*
 *									*
 * Callback function for the smear confirmation "OK" button.		*
 *									*
 * static void pgsmear_confirmOkCb ( wdgt, clnt, call )			*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		Widget that activated callback		*
 *	clnt	XtPointer	Pointer to client data			*
 *	call	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/06	initial coding				*
 * J. Wu/SAIC		10/07	refresh/rebuild range after redraw	*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/
    
    pgsmear_smearAll();

/*
 * Unset the computational window temporarily.
 * Note: should rebuild range record after redraw,  since the auto 
 *       placement may relocate GFA text box.
 */
    ncw_unset ();

    cvg_redraw( NULL, &ier );
    
    pgpalw_refresh( );
        
    switch ( _state ) {

        case SMEAR_ALL:		/* Smear All */	
	    pgsmear_exit();

	  break;

        case SMEAR_TAG:		/* Smear Tag */
	    _numElSelected = 0;
	    pgpalw_setCurBtns ( FUNC_SMEAR, -1, -1 );    
            pgpalw_setupOper ();
            
	    _state = SMEAR_TAG;
	    mbotw_mouseSet( LMHINT_SELECT, MMHINT_CANCEL );
	     	
/*
 * Reset the computational window.
 */
            ncw_set ();
            ncw_sproj ( "PREFS" );

	  break;    
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgsmear_confirmCancelCb ( Widget wid, XtPointer clnt, 
					XtPointer call )
/************************************************************************
 * pgsmear_confirmCancelCb						*
 *									*
 * Callback function for the smear confirmation "Cancel" button.	*
 *									*
 * static void pgsmear_confirmCancelCb ( wid, which, call )		*
 *									*
 * Input parameters:							*
 *	wid	Widget		Widget that activated callback		*
 *	clnt	XtPointer	Pointer to client data			*
 *	call	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		06/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
/*
 * Unset the computational window temporarily
 */
    ncw_unset ();
    
    pghdlb_deselectAll ();
    pgactv_clearActv();     
    
    _numElSelected = 0;
    
    if ( _state == SMEAR_TAG ) {    
	pgpalw_setCurBtns ( FUNC_SMEAR, -1, -1 );    
        pgpalw_setupOper ();
        _state = SMEAR_TAG;
	mbotw_mouseSet( LMHINT_SELECT, MMHINT_CANCEL );
    }

/*
 * Reset the computational window
 */
    ncw_set ();
    ncw_sproj ( "PREFS" );
}

/*=====================================================================*/

static int pgsmear_getSubType( VG_DBStruct *el, int category )
/************************************************************************
 * pgsmear_getSubType		                                       	*
 *                                                                      *
 * This function gets the subtype from the hazard type of input element *
 * and the input category.						*
 *									*
 * static int pgsmear_getSubType ( el, category )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	input GFA element array		*
 *	category	int		category			*
 *									*
 * Output parameters:                                                   *
 *    			None			       		 	*
 *									*
 * Return parameters:                                             	*
 *    			int		subtype  		 	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		7/06	Created					*
 ***********************************************************************/
{
    int  ier, subtype;
    char haz[ 32 ], level[ 32 ];
/*---------------------------------------------------------------------*/
     
    cvg_getFld ( el, TAG_GFA_AREATYPE, haz, &ier );

/*
 *  Check if the hazard is a surface level FZLVL.
 */
    if ( strcasecmp( haz, "FZLVL" ) ) {

       cvg_getFld ( el, "Level", level, &ier );

       if ( strcasecmp( level, "SFC" ) == 0 ) {

	  strcat( haz, "_SFC" );

       }
    }

    pggfaw_makeSubtype( pggfaw_getHazardType( haz ), category, &subtype, &ier );

    if ( ier != 0 ) subtype = 99;

    return subtype;

}

/***********************************************************************/

static Boolean pgsmear_isFmtable ( char *hazIn )
/************************************************************************
 * pgsmear_isFmtable                                                   	*
 *                                                                      *
 * This routine checks if a hazard belongs to one of the three valid	*
 * categories - "SIERRA", "TANGO", or "ZULU". If yes, it is formattable.*
 * If not, it could be smeared but cannot be formatted.			*
 *                                                                      *
 * static Boolean pgsmear_hasValidCat ( hazIn )				* 
 *                                                                      *
 * Input parameters:                                         		*
 *      *hazIn			Char     	hazard to be checked	*
 *                                                                      *
 * Output parameters:                                        	 	*
 *      pgsmear_isFmtable()	Boolean		True/False		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	06/08   Copied from pgfrom_isFmtable            *
***********************************************************************/
{
    int         ii, nHaz, ier, ntypes = 3;
    char	hazList[ STD_STRLEN ], cat[ 32 ];
    char	*typeStr[] = { "SIERRA", "TANGO", "ZULU" };
    Boolean	validCat;
/*---------------------------------------------------------------------*/
/*
 *  Load the hazard list first.
 */
    ctb_gfaghaz ( ";", &nHaz, hazList, &ier );

    if ( nHaz == 0 ) {
	ctb_gfard ( &ier );
    }
	
/*
 *  Get the hazard's category.
 */
    ctb_gfagcat ( hazIn, cat, &ier );

/*
 *  Compare with the valid category list.
 */
    validCat = False;
    for ( ii = 0; ii < ntypes; ii++ ) {
        if ( strcasecmp( typeStr[ ii ], cat ) == 0 ) { 
            validCat = True;
            break;
	}
    }
    
    return ( validCat );    	
}

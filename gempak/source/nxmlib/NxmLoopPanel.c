#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmAnimationBits.h"

Widget NXMloopFrwdButton;
Widget NXMloopBkwdButton;
/*  NEW  */
Widget NXMloopFrwdBkwdButton;
Widget NXMstepFrwdButton;
Widget NXMstepBkwdButton;

_NXMpixmapData		NXMpixmapData;
_NXManimationFlags	NXManimationFlags;
_NXManimationDwell	NXManimationDwell;
_NXMbuttonColor		NXMbuttonColor;

WidgetList NXMloopInsensitive;
WidgetList NXMstepInsensitive;

int NXMloopInsensitiveNo;
int NXMstepInsensitiveNo;

static int	loopfrwdbkwd_increment=1;

enum NXManimationStatus_t NXManimationStatus;

static XtIntervalId NXMloopTimeoutId = (XtIntervalId)NULL;

/*
 *  Private functions
 */
void (*NxmDisplayPixmap) ( void );
void NxmAnimationLoopBackward ( void );
void NxmAnimationLoopForeward ( void );
void NxmAnimationLoopForewardBackward ( void );
void NxmAnimationStepBackward ( void );
void NxmAnimationStepForeward ( void );
static void NxmAnimationTimeoutProc ( XtIntervalId *id );
void NxmPushbuttonClickIn  ( Widget w );
void NxmPushbuttonClickOut ( Widget w );



static  XtActionsRec actionsTable[] = {
                {"NxmAnimationStepBackward",     (XtActionProc)NxmAnimationStepBackward},
                {"NxmAnimationStepForeward",     (XtActionProc)NxmAnimationStepForeward},
                {"NxmAnimationLoopForeward",     (XtActionProc)NxmAnimationLoopForeward},
                {"NxmAnimationLoopForeward",     (XtActionProc)NxmAnimationLoopForeward},
                {"NxmAnimationLoopForewardBackward", (XtActionProc)NxmAnimationLoopForewardBackward}
        };

static char defaultTranslations[] =
       "<Key>minus:             NxmAnimationStepBackward() \n\
        <Key>plus:              NxmAnimationStepForeward() \n\
        <Key>underscore:        NxmAnimationStepBackward() \n\
        <Key>equal:             NxmAnimationStepForeward() \n\
        <Key>L:                 NxmAnimationLoopForeward() \n\
        <Key>l:                 NxmAnimationLoopForeward() \n\
        <Key>!:                 NxmAnimationLoopForewardBackward()";

/************************************************************************
 * NxmLoopPanel.c							*
 *									*
 * CONTENTS:								*
 *	NxmAnimationPanelCreate						*
 ***********************************************************************/

Widget NxmAnimationPanelCreate ( Widget parent, char *panel_name,
			char *bgcolr_name, char *fgcolr_name,
			WidgetList loop_insensitive, int nloop_insensitive,
			XtCallbackProc callback, void (*display_image)(void) )
/************************************************************************
 * NxmAnimationPanelCreate                                              *
 *                                                                      *
 * This function creates a panel of five buttons, which controls the    *
 * animation effect.                                                    *
 *                                                                      *
 * Widget NxmAnimationPanelCreate(parent, panel_name, bgcolr_name, 	*
 *		fgcolr_name, loop_insensitive, nloop_insensitive,  	*
 *		callback, display_image)   	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *  *panel_name   char   name of the panel                              *
 *  *bgcolr_name  char   name of the control button background color    *
 *  *fgcolr_name  char   name of the control button foreground color    *
 *  loop_insensitive  WidgetList  list of widgets when looping          *
 *  nloop_insensitive	int    number of widgets in loop_insensitive  	*
 *  callback		void   callback function for the control buttons*
 *  display_image	void   function to display the pixmap           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 * 	NxmAnimationPanelCreate	Widget   The widget ID of the panel     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       10/94  disable multi-click for loop buttons.        *
 * C. Lin/EAI        1/95  add initialization to NXMpixmapData struct   *
 * C. Lin/EAI        1/96  XtAddTimeOut -> XtAppAddTimeOut          	*
 * S. Wang/GSC	    06/97  remove default_dwell from parm. list  and 	*
 *				call NxmDwell_getDwellPtr()		*
 * R. Tian/SAIC	    01/03  add True flag to NxmBxmBtn_create(Multi)	*
 ***********************************************************************/
{
XtTranslations	    trans_table;
static	char	    text_label[5][30];
int		    *default_dwell;
Widget              rc;
int		    i;

/*---------------------------------------------------------------------*/

	NXManimationFlags.loopfrwd = 0;
	NXManimationFlags.loopback = 0;
	NXManimationStatus = NXM_NOLOOP; 
	NXManimationFlags.loopfrwdbkwd = 0;

/*
 * initialize the NXMpixmapData structure
 */
	NXMpixmapData.total   = 0; 
	NXMpixmapData.current = 0; 

	rc = XtVaCreateManagedWidget( panel_name, 
		xmRowColumnWidgetClass, parent,
		XmNorientation,         XmHORIZONTAL,
		NULL);

	strcpy( text_label[0], "loop backward" );
	NXMloopBkwdButton = NxmBxmBtn_create( rc, 
		"NxmLoopback_button", NULL, NXMloopButtonWidth,
		NXMloopButtonHeight, fgcolr_name, bgcolr_name,
		(char*)NXMloopbInsensitive_bits, (char*)NXMloopbBack_bits,
		text_label[0], True, callback, (XtPointer)0);

        XtVaSetValues(NXMloopBkwdButton, 
		XmNmultiClick, XmMULTICLICK_DISCARD,
		NULL);
		 
	strcpy( text_label[1], "step backward" );
	NXMstepBkwdButton = NxmBxmBtn_create( rc, 
		"NxmStepback_button", NULL, NXMloopButtonWidth,
		NXMloopButtonHeight, fgcolr_name, bgcolr_name,
		(char*)NXMloopbInsensitive_bits, (char*)NXMstepbBack_bits,
		text_label[1], True, callback, (XtPointer)1 );

	strcpy( text_label[2], "step forward" );
	NXMstepFrwdButton = NxmBxmBtn_create( rc, 
		"NxmStepfrwd_button", NULL, NXMloopButtonWidth,
		NXMloopButtonHeight, fgcolr_name, bgcolr_name,
		(char*)NXMloopbInsensitive_bits, (char*)NXMstepbFrwd_bits,
		text_label[2], True, callback, (XtPointer)2 );

	strcpy( text_label[3], "loop forward" );
	NXMloopFrwdButton = NxmBxmBtn_create( rc, 
		"NxmLoopfrwd_button", NULL, NXMloopButtonWidth,
		NXMloopButtonHeight, fgcolr_name, bgcolr_name,
		(char*)NXMloopbInsensitive_bits, (char*)NXMloopbFrwd_bits,
		text_label[3], True, callback, (XtPointer)3 );

        XtVaSetValues(NXMloopFrwdButton, 
		XmNmultiClick, XmMULTICLICK_DISCARD,
		NULL);

	strcpy( text_label[4], "rock" );
	NXMloopFrwdBkwdButton = NxmBxmBtn_create( rc, 
		"NxmLoopfrwdbkwd_button", NULL, NXMloopButtonWidth,
		NXMloopButtonHeight, fgcolr_name, bgcolr_name,
		(char*)NXMloopbInsensitive_bits, (char*)NXMloopbFrwdBkwd_bits,
		text_label[4], True, callback, (XtPointer)4 );

        XtVaSetValues(NXMloopFrwdBkwdButton, 
		XmNmultiClick, XmMULTICLICK_DISCARD,
		NULL);

        XtVaGetValues(NXMloopFrwdButton, 
		XmNbackground,        &(NXMbuttonColor.bgColor),
		XmNtopShadowColor,    &(NXMbuttonColor.topShadowColor),
		XmNbottomShadowColor, &(NXMbuttonColor.bottomShadowColor),
		XmNarmColor,          &(NXMbuttonColor.armColor),
		NULL);

	if ( loop_insensitive ) {
	    NXMloopInsensitive = (WidgetList) XtMalloc( 
		(size_t)nloop_insensitive * sizeof(Widget)); 

	    for ( i = 0; i < nloop_insensitive ; i++ )
		NXMloopInsensitive[i] = loop_insensitive[i];

	    NXMloopInsensitiveNo = nloop_insensitive;
	}

	NxmDisplayPixmap = display_image;

	default_dwell = NxmDwell_getDwellPtr();

	NXManimationDwell.first = (unsigned long)default_dwell[0];
	NXManimationDwell.loop  = (unsigned long)default_dwell[1];
	NXManimationDwell.last  = (unsigned long)default_dwell[2];

        XtAppAddActions(NXMapp, actionsTable, XtNumber(actionsTable));

	trans_table = XtParseTranslationTable(defaultTranslations);

	XtAugmentTranslations(NXMloopFrwdButton,
				trans_table);
	XtAugmentTranslations(NXMloopBkwdButton,
				trans_table);
	XtAugmentTranslations(NXMloopFrwdBkwdButton,
				trans_table);
	XtAugmentTranslations(NXMstepFrwdButton,
				trans_table);
	XtAugmentTranslations(NXMstepBkwdButton,
				trans_table);

	return ( rc );

}

/*=====================================================================*/
/* ARGSUSED */
void NxmLoopButtonCallback ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmLoopButtonCallback						*
 *									*
 * This is a loop button function.					*
 *									*
 * void NxmLoopButtonCallback ( w, which, cbs )				*
 *									*
 * Input parameters:							*
 *	w		Widget		Widget id			*
 *	which		long		client data			*
 *	cbs		XtPointer	callback structure		*
 *									*
 * Output parameters:							*
 *	None.								*
 **									*
 * Log:									*
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	switch( which ) {
	    case 0: /* loop backward */
		NxmAnimationLoopBackward();
		break;

	    case 1: /* step backward */
		NxmAnimationStepBackward();
		break;

	    case 2: /* step forward */
		NxmAnimationStepForeward();
		break;

	    case 3: /* loop forward */
		NxmAnimationLoopForeward();
		break;

	    case 4: /* loop forward and backward */
		NxmAnimationLoopForewardBackward();
		break;
	}
}

/*======================================================================*/

void NxmAnimationLoopBackward ( void )
/************************************************************************
 * NxmAnimationLoopBackward                                             *
 *                                                                      *
 * This function initiates the reverse animation.                       *
 *                                                                      *
 * void NxmAnimationLoopBackward( )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       02/95   add NXMloopTimeoutId                        *
 * S. Jacobs/NCEP	 2/14	Make sure something was loaded before	*
 *				trying to step				*
 ***********************************************************************/
{
int                 i;
/*---------------------------------------------------------------------*/

  if ( NXMpixmapData.total > 0 ) {
    if(!NXManimationFlags.loopfrwd && !NXManimationFlags.loopfrwdbkwd) {
		
/*
 * stop the backward loop
 */
	if(NXManimationFlags.loopback) {

	    if ( NXMloopTimeoutId != (XtIntervalId)NULL )
               		XtRemoveTimeOut(NXMloopTimeoutId);

	    NxmPushbuttonClickOut( NXMloopBkwdButton );
	    NXManimationFlags.loopback = 0;
	
	    NXMpixmapData.current = (NXMpixmapData.current + 1 
		+ NXMpixmapData.total ) % ( NXMpixmapData.total );
	
	    XtSetSensitive(NXMloopFrwdButton, True);
	    XtSetSensitive(NXMloopFrwdBkwdButton, True);
	    XtSetSensitive(NXMstepFrwdButton, True);
	    XtSetSensitive(NXMstepBkwdButton, True);
	
	    if ( NXMloopInsensitive ) 
	        for ( i = 0; i < NXMloopInsensitiveNo; i++ )
	    	    XtSetSensitive(NXMloopInsensitive[i], True); 

	}
        else {
/*
 * start the backward loop
 */
	    NxmPushbuttonClickIn( NXMloopBkwdButton );
	    NXManimationFlags.loopback = 1;
	
	    XtSetSensitive(NXMloopFrwdButton, False);
	    XtSetSensitive(NXMloopFrwdBkwdButton, False);
	    XtSetSensitive(NXMstepFrwdButton, False);
	    XtSetSensitive(NXMstepBkwdButton, False);

	    if ( NXMloopInsensitive ) 
	        for ( i = 0; i < NXMloopInsensitiveNo; i++ )
	    	    XtSetSensitive(NXMloopInsensitive[i], False); 

                   if ( NXMloopTimeoutId != (XtIntervalId)NULL )
                                XtRemoveTimeOut(NXMloopTimeoutId);

		    NXMloopTimeoutId = XtAppAddTimeOut(NXMapp, 1L, 
			(XtTimerCallbackProc)NxmAnimationTimeoutProc, 
			(XtPointer)NULL);

	}
    }
  }
}

/*=====================================================================*/

void NxmAnimationLoopForeward ( void )
/************************************************************************
 * NxmAnimationLoopForeward                                             *
 *                                                                      *
 * This function initiates the forward animation.                       *
 *                                                                      *
 * void NxmAnimationLoopForeward( )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       02/95   add NXMloopTimeoutId                        *
 * S. Jacobs/NCEP	 2/14	Make sure something was loaded before	*
 *				trying to step				*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/

  if ( NXMpixmapData.total > 0 ) {
    if(!NXManimationFlags.loopback && !NXManimationFlags.loopfrwdbkwd) {

/*
 * stop the forward loop
 */
	if(NXManimationFlags.loopfrwd) {

	    if ( NXMloopTimeoutId != (XtIntervalId)NULL )
               		XtRemoveTimeOut(NXMloopTimeoutId);

	    NxmPushbuttonClickOut( NXMloopFrwdButton );
	    NXManimationFlags.loopfrwd = 0;

            NXMpixmapData.current = (NXMpixmapData.current - 1 
		+ NXMpixmapData.total) % (NXMpixmapData.total);

	    XtSetSensitive(NXMloopBkwdButton, True);
	    XtSetSensitive(NXMloopFrwdBkwdButton, True);
	    XtSetSensitive(NXMstepFrwdButton, True);
	    XtSetSensitive(NXMstepBkwdButton, True);

	    if ( NXMloopInsensitive ) 
	        for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
	    	    XtSetSensitive(NXMloopInsensitive[ii], True); 
	}
	else {

/*
 * start the forward loop
 */
	    NxmPushbuttonClickIn( NXMloopFrwdButton );
	    NXManimationFlags.loopfrwd = 1;

	    XtSetSensitive(NXMloopBkwdButton, False);
	    XtSetSensitive(NXMloopFrwdBkwdButton, False);
	    XtSetSensitive(NXMstepFrwdButton, False);
	    XtSetSensitive(NXMstepBkwdButton, False);

	    if ( NXMloopInsensitive ) 
	        for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
	    	    XtSetSensitive(NXMloopInsensitive[ii], False);

		NXMloopTimeoutId = XtAppAddTimeOut(NXMapp, 1L, 
			(XtTimerCallbackProc)NxmAnimationTimeoutProc, 
		        (XtPointer)NULL);
	}
    }
  }
}

/*======================================================================*/

void NxmAnimationLoopForewardBackward ( void )
/************************************************************************
 * NxmAnimationLoopForewardBackward                                     *
 *                                                                      *
 * This function initiates the forward and backward animation.          *
 *                                                                      *
 * void NxmAnimationLoopForewardBackward( )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       02/95   add NXMloopTimeoutId                        *
 * S. Jacobs/NCEP	 2/14	Make sure something was loaded before	*
 *				trying to step				*
 ***********************************************************************/
{
int                 ii;
/*---------------------------------------------------------------------*/

  if ( NXMpixmapData.total > 0 ) {
    if(!NXManimationFlags.loopfrwd && !NXManimationFlags.loopback) {

/*
 * stop the forward-backward loop
 */
	if(NXManimationFlags.loopfrwdbkwd) {

            if ( NXMloopTimeoutId != (XtIntervalId)NULL )
                        XtRemoveTimeOut(NXMloopTimeoutId);

	    NxmPushbuttonClickOut( NXMloopFrwdBkwdButton );
	    NXManimationFlags.loopfrwdbkwd = 0;

            NXMpixmapData.current = (NXMpixmapData.current - loopfrwdbkwd_increment
		+ NXMpixmapData.total) % (NXMpixmapData.total);

	    XtSetSensitive(NXMloopFrwdButton, True);
	    XtSetSensitive(NXMloopBkwdButton, True);
	    XtSetSensitive(NXMstepFrwdButton, True);
	    XtSetSensitive(NXMstepBkwdButton, True);

	    if ( NXMloopInsensitive ) 
	        for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
	    	    XtSetSensitive(NXMloopInsensitive[ii], True); 
	}
	else {
	
/*
 * start the forward-backward loop
 */
	    NxmPushbuttonClickIn( NXMloopFrwdBkwdButton );
	    NXManimationFlags.loopfrwdbkwd = 1;

	    XtSetSensitive(NXMloopFrwdButton, False);
	    XtSetSensitive(NXMloopBkwdButton, False);
	    XtSetSensitive(NXMstepFrwdButton, False);
	    XtSetSensitive(NXMstepBkwdButton, False);
	
	    if ( NXMloopInsensitive ) 
	        for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
	    	    XtSetSensitive(NXMloopInsensitive[ii], False);
	
	    NXMloopTimeoutId = XtAppAddTimeOut(NXMapp, 1L, 
		(XtTimerCallbackProc)NxmAnimationTimeoutProc, 
	        (XtPointer)NULL);
	}
    }
  }
}

/*=====================================================================*/

void NxmAnimationStepBackward ( void )
/************************************************************************
 * NxmAnimationStepBackward                                             *
 *                                                                      *
 * This function displays the previous frame of the circular loop.      *
 *                                                                      *
 * void NxmAnimationStepBackward( )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/94						*
 * S. Jacobs/NCEP	 2/14	Make sure something was loaded before	*
 *				trying to step				*
 ***********************************************************************/
{

  if ( NXMpixmapData.total > 0 ) {
    if(!NXManimationFlags.loopfrwd && !NXManimationFlags.loopback &&
	!NXManimationFlags.loopfrwdbkwd ) {

	NXMpixmapData.current = ( NXMpixmapData.current - 1
		+ NXMpixmapData.total ) % ( NXMpixmapData.total );

	NxmDisplayPixmap();
    }
  }
}

/*======================================================================*/

void NxmAnimationStepForeward ( void )
/************************************************************************
 * NxmAnimationStepForeward                                             *
 *                                                                      *
 * This function displays the next frame of the circular loop.          *
 *                                                                      *
 * void NxmAnimationStepForeward( )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/94						*
 * S. Jacobs/NCEP	 2/14	Make sure something was loaded before	*
 *				trying to step				*
 ***********************************************************************/
{

  if ( NXMpixmapData.total > 0 ) {
    if(!NXManimationFlags.loopfrwd && !NXManimationFlags.loopback &&
		!NXManimationFlags.loopfrwdbkwd ) {

	NXMpixmapData.current = ( NXMpixmapData.current + 1
		+ NXMpixmapData.total ) % ( NXMpixmapData.total );

	NxmDisplayPixmap();

    }
  }
}

/*======================================================================*/

int NxmQueryAnimationStatus ( void )
/************************************************************************
 * NxmQueryAnimationStatus						*
 *                                                                      *
 * This function returns the current animation status.          	*
 *                                                                      *
 * int NxmQueryAnimationStatus( )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *  NxmQueryAnimationStatus	int	current animation status        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       01/95                                               *
 ***********************************************************************/
{

    if ( NXManimationFlags.loopfrwd )
	return ( NXM_LOOPFRWD );

    if ( NXManimationFlags.loopback )
	return ( NXM_LOOPBACK );

    if ( NXManimationFlags.loopfrwdbkwd )
	return ( NXM_LOOPFRWDBKWD );

    return ( NXM_NOLOOP );
}

/*=====================================================================*/

void NxmStopAnimation ( void )
/************************************************************************
 * NxmStopAnimation							*
 *                                                                      *
 * This function stops the animation if one exists.			*
 *                                                                      *
 * void NxmStopAnimation( )                                       	*
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       01/95                                               *
 ***********************************************************************/
{
	if ( NXManimationFlags.loopfrwd ) {
		NxmAnimationLoopForeward();
		NXManimationStatus = NXM_LOOPFRWD; 
		XSync(NXMdisplay, False);
		return;
	}

	if ( NXManimationFlags.loopback ) {
		NxmAnimationLoopBackward();
		NXManimationStatus= NXM_LOOPBACK; 
		XSync(NXMdisplay, False);
		return;
	}

	if ( NXManimationFlags.loopfrwdbkwd ) {
		NxmAnimationLoopForewardBackward();
		NXManimationStatus = NXM_LOOPFRWDBKWD; 
		XSync(NXMdisplay, False);
		return;
	}
}

/*=====================================================================*/

void NxmRestartAnimation ( void )
/************************************************************************
 * NxmRestartAnimation                                                  *
 *                                                                      *
 * This function restarts the animation if the animation was terminated *
 * by calling NxmStopAnimation. It should be used with NxmStopAnimation.*
 *                                                                      *
 * void NxmRestartAnimation ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       01/95                                               *
 ***********************************************************************/
{

	XSync(NXMdisplay, False);

	switch ( NXManimationStatus ) {

	case NXM_LOOPFRWD: 
            		NXMpixmapData.current = 0;
			NXManimationStatus = NXM_NOLOOP;
			NxmAnimationLoopForeward();
			return;

	case NXM_LOOPBACK: 
            		NXMpixmapData.current = 0;
			NXManimationStatus = NXM_NOLOOP;
			NxmAnimationLoopBackward();
			return;

	case NXM_LOOPFRWDBKWD: 
            		NXMpixmapData.current = 0;
			NXManimationStatus = NXM_NOLOOP;
			NxmAnimationLoopForewardBackward();
			return;

	case NXM_NOLOOP:
		return;
	}
}

/*=====================================================================*/
/* ARGSUSED */
static void NxmAnimationTimeoutProc ( XtIntervalId *id ) /* Pixmap looping routine */
/************************************************************************
 * NxmAnimationTimeoutProc                                              *
 *                                                                      *
 * This function controls the animation.                                *
 *                                                                      *
 * static void NxmAnimationTimeoutProc ( id )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  id       XtIntervalId*  timer ID ( not used )                       *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 ***********************************************************************/
{
unsigned long       wait;
/*---------------------------------------------------------------------*/

   if(NXManimationFlags.loopfrwd || NXManimationFlags.loopback) {

       	wait = NXManimationDwell.loop;

       	if(NXMpixmapData.current == 0) 
       		wait = NXManimationDwell.first;
       	else 
	if(NXMpixmapData.current == (NXMpixmapData.total - 1) )
       		wait = NXManimationDwell.last;

       	NxmDisplayPixmap();

       	if(NXManimationFlags.loopfrwd)
	    NXMpixmapData.current = ( NXMpixmapData.current + 1
			+ NXMpixmapData.total ) % ( NXMpixmapData.total );

        if(NXManimationFlags.loopback) 
		    NXMpixmapData.current = ( NXMpixmapData.current - 1
			+ NXMpixmapData.total ) % ( NXMpixmapData.total );
		
      	NXMloopTimeoutId = XtAppAddTimeOut(NXMapp, wait, 
		(XtTimerCallbackProc)NxmAnimationTimeoutProc, 
		(XtPointer)NULL);
    }
    else if(NXManimationFlags.loopfrwdbkwd) {

            	wait = NXManimationDwell.loop;
            	if(NXMpixmapData.current == 0)
               		loopfrwdbkwd_increment =  1;
            	else if(NXMpixmapData.current == (NXMpixmapData.total - 1) )
               		loopfrwdbkwd_increment = -1;

            	NxmDisplayPixmap();

		    NXMpixmapData.current = ( NXMpixmapData.current + loopfrwdbkwd_increment
			+ NXMpixmapData.total ) % ( NXMpixmapData.total );

      		NXMloopTimeoutId = XtAppAddTimeOut(NXMapp, wait, 
			(XtTimerCallbackProc)NxmAnimationTimeoutProc, 
			(XtPointer)NULL);
   }
}

/*=====================================================================*/

void NxmPushbuttonClickIn ( Widget wdgt )
/************************************************************************
 * NxmPushbuttonClickIn                                                 *
 *                                                                      *
 * This function makes the pushbutton look like it was pushed in.       *
 *                                                                      *
 * void NxmPushbuttonClickIn(wdgt)                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt       Widget   pushbutton widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    XtVaSetValues( wdgt,
		XmNbackground,        NXMbuttonColor.armColor,
		XmNtopShadowColor,    NXMbuttonColor.bottomShadowColor,
		XmNbottomShadowColor, NXMbuttonColor.topShadowColor,
		NULL);
}

/*=====================================================================*/

void NxmPushbuttonClickOut ( Widget wdgt )
/************************************************************************
 * NxmPushbuttonClickOut                                                *
 *                                                                      *
 * This function makes the pushbutton look like it was released.        *
 *                                                                      *
 * void NxmPushbuttonClickOut(wdgt)                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt       Widget   pushbutton widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{

	XtVaSetValues( wdgt,
		XmNbackground,        NXMbuttonColor.bgColor,
		XmNtopShadowColor,    NXMbuttonColor.topShadowColor,
		XmNbottomShadowColor, NXMbuttonColor.bottomShadowColor,
		NULL);

}

/*=======================================================================*/

void NxmChangePixmapData ( int current, int total )
/************************************************************************
 * NxmChangePixmapData                                                  *
 *                                                                      *
 * This function gets the current pixmap index and the total pixmap     *
 * 	number of from the application.                                 *
 *                                                                      *
 * void NxmChangePixmapData ( current, total )      		        *
 *                                                                      *
 * Input parameters:                                                    *
 *  current     int   	current pixmap index                    	*
 *  total       int   	total pixmaps                     	        *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    NXMpixmapData.current = current;
    NXMpixmapData.total   = total;
}

/*=====================================================================*/

void NxmLoopbuttonSensitive ( Boolean state )
/************************************************************************
 * NxmLoopbuttonSensitive                                               *
 *                                                                      *
 * This function controls the sensitive status of loop buttons.         *
 *                                                                      *
 * void NxmLoopbuttonSensitive(state)                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *  state   Boolean   sensitive status of loop buttons                  *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    int ii;
/*--------------------------------------------------------------------*/

    XtSetSensitive( NXMloopFrwdButton, state );
    XtSetSensitive( NXMloopBkwdButton, state );
    XtSetSensitive( NXMloopFrwdBkwdButton, state );
    XtSetSensitive( NXMstepFrwdButton, state );
    XtSetSensitive( NXMstepBkwdButton, state );

    if ( state == False )
	if ( NXMloopInsensitive )
	    for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
                XtSetSensitive(NXMloopInsensitive[ii], True);

    if ( (state == True) & ( NXManimationFlags.loopfrwd |
		NXManimationFlags.loopback  | NXManimationFlags.loopfrwdbkwd ) ) {

	if ( NXMloopInsensitive )
	    for ( ii = 0; ii < NXMloopInsensitiveNo; ii++ )
                    XtSetSensitive(NXMloopInsensitive[ii], False);

	XtSetSensitive( NXMstepFrwdButton, False );
	XtSetSensitive( NXMstepBkwdButton, False );

	if ( NXManimationFlags.loopfrwd )  {
		XtSetSensitive( NXMloopBkwdButton, False );
		XtSetSensitive( NXMloopFrwdBkwdButton, False );
	}
	if ( NXManimationFlags.loopback )  {
		XtSetSensitive( NXMloopFrwdButton, False );
		XtSetSensitive( NXMloopFrwdBkwdButton, False );
	}
	if ( NXManimationFlags.loopfrwdbkwd )  {
		XtSetSensitive( NXMloopFrwdButton, False );
		XtSetSensitive( NXMloopBkwdButton, False );
	}
    }
}

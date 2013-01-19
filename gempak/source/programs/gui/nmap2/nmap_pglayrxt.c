#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

static Widget	   _pglayrxtWin;
static Widget	   _exitLbl;

static int         _exitLayer;      
static int         _origLayer;      
static Boolean     _exitAll = FALSE;      

/*
 *  private callback functions
 */
void pglayrxt_ctlBtnCb ( Widget, long, XtPointer );

/*
 *  private functions
 */

/************************************************************************
 * nmap_pglayrxt.c							*
 *									*
 * This module defines a layer exit popup window for VGF "Layering"     *
 * capability.                                                          *
 *									*
 * CONTENTS:								*
 *	pglayrxt_create()	create the layer exit window	        *
 *	pglayrxt_popup()	pop up the layer exit window	        *
 *	pglayrxt_popdown()	pop down the layer exit window	        *
 *	pglayrxt_isUp()		query if the exit window is up 		*
 *	pglayrxt_exitAll()	exit from layering			*
 *      pglayrxt_setExitMsg()	set/update the exit message		*
 *									*
 *	pglayrxt_ctlBtnCb()	callback for control buttons 		*
 ***********************************************************************/

/*=====================================================================*/

Widget pglayrxt_create ( Widget parent )
/************************************************************************
 * pglayrxt_create							*
 *									*
 * This function creates the layer exit window.		                *
 *									*
 * Widget pglayrxt_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pglayrxt_create	Widget  ID of the layer name window	        *
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	     03/02	initial coding                          *
 * J. Wu/SAIC	     03/02	adjust the size                         *
 ***********************************************************************/
{
    Widget	pane;
    WidgetList	exit_btnw;
    int		ii, nn;
    char	*btnstrs[] = {"Yes", "No", "Cancel"};
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _pglayrxtWin = XmCreateFormDialog(parent, "pglayrxt_popup",
				     NULL, 0);
    XtVaSetValues(_pglayrxtWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pglayrxtWin),
		  XmNtitle, "Exit Confirmation",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pglayrxt_pane",
			    xmPanedWindowWidgetClass, _pglayrxtWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

    /*
     * Create the message label.
     */
    _exitLbl = XmCreateLabel( pane, "", NULL, 0 );
    XtVaSetValues( _exitLbl,
		   XmNmarginWidth,        10,
		   XmNmarginHeight,       10,
		   NULL );
    XtManageChild( _exitLbl );
    
    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    exit_btnw = (WidgetList)XtMalloc(nn*sizeof(Widget));
    NxmCtlBtn_create( pane, 1, "pglayrxt_ctlBtn", nn, btnstrs, 
			(XtCallbackProc)pglayrxt_ctlBtnCb, exit_btnw );
    for ( ii = 0; ii < nn; ii++ ) {
         XtVaSetValues( exit_btnw[ii],
		        XmNmarginHeight,     5,
		        NULL );
    }
    XtFree((XtPointer)exit_btnw);
    
    XtManageChild(pane);

    return(_pglayrxtWin);
    
}

/*=====================================================================*/

void pglayrxt_popup ( void )
/************************************************************************
 * pglayrxt_popup							*
 *									*
 * This function pops up the exit window for the unsaved layer found.	*
 *									*
 * void pglayrxt_popup( void )						*
 *									*
 * Input parameters:                                                    *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 * J. Wu/SAIC		03/02	call pglayrxt_setExitMsg()		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
           
    _exitLayer = pglayer_getChngLayer ( 0 );
    _origLayer = pglayer_getCurLayer ();
        
    if ( _exitLayer < 0 )  {
        pglayrw_exit ();    
    }
    else {		
        pglayrxt_setExitMsg ();         
	XtManageChild ( _pglayrxtWin );
    }
      
}

/*=====================================================================*/

void pglayrxt_popdown ( void ) 
/************************************************************************
 * pglayrxt_popdown							*
 *									*
 * This function pops down the layer exit window.			*
 *									*
 * void pglayrxt_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 * J. Wu/SAIC		03/02	reset PGEN oper. to "SELECT" 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pglayrxtWin)) {
	XtUnmanageChild (_pglayrxtWin);
        
        /*
         *  Reset PGEN to "SELECT" status.
         */         
        pgpalw_setCurBtns (0, -1, -1);
	
    }
    
}

/*=====================================================================*/

Boolean pglayrxt_isUp ( void ) 
/************************************************************************
 * pglayrxt_isUp							*
 *									*
 * This function queries whether the layer exit window is up.	        *
 *									*
 * Boolean pglayrxt_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 *    pglayrxt_isUp	Boolean		True -- up			*
 * 					False -- down   		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (XtIsManaged (_pglayrxtWin));
}

/*=====================================================================*/

/* ARGSUSED */
void pglayrxt_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pglayrxt_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the layering exit window.	*
 *									*
 * void pglayrxt_ctlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 * J. Wu/SAIC		03/02	reset PGEN exit flag upon cancel	*
 * J. Wu/SAIC		03/02	reset NMAP exit flag upon cancel	*
 * J. Wu/SAIC		03/02	reset to prev. PGEN oper. if necessary	*
 * J. Wu/SAIC		03/02	rm pglayrxt_popdown() in "No" case	*
 * E. Safford/SAIC      03/02   made call to pglayrxt_setExitMsg only   *
 *                              for NO; YES is updtd via _exitAll below *
 * E. Safford/SAIC	11/03	call pgfilw_save, not _saveAcceptCb	*
 ***********************************************************************/
{
    int		next_layer;
/*---------------------------------------------------------------------*/
            
    switch ( which ) {
      
      case 0:	/* Ok to save */
      case 1:	/* No, do not save */
        
	next_layer = pglayer_getChngLayer ( _exitLayer+1 );
	
	if ( which == 0 ) {  /* save */
	    pglayer_setCurLayer ( _exitLayer );
	    if ( pglayer_isFileSaved ( _exitLayer ) ) {
	        pgfilw_save();
            }
	    else { 
 	        pgfilw_popup( FUNC_SAVE_VGF );
	    }
	}

	/*
	 *  Check next layer. For "Ok",  "Exit" should be delayed
	 *  into the file save callback to ensure correct save. 
	 */
	if ( next_layer > 0 ) {
	    _exitLayer = next_layer; 	    	

	    /*
	     *  If this is NO, update the exit message.  YES will be updated
	     *  via pglayrxt_exitAll, which is called by the callback func
	     *  for the File Save window.
	     */
	    if ( which == 1) {
	        pglayrxt_setExitMsg ();
  	    }
	}
	else {	    	    
	    _exitAll = TRUE;
	    if ( which == 1 ) {
		pglayrxt_exitAll ();
	    }
	}

	break;
 
      case 2:  /* Cancel */
	pglayrxt_popdown ();        
        pglayer_setCurLayer ( _origLayer );
	if ( pgpalw_isExitPGEN() ) pgpalw_setPrevOper ();
	pgpalw_setExitPGEN ( FALSE ); /* reset PGEN exit flag */
	mmenuw_setExitNMAP ( FALSE ); /* reset NMAP exit flag */
        
	break;

    } /* end of switch */
    
}

/*=====================================================================*/

void pglayrxt_exitAll ( void ) 
/************************************************************************
 * pglayrxt_exitAll							*
 *									*
 * This function exits the layering if the _exitAll flag is True.	*
 * It also exits PGEN/NMAP if _exitPGEN/_exitNMAP flags are True. 	*
 *									*
 * void pglayrxt_exitAll ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 * J. Wu/SAIC		03/02	add exit for PGEN			*
 * J. Wu/SAIC		03/02	add exit for NMAP application		*
 * J. Wu/SAIC		03/02	add call to pglayrxt_popdown ()		*
 * E. Safford/SAIC      03/02   add pglayrxt_setExitMsg() call          *
 * T. Piper/SAIC	03/06	fix bluecurve bug by not calling 	*
 *				pglayrxt_setExitMsg unnecessarily	*
 ***********************************************************************/
{
    Widget	draw_w;
/*---------------------------------------------------------------------*/

    if ( _exitAll ) {
        
	pglayrw_exit();                        
	pglayrxt_popdown ();        	    	    
	_exitAll = FALSE; 
        
	/*
	 *  If the "EXIT" is initiated from PGEN palatte, exit it.
	 */
	pgpalw_exit( NULL, NULL, NULL );        

	/*
	 *  If the "EXIT" is initiated from main window, exit NMAP.
	 */
        draw_w = (Widget)mcanvw_getDrawingW();
	mmenuw_exitNMAPCb( draw_w, NULL, NULL );        

    }
    else {
	pglayrxt_setExitMsg();
    }
 
}

/*=====================================================================*/

void pglayrxt_setExitMsg ( void ) 
/************************************************************************
 * pglayrxt_setExitMsg							*
 *									*
 * This function sets the proper message for exiting layering/PGEN.	*
 *									*
 * void pglayrxt_setExitMsg ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 ***********************************************************************/
{
    char	exit_msg[256], 
                msg1[] = { "There are unsaved changes in "},
    		msg2[] = { ".\nDo you want to save them?"};
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if ( _exitLayer >= 0 && _exitLayer < MAX_LAYERS ) {
	
        if ( XtIsManaged ( _exitLbl ) ) {
           XtUnmanageChild ( _exitLbl );
        }
	
	strcpy ( exit_msg, msg1 ); 
	
	if ( pglayrw_isUp() ) {    
            strcat ( exit_msg, pglayer_getName(_exitLayer) ); 
	}
	else {
            strcat ( exit_msg, "product generation" ); 
	}	    
        
	strcat ( exit_msg, msg2 ); 
        xmstr = XmStringCreateLtoR ( exit_msg, XmFONTLIST_DEFAULT_TAG );
	XtVaSetValues( _exitLbl, XmNlabelString, xmstr, NULL );
        XmStringFree ( xmstr );
        	
	XtManageChild ( _exitLbl );
	
    }
        
}

/*=====================================================================*/

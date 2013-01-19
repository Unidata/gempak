#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"
#include "proto_xw.h"

#define MAXNOPT	     35	    /* max number of option menu items  */

struct convertTblStrc {
    char        ori_grptyp;
    char        dest_grptyp;
    int		ori_grpnum;
    int         dest_grpnum;
    struct convertTblStrc *next;
    struct convertTblStrc *prev;
};

struct optMenuStrc {
    int		current[MAXNOPT];
    Boolean     chng_flag[MAXNOPT];
    Boolean     exist[MAXNOPT];
    Boolean     disabled[MAXNOPT];
    Widget	form;
    Widget	label;
    Widget	menu;
    Widget	pb[MAXNOPT];
};

static	struct	optMenuStrc	_curGrpStrc;
static  int     _curGrpCurr,    _curGrpNum;

static	struct	optMenuStrc	_chngToStrc;
static  int     _chngToNum;

static Widget	_grpChngWin;

static char**   _curGrpStr; 
static char**   _chngToStr;
static int	_numCurGrp = 0, _numChngTo = 0;

/*
 *  private functions
 */
void pggrpch_update  ( void );
void pggrpch_chngGrp ( void );

/*
 *  private callback functions
 */
void pggrpch_curGrpOptCb ( Widget, XtPointer, XtPointer );
void pggrpch_chngToOptCb ( Widget, XtPointer, XtPointer );
void pggrpch_ctlBtnCb    ( Widget, long, XtPointer );


/************************************************************************
 * nmap_pggrpch.c							*
 *									*
 * This module creates and displays the VG group change window. It also	*
 * contains the callbacks for it.					*
 *									*
 * CONTENTS:								*
 *	pggrpch_create()	create VG Group Change window	        *
 *	pggrpch_popup()	        pop up VG Group Change window	        *
 *	pggrpch_popdown()       popdown VG Group Change window	        *
 *	pggrpch_isUp()	        query if VG Group Change window is up 	*
 *                                                                      *
 *      pggrpch_update()        updates _grpChngWin window              *
 *	pggrpch_chngGrp()	change the group type of the elements	*
 *									*
 *      pggrpch_curGrpOptCb()   callback for current group option menu  *
 *      pggrpch_chngToOptCb()   callback for "change to" option menu    *
 *	pggrpch_ctlBtnCb()      callback for _grpChngWin control buttons*
 ***********************************************************************/

/*=====================================================================*/

void pggrpch_create ( Widget parent )
/************************************************************************
 * pggrpch_create						        *
 *									*
 * This function creates VG Group Change popup window.		        *
 *									*
 * void   pggrpch_create (parent)				        *
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/01   initial coding                          *
 * H. Zeng/EAI          09/01   removed unused variables                *
 * H. Zeng/EAI          03/02   renameed for new nmap_pggrpch file      *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget	pane, form1, form2, button, formbtn, label1;
    char	*btnstrs[] = {"Apply", "Cancel"};
    int		loff = 5, toff2 = 5;
    long	ii, nn;
    /*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _grpChngWin = XmCreateFormDialog ( parent, "grpchngw_popup",
				       NULL, 0);
    XtVaSetValues ( _grpChngWin, 
		    XmNnoResize,        True, 
		    NULL);
    XtVaSetValues ( XtParent(_grpChngWin),
		    XmNtitle, "VG Group Change",
		    NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("grpchngw_pane",
			    xmPanedWindowWidgetClass, _grpChngWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    /*
     * create current group choosing area.
     */
    form1 = XtVaCreateWidget ( "form1",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     * CURRENT GROUP
     */
    label1 = XtVaCreateManagedWidget ("CURRENT GROUP:",
				     xmLabelWidgetClass,	form1,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    /*
     * Create current group menu
     */
    _curGrpCurr  = 0;
    pgutls_createOptionMenu (form1, MAXNOPT, (XtPointer)&_curGrpCurr, NULL, 
                             pggrpch_curGrpOptCb, &_curGrpStrc.form, 
                             &_curGrpStrc.label, &_curGrpStrc.menu, 
                             _curGrpStrc.pb, NULL );
 
    XtVaSetValues (_curGrpStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	label1,
                   XmNleftOffset,       loff+5,
		   NULL);

    XtManageChild(form1);


    /*
     * create "change to" option area.
     */
    form2 = XtVaCreateWidget ( "form2",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     * "CHANGE TO" label
     */
    label1 = XtVaCreateManagedWidget ("CHANGE TO:",
				     xmLabelWidgetClass,	form2,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);


    /*
     * Create "change to" option menu
     */
    _chngToStrc.current[0] = 0;

    pgutls_createOptionMenu (form2, MAXNOPT, 
                             (XtPointer)&_chngToStrc.current[0], NULL, 
                             pggrpch_chngToOptCb, &_chngToStrc.form, 
                             &_chngToStrc.label, &_chngToStrc.menu, 
                             _chngToStrc.pb, NULL );
 
    XtVaSetValues (_chngToStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	label1,
                   XmNleftOffset,       loff+50,
		   NULL );

    XtManageChild(form2);


    /*
     * create control buttons
     */
    nn = XtNumber ( btnstrs );
    formbtn = XtVaCreateWidget("formbtn",
                            xmFormWidgetClass, pane,
			    XmNfractionBase,   (nn * 100),
                            NULL                       ); 

    for ( ii = 0; ii < nn; ii++ )  {

	button = XtVaCreateManagedWidget ( btnstrs[ii], 
			xmPushButtonWidgetClass, formbtn,
			XmNheight,               25,
			XmNwidth,                100,
                        XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         (ii * 100),
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        ((ii + 1) * 100),
			NULL );

	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)pggrpch_ctlBtnCb, (XtPointer)ii );

    }


    XtManageChild(formbtn);
    XtManageChild(pane);

}

/*=====================================================================*/

void pggrpch_popup ( void )
/************************************************************************
 * pggrpch_popup				      			*
 *									*
 * This function pops up VG Group Change window.		        *
 *									*
 * void pggrpch_popup()						        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/01   initial coding                          *
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 * H. Zeng/EAI          04/02   modified to initialize _grpStr          *
 * H. Zeng/EAI          05/02   modified to use master group type list  *
 ***********************************************************************/
{
int       ii, el_num, el_loc, grpnum, ngrp, selection, iret, ier2;
char      grptyp, cc[20], grpnam[20];
char      *names = NULL;
char      *ptr;
XmString  xmstr;
static  Boolean  first = TRUE;

/*---------------------------------------------------------------------*/

    /*
     * Set "CURRENT GROUP" and "CHANGE TO" option menus at
     * the first time.
     */
    if( first ) {

      /*
       * Initialize _curGrpStr and _chngToStr.
       */
      ces_gtgmgrps(FALSE, &ngrp, &names, &iret);
      _numCurGrp = ngrp;
      _curGrpStr = (char**) malloc( _numCurGrp * sizeof(char*) );

      ii = 0; 
      ptr = strtok(names, ";");
      while ( ptr != (char *)NULL ) {
          _curGrpStr[ii] = (char*) malloc( strlen(ptr)+5 );
          strcpy(_curGrpStr[ii], ptr); 

	  ptr = strtok(NULL, ";" );
          ii++;

      }
      if ( names != NULL )  free (names);


      ces_gtggrps(&ngrp, &names, &iret);
      _numChngTo = ngrp;
      _chngToStr = (char**) malloc( _numChngTo * sizeof(char*) );

      ii = 0; 
      ptr = strtok(names, ";");
      while ( ptr != (char *)NULL ) {
          _chngToStr[ii] = (char*) malloc( strlen(ptr)+5 );
          strcpy(_chngToStr[ii], ptr); 

	  ptr = strtok(NULL, ";" );
          ii++;

      }
      if ( names != NULL )  free (names);


      _curGrpNum = _numCurGrp + 1;
      _chngToNum = _numChngTo + 1;

      /*
       * Set "CURRENT GROUP" option menu:
       */
      sprintf (cc, "%s", "ALL GROUPS");
      xmstr = XmStringCreateLocalized (cc);
      XtVaSetValues(_curGrpStrc.pb[0],
	  XmNlabelString, 	xmstr,
	  NULL);
      XtManageChild(_curGrpStrc.pb[0]);
      XmStringFree (xmstr);

      for (ii = 1; ii < _curGrpNum; ii++) {
	  sprintf (cc, "%s", _curGrpStr[ii-1] );
          xmstr = XmStringCreateLocalized (cc);
	  XtVaSetValues(_curGrpStrc.pb[ii],
	      XmNlabelString, 		xmstr,
	      NULL);
    	  XtManageChild(_curGrpStrc.pb[ii]);
          XmStringFree (xmstr);
      }

      for (ii = _curGrpNum; ii < MAXNOPT; ii++) {
    	  XtUnmanageChild( _curGrpStrc.pb[ii] );
      }

      /*
       * Set "CHANGE TO" option menu.
       */
      sprintf (cc, "%s", "NO CHANGE");
      xmstr = XmStringCreateLocalized (cc);
      XtVaSetValues(_chngToStrc.pb[0],
	  XmNlabelString, 	xmstr,
	  NULL);
      XtManageChild(_curGrpStrc.pb[0]);
      XmStringFree (xmstr);

      for (ii = 1; ii < _chngToNum; ii++) {
	  sprintf (cc, "%s", _chngToStr[ii-1] );
          xmstr = XmStringCreateLocalized (cc);
	  XtVaSetValues(_chngToStrc.pb[ii],
	      XmNlabelString, 		xmstr,
	      NULL);
    	  XtManageChild(_chngToStrc.pb[ii]);
          XmStringFree (xmstr);
      }

      for (ii = _chngToNum; ii < MAXNOPT; ii++) {
    	  XtUnmanageChild( _chngToStrc.pb[ii] );
      }

      first = FALSE;

    }

    /*
     * Set default info. when popping up the VG Group Change window.
     */
    _curGrpCurr = 0;
    for (ii = 0; ii < _curGrpNum; ii++) {
        _chngToStrc.current[ii] = 0;
        _chngToStrc.chng_flag[ii] = FALSE;
        _chngToStrc.exist[ii] = FALSE;
        _chngToStrc.disabled[ii] = FALSE;
        XtSetSensitive( _curGrpStrc.pb[ii], FALSE );

    }
    XtSetSensitive( _curGrpStrc.form, FALSE );    


    /*
     * Search available group types based on the VG file on main window.
     */
    for (el_num = 0; el_num < MAX_EDITABLE_ELEMS; el_num++) {
	crg_goffset (el_num, &el_loc, &ier2);

        /*
         * Skip cleared range record
         */
	if (el_loc == -1) {
           continue;
        }

 	crg_ggrp (el_num, &grptyp, &grpnum, &ier2);	

	if (grpnum && grptyp != GRPTYP_OTHERS
                   && grptyp != GRPTYP_COMSYM
                   && grptyp != GRPTYP_WATCH
                   && grptyp != GRPTYP_CCF     ) {

            ces_gtgnam((int)grptyp, grpnam, &ier2);
            for( ii = 0; ii < _numCurGrp; ii++ ) {
	       if(strcmp(_curGrpStr[ii], grpnam) == 0) {
                 selection = ii + 1;
                 break;
               }
            }

            if( ii < _numCurGrp ) {
               _chngToStrc.exist[selection] = TRUE;
               XtSetSensitive( _curGrpStrc.pb[selection], TRUE );  

               /*
                * When there is at least one group, "ALL GROUPS" choice
                * makes sense.
                */
               _chngToStrc.exist[0] = TRUE;             

            } /* the end of if( ii < _numCurGrp ) */

	} /* the end of if (grpnum &&... */ 

    } /* the end of for (el_num... */


    /*
     * Make "ALL GROUPS" choice sensitive or insensitive according to
     * its "exist" flag.
     */
    if( _chngToStrc.exist[0] == TRUE ) {
        XtSetSensitive( _curGrpStrc.pb[0], TRUE );  
        XtSetSensitive( _curGrpStrc.form,  TRUE );    
    }

    /*
     * Update the VG Group Change Window
     */
    pggrpch_update();

    if( !pggrpch_isUp() ) {
        XtManageChild (_grpChngWin);
    }
    
}

/*=====================================================================*/

Boolean pggrpch_isUp ( void ) 
/************************************************************************
 * pggrpch_isUp							        *
 *									*
 * This function queries whether VG Group Change window is up.	        *
 *									*
 * Boolean pggrpch_isUp ()					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pggrpch_isUp 	Boolean       	True -- up,	False -- down	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/01	initial coding				*
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_grpChngWin));
}

/*=====================================================================*/

void pggrpch_popdown ( void ) 
/************************************************************************
 * pggrpch_popdown						        *
 *									*
 * This function pops down VG Group Change window.		        *
 *									*
 * void pggrpch_popdown ()					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/01	initial coding				*
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_grpChngWin)) {
	XtUnmanageChild (_grpChngWin);

    }
    
}

/*=====================================================================*/

void pggrpch_update ( void )
/************************************************************************
 * pggrpch_update                                                       *
 *                                                                      *
 * This function updates VG Group Change window.  		        *
 *                                                                      *
 * void pggrpch_update ()                                     	        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                   NONE                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      05/01   initial coding                              *
 * H. Zeng/EAI      03/02   renamed for new nmap_pggrpch file           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * Set the current "CURRENT GROUP" and "CHANGE TO" values
     */
    XtVaSetValues(_curGrpStrc.menu,
                  XmNmenuHistory,    _curGrpStrc.pb[_curGrpCurr],
                  NULL  );

    XtVaSetValues(_chngToStrc.menu,
                  XmNmenuHistory,    
                  _chngToStrc.pb[ _chngToStrc.current[_curGrpCurr] ],
                  NULL  );

    if( _chngToStrc.disabled[_curGrpCurr] == TRUE ) { 
        XtSetSensitive( _chngToStrc.form,  FALSE );    
    }
    else {
        XtSetSensitive( _chngToStrc.form,  TRUE );    
    }


}

/*=====================================================================*/
/* ARGSUSED */
void pggrpch_curGrpOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggrpch_curGrpOptCb							*
 *									*
 * This is the callback function for current group option menu.		*
 *									*
 * void pggrpch_curGrpOptCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/01   initial coding                          *
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _curGrpCurr = (long)clnt;
    pggrpch_update();

}

/*=====================================================================*/
/* ARGSUSED */
void pggrpch_chngToOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggrpch_chngToOptCb							*
 *									*
 * This is the callback function for "change to" option menu.		*
 *									*
 * void pggrpch_chngToOptCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt	XtPointer	client data			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/01   initial coding                          *
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 * H. Zeng/EAI          05/02   modified to use master group type list  *
 * T. Piper/SAIC	03/05	removed pre_val - not used		*
 ***********************************************************************/
{
int     cur_val, ii, jj, disable_id, grpid, iret;
/*---------------------------------------------------------------------*/

    _chngToStrc.current[_curGrpCurr] = cur_val = (long)clnt;
    switch( _curGrpCurr ) {
       case 0:

	    /*
             * When choosing "ALL GROUPS" for "CURRENT GROUP"
             */
	    if ( cur_val != 0 ) {
     
	       /*
                * If all groups change to a same particular group type,
                * make other group type choices insensitive.
                */
               for (ii = 1; ii < _curGrpNum; ii++) {
                   if( _chngToStrc.exist[ii] == TRUE ) {
    	               XtSetSensitive( _curGrpStrc.pb[ii], FALSE );
                       _chngToStrc.current[ii] = cur_val;
                       if (strcmp(_curGrpStr[ii-1], 
                                  _chngToStr[cur_val-1]) != 0) {
                          _chngToStrc.chng_flag[ii] = TRUE;
                       }
                       else {
                          _chngToStrc.chng_flag[ii] = FALSE;
                       }
		   } /* the end of if( _chngToStrc... */
               }              
            }
            else {

	       /*
                * If choosing "NO CHANGE" for "ALL GROUPS",
                * make other valid group type choices back to sensitive.
                */
               for (ii = 1; ii < _curGrpNum; ii++) {
                   if( _chngToStrc.exist[ii] == TRUE ) {
    	               XtSetSensitive( _curGrpStrc.pb[ii], TRUE );
                       _chngToStrc.current[ii] = 0;                  
                       _chngToStrc.chng_flag[ii] = FALSE;
                   }
                 
               }           

            } /* the end of else */
            break;


       /*
        * When choosing an individual group type for "CURRENT GROUP"
        */
       default:

	    if (cur_val != 0 &&
                strcmp(_curGrpStr[_curGrpCurr-1], 
                       _chngToStr[cur_val-1]) != 0) {
 
	        /*
                 * If an individual group type changes to another group 
                 * type, set the change flag accordingly.
                 */
                _chngToStrc.chng_flag[_curGrpCurr] = TRUE;
            }
            else {
            
	        /*
                 * If an individual group type doesn't change to another
                 * group type, set the change flag  accordingly.
                 */
                _chngToStrc.current[_curGrpCurr] = 0;
                _chngToStrc.chng_flag[_curGrpCurr] = FALSE;
            }
            break;

    } /* the end of switch */


    /*
     * Set "disabled" flags according to the changes above.
     */
    for (ii = 1; ii < _curGrpNum; ii++) {
        _chngToStrc.disabled[ii] = FALSE;
    }
    for (ii = 1; ii < _curGrpNum; ii++) {
        if( _chngToStrc.current[ii] != 0 ) {
            disable_id = ces_gtgmsid ( _chngToStrc.current[ii]-1 );

            for (jj = 1; jj < _curGrpNum; jj++) {
                 ces_gtgid (_curGrpStr[jj-1], &grpid, &iret);
                 if ( grpid == disable_id ) {
                      _chngToStrc.disabled[jj]  = TRUE;
                      _chngToStrc.chng_flag[jj] = FALSE;
                      _chngToStrc.current[jj]   = 0;
                      break;
                 }

            }

        }
    }

    pggrpch_update();

}

/*=====================================================================*/
/* ARGSUSED */
void pggrpch_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggrpch_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of VG Group      *
 * Change window.						        *
 *									*
 * void pggrpch_ctlBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          05/01   initial coding                          *
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 ***********************************************************************/
{ 
int     ii;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Apply */ 
        for (ii = 1; ii < _curGrpNum; ii++) {
           if( _chngToStrc.chng_flag[ii] == TRUE ) {
               pggrpch_chngGrp();
               break; 
           }
        }    
	pggrpch_popdown ();
        pgpalw_setPrevOper ();

	break;

      case 1:	/* Cancel */
	pggrpch_popdown ();
        pgpalw_setPrevOper ();

	break;

    } /* the end of switch */

}

/*=====================================================================*/

void pggrpch_chngGrp ( void )
/************************************************************************
 * pggrpch_chngGrp							*
 *									*
 * Change the group type of the elements according to instructions on   *
 * VG Group Change Window.                                              *
 *									*
 * void pggrpch_chngGrp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/01	initial coding				*
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	change only groups on current layer	*
 * H. Zeng/EAI          03/02   renamed for new nmap_pggrpch file       *
 * H. Zeng/EAI          05/02   modified to use master group type list  *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC         	07/04   add filter param to crg_get		*
 * B. Yin/SAIC          08/04   added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
    int		el_num, new_num, el_loc, extra = 5, dest_grpnum;
    int		ori_grpnum, ier2, elN, new_location, ii, selection, iret;
    int         grpid, cur_layer, el_layer, pl_found, update_crg;
    float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury;
    float       *elX, *elY, inf_bbox[4];
    char	ori_grptyp, dest_grptyp, ori_grpnam[20], dest_grpnam[20];
    Boolean     found;
    VG_DBStruct	el;
    struct convertTblStrc  *convert_tbl, *ptr, *ptr_prev;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    m_llx = 999999.0F;
    m_lly = 999999.0F;
    m_urx = 0.0F;
    m_ury = 0.0F;

    convert_tbl = NULL;
    ptr   = NULL;
    ptr_prev    = NULL;
    update_crg = 0;

    pgundo_newStep();
    cur_layer = pglayer_getCurLayer( );
    for (el_num = 0; el_num < MAX_EDITABLE_ELEMS; el_num++) {
	crg_goffset (el_num, &el_loc, &ier2);
	el_layer = crg_getLayer ( el_loc );

        /*
         * Skip cleared range record or those not on current layer.
         */
	if (el_loc == -1 || el_layer != cur_layer) {
           continue;
        }

 	crg_ggrp (el_num, &ori_grptyp, &ori_grpnum, &ier2);	

	if (ori_grpnum && ori_grptyp != GRPTYP_OTHERS
                       && ori_grptyp != GRPTYP_COMSYM
                       && ori_grptyp != GRPTYP_WATCH
                       && ori_grptyp != GRPTYP_CCF     ) {

            ces_gtgnam((int)ori_grptyp, ori_grpnam, &ier2);
            for( ii = 0; ii < _numCurGrp; ii++ ) {
	       if(strcmp(_curGrpStr[ii], ori_grpnam) == 0) {
                 selection = ii + 1;
                 break;
               }
            }

            if(ii < _numCurGrp && 
               _chngToStrc.chng_flag[selection] == TRUE) {

               strcpy(dest_grpnam, 
                      _chngToStr[ _chngToStrc.current[selection]-1 ]);
               ces_gtgid(dest_grpnam, &grpid, &ier2);
               dest_grptyp = (char)grpid;

               /* Search on conversion table to see if there is entry
                * that has the same ori_grptyp, ori_grpnum and dest_grptyp.
                * If yes, get dest_grpnum from there.
                */
               found = FALSE;
               ptr = convert_tbl;
               while(ptr != NULL) {
		     if(ori_grptyp == ptr->ori_grptyp &&
                        dest_grptyp== ptr->dest_grptyp&&
                        ori_grpnum == ptr->ori_grpnum    ) {

                        dest_grpnum = ptr->dest_grpnum;
                        found = TRUE;
                        break;
                     }
                     ptr = ptr->next;
               }

               /*
                * If not found on conversion table, get next available
                * group number. Add new entry into conversion table.
                */
               if(!found) {
                  crg_ggnxt(dest_grptyp, &dest_grpnum, &ier2);

                  if(convert_tbl == NULL) {
                     convert_tbl = (struct convertTblStrc*)malloc(
                                    sizeof(struct convertTblStrc)  );

                     convert_tbl->ori_grptyp = ori_grptyp;
                     convert_tbl->dest_grptyp= dest_grptyp;
                     convert_tbl->ori_grpnum = ori_grpnum;
                     convert_tbl->dest_grpnum= dest_grpnum;
                     convert_tbl->next       = NULL;
                     convert_tbl->prev       = NULL;
                  }
                  else {
                     ptr = convert_tbl;
		     while(ptr->next != NULL) ptr = ptr->next;
                     ptr->next = (struct convertTblStrc*)malloc(
                                    sizeof(struct convertTblStrc)  );
                     ptr_prev  = ptr;
                     ptr = ptr->next;

                     ptr->ori_grptyp = ori_grptyp;
                     ptr->dest_grptyp= dest_grptyp;
                     ptr->ori_grpnum = ori_grpnum;
                     ptr->dest_grpnum= dest_grpnum;
                     ptr->next       = NULL;
                     ptr->prev       = ptr_prev;

                  }     
               } /* the end of if(!found... */


	       cvg_rdrec (cvg_getworkfile(), el_loc, &el, &ier2);

	       /*
                * Create a copy of the element with new group info,
                */
	       pgactv_setActvElm ( &el, el_loc);
               pgactv_getDevPts (&elN, &elX, &elY);
               pgvgf_saveNewElm(NULL, sys_D, &el, 
                     elN, elX, elY, FALSE, &new_location, &iret);
               cvg_setginf(cvg_getworkfile(), new_location, 
                           dest_grptyp, dest_grpnum, &iret);

               /*
                * Free TCA/GFA memory
                */
               if ( el.hdr.vg_type == TCA_ELM ) {
                  cvg_freeBkpts ( &el );
               }
	       else if ( el.hdr.vg_type == GFA_ELM ) {
                   cvg_freeElPtr ( &el );
               }
               
               cvg_rdrec(cvg_getworkfile(), new_location, &el, &iret);
               crg_set (&el, new_location, cur_layer, &iret);
	       crg_getinx (new_location, &new_num, &iret);
	       crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);

	       if (m_llx > llx)
                   m_llx = llx;
               if (m_lly > lly)
                   m_lly = lly;
               if (m_urx < urx)
                   m_urx = urx;
               if (m_ury < ury)
                   m_ury = ury;

               /*
                * Mark elements in placement that are effected by
                * the new element, and get the area of influence back
                */
	       cvg_checkplace(&el, 0, new_location, &pl_found, inf_bbox, &iret);
               if (pl_found > 0) {
                   /*
                    * Update the refresh extent if the area impacted by
                    * placement is bigger 
                    */
                   m_llx = G_MIN(m_llx, inf_bbox[0]);
                   m_lly = G_MIN(m_lly, inf_bbox[2]);
                   m_urx = G_MAX(m_urx, inf_bbox[1]);
                   m_ury = G_MAX(m_ury, inf_bbox[3]);
                   update_crg = 1;
               }

               /*
                * Free TCA/GFA memory
                */
               if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
               }
	       else if ( el.hdr.vg_type == GFA_ELM ) {
                   cvg_freeElPtr ( &el );
               }
	       
	       pgundo_storeThisLoc(new_location, 
                                UNDO_ADD, &iret);

               /*
                * Mark elements in placement that are effected by
                * the delete, and get the area of influence back
                */
               cvg_rdrec(cvg_getworkfile(), el_loc, &el, &iret);
	       cvg_checkplace(&el, 1, el_loc, &pl_found, inf_bbox, &iret);
               if (pl_found > 0) {
                   /*
                    * Update the refresh extent if the area impacted by
                    * placement is bigger 
                    */
                   m_llx = G_MIN(m_llx, inf_bbox[0]);
                   m_lly = G_MIN(m_lly, inf_bbox[2]);
                   m_urx = G_MAX(m_urx, inf_bbox[1]);
                   m_ury = G_MAX(m_ury, inf_bbox[3]);
                   update_crg = 1;
               }

               /*
                * Free TCA/GFA memory
                */
               if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
               }
	       else if ( el.hdr.vg_type == GFA_ELM ) {
                   cvg_freeElPtr ( &el );
               }

               /* 
                * Mark the original element as deleted.
                */
               cvg_delet(cvg_getworkfile(), el_loc, TRUE, &ier2);
	       crg_get (el_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier2);
	
	       if (m_llx > llx)
                   m_llx = llx;
               if (m_lly > lly)
                   m_lly = lly;
               if (m_urx < urx)
                   m_urx = urx;
               if (m_ury < ury)
                   m_ury = ury;

               crg_clear(el_num, &ier2);
	       pgundo_storeThisLoc (el_loc, UNDO_DEL, &ier2);

            } /* the end of if(ii < _numCurGrp ... ) */

	} /* the end of if (ori_grpnum &&... */ 

    } /* for (el_num = 0 ... */
    pgundo_endStep();

    m_llx -= (float)extra;
    m_lly -= (float)extra;
    m_urx += (float)extra;
    m_ury += (float)extra;
    
    xpgpaste (m_llx, m_lly, m_urx, m_ury, &ier2);
    cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier2); 

    /*
     * If we may have impacted other elements with placement
     * we will need to rebuild the range records
     */
    if (update_crg) {
        crg_rebuild();
    }

    /*
     * Free conversion table
     */
    if(convert_tbl != NULL) {

       ptr = convert_tbl;
       while(ptr->next != NULL) ptr = ptr->next;
       do {
            ptr_prev = ptr->prev;
            free(ptr);
            ptr = ptr_prev;
       }
       while(ptr != NULL);

    }

}

/*=====================================================================*/

#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"


#define MAXNREF      10     /* max number of references */
#define MAXNOPT	     20	    /* max number of option menu items  */


struct optMenuStrc {
    int		current[MAXNREF], deflt[MAXNREF], 
                save[MAXNREF];
    Widget	form;
    Widget	label;
    Widget	menu;
    Widget	pb[MAXNOPT];
};

static	struct	optMenuStrc	_curRefStrc;
static  int     _curRefCurr, _curRefDeflt, _curRefNum;

static	struct	optMenuStrc	_curTypStrc;
static  int     _curTypNum;

static	struct	optMenuStrc	_curColorStrc;
static  int     _curColorNum;
static  char    *_curColorStr[] = {"red",  "black",   "green", "yellow",
                                   "cyan", "magenta", "white" };

static  char		*_curStatus;

static curtypTbl_t*	_curtypTbl;  
static currefTbl_t*	_currefTbl;

static Widget		_cursorwWin;
static WidgetList       _fmtLabel;

/*
 *  private functions
 */
void cursorw_update ( void );
void cursorw_tblUpdate ( void );

/*
 *  private callback functions
 */
void cursorw_ctlBtnCb	   ( Widget, long, XtPointer );
void cursorw_curColorOptCb ( Widget, XtPointer, XtPointer );
void cursorw_curRefOptCb   ( Widget, XtPointer, XtPointer );
void cursorw_curTypOptCb   ( Widget, XtPointer, XtPointer );


/************************************************************************
 * nmap_cursorw.c							*
 *									*
 * This module defines a cursor edit popup window for nmap              *
 *									*
 * CONTENTS:								*
 *	cursorw_create()	create the cursor edit window	        *
 *	cursorw_popup()		pop up the cursor edit window	        *
 *	cursorw_popdown()	popdown the cursor edit window	        * 
 *									*
 *	cursorw_isUp()		query if the window is up 		*
 *									*
 *      cursorw_update()        update the cursor edit window           *
 *      cursorw_tblUpdate()     update the cursor ref. table info.      *
 *                                                                      *
 *      cursorw_curRefOptCb()   callback for cursor ref. option menu    *
 *      cursorw_curTypOptCb()   callback for cursor type option menu    *
 *      cursorw_curColorOptCb() callback for cursor color option menu   *
 *	cursorw_ctlBtnCb()      callback for control buttons            *
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void cursorw_create ( Widget parent ) 
/************************************************************************
 * cursorw_create							*
 *									*
 * This function creates the cursor edit popup window.		        *
 *									*
 * void   cursorw_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/00   initial coding                          *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget	pane, form1, form2, button, formbtn;
    char	*btnstrs[] = {"OK", "Defaults", "Cancel"};
    int		loff = 5, toff2 = 5;
    long	ii, nn;
    /*---------------------------------------------------------------------*/
    /*
     * create dialog shell
     */
    _cursorwWin = XmCreateFormDialog ( parent, "cursorw_popup",
				       NULL, 0);
    XtVaSetValues ( _cursorwWin, 
		    XmNnoResize,        True, 
		    NULL);
    XtVaSetValues ( XtParent(_cursorwWin),
		    XmNtitle, "CURSOR SELECT and EDIT",
		    NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("cursorw_pane",
			    xmPanedWindowWidgetClass, _cursorwWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    /*
     * create cursor reference choosing area.
     */
    form1 = XtVaCreateWidget ( "form1",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     *  create label widgets
     */
    nn = 3;
    _fmtLabel = (WidgetList)XtMalloc(nn*sizeof(Widget));

    /*
     * CURSOR REF.
     */
    _fmtLabel[0] = XtVaCreateManagedWidget ("CURSOR REF.",
				     xmLabelWidgetClass,	form1,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    /*
     * Create cursor reference menu
     */
    _curRefDeflt = 0;
    _curRefCurr  = 0;
    pgutls_createOptionMenu (form1, MAXNOPT, (XtPointer)&_curRefCurr, NULL, 
                             cursorw_curRefOptCb, &_curRefStrc.form, 
                             &_curRefStrc.label,  &_curRefStrc.menu, 
                             _curRefStrc.pb, NULL );
 
    XtVaSetValues (_curRefStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_fmtLabel[0],
                   XmNleftOffset,       loff+5,
		   NULL);


    XtManageChild(form1);

    /*
     * create cursor editing area.
     */
    form2 = XtVaCreateWidget ( "form2",
				xmFormWidgetClass,  pane,
				NULL );

    /*
     * CURSOR TYPE
     */
    _fmtLabel[1] = XtVaCreateManagedWidget ("CURSOR TYPE",
				     xmLabelWidgetClass,	form2,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff2,
                                     XmNleftAttachment,         XmATTACH_FORM,
                                     XmNleftOffset,             loff,
                         	     NULL);

    /*
     * Create cursor type menu
     */
    _curTypStrc.deflt[0] = 0;
    _curTypStrc.current[0] = 0;

    pgutls_createOptionMenu (form2, MAXNOPT, (XtPointer)&_curTypStrc.current[0], NULL, 
                             cursorw_curTypOptCb, &_curTypStrc.form, 
                             &_curTypStrc.label, &_curTypStrc.menu, 
                             _curTypStrc.pb, NULL );
 
    XtVaSetValues (_curTypStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_fmtLabel[1],
                   XmNleftOffset,       loff+2,
		   NULL );

    /*
     * Create cursor color  menu
     */
    _curColorNum = XtNumber(_curColorStr);
    _curColorStrc.deflt[0] = 0;
    _curColorStrc.current[0] = 0;

    pgutls_createOptionMenu (form2, _curColorNum, (XtPointer)&_curColorStrc.current[0], 
                             NULL, cursorw_curColorOptCb, &_curColorStrc.form, 
                             &_curColorStrc.label, &_curColorStrc.menu, 
                             _curColorStrc.pb, _curColorStr );
 
    XtVaSetValues (_curColorStrc.form, 
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        _curTypStrc.form,
		   XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		   XmNleftWidget,	_curTypStrc.form,
		   NULL);

    /*
     * COLOR
     */
    _fmtLabel[2] = XtVaCreateManagedWidget ("COLOR",
			      xmLabelWidgetClass,    form2,
                              XmNtopAttachment,	     XmATTACH_OPPOSITE_WIDGET,
			      XmNtopWidget,	     _curColorStrc.form,
                              XmNtopOffset,          toff2,
                              XmNleftAttachment,     XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,         _fmtLabel[1],
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
			(XtCallbackProc)cursorw_ctlBtnCb, (XtPointer)ii );

    }
    XtManageChild(formbtn);
    XtManageChild(pane);
}

/*=====================================================================*/

void cursorw_popup ( void )
/************************************************************************
 * cursorw_popup				      			*
 *									*
 * This function pops up the cursor edit popup window.		        *
 *									*
 * void cursorw_popup()						        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/00   initial coding                          *
 ***********************************************************************/
{
int       ii, jj, index;
char      cc[20];
XmString  xmstr;
static  Boolean  first = TRUE;
/*---------------------------------------------------------------------*/
    /*
     * Get pointers to cursor reference&type information at
     * the first time.
     */
    if( first ) {
      _curtypTbl = (curtypTbl_t*)NxmCursor_getTypTbl();
      _currefTbl = (currefTbl_t*)NxmCursor_getRefTbl();

      _curTypNum = _curtypTbl->ntyp;
      _curRefNum = _currefTbl->nref;

      /*
       * Initialize the _curStatus values.
       */
      _curStatus = (char *)malloc( _curRefNum * sizeof(char) );
      for (ii = 0; ii < _curRefNum; ii++) {
         _curStatus[ii] = 0;
      } 

      /*
       * Set cursor ref. item strings based on cursor ref. table info.
       */
      for (ii = 0; ii < _curRefNum; ii++) {
	  sprintf (cc, "%s", _currefTbl->currefs[ii].ref_name );
          xmstr = XmStringCreateLocalized (cc);
	  XtVaSetValues(_curRefStrc.pb[ii],
	      XmNlabelString, 		xmstr,
	      NULL);
    	  XtManageChild(_curRefStrc.pb[ii]);
          XmStringFree (xmstr);
      }

      for (ii = _curRefNum; ii < MAXNOPT; ii++) {
    	  XtUnmanageChild( _curRefStrc.pb[ii] );
      }

      /*
       * Set cursor type item strings based on cursor type table info.
       */
      for (ii = 0; ii < _curTypNum; ii++) {
	  sprintf (cc, "%s", _curtypTbl->curtyps[ii].ext_name );
          xmstr = XmStringCreateLocalized (cc);
	  XtVaSetValues(_curTypStrc.pb[ii],
	      XmNlabelString, 		xmstr,
	      NULL);
    	  XtManageChild(_curTypStrc.pb[ii]);
          XmStringFree (xmstr);
      }

      for (ii = _curTypNum; ii < MAXNOPT; ii++) {
    	  XtUnmanageChild( _curTypStrc.pb[ii] );
      }

      /*
       * Get default type&color info. for each reference.
       */
      for (ii = 0; ii < _curRefNum; ii++) {
	  for(jj=0; jj<_curTypNum; jj++) {
             if(strcmp(_currefTbl->currefs[ii].typ_name, 
                       _curtypTbl->curtyps[jj].ext_name )==0) {
                index = jj;
                break;          

             }
          }
          _curTypStrc.deflt[ii] = index;
          _curTypStrc.current[ii] = index;

	  for(jj=0; jj<_curColorNum; jj++) {
             if(strcmp(_currefTbl->currefs[ii].color, 
                       _curColorStr[jj] )==0) {
                index = jj;
                break;          

             }
          }
          _curColorStrc.deflt[ii] = index;
          _curColorStrc.current[ii] = index;

      }
      first = FALSE;
    }

    /*
     * Save current values.
     */
    for(ii = 0; ii< _curRefNum; ii++) {
        _curTypStrc.save[ii] = _curTypStrc.current[ii];
        _curColorStrc.save[ii] = _curColorStrc.current[ii];

    }

    cursorw_update();

    if( !(cursorw_isUp()) ) {
        XtManageChild (_cursorwWin);
    }
}

/*=====================================================================*/

Boolean cursorw_isUp ( void ) 
/************************************************************************
 * cursorw_isUp								*
 *									*
 * This function queries whether the cursor edit window is up.	        *
 *									*
 * Boolean cursorw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * cursorw_isUp	 Boolean       	True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/00	initial coding				*
 ***********************************************************************/
{
	return (XtIsManaged (_cursorwWin));
}

/*=====================================================================*/

void cursorw_popdown ( void ) 
/************************************************************************
 * cursorw_popdown							*
 *									*
 * This function pops down the cursor edit window.		        *
 *									*
 * void cursorw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/00	initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_cursorwWin)) {
	XtUnmanageChild (_cursorwWin);
    }
}

/*=====================================================================*/

void cursorw_update ( void )
/************************************************************************
 * cursorw_update                                                       *
 *                                                                      *
 * This function updates the cursor edit window.  		        *
 *                                                                      *
 * void cursorw_update ()                                     	        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                   NONE                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      05/00   initial coding                              *
 ***********************************************************************/
{
    /*
     * Set the current format values
     */
    XtVaSetValues(_curRefStrc.menu,
                  XmNmenuHistory,    _curRefStrc.pb[_curRefCurr],
                  NULL  );

    XtVaSetValues(_curTypStrc.menu,
                  XmNmenuHistory,    
                  _curTypStrc.pb[ _curTypStrc.current[_curRefCurr] ],
                  NULL  );

    XtVaSetValues(_curColorStrc.menu,
                  XmNmenuHistory,    
                  _curColorStrc.pb[ _curColorStrc.current[_curRefCurr] ],
                  NULL  );
}

/*=====================================================================*/
/* ARGSUSED */
void cursorw_curRefOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * cursorw_curRefOptCb							*
 *									*
 * This is the callback function for cursor ref. option menu.		*
 *									*
 * void cursorw_curRefOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/00   initial coding                          *
 ***********************************************************************/
{
    _curRefCurr = (long)clnt;
    cursorw_update();
}

/*=====================================================================*/
/* ARGSUSED */
void cursorw_curTypOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * cursorw_curTypOptCb							*
 *									*
 * This is the callback function for cursor type option menu.		*
 *									*
 * void cursorw_curTypOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/00   initial coding                          *
 ***********************************************************************/
{
    _curTypStrc.current[_curRefCurr] = (long)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void cursorw_curColorOptCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * cursorw_curColorOptCb						*
 *									*
 * This is the callback function for cursor color option menu.	        *
 *									*
 * void cursorw_curColorOptCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		the widget calling this function*
 *	clnt		XtPointer	which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          05/00   initial coding                          *
 ***********************************************************************/
{
    _curColorStrc.current[_curRefCurr] = (long)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void cursorw_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * cursorw_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of cursor edit   *
 * popup window.						        *
 *									*
 * void cursorw_ctlBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          05/00   initial coding                          *
 ***********************************************************************/
{ 
int     ii;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* OK */
        cursorw_tblUpdate();

        for (ii = 0; ii < _curRefNum; ii++) {
           if(_curStatus[ii]) {
             NxmCursor_createCursor(ii);

             /*
              * Set default cursor immediately.
              */
             if(ii == 0) {
               mcanvw_setCursor(CURS_BUSY);  
               mcanvw_setCursor(CURS_DEFAULT);
             }

             _curStatus[ii] = 0;
           }
        }
     
	cursorw_popdown ();

	break;

      case 1:	/* Defaults */
        _curRefCurr = _curRefDeflt;

        for(ii = 0; ii< _curRefNum; ii++) {
            _curTypStrc.current[ii] = _curTypStrc.deflt[ii];
            _curColorStrc.current[ii] = _curColorStrc.deflt[ii];
        }

	cursorw_update();

	break;

      case 2:	/* Cancel */
        for(ii = 0; ii< _curRefNum; ii++) {
            _curTypStrc.current[ii] = _curTypStrc.save[ii];
            _curColorStrc.current[ii] = _curColorStrc.save[ii];

        }

	cursorw_popdown();

	break;

    } /* the end of switch */
}

/*=====================================================================*/

void cursorw_tblUpdate ( void )
/************************************************************************
 * cursorw_tblUpdate                                                    *
 *                                                                      *
 * This function updates the cursor ref. table info.  		        *
 *                                                                      *
 * void cursorw_tblUpdate()                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                   NONE                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      05/00   initial coding                              *
 ***********************************************************************/
{
int   ii;
/*---------------------------------------------------------------------*/

    for(ii = 0; ii < _curRefNum; ii++) {
      if(strcmp(_currefTbl->currefs[ii].typ_name, 
           _curtypTbl->curtyps[ _curTypStrc.current[ii] ].ext_name)!=0) {

        strcpy(_currefTbl->currefs[ii].typ_name, 
             _curtypTbl->curtyps[ _curTypStrc.current[ii] ].ext_name);
        _curStatus[ii] = 1;

      }

      if(strcmp(_currefTbl->currefs[ii].color, 
	    _curColorStr[ _curColorStrc.current[ii] ])!=0) {

        strcpy(_currefTbl->currefs[ii].color, 
             _curColorStr[ _curColorStrc.current[ii] ]  );
        _curStatus[ii] = 1;

      }

    }
}

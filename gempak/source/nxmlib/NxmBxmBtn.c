#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h" 
#include "NxmInit.h"
#include "proto_xw.h"

#define  TIME_DELAY	200
#define  BTN_OFFSET	0	

static int       _bxmDpth;	
static Drawable  _bxmWin;
static GC	 _bxmGc;
static Screen	 *_bxmScreen;


/* 
 * structure for icon button and its auto text label		 
 */
typedef struct {
Widget  pxm_btn;
char	txt_label[50];
} pop_t;


static Widget	_bttnPopW; 	  /* popup label widget  */
static int	_labelUpFlg;  	  /* label popup flag	 */
static int	_btnPressedFlg = 0;  /* btn pressed flag */
static int	_enableFlg;	  /* popup enable flag	 */
static XtIntervalId  _timeoutId;  /* id of timeout event */


/*
 *  Private functions
 */
static void NxmBxmBtn_btnPressed ( Widget, XtPointer, XtPointer );
static Widget NxmBxmBtn_createBtn ( Widget parent, char button_name[], 
					WidgetClass button_class );
static Pixmap NxmBxmBtn_createBxm ( Widget button, GC bxm_gc, 
					char bxm_data[], unsigned int width, 
					unsigned int height );
static void NxmBxmBtn_createLabel ( Widget );
static void NxmBxmBtn_getGc ( Widget button, char *fg_name, 
						char *bg_name );
static void NxmBxmBtn_initScreen ( Widget );
static int  NxmBxmBtn_isCursorIn ( Widget );
static void NxmBxmBtn_placeLabel ( Widget );
static void NxmBxmBtn_popupLabel ( XtPointer, XtIntervalId );
static void NxmBxmBtn_popdownLabel ( XtPointer, XtIntervalId );
static void NxmBxmBtn_setLabel ( char *pop_label );
static void NxmBxmBtn_startDspyLabel ( Widget, XtPointer, XEvent *event );


/************************************************************************
 * NxmBxmBtn.c								*
 * 									*
 * This module contains functions that create an icon push button.	*
 * Each button may have mutiple sets of bitmap labels, each set 	*
 * consists of sensitive bits and insensitive bits.  The bitmap label	*
 * can be changed individually.  For Motif version 1.2 and above, 	*
 * Text label can be created which can automatically pops up when 	*
 * cursor is placed over the top of the button.  This feature of 	*
 * automatically displaying text label can be enabled/disabled by the	*
 * application.								*
 * 									*
 * CONTENTS:								*
 *									*
 * NxmBxmBtn_create()        create single pixmap labeled button        *
 * NxmBxmBtn_addBxmLabel()   replaces a string label with pixmap	*
 * NxmBxmBtn_createMulti()   create multiple pixmap labeled button	*
 * NxmBxmBtn_changeLabel()   change the content of the text label	*
 * NxmBxmBtn_enableLabel()   enable/disable auto display label		*
 * NxmBxmBtn_setPxm()        assign a pixmap to the label of a button	*
 *									*
 * NxmBxmBtn_setLabel()      set the text label character string	*
 * NxmBxmBtn_createLabel()   create the text label widget		*
 * NxmBxmBtn_popupLabel()    popup the text label			*
 * NxmBxmBtn_popdownLabel()  popdown the text label			*
 * NxmBxmBtn_placeLabel()    set the position of the popup label	*
 * NxmBxmBtn_startDspyLabel()   start time out for text label		*
 *									*
 * NxmBxmBtn_createBtn()     create a pixmap button			*
 * NxmBxmBtn_createBxm()     create a pixmap for  push button label	*
 * NxmBxmBtn_getGc()	     obtain the GC for the pixmap		*
 * NxmBxmBtn_isCursorIn()    check if cursor is inside the pxm button	*
 * NxmBxmBtn_initScreen()    initialize drawables and windows		*
 * NxmBxmBtn_btnPressed()    icon button pressed event handler		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmBxmBtn_create( 
Widget         		parent,
char           		button_name[],
WidgetClass		button_class,
unsigned int	       	width, 
unsigned int	       	height,
char                    *button_fgcolor,
char                    *button_bgcolor,
char  			insensitive_bits[], 
char			sensitive_bits[],
char			*text_label,
Boolean     		press_flag,
XtCallbackProc		callback,
XtPointer      		callback_data ) 
/************************************************************************
 * NxmBxmBtn_create							*
 *									*
 * This function creates an icon push button that has two colors. The	*
 * bitmap bits can be either data or file name. The insensitive_bits	*
 * parameter can be set to NULL if the button will never be made	*
 * insensitive. Text label will not pop up if a null string is passed	*
 * to text_label.							*
 *									*
 * Note: if text_label is not NULL, it should be defined as static	*
 *	 in the application.						*
 *									*
 * Widget NxmBxmBtn_create( parent, button_name, button_class, width, 	*
 *	height, button_fgcolor,	button_bgcolor, insensitive_bits, 	*
 *	sensitive_bits, text_label, callback, callback_data )		*
 *									*
 * Input parameters:							*
 *  parent           Widget        parent widget ID                     *
 *  button_name[]    char	   name of the pixmap button		*
 *  button_class     WidgetClass   button widget class			*
 *  width      	     unsigned int  width of the button                  *
 *  height           unsigned int  height of the button                 *
 *  *button_fgcolor   char         button foreground color name         *
 *  *button_bgcolor   char         button background color name         *
 *  insensitive_bits[] char	   the bitmap data or file of the 	*
 *					button when it's status is 	*
 *					insensitive      		*
 *  sensitive_bits[] char	   the bitmap data or file of the 	*
 *					button when it's status is 	*
 *					sensitive        		*
 *  *text_label	     char	   text label character string		*
 *  *callback()      void          callback function                    *
 *  press_flag	     Boolean	   type of action for button		*
 *				        True = button press event	*
 *					False = activate callback	*
 *  callback_data    XtPointer     data passed into the callback        *
 *                                      function                        *
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 * Return parameters:							*
 *			The widget ID of the button			*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		05/94	original interface			*
 * S. Wang/GSC		09/96	rewrite contents			*
 * C. Lin/EAI		10/96	take out the call to NxmBxmBtn_setPxm	*
 * S. Wang/GSC		04/97	add auto text label and re-organize	*
 * S. Law/GSC		03/99	changed callback from Release -> Press	*
 * R. Tian/SAIC		10/02	changed callback from Press -> Click	*
 * R. tian/SAIC		01/03	added press_flag			*
 ***********************************************************************/
{
    Widget	button_id;
    Pixmap	button_map, insensitive_map;

/*---------------------------------------------------------------------*/

    NxmBxmBtn_initScreen(parent);

/*
 * create the push button
 */
    button_id = NxmBxmBtn_createBtn(parent,button_name,button_class);

/*
 * create gc
 */
    NxmBxmBtn_getGc( button_id, button_fgcolor, button_bgcolor );

/*
 * create pixmap for insensitive button
 */

    if ( insensitive_bits ) {
	insensitive_map = 
	    NxmBxmBtn_createBxm( button_id, _bxmGc, 
				insensitive_bits, width, height );

	XtVaSetValues( button_id,
		      XmNlabelType,     XmPIXMAP,
		      XmNlabelInsensitivePixmap,
		      insensitive_map,
		      NULL );
    }

/*
 * create pixmap for sensitive button
 */

    if ( sensitive_bits  ) { 
	button_map = NxmBxmBtn_createBxm(button_id, _bxmGc, 
					 sensitive_bits, width, height);

	XtVaSetValues( button_id,
		      XmNlabelType,     XmPIXMAP,
		      XmNlabelPixmap,   button_map,
		      NULL );
    }

    if (text_label != NULL ) {
	XtAddEventHandler(button_id, EnterWindowMask |
			  LeaveWindowMask, FALSE, 
			  (XtEventHandler)NxmBxmBtn_startDspyLabel, 
			  text_label);
    }

/*
 * create popup label if not created
 */
    if ( _bttnPopW == (Widget)NULL )
	NxmBxmBtn_createLabel(parent);

    XtAddEventHandler( button_id, ButtonPressMask, FALSE, 
		      (XtEventHandler)NxmBxmBtn_btnPressed, NULL );

    if ( callback ) {
        if( press_flag ) {
	    XtAddEventHandler( button_id, ButtonPressMask, FALSE,
                               (XtEventHandler)callback, callback_data );
        }
        else {
	    XtAddCallback( button_id, XmNactivateCallback, 
                           (XtCallbackProc)callback, callback_data );
        }
    }

    return( button_id );

}

/*=====================================================================*/

void NxmBxmBtn_addBxmLabel( 
Widget 			button_id, 
unsigned int 		width, 
unsigned int	       	height,
char                    *button_fgcolor,
char                    *button_bgcolor,
char			iconfile[],
char			*text_label )
/************************************************************************
 * NxmBxmBtn_addBxmLabel						*
 *									*
 * This function replaces a text string label with a pixmap. 		*
 *									*
 * NxmBxmBtn_addBxmLabel ( button_id, width, height, button_fgcolor,	*
 *				button_bgcolor, iconfile, text_label)	*
 *									*
 * Input parameters:							*
 *  button_id        Widget        button widget ID                     *
 *  width      	     unsigned int  width of the  pixmap                 *
 *  height           unsigned int  height of the pixmap                 *
 *  button_fgcolor   char*         button foreground color name         *
 *  button_bgcolor   char*         button background color name         *
 *  iconfile[]       char	   the bitmap file 			*
 *  *text_label	     char	   text label character string		*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 *									*
 * W.Li/EAI		04/99						*
 ***********************************************************************/
{
    Pixmap	button_map;
/*---------------------------------------------------------------------*/
/*
 * create gc
 */
    NxmBxmBtn_getGc( button_id, button_fgcolor, button_bgcolor );

/*
 * create pixmap label for button
 */
    button_map = NxmBxmBtn_createBxm(button_id, _bxmGc, 
					 iconfile, width, height);
    XtVaSetValues( button_id,
		  XmNlabelType,		XmPIXMAP,
		  XmNlabelPixmap,	button_map,
		  NULL );
   
    if (text_label != NULL ) {
	XtAddEventHandler(button_id, EnterWindowMask |
			  LeaveWindowMask, FALSE, 
			  (XtEventHandler)NxmBxmBtn_startDspyLabel, 
			  text_label);
    }

/*
 * create popup label if not created
 */
    if ( _bttnPopW == (Widget)NULL )
	NxmBxmBtn_createLabel(button_id);

    XtAddEventHandler( button_id, ButtonPressMask, FALSE, 
		      (XtEventHandler)NxmBxmBtn_btnPressed, NULL );

}

/*=====================================================================*/

Widget NxmBxmBtn_createMulti( 
Widget        	parent,
char           	*button_name,
WidgetClass	button_class,
unsigned int    width, 
unsigned int	height,
struct bxmInfo	*bxm_info,
int		n_set,
char		*text_label,
Boolean         press_flag,
XtCallbackProc  callback,
XtPointer      	callback_data,
struct pxmBuf 	*pxm_buffer )
/************************************************************************
 * NxmBxmBtn_createMulti                                                *
 *                                                                      *
 * This function creates an icon push button and a set of pixmaps	*
 * that can be assigned to the label of the button. The default		*
 * button label	is the first set of pixmap. The bitmap bits can be 	*
 * either data or file name. The insensitive_bits parameter can be	*
 * set to NULL if the button will never be set to insensitive. Text 	*
 * label will not pop up if a null string is passed to text_label.	*
 * 									*
 * Note: if text_label is not NULL, it should be defined as static	*
 *	 in the application.						*
 *                                                                      *
 * Widget NxmBxmBtn_createMulti(parent, button_name, button_class, 	*
 * 		width, height,bxm_info, n_set, text_label, callback,	*
 *		callback_data, pxm_buffer )  				*
 *                                                                      *
 * Input parameters:                                                    *
 *   parent		Widget        parent widget ID                  *
 *   *button_name	char          name of the bitmap button         *
 *   button_class	WidgetClass   button widget class		*
 *   width		unsigned int  width of the button    		*
 *   height		unsigned int  height of the button              *
 *   *bxm_info		struct bxmInfo pointer to pixmap infos		*
 *   n_set		int		number of set of pixmaps	*
 *   *text_label	char		text label			*
 *   press_flag         Boolean       type of action for button         *
 *                                      True = button press event       *
 *                                      False = activate callback       *
 *   *callback()	void          callback function                 *
 *   callback_data	XtPointer     data passed into the callback     *
 *                                      function                        *
 * Output parameters:                                                   *
 *   *pxm_buffer	struct pxmBuf   pointer to pixmaps buffer	*
 *                                                                      *
 * Return parameters:                                                   *
 *   NxmBxmBtn_createMulti    Widget     The widget ID of the button    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * S. Wang/GSC      	09/96 			                        *
 * C. Lin/EAI       	10/96   take out the call to NxmBxmBtn_setPxm   *
 * S. Wang/GSC	    	04/97	add auto text label and re-organize	*
 * E. Safford/GSC	04/99	add insensitive pixmap to the button    *
 * E. Safford/GSC	09/00	link callback to button press event     *
 * R. Tian/SAIC		10/02	changed callback from Press -> Click	*
 * R. tian/SAIC         01/03   added press_flag                        *
 ***********************************************************************/
{
int		i;
Widget          button_id;
struct  bxmInfo *bxm_pt;
struct  pxmBuf 	*pxm_pt;

/*---------------------------------------------------------------------*/
         
	NxmBxmBtn_initScreen(parent);

/*
 * create the push button
 */
	button_id = NxmBxmBtn_createBtn(parent,button_name,button_class);

	bxm_pt    = bxm_info;
	pxm_pt    = pxm_buffer;

/*
 * create multiple sets of pixmaps for the button
 */
	for ( i=0 ; i<n_set ; i++, pxm_pt++, bxm_pt++ ) {
	    NxmBxmBtn_getGc(button_id,bxm_pt->fgcolor,bxm_pt->bgcolor);

/*
 * create pixmap for insensitive button
 */

	    if ( bxm_info[i].insens_bits )  {
	    	pxm_pt->insnstv = NxmBxmBtn_createBxm(button_id, _bxmGc, 
				bxm_pt->insens_bits, width, height );

		XtVaSetValues (button_id, 
			XmNlabelType,			XmPIXMAP,
			XmNlabelInsensitivePixmap, 	pxm_pt->insnstv,
			NULL);
  	    }

/*
 * create pixmap for sensitive button
 */

	    if ( bxm_info[i].sens_bits  ) 
	    	 pxm_pt->snstv = NxmBxmBtn_createBxm(button_id, _bxmGc, 
				bxm_pt->sens_bits, width, height );
	}	

/*
 * assign the first set of sensitive pixmap to button
 */
	if ( pxm_buffer->snstv ) 
        	XtVaSetValues( button_id,
                               XmNlabelType,     XmPIXMAP,
                               XmNlabelPixmap,   pxm_buffer->snstv,
                               NULL );

	if (text_label != NULL )
        	XtAddEventHandler(button_id, EnterWindowMask |
			LeaveWindowMask, FALSE, 
			(XtEventHandler)NxmBxmBtn_startDspyLabel, 
			text_label);

	if ( _bttnPopW == (Widget)NULL )
		NxmBxmBtn_createLabel(parent);

       	XtAddEventHandler( button_id, ButtonPressMask, FALSE, 
		(XtEventHandler)NxmBxmBtn_btnPressed, NULL );

	if ( callback ) {
            if( press_flag ) {
                XtAddEventHandler( button_id, ButtonPressMask, FALSE,
                                   (XtEventHandler)callback, callback_data );
            }
            else {
                XtAddCallback( button_id, XmNactivateCallback,
                               (XtCallbackProc)callback, callback_data );
            }
	}

	return(button_id);

}	

/*=====================================================================*/

void NxmBxmBtn_changeLabel ( char *txt_label, char label_string[50] )
 /***********************************************************************
 * NxmBxmBtn_changeLabel                                               	*
 *                                                                      *
 * This function change the label text string content			*
 * 									*
 * NxmBxmBtn_changeLabel ( txt_label, label_string )			*
 *									*
 * Input parameters:                                                    *
 *   *txt_label	    char	pointer to label text string		*
 *   label_string   char[]	content of new text			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      05/97 			                        *
 ***********************************************************************/
{
    strcpy(txt_label, label_string);
}

/*=====================================================================*/

void NxmBxmBtn_enableLabel ( int flag )
 /***********************************************************************
 * NxmBxmBtn_enableLabel                                               	*
 *                                                                      *
 * This function enables/disables the auto text label display.		*
 * 									*
 * NxmBxmBtn_enableLabel ( flag )					*
 *									*
 * Input parameters:                                                    *
 *   flag	int		=1 enable label,  =0 disable label	*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      04/97 			                        *
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
    _enableFlg = flag;
}

/*=====================================================================*/

void NxmBxmBtn_setPxm ( Widget button_id, Pixmap sensitive_map, 
						Pixmap insensitive_map ) 
 /***********************************************************************
 * NxmBxmBtn_setPxm                                               	*
 *                                                                      *
 * This function assign a set of pixmap to a push button		*
 * 									*
 * void NxmBxmBtn_setPxm( button_id, sensitive_map, insensitive_map ) 	*
 *									*
 * Input parameters:                                                    *
 *									*
 *   button_id        Widget       id of the push button                *
 *   sensitive_map    Pixmap	   pointer to pixmap for sensitive bits *
 *   insensitive_map  Pixmap	   pointer to pixmap for insensitive    *
 *				   bits 				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      09/96 			                        *
 * C. Lin/EAI       02/97	add XmUpdateDisplay() 			*
 * S. Wang/GSC	    04/97	change name and move to nxm library	*
 * S. Jacobs/NCEP   12/98	Fixed cast of NULL to Pixmap for LINUX	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	if ( sensitive_map != (Pixmap)NULL )
            XtVaSetValues( button_id,
                	XmNlabelType,     XmPIXMAP,
                	XmNlabelPixmap,   sensitive_map,
                	NULL );

	if ( insensitive_map != (Pixmap)NULL ) 
                XtVaSetValues( button_id, 
                    	XmNlabelType,     XmPIXMAP,
		    	XmNlabelInsensitivePixmap,  
		    			  insensitive_map,
		    	NULL );

	XmUpdateDisplay( button_id );
}

/*=====================================================================*/

static void NxmBxmBtn_setLabel ( char *pop_label )
 /***********************************************************************
 * NxmBxmBtn_setLabel                                               	*
 *                                                                      *
 * This function set the text label character string			*
 * 									*
 * static void NxmBxmBtn_setLabel(pop_label)				*
 *									*
 * Input parameters:                                                    *
 *   *pop_label	    char       text label string			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      	04/97 			                        *
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * E. Safford/SAIC	12/01	remove malloc on child_widget      	*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
XmString	new_string;
WidgetList	child_widget;

/*---------------------------------------------------------------------*/

    if ( _bttnPopW == NULL )
	return;

    new_string = XmStringCreateLocalized(pop_label);

    XtVaGetValues( _bttnPopW,
                   XmNchildren,     &child_widget,
                   NULL );
    XtVaSetValues( child_widget[0],
                   XmNlabelString,  new_string,
                   NULL );

    XmStringFree(new_string);
}

/*=====================================================================*/

static void NxmBxmBtn_createLabel ( Widget parent )
 /***********************************************************************
 * NxmBxmBtn_createLabel	   	                               	*
 *                                                                      *
 * This function creates the text label popup widget.  Note that	*
 * each application will use the same text label widget to display	*
 * text labels for all icon buttons.					*
 * 									*
 * NxmBxmBtn_createLabel(parent)					*
 *									*
 * Input parameters:                                                    *
 *	parent	Widget		id of parent widget			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	        04/97						*
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 * T. Piper/SAIC	06/03	removed initialization of _enableFlg	*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
XmString	xm_text;
Pixel    	color_temp;
int		ier;

/*--------------------------------------------------------------------*/
/*
 * return if the popup label widget has been created
 */
	if ( _bttnPopW != NULL )
		return;

	xsncolr("white", &color_temp, &ier);

	xm_text = XmStringCreateLocalized("popup_label");

/*
 * create popup widget
 */
	_bttnPopW = XmVaCreateSimplePopupMenu( parent,
			"txt_label", NULL, XmVaPUSHBUTTON, xm_text, 
			NULL, NULL, NULL, NULL );

/*
 * set the text string and background color
 */
        XtVaSetValues(  _bttnPopW,
                        XmNbackground,  	color_temp,
			XmNshadowThickness,	1,
                        NULL  );

	XmStringFree(xm_text);

}

/*=====================================================================*/
/* ARGSUSED */
static void NxmBxmBtn_popupLabel ( XtPointer clnt, XtIntervalId id )
 /***********************************************************************
 * NxmBxmBtn_popupLabel                                       		*
 *                                                                      *
 * This function pops up the text label					*
 * 									*
 * static void NxmBxmBtn_popupLabel( clnt, id )				*
 *									*
 * Input parameters:                                                    *
 *	clnt	XtPointer	client data				*
 *	id	XtIntervalId	pointer to timeout event id		*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      04/97 			                        *
 * E. Safford/SAIC	12/01	make function static (limit scope)	*
 ***********************************************************************/
{
pop_t	    *pop_data;

/*---------------------------------------------------------------------*/

	pop_data = (pop_t *)clnt;

/*
 * do not pop up the label if icon button is currently being 
 * pressed down
 */
	if ( _btnPressedFlg == 1 ) { 
	    XtRemoveTimeOut(_timeoutId); 
	    _btnPressedFlg = 0;
	    return;
	}

/*
 * do not pop up if icon button is already poped up or if cursor
 * is not on top of the icon button
 */

	if ( _labelUpFlg == 1 || 
			!NxmBxmBtn_isCursorIn(pop_data->pxm_btn) ) {
	    XtRemoveTimeOut(_timeoutId);
	    return;
	}

	if ( !XtIsManaged(_bttnPopW) ) {

/*
 * position labe at the right place
 */
	    NxmBxmBtn_placeLabel(pop_data->pxm_btn);

/* 
 * set the label text string
 */
	    NxmBxmBtn_setLabel(pop_data->txt_label);	

/*
 * popup the text label widget
 */
	    XtManageChild(_bttnPopW);

	    _labelUpFlg = 1;
	    XtRemoveTimeOut(_timeoutId);
	}
}

/*=====================================================================*/
/* ARGSUSED */
static void NxmBxmBtn_popdownLabel ( XtPointer clnt, XtIntervalId id )
 /***********************************************************************
 * NxmBxmBtn_popdownLabel                                     		*
 *                                                                      *
 * This function pops down the text label				*
 * 									*
 * void NxmBxmBtn_popdownLabel( clnt, id )				*
 *									*
 * Input parameters:                                                    *
 *   clnt	XtPointer	client data				*
 *   id		XtIntervalId	timeout event id			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      04/97 			                        *
 * E. Safford/SAIC	12/01	make function static (limit scope)	*
 ***********************************************************************/
{
pop_t	    *pop_data;

/*---------------------------------------------------------------------*/

	pop_data = (pop_t *)clnt;

/*
 * reset button presssed flag if it has been set
 */
	if ( _btnPressedFlg == 1 ) { 
	    XtRemoveTimeOut(_timeoutId); 
	    _btnPressedFlg = 0;
	    return;
	}

/*
 * no need to pop down if text label is not up
 */
	if ( _labelUpFlg == 0 ) {
	    XtRemoveTimeOut(_timeoutId);
	    return;
	}

/*
 * pop down the label if label is up and cursor has moved
 * outside icon button
 */
	if ( !NxmBxmBtn_isCursorIn(pop_data->pxm_btn) &&
                          	XtIsManaged(_bttnPopW) ) {
		XtUnmanageChild(_bttnPopW);
		_labelUpFlg = 0;
	}
	XtRemoveTimeOut(_timeoutId);
}

/*=====================================================================*/

static void NxmBxmBtn_placeLabel ( Widget button_id )
/************************************************************************
 * NxmBxmBtn_placeLabel                                                 *
 *                                                                      *
 * This function positions the text label. Normally, the label is	*
 * placed under the icon button. If there is not enough space on 	*
 * the screen, place the label above the icon button.			*
 *                                                                      *
 * static void NxmBxmBtn_placeLabel(button_id)                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  button_id	Widget		widget id of the icon button		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      04/97                                               *
 * E. Safford/SAIC	12/01	make function static (limit scope)	*
 ***********************************************************************/
{
Dimension   width, height;
Position    root_x, root_y;
int	    temp1, temp2;

/*---------------------------------------------------------------------*/
/*
 * get icon button's position and size
 */
	XtVaGetValues( button_id,
		XmNwidth,  &width,
		XmNheight, &height,
		NULL);
	XtTranslateCoords( button_id, 0, 0, &root_x, &root_y );
	
/*
 * position popup label
 */ 
	temp1 = root_x+width/2;
	if ( (temp1+100) < XWidthOfScreen(_bxmScreen) )
	    XtVaSetValues( _bttnPopW,
		XmNx, 	temp1, 
		NULL);
	else
	    XtVaSetValues( _bttnPopW,
		XmNx, 	(root_x-30), 
		NULL);

	temp2 = root_y+height;
	if ( (temp2+30) < XHeightOfScreen(_bxmScreen) )
	    XtVaSetValues( _bttnPopW,
		XmNy, 	(temp2+3), 
		NULL);
	else
	    XtVaSetValues( _bttnPopW,
		XmNy, 	(root_y-30), 
		NULL);

}

/*=====================================================================*/

static void NxmBxmBtn_startDspyLabel ( Widget wdgt, XtPointer clnt, 
							XEvent *event )
 /***********************************************************************
 * NxmBxmBtn_startDspyLabel	                                       	*
 *                                                                      *
 * This is the handler of cursor entering and leaving the icon 		*
 * button events.							*
 * 									*
 * static void NxmBxmBtn_startDspyLabel(wdgt, clnt, event)		*
 *									*
 * Input parameters:                                                    *
 *									*
 *   wdgt	Widget    id of the push button				*
 *   clnt	XtPointer   client data					*
 *   *event	XEvent    event structure				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *               	NONE				                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	        04/97						*
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * E. Safford/SAIC	12/01	make function static (limit scope)	*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
static	pop_t	pop_data;
char		*text;

/*---------------------------------------------------------------------*/


	text = (char *)clnt;

/*
 * do nothing if label display is disabled 
 * or label widget not created yet or 
 * label content is empty
 */
	if ( _enableFlg == 0 || _bttnPopW == NULL || text == NULL )
		return;

	pop_data.pxm_btn = wdgt;
	strcpy(pop_data.txt_label, text);
	
/*
 * add a timeout when cursor enters the icon button
 */	
        if ( event->type == EnterNotify ) { 
	    _timeoutId = XtAppAddTimeOut( NXMapp, TIME_DELAY, 
		(XtTimerCallbackProc)NxmBxmBtn_popupLabel, &pop_data );
	}
/*
 * add a timeout when cursor leaves the icon button
 */	
	else if ( event->type == LeaveNotify ) {
            _timeoutId = XtAppAddTimeOut( NXMapp, TIME_DELAY, 
		(XtTimerCallbackProc)NxmBxmBtn_popdownLabel, &pop_data );
	}
}

/*=====================================================================*/

static Widget NxmBxmBtn_createBtn ( Widget parent, char button_name[], 
						WidgetClass button_class )
/************************************************************************
 * NxmBxmBtn_createBtn                                                  *
 *                                                                      *
 * This function creates a push button widget                           *
 *                                                                      *
 * static Widget NxmBxmBtn_createBtn(parent, button_name, button_class) *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent           Widget        parent widget ID                     *
 *  button_name[]    char          name of the bitmap button            *
 *  button_class     WidgetClass   button widget class                  *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *           widget id of the push button                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      09/96                                               *
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 ***********************************************************************/
{
Widget  widget;

/*---------------------------------------------------------------------*/


        if ( button_class == NULL )
            widget = XtVaCreateManagedWidget( button_name,
                        xmPushButtonWidgetClass,       parent,
                        NULL);
        else
            widget = XtVaCreateManagedWidget( button_name,
                        button_class,                  parent,
                        NULL);
	
        return( widget );

}

/*=====================================================================*/

static Pixmap NxmBxmBtn_createBxm ( Widget button, GC bxm_gc, 
				    char bxm_data[], unsigned int width, 
						    unsigned int height )
 /***********************************************************************
 * NxmBxmBtn_createBxm	   	                                       	*
 *                                                                      *
 * This function creates a pixmap for a push button			*
 * 									*
 * Pixmap NxmBxmBtn_createBxm(button, bxm_gc, bxm_data, width, height)	*
 *									*
 * Input parameters:                                                    *
 *									*
 *  button           Widget    	   id of the push button                *
 *  bxm_gc	     GC		   gc for the pixmap			*
 *  bxm_data[]	     char	   file name or data of the pixmap	*
 *  width      	     unsigned int  width of the button                  *
 *  height           unsigned int  height of the button                 *
 *									*
 * Output parameters:                                                   *
 *			NONE						*
 *                                                                      *
 * Return parameters:                                                   *
 *               Pixmap       pointer to the pixmap	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	    05/94 			                        *
 * S. Wang/GSC      09/96 	regroup and restructure                 *
 * S. Wang/GSC	    04/97	change name and move to nxm library	*
 * G. Krueger/EAI   09/97	Changed NxmWarning -> NxmWarn_show	*
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 ***********************************************************************/
{
Pixmap	bt_bitmap;
Pixmap	bt_pixmap;
char	warningstring[100];

/*---------------------------------------------------------------------*/

	if ( (bxm_data[0] != '\0') && 
				(access(bxm_data, F_OK) == 0) ) {
 	    if ( XReadBitmapFile(XtDisplay(button), 
			     _bxmWin, 
			     bxm_data,	
			     &width, 
			     &height, 
			     &bt_bitmap, 
			     NULL, NULL) != BitmapSuccess ) {
	   	sprintf(warningstring, "Cannot open bitmap file %s", 
							bxm_data);
		NxmWarn_show(button, warningstring); 
	    }
	}
	else {
	   	bt_bitmap = XCreateBitmapFromData( XtDisplay(button), 
			_bxmWin, (char *)bxm_data, width, height);
	}
	bt_pixmap = XCreatePixmap( XtDisplay(button),
       		         _bxmWin, width, height, (Cardinal)_bxmDpth );

        XCopyPlane( XtDisplay(button), bt_bitmap, bt_pixmap, bxm_gc, 
			0, 0, width, height, 0, 0, 1);
	
	return ( bt_pixmap );

}

/*=====================================================================*/

static void NxmBxmBtn_getGc ( Widget button, char *fg_name, char *bg_name )
 /***********************************************************************
 * NxmBxmBtn_getGc		                                       	*
 *                                                                      *
 * This function creates or changes the gc for the icon button's	*
 * pixmap label								*
 * 									*
 * void NxmBxmBtn_getGc(button, fg_name, bg_name)		        *
 *									*
 * Input parameters:                                                    *
 *  button           Widget    	   id of the push button                *
 *  *fg_name	     char 	   foreground color name                *
 *  *bg_name	     char 	   background color name                *
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      09/96 			                        *
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 * T. Piper/SAIC        05/03   replaced XAllocNamedColor w/xsncolr     *
 ***********************************************************************/
{
Pixel	  ba_ground, fo_ground;
XGCValues gc_values;
int	  ier;

/*---------------------------------------------------------------------*/

	XtVaGetValues( button, 
		XmNbackground, 	   &ba_ground, 
		XmNforeground, 	   &fo_ground, 
		NULL );
	
	if ( fg_name ) {
		xsncolr(fg_name, &gc_values.foreground, &ier);
	}
	else
		gc_values.foreground = fo_ground;	

	if ( bg_name ) {
		xsncolr(bg_name, &gc_values.background, &ier);
	}
	else
		gc_values.background = ba_ground;	

	if ( _bxmGc == NULL )
		_bxmGc = XCreateGC( XtDisplay(button), _bxmWin, 
				GCBackground|GCForeground, &gc_values );
	else
		XChangeGC( XtDisplay(button), _bxmGc,
			GCBackground|GCForeground, &gc_values );         

}

/*=====================================================================*/

static int NxmBxmBtn_isCursorIn ( Widget pxm_btn_id )
/************************************************************************
 * NxmBxmBtn_isCursorIn                                                 *
 *                                                                      *
 * This function checks if cursor is inside the icon button             *
 *                                                                      *
 * static int NxmBxmBtn_isCursorIn(pxm_btn_id)                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  pxm_btn_id       Widget    icon button widget id                    *
 *                                                                      *
 * Output parameters:                                                   *
 *           NONE                                            		*
 *                                                                      *
 * Return parameters:                                                   *
 *           =1 cursor inside pxm button, =0 outside                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      04/97                                               *
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 ***********************************************************************/
{
int	    flag;
Dimension   width, height;
int    	    root_x, root_y;
int    	    win_x, win_y;
unsigned    int	mask;
Window	    root, child;

/*---------------------------------------------------------------------*/

	flag = 0;	
/*
 * get icon button size
 */
	XtVaGetValues( pxm_btn_id,
			XmNwidth,  &width,
			XmNheight, &height,
			NULL);

/*
 * get the current cursor position
 */
	XQueryPointer( XtDisplay(pxm_btn_id), XtWindow(pxm_btn_id), &root, 
		&child, &root_x, &root_y, &win_x, &win_y, &mask );
/*
 * set the flag indicating whether cursor is inside the icon button
 */
	if ( ( win_x >= BTN_OFFSET && win_x <= (width-BTN_OFFSET) && 
		win_y >= BTN_OFFSET && win_y <= (height-BTN_OFFSET) ) ) 
	    flag = 1;
	else
	    flag = 0;
	return (flag); 
}

/*=====================================================================*/

static void NxmBxmBtn_initScreen ( Widget parent )
/************************************************************************
 * NxmBxmBtn_initScreen				                        *
 *                                                                      *
 * This function initialize drawables and windows			*
 *                                                                      *
 * static void NxmBxmBtn_initScreen(parent)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent           Widget        parent widget ID                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      	04/97                                           *
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 ***********************************************************************/
{
/*
 * initialize display and color map
 */
	if ( !NXMisInitialized )
		NxmInitialize( parent );

	if ( _bxmDpth == (int)NULL )
            _bxmDpth   = DefaultDepth( XtDisplay(parent), 
				DefaultScreen((XtPointer)XtDisplay(parent)) );
	if ( _bxmWin == (Drawable)NULL ) {
	    _bxmScreen   = XtScreen(parent);
            _bxmWin = RootWindowOfScreen( _bxmScreen );
	}

}

/*=====================================================================*/
/* ARGSUSED */
static void NxmBxmBtn_btnPressed ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * NxmBxmBtn_btnPressed				                        *
 *                                                                      *
 * This is the button press event handler				*
 *                                                                      *
 * NxmBxmBtn_btnPressed ( wdgt, clnt, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt          Widget     	widget ID                               *
 *  clnt          XtPointer  	not used                                *
 *  call          XtPointer	callback structure  			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      	04/97                                           *
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * E. Safford/SAIC	12/01	make static (limit scope to this module)*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
    _btnPressedFlg = 1;
    XtUnmanageChild(_bttnPopW);
    _labelUpFlg = 0;
}

#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

extern _NXManimationDwell NXManimationDwell;

#define DWELLBTN 	5
#define MAXDWELLRATE	3000
#define DWELL_TABLE    "loop_speed.tbl"

static Widget _dwellW;
WidgetList	_button;		/*  Dwell buttons	*/
static Widget _dwellValueText[3];    /*  text input field widget */
static Widget _dwellScale[3];        /*  scale widget */
static float  _dwellScaleRate[DWELLBTN][3]; /* easy buttons */
static int    _curDwellRate[3];

/*
 *  Private functions
 */
Widget  NxmDwell_buttonsCreate ( Widget parent, 
				 void (*callback)(Widget, long, XtPointer) );
void    NxmDwell_buttonSelectCb ( Widget, long which, XtPointer );
void    NxmDwell_popDown ( Widget, XtPointer, XtPointer );
void    NxmDwell_readTable ( void );
void    NxmDwell_scaleCb ( Widget, long which, XmScaleCallbackStruct *cbs );
Widget  NxmDwell_scalesCreate ( Widget parent );
Widget  NxmDwell_scaleSingleCreate ( Widget parent, long which );
void    NxmDwell_setDefault ( void );
void    NxmDwell_setValue ( int which, int value );
void    NxmDwell_textValueCb ( Widget, long which, XtPointer );


/************************************************************************
 * NxmDwell_popup.c                                                     *
 *                                                                      *
 * This module defines the dwell rate setting popup window.             *
 *                                                                      *
 * CONTENTS:                                                            *
 *      NxmDwell_popupCreate()  creates the dwell rate editing popup.   *
 *      NxmDwell_scalesCreate() creates the dwell scales.               *
 *      NxmDwell_scaleSingleCreate() creates one dwell scale/text. 	*
 *      NxmDwell_buttonsCreate() creates easy dwell rate buttons. 	*
 *	NxmDwell_popDown()	 pops down the dwell setting window	*
 *	NxmDwell_getDwellPtr()	 get current dwell rate array pointer	*
 *									*
 *      NxmDwell_textValueCb()  callback function for text field input. *
 *      NxmDwell_scaleCb()      callback function for dwell rate scale. *
 *      NxmDwell_buttonSelectCb() callback function for easy buttons. 	*
 *									*
 *      NxmDwell_setValue()      set dwell rates. 			*
 *	NxmDwell_setDefault()	 set default value of dwell rate.	*
 *      NxmDwell_readTable()     read the dwell table. 			*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmDwell_popupCreate ( Widget parent, char *dialog_name )
/************************************************************************
 * NxmDwell_popupCreate                                                 *
 *                                                                      *
 * This function creates a dwell-rate setting popup window. There are   *
 * three scale widgets associated with three text widgets for setting   *
 * up the dwell rate of first, intermediate, and last frame in the loop,*
 * separately. When the user changes the dwell rate, he/she can either  *
 * slide the scale widget or type the dwell rate into the text widget.  *
 * In this popup widget, pushbuttons buttons are also created for       *
 * those frequently used dwell rate setup.                              *
 *                                                                      *
 * Widget NxmDwell_popupCreate(parent, dialog_name)			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent          Widget     parent widget ID                         *
 *  dialog_name     char*      name of the popup widget                 *
 *                                                                      *
 * Output parameters:          	                                        *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      The dialog widget                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       09/96  change input calling parm                    *
 * S. Wang/GSC	    05/97  change interface and modify			*
 * G. Krueger/EAI   09/97  _NxmClosePopupCallback -> NxmClose_popupCb	*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
Widget      frm, pane, button;
XmString    xmstr;

/*---------------------------------------------------------------------*/

        NxmDwell_readTable();

	_dwellW = XmCreateBulletinBoardDialog(parent,
		dialog_name, NULL, 0);

	xmstr = XmStringCreateLocalized("Dwell Rate");

	XtVaSetValues(_dwellW,
		XmNnoResize, 	True,
		XmNdialogTitle, xmstr,
		XmNwidth,  	320,
		XmNheight, 	380,
		NULL);
	XmStringFree(xmstr);

	pane = XtVaCreateManagedWidget("pane",
		xmPanedWindowWidgetClass, _dwellW,
		XmNsashWidth,             1,
		XmNsashHeight,            1,
		XmNx, 			  20,
		XmNy, 			  30,
		NULL);

	frm = XtVaCreateManagedWidget("frame",
                xmFrameWidgetClass,     pane,
		NULL);

	NxmDwell_scalesCreate(frm);

    	NxmDwell_buttonsCreate( pane, 
				NxmDwell_buttonSelectCb );

	NxmDwell_setDefault();

	xmstr = XmStringCreateLocalized("Close");
	button = XtVaCreateManagedWidget("DwellClose",
                xmPushButtonWidgetClass, pane,
                XmNlabelString,          xmstr,
                XmNx,                    90,
                XmNy,                    490,
                XmNmarginLeft,           20,
                XmNmarginRight,          20,
                XmNshadowThickness,      3,
                NULL);
	XmStringFree (xmstr);

	XtAddCallback( button,XmNactivateCallback,
                (XtCallbackProc)NxmClose_popupCb,
                (XtPointer)_dwellW );

	return( _dwellW );
}

/*=====================================================================*/

Widget NxmDwell_scalesCreate ( Widget parent )
/************************************************************************
 * NxmDwell_scalesCreate                                                *
 *                                                                      *
 * This function creates a dwell-rate setting popup window. There are   *
 * three scale widgets associated with three text widgets for setting   *
 * up the dwell rate of first, intermediate, and last frame in the loop,*
 * separately. When the user changes the dwell rate, he/she can either  *
 * slide the scale widget or type the dwell rate into the text widget.  *
 * In this popup widget, pushbuttons are also created for those         *
 * frequently used dwell rate setup.                              	*
 *                                                                      *
 * Widget NxmDwell_scalesCreate(parent)				        *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent          Widget     parent widget ID                         *
 *                                                                      *
 * Output parameters:                                                   *
 * NxmDwell_scalesCreate	Widget        widget id of the scale    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    05/97	remove last two parmeters		*
 ***********************************************************************/
{
Widget rc;

/*---------------------------------------------------------------------*/

	rc = XtVaCreateManagedWidget("dwell_scales",
		xmRowColumnWidgetClass, parent,
		XmNorientation,         XmVERTICAL,
		NULL);

	_dwellValueText[0] = NxmDwell_scaleSingleCreate(rc, 0);

    	XtCreateManagedWidget("first",
		xmLabelWidgetClass, rc,
                NULL, 0);

	_dwellValueText[1] = NxmDwell_scaleSingleCreate(rc, 1);

    	XtCreateManagedWidget("intermediate",
		xmLabelWidgetClass, rc,
                NULL, 0 );

	_dwellValueText[2] = NxmDwell_scaleSingleCreate(rc, 2);

    	XtCreateManagedWidget("last",
		xmLabelWidgetClass, rc,
                NULL, 0);

	return( rc );

}

/*=====================================================================*/

Widget NxmDwell_scaleSingleCreate ( Widget parent, long which )
/************************************************************************
 * NxmDwell_scaleSingleCreate                                           *
 *                                                                      *
 * This function creates  one scale widget as well as the               *
 * associated text widget for changing the dwell rate .                 *
 *                                                                      *
 * Widget NxmDwell_scaleSingleCreate(parent, which)		        *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent          Widget     parent widget ID                         *
 *  which           int        scale identifier                         *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 * NxmDwell_scaleSingleCreate	Widget	The widget holding one scale	*
 *					 and one text widget            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    05/97	remove last two parmeters		*
 ***********************************************************************/
{
Widget rc, text;
int    max_scale;
/*---------------------------------------------------------------------*/


	rc = XtVaCreateManagedWidget("rc",
		xmRowColumnWidgetClass, parent,
		XmNorientation,         XmHORIZONTAL,
		NULL);

	max_scale   =  (int)(MAXDWELLRATE)/10;

	_dwellScale[which] = XtVaCreateManagedWidget("scale",
		xmScaleWidgetClass, rc,
		XmNorientation,     XmHORIZONTAL,
		XmNx,		    3,
		XmNwidth,	    200,
		XmNheight,	    15,
		XmNminimum,	    1,
		XmNdecimalPoints,   2,
		XmNmaximum,	    max_scale,
		NULL);

	XtAddCallback(_dwellScale[which],XmNvalueChangedCallback,
		(XtCallbackProc)NxmDwell_scaleCb, (XtPointer)which);
	XtAddCallback(_dwellScale[which],XmNdragCallback,
		(XtCallbackProc)NxmDwell_scaleCb, (XtPointer)which);

        text = XtVaCreateManagedWidget("value",
		xmTextFieldWidgetClass,    rc,
                XmNcolumns,                4,
                XmNcursorPositionVisible,  False,
                NULL);

        XtAddCallback(text, XmNactivateCallback,
                (XtCallbackProc)NxmDwell_textValueCb, (XtPointer)which);

	return( text );

}

/*=====================================================================*/

Widget NxmDwell_buttonsCreate ( Widget parent, 
				void (*callback)(Widget, long, XtPointer) )
/************************************************************************
 * NxmDwell_buttonsCreate                                               *
 *                                                                      *
 * This function creates a set of easy buttons for default dwell        *
 * setting.   								*
 *                                                                      *
 * Widget NxmDwell_buttonsCreate(parent, callback)             		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent          Widget     parent widget ID                         *
 *  *callback()     void       callback function for the buttons        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * S. Wang/GSC	    	05/97	rewrite					*
 * E. Safford/GSC	04/99	add return -- fix irix6 compiler warning*
 * T. Piper/SAIC	12/02	Fixed Motif 2.2 radio button problem	*
 * T. Piper/SAIC	02/04	removed unused variable middle		*
 ***********************************************************************/
{
Widget	bb, rc, rc1;
long	ii;
/*---------------------------------------------------------------------*/

	bb = XtVaCreateManagedWidget("SelectSpeed",
		xmBulletinBoardWidgetClass, parent,
		NULL);

	rc1 = XtVaCreateManagedWidget("rc1",
		xmRowColumnWidgetClass, bb,
		XmNorientation,         XmHORIZONTAL,
		NULL);

	XtVaCreateManagedWidget("Slow",
		xmLabelWidgetClass, rc1,
		NULL);

	rc = XtVaCreateManagedWidget("rc",
		xmRowColumnWidgetClass, rc1,
		XmNorientation,         XmHORIZONTAL,
		XmNradioBehavior,       False,
		NULL);

	_button = (WidgetList)XtMalloc(DWELLBTN *  sizeof(Widget) );
	
	for( ii = 0; ii < DWELLBTN; ii++ ) {
		_button[ii] = XtVaCreateManagedWidget ( "",
			      xmToggleButtonGadgetClass, rc,
                              NULL );
		if (callback)
		    XtAddCallback(_button[ii], XmNvalueChangedCallback,
			(XtCallbackProc)callback, (XtPointer)ii );
	}

	XtVaCreateManagedWidget("Fast",
		xmLabelWidgetClass, rc1,
		NULL);

	return (Widget)NULL;
}

/*=====================================================================*/
/* ARGSUSED */
void NxmDwell_popDown ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * NxmDwell_popDown	                                                *
 *                                                                      *
 * This function pops down the dwell setting panel.			*
 *                                                                      *
 * void NxmDwell_popDown(w, clnt, cbs)		                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w           Widget       widget ID                              *
 *      clnt	XtPointer	client data				*
 *      cbs       XtPointer    resource                               *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *         		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC		05/97						*
 ***********************************************************************/
{
    XtUnmanageChild(_dwellW);
}

/*=====================================================================*/

int *NxmDwell_getDwellPtr ( void )
/************************************************************************
 * NxmDwell_getDwellPtr	                                                *
 *                                                                      *
 * This function gets the current dwell rate array pointer.		*
 *                                                                      *
 * int* NxmDwell_getDwellPtr()				                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *         		NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *    NxmDwell_getDwellPtr   int*   pointer to current dwell rate array	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC		06/97						*
 ***********************************************************************/
{
    return(_curDwellRate);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmDwell_textValueCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmDwell_textValueCb                                                 *
 *                                                                      *
 * This function creates a set of easy buttons for default dwell        *
 * setting.   								*
 *                                                                      *
 * void NxmDwell_textValueCb(w, which, cbs)               		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w          Widget     calling widget ID                             *
 *  which      long        button index          			*
 *  cbs      XtPointer  never used        				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *         		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
String	fvalue;
int	value;
/*---------------------------------------------------------------------*/

	XtVaGetValues( _dwellValueText[which], 
		XmNvalue, &fvalue,
		NULL);

	value = (int)(atof(fvalue) * 1000.0);
	NxmDwell_setValue( (int)which, value );

}

/*=====================================================================*/
/* ARGSUSED */
void NxmDwell_scaleCb ( Widget w, long which, XmScaleCallbackStruct *cbs )
/************************************************************************
 * NxmDwell_scaleCb                                                   	*
 *                                                                      *
 * This is the callback function for dwell rate scale.			*
 *                                                                      *
 * void NxmDwell_scaleCb(w, which, cbs)               			*
 *                                                                      *
 * Input parameters:                                                    *
 *  w          Widget     		calling widget ID               *
 *  which      long        		button index          		*
 *  *cbs      XmScaleCallbackStruct    callback struct       		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *         		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    05/97    Reset easy buttons				*
 * T. Piper/SAIC	12/02	Fixed Motif 2.2 radio button problem	*
 ***********************************************************************/
{
	int	ii;
/*---------------------------------------------------------------------*/
	for ( ii = 0; ii < DWELLBTN; ii++)
	    XmToggleButtonSetState(_button[ii], FALSE, FALSE); 

        if (cbs != (XmScaleCallbackStruct *) NULL)
                NxmDwell_setValue( (int)which, cbs->value*10 );

}

/*=====================================================================*/
/* ARGSUSED */
void NxmDwell_buttonSelectCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmDwell_buttonSelectCb                                              *
 *                                                                      *
 * This is the callback function for easy buttons			*
 *                                                                      *
 * void NxmDwell_buttonSelectCb(w, which, cbs)               		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w          Widget     calling widget ID                             *
 *  which      long        button index          			*
 *  cbs      XtPointer  never used        				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *         		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    05/97	add current button _curBtn		*
 * T. Piper/SAIC	12/02	Fixed Motif 2.2 radio button problem	*
 ***********************************************************************/
{
long	ii;
int	value;
/*---------------------------------------------------------------------*/

	for ( ii = 0; ii < DWELLBTN; ii++) {
	    if ( ii != which )  XmToggleButtonSetState(_button[ii], FALSE, FALSE);
	}		
	for ( ii = 0; ii < 3; ii++) {
		value = (int)( _dwellScaleRate[which][ii]*1000.0F);
		NxmDwell_setValue( (int)ii, value );
	}
}
 
/*=====================================================================*/

void NxmDwell_setValue ( int which, int value )
/************************************************************************
 * NxmDwell_setValue                                                    *
 *                                                                      *
 * This function sets the specified dwell rate.                         *
 *                                                                      *
 * void NxmDwell_setValue(which, value)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int     dwell rate identifier                           *
 *  value       int     dwell rate in milliseconds                      *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * S. Wang/GSC	    05/97	modified				*
 ***********************************************************************/
{
float   temp;
char	name[10];
/*---------------------------------------------------------------------*/

	if ( value > MAXDWELLRATE )
		value = MAXDWELLRATE;

	_curDwellRate[which] = value;

	switch ( which ) {
            case 0:
        	NXManimationDwell.first = (unsigned long)value;
        	temp = (float)NXManimationDwell.first/1000.0F;
        	break;

            case 1:
        	NXManimationDwell.loop = (unsigned long)value;
        	temp = (float)NXManimationDwell.loop/1000.0F;
        	break;

            case 2:
        	NXManimationDwell.last = (unsigned long)value;
        	temp = (float)NXManimationDwell.last/1000.0F;
        	break;
        }

        sprintf(name, "%3.2f", temp );

        XtVaSetValues( _dwellValueText[which],
                        XmNvalue, name,
                        NULL);

	XtVaSetValues( _dwellScale[which],
                        XmNvalue, value/10,
                        NULL);
}

/*=====================================================================*/

void NxmDwell_setDefault ( void )
/************************************************************************
 * NxmDwell_setDefault                                                  *
 *                                                                      *
 * This function sets the default dwell rate.				*
 *                                                                      *
 * void NxmDwell_setDefault()	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC		06/97						*
 * T. Piper/SAIC	12/02	Fixed Motif 2.2 radio button problem	*
 * ***********************************************************************/
{
	int	ii;
/*---------------------------------------------------------------------*/
	for ( ii = 0; ii<3; ii++ ) 
	    _curDwellRate[ii] = (int)(1000.0F*_dwellScaleRate[2][ii]);

	XmToggleButtonSetState(_button[2], TRUE, TRUE);
}

/*=====================================================================*/

void NxmDwell_readTable ( void )
/************************************************************************
 * NxmDwell_readTable                                                   *
 *                                                                      *
 * This function reads in dwell table.                    		*
 *                                                                      *
 * void NxmDwell_readTable()		                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   06/97						*
 ***********************************************************************/
{
FILE	*fp;
char	def_dir[100], buffer[256];
int	i, j, ier;

/*---------------------------------------------------------------------*/

	strcpy( def_dir, "loop" );
        fp = cfl_tbop( DWELL_TABLE, def_dir, &ier );

        if ( fp == NULL || ier != 0 ) {
	    printf(" Need dwell table <%s>. See system administrator.\n",
			DWELL_TABLE);
	    exit(0);
        }

	for( i=0, j=0; i<DWELLBTN; i++ ) {
            cfl_trln( fp, 256, buffer, &ier );
	    if ( ier == 0 ) {
		sscanf( buffer, "%f %f %f", 
			&(_dwellScaleRate[j][0]), 
			&(_dwellScaleRate[j][1]), 
			&(_dwellScaleRate[j][2]) );
		j++;
	    }
	}
	
	if ( j== 0 ) {
	    printf( " Error reading dwell table <%s>. \n", DWELL_TABLE );
	    exit(0);
	}
	else { 
	     while ( j < DWELLBTN ) {
		_dwellScaleRate[j][0] = _dwellScaleRate[j-1][0];	
		_dwellScaleRate[j][1] = _dwellScaleRate[j-1][1];	
		_dwellScaleRate[j][2] = _dwellScaleRate[j-1][2];	
		j++;
	    }
	}

	fclose(fp);	
}

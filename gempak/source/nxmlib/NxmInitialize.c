#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

Display       *NXMdisplay;
Colormap      NXMcmap;
Boolean       NXMisInitialized = False;
XtAppContext  NXMapp;


void NxmInitialize ( Widget widget ) 
/************************************************************************
 * NxmInitialize                                                        *
 *                                                                      *
 * This function gets the display ID and the color map ID for Nxm       *
 *    library.                                                          *
 *                                                                      *
 * NxmInitialize ( widget )   		                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  widget     Widget     ID of a widget.                               *
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI        1/96  add application context NXMapp          	*
 ***********************************************************************/
{

	NXMdisplay = XtDisplay( widget );
	NXMcmap    = DefaultColormap ( (XtPointer)NXMdisplay, DefaultScreen((XtPointer)NXMdisplay) ); 
	NXMapp = XtDisplayToApplicationContext( NXMdisplay );

	NXMisInitialized = True;

}

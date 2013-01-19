#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "xwprm.h"
#include "proto_xw.h"

/************************************************************************
 * NxmGmpk.c								*
 *									*
 * This module contains functions that set up XW driver from GUI 	*
 * applications.							*
 *									*
 * CONTENTS:								*
 *									*
 * NxmGmpkInit()	initializes GEMPAK variables			*
 * NxmGmpkRgstr()	registers a window (drawing widget) as a GEMPAK	* 
 *			window						*
 ***********************************************************************/

/*=====================================================================*/

int NxmGmpkInit ( Widget wid, int mode, void (*init_func)(int *iret) )
/************************************************************************
 * NxmGmpkInit                                                         	*
 *                                                                      *
 * This function initializes GEMPAK variables. 				*
 *                                                                      *
 * int NxmGmpkInit(wid, mode, init_func)                               	*
 *                                                                      *
 * Input parameters:                                                    *
 *  wid	 	Widget      widget ID					*
 *  mode 	int	    1 map, 2 graph				*
 *  *init_func()	void	    initialization function from appl.	*
 *                                                                      *
 * Output parameters:                                                   *
 *  NONE  		                                                *
 *                                                                      *
 * Return parameters:                                                   *
 *	NxmGmpkInit	int	G_NORMAL	successful		*
 *				=  -1		error occured		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	02/93                                           *
 * C. Lin/EAI       	01/95   add comments,clean up add GetColorDefs()*
 * S. Wang/GSC	    	01/97   add init_func() and rename 		*
 * I. Durham/GSC    	05/98   changed call for underscore		*
 * E. Safford/GSC	01/99	copied from Ntrans for Ncolor		*
 * A. Person/Penn State 06/02	Updated to support 16- and 24-bit	*
 *				graphics				*
 * T. Piper/SAIC	07/03	removed GraphCid, SatCid, RadCid	*
 * T. Piper/SAIC	08/03	removed xwcmn, added xginit()		*
 * T. Piper/SAIC        01/08   Removed IN_BDTA; included in IP_INIT	*
 ***********************************************************************/
{
int  iret=0, ignore, respond;
int  level, bufflg, dttmflg;
char msg[]="Fatal Error: no graphic colors. Check ntl.\n";

/*---------------------------------------------------------------------*/
/*
 * Initialize allocflag and set colormap, root, visual in xwcmn.h
 */
	xginit(wid, &iret);
	if ( iret != G_NORMAL ) {
	    return(-1);
	}

/*
 * Allocate the graphic colors for GUI applications
 */
        xcaloc( GraphCid, &iret );
	if ( iret == G_NCLRAL ) {
                er_wmsg("gemplt", &iret, NULL, &ignore,
                                strlen("gemplt"), 0 );
                return(-1);
        }
        else {
                if ( iret == G_ZEROCB ) {
                  printf(msg);
                  return(-1);
                }
        }
	xscint( &iret );

/*
 *  Initialize GEMPAK common blocks
 */
        ip_init( &respond, &iret );

/*
 * set the error bufferring scheme
 * different level may be set by the application
 */
        level   = 0;
        bufflg  = 1;
        dttmflg = 1;
        er_stat( &level, &bufflg, &dttmflg, &iret);

/*
 * initialize GEMPAK plotting package
 */
        gg_init( &mode, &iret );

	if ( init_func ) {
	    init_func(&iret);	
	    if ( iret != 0 )  return(-1);
	}

	return(G_NORMAL);

}

/*=====================================================================*/

void NxmGmpkRgstr ( Widget wid, char *name, void (*rgstr_func)(void) )
/************************************************************************
 * NxmGmpkRgstr                                                         *
 *                                                                      *
 * This function registers a window (drawing widget) as a GEMPAK        *
 * window.                                                              *
 *                                                                      *
 * void NxmGmpkRgstr(wid, name, restr_func)                             *
 *                                                                      *
 * Input parameters:							*
 *  wid		Widget      window widget ID				*
 *  *name	char        window name 				*
 *  *rgstr_func()	void     initilization function			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *  NONE		                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      4/96                                                 *
 * S. Wang/GSC	   1/97  	add restr_func() and rename 		*
 * H. Zeng/EAI    04/00         changed cursor change function          *
 * T. Piper/SAIC	08/03	removed xwcmn, adedd XtDisplay(wid)	*
 ***********************************************************************/
{
Window    gwin;
char      wname[20];
int       iret, xdpth;
GC        gemgc;
Dimension width, height;
/*---------------------------------------------------------------------*/
/*
 * set the cursor to the default
 */
        NxmCursor_setCursor(wid, CURS_DEFAULT);
        strcpy(wname, name);

/*
 * get the window info
 */
        gwin = XtWindow(wid);
        gemgc = XCreateGC(XtDisplay(wid), gwin, 0, 0);

        xdpth = DefaultDepth( XtDisplay(wid), DefaultScreen( XtDisplay(wid) ) );
        XtVaGetValues(wid, XmNwidth,  &width,
                         XmNheight, &height,
                         NULL );

/*
 * call GEMPAK to register the window (drawing area) 
 * from now on, this drawing area will be GEMPAK's
 * window identified by name = wname.
 */
        xmotifw( gwin, wname, gemgc, (int)width, (int)height,
                xdpth, &iret );

/*
 * set DEVICE in gempak
 */
	gg_motf( wname, &iret, strlen(wname));

	if ( rgstr_func )
		rgstr_func();
}

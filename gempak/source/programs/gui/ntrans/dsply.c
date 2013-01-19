#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "Nxm.h"
#include "xwcmn.h"

extern	Widget	legend_labelW[];
extern _NXMpixmapData  NXMpixmapData;

/************************************************************************
 *									*
 * S. Wang/GSC		01/97	clean up				*
 * I. Durham/GSC	05/98	changed call for underscore		*
 * A. Hardy/GSC         03/99   legend_labelW range 2 -> 3		*
 * S. Law/GSC           01/00   replaced curpxm with xscpxm		*
 * E. Safford/GSC	08/00   update for new xwcmn.h     		*
 * J. Wu/GSC		05/01   free XmStrings     			*
 * T. Piper/SAIC	07/03	replaced gemdisplay with XtDisplay()	*
 ***********************************************************************/

/*=====================================================================*/

void display_pixmap ( void )
{
        NxmChangePixmapData( PixmapData.current_pixmap,
                        PixmapData.pixmap_no);
        displayPixmap();
}

/*=====================================================================*/

void displayPixmap ( void )
{

char name[7];
int     iret;
XmString	xmstr;

/*---------------------------------------------------------------------*/

        xscpxm (NXMpixmapData.current, &iret);

        sprintf(name, "%d/%d", NXMpixmapData.current + 1 ,
                        NXMpixmapData.total);

        xmstr = XmStringCreateLocalized(name);
	XtVaSetValues(legend_labelW[2], XmNlabelString, xmstr, NULL );
	XmStringFree( xmstr );
	
        XCopyArea (XtDisplay(DrawingW),
               gemwindow[current_window].pxms[0][NXMpixmapData.current],
               XtWindow(DrawingW), gemwindow[current_window].gc,
               0, 0, (unsigned int)(gemwindow[current_window].width),
               (unsigned int)(gemwindow[current_window].height), 0, 0);

        XFlush(XtDisplay(DrawingW));

}

/*=====================================================================*/

void clear_window ( void )
{
int	iret;

/*---------------------------------------------------------------------*/

        gclear(&iret);
	PixmapData.old_pixmap_no = 1;
        PixmapData.current_pixmap = 0;
        display_pixmap();

}

#include "geminc.h"

int error_count;
int handler ( Display *display, XErrorEvent *error );

int handler ( Display *display, XErrorEvent *error )
/************************************************************************
 * handler								*
 *									*
 *	This is a function used by freeColors.				*
 *									*
 * int handler ( display, error )					*
 *									*
 * Input parameters:							*
 *	*display	Display						*
 *	*error		XErrorEvent					*
 *									*
 * Output parameters:							*
 *	handler		int						*
 **									*
 * Log:									*
 ***********************************************************************/
{
    error_count +=1;
    return 1; /* This result is actually ignored. */
}

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * freeColors								*
 *									*
 * This program reports the number of colors in the default colormap 	*
 * that are allocated read-only and shareable, allocated writeable and	*
 * private, or unallocated.						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *		NONE 							*       
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		12/97                                           *
 ************************************************************************/
{
Display *dpy;
Colormap cmap;
Screen *screen;
int s, i, count, unwriteable;
unsigned long mask, pixel;
XColor color;

if ((dpy = XOpenDisplay(NULL)) == NULL) {
    fprintf(stderr, "%s: Can't open %s\n", argv[0], XDisplayName(NULL));
    exit(1);
}
  for (s=0; s<ScreenCount((XtPointer)dpy); s++) {
    screen = ScreenOfDisplay((XtPointer)dpy, s);
    if (ScreenCount((XtPointer)dpy) >1) {
        printf("screen %d\n", s);
    }
    cmap = DefaultColormapOfScreen(screen);
    XGrabServer(dpy); /* Don't interrupt while I am counting */
    XSetErrorHandler(handler);
    error_count = 0;
    for (i=0; i<CellsOfScreen(screen); i++) {
         color.pixel = i;
         XQueryColor(dpy, cmap, &color);
	 /* Try to set color to same value.
	  * Any client can write to a writeable cell.
	  * Unallocated and read-only cells will increment error_count.
	  */
	 XStoreColors(dpy, cmap, &color, 1);
    }
    XSync (dpy, False);
    unwriteable = error_count;

    count = 0;
    while (XAllocColorCells(dpy, cmap, True, &mask, 0, &pixel, 1))
	count += 1;
    for (pixel=0; pixel<CellsOfScreen(screen); pixel++) {
	XFreeColors(dpy, cmap, &pixel, 1,0);
    }

    XUngrabServer (dpy);
    XSync (dpy, False);
	printf("\n\n\t\t\tCURRENTLY YOUR SYSTEM HAS:\n");
	printf("\t\t========================================\n");
	printf("\t\t    %d total color cells.\n", CellsOfScreen(screen));
	printf("\t\t    %d free  color cells.\n", count);
	printf("\t\t    %d shared read-only color cells.\n", unwriteable-count);
	printf("\t\t    %d private writeable color cells.\n", 
		CellsOfScreen(screen)-unwriteable);
	printf("\t\t========================================\n\n");
  }    
  XCloseDisplay(dpy);
  return 0;
}

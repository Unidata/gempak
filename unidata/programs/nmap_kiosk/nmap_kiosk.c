/* compile with cc nmap_kiosk.c -o nmap_kiosk -lXtst -lX11 -lXext */

/*
Copyright 2005 University Corporation for Atmospheric Research/Unidata

Portions of this software were developed by the Unidata Program at the 
University Corporation for Atmospheric Research.

Access and use of this software shall impose the following obligations
and understandings on the user. The user is granted the right, without
any fee or cost, to use, copy, modify, alter, enhance and distribute
this software, and any derivative works thereof, and its supporting
documentation for any purpose whatsoever, provided that this entire
notice appears in all copies of the software, derivative works and
supporting documentation.  Further, UCAR requests that the user credit
UCAR/Unidata in any publications that result from the use of this
software or in any product that includes this software. The names UCAR
and/or Unidata, however, may not be used in any advertising or publicity
to endorse or promote any products or commercial entity unless specific
written permission is obtained from UCAR/Unidata. The user also
understands that UCAR/Unidata is not obligated to provide the user with
any support, consulting, training or assistance of any kind with regard
to the use, operation and performance of this software nor to provide
the user with any updates, revisions, new versions or "bug fixes."

THIS SOFTWARE IS PROVIDED BY UCAR/UNIDATA "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL UCAR/UNIDATA BE LIABLE FOR ANY SPECIAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XTest.h>

int sleeptime=30;
char displayname[80]="localhost:0";

void usage(char *prog)
{
    printf("usage: %s -w remote_windowid [-h remote_displayname] [-t sleep ] fmin fmax\n",prog);
    printf("example: %s -t 30 -w 0x500000e -h 10.99.99.99:0 1 8\n",prog);
    exit(1);
}

int main(int argc,char *argv[])
{
  char keystr[8];
  int icnt, fmin, fmax;
  int major_op, first_event, first_error;

  Display *dpy2 = NULL;
  Window  win1, win2;
  XWindowAttributes winatts;
  int revert, istatus;

  KeySym keysym;
  KeyCode keycode;

  extern char	*optarg;
  extern int	optind, optopt, opterr;
  int argnum, optval;

  while ( ( optval = getopt(argc, argv, "t:h:w:")) != -1)
     switch ( optval )
        {
        case 't':
		sscanf(optarg,"%d",&sleeptime);
		break;
        case 'h':
		strcpy(displayname, optarg);
		break;
	case 'w':
		sscanf(optarg,"0x%lx", &win2);
		break;
	default:
		usage(argv[0]);
        }

  argnum = optind;
  if ( argc - argnum != 2 )
      usage(argv[0]);

  /* read input */
  sscanf(argv[argnum], "%d\n",&fmin); argnum++;
  sscanf(argv[argnum], "%d\n",&fmax); argnum++;
  if ( fmin < 1 || fmin > 16 || fmax < 1 || fmax > 16 ) {
     printf("loop range must be between 1 and 16\n");
     exit(1);
  }
  icnt = fmin;

  /* open display */
  if ((dpy2 = XOpenDisplay(displayname)) == NULL) { /* Open remote display */
    fprintf(stderr, "Failed: open display %s\n", XDisplayName(displayname));
    exit(1);
  }

  if ( ! XQueryExtension(dpy2, "XTEST", &major_op, &first_event, &first_error) ) {
     printf("Xtestextension not supported\n");
     exit(1);
  }


  while (1) {
     istatus = XGetWindowAttributes(dpy2, win2, &winatts);
     if ( ( istatus ) && ( winatts.map_state == IsViewable ) )
        {
        sprintf(keystr,"F%d",icnt);
        keysym = XStringToKeysym(keystr);
        keycode = XKeysymToKeycode(dpy2, keysym);
   
        /* Store information about where our current focus is */
        XGetInputFocus(dpy2, &win1, &revert);
   
        /* set input focus to the nmap window */
        XSetInputFocus(dpy2, win2, 1, CurrentTime);
        XTestFakeKeyEvent (dpy2, keycode, 1, 0);
        XTestFakeKeyEvent (dpy2, keycode, 0, 0);
   
        /* restore focus */
        XSetInputFocus(dpy2, win1, revert, CurrentTime);
        XFlush(dpy2);
        icnt++;
        if ( icnt > fmax ) icnt = fmin;
        }
     sleep(sleeptime);
  } 

}

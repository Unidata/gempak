/************************************************************************
 * app.h                                                             	*
 *                                                                      *
 * Header file for application.                          		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi      08/94          					*
 * C. Lin            5/95        Reconstruct program                    *
 * R. Tian/SAIC      5/02        Added faxClr in _resrcs struct         *
 * T. Piper/SAIC	01/04	removed apptabCheck prototype		*
 * T. Piper/SAIC	01/04	changed calling sequence of apptabRead	*
 * E. Safford/SAIC	01/08	included Nxm.h for NxmWarn_Show prototyp*
 ***********************************************************************/

#ifndef _app_H_
#define _app_H_

#include "defs.h"
#include "Nxm.h"
#include "prog.h"


/*
 * Application table file record format.
 */
typedef struct apptab {
   char         name[MAX_PROG_NAME];            /* application name */
   char         icon[MAX_ICON_NAME];            /* icon file name */
   char         fg[MAX_COLOR_NAME];             /* foreground */
   char         bg[MAX_COLOR_NAME];             /* background */
   Widget       button;                         /* pushbutton widget */
   Widget       label;                          /* label widget */
   unsigned     copies;                         /* Decides how many */
                                                /* copies of the appl */
   struct apptab   *pNext;                      /* to next avail app */
} apptab;

struct _resrcs {
        int     graphClr;       /* # of graphic colors */
        int     satClr;         /* # of satellite colors */
        int     radClr;         /* # of radar colors */
        int     faxClr;         /* # of fax colors */
        char    *appbgName;  /* application background color name */
        int     appbgRed;    /* application background color red component */
        int     appbgGreen;  /* application background color green component */
        int     appbgBlue;   /* application background color blue component */
        Boolean verbose;        /* verbose mode */
        Boolean help;           /* print help */
};

extern struct _resrcs Resrcs; /* global variable for application resources */

extern apptab *apps_bp;                         /* global variable */
                                                /* base pointer to */
                                                /* applications table */
extern int numOfProcess;
extern int processPID[];

/* Prototypes */

int	appbtnCreate ( Widget parent, apptab *ap);

unsigned apptabRead ( Widget parent, apptab **bp);

int     colr_init ( int nbank, int banks[], Boolean verbose );


#endif /* _app_H_ */

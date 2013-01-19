/************************************************************************
 * defs.h                                                               *
 *                                                                      *
 * Header file for constants definition.                           	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi      08/94                                              *
 * C. Lin            5/95        Reconstruct program                    *
 * T. Piper/SAIC        01/04	Added APP_DIR, CLR_DIR, CLR_TBL		*
 * T. Piper/SAIC	01/04	Renamed APP_TABLE to APP_TBL		*
 * T. Piper/SAIC	07/04	Moved CLR_DIR and CLR_TBL to color.h	*
 ***********************************************************************/

#ifndef _defs_H_
#define _defs_H_

/*
 * Default button width and height
 */
#define BUTTONW 32
#define BUTTONH 32

/*
 * Define maximum size of arrays. The numbers were chosen arbitrarily so that
 * they won't be exceeded at this moment. They can be any number basically.
 */
#define MAX_LBUF                128
#define MAX_MESSAGE             150
#define MAX_PROC                50

#define MAX_PROG_NAME           32
#define MAX_ICON_NAME           128
#define MAX_COLOR_NAME          22              /* The longest color name */
                                                /* in rgb.txt for X11R5   */

#define MAX_TOTAL_COLOR		200		/* max total # of colors */

#define WHITE                   " \t\n"         /* characters btwn tokens */

#define	APP_DIR			"config"
#define APP_TBL			"ntl.tbl"       /* table file name */

#endif /* _defs_H_ */

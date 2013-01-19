/************************************************************************
 * GIFCMN.H								*
 * This header file delcares the variables used in asynch gif driver.	*
 *									*
 **									*
 *Log:									*
 * D. Austin		 5/96						*
 * T. Lee/GSC		 7/00	Renamed from gdr.h; renamed functions	*
 * S. Danz/AWC		11/03	Added fill pattern size/type vars,      *
 *                              changed xsize/ysize to int from int*    *
 * E. Safford/SAIC	01/08	include proto_xw.h			*
 ***********************************************************************/

#ifndef _GIFCMN_H 
#define _GIFCMN_H 

#include "geminc.h"
#include "gemprm.h"
#include "gd.h"
#include "proto_xw.h"


#define BG_RED          0
#define BG_GREEN        0
#define BG_BLUE         0

#define MXCOL  256		/* max num of colors in lookup table */

typedef struct {
	int  xsize;		/* pixel width	*/
	int  ysize;		/* pixel height	*/
	char *fname;		/* output filename	*/
} gdr_image;

/*
 * Globals.
 */

#ifdef GDR_GLOBAL
    /* Fill pattern size and type */
    float tszfil;
    int   kfillt;

    gdImagePtr Current_Im;
    FILE *outfile;
    int CurrentColorIndex;
    int CurrentBGColorIndex;
    int LUT[MXCOL];
    gdr_image *Current;
#else
    /* Fill pattern size and type */
    extern float tszfil;
    extern int   kfillt;

    extern gdImagePtr Current_Im;
    extern FILE *outfile;
    extern int CurrentColorIndex;
    extern int CurrentBGColorIndex;
    extern int LUT[MXCOL];
    extern gdr_image *Current;
#endif

 
/*
 * Prototypes
 */
void wbcolr (int *Red, int *Green,int *Blue, int *iret);
void wclear (int *iret);
void wclosp (char *filnam, int *gfplot, int *iret);
void wfill  (int *npt, int *xpt,int *ypt, int *iret);
void winit  (int *iright, int *ibot, int *iret);
void wline  (int *npts, int xpts[], int ypts[], int *iret);
void wsatim ( char *imgnam, int *xispace0, int *yispace0, int *xispace1,
			int *yispace1, int *ist, int *inum, int *iret );
void wsbrgb (int *icbank, int *indx, int *Red, int *Green, int *Blue, int *iret);
void wscolb (int *indx, int *iret);
void wscolr (int *Red, int *Green,int *Blue, int *iret);
void wsfill ( float *szfil, int *iftyp, int *iret);
void wsicmn ( int *hv, int *iret );

#endif /* _GIFCMN_H */

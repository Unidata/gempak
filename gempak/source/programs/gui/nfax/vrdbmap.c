#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"
#include "scnfll.h"

extern Widget toplevel;
extern Widget draw_area;
extern GC gc;      /* graphics context declaration */
extern GC gc_not;

int vrdbmap ( char *filename, Cardinal xsize, Cardinal ysize, 
		int samp_factor, int mirror, int flip, 
		Pixmap *pmap, int *iret )
/************************************************************************
 * vrdbmap								*
 *									*
 * This function reads a bit file (normally produced as a fax 		*
 * product) into a pixmap widget that has been passed as a parameter.	*
 *									*
 * int vrdbmap ( filename, xsize, ysize, samp_factor, mirror, flip,	*
 *							pmap, iret)	*
 *									*
 * Input parameters:							*
 *	*filename	char 	Name of file containing bits plane 	*
 *	xsize		Cardinal	size of product in x		*
 *	ysize		Cardinal	size of product in y		*
 * 	samp_factor	int	subsampling factor for the read oper.	*
 *	mirror		int						*
 *	flip		int						*
 *									*
 * Output parameters:							*
 *	*pmap		Pixmap 	Pixmap to create from file		*
 *	*iret		int	Return code				*
 *	vrdbmap		int						*
 **									*
 * Log:									*
 * E. Wehner/EAi	6/96	Created					*
 * E. Wehner/EAi	11/96	Switch File I/O to CFL calls		*
 * E. Wehner/EAi	3/97	Check map size before init		*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    FILE *fp;
    long fsize;
    char filnm[160];
    int i,j;
    int shiftval;
    char pixval;
    int showval;
    int b_it; /* bits */
    int b_in;  /* bytes in */
    int bytenum = 0;
    char *b;  /* buffer to read data into */


/* 
 * First, get the size of the raster file to be read, then allocate
 * a buffer large enough to hold it.
 */
    cfl_inqr(filename, getenv("FAX_TEMP"), &fsize, filnm, iret);
    if (*iret < 0)
    {
        return -1;
    }

    b = (char *)malloc((size_t)fsize);

    fp = cfl_ropn(filename, getenv("FAX_TEMP"), iret); 
    if (!fp)
    {

        return -1;
    }

/* 
 * Read the file data in...
 */
    cfl_read(fp, (int)fsize, (unsigned char*)b, &b_in, iret);


    cfl_clos(fp, iret);

/*
 * Create and initialize the in-memory pixmap
 */
    if ( (xsize <= 0) || (ysize <= 0) )
    {
        return -1;
    }
    
     *pmap = XCreatePixmap(XtDisplay(draw_area),
		RootWindowOfScreen(XtScreen(toplevel)),
		xsize, ysize, get_depth(draw_area) );

    XFillRectangle( XtDisplay(draw_area), *pmap, gc_not, 0, 0, xsize, ysize);

/* 
 * Now, the main loop.  For each bit in the inbound bitplane, if
 * the bit is set (tested by shifting the bit to the LSB, then ANDing
 * with a 1), then set a corresponding bit in the outbound pix map.
 */
    bytenum = 0;
 
    for (i = 0;i < (int)ysize; i++)
    {
 
        for (j = 0; j< (int)xsize/8; j++)
        {
           for (b_it = 0; b_it < 8; b_it++)
           {
              shiftval = 7-b_it;
           
               pixval = (char)(b[bytenum] >> shiftval);
               showval = pixval & 0x01;

             if (showval)
             {
                if (mirror)
                {
                    if (flip)
                    {
                       XDrawPoint(XtDisplay(draw_area), *pmap, gc, 
                              ( ( (j*8)+b_it)/samp_factor),
                              (i/samp_factor)); 
                    }
                    else
                    {
                       XDrawPoint(XtDisplay(draw_area), *pmap, gc, 
                             ((int)xsize/samp_factor - ( ((j*8)+b_it)/samp_factor)),
                               (i/samp_factor)); 
                    }
                }
                else
                {
                    if (flip)
                    {
                       XDrawPoint(XtDisplay(draw_area), *pmap, gc, 
                             ( ((j*8)+b_it)/samp_factor),
                              ((int)ysize/samp_factor - (i/samp_factor))); 
                    }
                    else
                    {
                       XDrawPoint(XtDisplay(draw_area), *pmap, gc, 
                             ((int)xsize/samp_factor - ( ((j*8)+b_it)/samp_factor)),
                              ((int)ysize/samp_factor - (i/samp_factor))); 
                    }
                }

             }
           }
          bytenum++;
        }
    }
    free(b);
    return 0;    
}

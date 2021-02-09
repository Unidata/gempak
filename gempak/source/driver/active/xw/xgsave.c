#include "xwcmn.h"

int WriteGIF (  FILE            *fp,
                unsigned char   *pic,
                int              w,
                int              h,
                unsigned char   *rmap,
                unsigned char   *gmap,
                unsigned char   *bmap,
                int              numcols,
                int              colorstyle );

void xgsave ( char filnam[], int *len, int *iframe, int *nframe, int *iret )
/************************************************************************
 * xgsave								*
 *									*
 * This program exports the current window to a graphics file.          *
 *									*
 * xgsave ( filnam, len, iframe, nframe, iret )				*
 *									*
 * Input parameters:							*
 *	filnam[]	char		Gif File name			*
 *	*len		int		length of the window name	*
 *      *iframe         int		frame index			*
 *	*nframe		int		number of frames		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *			G_NORMAL = normal return (no size change)	*
 *			G_NIWNAM = invalid window name			*
 **									*
 * Log:									*
 * C. Bailey/HPC	 1/05						*
 * S. Jacobs/NCEP	 4/05	Use pixmap dimensions instead of window	*
 * S. Guan/NCEP          4/19   Modified cloop->pxm_wdth in order to    *
 *                              work for 8 bits on RHEL 7               *
 ***********************************************************************/
{
    Window_str		*cwin;
    winloop_t		*cloop;
    Pixmap	      	pxmap;
    int     		ipxm, j, rbytes;
    char		tmpfil[200];
    unsigned char       xred[256], xgreen[256], xblue[256];
    unsigned char       *imgdatptr;
    XColor              colors[256];
    XImage              *image ;
    char                *tp1, *tp2 ;

    FILE        	*fp;
    int                 icnt, exist, ier, ierr, mxcol, linbyte, nBpp, loop;
    long                ilen;
    char                newfil[72], dummy[133], filebody[72], filext[10], *array[72];
    
/*---------------------------------------------------------------------*/
    *iret   = G_NORMAL;
    /*
     *  Set the number ofcolors in the color map.
     */
    mxcol = gemvis->map_entries;

    /*
     * Split file name into file name body and file extension
     */
    strcpy (tmpfil, filnam);
    
    array[0] = strtok(filnam, ".");
    if (array[0] == NULL) {
       printf("   No Filename Entered   \n");
       *iret = -120;
       return;
    }
    for (loop=1; loop<72; loop++) {
        array[loop] = strtok(NULL, ".");
        if(array[loop] == NULL) {
            if(loop < 2) {
                *iret = -120;
                return;
            }
            break;
        }
    }
    for (loop = 0; loop < 72; loop++) {
        if (array[loop] == NULL ) { 
            sprintf(filext, "%s", array[loop-1]);
            break;
        } else if (loop == 0 ) {
            sprintf(filebody, "%s", array[loop]);
        } else if (loop > 1 ) {
            strcat(filebody, ".");
            strcat(filebody, array[loop-1]);
        }
    }
    
    /* 
     * Determine if file extension is .gif. If not return error.
     */
    if(strcmp(filext,"gif") != 0) {
        *iret = -120;
        return;
    }

    /*
     *  Write error message if the file cannot be opened.
     */

    icnt  = 0;
    exist = G_TRUE;

    while ( exist && icnt < 1000 )
    {
        if ( icnt == 0 ) 
            sprintf ( newfil, "%s", tmpfil );
        else
            sprintf ( newfil, "%s_%03d.%s", filebody, icnt, filext );
	
        cfl_inqr ( newfil, NULL, &ilen, dummy, &ierr );
        if ( ierr != 0 )  exist = G_FALSE;
        icnt++;
    }
    fp = cfl_wopn ( newfil, &ier );
    
    if ( ier != G_NORMAL )  
    {
        *iret = -1;
        return;
    }

    cwin  = &(gemwindow[current_window]); 
    ipxm  = cwin->curpxm[cwin->curr_loop];
    pxmap = cwin->pxms[cwin->curr_loop][ipxm];
    cloop = &(cwin->loop[cwin->curr_loop]);
/*
 *   To find the image depth.
 */   
    image = XGetImage( gemdisplay, pxmap, 0, 0,
                       (unsigned int)cloop->pxm_wdth,
                       (unsigned int)cloop->pxm_hght,
                       AllPlanes, ZPixmap );
    nBpp = image->bits_per_pixel / 8;
    linbyte = image->width * nBpp;

/*
 When the default display color depth is set to 8 bit or 16 bit color
 the GIF exports are sometimes distorted on RHEL 7. There is no distortion 
 if the bytes per line of an image are equal to its bytes per pixel times its width.
*/

    if ((image->depth <= 16) && (image->bytes_per_line != linbyte))
    { 
        cloop->pxm_wdth =  floor(cloop->pxm_wdth/4.0) * 4; 
        XDestroyImage ( image );
        image = XGetImage( gemdisplay, pxmap, 0, 0,
                       (unsigned int)cloop->pxm_wdth,
                       (unsigned int)cloop->pxm_hght,
                       AllPlanes, ZPixmap );
    }

    /*
     *   Remove any padding bytes at the end of each line
     */

    if ( image->bytes_per_line > linbyte )
    {
        tp1 = tp2 = image->data;
        rbytes = image->bytes_per_line - linbyte;
        for ( j = 0; j < image->height; j++ )
        {
            memcpy ( tp1, tp2, (size_t)linbyte);
            tp1 += linbyte ;
            tp2 += linbyte + rbytes ;
        }
    }

    if(image->depth <= 8) {
        /* 
         *       Allocate The XColor Array Used With The Colormap
         */
        for( j=0; j<mxcol; j++ ) {
            colors[j].pixel = (Pixel)j;
            colors[j].flags = DoRed | DoGreen | DoBlue;
        }

        imgdatptr = (unsigned char *)image->data;
    }
    else {
        /*
         * for 16/24 bit data, create an indexed pixmap
         */
        int istart=0, negflg = 0, ivalue, ii, jj, inc=0, ifc;
        int nbanks,ir,il;
        unsigned char uctmp;

        nbanks = ColorBanks.nbank;
	
        for(ii = 0; ii < nbanks; ii++) {
            if(!allocflag[ii]) continue;

            for(jj = 0; jj<ColorBanks.banks[ii]; jj++) {
                colors[inc].pixel = ColorBanks.colrs[ii][jj];
                colors[inc].flags = DoRed | DoGreen | DoBlue;
                inc++;
            }
        }

        mxcol = inc;
    
        imgdatptr = (unsigned char *)malloc((size_t)(image->width * image->height));

        for(ii = 0; ii < (image->width * image->height); ii++) {

            if (image->bitmap_bit_order == LSBFirst) {
                /* need to flip bytes */
                il = 0; ir = nBpp - 1;
                while (il < ir) {
                    uctmp = image->data[istart + il]; 
                    image->data[istart + il] = image->data[istart + ir];
                    image->data[istart + ir] = uctmp;
                    il++;
                    ir--;
                }
            }
            mv_btoi ( (unsigned char *)(image->data),
                     &istart, &nBpp, &negflg, &ivalue, &ier);
            istart += nBpp;


            /* find value */
            jj = 0; ifc = -1;
            do {
                if ((Pixel)ivalue == colors[jj].pixel) 
                    ifc = jj;
                else
                    jj++;
            } while ( ( jj < mxcol )&&(ifc == -1) );

            if (ifc == -1)  ifc = 0;
            imgdatptr[ii] = (unsigned char)ifc;
        }
    }

    XQueryColors( gemdisplay, gemmap, colors, mxcol );

    for( j=0; j<mxcol; j++ )
    {
        xred[j] = (unsigned char)(colors[j].red >> 8);
        xblue[j] = (unsigned char)(colors[j].blue >> 8);
        xgreen[j] = (unsigned char)(colors[j].green >> 8);
    }
    
    WriteGIF( fp, imgdatptr,
	      cloop->pxm_wdth,
	      cloop->pxm_hght,
	      xred, xgreen, xblue, mxcol, 3 );
    
    if(image->depth > 8) free(imgdatptr);
    XDestroyImage ( image );
    
    cfl_clos (fp, iret);
}

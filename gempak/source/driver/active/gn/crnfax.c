#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "cpgcmn.h"
#include "proto_xw.h"


static void cvtmap(char *bitmap, int xsize, int ysize, int samp_factor, 
	        int mirror, int flip, unsigned char *pixmap, int *iret);

/************************************************************************
 * crnfax.c								*
 *									*
 * CONTENTS:								*
 * crnfax								*
 * cvtmap								*
 ***********************************************************************/

void crnfax ( char *dev, char *imgnam, int *iret )
/************************************************************************
 * crnfax								*
 *									*
 * This function serves as the controlling routine to convert a six	*
 * plane to a flat expanded raster plane whereby each pixel has a	*
 * real location in the bitmap.  The resultant plane is stored in a 	*
 * temporary file.							*
 *									*
 * crnfax ( pfname, psubset, pdesc, ixlen, iylen, bpp, iret )		*
 *									*
 * Input parameters:							*
 *	*pfname	 char	Name of the product file			*
 *	*psubset char	Subset of product being extracted		*
 * 	*pdesc	 char	Description of product				*
 * 	*ixlen	 int	Width of product being extracted		*
 *	*iylen	 int	Length of product being extracted		*
 *	*bpp	 int	Bits per pixel of product (always 1 for fax)	*
 *									*
 * Output parameters:							*
 *	*iret	int	Return code					*
 **									*
 * Log:									*
 * R. Tian/SAIC		04/02	Modified from cpg_sixrd			*
 ***********************************************************************/
{
    int mapsize;
    int samp_factor, mirror, flip;
    int numcuts;
    int num_read;
    char *pixmap;
    char *bitmap;
    FILE *rfp;
    int my_cut;
    int start_at;
    int stop_at;
    char pdesc[120];
    char psubset[5];
    ConvertRec crec[MAX_CUTS];

/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 * Allocate space for the raw image data
 */
    if ( ( rawData == (unsigned char *) NULL ) ||
         ( imldat > (int)last_rawsize) ) {
        if ( rawData != (unsigned char *) NULL)  {
            free(rawData);
        }

        rawData = (unsigned char *) calloc ( imldat,
                   sizeof(unsigned char) );

        if ( rawData == (unsigned char *) NULL )  {
            *iret = G_NMEMRY;
            return;
        }

        last_rawsize = imldat ;
    }
    pixmap = (char *)rawData;

/*
 * read the 6 bit file into memory.
 */
    rfp = (FILE *)cfl_ropn(imgnam, getenv("FAX_TEMP"), iret);
    if (!rfp)
    {
        *iret = G_NORDOPN;
        return;
    }
    cfl_read(rfp, imldat, rawData, &num_read, iret);
    cfl_clos(rfp, iret);

/* 
 * keep in mind that one packed 6 bit file can be composed of multiple
 * "files", each with its own "ffffff...fffffd" set of records... 
 * but only the EOF has a fffffc.
 * This while loop loops for each "ffffff->fffffd" sequence looking
 * for either an overrun beyond the size of the plance, or a return
 * on my_cut which indicates some number with the proper sequence 
 */
    start_at = 0;
    my_cut = -1;
    while ( ( start_at < num_read) && (my_cut == -1) && (*iret == 0) )
    {

/* 
 * find the start of the next embedded file 
 */
        pg_find_start(pixmap, num_read, &start_at);

/* 
 * read the header of a 6 bit file 
 */
        pg_rdhdr(pixmap, start_at, pdesc);

/*
 * retrieve SUBSET from the header.
 */
	strncpy(psubset, &pdesc[1], 4);
	psubset[4] = '\0';

/* 
 * read the cuts from the pixmap 
 */
        numcuts = 0;
        pg_read_cuts( psubset, pixmap, start_at, num_read, crec, 
				&numcuts, &my_cut, iret);

/* 
 * if at the bottom, the cut wasn't found, set to look at next rec 
 */
        if (my_cut == -1)
            start_at += 1440;
    }

/* 
 * if a matching cut was found in this file 
 */ 
    if (my_cut >= 0)
    {
/* 
 * get location for the end of the bit plane for decompression 
 */
        pg_find_stop(pixmap, num_read, start_at,  &stop_at);

/* 
 * read the file....after allocating space for the bitmap 
 */
        mapsize = (imnpix * imnlin) /8.0;
        bitmap = (char *) malloc(mapsize+2160);
        memset(bitmap, 0, mapsize+2160);

/* 
 * create a scanline 
 */
        pg_xplane(imnpix, &imnlin, pixmap, start_at, stop_at, bitmap);

	if ( strncmp(dev, "PS", 2) == 0 ) {

/*
 * simplily copy the bitmap into the pixmap
 */
	    memcpy(imgData, bitmap, mapsize+2160);
	} else {

/*
 * converts the bitmap (depth 1) into a pixmap of depth 8. 
 * Note: it is possible to create the pixmap directly. 
 * However, that requires more modification and is error prone. 
 * So decision is made to use this inefficient way.
 */
	    samp_factor = 1;
	    mirror = 1;
	    flip = 1;
	    cvtmap(bitmap, imnpix, imnlin, samp_factor, mirror, flip, 
                   imgData, iret);
	}

        free(bitmap);
    }
    else   /* the requested cut was not in the file...bad time */
    {
        *iret = G_NOPROD;
    }
}

/*=====================================================================*/

static void cvtmap(char *bitmap, int xsize, int ysize, int samp_factor, 
		  int mirror, int flip, unsigned char *pixmap, int *iret)
/************************************************************************
 * cvtmap								*
 *									*
 * This function converts a bitmap (normally produced as a fax		*
 * product) into a pixcel map with depth 8.				*
 *									*
 * cvtmap ( bitmap, xsize, ysize, samp_factor, mirror, flip,		*
 *							pixmap, iret)	*
 *									*
 * Input parameters:							*
 *	*bitmap		char 	bit map plane			 	*
 *	xsize		int	size of product in x			*
 *	ysize		int	size of product in y			*
 * 	samp_factor	int	subsampling factor			*
 *	mirror		int						*
 *	flip		int						*
 *									*
 * Output parameters:							*
 *	*pixmap		unsigned char 	Pixmap to create from file	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * R. Tian/SAIC		04/02	Modified from vrdbamp			*
 ***********************************************************************/
{
    int i,j;
    int shiftval;
    char pixval;
    int showval;
    int b_it; 
    int bytenum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /* 
     * Now, the main loop.  For each bit in the inbound bitplane, if
     * the bit is set (tested by shifting the bit to the LSB, then ANDing
     * with a 1), then set a corresponding byte in the outbound pixmap.
     */
    for (i = 0;i < ysize; i++) {
        for (j = 0; j < (xsize/8); j++) {
            for (b_it = 0; b_it < 8; b_it++) {
                shiftval = 7-b_it;
           
                pixval = bitmap[bytenum] >> shiftval;
                showval = pixval & 0x01;

                if (showval) {
                    if (mirror) {
                        if (flip) {
                            pixmap[(i*xsize+(j*8+b_it))/samp_factor] = 0;
                        } else {
                           pixmap[(i*xsize+xsize-(j*8+b_it))/samp_factor] = 0;
                        }
                    } else {
                        if (flip) {
                           pixmap[((ysize-i-1)*xsize+(j*8+b_it))/samp_factor] = 0;
                        } else {
                           pixmap[((ysize-i-1)*xsize+xsize-(j*8+b_it))/samp_factor] = 0;
                        }
                    }
                }
                else {
                    if (mirror) {
                        if (flip) {
                            pixmap[(i*xsize+(j*8+b_it))/samp_factor] = 255;
                        } else {
                           pixmap[(i*xsize+xsize-(j*8+b_it))/samp_factor] = 255;
                        }
                    } else {
                        if (flip) {
                           pixmap[((ysize-i-1)*xsize+(j*8+b_it))/samp_factor] = 255;
                        } else {
                           pixmap[((ysize-i-1)*xsize+xsize-(j*8+b_it))/samp_factor] = 255;
                        }
                    }
                }
            }
            bytenum++;
        }
    }
}

#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


extern ConvertRec crec[MAX_CUTS];  /* instantiate a conversion record */

void cpg_sixrd ( char *pfname, char *psubset, char *pdesc, int *ixlen, 
					int *iylen, int *bpp, int *iret )
/************************************************************************
 * cpg_sixrd								*
 *									*
 * This function serves as the controlling routine to convert a six	*
 * plane to a flat expanded raster plane whereby each pixel has a	*
 * real location in the bitmap.  The resultant plane is stored in a 	*
 * temporary file.							*
 *									*
 * cpg_sixrd ( pfname, psubset, pdesc, ixlen, iylen, bpp, iret )	*
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
 * E. Wehner/EAi	 6/96	Created				 	*
 * E. Wehner/EAi	10/96	Use cfl_inqr instead of stat call	*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    int mapsize;
    FILE *wfp;
    long fsize;		/* size of source file with 6 bits data */
    int numcuts;
    int num_read;
    char mapfil[80];
    char tmpfil[120];
    char *pixmap;
    char *bitmap;
    FILE *rfp;
    int my_cut;
    int start_at;
    int stop_at;
    int scanlines;
    int istat;



    *iret = 0;

    /* retrieve the variable for where the files are temporarily kept */
   
    if ( (psubset[0] <= ' ') || (psubset[0] > 'z') ) 
    {
        *iret = G_BADSUB;
        return;
    }



    /* find out the size of the 6 bit file before we read it in */
    fsize =  0;
    cfl_inqr(pfname, getenv("FAX_TEMP"), &fsize, tmpfil, iret);

    if (*iret < 0)
    {
        *iret = G_NORDOPN;
        return;
    }

    pixmap = ( char *) malloc((size_t)fsize);


    /* initialize memory area */
    memset(pixmap, 0, (size_t)fsize);


    rfp = (FILE *)cfl_ropn(pfname,getenv("FAX_TEMP"),  iret);
    if (!rfp)
    {
        *iret = G_NORDOPN;
        return;
    }


    cfl_read(rfp, (int)fsize, (unsigned char*)pixmap, &num_read, iret);

    /* keep in mind that one packed 6 bit file can be composed of multiple
       "files", each with its own "ffffff...fffffd" set of records... 
       but only the EOF has a fffffc.
       This while loop loops for each "ffffff->fffffd" sequence looking
       for either an overrun beyond the size of the plance, or a return
       on my_cut which indicates some number with the proper sequence */
    start_at = 0;
    my_cut = -1;
    while ( ( start_at < num_read) && (my_cut == -1) && (*iret == 0) )
    {

        /* find the start of the next embedded file */
        pg_find_start(pixmap, num_read, &start_at);

        /* read the header of a 6 bit file */
        pg_rdhdr(pixmap, start_at, pdesc);
        numcuts = 0;
        /* read the cuts from the pixmap */
        pg_read_cuts( psubset, pixmap,start_at, num_read, crec, 
				&numcuts, &my_cut, iret);

        if (*iret != 0)
        {
           return;
        }

        /* if at the bottom, the cut wasn't found, set to look at next rec */
        if (my_cut == -1)
            start_at += 1440;
       
    }

    /* if a matching cut was found in this file */ 
    if (my_cut >= 0)
    {
        *bpp = 1;
        *ixlen = crec[my_cut].xsize;
        *iylen = crec[my_cut].ysize;

        /* get location for the end of the bit plane for decompression */
        pg_find_stop(pixmap, num_read, start_at,  &stop_at);

        /* sometimes, fax control record doesnt show # scanlines, for
         * these cases, make a default size plane as large as the largest
         * map, but keep iylen at zero and set it later on... 
         */
        if (*iylen == 0)
        {
            scanlines = 7200;
        }
        else
        {
            scanlines = *iylen;
        }


        /* read the file....after allocating space for the bitmap */
        mapsize = (int)((float)(*ixlen * scanlines) * (float)*bpp/8.0F);


        bitmap = (char *) malloc((size_t)mapsize+2160);

        memset(bitmap, 0, (size_t)mapsize+2160);

        /* create a scanline */
        pg_xplane(*ixlen, iylen, pixmap, start_at, 
				stop_at, bitmap);


        sprintf(mapfil, "%s/%s.ras", getenv("FAX_TEMP"), psubset);
        wfp = cfl_wopn(mapfil,  iret);
        if (!wfp)
        {
          *iret = G_NOWROPN;
          return;
        }
        cfl_writ(wfp, mapsize, (unsigned char*)bitmap, &istat);  

        cfl_clos(wfp, iret);

        free(bitmap);

    }
    else   /* the requested cut was not in the file...bad time */
    {
        *iret = G_NOPROD;
    }

    free(pixmap);


    cfl_clos(rfp, iret);

}

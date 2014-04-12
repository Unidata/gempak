#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"

void im_rfax ( char *filnam, int *lenfil, int *isize, int *ixlen, 
                                                  int *iylen, int *iret )
/************************************************************************
 * im_rfax								*
 *									*
 * This function retireves the size info from the file.			*
 *									*
 * im_rfax ( filnam, lenfil, isize, ixlen, iylen, iret )		*
 *									*
 * Input parameters:							*
 *	*filnam		char	Name of the product file		*
 *	*lenfil		int	Length of the file name			*
 *									*
 * Output parameters:							*
 *	*isize		int	Size of the product file		*
 * 	*ixlen		int	Pixels per image line           	*
 *	*iylen		int	Image lines                      	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * R. Tian/SAIC		04/02	Modified from cpg_sixrd			*
 ***********************************************************************/
{
    long fsize;		/* size of source file with 6 bits data */
    int numcuts;
    int num_read;
    char tmpfil[120];
    char *pixmap;
    FILE *rfp;
    int my_cut;
    int start_at;
    char pdesc[120];
    char psubset[5];
    ConvertRec crec[MAX_CUTS];

/*---------------------------------------------------------------------*/

    *iret = 0;

    /* 
     * find out the size of the 6 bit file before we read it in 
     */
    fsize =  0;
    filnam [*lenfil] = '\0';
    cfl_inqr(filnam, getenv("FAX_TEMP"), &fsize, tmpfil, iret);
    if (*iret < 0 || fsize == 0)
    {
        *iret = G_NORDOPN;
        return;
    }

    /*
     * read the 6 bit file into memory.
     */
    rfp = cfl_ropn(filnam, getenv("FAX_TEMP"), iret);
    if (!rfp)
    {
        *iret = G_NORDOPN;
        return;
    }
    pixmap = ( char *) calloc(fsize, sizeof(char));
    if ( ! pixmap ) {
        *iret = G_NORDOPN;
        return;
    }
    cfl_read(rfp, fsize, (unsigned char*)pixmap, &num_read, iret);
    cfl_clos(rfp, iret);
    if ( *iret < 0 ) {
        free(pixmap);
        *iret = G_NORDOPN;
	return;
    }

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
    free(pixmap);

    /* 
     * if a matching cut was found in this file 
     */ 
    if (my_cut >= 0)
    {
        /* 
         * sometimes, fax control record doesnt show # scanlines, for
         * these cases, make a default size plane as large as the largest
         * map, but keep iylen at zero and set it later on... 
         */
        if (crec[my_cut].ysize == 0)
        {
            crec[my_cut].ysize = 7200;
        }

	*isize = fsize;
        *ixlen = crec[my_cut].xsize;
        *iylen = crec[my_cut].ysize;
    }
    else   /* the requested cut was not in the file...bad time */
    {
        *iret = G_NOPROD;
    }
}

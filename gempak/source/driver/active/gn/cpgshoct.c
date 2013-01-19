#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


extern ConvertRec crec[MAX_CUTS];  /* instantiate a conversion record */

void cpg_shoct ( char *pfname, char *pdesc, int *iret )
/************************************************************************
 * cpg_shoct								*
 *									*
 * This function serves as the controlling routine to show the cuts	*
 * that are present in a 6-bit file. 					*
 *									*
 * cpg_shoct ( pfname, pdesc, iret )					*
 *									*
 * Input parameters:							*
 *	*pfname	char	Name of the product file			*
 * 	*pdesc	char	Description of product				*
 *									*
 * Output parameters:							*
 *	*iret	int	Return code					*
 **									*
 * Log:									*
 * E. Wehner/EAi	 1/97	Created				 	*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    long fsize;		/* size of source file with 6 bits data */
    int numcuts;
    int num_read;
    char tmpfil[120];
    char *pixmap;
    FILE *rfp;
    int start_at;



    *iret = 0;


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


    rfp = cfl_ropn(pfname,getenv("FAX_TEMP"),  iret);
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
    printf(" Subset  Offset #Scan       Descriptor  \n");
    printf(" ------  ------ -----  ----------------------------------\n");
    start_at = 0;
    while ( ( start_at < num_read) && (*iret == 0) )
    {

        /* find the start of the next embedded file */
        pg_find_start(pixmap, num_read, &start_at);

        /* read the header of a 6 bit file */
        pg_rdhdr(pixmap, start_at, pdesc);
        numcuts = 0;
        /* read the cuts from the pixmap */
        cpg_dmpct( pixmap,pdesc, start_at, num_read, crec, 
				&numcuts,  iret);

        if (*iret != 0)
        {
           *iret = 0;
           return;
        }

        start_at += 1440;
       
    }

    free(pixmap);


    cfl_clos(rfp, iret);

}

#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


void pg_read_cuts ( char *subset, char *plane, int start_pack, 
			int plane_sz, ConvertRec crec[], int *numcuts, 
			int *match_cut, int *iret )
/************************************************************************
 * pg_read_cuts								*
 *									*
 * This function reads the ISCHED record to determine which cuts are 	*
 * active for this set of products.					*
 *									*
 * pg_read_cuts ( subset, plane, start_pack, plane_sz, crec, numcuts,	*
 *		  match_cut, iret )					*
 *									*
 * Input parameters:							*
 *	*subset		char		Subset number searched for	*
 *	*plane		char		Plane of 6-bit data		*
 *	start_pack	int		Packed file start		*
 *	plane_sz	int		Size of inbound plane		*
 *									*
 * Output parameters:							*
 *	crec	[]	ConvertRec	Conversion record (cut info.)	*
 *	*numcuts        int             Total number of cuts in the file*
 *	*match_cut	int		Cut number that matches subset	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * E. Wehner/EAi	11/96	Bitmasked byte movements		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    int pos;
    int isched_pos;    /* will be the beginning of the isched record */
    int recnum = 0;
    int more = 1;
    int word1;
    int word2;
    int tmp_word;
    int isrch_sub;    /* searching subset */


    *iret = 0;

    /* default ...means no cut was found in this set */
    *match_cut = -1;

    /* get an integer representation of the requested subset number */
    isrch_sub = atoi(subset);

    /* find cut record....hint!  it is on a 1440 byte boundary and
     * begins with a "0xFFFFFD"
     */
    pos = 1440+start_pack;
    isched_pos = 0;
    while ( (isched_pos == 0) && (pos < plane_sz))
    {
        if ( (  ( plane[pos] & 0xff)  == 0xff ) &&
             (  ( plane[pos+1] & 0xff) == 0xff) &&
             (  ( plane[pos+2] & 0xff) == 0xfd) )
            isched_pos = pos;
        else
            pos += 1440;
    }

    if (isched_pos == 0)
    {
        *iret = G_NOISCHD;
        return;
    }

    /* the first 16 byte record is trown away...then starts regular data... */
    pos += 16;

    while ( (recnum < MAX_CUTS) && (more) )
    {
        /* if the first 4 bytes are zero, this signals the end of the cuts */

        /* load the first and second words, then have to cut them as subsets */
        word1 = (plane[pos++] << 8 ) & 0xff00;
        word1 += (plane[pos++] & 0xff);

        word2 = (plane[pos++] << 8 ) & 0xff00;
        word2 += (plane[pos++] & 0xff);


        if ( (word1 == 0) && (word2 == 0) )
        {
            more = 0;  /* no more cuts! */
        }
        else
        {
            if (word1 == isrch_sub)
	    {
                *match_cut = *numcuts;
	    }

            (*numcuts)++;
        }

        sprintf(crec[recnum].psubset, "%i", word1);
        crec[recnum].origy = word2;

        /* skip next 2 bytes */
        pos += 2;

        /* number of scan lines in this section */
        tmp_word = (plane[pos++] << 8 ) & 0xff00;
        tmp_word += (plane[pos++]);
        crec[recnum].ysize = tmp_word;


        /* for now....SKIP the two byes of the flag bits.... */
        pos += 2;

        /* the xsize of the section is represented as the number of 8 bit
           bytes.  This fax reader expects it in number of pixels, so
           multiply by 8 to get the number of pixels..... */
	crec[recnum].xsize = 0;
        crec[recnum].xsize = (plane[pos++] & 0xff) << 3;
     /*    crec[recnum].xsize = plane[pos++] * 8; */

        /* for now, skip the final four bytes which talk about insets
           that are held inside of subsets.... */
        pos += 4;

        recnum ++;

    }

}


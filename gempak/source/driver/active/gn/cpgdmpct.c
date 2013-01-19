#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


void cpg_dmpct ( char *plane, char *descr, int start_pack, 
		int plane_sz, ConvertRec crec[], int *numcuts, int *iret )
/************************************************************************
 * cpg_dmpct								*
 *									*
 * This function reads the ISCHED record to determine which cuts are	*
 * active for this set of products.					*
 *									*
 * cpg_dmpct ( plane, descr, start_pack, plane_sz, crec, numcuts, iret) *
 *									*
 * Input parameters:							*
 *	*plane		char		Inbound plane of 6-bit data	*
 *	*descr		char		Description			*
 *	start_pack	int		Where this packed file starts	*
 *	plane_sz	int		Size of inbound plane		*
 *									*
 * Output parameters:							*
 *	crec	[]	ConvertRec	Conversion record (cut info.)	*
 *	*numcuts	int		Total number of cuts in file	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * E. Wehner/EAi	11/96	Bitmasked byte movements		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Jacobs/NCEP	 4/98	Fixed byte count at end of schedule info*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC		05/02	Renamed pgcmn.h to be cpgcmn.h		*
 ***********************************************************************/
{
    int pos;
    int isched_pos;    /* will be the beginning of the isched record */
    int recnum = 0;
    int more = 1;
    int word1;
    int word2;
    int tmp_word;


    *iret = 0;

    /* default ...means no cut was found in this set */

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
            (*numcuts)++;

            /* skip next 2 bytes */
            pos += 2;

            /* number of scan lines in this section */
            tmp_word = (plane[pos++] << 8 ) & 0xff00;
            tmp_word += (plane[pos++]);
            printf("  %4i   %4i    %4i  %s \n", word1, word2, tmp_word, descr);


            /* for now....SKIP the two byes of the flag bits.... */
            pos += 2;

            /* for now, skip the final four bytes which talk about insets
               that are held inside of subsets.... */
            pos += 5;

            recnum ++;
        }

    }

}

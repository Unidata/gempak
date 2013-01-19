#include "geminc.h"
#include "cpgcmn.h"

void pg_rdhdr ( char *plane, int start_at, char *text )
/************************************************************************
 * pg_rdhdr								*
 *									*
 * This function extracts the 45 byte character header from an NMC	*
 * 6-bit formatted product.						*
 *									*
 * pg_rdhdr ( plane, start_at, text )					*
 *									*
 * Input parameters:							*
 *  *plane	char	Plane of entire 6-bit file 			*
 *  start_at	int 	Indentation of embedded file in 6-bit		*
 *									*
 * Output parameters:							*
 *  *text	char	ASCII text retrieved from the header		*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * S. Jacobs/NCEP	 2/98	Fixed display of numbers in description	*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
    int i;
    /* the header is int byte 3-48 of the fax file that in this case
     * is stored in "plane".  It is stored as "Encoded CDC display code".
     * Retrieve and store in the text string... */

    /* for each of the bytes in the header, map it back to an ASCII value */   
    for (i=0;i<45;i++)
    {
        /* between 1 and 1a is A-Z in ASCII */
        if ( (plane[i+start_at+3] >= 1) && (plane[i+start_at+3] <= 0x1a) )
        {
            text[i] = (char)(plane[i+start_at+3] + 64);
        }
        else
        {
            /* between 1b and hex 0x24 is 1-9 */
            if ( (plane[i+start_at+3] >= 0x1b ) && 
                 (plane[i+start_at+3] <= 0x24) )
            {
                text[i] = (char)(plane[i+start_at+3]+21);
            }
            else   /* other wise, just make it a space */
            {
                text[i] = ' '; 
            }
        }
    } 
}


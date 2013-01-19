#include "geminc.h"
#include "cpgcmn.h"


/* look at the file of packed files, and find the start of the next file
either at or beyond the current map */

void pg_find_start ( char *plane, int peof, int *start_at )
/************************************************************************
 * pg_find_start							*
 *									*
 * This function locates the start of message in an NMC 6-bit file.	*
 *									*
 * pg_find_start ( plane, peof, start_at )				*
 *									*
 * Input parameters:							*
 *	*plane	char	Plane of data being searched for EOF		*
 *	peof	int	Plane end of file (bytes in plane)		*
 *									*
 * Output parameters:							*
 * 	*start_at int	Byte where plane starts				*
 **									*
 * Log:									*
 *	E. Wehner/EAi	 6/96		Created				*
 *	E. Wehner/EAi	11/96		Bit masked byte movements	*
 *	T. Piper/GSC	10/98		Prolog update			*
 ***********************************************************************/
{
    int quit = 0;

    while ( (!quit) && (*start_at < peof) )
    {
        if ( ( (plane[*start_at] & 0xff) == 0xff) && 
             ( (plane[*(start_at)+1] & 0xff) == 0xff) &&
             ( (plane[*(start_at)+2] & 0xff) == 0xff) )
            quit = 1;   /* this matches, exit */
        else
            *start_at += 1440;
    }

}


#include "geminc.h"
#include "cpgcmn.h"

void pg_find_stop ( char *plane, int peof, int start_at, int *stop_at )
/************************************************************************
 * pg_find_stop								*
 *									*
 * This function locates the end of message flag in an NMC 6-bit file.	*
 *									*
 * pg_find_stop ( plane, peof, start_at, stop_at )			*
 *									*
 * Input parameters:							*
 *	*plane	char	Plane of data being searched for EOF		*
 *	peof	int	Plane end of file (bytes in plane)		*
 * 	start_at int	Byte where plane starts				*
 *									*
 * Output parameters:							*
 * 	*stop_at int	Byte where plane stops				*
 **									*
 * Log:									*
 *	E. Wehner/EAi	 6/96	Created					*
 *	E. Wehner/EAi	11/96	Bitmasked byte movements		*
 *	T. Piper/GSC	10/98	Prolog update				*
 ***********************************************************************/
{
    int quit = 0;

    /* each fax record is 1440 bytes large.  A fax product HAS to be 
     * at least a few records large.  Adding 1440 bytes moves counter
     * to a point where it is known the record wont loop thereby 
     * identifying ityself as its own end of picture record.
     */

    *stop_at = start_at+1440;

    while ( (!quit) && (*stop_at < peof) )
    {
        if ( (  (plane[*stop_at] & 0xff) == 0xff) && 
             (  (plane[*stop_at+1] & 0xff) == 0xff))
            quit = 1;   /* this matches, exit */
        else
            *stop_at += 1440;
    }

}



#include "geminc.h"
#include "gemprm.h"

void clo_cmpwds ( char *dir, int *idir, char *cmpdir, int *icmp, int *iret )
/************************************************************************
 * clo_cmpwds                                                           *
 *                                                                      *
 * This function takes a 16-point compass direction (N, NNE, NE, etc.)  *
 * and returns the full word of that direction.                         *
 *                                                                      *
 * clo_cmpwds ( dir, idir, cmpdir, icmp, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dir    char    16-point compass direction                      *
 *	*idir	int	Length of 16-point compass direction		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *cmpdir char    Word compass direction  			*
 *	*icmp	int	Length of word compass direction		*
 *      *iret   int     Return value                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99           created                         *
 * M. Li/GSC		10/99		added a return value to check	*
 *					for invalid input; modified to	*
 *					be called FORTRAN		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( strcmp ( dir,"N") == 0 )
          strcpy ( cmpdir, "NORTH");
    else if ( strcmp ( dir,"NNE")  == 0)
          strcpy ( cmpdir, "NORTH NORTHEAST");
    else if ( strcmp ( dir,"NE")  == 0)
          strcpy ( cmpdir, "NORTHEAST");
    else if ( strcmp ( dir,"ENE") == 0 )
          strcpy ( cmpdir, "EAST NORTHEAST");
    else if ( strcmp ( dir,"E")  == 0)
          strcpy ( cmpdir, "EAST");
    else if ( strcmp ( dir,"ESE")  == 0)
          strcpy ( cmpdir, "EAST SOUTHEAST");
    else if ( strcmp ( dir,"SE")  == 0)
          strcpy ( cmpdir, "SOUTHEAST");
    else if ( strcmp ( dir,"SSE")  == 0)
          strcpy ( cmpdir, "SOUTH SOUTHEAST");
    else if ( strcmp ( dir,"S")  == 0)
          strcpy ( cmpdir, "SOUTH");
    else if ( strcmp ( dir,"SSW")  == 0)
          strcpy ( cmpdir, "SOUTH SOUTHWEST");
    else if ( strcmp ( dir,"SW")  == 0)
          strcpy ( cmpdir, "SOUTHWEST");
    else if ( strcmp ( dir,"WSW")  == 0)
          strcpy ( cmpdir, "WEST SOUTHWEST");
    else if ( strcmp ( dir,"W")  == 0)
          strcpy ( cmpdir, "WEST");
    else if ( strcmp ( dir,"WNW")  == 0)
          strcpy ( cmpdir, "WEST NORTHWEST");
    else if ( strcmp ( dir,"NNW")  == 0)
          strcpy ( cmpdir, "NORTH NORTHWEST");
    else if ( strcmp ( dir,"NW")  == 0)
          strcpy ( cmpdir, "NORTHWEST");
    else {
    	  *iret = -1;
          *cmpdir = '\0';	 
    }
    
    *icmp = (int)strlen( cmpdir );

    return;

}


#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_brdrec ( long strec, int *iret )
/************************************************************************
 * clo_brdrec								*
 *									*
 * This function reads a set of bounds coordinates into memory.  The	*
 * number of points read is stored in npBnd and the earth coordinates 	*
 * are stored in xpBnd and ypBnd.  Reading begins at starting record 	*
 * pointer strec. The bounds data file is assumed to already be open 	*
 * with global file pointer fpBnd.					*
 *									*
 * clo_brdrec ( strec, iret )						*
 *									*
 * Input parameters:							*
 *	strec		long		starting record			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= 0  - normal			*
 *					= -1 - unable to read record	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/01	Created					*
 ***********************************************************************/
{
int	npts, ier;
char	buff[128], *ptr;
float   lat1, lon1, lat2, lon2;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Seek to proper file location and read one line.
     *  This line contains the number of points, max (lat,lon) area
     *  and the first point of th ebound area.
     */
    cfl_seek ( fpBnd, strec, 0, &ier );
    cfl_trln( fpBnd, sizeof(buff), buff, &ier );

    npBnd = 0;
    sscanf ( buff, "%d %f %f %f %f %f %f", &npts,
                   &lat1, &lat2, &lon1, &lon2,
                   &(xpBnd[npBnd]), &(ypBnd[npBnd]) );
    npts /= 2;
    npBnd++;

    /*
     *  Loop until no more points, each time reading one line
     *  of points and decoding them.
     */
    while ( npBnd < npts )  {

        cfl_trln ( fpBnd, sizeof(buff), buff, &ier );

        /*
         *  Bounds points come in pairs of (lat,lon),
         *  i.e., they are never split between lines.
         */

        ptr = strtok ( buff, " " );
        while ( ptr != '\0' )  {

            sscanf ( ptr, "%f", &(xpBnd[npBnd]) );
            ptr = strtok ( NULL, " " );

            sscanf ( ptr, "%f", &(ypBnd[npBnd]) );
            ptr = strtok ( NULL, " " );

            npBnd++;

        }

    }

}

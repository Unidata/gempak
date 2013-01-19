#include "geminc.h"
#include "gemprm.h"
#include "proto_cmm.h"

void *cmm_malloc1d ( int nobj, int objsiz, int *iret )
/************************************************************************
 * cmm_malloc1d                                                         *
 *                                                                      *
 * This subroutine dynamically allocates a 1-dimensional array.		*
 *                                                                      *
 * cmm_malloc1d ( nobj, objsiz, iret )					*
 *                                                                      *
 * Input parameters:                                         		*
 *	nobj		int		Number of object		*
 *	objsiz		int		Size of each object		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *cmm_malloc1d	void		Pointer to the 1-D array	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -1 = memory allocation failed	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         01/06                                           *
 ************************************************************************/
{
    void *p;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
    if ( ( p = calloc ( nobj, objsiz ) ) == NULL ) *iret = -1;
    */
    if ( ( p = malloc ( nobj * objsiz ) ) == NULL ) *iret = -1;

    return p;
}

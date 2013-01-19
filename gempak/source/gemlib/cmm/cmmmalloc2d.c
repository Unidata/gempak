#include "geminc.h"
#include "gemprm.h"
#include "proto_cmm.h"

void **cmm_malloc2d ( int nrow, int ncol, int objsiz, int *iret )
/************************************************************************
 * cmm_malloc2d                                                         *
 *                                                                      *
 * This subroutine dynamically allocates a 2-dimensional array.		*
 *                                                                      *
 * cmm_malloc2d ( nrow, ncol, objsiz, iret )				*
 *                                                                      *
 * Input parameters:                                         		*
 *	nrow		int		Number of row			*
 *	ncol		int		Number of column		*	
 *	objsiz		int		Size of each object		*
 *                                                                      *
 * Output parameters:                                                   *
 *      **cmm_malloc2d	void		Pointer to the 2-D array	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -1 = memory allocation failed	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         01/06                                           *
 ************************************************************************/
{
    void **p1, *p2;
    char *c2;
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Allocate a single piece of memory to hold the whole 2-D array.
    if ( ( p2 = calloc ( nrow * ncol,  objsiz ) ) == NULL ) {
     */
    if ( ( p2 = malloc ( nrow * ncol * objsiz ) ) == NULL ) {
	*iret = -1;
	return NULL;
    }

    /*
     * Allocate the row pointers.
     */
    if ( ( p1 = (void **)malloc ( nrow * sizeof(void *) ) ) == NULL ) {
	*iret = -1;
        free ( p2 );
	return NULL;
    }

    /*
     * Assign the row pointers to point to the beginning of each row.
     */
    c2 = (char *)p2;
    for ( i = 0; i < nrow; i++ ) {
        p1[i] = (void *)c2;
	c2 += ( ncol * objsiz );
    }

    return p1;
}

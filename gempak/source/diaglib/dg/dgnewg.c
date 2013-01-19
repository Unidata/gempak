#include "dg.h"

void dg_newg ( const int *grdsiz, float **grdptr, int *iret )
/************************************************************************
 * dg_newg                                                              *
 *                                                                      *
 * This subroutine dynamically allocates the memory for a grid.	The	*
 * grid is initialized to default value.				*
 *                                                                      *
 * dg_newg ( grdsiz, grdptr, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*grdsiz		const int	Requested grid size		*
 *                                                                      *
 * Output parameters:                                                   *
 *	**grdptr	float		Pointer to allcated grid	*
 *      *iret           int             Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = memory allocation failed	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06                                           *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    float *grid;
/*----------------------------------------------------------------------*/
    *iret = 0;

/*
 * Allocate memory for the grid.
 */
    G_MALLOC ( grid, float, *grdsiz, "dg_newg - grid" );
    if ( grid == NULL ) {
        *iret = -73;
	*grdptr = NULL;
	return;
    }

    *grdptr = grid;

    return;
}

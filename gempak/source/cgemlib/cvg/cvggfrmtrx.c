#include "cvgcmn.h"


void cvg_gfrmtrx ( CVG_mtrx_t *matrix, int *size )
/************************************************************************
 * cvg_gfrmtrx								*
 *									*
 * This function frees any memory allocated for the matrix structure.	*
 *									*
 * Memory is allocated for this structure by a call to cvg_ggmtrx()	*
 *									*
 * cvg_gfrmtrx ( matrix, size )						*
 *									*
 * Input parameters:							*
 *	*matrix		CVG_mtrx_t	transformation matrix		*
 *	*size		int		size of matrix array		*
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/02	initial coding               		*
 * E. Safford/SAIC	04/02	fix Free Unallocated Memory (FUM) error	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    if ( matrix == NULL ) return;

    for (ii=0; ii< *size; ii++) {
	if ( matrix[ii].grpin  != NULL ) free ( matrix[ii].grpin );
	if ( matrix[ii].grpout != NULL ) free ( matrix[ii].grpout );
    }

    if ( *size > 0 ) {
        free ( matrix );
    }

    matrix = NULL;
    *size  = 0; 

}

/*=====================================================================*/

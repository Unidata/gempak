#include "nccmn.h"

void mrflhd ( int *iret )
/************************************************************************
 * mrflhd								*
 *									*
 * This subroutine reads the file header from the metafile.		*
 *									*
 * mrflhd ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if ( ( fread ( &meta_head, sizeof(nc_file_header),
			       1, meta_fp )
	     ) != (size_t)1 ) {
	    *iret = G_NOMETA;
	    return;
	}

}

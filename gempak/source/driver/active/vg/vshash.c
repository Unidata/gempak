#include	"vgcmn.h"

void vshash ( float *size, int *ihwid, int *ilwid, int *iret )
/************************************************************************
 * vshash								*
 *									*
 * This subroutine sets the hash mark attributes.			*
 *									*
 * vshash ( size, ihwid, ilwid, iret )					*
 *									*
 * Input parameters:							*
 *	*size		float		Hash mark size			*
 *	*ihwid		int		Hash mark line width		*
 *	*ilwid		int		Hash mark line spacing		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * I. Durham/GSC	 4/97						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	rhshsz = *size;
	khwid  = *ihwid;
	klwidh = *ilwid;

}

#include	"vgcmn.h"

void vstext ( int *mtxfn, int *mtxhw, float *ttxsz, int *mtxwid, 
			int *mbrdr, int *mrrotn, int *mjust, int *iret )
/************************************************************************
 * vstext								*
 *									*
 * This subroutine sets the text attributes.				*
 *									*
 * vstext ( mtxfn, mtxhw, ttxsz, mtxwid, mbrdr, mrrotn, mjust, iret )	*
 *									*
 * Input parameters:							*
 *	*mtxfn		int		Text font			*
 *	*mtxhw		int		Text sw/hw flag			*
 *	*ttxsz		float		Text size			*
 *	*mtxwid		int		Text width			*
 *	*mbrdr		int		Text border/blank fill flag	*
 *	*mrrotn		int		Text north-relative rot flag	*
 *	*mjust		int		Text justification		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * S. Jacobs/NCEP	 9/97	Changed calling sequence		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	ktxfn  = *mtxfn;
	ktxhw  = *mtxhw;
	rtxsz  = *ttxsz;
	ktxwid = *mtxwid;
	kbrdr  = *mbrdr;
	krrotn = *mrrotn;
	kjust  = *mjust;

}

#include "geminc.h"
#include "gemprm.h"
#define	IMGDEF
#include "imgdef.h"

void im_icmn ( int *hv, int *iret ) 	
/************************************************************************
 * im_icmn								*
 *									*
 * This subroutine sets some information in the image common header.	*
 *									*
 * im_icmn ( hv, iret )							*
 *									*
 * Input parameters:							*
 *	*hv		int		Pointer to first header variable*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP      6/04   From xsicmn				*
 * T. Piper/SAIC	 09/07	Replaced strncpy with cst_ncpy.		*
 ***********************************************************************/
{
int	ier;
char	*cptr;
/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;
/*
 *  Set variables - order is important here.
 *  Follow the order in the imgdef.h file.
 */
	imftyp = *hv++ ;
	imbank = *hv++ ;
	imdoff = *hv++ ;
	imldat = *hv++ ;
	imnpix = *hv++ ;
	imnlin = *hv++ ;
	imdpth = *hv++ ;
	rmxres = *(float *) hv++ ;
	rmyres = *(float *) hv++ ;
	imleft = *hv++ ;
	imtop  = *hv++ ;
	imrght = *hv++ ;
	imbot  = *hv++ ;
	rmxysc = *(float *) hv++ ;
	imbswp = *hv++ ;	
	imnchl = *hv++ ;
	imprsz = *hv++ ;
	imdcsz = *hv++ ;
	imclsz = *hv++ ;
	imlvsz = *hv++ ;
	imvald = *hv++ ;
	imrdfl = *hv++ ;
	immnpx = *hv++ ;
	immxpx = *hv++ ;
	imsorc = *hv++ ;
	imtype = *hv++ ;

	imradf = *hv++ ;
	rmbelv = *(float *) hv++ ;
	immode = *hv++ ;
	imdate = *hv++ ;
	imtime = *hv++ ;

	cptr = (char *)hv;

        cst_ncpy ( cmsorc, cptr, 20, &ier );
        cptr += 20;
        cst_ncpy ( cmtype, cptr, 8, &ier );
        cptr += 8;
        cst_ncpy ( cmstyp, cptr, 4, &ier );
        cptr += 4;
        cst_ncpy ( cmcalb, cptr, 4, &ier );
        cptr += 4;
	
}

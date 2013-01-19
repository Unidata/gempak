#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"

void im_dump ( int *iret )
/************************************************************************
 * im_dump								*
 *									*
 * This subroutine dumps the information in imgdef.h			*
 *									*
 * im_dump ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP      6/04   					*
 ***********************************************************************/
{
    *iret = 0;
    printf("\timftyp = %d\n", imftyp );
    printf("\timbank = %d\n", imbank );
    printf("\timdoff = %d\n", imdoff );
    printf("\timldat = %d\n", imldat );
    printf("\timnpix, imnlin, imdpth = %d %d %d\n", imnpix, imnlin, imdpth );
    printf("\trmxres, rmyres = %f %f\n", rmxres, rmyres );
    printf("\timleft, imtop, imrght, imbot = %d %d %d %d\n", 
	    imleft, imtop, imrght, imbot );
    printf("\timbswp = %d\n", imbswp );
    printf("\timnchl = %d\n", imnchl );
    printf("\timdcsz = %d\n", imdcsz );
    printf("\timclsz, imlvsz, imvald = %d %d %d\n", imclsz, imlvsz, imvald );
    printf("\timrdfl = %d\n", imrdfl );
    printf("\timmnpx, immxpx = %d %d\n", immnpx, immxpx );
    printf("\timradf = %d\n", imradf );
    printf("\trmbelv = %f\n", rmbelv );
    printf("\timmode = %d\n", immode );
    printf("\timdate, imtime = %d %d\n", imdate, imtime );
    printf("\tcmsorc = %s\n", cmsorc );
    printf("\tcmtype = %s\n", cmtype );
    printf("\tcmstyp = %s\n", cmstyp );
    printf("\tcmcalb = %s\n", cmcalb );
}

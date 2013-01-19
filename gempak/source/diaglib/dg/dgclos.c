#include "dg.h"

void dg_clos ( const int *idgfln, int *iret )
/************************************************************************
 * dg_clos								*
 *									*
 * This subroutine closes a grid file opened by the DG library.		*
 *									*
 *									*
 * dg_clos ( idgfln, iret )						*
 *									*
 * Input parameters:							*
 *	*idgfln		const int	DGFILE entry number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/96						*
 * D.W.Plummer/NCEP	 1/98	Bug fix-add internal grid check & reset	*
 * D.W.Plummer/NCEP	 1/98	Added reset for template processing	*
 * D.W.Plummer/NCEP	 4/98	Bug fix - idgfln refers to file number	*
 *				also, remove dgset=false		*
 * K. Brill/HPC		 2/04	Check nucode flag; update documentation	*
 *                              Do only initialization in nucode case	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int idg, k, ier;
/*----------------------------------------------------------------------*/
    *iret   = 0;
    idg = *idgfln;

    if ( _nfile.nucode == G_FALSE ) gd_clos ( &idg, &ier );
    _dgfile.idflnm[idg-1] = 0;
    _dgfile.tfirst[idg-1][0] = '\0';
    _dgfile.tlast[idg-1][0] = '\0';
    _dgfile.gdcur[idg-1][0] = '\0';
    _dgfile.tmpflg[idg-1] = G_FALSE;
    _dgfile.templt[idg-1][0] = '\0';
    _dgfile.tdattm[idg-1][0] = '\0';
    for ( k = 0; k < _dggrid.maxdgg; k++ ) {
	if ( _dggrid.ifiled[k] == idg ) _dggrid.ifiled[k] = 0;
    }

    return;
}

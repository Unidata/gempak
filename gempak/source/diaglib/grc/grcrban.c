#include "geminc.h"
#include "gemprm.h"

void grc_rban  ( float *anlblk, float *deltan, float *deltax, 
		float *deltay, float *gbnds, float *ebnds, float *dbnds, 
		int *iextnd, int *iret )
/************************************************************************
 * grc_rban								*
 *									*
 * This subroutine reads a Barnes analysis block.  All the bounds	*
 * are returned in the order:  lower left latitude; lower left		*
 * longitude; upper right latitude; upper right longitude.		*
 *									*
 * grc_rban  ( anlblk, deltan, deltax, deltay, gbnds, ebnds, dbnds,	*
 *            iextnd, iret )						*
 *									*
 * Input parameters:							*
 *	anlblk (llnanl)	float		Analysis block			*
 *									*
 * Output parameters:							*
 *	deltan		float		Station spacing			*
 *	deltax		float		Grid spacing in x dir		*
 *	deltay		float		Grid spacing in y dir		*
 *	gbnds [3]	float		Grid area bounds		*
 *	ebnds [3]	float		Extend area bounds		*
 *	dbnds [3]	float		Data area bounds		*
 *	iextnd [3]	int		Extend grid points		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = invalid analysis block	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/85						*
 * M. desJardins/GSFC	 9/88						*
 * K. Brill/GSC          4/90	Read for ANLBLK [0] = 2			*
 * M. desJardins/NMC	 4/91	Set DELTAX, DELTAY			*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_rban ( anlblk, deltan, deltax, deltay, gbnds, ebnds, dbnds,
	    iextnd, iret );

	return;
}

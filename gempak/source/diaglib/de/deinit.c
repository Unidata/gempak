#define DE_GLOBAL
#include "ensdiag.h"
#undef DE_GLOBAL

void de_init ( int *iret )
/************************************************************************
 * de_init								*
 *									*
 * This subroutine initializes the ensemble grid diagnostics. Note	*
 * that this subroutine is called by DG_NFIL. 				*
 *									*
 * de_init ( iret )							*
 *									*
 * Input parameters:							*
 *	NONE								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 1/05						*
 * T. Lee/SAIC		 6/05	Initialized iglist, igpntr, emvalu, and	*
 * 				enswts					*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * M. Li/SAIC		10/06	Initialize emvalu at MXMBRS and MXMBRS+1*
 * m.gamazaychikov/SAIC	01/08	Initialized iwlist, ewtval		*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Initialize ensemble file names and time stamps.
     */
    for ( i = 0; i < MXMBRS; i++ ) {
	_ensdiag.etmplt[i][0] = '\0';
	_ensdiag.enspth[i][0] = '\0';
	_ensdiag.ensfnm[i][0] = '\0';
	_ensdiag.etimes[i][0] = '\0';
	_ensdiag.enswts[i] = 0.;
	_ensdiag.emvalu[i] = 0.;
	_ensdiag.ewtval[i] = 0.;
	_ensdiag.iglist[i] = 0;
	_ensdiag.iwlist[i] = 0;
	_ensdiag.igpntr[i] = 0;
    }
    _ensdiag.emvalu[MXMBRS] = 0.;
    _ensdiag.emvalu[MXMBRS+1] = 0.;
    _ensdiag.ewtval[MXMBRS] = 0.;
    _ensdiag.ewtval[MXMBRS+1] = 0.;

    /*
     * Initialize ensemble specifications for GDFILE.
     */
    for ( i = 0; i < NGDFLS; i++ ) {
	_ensdiag.ensspc[i][0] = '\0';
    }

    /*
     * Initialize save strings for DGCMN contents.
     */
    _ensdiag.tmplsv[0] = '\0';
    _ensdiag.gpthsv[0] = '\0';
    _ensdiag.flnmsv[0] = '\0';
    _ensdiag.gdtmsv1[0] = '\0';
    _ensdiag.gdtmsv2[0] = '\0';
    _ensdiag.igtmsv[0] = '\0';

    /*
     * Initialize array of all arguments.
     */
    for ( i = 0; i < MXARGS; i++ ) {
	_ensdiag.allarg[i][0] = '\0';
    }

    /*
     * Set the index, ndxens,  which points to current ensemble which 
     * is specified by ensspc (ndxens). Set number of ensemble member
     * file names, nummbr. Initialize ID #, idgens, to name ENS_ output.
     */
    _ensdiag.ndxens = 0;
    _ensdiag.nummbr = 0;
    _ensdiag.idgens = 0;

    return;
}

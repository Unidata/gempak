#include "dg.h"

void dg_udig ( const char *ftype, const int *ignu, const int *ignv,
               int *idnum, char *stprm, int *iret )
/************************************************************************
 * dg_udig								*
 *									*
 * This subroutine updates the internal grid identifiers so grid(s)	* 
 * can be found when needed.  It also creates the substitution string 	*
 * (stprm) to replace the ftype function in the GFUNC/GDPFUN string.	*
 * It is called after DE_RSET.						*
 *									*
 * dg_udig ( iret )							*
 *									*
 * Input parameters:							*
 *	*ftype		const char	Type of function (LYR_ or ENS_)	*
 *	*ignu		const int	Grid number of scalar or u-comp	*
 *	*ignv		const int	Grid number of scalar or v-comp	*
 *									*
 * Input and output parameter:						*
 *	*idnum		int		Indentify number		*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string for ftype	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-22 = time is invalid		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 1/05						*
 * T. Lee/SAIC		 3/05	Added layer diagnostics			*
 * T. Lee/SAIC		 4/05	Initialized scalr			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char sid[5];
    int scalr;
    int ns, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check scalar specification.
     */
    scalr = ( *ignv == 0 ) ? G_TRUE : G_FALSE;
    (*idnum)++;
    cst_inch ( *idnum, sid, &ier );
    cst_lstr ( sid, &ns, &ier );

    if ( scalr == G_TRUE ) {
	_dggrid.ifiled[(*ignu)-1] 	= 1;
	_dggrid.leveld1[(*ignu)-1]	= 0;
	_dggrid.leveld2[(*ignu)-1]	= -1;
	_dggrid.ivcrdd[(*ignu)-1]	= 0;
	strcpy ( _dggrid.gparmd[(*ignu)-1], ftype );
	strcat ( _dggrid.gparmd[(*ignu)-1], "_" );
	strcat ( _dggrid.gparmd[(*ignu)-1], sid );
	_dggrid.iensmb[(*ignu)-1]	 = 0;
    } else {
	_dggrid.ifiled[(*ignu)-1] 	= 1;
	_dggrid.ifiled[(*ignv)-1]	= 1;
	_dggrid.leveld1[(*ignu)-1]	= 0;
	_dggrid.leveld1[(*ignv)-1]	= 0;
	_dggrid.leveld2[(*ignu)-1]	= -1;
	_dggrid.leveld2[(*ignv)-1]	= -1;
	_dggrid.ivcrdd[(*ignu)-1]	= 0;
	_dggrid.ivcrdd[(*ignv)-1]	= 0;
	strcpy ( _dggrid.gparmd[(*ignu)-1], "U" );
	strcat ( _dggrid.gparmd[(*ignu)-1], ftype );
	strcat ( _dggrid.gparmd[(*ignu)-1], "_" );
	strcat ( _dggrid.gparmd[(*ignu)-1], sid );
	strcpy ( _dggrid.gparmd[(*ignv)-1], "V" );
	strcat ( _dggrid.gparmd[(*ignv)-1], ftype );
	strcat ( _dggrid.gparmd[(*ignv)-1], "_" );
	strcat ( _dggrid.gparmd[(*ignv)-1], sid );
	_dggrid.iensmb[(*ignu)-1]	= 0;
	_dggrid.iensmb[(*ignv)-1]	= 0;
    }

    /*
     * Make the time stamps on the output grids match ingdtm.
     */
    grc_gtim ( _dginpt.ingdtm, _dgfile.tfirst[0], _dgfile.tlast[0],
	_dggrid.dttimd1[(*ignu)-1], _dggrid.dttimd2[(*ignu)-1], &ier );
    if ( (*ignv) != 0 ) {
        grc_gtim ( _dginpt.ingdtm, _dgfile.tfirst[0], _dgfile.tlast[0],
	    _dggrid.dttimd1[(*ignv)-1], _dggrid.dttimd2[(*ignv)-1], &ier );
    }
    if ( ier != 0 ) *iret = -22;

    strncpy ( stprm, ftype, 4 );
    strcpy ( &stprm[4], "_" );
    strcat ( stprm, sid );
    strcat ( stprm, "@0%NONE" );

    return;
}

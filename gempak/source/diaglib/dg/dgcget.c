#include "dg.h"

void dg_cget ( const char *ckey, char *cval, int *iret )
/************************************************************************
 * dg_cget                                                              *
 *                                                                      *
 * This subroutine queries, based on the key provided, the value of a	*
 * character type global variable.					*
 *                                                                      *
 * dg_cget ( ckey, cval, iret )                                  	*
 *                                                                      *
 * Input parameter:							*
 *	*ckey		const char	Variable key			*
 * Output parameters:                                                   *
 *	*cval		char		Variable value			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/05						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( strcmp ( ckey, "CPRJ" ) == 0 ) {
	strcpy ( cval, _dgfile.cprj );
    } else if ( strcmp ( ckey, "TFIRST_1" ) == 0 ) {
	strcpy ( cval, _dgfile.tfirst[0] );
    } else if ( strcmp ( ckey, "TFIRST_2" ) == 0 ) {
	strcpy ( cval, _dgfile.tfirst[1] );
    } else if ( strcmp ( ckey, "TFIRST_3" ) == 0 ) {
	strcpy ( cval, _dgfile.tfirst[2] );
    } else if ( strcmp ( ckey, "TLAST_1" ) == 0 ) {
	strcpy ( cval, _dgfile.tlast[0] );
    } else if ( strcmp ( ckey, "TLAST_2" ) == 0 ) {
	strcpy ( cval, _dgfile.tlast[1] );
    } else if ( strcmp ( ckey, "TLAST_3" ) == 0 ) {
	strcpy ( cval, _dgfile.tlast[2] );
    } else if ( strcmp ( ckey, "NTMPLT_1" ) == 0 ) {
	strcpy ( cval, _nfile.ntmplt[0] );
    } else if ( strcmp ( ckey, "NTMPLT_2" ) == 0 ) {
	strcpy ( cval, _nfile.ntmplt[1] );
    } else if ( strcmp ( ckey, "NTMPLT_3" ) == 0 ) {
	strcpy ( cval, _nfile.ntmplt[2] );
    } else if ( strcmp ( ckey, "GFLPTH_1" ) == 0 ) {
	strcpy ( cval, _nfile.gflpth[0] );
    } else if ( strcmp ( ckey, "GFLPTH_2" ) == 0 ) {
	strcpy ( cval, _nfile.gflpth[1] );
    } else if ( strcmp ( ckey, "GFLPTH_3" ) == 0 ) {
	strcpy ( cval, _nfile.gflpth[2] );
    } else if ( strcmp ( ckey, "CRTFNM_1" ) == 0 ) {
	strcpy ( cval, _nfile.crtfnm[0] );
    } else if ( strcmp ( ckey, "CRTFNM_2" ) == 0 ) {
	strcpy ( cval, _nfile.crtfnm[1] );
    } else if ( strcmp ( ckey, "CRTFNM_3" ) == 0 ) {
	strcpy ( cval, _nfile.crtfnm[2] );
    } else if ( strcmp ( ckey, "AFTRBR_1" ) == 0 ) {
	strcpy ( cval, _nfile.aftrbr[0] );
    } else if ( strcmp ( ckey, "AFTRBR_2" ) == 0 ) {
	strcpy ( cval, _nfile.aftrbr[1] );
    } else if ( strcmp ( ckey, "AFTRBR_3" ) == 0 ) {
	strcpy ( cval, _nfile.aftrbr[2] );
    } else if ( strcmp ( ckey, "CRTGDT1_1" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt1[0] );
    } else if ( strcmp ( ckey, "CRTGDT1_2" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt1[1] );
    } else if ( strcmp ( ckey, "CRTGDT1_3" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt1[2] );
    } else if ( strcmp ( ckey, "CRTGDT2_1" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt2[0] );
    } else if ( strcmp ( ckey, "CRTGDT2_2" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt2[1] );
    } else if ( strcmp ( ckey, "CRTGDT2_3" ) == 0 ) {
	strcpy ( cval, _nfile.crtgdt2[2] );
    } else if ( strcmp ( ckey, "INGLEV" ) == 0 ) {
        strcpy ( cval, _dginpt.inglev );
    } else if ( strcmp ( ckey, "INGDTM" ) == 0 ) {
        strcpy ( cval, _dginpt.ingdtm );
    } else if ( strcmp ( ckey, "ERRST" ) == 0 ) {
        strcpy ( cval, _dgerr.errst );
    }

    return;
}

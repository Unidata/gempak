#include "dg.h"

void dg_cset ( const char *ckey, const char *cval, int *iret )
/************************************************************************
 * dg_cset                                                              *
 *                                                                      *
 * This subroutine sets, based on the key provided, the character	*
 * value of a global variable.						*
 *                                                                      *
 * dg_cseT ( ckey, cval, iret )                                  	*
 *                                                                      *
 * Input parameter:							*
 *	*ckey		const char	Variable key			*
 *	*cval		const char	Variable value			*
 * Output parameters:                                                   *
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
	strcpy ( _dgfile.cprj , cval );
    } else if ( strcmp ( ckey, "TFIRST_1" ) == 0 ) {
	strcpy ( _dgfile.tfirst[0], cval );
    } else if ( strcmp ( ckey, "TFIRST_2" ) == 0 ) {
	strcpy ( _dgfile.tfirst[1], cval );
    } else if ( strcmp ( ckey, "TFIRST_3" ) == 0 ) {
	strcpy ( _dgfile.tfirst[2], cval );
    } else if ( strcmp ( ckey, "TLAST_1" ) == 0 ) {
	strcpy ( _dgfile.tlast[0], cval );
    } else if ( strcmp ( ckey, "TLAST_2" ) == 0 ) {
	strcpy ( _dgfile.tlast[1], cval );
    } else if ( strcmp ( ckey, "TLAST_3" ) == 0 ) {
	strcpy ( _dgfile.tlast[2], cval );
    } else if ( strcmp ( ckey, "NTMPLT_1" ) == 0 ) {
	strcpy ( _nfile.ntmplt[0], cval );
    } else if ( strcmp ( ckey, "NTMPLT_2" ) == 0 ) {
	strcpy ( _nfile.ntmplt[1], cval );
    } else if ( strcmp ( ckey, "NTMPLT_3" ) == 0 ) {
	strcpy ( _nfile.ntmplt[2], cval );
    } else if ( strcmp ( ckey, "GFLPTH_1" ) == 0 ) {
	strcpy ( _nfile.gflpth[0], cval );
    } else if ( strcmp ( ckey, "GFLPTH_2" ) == 0 ) {
	strcpy ( _nfile.gflpth[1], cval );
    } else if ( strcmp ( ckey, "GFLPTH_3" ) == 0 ) {
	strcpy ( _nfile.gflpth[2], cval );
    } else if ( strcmp ( ckey, "CRTFNM_1" ) == 0 ) {
	strcpy ( _nfile.crtfnm[0], cval );
    } else if ( strcmp ( ckey, "CRTFNM_2" ) == 0 ) {
	strcpy ( _nfile.crtfnm[1], cval );
    } else if ( strcmp ( ckey, "CRTFNM_3" ) == 0 ) {
	strcpy ( _nfile.crtfnm[2], cval );
    } else if ( strcmp ( ckey, "AFTRBR_1" ) == 0 ) {
	strcpy ( _nfile.aftrbr[0], cval );
    } else if ( strcmp ( ckey, "AFTRBR_2" ) == 0 ) {
	strcpy ( _nfile.aftrbr[1], cval );
    } else if ( strcmp ( ckey, "AFTRBR_3" ) == 0 ) {
	strcpy ( _nfile.aftrbr[2], cval );
    } else if ( strcmp ( ckey, "CRTGDT1_1" ) == 0 ) {
	strcpy ( _nfile.crtgdt1[0], cval );
    } else if ( strcmp ( ckey, "CRTGDT1_2" ) == 0 ) {
	strcpy ( _nfile.crtgdt1[1], cval );
    } else if ( strcmp ( ckey, "CRTGDT1_3" ) == 0 ) {
	strcpy ( _nfile.crtgdt1[2], cval );
    } else if ( strcmp ( ckey, "CRTGDT2_1" ) == 0 ) {
	strcpy ( _nfile.crtgdt2[0], cval );
    } else if ( strcmp ( ckey, "CRTGDT2_2" ) == 0 ) {
	strcpy ( _nfile.crtgdt2[1], cval );
    } else if ( strcmp ( ckey, "CRTGDT2_3" ) == 0 ) {
	strcpy ( _nfile.crtgdt2[2], cval );
    } else if ( strcmp ( ckey, "ERRST" ) == 0 ) {
    	strcpy ( _dgerr.errst, cval );
    } else if ( strcmp ( ckey, "INGDTM" ) == 0 ) {
        strcpy ( _dginpt.ingdtm, cval );
    }

    return;
}

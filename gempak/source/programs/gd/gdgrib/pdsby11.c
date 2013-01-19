#include "gdgrib.h"

void pds_by11 ( const int *level1, const int *level2, int *iscale,
                unsigned char *byte10, int *ibyt10, unsigned char *byte11,
		int *ibyt11, unsigned char *byte12, int *ibyt12, int *iret )
/************************************************************************
 * pds_by11								*
 *									*
 * This subroutine uses the GEMPAK LEVEL contents to set PDS bytes 11	*
 * and 12.								*
 *									*
 * pds_by11  ( level, iscale, byte10, ibyt10, byte11, ibyt11, byte12, 	*
 *							ibyt12, iret )	*
 *									*
 * Input parameters:							*
 *	*level1		const int	GEMPAK level			*
 *	*level2		const int	GEMPAK level			*
 *									*
 * Input and output parameters:						*
 *	*iscale		int		Power of 10 scaling from GEMPAK	*
 *	*byte10		unsigned char	CHAR vertical coordinate type	*
 *	*ibyt10		int		INT vertical coordinate type	*
 *									*
 * Output parameters:							*
 *	*byte11		unsigned char	Byte with GRIB LEVEL 1		*
 *	*ibyt11		int		Integer value of byte 11	*
 *	*byte12		unsigned char	Byte with GRIB LEVEL 2		*
 *	*ibyt12		int		Integer value of byte 12	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-87 = level value too large	*
 *					-88 = level is less than zero	*
 **									*
 * Log:									*
 * K. Brill/HPC		 9/99						*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    int lvl, ib1, ib2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *byte11 = (unsigned char)( 255 );
    *ibyt11 = 255;
    *byte12 = (unsigned char)( 255 );
    *ibyt12 = 255;

    if ( *level2 == -1 ) {
	/*
	 * Only one level to store in both bytes.
	 */
	lvl = (int)( *level1 / pow ( 10, *iscale ) );
	if ( lvl < 0 ) {
	    *iret = -88;
	    return;
	}
	ib2 = lvl % 256;
	ib1 = lvl / 256;
	if  ( ib2 > 255 ) {
	    *iret = -87;
	    return;
	} else {
	    *ibyt11 = ib1;
	    *ibyt12 = ib2;
	    *byte11 = (unsigned char) ( *ibyt11 );
	    *byte12 = (unsigned char) ( *ibyt12 );
	}
    } else {
	/*
	 * Reset vertical coordinate type for two-level type.
	 */
	if ( *ibyt10 == 100 ) {
	    *ibyt10 = 101;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 1;
	} else if ( *ibyt10 == 103 ) {
	    *ibyt10 = 104;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 2;
	} else if ( *ibyt10 == 105 ) {
	    *ibyt10 = 106;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 2;
	} else if ( *ibyt10 == 107 ) {
	    *ibyt10 = 108;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 2;
	} else if ( *ibyt10 == 109 ) {
	    *ibyt10 = 110;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 0;
	} else if ( *ibyt10 == 111 ) {
	    *ibyt10 = 112;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 0;
	} else if ( *ibyt10 == 113 ) {
	    *ibyt10 = 114;
	    *byte10 = (unsigned char) ( *ibyt10 );
	    *iscale = 0;
	} else if ( *ibyt10 == 128 ) {
	    *iscale = 1;
	}
	lvl = (int)( *level1 / pow ( 10, *iscale ) );
	if ( lvl < 0 ) {
	    *iret = -88;
	    return;
	}
	if ( lvl > 255 ) {
	    *iret = -87;
	    return;
	} else {
	    *ibyt11 = lvl;
	    *byte11 = (unsigned char) ( *ibyt11 );
	}
	lvl = (int)( *level2 / pow ( 10, *iscale ) );
	if ( lvl < 0 ) {
	    *iret = -88;
	    return;
	}
	if ( lvl > 255 ) {
	    *iret = -87;
	    return;
	} else {
	    *ibyt12 = lvl;
	    *byte12 = (unsigned char) ( *ibyt12 );
	}
    }

    return;
}

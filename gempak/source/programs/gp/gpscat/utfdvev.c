#include "geminc.h"
#include "gemprm.h"

void utf_dvev ( unsigned char *ptr, int *vdir, int *zt, int *zf,
		int *vlen, int *ipnt, int *jpnt, unsigned int *bits,
		int *add, int *iret )
/************************************************************************
 * utf_dvev                                                    	        *
 *									*
 * This function decodes the VEV vector (C6) header in a UTF file.	*
 *									*
 * utf_dvev ( ptr, vdir, zt, zf, vlen, ipnt, jpnt, bits, add, iret )	*
 * 									*
 * Input parameters:							*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*vdir		int		Vector direction		*
 *	*zt		int		Zoom threshold			*
 *	*zf		int		Zoom factor			*
 *	*vlen		int		Vector length			*
 *	*ipnt		int		I coordinate			*
 *	*jpnt		int		J coordinate			*
 *	*bits		unsigned int	Number of bits of data		*
 *	*add		int		Number of bytes in this record	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    *bits = 0;
    *add = 0;
    ptr += 1;

/*
**  Decode vector direction (bit 8-10 in the first word).
*/
    *vdir = *ptr >> 5;

/*
**  Decode zoom threshold (bit 11-12 in the first word).
*/
    *zt = (*ptr >> 3) & 0x03;

/*
**  Decode zoom factor (bit 13-15 in the first word).
*/
    *zf =  (((*ptr & 0x07) + 1) + 1) * (((*ptr & 0x07) + 1) + 1); 
    ptr += 1;

/*
**  Decode vector length (bit 0-1 in the second word).
*/
    *vlen = ((*ptr >> 6) + 1);

/*
**  Decode I coordinate.
*/
    *ipnt = (((*ptr << 8) + *(ptr + 1)) & 0x3fff);
    ptr += 2;

/*
**  Decode J coordinate.
*/
    *jpnt = ((*ptr << 8) + *(ptr + 1));
    ptr += 2;

/*
**  Decode number of bits of data in this record.
*/
    *bits = (*ptr << 8) + *(ptr + 1);

/*
**  Compute the number of bytes in the record.
*/
    *add = *bits / 8;

    if ( ( *bits % 8 ) > 0 )
	(*add)++;
    if ( ( *add % 2 ) != 0 )
	(*add)++;
    *add = *add + 8;

    ptr += 2;

}

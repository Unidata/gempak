#include "geminc.h"
#include "gemprm.h"

void utf_dvctr ( int shift_x, int shift_y, unsigned char *ptr, int *zd,
		 int *zt, int *zf, int *ipnt, int *jpnt, int *len,
		 int *add, int *iret )
/************************************************************************
 * utf_dvctr								*
 *									*
 * This function decodes the relative vector (C3) header in a UTF file.	*
 *									*
 * utf_dvctr ( shift_x, shift_y, ptr, zd, zt, zf, ipnt, jpnt, len, add,	*
 *	       iret )							*
 *									*
 * Input parameters:							*
 *	shift_x		int		X shift factor			*
 *	shift_y		int		Y shift factor			*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*zd		int		Zoom disable			*
 *	*zt		int		Zoom threshold			*
 *	*zf		int		Zoom factor			*
 *	*ipnt		int		I coordinate			*
 *	*jpnt		int		J coordinate			*
 *	*len		int		Number of words in this record	*
 *	*add		int		Total # of bytes in this record	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    ptr += 1;

/*
**  Decode zoom disable (bit 10 in the first word).
*/
    *zd = (*ptr >> 5) & 0x01;

/*
**  Decode zoom threshold (bit 11-12 in the first word).
*/
    *zt = (*ptr >> 3) & 0x03;

/*
**  Decode zoom factor (bit 13-15 in the first word).
*/
    if  ( ( ( *ptr & 0x20 ) != 32 ) || ( shift_x ) )
	*zf =  1;
    else
	*zf =  (((*ptr & 0x07) + 1) + 1) * (((*ptr & 0x07) + 1) + 1);
    ptr += 1;

/*
**  Decode I coordinate.
*/
    *ipnt = ((*ptr << 8) + *(ptr + 1)) << shift_x;
    ptr += 2;

/*
**  Decode J coordinate.
*/
    *jpnt = ((*ptr << 8) + *(ptr + 1)) << shift_y;
    ptr += 2;

/*
**  Decode length of record (# of words).
*/
    *len = (*ptr << 8) + *(ptr + 1);
    *add = *len * 2 + 8;

    ptr += 2;

}

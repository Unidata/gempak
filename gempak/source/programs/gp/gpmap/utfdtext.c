#include "geminc.h"
#include "gemprm.h"

void utf_dtext ( int offflg, int shift_x, int shift_y, int zm,
		 unsigned char *endbuf, unsigned char *ptr, int *g,
		 int *b, int *rb, int *zt, int *pltfil, int *zf,
		 int *chrsiz, int *ipnt, int *jpnt, int *len, int *add,
		 int *iret )
/************************************************************************
 * utf_dtext								*
 *									*
 * This function decodes both text character (C8) and (C5) headers in a	*
 * UTF file.								*
 *									*
 * utf_dtext ( offflg, shift_x, shift_y, zm, endbuf, ptr, g, b, rb, zt,	*
 *		pltfil,	zf, chrsiz, ipnt, jpnt, len, add, iret )	*
 *									*
 * Input parameters:							*
 *	offflg		int		Offset vs regular text flag	*
 *	shift_x		int		X shift factor			*
 *	shift_y		int		Y shift factor			*
 *	zm		int		Zoom threshold (from user)	*
 *	*endbuf		unsigned char	End of buffer			*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*g		int		Graphic vs non-graphic record	*
 *	*b		int		Block character mode		*
 *	*rb		int		Reverse block mode		*
 *	*zt		int		Zoom threshold			*
 *	*pltfil		int		Plot filter flag		*
 *	*zf		int		Zoom factor			*
 *	*chrsiz		int		Character size			*
 *	*ipnt		int		I coordinate			*
 *	*jpnt		int		J coordinate			*
 *	*len		int		# of bytes of data in record	*
 *	*add		int		Total # of bytes in this record	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
    unsigned char	*tmp;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *len = 0;
    ptr += 1;

/*
**  Decode graphic vs non-graphic flag (bit 8 in the first word).
*/
    *g = *ptr & 0x80;

/*
**  Decode block character mode (bit 9 in the first word).
*/
    *b = *ptr & 0x40;

/*
**  Decode reverse block mode (bit 10 in the first word).
*/
    *rb = *ptr & 0x20;

/*
**  Decode zoom threshold (bit 11-12 in the first word).
*/
    *zt = *ptr & 0x18;

/*
**  Determine plot filter.
*/
    *pltfil = G_FALSE;
    switch (zm) {
	case 3:
	    *pltfil = G_TRUE;
	    break;
	case 2:
	    if ( *zt <= 16 )
		*pltfil = G_TRUE;
	    break;
	case 1:
	    if ( *zt <= 8 )
		*pltfil = G_TRUE;
	    break;
	case 0:
	    if ( *zt == 0 )
		*pltfil = G_TRUE;
	    break;
    }

/*
**  Decode zoom factor (bit 13-15 in the first word).
*/
    *zf = *ptr & 0x07;
    ptr += 1;

/*
**  Decode character size (bit 0-1 in second word).
*/
    *chrsiz = ( *ptr >> 6 ) & 0x03;

/*
**  Decode I coordinate.
*/
    *ipnt = ((((*ptr << 8) + *(ptr + 1)) & 0x3fff) << shift_x);
    ptr += 2;

/*
**  Decode J coordinate.
*/
    *jpnt = (((*ptr << 8) + *(ptr + 1)) << shift_y);
    ptr += 2;

    if ( offflg ) {
	*add = 8;
	ptr += 2;
    }
    else
	*add = 6;

    tmp = ptr;

/*
**  Determine the number of bytes of data in record.
*/
    while ( ( *tmp < 128 ) && ( tmp < endbuf ) ) {
	(*len)++;
	tmp++;
    }
    *add = *add + *len;

}

#include "geminc.h"
#include "gemprm.h"

void utf_gphgd ( unsigned char *ptr, int siz, int *shift_x, int *shift_y,
		 int *pi, int *gs, unsigned int *imax, unsigned int *jmax,
		 unsigned int *imaxad, unsigned int *jmaxad, int *day,
		 int *month, int *year, int *time, int *pdc, int *add,
		 int *iret )
/************************************************************************
 * utf_gphgd								*
 *									*
 * This function decodes the graphic product definition (C1) header	*
 * in a UTF file.							*
 *									*
 * utf_gphgd ( ptr, siz, shift_x, shift_y, pi, gs, imax, jmax, imaxad,	*
 *		jmaxad, day, month, year, time, pdc, add, iret )	*
 *									*
 * Input parameters:							*
 *	*ptr		unsigned char	Position in buffer		*
 *	siz		int		Number of bytes in buffer	*
 *									*
 * Output parameters:							*
 *	*shift_x	int		X shift factor			*
 *	*shift_y	int		Y shift factor			*
 *	*pi		int		Projection indicator		*
 *	*gs		int		Geography scale			*
 *	*imax		unsigned int	Horizontal graphic width	*
 *	*jmax		unsigned int	Vertical graphic width		*
 *	*imaxad		unsigned int	Adjusted horizontal graph width	*
 *	*jmaxad		unsigned int	Adjusted vertical graphic width	*
 *	*day		int		Day product is valid		*
 *	*month		int		Month product is valid		*
 *	*year		int		Year product is valid		*
 *	*time		int		Time product is valid		*
 *	*pdc		int		Product display code		*
 *	*add		int		Size of record in bytes		*
 *	*iret		int		Return code			*
 *					 -7 = buffer is not stripped	*
 **									*
 * Log:									*
 * D. Keiser/GSC	 7/96						*
 * D. Keiser/GSC	12/96	General clean up			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    *shift_x = *shift_y = 0;
    *add = 12;

    if  ( siz == 0 || *ptr != 193 )
	*iret = -7;
    else {
/*
**	Decode projection indicator, geography scale, and graphic widths.
*/
	ptr += 1;
	*pi = *ptr;
	ptr += 1;
	*gs = (*ptr << 8) + *(ptr + 1);
	ptr += 2;
	*imax = (*ptr << 8) + *(ptr + 1);
	ptr += 2;
	*jmax = (*ptr << 8) + *(ptr + 1);

/*
**	Set scaling factors if necessary.
*/
	*imaxad = *imax;
	*jmaxad = *jmax;

	while ( *imaxad < 4096 ) {
	    *imaxad = *imaxad * 2;
	    *shift_x += 1;
	}

	while ( *jmaxad < 3072 ) {
	    *jmaxad = *jmaxad * 2;
	    *shift_y += 1;
	}

/*
**	Decode day, month, year, time, and product display code.
*/
	ptr += 2;
	*day = (*ptr >> 3) & 0x1f;
	*month = ((*(ptr + 1) >> 7) & 0x01) +
					(((*ptr & 0x07) << 1 ) & 0x0e);
	ptr += 1;
	*year = *ptr & 0x7f;
	ptr += 1;
	*time = ((*(ptr + 1) >> 4) & 0x0f) + ((*ptr << 4) & 0xff0);
	ptr += 1;
	*pdc = *ptr & 0x0f;
    }

}

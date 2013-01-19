#include "geminc.h"
#include "gemprm.h"

void utf_dump ( unsigned char *buffer, int size, int sbyte, int nout,
		char *ans, int zm, int shift_x, int shift_y,
		int gphflg, int *iret )
/************************************************************************
 * utf_dump								*
 *									*
 * This function dumps the contents of a buffer from a UTF file. The	*
 * function can also return decoded header information. However, a range*
 * of bytes including a record type must be specified for that record to*
 * be decoded.								*
 *									*
 * utf_dump ( buffer, size, sbyte, nout, ans, zm, shift_x, shift_y,	*
 *		gphflg, iret )						*
 *									*
 * Input parameters:							*
 *	*buffer		unsigned char	Contents of UTF buffer		*
 *	size		int		Number of bytes in buffer (size)*
 *	sbyte		int		Byte to start outputting from	*
 *	nout		int		Number of bytes to output	*
 *	*ans		char		Output actual contents of buffer*
 *	zm		int		Data density			*
 *	shift_x		int		X shift factor			*
 *	shift_y		int		Y shift factor			*
 *	gphflg		int		Graphic product definition flag	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  1 = no record type in range	*
 *					 -7 = buffer not stripped	*
 *					 -9 = # bytes to output too big	*
 *					-10 = invalid byte # to	start	*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * D. Keiser/GSC	 1/97	Change er_wmsg to er_lmsg		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 ***********************************************************************/
{
    int			ier1, pltfil, offflg;
    int			add, ier, zd, zt, zf, ipnt, i;
    int			jpnt, len, flag, vdir, vlen, g, b, rb, chrsiz, j;
    int			level;
    unsigned int	bits;
    char		grp[4], string[2];
    unsigned char	*outpos, *start, *endbuf;
/*---------------------------------------------------------------------*/
    *iret = 0;
    flag = G_FALSE;
    strcpy(grp, "UTF");
    strcpy(string, " ");
    level = 2;
    if  ( ans[0] == 'y' || ans[0] == 'Y' )
	flag = G_TRUE;

/*
**  Dump the contents of the buffer.
*/
    if  ( size == 0 )
	*iret = -7;
    else if ( ( sbyte < 0 ) || ( sbyte > size ) )
	*iret = -10;
    else if ( nout > (size - sbyte + 1) )
	*iret = -9;
    else {
	start = buffer + sbyte;
	endbuf = buffer + size;
	outpos = start + nout;
	buffer = start;
	printf ( "\n\n" );
	for ( i = sbyte; start < outpos; start++, i++ ) {
	    printf("byte# %5i  decimal %5u  hex%5x\n",
						i, *start, *start);
	}
	printf ( "\n" );

	j = 0;
	while ( ( buffer < outpos ) && ( flag ) ) {

	    if  ( *buffer == 195 ) {

		utf_dvctr( shift_x, shift_y, buffer, &zd, &zt, &zf,
			   &ipnt, &jpnt, &len, &add, &ier );

		if  ( ier != 0 ) {
		    er_lmsg( &level, grp, &ier, string, &ier1,
				strlen(grp), strlen(string) );
		    *iret = ier;
		    return;
		}
		else {
		    printf( "\nRelative Vector C3\n" );
		    printf( "Zoom disable: %i\n", zd );
		    printf( "Zoom threshold: %i\n", zt );
		    printf( "Zoom factor: %i\n", zf );
		    printf( "I Coordinate: %i\n", ipnt );
		    printf( "J Coordinate: %i\n", jpnt );
		    printf( "# words of data in this C3 record: %i\n",
							len );
		    printf( "# of bytes in this C3 record: %u\n\n",
								add );
		    buffer += add;
		    j++;
		}
	    }

	    else if ( *buffer == 198 ) {

		utf_dvev( buffer, &vdir, &zt, &zf, &vlen, &ipnt, &jpnt,
			  &bits, &add, &ier );

		if  ( ier != 0 ) {
		    er_lmsg( &level, grp, &ier, string, &ier1,
				strlen(grp), strlen(string) );
		    *iret = ier;
		    return;
		}
		else {
		    printf( "\nVEV Vector C6\n" );
		    printf( "Vector direction: %i\n", vdir );
		    printf( "Zoom threshold: %i\n", zt );
		    printf( "Zoom factor: %i\n", zf );
		    printf( "Vector length: %i\n", vlen );
		    printf( "I Coordinate: %i\n", ipnt );
		    printf( "J Coordinate: %i\n", jpnt );
		    printf( "# bits of data in this C6 record: %u\n",
								bits );
		    printf( "# of bytes in this C6 record: %i\n\n",
								add );
		    buffer += add;
		    j++;
		}
	    }

	    else if ( *buffer == 197 ) {
		offflg = G_FALSE;

		utf_dtext( offflg, shift_x, shift_y, zm, endbuf, buffer,
			   &g, &b, &rb, &zt, &pltfil, &zf, &chrsiz,
			   &ipnt, &jpnt, &len, &add, &ier );

		if  ( ier != 0 ) {
		    er_lmsg( &level, grp, &ier, string, &ier1,
				strlen(grp), strlen(string) );
		    *iret = ier;
		    return;
		}
		else {
		    printf( "\nRegular Text C5\n" );
		    if  ( g != 0 )
			printf( "Non graphic C5 record\n" );
		    else
			printf( "Graphic C5 record\n" );
		    if  ( b != 0 )
			printf( "Block character mode: ON\n" );
		    else
			printf( "Block character mode: OFF\n" );
		    if  ( rb != 0 )
			printf( "Reverse block character mode: ON\n" );
		    else
			printf( "Reverse block character mode: OFF\n" );
		    printf( "Zoom threshold: %i\n", zt );
		    printf( "Zoom factor: %i\n", zf );
		    printf( "Character size: %i\n", chrsiz );
		    printf( "I Coordinate: %i\n", ipnt );
		    printf( "J Coordinate: %i\n", jpnt );
		    printf( "# words of data in this C5 record: %i\n",
							len );
		    printf( "# of bytes in this C5 record: %u\n\n",
								add );
		    buffer += add;
		    j++;
		}
	    }

	    else if ( *buffer == 200 ) {
		offflg = G_TRUE;

		utf_dtext( offflg, shift_x, shift_y, zm, endbuf, buffer,
			   &g, &b, &rb, &zt, &pltfil, &zf, &chrsiz,
			   &ipnt, &jpnt, &len, &add, &ier );

		if  ( ier != 0 ) {
		    er_lmsg( &level, grp, &ier, string, &ier1,
				strlen(grp), strlen(string) );
		    *iret = ier;
		    return;
		}
		else {
		    printf( "\nOffset Text C8\n" );
		    if  ( g != 0 )
			printf( "Non graphic C8 record\n" );
		    else
			printf( "Graphic C8 record\n" );
		    if  ( b != 0 )
			printf( "Block character mode: ON\n" );
		    else
			printf( "Block character mode: OFF\n" );
		    if  ( rb != 0 )
			printf( "Reverse block character mode: ON\n" );
		    else
			printf( "Reverse block character mode: OFF\n" );
		    printf( "Zoom threshold: %i\n", zt );
		    printf( "Zoom factor: %i\n", zf );
		    printf( "Character size: %i\n", chrsiz );
		    printf( "I Coordinate: %i\n", ipnt );
		    printf( "J Coordinate: %i\n", jpnt );
		    printf( "# words of data in this C8 record: %i\n",
							len );
		    printf( "# of bytes in this C8 record: %u\n\n",
								add );
		    buffer += add;
		    j++;
		}
	    }

	    else
		buffer++;
	}
	if  ( ( j == 0 ) && ( flag ) )
	    *iret = 1;
    }

}

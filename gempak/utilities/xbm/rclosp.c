#include "faxcmn.h"
#include "xbm.h"
#define BitsPerByte	8

void rclosp ( int *iret )
/************************************************************************
 * rclosp								*
 *                                                                      *
 * This subroutine writes the bitmap file.				*
 *                                                                      *
 * rclosp ( iret )							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log                                                                  *
 * M. Linda/GSC		 6/97	XBM version based on the FAX driver	*
 ***********************************************************************/
{
	char	bigbuf [ 1000 ], tbuf [ 80 ], dummy [ 133 ];
	FILE	*fp;
	int	i, lct, bcnt, exist, icnt;
	long	flen;


/*--------------------------------------------------------------------*/
    if  ( opnfil ) {

	opnfil = G_FALSE ;
/*
 *	Create a unique file name.
 */
	icnt  = 0;
	exist = G_TRUE;
	while ( exist && icnt < 1000 ) {
	    sprintf ( filnam, "bitmap%03d", icnt );
	    cfl_inqr ( filnam, NULL, &flen, dummy, iret );
	    if ( *iret !=  G_NORMAL ) exist= G_FALSE;
	    icnt ++;
	}
/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;
/*
 *	Create a header for the bitmap file.
 */
	strcpy  ( bigbuf, "\0" );

	sprintf ( tbuf, "#define %s_width  %i\n", filnam, bpscan );
	strcat  ( bigbuf, tbuf );

	sprintf ( tbuf, "#define %s_height %i\n", filnam, num_scans );
	strcat  ( bigbuf, tbuf );

	sprintf ( tbuf, "static unsigned char %s_bits[] = {\n", filnam );
	strcat  ( bigbuf, tbuf );
/*
 *	Convert pixmap to bitmap file format; stage it in a big buffer.
 */
	bcnt = ( bpscan * num_scans ) / BitsPerByte;
	lct = 0;

	for ( i = 0 ; i < bcnt ; i ++ ) {

	    if  ( lct == 0 ) {
		strcat  ( bigbuf, "   " );
	    }

	    if  ( i < ( bcnt - 1 ) ) {
		sprintf ( tbuf, "0x%2.2x, ", ( pixmap [ i ] & 0xff ) );
	    }
	    else {
		sprintf ( tbuf, "0x%2.2x };\n", ( pixmap [ i ] & 0xff ) );
	    }

	    strcat ( bigbuf, tbuf );
            lct ++;

            if  ( ( lct > 11 ) && ( i < ( bcnt - 1 ) ) ) {
		strcat ( bigbuf, "\n" );
                lct = 0;
	    }
        }

/*
 *	Write the big buffer with header and pixmap to the bitmap file.
 */
	fp = cfl_wopn ( filnam, iret );
        if ( *iret != G_NORMAL ) { return; }

	cfl_writ ( fp, (int) strlen ( bigbuf ), bigbuf, iret );
	if ( *iret != G_NORMAL ) { return; }

	cfl_clos ( fp, iret );
    }
}

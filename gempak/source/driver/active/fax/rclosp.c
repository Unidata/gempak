#include "faxcmn.h"

void rclosp ( int *iret )
/************************************************************************
 * rclosp								*
 *									*
 * This subroutine closes the raster image and writes the final image	*
 * to a file.								*
 *									*
 * If the user wants a 6-bit FAX output, the raster image is then	*
 * converted.								*
 *									*
 * Note: If the user does not enter a wheel and subset numbers, then	*
 *	 the output file that is created can be read by the X11		*
 *	 programs "bitmap" or "xv". This capability is used only for	*
 *	 testing the driver outside of the 6-bit FAX environment.	*
 *									*
 * rclosp  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAI	 4/96						*
 * E. Wehner/EAi	 1/97	Switched wheel and subset		*
 * E. wehner/EAi	 3/97	Removed xsize/ysize in call to ras26bit	*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Added "bitmap" output for testing	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 ***********************************************************************/
{

	char	bigbuf[MAXSIZ], tbuf[80], tbuf2[80];
	FILE	*fp;
	int	i, lct;

/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Dump the raster image to a file, then compress the file to 6 bit.
 */
	if  ( opnfil )  {

	    opnfil = G_FALSE; 
	    isfont = 0;

	    if  ( faxflg )  {

/* 
 *	    	If the user wants a 6-bit output file, convert the
 *		entire raster image.
 */
		rsdump ( iret );
		if  ( *iret != G_NORMAL )  return;

		ras26bit ( iret );
	    }
	    else  {
/*
 *		Otherwise, create a file for use with the X11 program
 *		"bitmap".
 *
 *		Create a header for the bitmap file.
 */
		strcpy  ( bigbuf, "\0" );

		sprintf ( tbuf, "#define %s_width  %i\n",
			  filnam, kbit[0] );
		strcat  ( bigbuf, tbuf );

		sprintf ( tbuf, "#define %s_height %i\n",
			  filnam, klin[0] );
		strcat  ( bigbuf, tbuf );

		sprintf ( tbuf, "static unsigned char %s_bits[] = {\n",
			  filnam );
		strcat  ( bigbuf, tbuf );

/*
 *		Convert raster image to bitmap file format; stage it
 *		in a big buffer.
 */
		lct = 0;

		for ( i = 0 ; i < msize ; i ++ ) {

		    if  ( lct == 0 ) {
			strcpy  ( tbuf2, "   " );
		    }

		    if  ( i < ( msize - 1 ) ) {
			sprintf ( tbuf, "0x%2.2x, ",
				  ( rasimg [ i ] & 0xff ) );
		    }
		    else {
			sprintf ( tbuf, "0x%2.2x };\n",
				  ( rasimg [ i ] & 0xff ) );
		    }

		    strcat ( tbuf2, tbuf );
		    lct ++;

		    if  ( ( lct > 11 ) && ( i < ( msize - 1 ) ) ) {
			strcat ( tbuf2, "\n" );
			strcat ( bigbuf, tbuf2 );
			lct = 0;
		    }
		}

		if  ( lct != 0 )  {
		    strcat ( bigbuf, tbuf2 );
		}

/*
 *		Write the big buffer with header and raster image to
 *		the bitmap file.
 */
		fp = cfl_wopn ( filnam, iret );
		if ( *iret != G_NORMAL ) { return; }

		cfl_writ ( fp, strlen ( bigbuf ), (unsigned char *)bigbuf, iret );
		if ( *iret != G_NORMAL ) { return; }

		cfl_clos ( fp, iret );

	    }

	}

}

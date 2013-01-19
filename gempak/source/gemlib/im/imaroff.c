#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define im_aroff	im_aroff_
#endif

void	im_aroff ( char *imgfil, int *imdoff, int *ncomm, int *iret )	
{
	long foff;
	unsigned char buf[8];
	int nbin;
	static char defdir[]=" ";
	FILE *fp;

	*iret = 0;

	fp = cfl_ropn ( imgfil, defdir, iret );

	if ( *iret != 0 ) {
	    return;
        }

	foff = *imdoff + *ncomm;
	cfl_seek ( fp, foff, SEEK_SET, iret );

        if ( *iret == 0 ) {
	    cfl_read ( fp, 8, buf, &nbin, iret);
	    if ( ( *iret == 0 ) && ( nbin == 8 ) ) {
		/*
		** Check PNG magic word (8 first bytes):
		** http://www.libpng.org/pub/png/spec/1.2/PNG-Rationale.html
		** decimal     -  137 80 78 71 13 10   26 10
		** hexadecimal -   89 50 4e 47 0d 0a   1a 0a
		** ASCII       - \211  P  N  G \r \n \032 \n
		*/
		if ( buf[0] == (unsigned char)137 &&
		    buf[1] == (unsigned char) 80 &&
		    buf[2] == (unsigned char) 78 &&
		    buf[3] == (unsigned char) 71 &&
		    buf[4] == (unsigned char) 13 &&
		    buf[5] == (unsigned char) 10 &&
		    buf[6] == (unsigned char) 26 &&
		    buf[7] == (unsigned char) 10     ) {             /* PNG test passed */

		    /* set imdoff to new location */
		    *imdoff = (int)foff;
		}

	    }
        }

	cfl_clos ( fp, iret);
}

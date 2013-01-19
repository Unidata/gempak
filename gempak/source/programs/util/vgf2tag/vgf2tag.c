#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"

int main ( int argc, char **argv )
/************************************************************************
 * VGF2TAG								*
 *									*
 * This program converts a file of pgen elements in <tag>value format	*
 * into a standard VGF file.						*
 *									*
 **									*
 * Log: 								*
 * D.W.Plummer/NCEP	06/03						*
 * S. Jacobs/NCEP	 5/05	Increased the size of the buffer	*
 * S. Jacobs/NCEP	12/09	Changed cvg_rdrecnoc to cvg_rdjrecnoc	*
 * S. Jacobs/NCEP	 1/12	Changed cvg_jrdrecnoc to cvg_rdrecnoc	*
 ***********************************************************************/
{
int		ne, more, ier;
int		wrtflg, curpos;
VG_DBStruct	el;
char		asfn[32], vgfn[128];
char		comment[8]={"!\n"}, buffer[10000];
char		infile[128], ifname[128];
long		ifilesize;
int		nw;
FILE		*ifptr, *ofptr;
/*---------------------------------------------------------------------*/
    
    printf ( "Enter the VGF file name to convert from :\n" );
    scanf ( " %s", vgfn );
    printf ( "Enter the ASCII file name to write tags to :\n" );
    scanf ( " %s", asfn );

    wrtflg = 0;
    cvg_open ( vgfn, wrtflg, &(ifptr), &ier );
    if ( ier != 0 )  {
        printf("Error opening VGF file %s\n", infile );
        exit (0);
    }
    cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

    ofptr = cfl_wopn ( asfn, &ier );

    nw = 0;
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

	cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );

        if ( ier < 0 )  {
            more = G_FALSE;
        }
        else if ( el.hdr.recsz > 0 )  {
	    curpos += el.hdr.recsz;
            cvg_v2t ( &el, buffer, &ier );
	    if ( ier == 0 )  {
	        cfl_writ ( ofptr, (int)strlen("!\n"), (unsigned char*)comment, &ier );
		strcat ( buffer, "\n" );
	        cfl_writ ( ofptr, (int)strlen(buffer), (unsigned char*)buffer, &ier );
		if ( ier == 0 )  nw++;
	    }
	}


    }
    printf("Number of elements encoded and written successfully = %d\n", nw );

    return ( 0 );

}

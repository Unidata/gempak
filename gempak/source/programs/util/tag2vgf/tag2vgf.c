#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

int main ( int argc, char **argv )
/************************************************************************
 * TAGTOVGF								*
 *									*
 * This program converts a file of pgen elements in <tag>value format	*
 * into a standard VGF file.						*
 *									*
 **									*
 * Log: 								*
 * D.W.Plummer/NCEP	06/03						*
 * S. Jacobs/NCEP	 5/05	Increased size of the buffer		*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function    *
 * Q. Zhou/Chu          06/12   Added converting head only file         *
 ***********************************************************************/
{
int		loc, ier;
VG_DBStruct	el;
char	fn[32], vgfn[128], buffer[10000], *str;
int		nw;
FILE	*fp;

/*---------------------------------------------------------------------*/
    
    printf ( "Enter the ASCII file name to read element from :\n" );
    scanf ( " %s", fn );
    printf ( "Enter the VGF file name to write element to :\n" );
    scanf ( " %s", vgfn );

    fp = cfl_ropn ( fn, "", &ier );
    nw = 0;
    while ( ier == 0 )  {

	str = fgets ( buffer, sizeof(buffer), fp );

	if ( str != (char *)NULL )  {
	    if ( buffer[0] != '!' )  {
               cvg_t2v ( buffer, &el, &ier );
 
		if ( ier == 0 )  {
		  if ( el.hdr.vg_type != FILEHEAD_ELM )  {
	            cvg_writefD ( &el, -1, el.hdr.recsz, vgfn, &loc, &ier );
 	            if ( ier != 0 )  {
		        printf("ERROR writing element to file %s\n", vgfn );
	            }
	            else  {
		        nw++;
	            }
	          }
	        }
		else  {
		    printf("ERROR processing buffer=\"%s\"\n", buffer );
		}
	    }
	}
	else  {
	    ier = -1;
	}
    }

    if ( nw==0) { 
    	cvg_crvgf(vgfn, &ier);
    }
    printf("Number of elements decoded and written successfully = %d\n", nw );

    return ( 0 );

}

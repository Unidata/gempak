#include "geminc.h"
#include "gemprm.h"
#include "proto_vf.h"

int main ( int argc, char *argv[] )
/************************************************************************
 * spctxt								*
 *                                                                      *
 * This program reads watch format text product created by NMAP and     *
 * creates the SAW, SEV and SEL SPC weather watch text products.        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          7/99	Created					*
 * M. Li/GSC		10/99	Added a '\r' to the end of each line	*
 * A. Hardy/GSC         11/99	Added cancel option			*
 * A. Hardy/GSC		 2/00   Reworked to use vf library		*
 * A. Hardy/GSC		 3/00   Added VFWWCL                            *
 * A. Hardy/GSC		 4/00   Added VFWOUI, VFWAWN and VFWPWN         *
 * R. Tian/SAIC		05/02	Removed call to vfwwcl			*
 * T. Piper/SAIC	07/05	Removed calls to vfwawn and vfwpwn	*
 * G. Grosshans/SPC	11/05	Added ctb_pfread to support lat/lon in  *
 * 				SAW product				*
 ***********************************************************************/
{
    char        fname[256], argtwo[7], strin[100], ans[2];
    int         iret;
/*---------------------------------------------------------------------*/

    iret = -1;

   /*
    * If WW filename is not one the command line, exit.
    */

    if ( argv[1] == NULL ){
         printf("\nThe filename should be on the command line\r\n\n");
         exit (1);
    }

    strcpy ( fname, argv[1] );
    if (  argv[2] != NULL ) cst_uclc ( argv[2], argtwo, &iret );

   ctb_pfread ( &iret );
  
   /*
    * Read the text file.
    */
    vfgttxt ( fname, &iret );
    
    if ( iret == 0 ) {
        if ( argv[2] == NULL ){

           /*
            * Create the text files.
            */
            printf ( "Are there CONTINUING watches (y/n) ? \n");
            scanf  ("%s",ans);

            if ( (strcmp ( ans,"y") == 0 ) || 
	                               (strcmp ( ans,"Y") == 0 ) ) {
                printf("Enter the watch numbers separated by spaces.\n");
                scanf  ("\n%[^\n]s",strin);
            }

            vfwsel ( strin, &iret );
            vfwsaw ( &iret );
            vfwsev ( &iret );
            vfwoui ( &iret );

        }

        else if ( strcmp (  argtwo, "cancel" ) == 0 ) {

       /*
        * Create the cancel text files.
        */

         vfcnsaw ( &iret );
         vfcnsel ( &iret );

        }

    }
    return 0;
}

#include "geminc.h"
#include "gemprm.h"

#define AODT_VTBL      "aodtvers.tbl"

/* Default version						*/
#define DEFVERSION	"6.4"

/* List of valid options processed by getopt			*/
#define	OPTSTRING	"a:c:d:f:i:ehlm:n:o:s:v:y:"


/************************************************************************
 * aodt.c                                                            	*
 *                                                                      *
 * CONTENTS:                                                            *
 * aodt                                                              	*
 ***********************************************************************/

/*
 *  Prototype for aodtv64_drive and aodtv72_drive
 */
int aodtv64_drive( int argc, char **argv );
void aodtv64_runautomode ( int fcsttype, char *fcstfile, char *imagefile,
                      float *cenlat, float *cenlon, int *posm );
int aodtv72_drive( int argc, char **argv );
void aodtv72_runautomode ( int fcsttype, char *fcstfile, char *imagefile,
                      float *cenlat, float *cenlon, int *posm );

int main ( int argc, char **argv )
/************************************************************************
 * AODT                                                              	*
 *                                                                      *
 * This program performs the Advanced Objective Dvorak Technique.	*
 *  From: Chris Velden, Tim Olander					*
 *  University of Wisconsin - Madison					*
 *  Cooperative Institute for Meteorological Satellite Studies		*
 *            								*
 *                                                                      *
 * Usage: aodt [options] image_filename					*
 *									*
 * -v  version  Version of the AODT library, i.e. 6.4, or 7.2  ...	*
 *		(default = 6.4)						*
 *                                                                      *
 * image_filename        Image filename					*
 *                                                                      *
 *                                                                      *
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	05/04	After NODT				*
 * M. Li/SAIC		12/05	Modified for version 6.4		*
 * M. Li/SAIC		12/06	Added version 7.2			*
 ***********************************************************************/

{
char	aodtversion[4], avversions[4][12];
char	buffer[256], temp[12];
int	ii, jj, nn, nr, len, vermax, ier; 
Boolean found;
FILE    *fp;

/*
 * These variables are used by getopt. Unset the error reporting.
 */
int	ch, errflg;

/*---------------------------------------------------------------------*/

    vermax = 0;
    strcpy ( aodtversion, DEFVERSION );

   /*
    * Get the list of available AODT versions from table aodtvers.tbl.
    */

    fp = (FILE *)cfl_tbop( AODT_VTBL, "nmap", &ier);
    if ( fp == NULL  ||  ier != 0 ) {
	printf ( "Couldn't open table %s\n", AODT_VTBL );	
    }

    cfl_tbnr(fp, &nr, &ier);
    if ( ier != 0 || nr == 0 ) {
	printf ( "Couldn't read table %s, or the table is empty.\n", AODT_VTBL );	
    }

    rewind(fp);
    nn  = 0;
    while ( nn < nr && !feof(fp) ) {

   	cfl_trln( fp, 256, buffer, &ier );
        if ( ier != 0 ) break;

        sscanf( buffer, "%s %s", temp, avversions[nn] );
        nn++;
    }

    cfl_clos(fp, &ier);


   /*
    * Use the first entry in the table as the default version.
    */

    if ( nn > 0 )  {
	strcpy ( aodtversion, avversions[0] ); 
	vermax = nn;
    }


    /*
     * Get the options and set the appropriate flags.
     */
    opterr = 1;
    errflg = 0;
    while ( ( ch = getopt ( argc, argv, OPTSTRING ) ) != EOF ) {
        switch ( ch ) {

            case 'v':                           /* version option       */

                strcpy ( aodtversion, optarg );
                cst_rxbl ( aodtversion, aodtversion, &len, &ier );
                break;

            case '?':				/* unknown option	*/

                errflg++;
                break;

            default:
                break;
        }
    }

    found = False;
    for (ii = 0; ii < vermax; ii ++) {

       if ( strcmp(aodtversion,avversions[ii]) == 0 ) {
           printf("Calling AODT_%s \n", aodtversion);
           if ( strcmp(aodtversion,"7.2") == 0 ) {
	       aodtv72_drive ( argc, argv );
	   }
	   else  if ( strcmp(aodtversion,"6.4") == 0 ) {
	       aodtv64_drive ( argc, argv );
	   }
	   found = True;
           break;
       }
    }

    if ( !found ) {
    	printf("AODT VERSION %s UNAVAILABLE \n", aodtversion);
        printf("VERSIONS AVAILABLE: ");
        for (jj = 0; jj<vermax; jj ++) {
            printf("%s ", avversions[jj]);
        } 
        printf ("\n");
    }

    /*
     */

    return (0);

}

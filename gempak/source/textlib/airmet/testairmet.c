#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"

#define XSLT_DIR	"$GEMTBL/xslt/"
#define	NUM_AREA	6
#define	NUM_TYPE	3
#define LINE_LEN	66

/************************************************************************
 * testairmet.c                                                      	*
 *                                                                      *
 * CONTENTS:                                                            *
 * main                                                                 *
 ***********************************************************************/

int main ( int argc, char **argv )
/************************************************************************
 * testairmet	                                                        *
 *                                                                      *
 * This program accepts as input VGF files containing GFA elements and  *
 * generates an airmet bulletin.					*
 *                                                                      *
 * Usage:                                                               *
 *                                                                      *
 * testairmet FA_Area(s) cycle_time flag VGF_file(s)                 	*
 *                                                                      *
 * 	FA_Areas	-  a list of FA area separated with ':'         *
 * 	cycle_time	-  cycle time                                   *
 * 	flag		-  normal/test flag                             *
 * 	VGF_Files	-  a list of VGF files separated by whitespace  *
 *                                                                      *
 *                                                                      *
 * main ( argc, argv )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  	argc	int      	number of parameters of command line    *
 *  	argv   	char**   	parameter array of command line         *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	NONE     		                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created                         	*
 * B. Yin/SAIC		11/04	Added help information          	*
 * B. Yin/SAIC		11/04	Wrapped the output text         	*
 *				  Area str can be in lower case		*
 * B. Yin/SAIC          12/04   Fixed the bug of NULL txt       	*
 * B. Yin/SAIC		12/04	Allocated elIn only once        	*
 *				  Released elOut memory			*
 * E. Safford/SAIC	03/05	allow a 0 smear airmet report		*
 * J. Wu/SAIC		07/05	overhaul GFA bound clipping algorithm	*
 * E. Safford/SAIC	06/05	perform 2 stage xml_transform		*
 * E. Safford/SAIC	10/05	add day param for af_create call	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 * B. Yin/SAIC		03/07	Added issuance into the argument list	*
 ***********************************************************************/
{
    int 	ii, jj, kk, ier, nextEl, curPos, nAreas, nIn;
    int		line = LINE_LEN;
    long	fileSize;
    char    	vgfname[ 128 ], faArea[ NUM_AREA ][ 8 ], areaStr[ 10 ];
    char	blank[2]={' '}, typeLC[ 32 ], xslFile[ 64 ];
    char	*airmetType[ NUM_TYPE ] = { "SIERRA", "TANGO", "ZULU" };
    char	*tmpStr, *fmtStr[NUM_AREA][NUM_TYPE];

    unsigned char *txtStr, *interumStr;

    FILE	*fptr;

    VG_DBStruct *elIn, el;

    /*
     *  Variables to initialize device and projection
     */
    int		mode = 1, iunit = 1, itype = 1, istat;
     	
    float       xsize = 500, ysize = 500;
    float	lllat = 10, lllon = -120, urlat = 50, urlon = -50;
    float	prjang1 = 90, prjang2 = -105, prjang3 = 0;

    char        device[ 3 ] = "GN", dfilnam[ 20 ] = "testairmet";
    char	proj[ 4 ] = "str";
/*---------------------------------------------------------------------*/

    /*
     *  Check if the number of input arguments is correct.
     */
    if ( argc < 6 ) {
  
       printf ( "\nUsage: testairmet FA_Area[:FA_Area] cycle_time day flag issuance VGF_file[ VGF_file ]\n" );
       printf ( "\n       testairmet -h for more help information.\n\n" );

       if ( ( argc >= 2 ) && ( strncasecmp( argv[ 1 ], "-h", 2 ) == 0 ) )  {
	  
	  printf ( "FA_Area:\t Any combination of SFO, SLC, CHI, DFW, BOS, and MIA separated by ':'.\n" );
	  printf ( "cycle_time:\t Cycle time (00, 06 or other values).\n" );
	  printf ( "day:\t\t Day of the month (2 digits).\n" );
	  printf ( "flag:\t\t 0 (Normal) or 1 (Test).\n" );
	  printf ( "issuance:\t 0 (Normal) or 1 (Other).\n" );
	  printf ( "VGF_files:\t VG files separated by white space.\n\n" );

       }

       exit ( 0 );

    }

    /* 
     *  Initialize clo, device, and projection
     */
    clo_init ( &ier );

    ginitp ( &mode, &istat, &ier );

    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
             strlen ( device ), strlen ( dfilnam ) );

    gsmprj ( proj, &prjang1, &prjang2, &prjang3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen ( proj ) );

    /*
     *  Get the total number of GFA elements
     */
    nIn = 0;

    for ( ii = 6; ii < argc; ii++ ) {

	if ( access ( argv[ ii ], R_OK ) != 0 ) {

           printf("Could not read VG file %s.\n", argv[ ii ] );
	   continue;

	}

        cvg_open ( argv[ ii ], 0, &fptr, &ier );

        if ( ier != 0 )  {

           printf("Error opening VG file %s.\n", argv[ ii ] );
           printf("Skip VG file %s.\n", argv[ ii ] );
	   continue;

        }

	cfl_inqr ( argv[ ii ], NULL, &fileSize, vgfname, &ier );

    	curPos 	= 0;
    	nextEl 	= 0;
    	ier 	= 0;

    	while ( nextEl < MAX_EDITABLE_ELEMS )  {

            cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

	    if ( ier != 0 ) break;

            if ( el.hdr.recsz > 0 )  {

               curPos += el.hdr.recsz;

               if ( (int)el.hdr.vg_type == GFA_ELM &&
		    !el.hdr.delete )  {

	          nIn++;
		  cvg_freeElPtr ( &el );

               }
            }

            nextEl++;
        }					/* element loop  */

	cvg_clos ( fptr, &ier );

    } 						/* vgf file loop */

    G_MALLOC ( elIn, VG_DBStruct, nIn, "testairmet" ); 

    /*
     *  Loop over VG files to read GFA elements
     */
    jj = 0;
    for ( ii = 6; ii < argc; ii++ ) {

	if ( access ( argv[ ii ], R_OK ) != 0 ) {
	   continue;
	}

        cvg_open ( argv[ ii ], 0, &fptr, &ier );

        if ( ier != 0 )  {
	   continue;
        }

	/*
	 *  Read GFA elements
	 */
	cfl_inqr ( argv[ ii ], NULL, &fileSize, vgfname, &ier );

    	curPos 	= 0;
    	nextEl 	= 0;
    	ier 	= 0;

    	while ( ( nextEl < MAX_EDITABLE_ELEMS ) && ( jj < nIn ) )  {

            cvg_rdrecnoc ( vgfname, fptr, curPos, &elIn[ jj ], &ier );

	    if ( ier != 0 ) break;

            if ( elIn[ jj ].hdr.recsz > 0 )  {

               curPos += elIn[ jj ].hdr.recsz;

               if ( (int)elIn[ jj ].hdr.vg_type == GFA_ELM &&
		    !elIn[ jj ].hdr.delete )  {

	          jj++;

               }
            }

            nextEl++;
        }					/* element loop  */

	cvg_clos ( fptr, &ier );

    } 						/* vgf file loop */
    
    
    /*
     *  Get FA areas
     */
    tmpStr = strtok ( argv[ 1 ], ":" );
    nAreas = 0;

    while ( tmpStr && ( nAreas < NUM_AREA ) ) { 

	cst_lcuc ( tmpStr, areaStr, &ier );
	strcpy ( faArea [ nAreas++ ], areaStr );
	tmpStr = strtok ( NULL, ":" );

    }
    
        
    /*
     *  Generate the array of information string for requested
     *  areas and types.
     */
    af_create ( nAreas, faArea, NUM_TYPE, airmetType, argv[ 3 ], argv[ 2 ], 
	        nIn, elIn, atoi(argv[5]), fmtStr, &ier );

    
    /*
     *  Loop over FA areas and airmet types to generate the bulletin
     */
    for ( ii = 0; ii < nAreas; ii++ ) {

	for ( jj = 0; jj < NUM_TYPE; jj++ ) {

	   /*
	     *  Generate the bulletin
	     */
	    cst_uclc ( airmetType[ jj ], typeLC, &ier ),
	    xslFile[ 0 ] = '\0';
	    sprintf ( xslFile, "%sairmet_%s.xsl", XSLT_DIR, typeLC );
  
	    xml_transform( fmtStr[ii][jj], strlen ( fmtStr[ii][jj] ), xslFile, 
			   &interumStr, &ier );

	    strcpy( xslFile, "indent.xsl" );
	    xml_transform( (char *)interumStr, strlen ( (char *)interumStr ), 
	    			xslFile, &txtStr, &ier );
	    if ( ier == 0 ) {

               cst_wrap ( (char*)txtStr, blank, &line, "\n", (char *)NULL, (char*)txtStr, &ier );
	       printf ( "\n\n%s\n\n", txtStr );
	       G_FREE ( txtStr, unsigned char );
               G_FREE ( interumStr, unsigned char );
	    }

	    G_FREE ( fmtStr[ii][jj], char );
	}
    }

    for ( kk = 0; kk < nIn; kk++ ) {
	G_FREE ( elIn[ kk ].elem.gfa.info.blockPtr[ 0 ], gfaBlock_t );
    }

    G_FREE ( elIn, VG_DBStruct );

    return 0;

}

#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"

#include "vgstruct.h"
#include "afcreatexml.h"
#include "afbufr_common.h"

#define XSLT_DIR        "$GEMTBL/xslt/"
#define NUM_TYPE        3
#define MAXST           128

/************************************************************************
  airmet_vgfbufr.c

  This module is the main driver program for converting GFA VGF files
    to XML and BUFR

  CONTENTS:
  main

  private functions:
    writeToXmlfile            - Writes Xml Info string to File
    setType                   - Replaces <type> in Output Format String
                                with SIERRA TANGO or ZULU
 Log: L. Hinson/AWC   1/06   Created
 Log: L. Hinson       4/09   Add Function isGAirmetObj to support mixed
                             C&V VGF files containing IFR objects
 ***********************************************************************/

Boolean isGAirmetObj(char *hazard);

void afcreate_bufr(char *docname, int size, char *outputformat,
                   char *issDateTime, char *fxyPath, int debuglvl, int *ier);

int main ( int argc, char **argv )
/************************************************************************
 * airmet_vgfbufr                                                       *
 *                                                                      *
 * This program reads VGF files containing GFA elements and             *
 * generates XML and BUFR documents.                                    *
 *                                                                      *
 * Usage:                                                               *
 *                                                                      *
 * airmet_vgfbufr -refDate <YYYYMMDD> -cycle <cycle_time>               *
 *      [-i <VGF_FILE>] [-ipxml <pxml document>]                        *
 *      [-opxml <xml formatstring>] [-oxml <xml formatstring>]          *
 *      [-fxyPath <path>] [-obufr <bufr formatstring>]                  *
 *      [-bulletin <SIERRA|TANGO|ZULU>]                                 *
 *      [-filterTimeSmears] [-debug <lvl>]                              *
 *                                                                      *
 * where -refDate is the date of the product to be issued.              *
 *       -cycle is the cycle hour of the issuance time of the product   *
 *       -i the name of the Input VGF file - This file may contain an   *
 *        individual or collective set of airmets.                      *
 *       -ipxml is the name of a pre-xml document.  This file may       *
 *         be used instead of a VGF file for input, and may contain an  *
 *         individual or collective set of airmets.                     *
 *       -opxml - Use to specify a format string for pre xml output     *
 *         files. The default is "%Y%m%d_%H%M_<type>_<cycle>_pre.xml"   *
 *         (Reference man * pages on strftime format specs)             *
 *       -oxml - Use to specify a format string for xml output files.   *
 *         The default is  "%Y%m%d_%H%M_<type>_<cycle>.xml              *
 *       -fxyPath - Use to specify the Path to the FXY descriptor files *
 *                  Otherwise, current working directory assumed        *
 *       -obufr - Use to specify a format string for bufr output files. *
 *           The default is "%Y%m%d_%H%M_<type>_<cycle>.bufr            *
 *           pages on strftime format specs)                            *
 *        -bulletin - Use to specify which documents get generated.  In *
 *           practice.  The default is to generate SIERRA, TANGO, and   *
 *           ZULU documents.                                            *
 *        -debug <level> - Use to print out additional debugging info   *
 *             level=0 - No Debugging; 1 - Print out Data_MixVal_t      *
 *             structures; >1 Print detailed MEL_BUFR diagnostics       *
 *        -filterTimeSmears - Filter Time Smears (Those that have "-"   *
 *            in time.                                                  *
 *                                                                      *
 *       By default, all 3 documents for SIERRA, TANGO, and ZULU will   *
 *       be generated.                                                  *
 *                                                                      *
 * main ( argc, argv )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      argc    int             number of parameters of command line    *
 *      argv    char**          parameter array of command line         *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      NONE                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson/AWC        1/06    Created                                 *
 * L. Hinson/AWC        4/09    Add call to isGAirmetObj() for C&V      *
 ***********************************************************************/
{
    int         jj, ier, nextEl, curPos, nIn = 0;
    long        fileSize;
    char        vgfname[ MAXST ];
    char        xslFile[ MAXST ];
    char        *airmetType[ NUM_TYPE ] = { "SIERRA", "TANGO", "ZULU" };
    char        *fmtStr[NUM_TYPE];
    char        hazard[STD_STRLEN];

    unsigned char *interumStr;

    FILE        *fptr;

    VG_DBStruct *elIn = NULL, el;

    /*
     *  Variables to initialize device and projection
     */
    int         mode = 1, iunit = 1, itype = 1, istat;

    float       xsize = 500, ysize = 500;
    float       lllat = 10, lllon = -120, urlat = 50, urlon = -50;
    float       prjang1 = 90, prjang2 = -105, prjang3 = 0;

    char        device[ 3 ] = "GN", dfilnam[ 20 ] = "airmet_format";
    char        proj[ 4 ] = "str";
/* LJH Variables to manage XML and BUFR creation*/
    int i,j;
    Boolean infileset=False, dayset=False, cycleset=False;
    Boolean outputFormatXMLset=False, inpxmlFileset=False,
      issDateTimeset=False, outputFormatPXMLset=False,
      outputFormatBUFRset=False,
      bulletinDesignatorSet=False, filterTimeSmears=False;
    char refDate[9],cycle[3];
    char infilename[50][MAXST];
    char inpxmlfile[MAXST];
    char issDateTime[14]; /* Issue Date Time String to be used in XML/BUFR filename*/
    int nVgfFiles=0;
    char outputFormatXML[MAXST];
    char outputFormatPXML[MAXST];
    char outputFormatBUFR[MAXST];
    char outputFormatStr[MAXST];
    char bulletinDesignator[MAXST];
    char debugstring[MAXST];
    char fxyPath[MAXST];
    static char usageString[] = "Usage: airmet_vgfbufr -refDate <YYYYMMDD> "
                                "-cycle <cycle_time> "
                                "-issDateTime <YYYYMMDDHHMM>\n"
                                "[-i <VGF_FILE>] [-ipxml <pxml document>] "
                                "[-opxml <pxml formatstring>] "
                                "[-oxml <xml formatstring>] \n"
                                "[-fxyPath <path>] [-obufr <bufr formatstring>]"
                                "[-bulletin <SIERRA|TANGO|ZULU>]\n"
                                "[-filterTimeSmears] [-debug <lvl>]\n";
    int debuglvl = 0;
    fxyPath[0]='\0';
/*---------------------------------------------------------------------*/
    for (i=0; i<argc; i++) {
      if (strcmp(argv[i],"-h")==0) {
        printf(usageString);
        exit(1);
      }
      if (strcmp(argv[i],"-i")==0) {
        if (nVgfFiles < 50) {
          strcpy(infilename[nVgfFiles],argv[i+1]);
          infileset=True;
          nVgfFiles++;
        } else {
          printf("Limit on Input VGF files exceeded.  Limit is 50.");
        }
      }
      if (strcmp(argv[i],"-refDate")==0) {
        sscanf(argv[i+1],"%s",refDate);
        dayset=True;
      }
      if (strcmp(argv[i],"-cycle")==0) {
        sscanf(argv[i+1],"%s",cycle);
        cycleset=True;
      }
      if (strcmp(argv[i],"-ipxml")==0) {
        sscanf(argv[i+1],"%s",inpxmlfile);
        inpxmlFileset=True;
      }
      if (strcmp(argv[i],"-issDateTime")==0) {
        sscanf(argv[i+1],"%s",issDateTime);
        issDateTimeset=True;
      }
      if (strcmp(argv[i],"-opxml")==0) {
        sscanf(argv[i+1],"%s",outputFormatPXML);
        if (strcmp(outputFormatPXML,"-")==0)
          strcpy(outputFormatPXML, "%Y%m%d_%H%M_<type>_<cycle>_pre.xml");
        outputFormatPXMLset=True;
      }
      if (strcmp(argv[i],"-oxml")==0) {
        sscanf(argv[i+1],"%s",outputFormatXML);
        if (strcmp(outputFormatXML,"-")==0)
          strcpy(outputFormatXML, "%Y%m%d_%H%M_<type>_<cycle>.xml");
        outputFormatXMLset=True;
      }
      if (strcmp(argv[i],"-obufr")==0) {
        sscanf(argv[i+1],"%s",outputFormatBUFR);
        outputFormatBUFRset=True;
      }
      if (strcmp(argv[i],"-fxyPath")==0) {
        sscanf(argv[i+1],"%s",fxyPath);
      }
      if (strcmp(argv[i],"-bulletin") == 0) {
        sscanf(argv[i+1],"%s",bulletinDesignator);
        bulletinDesignatorSet=True;
      }
      if (strcmp(argv[i],"-filterTimeSmears") == 0) {
        filterTimeSmears=True;
      }
      if (strcmp(argv[i],"-debug") == 0) {
        if (i+1 < argc) {
          sscanf(argv[i+1],"%s",debugstring);
          if (atoi(debugstring)>0)
            debuglvl =atoi(debugstring);
          else
            printf("-debug switch needs integer >= 0...assigning to 0\n");
        } else {
          printf("-debug switch needs integer >= 0...assigning to 0\n");
        }
      }
    }
    if (! outputFormatXMLset ) {
      strcpy(outputFormatXML,"%Y%m%d_%H%M_<type>_<cycle>.xml");
    }
    if (! outputFormatBUFRset) {
      strcpy(outputFormatBUFR,"%Y%m%d_%H%M_<type>_<cycle>.bufr");
    }
    if (! (dayset && cycleset && (infileset||inpxmlFileset) && issDateTimeset)) {
      printf(usageString);
      exit(1);
    }
    if (inpxmlFileset) {
      /* Do work of afcreateprexml... including
         1.  allocating space for *fmtStr[3], 3 times, once per bulletin.
         2.  Load the appropriate fmtStr from the specified file.
         3.  Requires issTimeStr set from-issDateTime Arg...
             format = YYYYMMDDHHMM
      */
      for (j = 0; j < NUM_TYPE; j++) {
        G_MALLOC ( fmtStr[j], char, ONEBLOCK, "af_format string");
      }
      for (j = 0; j < NUM_TYPE; j++) {
        if (strcmp(bulletinDesignator,airmetType[j])==0) {
          loadXmlfile(&fmtStr[j],inpxmlfile,&ier);
        }
      }
    }

    if (! inpxmlFileset) {
        /*
        *  Initialize clo, device and projection
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
        for (i=0; i < nVgfFiles; i++) {

          if ( access ( infilename[i], R_OK ) != 0 ) {

            printf("Could not read VG file %s.\n", (unsigned char *) infilename[i] );
            return(1);

          }

          cvg_open ( infilename[i], 0, &fptr, &ier );

          if ( ier != 0 )  {

            printf("Error opening VG file %s.\n", (unsigned char *) infilename);
            printf("Skip VG file %s.\n", (unsigned char *) infilename);
            return(1);

          }

          cfl_inqr ( infilename[i], NULL, &fileSize, vgfname, &ier );

          curPos    = 0;
          nextEl    = 0;
          ier       = 0;

          while ( nextEl < MAX_EDITABLE_ELEMS )  {

              cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

              if ( ier != 0 ) break;

              if ( el.hdr.recsz > 0 )  {

                curPos += el.hdr.recsz;
                cvg_getFld ( &el, TAG_GFA_AREATYPE, hazard, &ier );
                if ( (int)el.hdr.vg_type == GFA_ELM &&
                      !el.hdr.delete  &&  isGAirmetObj(hazard))  {

                    nIn++;
                }
                cvg_freeElPtr ( &el );
              }

              nextEl++;
          }                                 /* End countinting elements  */

          cvg_clos ( fptr, &ier );
        }                                   /* vgf file loop */

        G_MALLOC ( elIn, VG_DBStruct, nIn, "airmet_xmlbufr" );

        /*
        *  Loop over VG files to read GFA elements
        */
        jj = 0;

        for (i=0; i < nVgfFiles; i++) {

          if ( access ( infilename[i], R_OK ) != 0 ) {
            return(1);
          }

          cvg_open ( infilename[i], 0, &fptr, &ier );

          if ( ier != 0 )  {
            return(1);
          }

          /*
          *  Read GFA elements
          */
          cfl_inqr ( infilename[i], NULL, &fileSize, vgfname, &ier );

          curPos    = 0;
          nextEl    = 0;
          ier       = 0;

          while ( ( nextEl < MAX_EDITABLE_ELEMS ) && ( jj < nIn ) )  {

              cvg_rdrecnoc ( vgfname, fptr, curPos, &elIn[ jj ], &ier );

              if ( ier != 0 ) break;

              if ( elIn[ jj ].hdr.recsz > 0 )  {

                curPos += elIn[ jj ].hdr.recsz;
                cvg_getFld ( &elIn[ jj ], TAG_GFA_AREATYPE, hazard, &ier );
                if ( (int)elIn[ jj ].hdr.vg_type == GFA_ELM &&
                      !elIn[ jj ].hdr.delete && isGAirmetObj(hazard))  {

                    jj++;

                } else {
                  cvg_freeElPtr(&elIn[jj]);
                }
              }

              nextEl++;
          }                                 /* element loop  */

          cvg_clos ( fptr, &ier );

        }                                   /* end vgf file loop */
        af_create_prexml ( NUM_TYPE, airmetType, refDate, cycle, nIn, elIn,
                           filterTimeSmears, issDateTime, fmtStr, &ier );
    }

    /*****************************************************************************/
    for ( jj = 0; jj < NUM_TYPE; jj++ ) {
      if (bulletinDesignatorSet) {
        if (strcmp(bulletinDesignator,airmetType[jj]) != 0)
            continue;
      }
      if (jj==0 || jj==1 || jj==2) {
        if (outputFormatPXMLset) {
          setCycleAndType( outputFormatPXML, cycle, airmetType[jj], outputFormatStr);
          writeToXmlfile((char *)fmtStr[jj], issDateTime, outputFormatStr, &ier);
        }
        xslFile[0] = '\0';
        if (jj==0) sprintf(xslFile,"%sxmlBufr_sierra.xsl",XSLT_DIR);
        if (jj==1) sprintf(xslFile,"%sxmlBufr_tango.xsl",XSLT_DIR);
        if (jj==2) sprintf(xslFile,"%sxmlBufr_zulu.xsl",XSLT_DIR);
        xml_transform(fmtStr[jj],strlen(fmtStr[jj]),xslFile,&interumStr,&ier);
        if (ier != 0) {
          printf("Problem with call to xml_transform.  Check that \n");
          printf("  xmlBufr_[sierra|tango|zulu].xsl files are installed\n");
          printf("  in either $GEMPAK|$NCSITE|$NCDESK/xslt or local directories.\n");
          exit(1);
        }
        if (outputFormatXMLset) {
          setCycleAndType( outputFormatXML, cycle, airmetType[jj], outputFormatStr );
          writeToXmlfile((char *)interumStr, issDateTime, outputFormatStr, &ier);
        }
        if (ier == 0 ) {
          if (outputFormatBUFRset) {
            setCycleAndType(outputFormatBUFR, cycle, airmetType[jj],
                            outputFormatStr);
            afcreate_bufr( (char *) interumStr, strlen((char *)interumStr),
                          outputFormatStr, issDateTime,
                          fxyPath, debuglvl, &ier);
          }
          G_FREE ( interumStr, unsigned char );
        }
      }

    }
    for (jj = 0; jj < NUM_TYPE; jj++) {
      G_FREE(fmtStr[jj], char);
    }
    if (! inpxmlFileset) {
      for ( jj = 0; jj < nIn; jj++ ) {
        G_FREE ( elIn[jj].elem.gfa.info.blockPtr[ 0 ] , gfaBlock_t);
      }
      G_FREE ( elIn, VG_DBStruct);
    }
    return(0);
}

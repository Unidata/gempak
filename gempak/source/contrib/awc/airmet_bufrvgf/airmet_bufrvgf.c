#include "afbufr_structures.h"
#include "afreadbufr.h"
#include "afcreatevgf.h"
#include <string.h>
#include <stdio.h>
#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"

#include "vgstruct.h"
#include "vgtag.h"
#include "drwids.h"
#include "afcreatexml.h"
#include "afbufr_common.h"

#define MAXST 128
#define _AFCREATEBUFR_GBL
#define _AFCVGF_GBL
#define NUM_TYPE 3
#define XSLT_DIR "$GEMTBL/xslt/"

unsigned char *interumStr;

/*********************************************************
airmet_bufrvgf.c

This module is the main driver program for converting GFA
BUFR code to XML and VGF files

CONTENTS:
main

Private functions:
free_prerec_items - Free up memory routines
free_giof_items - Free up memory routines
getDateCycleIssueTime - Infers Cycle/Day from expiration time
**********************************************************/

void free_prerec_items(GFAByDesignatorInfo *bufr_prerec);
void free_giof_items(GFAIdObsOrFcstLoc *giof);
void getDateCycleIssueTime(BULLETIN_t *bulletin, char *refDate, char *cycle,
                          char *issTimeStr);

int main ( int argc, char **argv )
{
/**************************************************************************
airmet_bufrvgf

This program reads BUFR files containing GFA elements and
generates XML and VGF files.

Usage:
airmet_bufrvgf -i <BUFR_FILE> [-opxml <pxml formatstring]
    [-oxml <xml formatstring>] [-ovgf <vgf formatString>]
    [-fcstHr <fcstHr>] [-fcstHrBin <fcstHr1>-<fcstHr2>] [-hazard <hazard>]
    [-debug <debuglvl>]

where -i  is the name of a GFA BUFR file to be used as input in this program.
      -opxml  -  Use to specify a format string for the pre xml output files. 
        The default is %Y%m%d_%H%M_<type>_<cycle>_pre.xml.  (Reference man 
        pages on strftime format specs)
        Note: SIERRA, TANGO, or ZULU are substituted for the parameter <type> 
          for the xml documents that are generated.
        Note: Use single quotes around the format string, should it include 
          the keyword <type>, to avoid problems with the command interpreter
          processing this as a redirection command.
      -oxml  Use to specify a format string for xml output filename.  
         The default is  %Y%m%d_%H%M_<type>_<cycle>.xml  (Reference man pages
         on strftime format specs)

      -ovgf  Use to specify a format string for vgf output filename.
         The default is %Y%m%d_%H%M_<type>_<cycle>.vgf (Reference man pages on
           strftime format specs)
         Note: SIERRA, TANGO, or ZULU are substituted for the parameter <type>
           for the xml documents that are generated.
         Note: Use single quotes around the format string, should it include 
           the keyword <type> or <cycle>, to avoid problems with the command
           interpreter processing this as a redirection command.
               
       -fcstHr  Used for retrieving the GFA elements by the specified forecast 
         hour(s) out of the BUFR message and placing into VGF file(s). 
         Both Snapshots and Time-smears can be retrieved. Snapshots are denoted
         by a single forecast hour, whereas time-smears are denoted by a 
         hyphenated range of hours.  VGF filenames will include the forecast hour(s)
         at the end of the root file name.  Examples: 2006113015_SIERRA_F03.vgf, 
         2006113015_SIERRA_F03_06.vgf.

       -fcstHrBin  Used for retrieving the GFA elements valid over the 
         specified range of forecast hours out of the BUFR message and 
         placing into VGF file(s).   The elements extracted will be valid
         for the period from the first forecast hour until, but not 
         including the second forecast hour.  VGF filenames will include 
         the forecast hours appended to the end of the root file name.  
         Example: 2006113015_SIERRA_F03_F06.vgf

       -hazard  Used for retrieving the GFA elements by the specified hazard.
         VGF filenames will include the hazard type appended to the root file 
         name.  If -hazard is used included with the -fcstHr or -fcstHrBin 
         switches, the hazard type will precede the forecast hour information.  Examples: 2006113015_TANGO_TURB-HI_f00.vgf 2006113015_SIERRA_MT_OBSC_f03_06.vgf
 
       -debug <debug level> Use to indicate that additional diagnostic output
        is to be produced. This will be used to troubleshoot decoding bufr 
        messages

  main (argc, argv)
  
  
  Input parameters:
    argc   int     number of parameters of command line
    argv   char**  parameter array of command line
    
  Output parameters:
  Return parameters:
     NONE:
**
  Log:
  L. Hinson/AWC      10/07 Created
  L. Hinson/AWC      06/08 Add cycle in call to createVGF
*****************************************************************************/

  int i;
  char infilename[MAXST], outputFormatXML[MAXST],
     outputFormatPXML[MAXST], outputFormatVGF[MAXST],
     outputFormatStr[MAXST], vgffilename[MAXST], dateStamp[MAXST];
  Boolean infileset=False, outputFormatXMLset=False, outputFormatPXMLset=False,
     outputFormatVGFset=False;
  GFAByDesignatorInfo *bufr_prerec=0x0;
  enum GFADesignator GFADesignatorType;
  char *GFADesignatorNames[] = {"SIERRA","TANGO","ZULU"};
  char *fmtStr[NUM_TYPE];
  int num_vals;
  VG_DBStruct *elIn=0x0;
  int nIn;
  char refDate[9], cycle[3];
  char issTimeStr[14];
  char fcstHours[50][6];
  int numFcstHours=0;
  char hazards[50][10];
  int numHazards=0;
  Boolean getAllHoursSW = True;
  Boolean getAllHazardsSW = True;
  Boolean binFcstHoursSW = False;
  Boolean filterTimeSmears = False;
  char xslFile[MAXST];
  int ier;
  char debugstring[MAXST];
  int debuglvl = 0;

  int         mode = 1, iunit = 1, itype = 1, istat;

  float       xsize = 500, ysize = 500;
  float       lllat = 10, lllon = -120, urlat = 50, urlon = -50;
  float       prjang1 = 90, prjang2 = -105, prjang3 = 0;

  char        device[ 3 ] = "GN", dfilnam[ 20 ] = "airmet_format";
  char        proj[ 4 ] = "str";
  static char usageString[]="Usage: airmet_bufrvgf -i <BUFR_FILE> "
                          "[-opxml <pxml formatstring]\n"
                          "[-oxml <xml formatstring>] [-ovgf <vgf formatString>]\n"
                          "[-fcstHr <fcstHr>] [-fcstHrBin <fcstHr1>-<fcstHr2>] "
                          "[-hazard <hazard>] [-debuglvl]\n";
  for (i=0; i < argc; i++) {
    if (strcmp(argv[i], "-h")==0) {
      printf(usageString);
    }
    if (strcmp(argv[i],"-i")==0) {
      strcpy(infilename,argv[i+1]);
      infileset=True;
    }
    if (strcmp(argv[i],"-oxml")==0) {
      strcpy(outputFormatXML,argv[i+1]);
      outputFormatXMLset=True;
    }
    if (strcmp(argv[i],"-opxml")==0) {
      sscanf(argv[i+1],"%s",outputFormatPXML);
      if (strcmp(outputFormatPXML,"-")==0)
        /* strcpy(outputFormatPXML,"%Y%m%d_<cycle>_%H%M_<type>_pre.xml");*/
        strcpy(outputFormatPXML, "%Y%m%d_%H%M_<type>_<cycle>_pre.xml");
      outputFormatPXMLset=True;
    }
    if (strcmp(argv[i],"-ovgf")==0) {
      strcpy(outputFormatVGF,argv[i+1]);
      outputFormatVGFset=True;
    }
    if (strcmp(argv[i],"-fcstHr")==0) {
      getAllHoursSW = False;
      strcpy(fcstHours[numFcstHours],argv[i+1]);
      numFcstHours++;
    }
    if (strcmp(argv[i],"-fcstHrBin")==0) {
      getAllHoursSW = False;
      binFcstHoursSW = True;
      strcpy(fcstHours[numFcstHours],argv[i+1]);
      numFcstHours++;
    }
    if (strcmp(argv[i],"-hazard")==0) {
      getAllHazardsSW = False;
      strcpy(hazards[numHazards],argv[i+1]);
      numHazards++;
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
  if ( ! infileset ) {
      printf(usageString);
      exit(1);
  }
  if ( ! outputFormatVGFset) {
    strcpy(outputFormatVGF, "%Y%m%d_%H%M_<type>_<cycle>.vgf");
  }
  if (! outputFormatXMLset ) {
    strcpy(outputFormatXML,"%Y%m%d_%H%M_<type>_<cycle>.xml");
  }
  bufr_prerec=LoadRec(infilename,&GFADesignatorType,&num_vals,debuglvl);
  /* Infer Cycle/Day from Expiration Time less 6 hours.  Can't infer from Issuance
  Time as this could be an amendment time */
  getDateCycleIssueTime(bufr_prerec->b.bulletin, refDate, cycle, issTimeStr);
  setCycleAndType(outputFormatVGF, cycle, GFADesignatorNames[GFADesignatorType],
          outputFormatStr);
  buildFilenameAndDateStampFmIssTime(issTimeStr, outputFormatStr, vgffilename,
                                       dateStamp);
  /* sscanf(dateStamp,"%*4d%*2d%2s%*1s%2s", day, cycle); */

  elIn = createVGF(bufr_prerec, GFADesignatorType, vgffilename, cycle, getAllHoursSW,
                   getAllHazardsSW, binFcstHoursSW, fcstHours, numFcstHours, hazards,
                   numHazards, &nIn);
  /*el structure loaded, cleanup BUFR */
  /* Clean up bufr_prerec */
  free_prerec_items(bufr_prerec);
  free(bufr_prerec);
  /* Call these initialization routines so that af_create_prexml can be used*/
  /*
    *  Initialize clo, device and projection
  */
  clo_init ( &ier );

  ginitp ( &mode, &istat, &ier );

  gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
            strlen ( device ), strlen ( dfilnam ) );

  gsmprj ( proj, &prjang1, &prjang2, &prjang3,
            &lllat, &lllon, &urlat, &urlon, &ier, strlen ( proj ) );
  /* End Initialization */
  af_create_prexml(NUM_TYPE, GFADesignatorNames, refDate, cycle, nIn, elIn,
                   filterTimeSmears, issTimeStr, fmtStr, &ier );
  for (i=0;i<nIn;i++) {
    cvg_freeElPtr ( &elIn[i] );
  }
  if (outputFormatPXMLset) {
    setCycleAndType( outputFormatPXML, cycle, GFADesignatorNames[GFADesignatorType],
             outputFormatStr );
    writeToXmlfile((char *) fmtStr[GFADesignatorType],issTimeStr,outputFormatStr,
                    &ier);
  }
  xslFile[0] = '\0';
  switch (GFADesignatorType) {
    case SIERRA:
      sprintf(xslFile,"%sxmlBufr_sierra.xsl",XSLT_DIR);break;
    case TANGO:
      sprintf(xslFile,"%sxmlBufr_tango.xsl",XSLT_DIR);break;
    case ZULU:
      sprintf(xslFile,"%sxmlBufr_zulu.xsl",XSLT_DIR);break;
  }
  xml_transform(fmtStr[GFADesignatorType],strlen(fmtStr[GFADesignatorType]),xslFile,&interumStr,&ier);
  if (ier < 0) {
    printf("There was a problem in creating the GFA XML documents.\n");
    exit(1);
  }
  setCycleAndType(outputFormatXML, cycle, GFADesignatorNames[GFADesignatorType],
          outputFormatStr);
  writeToXmlfile((char*)interumStr,issTimeStr,outputFormatStr,&ier);
  G_FREE(interumStr, unsigned char );
  for (i=0; i<3; i++) {
    G_FREE(fmtStr[i], char);
  }
  G_FREE( elIn, VG_DBStruct);
  return(0);
}

void free_giof_items(GFAIdObsOrFcstLoc *giof)
/********************************************************************
This frees up the following objects ...
  1) lat/lon objects
  2) Horizontal Sections
  
free_giof_items(giof)
  Input Parameter:
    giof   GFAIdObsOrFcstLoc *   GFAIdObsOrFcstLoc Structure
**
  Log:
  L. Hinson 10/07
********************************************************************/
{
  int i;
  if (giof->df.repCount > 0) {
    for (i=0; i < giof->df.repCount; i++) {
      if (giof->df.hs[i].repCountOnCoords > 0) {
        G_FREE(giof->df.hs[i].location, Location);
      }
    }
    G_FREE(giof->df.hs, DESC_HorSect);
  }
}

void free_prerec_items(GFAByDesignatorInfo *bufr_prerec)
/********************************************************************

  1) IFR and MtnObsc objects
  2) Turb, SSW, LLWS objects
  3) Icing, Freezing Level, and Multi-Freezing level objects
  4) Sierra, Tango, Zulu are not Freed; They are not Malloced

free_prerec_items(bufr_prerec)
  Input Parameter:
    bufr_prerec   GFAByDesignatorInfo *   GFAByDesignatorInfo Structure
**
  Log:
  L. Hinson 10/07
********************************************************************/
{
  int i;
  switch (bufr_prerec->td) {
    case SIERRA:
      if (bufr_prerec->b.sierra->repFactorIFR > 0) {
        for (i=0;i<bufr_prerec->b.sierra->repFactorIFR;i++) {
          free_giof_items(&bufr_prerec->b.sierra->IFR[i].giof);
        }
        G_FREE(bufr_prerec->b.sierra->IFR, GFA_IFRCigAndVis_t);
      }
      if (bufr_prerec->b.sierra->repFactorMtnObsc > 0) {
        for (i=0;i<bufr_prerec->b.sierra->repFactorMtnObsc;i++) {
          free_giof_items(&bufr_prerec->b.sierra->MtnObsc[i].giof);
        }
        G_FREE(bufr_prerec->b.sierra->MtnObsc, GFA_MtnObsc_t);
      }
      break;
    case TANGO:
      if (bufr_prerec->b.tango->repFactorTurb > 0) {
        for (i=0;i<bufr_prerec->b.tango->repFactorTurb; i++) {
          free_giof_items(&bufr_prerec->b.tango->Turb[i].giof);
        }
        G_FREE(bufr_prerec->b.tango->Turb, GFA_Turbulence_t);
      }
      if (bufr_prerec->b.tango->repFactorSSW > 0) {
        for (i=0; i< bufr_prerec->b.tango->repFactorSSW; i++) {
          free_giof_items(&bufr_prerec->b.tango->SSW[i].giof);
        }
        G_FREE(bufr_prerec->b.tango->SSW, GFA_SSW_t);
      }
      if (bufr_prerec->b.tango->repFactorLLWS > 0) {
        for (i=0; i< bufr_prerec->b.tango->repFactorLLWS; i++) {
          free_giof_items(&bufr_prerec->b.tango->LLWS[i].giof);
        }
        G_FREE(bufr_prerec->b.tango->LLWS, GFA_LLWS_t);
      }
      break;
    case ZULU:
      if (bufr_prerec->b.zulu->repFactorIcing > 0) {
        for (i=0;i<bufr_prerec->b.zulu->repFactorIcing; i++) {
          free_giof_items(&bufr_prerec->b.zulu->Icing[i].giof);
        }
        G_FREE(bufr_prerec->b.zulu->Icing, GFA_Icing_t);
      }
      if (bufr_prerec->b.zulu->repFactorFrzLvl > 0) {
        for (i=0; i<bufr_prerec->b.zulu->repFactorFrzLvl; i++) {
          free_giof_items(&bufr_prerec->b.zulu->FrzLvl[i].giof);
        }
        G_FREE(bufr_prerec->b.zulu->FrzLvl, GFA_FreezingLvl_t);
      }
      break;
  }
}

void getDateCycleIssueTime(BULLETIN_t *bulletin, char *refDate, char *cycle,
                           char *issTimeStr) 
/***************************************************************************
  This routine derives the original issue time string in YYYYMMDDHHMM format
  from the reference date and the cycle.
  
  Input parameters:
    bulletin     BULLETIN_t * Bulletin Structure
    refDate      char *       Reference Date
    cycle        char *       The 2-digit cycle
  Output parameters:
    issTimeStr   char *       Issue Time Str 

  Log:
  L. Hinson      10/07
****************************************************************************/
{
  time_t origIssEpoch, issEpoch;
  char origIssTimeStr[15];

  /* char cycle_work[3]; */
  int cycle_hour, cycle_minute;
#ifdef Linux
  struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\0'};
#else
  struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0};
#endif
  trec.tm_year=bulletin->tp.ed.year-1900;
  trec.tm_mon=bulletin->tp.ed.month-1;
  trec.tm_mday=bulletin->tp.ed.day;
  trec.tm_hour=bulletin->tp.et.hour;
  trec.tm_min=bulletin->tp.et.minute;
  /* Convert to Epoch Time - 6 hours, 15 minutes */
  origIssEpoch = mktime (&trec) - 6*3600 - 15*60;
  strftime(origIssTimeStr,14,"%Y%m%d%H%M",localtime(&origIssEpoch));
  sscanf(origIssTimeStr,"%8s%02d%02d",refDate, &cycle_hour, &cycle_minute);
  /* Round to nearest hour for cycle times.  So times ending in 45 minutes get
  rounded up. Also account for wrap around time of 2345Z.  */
  sprintf(cycle,"%02d", ((int) (cycle_hour + cycle_minute/60.0 + .5)) % 24);
  trec.tm_year=bulletin->tp.bd.year-1900;
  trec.tm_mon=bulletin->tp.bd.month-1;
  trec.tm_mday=bulletin->tp.bd.day;
  trec.tm_hour=bulletin->tp.bt.hour;
  trec.tm_min=bulletin->tp.bt.minute;
  issEpoch = mktime (&trec);
  /* Grab day and hour, load to day & cycle */
  strftime(issTimeStr,14,"%Y%m%d%H%M",localtime(&issEpoch));
  /* sscanf(issTimeStr,"%8s%02d%02d",refDate, &cycle_hour, &cycle_minute);*/
}

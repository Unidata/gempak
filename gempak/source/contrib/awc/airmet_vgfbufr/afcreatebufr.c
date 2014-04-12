#include <stdio.h>
#include <libxml/xmlreader.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <assert.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "afcreatebufr.h"
/*
 *  Global definitions
 */
#define MAXST 128
#define MXXPATH 200

/*************************************************************************************
  afcreatebufr.c

  This module contains the subroutines to produce a BUFR coded message from an
  XML document.

  CONTENTS:
    afcreate_bufr              - Main routine - produce BUFR coded message from
XML document

    BuildRec                   - Primary Routine to load Descriptor Sequence
Structures
    buildFilenameAndDateStamp  - Routine to Create BUFR filename and Time Stamp
                                for initializing BUFR message.
    initBUFR                   - Routine to Initialize BUFR File Creation
    loadFXYs                   - Routine to load FXY Descriptor Table

  ROUTINES to Load Descriptor Sequence structures - Called by BuildRec:
    loadSierraIFR
    loadSierraMtnObsc
    loadTangoTurb
    loadTangoSSW
    loadTangoLLWS
    loadZuluIcing
    loadZuluFrzLvl
    loadProductValidTime
    loadGFAIdObsOrFcstLoc
    loadDescOfFeature
    loadHorSctnDescOfFeature

  ROUTINES to parse XML documents:
    get_number_of_nodes
    getInfo
    getInfoStr
    getInfoStrbySub
    getInfoInt
    getInfoIntbySub
    getInfoFloat
    getInfoFloatbySub
    elementExists

  ROUTINES to XFR primary descriptor sequence structures to MEL BUFR Data Arrays
      of type Data_MixVal_t
    fill_arraySIERRA
    fill_arrayTANGO
    fill_arrayZULU
    popSierraIFR
    popSierraMtnObsc
    popTangoTurb
    popTangoSSW
    popTangoLLWS
    popZuluIcing
    popZuluFrzLvl
    popGFAHeader
    popAirmetObsFcst
    popDescField

   Routines to Free up Memory
    free_prerec_items
    free_giof_items
    free_strings

   Routine to check if GAirmet Object
    isGAirmetObj

 Log: L. Hinson/AWC    01/06   Created
      L. Hinson/AWC    04/09   Add function isGAirmetObj
*******************************************************************************/

void afcreate_bufr(char *docname, int size, char *outputformat,
                   char *issTimeStr, char *fxyPath, int debuglvl, int *ier)
/*******************************************************************************
  afcreate_bufr

  This routine takes the contents of the input XML document to create a BUFR
message
  containing SIERRA, TANGO, or ZULU Hazards.

  Input parameters:
    *docname        char  XML Document String
    size            int   Length of XML document
    *outputformat   char  Output Format String used for BUFR Message filename
    *issTimeStr     char  Issue Time String - to be used as timestamp for BUFR
    *fxyPath        char  Path to FXY Descriptor Tables
    debuglvl        int   Debug Level - 0 for no debugging
                                        1 for Dump of Data_MixVal_t structure
                                       >1 for more detailed MEL-BUFR Output
  L. Hinson         01/06 Created
*******************************************************************************/
{
  xmlDocPtr doc;
  enum GFADesignator GFADesignatorType = SIERRA;
  char typeBulletin[8];
  int numberOfBulletins,i,j,num_vals;
  BUFR_Info_t bufr_info;
  Data_MixVal_t *bufr_rec;
  FXY_t *rec_fxy=0x0;
  int num_fxys;
  int vals_check=0;
  GFAByDesignatorInfo *bufr_prerec=0x0;
  char bufrfilename[MAXST];
  char dateStamp[16];
  doc = xmlReadMemory(docname,size,"noname.xml",NULL,0);
  if (doc==NULL) {
    printf("afcreate_bufr::Error in parsing document.\n");
    exit(1);
  }
  numberOfBulletins=get_number_of_nodes( doc, (xmlChar*) "//bulletin");
  for (i=0;i<numberOfBulletins;i++) {
    getInfoStrbySub(doc,"/gfaInfo","/bulletin[%d]/@type",i+1,typeBulletin);
    for(j=0;j<3;j++) {
      if (strncmp(GFADesignatorNames[j],typeBulletin,6)==0) {
        GFADesignatorType=(enum GFADesignator) j;
        break;
      }
    }
    bufr_prerec=BuildRec(doc,GFADesignatorType,&num_vals);
    buildFilenameAndDateStampFmIssTime(issTimeStr, outputformat,
                                         bufrfilename, dateStamp);
    if (debuglvl > 1) {
       BUFR_Debug(debuglvl);
       BUFR_Trace(debuglvl);
    }
    initBUFR(&bufr_info,bufrfilename,dateStamp,bufr_prerec->td);
    bufr_rec=(Data_MixVal_t *) malloc(sizeof(Data_MixVal_t) *num_vals);
    switch(GFADesignatorType) {
      case SIERRA:
        rec_fxy=loadFXYs(fxyPath,"afbufrfxy.sierra",&num_fxys);
        fill_arraySIERRA(bufr_prerec->b.sierra,bufr_rec,&vals_check);
        break;
      case TANGO:
        rec_fxy=loadFXYs(fxyPath,"afbufrfxy.tango",&num_fxys);
        fill_arrayTANGO(bufr_prerec->b.tango, bufr_rec, &vals_check);
        break;
      case ZULU:
        rec_fxy=loadFXYs(fxyPath,"afbufrfxy.zulu",&num_fxys);
        fill_arrayZULU(bufr_prerec->b.zulu, bufr_rec, &vals_check);
        break;
    }
    if (debuglvl > 0)
      printf("bufr_rec vals_check=%d \n",vals_check);
    if (vals_check!=num_vals) {
      printf("Inconsistency check on number of values loaded\n");
      printf("bufr_prerec, Number items Loaded: %d\n",num_vals);
      printf("bufr_rec, Number items Loaded: %d\n",vals_check);
      exit(1);
    }
    if (debuglvl > 0) {
      printf("Dumping bufr_rec contents...\n");
      for (i=0;i<vals_check;i++) {
        if (bufr_rec[i].Val_Type==DT_INT) {
          printf ("***DT_INT %d %d \n",i, bufr_rec[i].Val.int_number);
        }
        else if (bufr_rec[i].Val_Type==DT_STRING) {
          printf("***DT_STRING %d %s \n",i,bufr_rec[i].Val.string);
        }
        else if (bufr_rec[i].Val_Type==DT_FLOAT) {
          printf("***DT_FLOAT %d %f \n",i,bufr_rec[i].Val.ffloat);
        }
      }
    }
    if ( BUFR_Put_MixArray(bufr_rec,vals_check,rec_fxy,num_fxys)) {
      BUFR_perror(" Error on call to BUFR_Put_Array in encodeBUFR");

      exit(1);
    }
    if( BUFR_Encode( &bufr_info)) {
      BUFR_perror("Bufr_Encode error in encodeBUFR");
      BUFR_Destroy(1);
    }
    BUFR_Destroy(1);
    free(rec_fxy);
    free_prerec_items(bufr_prerec);
    free(bufr_prerec);
    /*free up all string pointers in bufr_rec*/
    free_strings(bufr_rec,vals_check);
    free(bufr_rec);
    xmlFreeDoc(doc);
  }
}

GFAByDesignatorInfo *BuildRec(xmlDocPtr doc, enum GFADesignator GFADesType,
                              int *num_vals)
{
  /***************************************************************************
  BuildRec

  This routine reads an XML document, containing information associated
  with one of three Hazard designators: SIERRA, TANGO, and ZULU, and populates
  the appropriate Descriptor Sequence structures.

  Input parameters:
    doc         xmlDocPtr       XML document
    GFADesType  GFADesignator   enumerated type SIERRA, TANGO, or ZULU
  Output parameters:
    *num_vals   int             Number of individual items loaded into
                                the structure.
  Returns:
    GFAByDesignatorInfo *       Pointer to untagged union structure
                                  containing SIERRA, TANGO, or ZULU elements.
  Log:
  L. Hinson         01/06
  *****************************************************************************/
  GFAByDesignatorInfo *rec=0x0;
  static SIERRA_t sierra;
  static TANGO_t tango;
  static ZULU_t zulu;
  int nv;
  char typeObject[MAXST];
  char xpathExpr[MAXST];
  enum GFATypes GFAObjectType = (enum GFATypes) 0;
  int numberOfGFAObjects;
  int i,j;
  rec=(GFAByDesignatorInfo *) malloc(sizeof(GFAByDesignatorInfo) * 1);
  nv=0;
  switch (GFADesType) {
    case SIERRA:
      sierra.repFactorIFR=sierra.repFactorMtnObsc=0;
      rec->td = SIERRA;
      rec->b.sierra=&sierra;

      nv=loadTimePeriod(doc, "//bulletin[@type='SIERRA']",
                      "/productValidTime", &sierra.tp , nv ) ;
      numberOfGFAObjects=get_number_of_nodes( doc,(xmlChar*)
                         "//bulletin[@type='SIERRA']/gfaObject");
      for(i=0;i<numberOfGFAObjects;i++) {
        sprintf(xpathExpr,"%s[%d]","//bulletin[@type='SIERRA']/gfaObject",i+1);
        getInfoStr(doc,xpathExpr,"/@type",typeObject);
        for (j=0;j<numGFATypes;j++) {
          if (strncmp(GFATypeNames[j],typeObject,7)==0) {
            GFAObjectType= (enum GFATypes) j;
            break;
          }
        }
        switch (GFAObjectType) {
          case IFR:
          case IFR_CIG:
          case IFR_VIS:
            sierra.repFactorIFR++;
            if( sierra.repFactorIFR == 1) {
              G_MALLOC ( sierra.IFR, GFA_IFRCigAndVis_t, sierra.repFactorIFR,
                     "BuildRec: sierra.IFR" );
            } else {
              G_REALLOC ( sierra.IFR, GFA_IFRCigAndVis_t, sierra.repFactorIFR,
                       "BuildRec: sierra.IFR" );
            }
            nv=loadSierraIFR( doc, xpathExpr,
                            &sierra.IFR[sierra.repFactorIFR-1], nv);
            break;
          case MT_OBSC:
            sierra.repFactorMtnObsc++;
            if (sierra.repFactorMtnObsc == 1) {
              G_MALLOC ( sierra.MtnObsc, GFA_MtnObsc_t, sierra.repFactorMtnObsc,
                       "BuildRec: sierra.MtnObsc" );
            } else {
              G_REALLOC (sierra.MtnObsc, GFA_MtnObsc_t, sierra.repFactorMtnObsc,
                       "BuildRec: sierra.MtnObsc" );
            }
            nv=loadSierraMtnObsc( doc, xpathExpr,
                                &sierra.MtnObsc[sierra.repFactorMtnObsc-1], nv);
            break;
          default:
            printf("GFA Object Type of %s does not fit GFA designator of %s",
                   GFATypeNames[GFAObjectType], GFADesignatorNames[GFADesType]);
            break;
        }
      }
      nv+=2;
      break;
    case TANGO:
      tango.repFactorTurb=tango.repFactorSSW=tango.repFactorLLWS=0;
      rec->td=TANGO;
      rec->b.tango=&tango;
      nv=loadTimePeriod(doc, "//bulletin[@type='TANGO']",
                             "/productValidTime", &tango.tp, nv);
      numberOfGFAObjects=get_number_of_nodes( doc, (xmlChar*)
                         "//bulletin[@type='TANGO']/gfaObject");
      for(i=0;i<numberOfGFAObjects;i++) {
        sprintf(xpathExpr,"%s[%d]","//bulletin[@type='TANGO']/gfaObject",i+1);
        getInfoStr(doc, xpathExpr, "/@type", typeObject);
        for(j=0;j<numGFATypes;j++) {
          if (strncmp(GFATypeNames[j],typeObject,7)==0) {
            GFAObjectType= (enum GFATypes) j;
            break;
          }
        }
        switch (GFAObjectType) {
          case TURB:
          case TURB_HI:
          case TURB_LO:
            tango.repFactorTurb++;
            if (tango.repFactorTurb == 1) {
              G_MALLOC ( tango.Turb, GFA_Turbulence_t, tango.repFactorTurb,
                       "BuildRec: tango.Turb" );
            } else {
              G_REALLOC ( tango.Turb, GFA_Turbulence_t, tango.repFactorTurb,
                        "BuildRec: tango.Turb" );
            }
            nv=loadTangoTurb( doc, xpathExpr, GFAObjectType,
                           &tango.Turb[tango.repFactorTurb-1],nv);
            break;
          case SFC_WND:
            tango.repFactorSSW++;
            if (tango.repFactorSSW == 1) {
              G_MALLOC ( tango.SSW, GFA_SSW_t, tango.repFactorSSW,
                       "BuildRec: tango.SSW" );
            } else {
              G_REALLOC ( tango.SSW, GFA_SSW_t, tango.repFactorSSW,
                        "BuildRec: tango.SSW" );
            }
            nv=loadTangoSSW( doc, xpathExpr, &tango.SSW[tango.repFactorSSW-1],
                           nv);
            break;
          case LLWS:
            tango.repFactorLLWS++;
            if (tango.repFactorLLWS == 1) {
              G_MALLOC ( tango.LLWS, GFA_LLWS_t, tango.repFactorLLWS,
                       "BuildRec:tango.LLWS" );
            } else {
              G_REALLOC ( tango.LLWS, GFA_LLWS_t, tango.repFactorLLWS,
                        "BuildRec: tango.LLWS" );
            }
            nv=loadTangoLLWS(doc, xpathExpr, &tango.LLWS[tango.repFactorLLWS-1],
                            nv);
            break;
          default:
            printf("GFA Object Type of %s does not fit GFA designator of %s \n",
                   GFATypeNames[GFAObjectType], GFADesignatorNames[GFADesType]);
            break;
        }
      }
      nv+=3;
      break;
    case ZULU:
      zulu.repFactorIcing=zulu.repFactorFrzLvl=0;
      rec->td=ZULU;
      rec->b.zulu=&zulu;
      nv=loadTimePeriod( doc, "//bulletin[@type='ZULU']",
                       "/productValidTime",&zulu.tp, nv);
      numberOfGFAObjects=get_number_of_nodes( doc, (xmlChar*)
                        "//bulletin[@type='ZULU']/gfaObject");
      for(i=0;i<numberOfGFAObjects;i++) {
        sprintf(xpathExpr,"%s[%d]","//bulletin[@type='ZULU']/gfaObject",i+1);
        getInfoStr(doc, xpathExpr, "/@type", typeObject);
        for(j=0;j<numGFATypes;j++) {
          if (strncmp(GFATypeNames[j],typeObject,7)==0) {
            GFAObjectType= (enum GFATypes) j;
            break;
          }
        }
        switch (GFAObjectType) {
          case ICE:
            zulu.repFactorIcing++;
            if (zulu.repFactorIcing == 1) {
              G_MALLOC ( zulu.Icing, GFA_Icing_t, zulu.repFactorIcing,
                       "BuildRec: zulu.Icing" );
            } else {
              G_REALLOC ( zulu.Icing, GFA_Icing_t, zulu.repFactorIcing,
                        "BuildRec: zulu.Icing" );
            }
            nv=loadZuluIcing( doc, xpathExpr,
                            &zulu.Icing[zulu.repFactorIcing-1], nv);
            break;
          case FZLVL:
          case M_FZLVL:
            zulu.repFactorFrzLvl++;
            if (zulu.repFactorFrzLvl == 1) {
              G_MALLOC ( zulu.FrzLvl, GFA_FreezingLvl_t, zulu.repFactorFrzLvl,
                         "BuildRec: zulu.FrzLvl" );
            } else {
              G_REALLOC ( zulu.FrzLvl, GFA_FreezingLvl_t, zulu.repFactorFrzLvl,
                          "BuildRec: zulu.FrzLvl" );
            }
            nv=loadZuluFrzLvl( doc, xpathExpr,
                               &zulu.FrzLvl[zulu.repFactorFrzLvl-1], nv);
            break;
          default:
            printf("GFA Object Type of %s does not fit GFA designator of %s \n",
                   GFATypeNames[GFAObjectType], GFADesignatorNames[GFADesType]);
            break;
        }
      }
      nv+=2;
      break;
  }
  *num_vals=nv;
  return (rec);
}

void initBUFR(BUFR_Info_t *bufr_info, char *bufrfilename, char *dateStamp,
              enum GFADesignator tp)
{
  /*****************************************************************************
  initBUFR

  This routines initializes Section 1 of the BUFR message.
  Note:
  1. Version 12 of the Master Tables were created to house the GFA descriptor
     sequences.  Version 12 came from the Version 11 Master Tables and were
     modified accordingly.
  2. The time stamp of the BUFR message originates from the Issue Time of the
     product, passed to this module via 'dateStamp'.

  void initBUFR

  Input parameters:
    *bufr_info      BUFR_Info_t     BUFR_Info Structure
    *bufrfilename   char            Name of File to save BUFR contents
    *dateStamp      char            Time Stamp of BUFR message
    tp              GFADesignator   Enumerated type with these values:
                                    0 for SIERRA message
                                    1 for TANGO message
                                    2 for ZULU message
                                    - Used to assign a value to the Data
                                      subcategory.
    Log:
  L. Hinson/AWC     01/06
  L. Hinson/AWC     01/10    Changed to reference version 13 tables
  S. Jacobs/NCO     05/13    Changed to reference version 19 tables
  S. Jacobs/NCO     10/13    Changed to reference version 13 tables, again
  ****************************************************************************/

  /* Initialize the BUFR information structure. */
  if( BUFR_Info_Init( bufr_info ) ) {
    BUFR_perror( "main" );
    exit(1);
  }
  sscanf(dateStamp,"%*2d%2d%2d%2d_%2d%2d%*s",&bufr_info->Year,
         &bufr_info->Month,
         &bufr_info->Day, &bufr_info->Hour, &bufr_info->Minute);
  printf(">>>dateStamp is %s\n",dateStamp);
  bufr_info->BUFR_MasterTable  = 0;        /* Use WMO Standard */
  bufr_info->BUFR_Edition  = 3;            /* Use WMO Standard */
  bufr_info->OriginatingCenter = 7;        /* US NWS/NCEP */
  bufr_info->SubCenter = 8;                /* Aviation Weather Center */
  bufr_info->UpdateSequenceNumber = 0;     /* 0 == Original Message */
  bufr_info->DataCategory = 13;            /* GFA - Set to 13 - Forecasts -
Table A */
  bufr_info->DataSubCategory = (int)tp + 1;  /* BUFR message sub-type */
  bufr_info->VersionNumberOfMasterTables = 13;
  bufr_info->VersionNumberOfLocalTables  = 0; /* No local tables */
  bufr_info->Century                     = 20;
  bufr_info->Num_Data_Sets               = 1;
  bufr_info->MinorLocalVersion           = 0;
  bufr_info->SoftVNum = 5;
  bufr_info->SoftV2Num = 2;

  bufr_info->ObservedData                = 0; /* Section 3 flag */

  Set_Flag(NoAuto_FTP);
  Set_Flag(Allow_Dup_Tab_Entry);
  Set_Flag(No_Warn_Dup_Tab_Entry);

  if ( BUFR_Init(bufr_info, bufrfilename, ENCODING) ) {

    BUFR_perror("extract");
    exit(1);

  } /* end if(BUFR_init) */
}

FXY_t* loadFXYs(char *fxyPath, char *filenamein, int *num_fxys) {
  /****************************************************************************
  loadFXYs

  This routine loads the FXY descriptors from a file and returns a pointer to
  to those FXY descriptors.

  FXY_t* loadFXYs(fxyPath, filename, num_fxys)

  Input parameters:
  *fxyPath      char    path to FXY files
  *filename     char    name of file containing descriptor table
  Output parameters:
  *numfxys      int     number of FXYs

  Returns:
    FXY_t       *       Pointer to array of FXY descriptors

  Log:
  L. Hinson     01/06
  ****************************************************************************/
  FILE *fxy_file;
  FXY_t *fxy_pr;
  int *fxy_i;
  int *ipr;
  int i;
  FXY_t *rec_fxy;
  char filename[MAXST];
  if (strlen(fxyPath)>0)
    sprintf(filename,"%s/%s",fxyPath,filenamein);
  else
    strcpy(filename,filenamein);
  if ((fxy_file=fopen(filename,"rt"))==NULL) {
    printf("Cannot find FXY file=%s\n",filename);
    exit(1);
  }
  /* read FXYs from fxy input file */

  /* read number of FXYs */
  fscanf(fxy_file,"%d",num_fxys);
  /* allocate memory for FXYs */
  fxy_i=(int *) malloc(sizeof(int) * (uint_t) *num_fxys);
  ipr = fxy_i;
  rec_fxy = (FXY_t *) malloc(sizeof(FXY_t ) * (uint_t) *num_fxys);  /* packed
FXYs */
  fxy_pr = rec_fxy;
  /*  pack FXYS */
  for ( i=0; i< *num_fxys; i++){
    fscanf(fxy_file, "%d", ipr);
    *fxy_pr = FXY_Pack_Dec(*ipr);
    printf(" i=%d  fxy=%d  packed = %X\n", i, *ipr++, (unsigned int) *fxy_pr++);
  }
  fclose(fxy_file);
  /* free a pointer...rec_fxy deleted later*/
  free(fxy_i);

  return rec_fxy;
}

int loadSierraIFR(xmlDocPtr doc, char * xpathExpr, GFA_IFRCigAndVis_t *ifr, int
nv)
{
  int i,nobsc,flag;
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&ifr->prodStat);
  getInfoInt(doc,xpathExpr,"//dataSig/@bufrCode",&ifr->dataSig);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                           &ifr->giof, nv);
  getInfoInt(doc,xpathExpr,"//flightRules/@bufrCode",&ifr->flightRules);

  if (elementExists(doc,xpathExpr,"//cloudBase")) {
    getInfoInt(doc,xpathExpr,"//cloudBaseLimit/@bufrCode",&ifr->typeLimitCig);
    getInfoInt(doc,xpathExpr,"//cloudBase",&ifr->cloudBase);
    (ifr->cloudBase)*=30.48;  /*Hundreds Feet to Meters Conversion */
  } else {
    ifr->typeLimitCig=(int)BUFR_MISSING_VALUE;
    ifr->cloudBase=(int)BUFR_MISSING_VALUE;
  }
  if (elementExists(doc,xpathExpr,"//visibility")) {
    getInfoInt(doc,xpathExpr,"//visibilityLimit/@bufrCode",&ifr->typeLimitVis);
    getInfoInt(doc,xpathExpr,"//visibility",&ifr->horVis);
    (ifr->horVis)*=1609.3;  /* SM to Meters conversion */
    getInfoInt(doc,xpathExpr,"//numberObscurations",&nobsc);
    ifr->obsc = 0;
    ifr->charObsc = (int)BUFR_MISSING_VALUE; /* Missing */
    for (i=0; i < nobsc; i++) {
       getInfoIntbySub(doc, xpathExpr, "//obscuration[%d]/@bufrFlag",
                       i+1, &flag);
       ifr->obsc |= 1 << (21-flag);  /*ifr->obscuration is int at 32 bits
                                             on ILP32 system */
       /*
       if (elementExists(doc,xpathExpr,"//obscuration[%d]/@bufrCode"))
         getInfoInt(doc, xpathExpr, "//obscuration[%d]/@bufrCode",
       &ifr->charObsc); */
       if (flag == 13)  ifr->charObsc = 6; /* Handle Blowing Snow */
    }
    /* Hypothetical Test */
    /* for (i=0; i < 21; i++) {
        if (ifr->obsc & 1 << (21-i))
          printf("bit %d is set\n",i);
       } */
  } else {
    ifr->typeLimitVis=(int)BUFR_MISSING_VALUE;
    ifr->horVis=(int)BUFR_MISSING_VALUE;
    ifr->obsc=(int)BUFR_MISSING_VALUE;
    ifr->charObsc=(int)BUFR_MISSING_VALUE;
  }
  ifr->dataSigCnl=(int)BUFR_MISSING_VALUE;
  ifr->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=11;
  return nv;
}


int loadSierraMtnObsc(xmlDocPtr doc,char * xpathExpr, GFA_MtnObsc_t *mtno, int
nv)
{
  int i, nobsc, flag;
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&mtno->prodStat);
  getInfoInt(doc,xpathExpr,"//dataSig/@bufrCode",&mtno->dataSig);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                &mtno->giof,nv);
  getInfoInt(doc,xpathExpr,"//flightRules/@bufrCode",&mtno->flightRules);
  getInfoInt(doc,xpathExpr,"//numberObscurations",&nobsc);
  mtno->obsc=0;
  mtno->charObsc=(int)BUFR_MISSING_VALUE;
  for (i=0; i < nobsc; i++) {
    getInfoIntbySub(doc, xpathExpr,"//obscuration[%d]/@bufrFlag",
                    i+1,&flag);
    mtno->obsc |= 1 << (21-flag);  /*ifr->obscuration is int at 32 bits
                                          on ILP32 system */
    if (flag == 13)  mtno->charObsc = 6; /* Handle Blowing Snow */

  }
  mtno->dataSigCnl=(int)BUFR_MISSING_VALUE;
  mtno->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=7;
  return nv;
}

int loadTangoTurb(xmlDocPtr doc, char * xpathExpr, enum GFATypes
                  GFAObjectType, GFA_Turbulence_t *turb, int nv)
{
  char string[6];
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&turb->prodStat);
  getInfoInt(doc,xpathExpr,"//metFeature/@bufrCode",&turb->metFeature);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                        &turb->giof, nv ) ;
  /* Exception:  Modify SeqID to include Letter Designator "H"
     for High-Level Turb.  Include Letter Designator "L" for
     Low-Level Turb.
  */
  switch (GFAObjectType) {
    case TURB_HI:
      sprintf(string,"H%s",turb->giof.GFASeqId);
      strcpy(turb->giof.GFASeqId, string);
      break;
    case TURB_LO:
      sprintf(string,"L%s",turb->giof.GFASeqId);
      strcpy(turb->giof.GFASeqId, string);
      break;
    default:
      printf("GFA Object Type of %s does not fit GFA designator of TANGO\n.",
             GFATypeNames[GFAObjectType]);
      break;
  }
  getInfoInt(doc,xpathExpr,"//degreeOfTurb/@bufrCode",&turb->degOfTurb);
  turb->metFeatureCnl=(int)BUFR_MISSING_VALUE;
  turb->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=5;
  return nv;
}

int loadTangoSSW(xmlDocPtr doc, char * xpathExpr, GFA_SSW_t *ssw, int nv)
{
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&ssw->prodStat);
  getInfoInt(doc,xpathExpr,"//dataSig/@bufrCode",&ssw->dataSig);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                          &ssw->giof,nv) ;
  getInfoInt(doc,xpathExpr,"//windSpeedLimit/@bufrCode",&ssw->typeLimitWndSpd);
  getInfoInt(doc,xpathExpr,"//windSpeed",&ssw->windSpeed10m);
  (ssw->windSpeed10m)*=.5144;
  ssw->dataSigCnl=(int)BUFR_MISSING_VALUE;
  ssw->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=6;
  return nv;
}

int loadTangoLLWS(xmlDocPtr doc, char * xpathExpr, GFA_LLWS_t *llws, int nv)
{
  int flag;
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&llws->prodStat);
  getInfoInt(doc,xpathExpr,"//metFeature/@bufrCode",&llws->metFeature);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                          &llws->giof, nv ) ;
  getInfoInt(doc, xpathExpr, "//otherWxPhenomena/@bufrflag", &flag ) ;
  llws->othWxPhen = 1 << 6;  /* bit 12 of 18 bit field */
  llws->intOthWxPhen = (int) BUFR_MISSING_VALUE;
  llws->metFeatureCnl=(int)BUFR_MISSING_VALUE;
  llws->prodStatCnl= (int) BUFR_MISSING_VALUE;
  nv+=6;
  return nv;
}

int loadZuluIcing(xmlDocPtr doc, char * xpathExpr, GFA_Icing_t *icing, int nv)
{
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&icing->prodStat);
  getInfoInt(doc,xpathExpr,"//metFeature/@bufrCode",&icing->metFeature);

  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                         &icing->giof, nv );
  getInfoInt(doc,xpathExpr,"//airframeIcing/@bufrCode",&icing->airframeICG);
  icing->metFeatureCnl=(int)BUFR_MISSING_VALUE;
  icing->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=5;
  return nv;
}

int loadZuluFrzLvl(xmlDocPtr doc, char * xpathExpr, GFA_FreezingLvl_t *frzlvl,
                   int nv)
{
  getInfoInt(doc,xpathExpr,"/productStatus/@bufrCode",&frzlvl->prodStat);
  getInfoInt(doc,xpathExpr,"//dataSig/@bufrCode",&frzlvl->dataSig);
  nv=loadGFAIdObsOrFcstLoc(doc, xpathExpr, "//gfaIdObsOrFcstLoc",
                           &frzlvl->giof, nv );
  frzlvl->dataSigCnl=(int)BUFR_MISSING_VALUE;
  frzlvl->prodStatCnl=(int)BUFR_MISSING_VALUE;
  nv+=4;
  return nv;
}

int loadTimePeriod(xmlDocPtr doc, const char* xpathExpr, const char* element,
                  TimePeriod *tp, int nv)
{
  char validFromDate[12], validUntilDate[12],
       validFromTime[10],validUntilTime[10];
  char fxpathExpr[MXXPATH];
  sprintf(fxpathExpr,"%s%s",xpathExpr,element);
  getInfoStr(doc, fxpathExpr, "/validFrom/@date",validFromDate);
  getInfoStr(doc, fxpathExpr, "/validUntil/@date",validUntilDate);
  getInfoStr(doc, fxpathExpr, "/validFrom/@time",validFromTime);
  getInfoStr(doc, fxpathExpr, "/validUntil/@time",validUntilTime);
  sscanf(validFromDate,"%4d-%2d-%2d",&tp->bd.year,&tp->bd.month,&tp->bd.day);
  sscanf(validFromTime,"%2d:%2d:%*2d",&tp->bt.hour,&tp->bt.minute);
  sscanf(validUntilDate,"%4d-%2d-%2d",&tp->ed.year,&tp->ed.month,&tp->ed.day);
  sscanf(validUntilTime,"%2d:%2d:%*2d",&tp->et.hour,&tp->et.minute);
  nv+=10;
  return nv;
}

int loadGFAIdObsOrFcstLoc(xmlDocPtr doc, const char* xpathExpr,
                          const char* element,
                          GFAIdObsOrFcstLoc *giof, int nv)
{
    char fxpathExpr[MXXPATH];
    sprintf(fxpathExpr,"%s%s",xpathExpr,element);
    getInfoStr(doc,fxpathExpr, "/id", giof->GFASeqId);
    getInfoInt(doc,fxpathExpr, "//timeSig/@bufrCode", &giof->timeSig);
    nv=loadTimePeriod(doc, fxpathExpr, "//validTime", &giof->tp, nv);
    nv=loadDescOfFeature(doc, fxpathExpr, "//objectDescription", &giof->df,
nv);
    giof->timeSigCnl=(int)BUFR_MISSING_VALUE;
    nv+=3;
    return nv;
}

int loadDescOfFeature(xmlDocPtr doc, const char* xpathExpr, const char* element,
                      DESC_Feature *df, int nv)
{
  int i;
  char fxpathExpr[MXXPATH];
  char elementsub[MAXST];
  sprintf(fxpathExpr,"%s%s",xpathExpr,element);
  getInfoInt(doc,fxpathExpr,"/dimensionSig/@bufrCode",&df->dimSig);
  getInfoInt(doc,fxpathExpr,"//numberOfLevels",&df->repCount);
  G_MALLOC(df->hs, DESC_HorSect, df->repCount, "loadDescOfFeature: df->hs" );
  /* df->hs = (DESC_HorSect *) malloc(sizeof(DESC_HorSect) * df->repCount);*/
  for(i=0;i<df->repCount;i++) {
    sprintf(elementsub,"//horizontalDescription[%d]",i+1);
    nv=loadHorSctnDescOfFeature(doc, fxpathExpr, elementsub, &df->hs[i], nv);
  }
  df->dimSigCnl=(int)BUFR_MISSING_VALUE;
  nv+=3;
  return (nv);
}

int loadHorSctnDescOfFeature(xmlDocPtr doc, const char* xpathExpr,
                             const char* element, DESC_HorSect *hs, int nv)
/******************************************************************************
    loadHorSctnDescOfFeature

    This routine takes the content of the input XML document to load the
    DESC_HorSect structure for Sequence 3-01-028 (Horizontal section of
    a feature described as a polygon, circle, line, or point.)

    Input parameters:
      xmlDocPtr    doc           XML document
      xpathExpr    const char*   X-Path Expression to describe path to
                                 sub-element
      element      const char*   sub-element
    Output parameters:
      hs           DESC_HorSect  Descriptor Structure
      nv           int           Number of Values

    L. Hinson    1/06          Created
    L. Hinson    7/08          Added flight level significance cancel sequence
******************************************************************************/
{
  int i;
  char fxpathExpr[MXXPATH];
  sprintf(fxpathExpr,"%s%s", xpathExpr, element);
  if (elementExists(doc,fxpathExpr,"/flightLevelSig"))
    getInfoInt(doc,fxpathExpr,"/flightLevelSig/@bufrCode",&hs->flightLevelSig);
  else
    hs->flightLevelSig=(int)BUFR_MISSING_VALUE;
  if (elementExists(doc,fxpathExpr,"/flightLevel")) {
    if (elementExists(doc, fxpathExpr,"/flightLevelLimit")) {
      getInfoInt(doc,fxpathExpr,"/flightLevelLimit/@bufrCode",&hs->typeLimit);
      /* The following is ued to get passed the HexStr_Set WARNING on values
      of 7.5 somehow derived from 7 */
      if (hs->typeLimit == 7) hs->typeLimit=(int)BUFR_MISSING_VALUE;
    } else {
      hs->typeLimit=(int)BUFR_MISSING_VALUE;
    }
    getInfoInt(doc,fxpathExpr,"/flightLevel",&hs->flightLvl);
    (hs->flightLvl)*=30.48;  /* Hundreds Feet to Meters conversion */
  }
  else {
    hs->typeLimit=(int)BUFR_MISSING_VALUE;
    hs->flightLvl=(int)BUFR_MISSING_VALUE;
  }
  getInfoInt(doc,fxpathExpr,"/numberVertexPoints",&hs->repCountOnCoords);
  /* Dynamically Allocate Vertex Points */
  G_MALLOC ( hs->location, Location, hs->repCountOnCoords,
             "loadHorSctnDescOfFeature: hs->location");
  for ( i = 0; i < hs->repCountOnCoords; i++) {
    getInfoFloatbySub(doc, fxpathExpr, "/vertex[%d]/@latitude",
                      i+1, &(hs->location[i].lat));

    getInfoFloatbySub(doc, fxpathExpr, "/vertex[%d]/@longitude",
                      i+1,&(hs->location[i].lon));
    nv+=2;
  }
  hs->radOfFeature = (int)BUFR_MISSING_VALUE;
  hs->flightLevelSigCnl = (int)BUFR_MISSING_VALUE;
  nv+=6;
  return (nv);
}

int get_number_of_nodes(xmlDocPtr doc,const xmlChar* xpathExpr)
{
  xmlXPathContextPtr xpathCtx;
  xmlXPathObjectPtr xpathObj;
  int numberNodes=0;
  assert(xpathExpr);
  xpathCtx = xmlXPathNewContext(doc);
  if(xpathCtx == NULL) {
    fprintf(stderr,"Error: unable to create new XPath context\n");
    xmlFreeDoc(doc);
    return(-1);
  }
  xpathObj = xmlXPathEvalExpression(xpathExpr,xpathCtx);
  if(xpathObj == NULL) {
    fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n",
xpathExpr);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return(-1);
  }
  /* Cleanup */
  numberNodes=(xpathObj->nodesetval) ? xpathObj->nodesetval->nodeNr: 0;
  xmlXPathFreeObject(xpathObj);
  xmlXPathFreeContext(xpathCtx);
  return (numberNodes);
}

int getInfo(xmlDocPtr doc, const xmlChar* xpathExpr,char* output)
{
  xmlNodePtr cur;
  int size;
  xmlNodeSetPtr nodes;
  xmlXPathContextPtr xpathCtx;
  xmlXPathObjectPtr xpathObj;
  assert(xpathExpr);
  output[0]='\0';
  xpathCtx = xmlXPathNewContext(doc);
  if(xpathCtx == NULL) {
    fprintf(stderr,"Error: unable to create new XPath context\n");
    xmlFreeDoc(doc);
    return(-1);
  }
  xpathObj = xmlXPathEvalExpression(xpathExpr,xpathCtx);
  if(xpathObj == NULL) {
    fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n",
xpathExpr);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return(-1);
  }
  nodes =  xpathObj->nodesetval;
  size = (nodes) ? nodes->nodeNr : 0;

  if (size > 1)
    printf("Warning... More than one node found for this xpathexpr: %s\n",
xpathExpr);
  if (size > 0) {
    assert(nodes->nodeTab[0]);
    cur=nodes->nodeTab[0];
    if (cur->children)
      if (!xmlStrcmp(cur->children->name,(unsigned char *) "text")) {
      strcpy(output,(const char *) cur->children->content);
      }
      ;
  }
  /* Cleanup */
  xmlXPathFreeObject(xpathObj);
  xmlXPathFreeContext(xpathCtx);
  return(size);
}

void getInfoStr(xmlDocPtr doc, const char* xpathExpr, const char* element, char*
str)
{
  char fullxpathExpr[MXXPATH];
  int num;
  sprintf(fullxpathExpr,"%s%s",xpathExpr,element);
  num=getInfo(doc,(xmlChar *)fullxpathExpr,str);
  if (num==0) {
    str[0]='\0';
    printf("getInfoStr::Could not find value for XPATH=%s%s\n",xpathExpr,
            element);
  }
}

void getInfoStrbySub(xmlDocPtr doc, const char* xpathExpr, const char*
element_cstring, int sub, char* str)
{
  char elementwrk[MXXPATH];
  sprintf(elementwrk,element_cstring,sub);
  getInfoStr(doc,xpathExpr,elementwrk,str);
}

void getInfoInt(xmlDocPtr doc, const char* xpathExpr,const char* element,int*
number)
/****************************************************************************
  getInfoInt
  
  This routine extracts an element from an XML document and converts it to
  an integer value.  If the element is not a number or is NULL, the  number
  is set to 0.
  
  Input parameters:
    doc         xmlDocPtr   The XML Document 
    xpathExpr   char *      An Xpath Expression to get at a specific element.
    element     char *      The name of an element.
  Output parameters:
    number      int *
  
  Log:  
  L. Hinson     01/06  Created
  L. Hinson     01/10  Corrected to handle an empty element
***************************************************************************/  
{
  char fullxpathExpr[MXXPATH];
  char str[MAXST];
  int num;
  sprintf(fullxpathExpr,"%s%s",xpathExpr,element);
  num=getInfo(doc, (xmlChar *) fullxpathExpr,str);
  if (strstr(str,"SFC")==NULL && str[0] != '\0') {
    if (num > 0) {
      if (sscanf(str,"%d",number) != 1) *number = 0;
    } else {
      *number=0;
      printf("getInfoInt::Could not find value for XPATH=%s%s\n",xpathExpr,
           element);
    }
  } else {
    *number=0;
  }
}

void getInfoIntbySub(xmlDocPtr doc, const char* xpathExpr,const char*
element_cstring,int sub,int *number)
{
  char elementwrk[MXXPATH];
  sprintf(elementwrk,element_cstring,sub);
  getInfoInt(doc, xpathExpr, elementwrk, number);
}

void getInfoFloat(xmlDocPtr doc, const char* xpathExpr,const char*
element,float* real)
{
  char fullxpathExpr[MXXPATH];
  char str[MAXST];
  int num;
  sprintf(fullxpathExpr,"%s%s",xpathExpr,element);
  num=getInfo(doc,BAD_CAST fullxpathExpr,str);
  if (num > 0)
    sscanf(str,"%f",real);
  else {
    *real=0.0;
    printf("getInfoFloat::Could not find value for XPATH=%s%s\n",xpathExpr,
           element);
  }

}

void getInfoFloatbySub(xmlDocPtr doc, const char* xpathExpr,const char*
element_cstring, int sub, float *real)
{
  char elementwrk[MXXPATH];
  sprintf(elementwrk,element_cstring,sub);
  getInfoFloat(doc, xpathExpr, elementwrk, real);
}

int elementExists(xmlDocPtr doc, const char* xpathExpr, const char* element)
{
  char fxpathExpr[MXXPATH];
  sprintf(fxpathExpr,"%s%s",xpathExpr,element);
  if (get_number_of_nodes(doc, (xmlChar *) fxpathExpr) > 0)
    return (1);
  else
    return (0);
}

void fill_arraySIERRA(SIERRA_t *s, Data_MixVal_t *rec, int *num_vals)
{
  int i,sp;
  sp=0;
  sp=popTimePeriod(&s->tp,rec,sp);
  rec[sp].Val.int_number=s->repFactorIFR;
  rec[sp].Val_Type=DT_INT;sp++;
  for ( i=0; i < s->repFactorIFR; i++)
    sp=popSierraIFR( &s->IFR[i], rec, sp);
  rec[sp].Val.int_number=s->repFactorMtnObsc;
  rec[sp].Val_Type=DT_INT;sp++;
  for ( i=0; i < s->repFactorMtnObsc; i++)
    sp=popSierraMtnObsc( &s->MtnObsc[i], rec, sp);
  *num_vals=sp;
}

void fill_arrayTANGO(TANGO_t *t, Data_MixVal_t *rec, int *num_vals)
{
  int i,sp;
  sp=0;
  sp=popTimePeriod(&t->tp,rec,sp);
  rec[sp].Val.int_number=t->repFactorTurb;
  rec[sp].Val_Type=DT_INT;sp++;
  for ( i=0; i < t->repFactorTurb; i++)
    sp=popTangoTurb( &t->Turb[i], rec, sp );
  rec[sp].Val.int_number=t->repFactorSSW;
  rec[sp].Val_Type=DT_INT;sp++;
  for ( i=0; i < t->repFactorSSW; i++ )
    sp=popTangoSSW( &t->SSW[i], rec, sp);
  rec[sp].Val.int_number=t->repFactorLLWS;
  rec[sp].Val_Type=DT_INT; sp++;
  for ( i=0; i< t->repFactorLLWS; i++ )
    sp=popTangoLLWS( &t->LLWS[i], rec, sp);
  *num_vals=sp;
}

void fill_arrayZULU(ZULU_t *z, Data_MixVal_t *rec, int *num_vals)
{
  int i,sp;
  sp=0;
  sp=popTimePeriod(&z->tp,rec,sp);
  rec[sp].Val.int_number=z->repFactorIcing;
  rec[sp].Val_Type=DT_INT;sp++;
  for ( i=0; i < z->repFactorIcing; i++)
    sp=popZuluIcing( &z->Icing[i], rec, sp );
  rec[sp].Val.int_number=z->repFactorFrzLvl;
  rec[sp].Val_Type=DT_INT;sp++;
  /* Not Available */
  for ( i=0; i < z->repFactorFrzLvl; i++)
    sp = popZuluFrzLvl( &z->FrzLvl[i], rec, sp );
  *num_vals=sp;
}

int popDescField(DESC_Feature *df, Data_MixVal_t *rec,int sp) {
  int i,spstart,spend;
  spstart=sp;
  rec[sp].Val.int_number=df->dimSig; sp++;
  rec[sp].Val.int_number=df->repCount; sp++;
  spend=sp;
  for (i=spstart;i<spend;i++) {
    rec[i].Val_Type=DT_INT;
  }
  for(i=0;i<df->repCount;i++) {
    sp=popHorSect(&df->hs[i],rec,sp);
  }
  rec[sp].Val.int_number=df->dimSigCnl;
  rec[sp].Val_Type=DT_INT; sp++;
  return sp;
}

int popHorSect(DESC_HorSect *hs, Data_MixVal_t *rec,int sp)
/******************************************************************************
    popHorSect

    This routine reads the Structure for Sequence 3-01-028 (Horizontal
    section of a feature described as a polygon, circle, line or point)
    to prepares the Data_MixVal_t record for the creation of the BUFR
    message.

    Input Parameter:
      hs      DESC_HorSect *    Structure pointing to the Horizontal Section
                                of a feature Descriptor
      sp      int               Numeric Pointer to indicate location
                                of last sequence in rec.
    Output Parameter:
     rec      Data_MixVal_t *  Structure for output to the BUFR message.

    Returns:
      sp      int              Location of last sequence in rec.

    L. Hinson       01/06      Created
    L. Hinson       07/08      Added flightLevelSigCnl sequence
*******************************************************************************/
{
  int i,spstart,spend;
  spstart=sp;
  rec[sp].Val.int_number=hs->flightLevelSig;sp++;
  rec[sp].Val.int_number=hs->typeLimit;sp++;
  rec[sp].Val.int_number=hs->flightLvl;sp++;
  rec[sp].Val.int_number=hs->repCountOnCoords;sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  /* Now load lat/lon coords */
  for (i=0;i<hs->repCountOnCoords;i++) {
    rec[sp].Val.ffloat=hs->location[i].lat;
    rec[sp].Val_Type=DT_FLOAT;
    sp++;
    rec[sp].Val.ffloat=hs->location[i].lon;
    rec[sp].Val_Type=DT_FLOAT;
    sp++;
  }
  spstart=sp;
  rec[sp].Val.int_number=hs->radOfFeature;sp++;
  rec[sp].Val.int_number=hs->flightLevelSigCnl;sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popTimePeriod(TimePeriod *tp, Data_MixVal_t *rec, int sp)
{
  int i,spstart,spend;
  spstart=sp;
  rec[sp].Val.int_number=tp->bd.year;sp++;
  rec[sp].Val.int_number=tp->bd.month;sp++;
  rec[sp].Val.int_number=tp->bd.day;sp++;
  rec[sp].Val.int_number=tp->bt.hour;sp++;
  rec[sp].Val.int_number=tp->bt.minute;sp++;
  rec[sp].Val.int_number=tp->ed.year;sp++;
  rec[sp].Val.int_number=tp->ed.month;sp++;
  rec[sp].Val.int_number=tp->ed.day;sp++;
  rec[sp].Val.int_number=tp->et.hour;sp++;
  rec[sp].Val.int_number=tp->et.minute;sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popGFAIdObsOrFcst(GFAIdObsOrFcstLoc *giof, Data_MixVal_t *rec, int sp)
{
  rec[sp].Val.string=(char *) malloc(sizeof(char) * 4);
  /*Size is 24 bits ( 3 chars + 1 for NULL) */
  rec[sp].Val_Type=DT_STRING;
  strcpy(rec[sp].Val.string,giof->GFASeqId); sp++;
  rec[sp].Val.int_number=giof->timeSig;
  rec[sp].Val_Type=DT_INT; sp++;
  sp = popTimePeriod(&giof->tp, rec, sp);
  sp = popDescField(&giof->df, rec, sp);
  rec[sp].Val.int_number=giof->timeSigCnl;
  rec[sp].Val_Type=DT_INT; sp++;
  return sp;
}

int popSierraIFR(GFA_IFRCigAndVis_t *ifr, Data_MixVal_t *rec, int sp)
{
  int i,spstart,spend;
  rec[sp].Val.int_number=ifr->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=ifr->dataSig;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&ifr->giof,rec,sp);
  spstart=sp;
  rec[sp].Val.int_number=ifr->flightRules; sp++;
  rec[sp].Val.int_number=ifr->typeLimitCig; sp++;
  rec[sp].Val.int_number=ifr->cloudBase; sp++;
  rec[sp].Val.int_number=ifr->typeLimitVis;sp++;
  rec[sp].Val.int_number=ifr->horVis; sp++;
  rec[sp].Val.int_number=ifr->obsc; sp++;
  rec[sp].Val.int_number=ifr->charObsc; sp++;
  rec[sp].Val.int_number=ifr->dataSigCnl; sp++;
  rec[sp].Val.int_number=ifr->prodStatCnl; sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popSierraMtnObsc(GFA_MtnObsc_t *mtno, Data_MixVal_t *rec, int sp)
{
  int i, spstart, spend;
  spstart=sp;
  rec[sp].Val.int_number=mtno->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=mtno->dataSig;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&mtno->giof,rec,sp);
  spstart=sp;
  rec[sp].Val.int_number=mtno->flightRules; sp++;
  rec[sp].Val.int_number=mtno->obsc; sp++;
  rec[sp].Val.int_number=mtno->charObsc; sp++;
  rec[sp].Val.int_number=mtno->dataSigCnl; sp++;
  rec[sp].Val.int_number=mtno->prodStatCnl; sp++;
  spend=sp;
  for (i=spstart; i< spend; i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popTangoTurb(GFA_Turbulence_t *turb, Data_MixVal_t *rec, int sp)
{
  int i, spstart, spend;
  rec[sp].Val.int_number=turb->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=turb->metFeature;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&turb->giof,rec,sp);
  spstart=sp;
  rec[sp].Val.int_number=turb->degOfTurb; sp++;
  rec[sp].Val.int_number=turb->metFeatureCnl; sp++;
  rec[sp].Val.int_number=turb->prodStatCnl; sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popTangoSSW(GFA_SSW_t *ssw, Data_MixVal_t *rec, int sp)
{
  int i, spstart, spend;
  rec[sp].Val.int_number=ssw->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=ssw->dataSig;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&ssw->giof,rec,sp);
  spstart=sp;
  rec[sp].Val.int_number=ssw->typeLimitWndSpd; sp++;
  rec[sp].Val.int_number=ssw->windSpeed10m; sp++;
  rec[sp].Val.int_number=ssw->dataSigCnl;sp++;
  rec[sp].Val.int_number=ssw->prodStatCnl; sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popTangoLLWS(GFA_LLWS_t *llws, Data_MixVal_t *rec, int sp)
{
  int i, spstart, spend;
  rec[sp].Val.int_number=llws->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=llws->metFeature;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&llws->giof,rec,sp);
  spstart=sp;
  rec[sp].Val.int_number=llws->othWxPhen; sp++;
  rec[sp].Val.int_number=llws->intOthWxPhen; sp++;
  rec[sp].Val.int_number=llws->metFeatureCnl; sp++;
  rec[sp].Val.int_number=llws->prodStatCnl; sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popZuluIcing(GFA_Icing_t *icing, Data_MixVal_t *rec, int sp)
{
  int i, spstart, spend;
  rec[sp].Val.int_number=icing->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=icing->metFeature;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&icing->giof, rec, sp);
  spstart=sp;
  rec[sp].Val.int_number=icing->airframeICG;sp++;
  rec[sp].Val.int_number=icing->metFeatureCnl;sp++;
  rec[sp].Val.int_number=icing->prodStatCnl;sp++;
  spend=sp;
  for (i=spstart;i<spend;i++)
    rec[i].Val_Type=DT_INT;
  return sp;
}

int popZuluFrzLvl(GFA_FreezingLvl_t *frzlvl, Data_MixVal_t *rec, int sp)
{
  rec[sp].Val.int_number=frzlvl->prodStat;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=frzlvl->dataSig;
  rec[sp].Val_Type=DT_INT; sp++;
  sp=popGFAIdObsOrFcst(&frzlvl->giof, rec, sp);
  rec[sp].Val.int_number=frzlvl->dataSigCnl;
  rec[sp].Val_Type=DT_INT; sp++;
  rec[sp].Val.int_number=frzlvl->prodStatCnl;
  rec[sp].Val_Type=DT_INT; sp++;
  return sp;
}


void free_giof_items(GFAIdObsOrFcstLoc *giof)
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
{
  /****************************************************************************
  free_prerec_items(bufr_prerec);

  This frees up the following objects ...
  1) lat/lon objects
  2) Horizontal Sections
  3) IFR and MtnObsc objects
  4) Sierra, Tango, Zulu are not Freed; They are not Malloced

******************************************************************************/
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

void free_strings(Data_MixVal_t *bufr_rec, int count)
{
  int i;
  for (i=0; i< count; i++) {
    if (bufr_rec[i].Val_Type==DT_STRING) {
      free(bufr_rec[i].Val.string);
    }
  }
}

Boolean isGAirmetObj(char *hazard) {
  Boolean found = False;
  int i;
  for (i=0; i < numGFATypes; i++) {
    if (strcmp(GFATypeNames[i],hazard)==0) {
      found = True;
      break;
    }
  }
  return found;
}




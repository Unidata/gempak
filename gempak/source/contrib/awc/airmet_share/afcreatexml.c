#include "strings.h"
#include "geminc.h"
#include "gemprm.h"

#include "vgtag.h"
#include "vgstruct.h"
#include "gpc.h"
#include "afcreatexml.h"
#include "string.h"
#include "proto_gpc.h"

#define MAXST 128

/************************************************************************
 * afcreatexml.c                                                        *
 *                                                                      *
 * This module contains the subroutines to produce the AIRMET           *
 * information string from snapshot and smeared GFA elements.           *
 *                                                                      *
 * CONTENTS:                                                            *
 *   global functions:                                                  *
 *      af_createxml            - produce pre-xml document from GFA     *
 *                                                                      *
 *   private functions [derived from afcreate.c]:                       *
 *      af_FROMLineRulesforBUFR - clip/prepare GFA elem for US region   *
 *      af_fmt2xmlforBUFR       - generate information string           *
 *      Copied unaltered from afcreate.c:                               *
 *        af_elm2poly           - creates a gpc polygon from an GFA elem*
 *        af_addTags            - form an XML string with the given tag *
 *        af_catStr             - add string to the end of first string *
 *        af_poly2fmt           - Put GPC Polygon into an array of GFA  *
 *                                format str                            *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson/AWC        12/05   Created                                 *
 ***********************************************************************/


/*
 *  Private structure
 */

typedef struct          /* GFA SMEAR/OUTLOOK preprocessing structure
                            snapshots, smear, and outlook with hazard
                            type & tag are grouped */
{
  char                haz_type[8];    /* GFA Hazard type */
  char                tag[4];         /* GFA tag */
  Boolean             openFzlvl;      /* Open of closed FZLVL? */
  int                 nsnapshot;      /* Number of snapshots */
  VG_DBStruct         **snapshots;    /* Array of pointers to snapshots */
  VG_DBStruct         *smear;         /* Pointer to smear */
  VG_DBStruct         *outlook;       /* Pointer to outlook */
} GFA_SmrOlk_Grp;

typedef struct          /* GFA Element Formatting Structure */
{
  int                 delete;         /* Deleted or not                 */
  char                region;         /* Region ('W', 'C' or 'E')             */
  char                area[4];        /* FA Area ("BOS", "MIA"m etc.)         */
  char                adjarea[4];     /* Adjacent FA Area("BOS", etc., or "NIL")*/
  VG_DBStruct         el;             /* VG Structure                         */
  gpc_polygon         *el_poly;       /* GPC polygon Structure                */
  Boolean             openLine;       /* Open or closed line                  */
  int                 *reduceFlg;     /* Reduce-able flag for polygon vertices */
  GFA_SmrOlk_Grp      *origInfo;      /* Pointer to original GFA group        */
  char                *wording;       /* Accumulated condition information    */
} GFA_Elem_Format;

/*
 *  Private functions
 */

static void af_FROMLineRulesforBUFR(VG_DBStruct el_in, int *numFmt,
                                    GFA_Elem_Format **fmt, int *iret );

static void af_fmt2xmlforBUFR ( int ntypes,
                                char *types[3], char *refDate, char *cycle, int nin,
                                 GFA_Elem_Format *fmt_in,
                                 Boolean filterTimeSmears,
                                 char *issTimeStr,
                                 char *string[NUM_TYPES],
                                 int *iret );

static void af_catStr ( char **str1, char *str2);

static void af_addTags ( VG_DBStruct *el, char *xmlTag, char *gfaTag,
                         char **outStr );

static void af_elm2poly ( VG_DBStruct el, gpc_polygon *gpc_poly, int *iret );

static void af_poly2fmt ( gpc_polygon poly_in, VG_DBStruct el,
                          char region, GFA_Elem_Format *fmt, int *iret );

static void parsetime_strtof(char *fcst_hr, float *fcsthr);

/*
 * Global variables
 */

static long _capacity = ONEBLOCK;

/* Public Function */
void af_create_prexml(int ntypes, char *types[3], char *refDate,
                      char *cycle, int nin, VG_DBStruct *el_in,
                      Boolean filterTimeSmears, char *issTimeStr,
                      char *string[NUM_TYPES], int *iret )
{
/*****************************************************************************
  af_create_prexml

  This routine generates an information string (xml format), from the input
    GFA elements.   This is referenced as the pre-xml document.

  void af_create_prexml(ntypes, types, day, cycle, nin, el_in, string, iret)

  Input parameters:
    ntypes         int             number of Bulletin Types
    *types[3]      char            array of Bulletin Names
    *refDate       char            reference Date in YYYYMMDD format
    *cycle         char            cycle string
    *nin           int             number of input elements
    *el_in         VG_DBStruct     array of GFA elements
    *issTimeStr    char            Issue Time String

  Output parameters:
    **string          char            array of tagged string
    filterTimeSmears  Boolean       Indicator used to filter out GFA objects
                                    whose forecast hour tag contains a hyphen
                                    ("-")
    *iret             int             Return code
                                    0: normal return
  Log:
  L. Hinson           1/06          Created
*******************************************************************************/
    int                 ii, jj, ier, numFmt;
    char                tmpStr[ STD_STRLEN ];

    GFA_Elem_Format     *fmt;
    fmt = (GFA_Elem_Format *)NULL;
    numFmt = 0;
    for ( ii=0; ii < nin; ii++ ) {
      cvg_getFld( &el_in[ii],  TAG_GFA_SUBTYPE, tmpStr, &ier );
      af_FROMLineRulesforBUFR (el_in[ii], &numFmt, &fmt, &ier);
    }
        /*
     *  Generate the XML format string from the GFA format struct.
    */
    for ( jj = 0; jj < ntypes; jj++ ) {
            string[jj] = (char *)NULL;
    }
    af_fmt2xmlforBUFR(ntypes, types, refDate, cycle,
                      numFmt, fmt, filterTimeSmears,
                      issTimeStr, string, &ier);
    /*
     *  Free GFA format structures.
     */
    for ( ii = 0; ii < numFmt; ii++ ) {
        gpc_free_polygon( fmt[ii].el_poly );

        G_FREE ( fmt[ii].el_poly, gpc_polygon );
        G_FREE ( fmt[ii].el.elem.gfa.info.blockPtr[ 0 ], gfaBlock_t );
    }

    G_FREE ( fmt, GFA_Elem_Format );
}

static void af_FROMLineRulesforBUFR(VG_DBStruct el_in, int *numFmt,
                              GFA_Elem_Format **fmt, int *iret )
/*******************************************************************************
    af_FROMLineRulesforBUFR

    This routine loads the GFA elements to an array of format structures.

    static void af_FROMLineRulesforBUFR (el_in, numFmt, fmt, iret)
    Input parameters:
    el_in     VG_DBStruct     GFA elements

    Input/Output parameters:
    *numFmt   int             Number of format structures
    **fmt     GFA_Elem_Format Array of format struct

    Output parameters:
    *iret     int             Return code
                              0: normal return

    Log:
    L. Hinson 01/06           Created
********************************************************************************/
{
    int ier;
    gpc_polygon gpc_el_poly;
    int numGfaElem;
    char region='U';
    *iret = 0;
    numGfaElem=*numFmt+1;
    af_elm2poly ( el_in, &gpc_el_poly, &ier);

    if ( numGfaElem == 1 ) {
        G_MALLOC ( (*fmt), GFA_Elem_Format, numGfaElem,
           "af_FROMLineRulesforBUFR: fmt" );
    }
    else {
        G_REALLOC ( (*fmt), GFA_Elem_Format, numGfaElem,
           "af_FROMLineRulesforBUFR: fmt" );
    }
    af_poly2fmt( gpc_el_poly, el_in, region, &(*fmt)[ *numFmt ], &ier );
    (*numFmt)++;
    gpc_free_polygon ( &gpc_el_poly );
}

static void af_fmt2xmlforBUFR ( int ntypes,
                                char *types[3], char *refDate, char *cycle, int nin,
                                GFA_Elem_Format *fmt_in,
                                Boolean filterTimeSmears,
                                char *issTimeStr,
                                char *string[NUM_TYPES],
                                int *iret )
/******************************************************************************
    af_fmt2xmlforBUFR

    This routine creates an XML document in a simple format for use in creation
    of the G-AIRMET BUFR message.  This document is the pre-XML format that is
    referenced throughout the code.

    static void af_fmt2xmlforBUFR ( ntypes, types, refDate, cycle, nin, fmt_in,
      string, filterTimeSmears, issTimeStr, iret)

    Input parameters:
      ntypes           int              Number of types (3)
      types            char *           Types (SIERRA/TANGO/ZULU)
      refDate          char *           Reference Date associated with Cycle:
                                          YYYYMMDD format
      cycle            char *           Cycle  - in 2 digit hour format
      nin              int              Number of GFA Objects
      fmt_in           GFA_Elem_Format* Pointer to GFA Element Structure
      filterTimeSmears Boolean          Filter Time Smears Switch
      issTimeStr       char *           Issue Time String: YYYYMMDDHHMM
    Output parameters:
      string           char*[NUM_TYPES] Output String Representation of XML
                                        document
      iret             int *            Return Code
                                        0: Normal Code
 Log:  L. Hinson       1/06             Created
       L. Hinson       4/09             Removed reading of GFA table in support
                                        of processing C&V objects.
       L. Hinson       1/10             Set Base to SFC, if bottom is NULL
********************************************************************************/
{
  int         ii, jj, icycle, ier, itype;
  int         strSize;
  int         npts;
  int         gfaSubType = GFA_INIT;
  char        hazList[ STD_STRLEN ];
  char        timeStr[ 16 ], otlkStr[16], tmpStr[ STD_STRLEN ];
  char        top[ 16 ], bottom[ 16 ];
  char        level[ 16 ];

  char        hazardType[ STD_STRLEN ] = "";
  time_t      tt, validFrom_epoch, validUntil_epoch;
  struct tm   *localTm, prodUntilTime;
  float fcsthr1 = 0.0, fcsthr2 = 0.0;
  char fcst_hr[12], fcst_hr_a[6], fcst_hr_b[6], validFromS[14], validUntilS[14];
  char  fzlTop[ STD_STRLEN ], fzlBase[ STD_STRLEN ];
  Boolean     isSmear = False;
  int year, month, day;
  char tag[4];

  VG_DBStruct *el;
  /*---------------------------------------------------------------------*/

  sscanf(refDate,"%04d%02d%02d",&year,&month,&day);
  for ( itype = 0; itype < ntypes; itype++ ) {
    strSize = ONEBLOCK;
    _capacity = ONEBLOCK;
    G_MALLOC ( string[itype], char, strSize, "af_format string");

    sprintf( string[itype], "%s", XML_HDR );

    af_catStr ( &string[itype], "<gfaInfo>" );

    af_catStr ( &string[itype], "<hdr>");

    tt = time ( NULL );
    localTm = localtime ( &tt );

    af_catStr ( &string[itype], "<issueTime>" );
    af_catStr( &string[itype],issTimeStr);
    af_catStr ( &string[itype], "</issueTime>" );

    /* Get until time */
    icycle = atoi ( cycle );
    localTm->tm_year = year - 1900;
    localTm->tm_mon = month - 1;
    localTm->tm_mday = day;
    localTm->tm_hour = icycle;
    localTm->tm_min  = 0;
    tt = mktime ( localTm ) + 6*60*60;
    localTm = localtime ( &tt );
    sprintf ( timeStr, "%04d%02d%02d%02d%02d",
              localTm->tm_year+1900, localTm->tm_mon+1,localTm->tm_mday,
              localTm->tm_hour, localTm->tm_min );
    /* calculate the end of the outlook */
    prodUntilTime = *localTm;
    tt = mktime( localTm ) + 6*60*60;
    localTm = localtime( &tt );
    sprintf( otlkStr, "%02d%02d%02d", localTm->tm_mday,
            localTm->tm_hour, localTm->tm_min );

    af_catStr ( &string[itype], "<untilTime>" );
    af_catStr ( &string[itype], timeStr );
    af_catStr ( &string[itype], "</untilTime>" );

    af_catStr ( &string[itype], "</hdr>" );

          /*
    *  Loop over all GFA format structures to find matching structures
    *  (FA area and airmat type) and extract information needed.
    *
          */
    for ( ii = 0; ii < nin; ii++ ) {

      el = &fmt_in[ ii ].el;

              /*
      *  Skip the element if it is marked as deleted
              */
      if ( fmt_in[ii].delete ) {
        continue;
      }

      /*
      *  Extract information from matching elements.
      */
      if ( (int)el->hdr.vg_type == GFA_ELM &&
            !el->hdr.delete )  {

        /*
        *  set the isSmear flag (False indicates outlook)
        */
        isSmear = True;
        cvg_getFld ( el, TAG_GFA_SUBTYPE, tmpStr, &ier );
        gfaSubType = atoi( tmpStr );

        if ( ( gfaSubType == GFA_USER_OUTLOOK ) ||
              ( gfaSubType == GFA_SYSTEM_OUTLOOK ) ) {
          isSmear = False;
        }

        cvg_getFld (el, TAG_GFA_FCSTHR, fcst_hr, &ier);

        /* Filter out Time Smears, if option -filterTimeSmears is
        * specified
        */
        if (filterTimeSmears && strchr(fcst_hr,'-') != NULL ) continue;

        /*
        *  LLWS can never produce an outlook.  Skip formatting the element
        *  if that's the combination that's found.
        *
        *  Similarly a FZLVL or M_FZLVL outlook has no meaning --
        *  there is no outlook section of the freezing level
        *  paragraph in the Zulu airmet.
        */
        if( !isSmear && strcasecmp( hazList, "LLWS" ) == 0 ) continue;
        if( !isSmear && strcasecmp( hazList, "FZLVL" ) == 0 ) continue;
        if( !isSmear && strcasecmp( hazList, "M_FZLVL" ) == 0 ) continue;

        af_catStr ( &string[itype], "<smear>" );

        af_addTags ( el, "<tag>", TAG_GFA_TAG, &string[itype] );
        af_addTags ( el, "<fcstHr>", TAG_GFA_FCSTHR, &string[itype] );
  /*
        *  Hazard Type
  */
        cvg_getFld( el, TAG_GFA_AREATYPE, hazardType, &ier );

        /* af_TURBRenameHILO ( hazardType ); */

        af_catStr( &string[itype], "<hazard>" );
        af_catStr( &string[itype], hazardType );
        af_catStr( &string[itype], "</hazard>" );

        /* Ok from fcstHr, create tags for validFrom, and validUntil...
          Will make these assumptions...
          1. The Until Time is the end of a 6 - hour forecast.
          2. Values of Forecast Hours can be a single value to
              represent a single
              time snapshot, or a hyphenated value for a time smear.
          3.  For a snapshot
                start_othtm=untilTime - 6 hours + Fcst Hour.
                end_othtm = NULL
          4.  For a time smear (Start & End values separated by "-")
                start_othtm = untilTime - 6 hours + Fcst Hour Start
                end_othtm = untilTime - 6 hours + Fcst Hour End */

        if (strstr(fcst_hr,"-")==NULL) {
          parsetime_strtof(fcst_hr,&fcsthr1);
          fcsthr2=-999.0;
        }
        else {
          strncpy(fcst_hr_a,fcst_hr,strchr(fcst_hr,'-')-fcst_hr);
          fcst_hr_a[strchr(fcst_hr,'-')-fcst_hr]='\0';
          strcpy(fcst_hr_b,strchr(fcst_hr,'-')+1);
          parsetime_strtof(fcst_hr_a, &fcsthr1);
          parsetime_strtof(fcst_hr_b, &fcsthr2);
        }

        validFrom_epoch=mktime(&prodUntilTime)-3600*6+(int)(fcsthr1*3600);

        strftime(validFromS,14,"%Y%m%d%H%M",localtime(&validFrom_epoch));
        strcpy(validUntilS,validFromS);
        if ( fcsthr2 > 0.01 ) {
          validUntil_epoch=(int)(3600*(fcsthr2-fcsthr1))+validFrom_epoch;
          strftime(validUntilS,14,"%Y%m%d%H%M",localtime(&validUntil_epoch));
        }
        af_catStr( &string[itype], "<validFrom>" );
        af_catStr( &string[itype], validFromS );
        af_catStr( &string[itype], "</validFrom>" );
        af_catStr( &string[itype], "<validUntil>" );
        af_catStr( &string[itype], validUntilS );
        af_catStr( &string[itype], "</validUntil>" );

  /*
        * Lat/Lon & Close Flag info
  */
        af_catStr ( &string[itype], "<closeFlg>" );
        sprintf( tmpStr, "%d", (int)(el->hdr.closed) );
        af_catStr ( &string[itype], tmpStr );
        af_catStr ( &string[itype], "</closeFlg>" );

        af_catStr ( &string[itype], "<nLatLonPts>" );
        sprintf( tmpStr, "%d", el->elem.gfa.info.npts );
        af_catStr ( &string[itype], tmpStr );
        af_catStr ( &string[itype], "</nLatLonPts>" );

        af_catStr ( &string[itype], "<latPts>" );

        npts = el->elem.gfa.info.npts;
        tmpStr[0] = '\0';

        for ( jj = 0; jj < npts; jj++ ) {

          sprintf( &(tmpStr[strlen(tmpStr)]), "%-8.4f ",
                    el->elem.gfa.latlon[jj]);
        }
        tmpStr[strlen(tmpStr)-1] = '\0';

        af_catStr ( &string[itype], tmpStr );
        af_catStr ( &string[itype], "</latPts>" );

        af_catStr ( &string[itype], "<lonPts>" );

        npts = el->elem.gfa.info.npts;
        tmpStr[0] = '\0';

        for ( jj = npts; jj < 2*npts; jj++ ) {

          sprintf( &(tmpStr[strlen(tmpStr)]), "%-9.4f ",
                    el->elem.gfa.latlon[jj]);
        }
        tmpStr[strlen(tmpStr)-1] = '\0';

        af_catStr ( &string[itype], tmpStr );
        af_catStr ( &string[itype], "</lonPts>" );

  /*
        *  Frequency, Severity
  */
        af_addTags ( el, "<Frequency>", "<Frequency>",
                    &string[itype] );
        af_addTags ( el, "<Severity>", "<Severity>",
                    &string[itype]);
  /*
        *  Top/Bottom
  */
        cvg_getFld ( el, TAG_GFA_TOP, top, &ier );

        if ( ier == 0 ) {
          sprintf ( tmpStr, "<Top>%s</Top>", top );
          af_catStr ( &string[itype], tmpStr );
        }

        cvg_getFld ( el, TAG_GFA_BOTTOM, bottom, &ier );
        if ( ier == 0 ) {
          if (bottom[0] != '\0') {
            sprintf ( tmpStr, "<Base>%s</Base>", bottom );
          } else {
            strcpy ( tmpStr, "<Base>SFC</Base>" );
            cvg_getFld ( el, TAG_GFA_TAG, tag, &ier );
            printf ( ">WARNING::Bottom Tag is Empty.  Assign Base Element to SFC on hazard=%s tag=%s fcsthr=%s\n",
                      hazardType, tag, fcst_hr);
          }
          af_catStr ( &string[itype], tmpStr );
        }
        /*
        *  Add the freezing level base and top if they exist.
        */
        cvg_getFld( el, TAG_GFA_FZL_TOP, fzlTop, &ier );
        if( ier == 0 ) {
          sprintf( tmpStr, "<FzlTop>%s</FzlTop>", fzlTop );
          af_catStr( &string[itype], tmpStr );
        }

        cvg_getFld( el, TAG_GFA_FZL_BOTTOM, fzlBase, &ier );
        if( ier == 0 ) {
          sprintf( tmpStr, "<FzlBase>%s</FzlBase>", fzlBase );
          af_catStr( &string[itype], tmpStr );
        }

        cvg_getFld ( el, "Level", level, &ier );
        if ( ier == 0 ) {
          if (strcmp(level,"000")==0)
            sprintf( tmpStr,"<Level>SFC</Level>");
          else
            sprintf ( tmpStr, "<Level>%s</Level>", level );
          af_catStr ( &string[itype], tmpStr );
        }

        /*
        *  Some GFEs have a "DUE TO" field.  But Ice uses a Type field.
        *  If DUE_TO returns nothing then try Type, and load that into the
        *  DUE_TO xml element.
        */
        cvg_getFld ( el, "DUE TO", tmpStr, &ier );

        if ( ier < 0 ) {
          cvg_getFld ( el, "Type", tmpStr, &ier );
        }

        if ( ier == 0 ) {
          af_catStr ( &string[itype], "<DUE_TO>" );
          af_catStr ( &string[itype], tmpStr );
          af_catStr ( &string[itype], "</DUE_TO>" );
        }

        af_catStr ( &string[itype], "<conditions>" );
        af_catStr ( &string[itype], "" );
        af_catStr ( &string[itype], "</conditions>" );

        af_addTags ( el, "<Status>", TAG_GFA_STATUS,
                    &string[itype] );

        af_catStr ( &string[itype], "</smear>" );
      }
    }

    af_catStr ( &string[itype], "</gfaInfo>" );

    *iret = 0;
  } /* type */
}

static void af_catStr ( char **str1, char *str2)
/************************************************************************
 * af_catStr                                                            *
 *                                                                      *
 * This routine adds str2 at the end of str1 and realloc memory for str1*
 * if str1 is not large enough to add str2.                             *
 *                                                                      *
 *                                                                      *
 * static void af_catStr ( str1, str2 )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      **str1          char            string                          *
 *      *str2           char            string to add to str2           *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04           Created                         *
 * B. Yin/SAIC          12/04           Fixed the capacity bug          *
 ***********************************************************************/
{
    if ( (long)(strlen( *str1 ) + strlen( str2 )) >= _capacity ) {
       G_REALLOC ( *str1, char, _capacity + ONEBLOCK, "af_format" );
       _capacity += ONEBLOCK;
    }
    strcat ( *str1, str2 );
}

/*=====================================================================*/

static void af_addTags ( VG_DBStruct *el, char *xmlTag, char *gfaTag,
                         char **outStr )
/************************************************************************
 * af_addTags                                                           *
 *                                                                      *
 * This routine adds the begining tag, the contents and the ending tag  *
 * into an XML string.                                                  *
 *                                                                      *
 * static void af_addTags ( el, xmlTag, gfaTag, outStr )                *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el             VG_DBStruct     gfa element                     *
 *      *xmlTag         char            xml tag                         *
 *      *gfaTag         char            gfa tag                         *
 *                                                                      *
 * Output parameters:                                                   *
 *      **outstr        char            output string                   *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created                                 *
 ***********************************************************************/
{
    int         ier;
    char        tmpStr[ STD_STRLEN ];
/*---------------------------------------------------------------------*/

    af_catStr ( outStr, xmlTag );

    cvg_getFld ( el, gfaTag, tmpStr, &ier );

    if ( ier == 0 ) {
       af_catStr ( outStr, tmpStr );
    }

    sprintf ( tmpStr, "%c%c%s", xmlTag[ 0 ], '/', &xmlTag[ 1 ] );

    af_catStr ( outStr, tmpStr );

}

/*=====================================================================*/

static void af_elm2poly ( VG_DBStruct el, gpc_polygon *gpc_poly, int *iret )
/************************************************************************
 * af_elm2poly                                                          *
 *                                                                      *
 * Creates a GPC polygon in normalized coordinate from the GFA's points.*
 *                                                                      *
 * static void af_elm2poly ( el, gpc_poly, iret )                       *
 *                                                                      *
 * Input parameters:                                                    *
 *      el              VG_DBStruct     GFA element                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *gpc_poly       gpc_polyon      GPC polygon structure           *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05    Created                                 *
 ***********************************************************************/
{
    int                 hole = 0, ier = 0, np;
    float               *xnormal, *ynormal;
    gpc_vertex_list     verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;


    /*
     * Convert to normalized coordinate first.
     */
    np = el.elem.gfa.info.npts;
    G_MALLOC ( xnormal, float, np, "af_elm2poly: xnormal" );
    G_MALLOC ( ynormal, float, np, "af_elm2poly: ynormal" );

    gtrans ( sys_M, sys_N, &np, el.elem.gfa.latlon,
             &el.elem.gfa.latlon[np], xnormal, ynormal,
             &ier, strlen(sys_M), strlen(sys_N) );
    /*
     * Fill GPC polygon structure with points in the incoming GFA element.
     */
    gpc_poly->num_contours = 0;
    gpc_poly->hole         = (int*)NULL;
    gpc_poly->contour      = (gpc_vertex_list*)NULL;

    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;

    np = el.elem.gfa.info.npts;
    gpc_cvlist ( np, xnormal, ynormal, &verts, &ier );
    gpc_poly->num_contours = 0;
    gpc_add_contour ( gpc_poly, &verts, hole );

    free ( verts.vertex );
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

}

/*=====================================================================*/

static void af_poly2fmt ( gpc_polygon poly_in, VG_DBStruct el,
                     char region, GFA_Elem_Format *fmt, int *iret )
/************************************************************************
 * af_poly2fmt                                                          *
 *                                                                      *
 * Puts GPC polygons into an array of GFA format structure. The input   *
 * polygons are assumed in normalized coordinate.                       *
 *                                                                      *
 * static void af_poly2fmt ( poly_in, fmt, iret )                       *
 *                                                                      *
 * Input parameters:                                                    *
 *      poly_in         gpc_polyon      GPC polygon structure           *
 *      el              VG_DBStruct     VG element structure            *
 *      region          char            region?                         *
 *                                                                      *
 * Output parameters:                                                   *
 *      *fmt            GFA_Elem_Format GFA format structure            *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05    Created                                 *
 * J. Wu/SAIC          08/05    Freed memory                            *
 * T. Piper/SAIC        09/05   replaced '1' with np, where np=1        *
 * J. Wu/SAIC           08/05           Initialize "delete" to G_FALSE  *
 ***********************************************************************/
{
    int         nblock, blockLen, np, ier;
    float       *xnormal, *ynormal;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Fill the information.
     */
    fmt->delete = G_FALSE;
    fmt->region = region;
    fmt->area[0] = '\0';
    fmt->adjarea[0] = '\0';


    /*
     * Copy the element, need to convert the polygon vertices
     * into map coordinate.
     */
    nblock = el.elem.gfa.info.nblocks;
    blockLen = nblock * STD_STRLEN * sizeof ( char );

    fmt->el.elem.gfa.info.nblocks = nblock;

    G_MALLOC ( fmt->el.elem.gfa.info.blockPtr[ 0 ],
               gfaBlock_t, nblock, "af_poly2fmt: fmt.el.blockPtr" );

    memcpy ( &(fmt->el.hdr), &(el.hdr), sizeof( VG_HdrStruct ) );

    memcpy ( fmt->el.elem.gfa.info.blockPtr[ 0 ],
             el.elem.gfa.info.blockPtr[ 0 ], blockLen );

    np = poly_in.contour[ 0 ].num_vertices;

    G_MALLOC ( xnormal, float, np, "af_poly2fmt: xnormal" );
    G_MALLOC ( ynormal, float, np, "af_poly2fmt: ynormal" );

    gpc_gvlist ( &poly_in.contour[ 0 ],
                 &(fmt->el.elem.gfa.info.npts), xnormal, ynormal, &ier );

    gtrans ( sys_N, sys_M, &np, xnormal, ynormal,
                 fmt->el.elem.gfa.latlon,
                 &(fmt->el.elem.gfa.latlon[ np ]),
                 &ier, strlen(sys_N), strlen(sys_M) );

    fmt->el.hdr.recsz = (int) ( sizeof( VG_HdrStruct )
                + 2 * sizeof( int ) + blockLen
                + ( sizeof( float ) * (size_t)( 2 * np )));

    /*
     *  Add the contour to the new GFA format structure's polygon
     */
    np = 1;
    G_MALLOC ( fmt->el_poly, gpc_polygon, np, "af_poly2fmt: fmt->el_poly" );
    fmt->el_poly->num_contours = 0;
    fmt->el_poly->hole         = (int*)NULL;
    fmt->el_poly->contour      = (gpc_vertex_list*)NULL;

    gpc_add_contour ( fmt->el_poly, &poly_in.contour[0], G_FALSE );

    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

}

/*=====================================================================*/

void loadXmlfile(char **xmlString, char *filename, int *ier)
{
/**********************************************************************
  This routine reads an XML file and stores the contents to xmlString

  Input parameters:
    xmlString    char**  Pointer to XML Character String
    filename     char*   Name of XML file to open
  Output parameters:
    ier          int*    Error status
  Log:
  L. Hinson/AWC      01/07         Created
**********************************************************************/
  FILE *fin;
  char line[ONEBLOCK];
  *ier=0;
  fin = fopen(filename,"rt");
  if(fin != NULL) {
    while (fgets(line,ONEBLOCK-1,fin)) {
       af_catStr(xmlString,line);
    }
    fclose(fin);
  } else {
    printf("Could not read %s\n",filename);
    exit(1);
  }
}

/*=====================================================================*/

void writeToXmlfile(char *xmlString, char *issTimeStr, char
    *outputformat, int *ier)
{
/*******************************************************************************
  This routine writes an XML character String to an output file.

  void writeToXmlfile(xmlString, issTimeStr, outputformat, ier)

  Input parameters:
    xmlString       char *         XML Character String
    issTimeStr      char *         Issue Time String (Form=YYYYmmDDHHMM)
    outputformat    char *         Output Format String to be converted
      by strftime
  Output parameters:
    ier             int *          Error status.
  Log:
  L. Hinson/AWC      01/06         Created

  ****************************************************************************/
  char xmlfilename[MAXST];
  FILE *fout;
  int year,month,day,hr,min;
#ifdef Linux
  struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\0'};
#else
  struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0};
#endif
  *ier = 0;
  sscanf(issTimeStr,"%4d%2d%2d%2d%2d",&year,&month,&day,&hr,&min);
  trec.tm_year=year-1900;
  trec.tm_mon=month-1;
  trec.tm_mday=day;
  trec.tm_hour=hr;
  trec.tm_min=min;
  strftime(xmlfilename,80,outputformat,&trec);
  fout=fopen(xmlfilename,"wt");
  fprintf(fout,"%s",xmlString);
  fclose(fout);
}

/*=====================================================================*/

static void parsetime_strtof(char *fcst_hr, float *fcsthr)
{
/***************************************************************************
  This routine converts a forecast time string in HH:MM format to a
  floating value.
  void parsetime_strtof(char *fcst_hr, float *fcsthr)
  Input parameters:
    fcst_hr     char *       forecast time string
  Output paramters:
    fcsthr      float *      forecast hr, converted to floating value
  Log:
  L. Hinson/AWC  01/07       Created
  *************************************************************************/

  int hour, minute;
  if (strchr(fcst_hr, ':') != NULL) {
    sscanf(fcst_hr,"%d:%d",&hour,&minute);
    *fcsthr = hour+minute/60.0;
  } else {
    sscanf(fcst_hr,"%d",&hour);
    *fcsthr = (float) hour;
  }
}

/*=====================================================================*/

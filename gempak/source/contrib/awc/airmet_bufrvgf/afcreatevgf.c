
#include "afcreatevgf.h"

enum obscTypes obscTypeOrder[] = {CLDS,PCPN,BR,FG,HZ1,FU,SN};
char * obscTypeNames[] = {"","FG","","","","","","BR","HZ","FU","","","",
  "SN","CLDS","PCPN"};
static char _cycle[3];

/************************************************************************
  afcreatevgf.c

  This module contains the routines to encode GFA objects into VGF files
  containing either SIERRA, TANGO, or ZULU hazards. The structure
  defined in afbufr_common.h is referenced.  That structure is populated
  by the afreadbufr.c module.

  Contents:
    Routines to build VGF el structure
      buildSierraVGF - Process IFR & MTOBSC objects  from SIERRA structure
                       and write them to a VGF
      buildTangoVGF - Process Turb, LLWS, Strong Surface Winds from TANGO
                       structure.
      buildZuluVGF - Process Icing, Freezing Level, Multi-Freeze Level from
                       ZULU structure.

      buildSierraIFR
      buildSierraMtnObsc
      buildTangoTurb
      buildTangoSSW
      buildTangoLLWS
      buildZuluIcing
      buildZuluFrzLvl

    Miscellaneous Routines -
      impliedFrzlHazard
      gfaObjectValidAtHour
      gfaObjectValidOvrHrRange
      initPreVGFTag
      setCommonTags
      setObsc
      parsetime_ftostr

    Sequence Processor Functions...
      setGFAIdObsFcst
      setDescFeature
      setTimePeriod
      setProdStat

    VGF writer routine -
      writeVGF

    VGF header attributes routine -
      cvg_crthdr
***************************************************************************/

VG_DBStruct *createVGF(GFAByDesignatorInfo *bufr_prerec, enum GFADesignator
    GFADesignatorType, char *filename, char *cycle, Boolean getAllHoursSW,
    Boolean getAllHazardsSW, Boolean binFcstHoursSW, char fcstHours[50][6],
    int numFcstHours, char hazards[50][10], int numHazards, int *nIn)
/**************************************************************************
  This is the main routine used in creating VGF files.  Foreach of the
  specified hazards and forecast hours, the routine checks for the existence
  of a VGF file via the cfl_inqr function.  If it does not exist, it
  initializes a VGF file via the cvg_crvgf function.  Then, these
  functions are called based on the GFADesignatorType, specific hazard,
  and forecast hour:  buildSierraVGF, buildTangoVGF, buildZuluVGF.

  createVGF
  Input Parameters:
    bufr_prerec       GFADesignatorInfo * Structure housing decoded BUFR contents
    GFADesignatorType   GFADesignator       SIERRA, TANGO, ZULU
    filename            char *              Name of VGF file
    getAllHoursSW       Boolean             Switch set True if we are wanting to
                                            get all forecast hours.
    getAllHazardsSW     Boolean             Switch set to True if we are wanting
                                             to get all hazards.
    binFcstHoursSW      Boolean             Switch set to True if we are wanting
                                             to get objects over the specified
                                             range of forecast  hours.
    fcstHours           char[50][6]       Array of 6-character Forecast Hour
                                           Strings.
    numFcstHours        int               Number of Forecast Hour Strings
    hazards             char[50][10]      Array of 10-character Hazard Strings.
    numHazards          int               Number of Hazard Strings

  Output Parameters:
    nIn         int *                     Number of GFA Objects found.

    Returns: VG_DBStruct *                Pointer to el structure

  Log:  L. Hinson    10/07
        L. Hinson    06/08                Add cycle parameter
********************************************************************************/
{
  long fsize;
  VG_DBStruct *el=0x0;
  char newfile[256];
  char filerootname[256];
  char newfilename[256];
  char hazardType[10];
  int len;
  int ier, iret;
  int i, j, hour1, hour2;
  *nIn=0;
  /* Load the settings table */
  ces_rtbl(&iret);
  /*Grab root name of filename */
  len = strstr(filename,".")-filename;
  strcpy(filerootname,filename);
  filerootname[len]='\0';
  strcpy(_cycle,cycle);
  if (getAllHoursSW) numFcstHours=1;
  if (getAllHazardsSW) numHazards=1;
  for (j=0; j < numHazards; j++) {
    if (getAllHazardsSW)
      hazardType[0]='\0';
    else
      sprintf(hazardType,"_%s",hazards[j]);
    for (i=0; i < numFcstHours; i++) {
      if (getAllHoursSW) {
        hour1 = hour2 = 0;
        sprintf(newfilename,"%s%s.vgf",filerootname,hazardType);
      } else if (strstr(fcstHours[i],"-")==NULL) {
        hour1=atoi(fcstHours[i]);
        hour2=hour1;
        sprintf(newfilename,"%s%s_f%02d.vgf",filerootname, hazardType, hour1);
      } else {
        sscanf(fcstHours[i],"%d-%d",&hour1,&hour2);
        sprintf(newfilename,"%s%s_f%02d_%02d.vgf",filerootname,
                hazardType, hour1, hour2);
      }

      cfl_inqr(newfilename, NULL, &fsize, newfile, &ier);
      if (ier < 0) {
        cvg_crvgf(newfilename, &ier);
      } else {
        remove(newfilename);
        cvg_crvgf(newfilename, &ier);
      }
      switch (GFADesignatorType) {
        case SIERRA:
          el = buildSierraVGF(bufr_prerec->b.sierra, newfilename, getAllHoursSW,
                              hour1, hour2, getAllHazardsSW, binFcstHoursSW,
                              hazards[j], nIn, el);
          break;
        case TANGO:
          el = buildTangoVGF(bufr_prerec->b.tango, newfilename, getAllHoursSW,
                            hour1, hour2, getAllHazardsSW, binFcstHoursSW,
                            hazards[j], nIn, el);
          break;
        case ZULU:
          el = buildZuluVGF(bufr_prerec->b.zulu, newfilename, getAllHoursSW,
                            hour1, hour2, getAllHazardsSW, binFcstHoursSW,
                            hazards[j], nIn, el);
          break;
      }
    }
  }
  return el;
}

VG_DBStruct * buildSierraVGF( SIERRA_t *s, char * filename, Boolean getAllHoursSW,
                              int fhour1, int fhour2, Boolean getAllHazardsSW,
                              Boolean binFcstHoursSW, char * hazardType, int *nIn,
                              VG_DBStruct *el)
/********************************************************************************
  This function processes the IFR and Mountain Obscuration objects represented
  in the SIERRA structure and writes them to a VGF file.  The function will
  have the ability to selectively choose specified hazards, for specific
  forecast hours or time ranges.  The function also returns the el structure
  of type VG_DBStruct.

  Input Parameters:
   s                SIERRA_t *   SIERRA structure
   filename         char *       VGF filename
   getAllHoursSW    Boolean      Switch set to true,  to obtain all forecast hours.
   fhour1           int          Used when getAllHoursSW is False. Forecast Hour 1
   fhour2           int          Used when getAllHoursSW is False. Forecast Hour 2
   getAllHazardsSW  int          Switch set to true, to obtain all hazards.
   binFcstHoursSW   int          Switch set to true, when binning forecast hours.
   hazardType       char *       Used when getAllHazardsSW is False.  Objects
                                    for specified hazardType will be processed.
   Output Parameters:
      nIn           int *        Number of GFA objects processed.
   Input Output Parameter:
      el            VG_DBStruct *  VGF el structure

  Returns VG_DBStruct *.

  Log:  L. Hinson  10/07
********************************************************************************/
{
  /*VG_DBStruct *el=0x0;*/
  preVGFtag_t pvt;
  int i;
  if (getAllHazardsSW || strstr(hazardType,"IFR")!=NULL) {
    for (i=0; i < s->repFactorIFR; i++) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&s->tp,&s->IFR[i].giof.tp, fhour1, fhour2) ||
        (binFcstHoursSW &&
        gfaObjectValidOvrHrRange(&s->tp,&s->IFR[i].giof.tp, fhour1, fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildSierraVGF" );
        }
        else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildSierraVGF" );
        }

        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].hdr.delete=0;
        el[*nIn].elem.gfa.info.nblocks=0;

        initPreVGFTag(&pvt);
        buildSierraIFR(&s->tp, &s->IFR[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn],&pvt,filename);
        (*nIn)++;
      }
    }
  }
  if (getAllHazardsSW || strcmp(hazardType,"MT_OBSC")==0) {
    for (i=0; i<s->repFactorMtnObsc; i++) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&s->tp,&s->MtnObsc[i].giof.tp, fhour1, fhour2) ||
        (binFcstHoursSW &&
        gfaObjectValidOvrHrRange(&s->tp,&s->MtnObsc[i].giof.tp, fhour1, fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildSierraVGF" );
        } else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildSierraVGF" );
        }
        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].hdr.delete=0;
        el[*nIn].elem.gfa.info.nblocks=0;
        initPreVGFTag(&pvt);
        buildSierraMtnObsc(&s->tp, &s->MtnObsc[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn],&pvt,filename);
        (*nIn)++;
      }
    }
  }
  return el;
}

VG_DBStruct * buildTangoVGF(TANGO_t *t, char *filename, Boolean getAllHoursSW,
                            int fhour1, int fhour2, Boolean getAllHazardsSW,
                            Boolean binFcstHoursSW, char * hazardType, int *nIn,
                            VG_DBStruct *el)
{
  preVGFtag_t pvt;
  int i;
  if (getAllHazardsSW || strstr(hazardType,"TURB")!=NULL) {
    for (i=0; i < t->repFactorTurb; i++) {
      if (getAllHazardsSW || (t->Turb[i].giof.GFASeqId[0] == 'H' && strcmp(hazardType,"TURB-HI")==0)
                          || (t->Turb[i].giof.GFASeqId[0] == 'L' && strcmp(hazardType,"TURB-LO")==0)) {
        if (getAllHoursSW ||
          gfaObjectValidAtHour(&t->tp,&t->Turb[i].giof.tp, fhour1, fhour2) ||
           (binFcstHoursSW &&
               gfaObjectValidOvrHrRange(&t->tp,&t->Turb[i].giof.tp,fhour1,fhour2))) {
          if (*nIn == 0) {
            G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
          } else {
            G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
          }
          el[*nIn].hdr.vg_class=(char) CLASS_MET;
          el[*nIn].hdr.vg_type=(char) GFA_ELM;
          el[*nIn].hdr.grptyp = 8;
          el[*nIn].hdr.grpnum = 0;
          el[*nIn].elem.gfa.info.nblocks=0;
          initPreVGFTag(&pvt);
          buildTangoTurb(&t->tp, &t->Turb[i], filename, &el[*nIn], &pvt);
          writeVGF(&el[*nIn],&pvt,filename);
          (*nIn)++;
        }
      }
    }
  }
  if (getAllHazardsSW || strcmp(hazardType,"SFC_WND")==0) {
    for (i=0; i< t->repFactorSSW; i++) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&t->tp,&t->SSW[i].giof.tp, fhour1, fhour2) ||
        (binFcstHoursSW &&
         gfaObjectValidOvrHrRange(&t->tp,&t->SSW[i].giof.tp,fhour1,fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
        } else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
        }
        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].elem.gfa.info.nblocks=0;
        initPreVGFTag(&pvt);
        buildTangoSSW(&t->tp, &t->SSW[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn],&pvt,filename);
        (*nIn)++;
      }
    }
  }
  if (getAllHazardsSW || strcmp(hazardType,"LLWS")==0) {
    for (i=0; i < t->repFactorLLWS; i++) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&t->tp,&t->LLWS[i].giof.tp, fhour1, fhour2)||
        (binFcstHoursSW &&
         gfaObjectValidOvrHrRange(&t->tp,&t->LLWS[i].giof.tp, fhour1, fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
        } else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildTangoVGF" );
        }
        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].elem.gfa.info.nblocks=0;
        initPreVGFTag(&pvt);
        buildTangoLLWS(&t->tp, &t->LLWS[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn],&pvt,filename);
        (*nIn)++;
      }
    }
  }
  return el;
}

VG_DBStruct * buildZuluVGF(ZULU_t *z, char *filename, Boolean getAllHoursSW,
                           int fhour1, int fhour2, Boolean getAllHazardsSW,
                           Boolean binFcstHoursSW, char *hazardType, int *nIn,
                           VG_DBStruct *el)
{
  preVGFtag_t pvt;
  int i;
  if (getAllHazardsSW || strcmp(hazardType,"ICE")==0) {
    for (i=0; i < z->repFactorIcing; i++) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&z->tp,&z->Icing[i].giof.tp, fhour1, fhour2) ||
          (binFcstHoursSW &&
          gfaObjectValidOvrHrRange(&z->tp,&z->Icing[i].giof.tp,fhour1,fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildZuluVGF" );
        } else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildZuluVGF" );
        }
        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].elem.gfa.info.nblocks=0;
        initPreVGFTag(&pvt);
        buildZuluIcing(&z->tp, &z->Icing[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn], &pvt, filename);
        (*nIn)++;
      }
    }
  }
  for (i=0; i < z->repFactorFrzLvl; i++) {
    if (getAllHazardsSW || (strcmp(hazardType,"FZLVL")==0 &&
        (impliedFrzlHazard(&z->FrzLvl[i]) == GFA_HAZARD_FZLVL_SFC
            || impliedFrzlHazard(&z->FrzLvl[i]) == GFA_HAZARD_FZLVL))
        || (strcmp(hazardType,"M_FZLVL")==0 &&
        impliedFrzlHazard(&z->FrzLvl[i]) == GFA_HAZARD_M_FZLVL)) {
      if (getAllHoursSW ||
        gfaObjectValidAtHour(&z->tp,&z->FrzLvl[i].giof.tp, fhour1, fhour2) ||
        (binFcstHoursSW &&
        gfaObjectValidOvrHrRange(&z->tp,&z->FrzLvl[i].giof.tp, fhour1, fhour2))) {
        if (*nIn == 0) {
          G_MALLOC ( el, VG_DBStruct, *nIn + 1, "buildZuluVGF" );
        } else {
          G_REALLOC ( el, VG_DBStruct, *nIn + 1, "buildZuluVGF" );
        }
        el[*nIn].hdr.vg_class=(char) CLASS_MET;
        el[*nIn].hdr.vg_type=(char) GFA_ELM;
        el[*nIn].hdr.grptyp = 8;
        el[*nIn].hdr.grpnum = 0;
        el[*nIn].elem.gfa.info.nblocks=0;
        initPreVGFTag(&pvt);
        buildZuluFrzLvl(&z->tp, &z->FrzLvl[i], filename, &el[*nIn], &pvt);
        writeVGF(&el[*nIn], &pvt, filename);
        (*nIn)++;
      }
    }
  }
  return el;
}

int impliedFrzlHazard(GFA_FreezingLvl_t *frzlvl)
/************************************************************************
This routine examines Freezing Level structure for the replication Counts,
the typeLimit, and the flightLevelSig variables to determine if we have a
Freezing Level at the Surface, a regular freezing level, or a multi-freezing
level.

  impliedFrzlHazard(frzlvl)
  Input Parameter:
    frzlvl   GFA_FreezingLvl_t *  Freezing Level Structure
  Returns:  Enumerated value for GFA_HAZARD_FZLVL_SFC,
              GFA_HAZARD_FZLVL, or GFA_HAZARD_M_FZLVL.
Log:  L. Hinson 10/07
      L. Hinson 06/08    Examine dataSig var. for code figure 11 (Freezing
                         Level), or 12 (Multi-Freezing Level)
*************************************************************************/
{
if (frzlvl->dataSig == 11)  /* Is this a Freezing Level, Code Figure 11? */
    if (frzlvl->giof.df.hs[0].flightLevelSig == 20)
      return GFA_HAZARD_FZLVL_SFC;
    else
      return GFA_HAZARD_FZLVL;
  else /* Otherwise this must be code figure 12, Multi-Freezing Level */
    return GFA_HAZARD_M_FZLVL;
}

Boolean gfaObjectValidAtHour(TimePeriod *tp, TimePeriod *htp, int fhour1, int fhour2)
/************************************************************************************
This routine determines if the GFA Objects' Time is valid at the specified forecast
hour or time smear of hours.

gfaObjectValidAtHour(tp,htp,fhour1,fhour2)
  Input Parameters:
    tp     TimePeriod*  Product From/Until Time Period
    htp    TimePeriod*  Valid From/Until Time Period
    fhour1 int          First Forecast Hour
    fhour2 int          Second Forecast Hour
  Returns:  Boolean Value True/False

Log: L. Hinson  10/07
***********************************************************************************/
{
  time_t prodFrom_epoch, prodUntil_epoch, validFrom_epoch, validUntil_epoch;
  int fcsthr1, fcsthr2;
  loadEpochTime(tp->bd, tp->bt, &prodFrom_epoch);
  loadEpochTime(tp->ed, tp->et, &prodUntil_epoch);
  loadEpochTime(htp->bd, htp->bt, &validFrom_epoch);
  loadEpochTime(htp->ed, htp->et, &validUntil_epoch);
  fcsthr1 = (validFrom_epoch - prodUntil_epoch + 3600*6)/3600;
  fcsthr2 = fcsthr1 + (validUntil_epoch - validFrom_epoch)/3600;
  if (fcsthr1 == fhour1 && fcsthr2 == fhour2)
    return True;
  else
    return False;
}

Boolean gfaObjectValidOvrHrRange(TimePeriod *tp, TimePeriod *htp, int fhour1,
                                 int fhour2)
/************************************************************************************
This routine determines if the GFA Objects' Time is valid within the specified
range of forecast hours.  If  fhour1 <= GFA Object Time < fhour2, the routine
returns true.

gfaObjectValidOvrHrRange(tp,htp,fhour1,fhour2)
  Input Parameters:
    tp     TimePeriod*  Product From/Until Time Period
    htp    TimePeriod*  Valid From/Until Time Period
    fhour1 int          First Forecast Hour
    fhour2 int          Second Forecast Hour
  Returns:  Boolean Value True/False

Log: L. Hinson  10/07
***********************************************************************************/
{
  time_t prodFrom_epoch, prodUntil_epoch, validFrom_epoch, validUntil_epoch;
  int fcsthr1, fcsthr2;
  loadEpochTime(tp->bd, tp->bt, &prodFrom_epoch);
  loadEpochTime(tp->ed, tp->et, &prodUntil_epoch);
  loadEpochTime(htp->bd, htp->bt, &validFrom_epoch);
  loadEpochTime(htp->ed, htp->et, &validUntil_epoch);
  fcsthr1 = (validFrom_epoch - prodUntil_epoch + 3600*6)/3600;
  fcsthr2 = fcsthr1 + (validUntil_epoch - validFrom_epoch)/3600;
  if (fcsthr1 >= fhour1 && fcsthr2 < fhour2)
    return True;
  else
    return False;
}

void initPreVGFTag(preVGFtag_t *pvt)
/************************************************************************
  This routine initializes the elements of the pre-VGF tag structure to
  either blank or some common value.

  initPreVGFTag
  Input/Output Parameter:
        pvt     preVGFtag_t  *   Pre-VGF Tag Structure

Log:  L. Hinson 10/07
************************************************************************/
{
  strcpy(pvt->gfa_subType," 2");
  strcpy(pvt->gfa_areaType,"");
  strcpy(pvt->gfa_lineWidth," 2");  /* 2 for snapshots, 3 for smears */
  strcpy(pvt->gfa_fcstHr,"");
  strcpy(pvt->gfa_tag,"");
  strcpy(pvt->gfa_status,"");
  strcpy(pvt->frequency,"OCNL");
  strcpy(pvt->severity,"MOD");
  strcpy(pvt->contour,"Closed");
  strcpy(pvt->type,"");
  strcpy(pvt->DUE_TO,"");
  strcpy(pvt->gfa_top,"");
  strcpy(pvt->gfa_bottom,"");
  strcpy(pvt->gfa_fzltop,"");
  strcpy(pvt->gfa_fzlbottom,"");
  strcpy(pvt->gfa_fzlrange,"");
  strcpy(pvt->gfa_lineElm,"20");  /* Common amongst most except IFR*/
  strcpy(pvt->gfa_lineType," 1");
  strcpy(pvt->gfa_lat,"");
  strcpy(pvt->gfa_lon,"");
}

void writeVGF(VG_DBStruct *el, preVGFtag_t *pvt, char *fname)
/****************************************************************************
This function performs the final routines necessary to write and append an
object to a VGF file.

writeVGF
  Input parameters:
    el         VG_DBStruct *el    VGF el structure
    pvt        preVGFtag_t *      pre VGF Tag structure
    fname      char *             Name of VGF file

Log: L. Hinson 10/07
****************************************************************************/
{
  int start=-1, np, ier, loc;
  el->hdr.filled = (char) 0;
  el->hdr.smooth = (char) 0;
  np=el->elem.gfa.info.npts;
  cvg_crthdr(el, np, el->elem.gfa.latlon, el->elem.gfa.latlon+np, &ier);
  el->hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof(int) * 2 +
      sizeof(char) * STD_STRLEN * el->elem.gfa.info.nblocks ) +
      sizeof(float) * np * 2;
#ifdef V5102
  cvg_writef(el, start, el->hdr.recsz, fname, &loc, &ier);
#else
  cvg_writefD(el, start, el->hdr.recsz, fname, &loc, &ier );
#endif
  /* cvg_freeElPtr ( el ); */
}

void loadEpochTime(YYYYMMDD1 d, HHMM t, time_t *time_epoch)
/******************************************************************
This routine builds an epoch time from the given YYYYMMDD and HHMM
structures.

  loadEpochTime
    Input Parameters...
     d           YYYYMMDD1   Year, Month, Day structure
     t           HHMM        Time Structure
    Output Parameters...
     time_epoch  time_t *  Epoch time.

Log: L. Hinson 10/07
******************************************************************/
{
  struct tm *time1;
  time_t tt;
  tt=time(NULL);
  time1 = localtime(&tt);
  time1->tm_year = d.year - 1900;
  time1->tm_mon = d.month - 1;
  time1->tm_mday = d.day;
  time1->tm_hour = t.hour;
  time1->tm_min = t.minute;
  time1->tm_sec = 0;
  *time_epoch=mktime(time1);
}

void buildSierraIFR(TimePeriod *tp, GFA_IFRCigAndVis_t *IFR, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt)
/*************************************************************************
This funtion sets up TAG values in the VGF tag structure preVGFtag_t pvt with
the appropriate values from the IFR structure. This is done to establish an
order to the TAGS placed into the VGF file, so the contents of the VGF files
can be compared via the dumpvgf software.  Once the pre-VGF tag structure
is populated, the TAGS can be set in an orderly fashion via the cvg_setFld
function.

buildSierraIFR
  Input Parameters:
         tp          TimePeriod *          Validity Time Period
         IFR         GFA_IFRCigAndVis_t *  IFR  Structure
         fname       char *                VGF file name
  Output Parameters:
         el          VG_DBStruct *         VGF el structure
         pvt         preVGFtag_t           VGF TAG structure to store tags

Log: L. Hinson  10/07
*************************************************************************/
{
  int ier;
  int subType;
  char type[STD_STRLEN]="";
  strcpy(pvt->gfa_areaType,"IFR");
  if (IFR->typeLimitCig != (int) BUFR_MISSING_VALUE) {
    strcpy(type,"CIG BLW 010");
  }
  if (IFR->typeLimitVis != (int) BUFR_MISSING_VALUE) {
    strcpy(type,"VIS BLW 3SM ");
  }
  if (IFR->typeLimitCig != (int) BUFR_MISSING_VALUE &&
      IFR->typeLimitVis != (int) BUFR_MISSING_VALUE) {
    strcpy(type,"CIG BLW 010/VIS BLW 3SM ");
  }
  setProdStat(IFR->prodStat, pvt);
  setGFAIdObsFcst(tp, &IFR->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setObsc(IFR->obsc,IFR->charObsc, type,pvt);
  setCommonTags( el, pvt);
  cvg_setFld (el, "<Type>", pvt->type, &ier);
}

void buildSierraMtnObsc(TimePeriod *tp, GFA_MtnObsc_t *MtnObsc, char *fname,
                        VG_DBStruct *el, preVGFtag_t *pvt)
/****************************************************************************
This function sets up TAG values in the VGF tag structure preVGFtag_t pvt with
the appropriate values from the Mountain Obscuration structure.  

buildSierraMtnObsc
  Input Parameters:
         tp          TimePeriod *      Validity Time Period
         MtnObsc     GFA_MtnObsc_t *   Mountain Obscuration Structure
         fname       char *            VGF file name
  Output Parameters:
         el          VG_DBStruct *     VGF el structure
         pvt         preVGFtag_t *     VGF TAG structure to store tags
  Log: L. Hinson  10/07
       L. Hinson  06/09 Reword MTNS OCNL OBSC BY to MTNS OBSC BY required
                        by incpgfatxt.c for the labels.
***************************************************************************/       
{
  int subType,ier;
  strcpy(pvt->gfa_areaType,"MT_OBSC");
  setProdStat(MtnObsc->prodStat, pvt);
  setGFAIdObsFcst(tp, &MtnObsc->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setObsc(MtnObsc->obsc, MtnObsc->charObsc, "MTNS OBSC BY ",pvt);
  setCommonTags( el, pvt);
  cvg_setFld (el, "<Type>", pvt->type, &ier);
}

void buildTangoTurb(TimePeriod *tp, GFA_Turbulence_t *Turb, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt)
{
  int subType, ier;
  strcpy(pvt->gfa_areaType,"TURB");
  switch (Turb->giof.GFASeqId[0]) {
    case 'H':
      strcpy(pvt->gfa_areaType,"TURB-HI");
      break;
    case 'L':
      strcpy(pvt->gfa_areaType,"TURB-LO");
      break;
  }
  setProdStat(Turb->prodStat, pvt);
  setGFAIdObsFcst(tp, &Turb->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setCommonTags( el, pvt );
  cvg_setFld (el, "<Severity>",pvt->severity, &ier);
  cvg_setFld (el, TAG_GFA_TOP, pvt->gfa_top, &ier);
  cvg_setFld (el, TAG_GFA_BOTTOM, pvt->gfa_bottom, &ier);
}

void buildTangoSSW(TimePeriod *tp, GFA_SSW_t *SSW, char *fname,
                   VG_DBStruct *el, preVGFtag_t *pvt)
{
  int subType, ier;
  strcpy(pvt->gfa_areaType,"SFC_WND");
  setProdStat(SSW->prodStat, pvt);
  setGFAIdObsFcst(tp, &SSW->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setCommonTags( el, pvt );
}

void buildTangoLLWS(TimePeriod *tp, GFA_LLWS_t *LLWS, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt)
{
  int subType, ier;
  strcpy(pvt->gfa_areaType,"LLWS");
  setProdStat(LLWS->prodStat, pvt);
  setGFAIdObsFcst(tp, &LLWS->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setCommonTags( el, pvt );
}

void buildZuluIcing(TimePeriod *tp, GFA_Icing_t *Icing, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt)
{
  int subType, ier;
  strcpy(pvt->gfa_areaType,"ICE");
  setProdStat(Icing->prodStat, pvt);
  setGFAIdObsFcst(tp, &Icing->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setCommonTags( el, pvt );
  cvg_setFld (el, "<Type>", "ICE", &ier);
  cvg_setFld (el, "<Severity>",pvt->severity, &ier);
  cvg_setFld (el, TAG_GFA_TOP, pvt->gfa_top, &ier);
  cvg_setFld (el, TAG_GFA_BOTTOM, pvt->gfa_bottom, &ier);
  if (strcmp(pvt->gfa_bottom,"FZL")==0) {
    if (strcmp(pvt->gfa_fzlbottom,"999")!=0) {
      cvg_setFld (el, TAG_GFA_FZL_TOP, pvt->gfa_fzltop, &ier);
      cvg_setFld (el, TAG_GFA_FZL_BOTTOM, pvt->gfa_fzlbottom, &ier);
    }
  }
}

void buildZuluFrzLvl(TimePeriod *tp, GFA_FreezingLvl_t *frzlvl, char *fname,
                     VG_DBStruct *el, preVGFtag_t *pvt)
/*************************************************************************
This funtion sets up TAG values in the VGF tag structure preVGFtag_t pvt with
the appropriate values from the Freezing Level structure. This is done
to establish an order to the TAGS placed into the VGF file, so the contents
of the VGF files can be compared via the dumpvgf software.  Once the pre-VGF
tag structure is populated, the TAGS can be set in an orderly fashion via
the cvg_setFld function.

buildSierraIFR
  Input Parameters:
         tp          TimePeriod *          Validity Time Period
         frzlvl      GFA_FreezingLvl_t *   Freezing Level Structure
         fname       char *                VGF file name
  Output Parameters:
         el          VG_DBStruct *         VGF el structure
         pvt         preVGFtag_t *         VGF TAG structure to store tags

Log: L. Hinson  10/07
     L. Hinson  06/08 Examine dataSig var. for code figure 11 (Freezing
                      Level), or 12 (Multi-Freezing Level)
*************************************************************************/
{
  int subType, ier;
  if (frzlvl->dataSig == 11)  /* Code figure 11=FZLVL, 12=M_FZLVL */
    strcpy(pvt->gfa_areaType,"FZLVL");
  else
    strcpy(pvt->gfa_areaType,"M_FZLVL");
  setProdStat(frzlvl->prodStat, pvt);
  setGFAIdObsFcst(tp, &frzlvl->giof, pvt, el);
  sscanf(pvt->gfa_subType, "%d", &subType);
  ces_get(subType, el, &ier);
  setCommonTags( el, pvt );
  cvg_setFld (el, "<Contour>",pvt->contour, &ier);
  if (frzlvl->dataSig == 11) {  /* Is this a FZLVL ? */
    cvg_setFld (el, "<Level>",pvt->level, &ier);
    cvg_setFld (el, TAG_GFA_FZLRANGE, pvt->gfa_fzlrange, &ier);
  } else {
    cvg_setFld (el, TAG_GFA_TOP, pvt->gfa_top, &ier);
    cvg_setFld (el, TAG_GFA_BOTTOM, pvt->gfa_bottom, &ier);
  }
}

void setCommonTags(VG_DBStruct *el, preVGFtag_t *pvt)
/*****************************************************************************
  This routine set up common tags for all GFA elements.  These tags are set via
  the cvg_setFld function.

  setCommonTags(el, pvt)
  Input Parameter
    el           VG_DBStruct *       VGF el structure
  Output parameter:
    pvt          preVGFtag_t *       pre VGF tag

  Log:  L. Hinson 10/07
        L. Hinson 06/08         Add TAG_GFA_CYCLE
*****************************************************************************/
{
  int ier;
  cvg_setFld (el, TAG_GFA_AREATYPE, pvt->gfa_areaType, &ier);
  cvg_setFld (el, TAG_GFA_CYCLE, pvt->gfa_cycle, &ier);
  cvg_setFld (el, TAG_GFA_FCSTHR, pvt->gfa_fcstHr, &ier);
  cvg_setFld (el, TAG_GFA_TAG, pvt->gfa_tag, &ier);
  cvg_setFld (el, TAG_GFA_STATUS, pvt->gfa_status, &ier);
  cvg_setFld (el, TAG_GFA_SUBTYPE, pvt->gfa_subType, &ier);
  cvg_setFld (el, TAG_GFA_LAT, pvt->gfa_lat, &ier);
  cvg_setFld (el, TAG_GFA_LON, pvt->gfa_lon, &ier);
  cvg_setFld (el, TAG_GFA_ARROW_LAT, pvt->gfa_lat, &ier);
  cvg_setFld (el, TAG_GFA_ARROW_LON, pvt->gfa_lon, &ier);
}

void setObsc(int obsc, int charObsc, char *prestring, preVGFtag_t *pvt)
/*************************************************************************
This routine decodes the obscurations from the integer flag code variable obsc.
It sets the type tag set for the DUE_TO lines.
  setObsc(obsc, prestring, pvt)
  Input parameters:
      obsc         int            Integer flag code
      prestring    char *         String to be prepended before obscurations.
  Output parameter:
      pvt          preVGFtag_t *  pre VGF tag
**************************************************************************/
{
  int i;
  Boolean obscTypeFlags [21];
  for (i=0;i<21;i++) obscTypeFlags[i]=False;
  if (obsc != (int) BUFR_MISSING_VALUE)
    for (i=0; i<21; i++)
      if (obsc & 1 << (21-i))
        obscTypeFlags[i] = True;
  strcpy(pvt->type,prestring);
  for (i=0; i<7; i++)
    if (obscTypeFlags[obscTypeOrder[i]]) {
      if (charObsc == 6 && strcmp(obscTypeNames[obscTypeOrder[i]],"SN")==0)
        strcat(pvt->type,"BL");
      strcat(pvt->type,obscTypeNames[obscTypeOrder[i]]);
      strcat(pvt->type,"/");
    }
  ;
  if (pvt->type[strlen(pvt->type)-1]=='/')
    pvt->type[strlen(pvt->type)-1]='\0';
}

void setGFAIdObsFcst(TimePeriod *tp, GFAIdObsOrFcstLoc *giof, preVGFtag_t *pvt,
                     VG_DBStruct *el)
/******************************************************************************
This routine performs the following.
  1. Sets the pvt->gfa_tag to the sequence ID in giof->GFASeqID.
     For Turbulence Tags beginning with an 'H' or an 'L' to denote high or
     low level turbulence, strip the first character.
  2. Call setTimePeriod to parse tp and giof->tp to set pvt->gfa_fcsthr,
     pvt->gfa_subtype
  3. Call setDescFeature to parse giof->df (description of feature) structure
       for flight levels and lat/lon information.

  setGFAIdObsFcst(tp,giof,pvt,el)
  Input parameters:
    tp            Time Period              Validity Time Period.
    giof          GFAIdObsOrFcstLoc *      GFA Identification and Observed or
                                           Forecast Location
  Output parameters:
    pvt          preVGFtag_t*              pre VGF tag structure
    el           VG_DBStruct               VGF Structure
********************************************************************************/

{
  int seqnum;
  char seqlet;
  switch (giof->GFASeqId[0]) {
    case 'H':
      sscanf(giof->GFASeqId,"H%d%c",&seqnum,&seqlet);
      sprintf(pvt->gfa_tag,"%d%c",seqnum,seqlet);
      break;
    case 'L':
      sscanf(giof->GFASeqId,"L%d%c",&seqnum,&seqlet);
      sprintf(pvt->gfa_tag,"%d%c",seqnum,seqlet);
      break;
    default:
      strcpy(pvt->gfa_tag,giof->GFASeqId);
      break;
  }
  setTimePeriod(tp,&giof->tp,giof->df.hs[0].flightLevelSig,pvt);
  setDescFeature(&giof->df,pvt,el);
}

void setDescFeature(DESC_Feature *df, preVGFtag_t *pvt, VG_DBStruct *el)
/**************************************************************************
This routine performs the following functions...
  1.  Examines df->dimSig (dimensional Significance) variable to discern
      whether the gfa object is represented as a line or closed polygon.
  2. Loads the lat-lon coords from the df->hs[0].location structure
     to the el->elem.gfa.latlon structure.
  3. Sets the lat/lon coord of the text attribute box from pvt->gfa_lat
                                                      and  pvt->gfa_lon
  4. Set the gfa_bottom, gfa_fzlbottom, gfa_top, gfa_fzltop tags.

  setDescFeature(df, pvt, el)
  Input parameters:
    df         DESC_Feature        Description of Feature
  Output parameters:
    pvt        preVGFtag_t *       pre VGF tag
    el         VG_DBStruct *       VGF el structure
 Log:  L. Hinson 10/07
       L. Hinson 06/08   Examine flightLevelSig var. for code figures
                         34, 35, 36, or 37.
*************************************************************************/
{
  int topFltLvl, botFltLvl, topFzlFltLvl, botFzlFltLvl;
  float ernlon=-999.0;
  float ernlat=0.0;
  int i, np;
  switch (df->dimSig) {
    case 1:
      el->hdr.closed = (char) 0;
      strcpy(pvt->contour,"Open");
      break;
    case 2:
      el->hdr.closed = (char) 1;
      strcpy(pvt->contour,"Closed");
      break;
    default:
      el->hdr.closed = (char) 1;
      strcpy(pvt->contour,"Closed");
      break;
  }
  np = df->hs[0].repCountOnCoords;
  el->elem.gfa.info.npts = np;
  for (i=0; i < np; i++)  {
    if (df->hs[0].location[i].lon > ernlon) {
      ernlon=df->hs[0].location[i].lon;
      ernlat=df->hs[0].location[i].lat;
    }
    el->elem.gfa.latlon[i]=df->hs[0].location[i].lat;
    el->elem.gfa.latlon[i+np] = df->hs[0].location[i].lon;
  }
  sprintf(pvt->gfa_lat," %5.2f", ernlat);
  sprintf(pvt->gfa_lon," %5.2f", ernlon);

  topFltLvl = -1;
  botFltLvl = 999;
  topFzlFltLvl = -1;
  botFzlFltLvl = 999;
  pvt->gfa_bottom[0]='\0';
  if (df->repCount >= 2) {
    for (i=0; i < df->repCount; i++) {
      switch (df->hs[i].flightLevelSig) {
        case 34:  /* FZL Base? */
          botFzlFltLvl = df->hs[i].flightLvl;
          sprintf(pvt->gfa_bottom,"%s","FZL");
          break;
        case 35:  /* FZL Top? */
          topFzlFltLvl = df->hs[i].flightLvl;
          sprintf(pvt->gfa_bottom,"%s","FZL");
          break;
        case 36:  /* Flight Level Base? */
          botFltLvl = df->hs[i].flightLvl;
          break;
        case 37:  /* Flight Level Top? */
          topFltLvl = df->hs[i].flightLvl;
          break;
        default:
          topFltLvl=G_MAX(df->hs[i].flightLvl,topFltLvl);
          botFltLvl=G_MIN(df->hs[i].flightLvl,botFltLvl);
          break;
      }
    }
    if (botFzlFltLvl == 0)
      strcpy(pvt->gfa_fzlbottom,"SFC");
    else
      sprintf(pvt->gfa_fzlbottom,"%03d",botFzlFltLvl);
    sprintf(pvt->gfa_fzltop,"%03d",topFzlFltLvl);
    if (strcmp(pvt->gfa_bottom,"FZL")!=0) {
      sprintf(pvt->gfa_bottom,"%03d",botFltLvl);
    }
    sprintf(pvt->gfa_top,"%03d",topFltLvl);
  }
  if (df->repCount == 1) {
    if (df->hs[0].flightLevelSig == 20)
      sprintf(pvt->level,"%s","SFC");
    else
      sprintf(pvt->level,"%03d", df->hs[0].flightLvl);
    if (df->hs[0].typeLimit == 3) {
      sprintf(pvt->gfa_bottom,"%s","SFC");
      sprintf(pvt->gfa_top,"%03d", df->hs[0].flightLvl);
    }
  }
}

void setTimePeriod(TimePeriod *tp,TimePeriod *htp, int flightLevelSig,
                   preVGFtag_t *pvt)
/***************************************************************************
The purpose of this routine is compute the forecast hour tag string derived
from information Validity Time period of the product and the time period of
the GFA Object.  The second purpose will be to compute and set the subType
Tag string used in the VGF files to discern between snapshots, timesmears
and outlooks. The subType is a function of the catType (determined by the
forecast hours), and the hazard Type.  The forecast hour tag will either be
represented as a single integer or two integers separated by hyphens.  This
sets pvt->gfa_fcstHr, pvt->subType

setTimePeriod(tp,  htp, flightLevelSig, pvt)
  Input parameters:
    tp                  TimePeriod         Validity Time Period of product.
    htp                 TimePeriod         Time Period of GFA Object
    flightLevelSig      int                Used for Setting the subType Tag
                                             string for SFC Freezing Levels.
  Output parameters:
    pvt                 preVGFtag_t*       pre VGF tag structure.

***************************************************************************/
{
  time_t prodFrom_epoch, prodUntil_epoch, validFrom_epoch, validUntil_epoch;
  float fcsthr1, fcsthr2;
  char fcst_hr_a[6], fcst_hr_b[6];
  int catType, hazType;
  strcpy(pvt->gfa_cycle, _cycle);
  loadEpochTime(tp->bd, tp->bt, &prodFrom_epoch);
  loadEpochTime(tp->ed, tp->et, &prodUntil_epoch);
  loadEpochTime(htp->bd, htp->bt, &validFrom_epoch);
  loadEpochTime(htp->ed, htp->et, &validUntil_epoch);
  fcsthr1 = (validFrom_epoch - prodUntil_epoch + 3600*6)/3600.0;
  fcsthr2 = fcsthr1 + (validUntil_epoch - validFrom_epoch)/3600.0;
  /* forecast hour 1 = forecast hour 2? */
  if (fabs(fcsthr1-fcsthr2) < .01) {
    /* Compute minutes if any */
    parsetime_ftostr(fcsthr1,pvt->gfa_fcstHr);
  } else {
    parsetime_ftostr(fcsthr1,fcst_hr_a);
    parsetime_ftostr(fcsthr2,fcst_hr_b);
    sprintf(pvt->gfa_fcstHr, "%s-%s", fcst_hr_a, fcst_hr_b);
  }
  /* Compute Subtype = hazType *10 + catType */
  /* Compute catType */
  /* if (fcsthr1 == fcsthr2) { */
  if (fabs(fcsthr1-fcsthr2) < .01) {
    catType = GFA_SNAPSHOT;
  }
  /* else if (fcsthr1 == 0 && fcsthr2 == 6) { */
  else if (fabs(fcsthr1) < .01 && fabs(fcsthr2 - 6) < .01) {
      catType = GFA_USER_SMEAR;
  }
  else {
      catType = GFA_USER_OUTLOOK;
  }
  /* Compute hazType */
  if (strcmp(pvt->gfa_areaType,"IFR")==0) {
    hazType = GFA_HAZARD_IFR;
  }
  else if (strcmp(pvt->gfa_areaType,"MT_OBSC")==0) {
    hazType = GFA_HAZARD_MT_OBSC;
  }
  else if (strcmp(pvt->gfa_areaType,"TURB")==0) {
    hazType = GFA_HAZARD_TURB;
  }
  else if (strcmp(pvt->gfa_areaType,"TURB-HI")==0) {
    hazType = GFA_HAZARD_TURB_HI;
  }
  else if (strcmp(pvt->gfa_areaType,"TURB-LO")==0) {
    hazType = GFA_HAZARD_TURB_LO;
  }
  else if (strcmp(pvt->gfa_areaType,"SFC_WND")==0) {
    hazType = GFA_HAZARD_SFC_WND;
  }
  else if (strcmp(pvt->gfa_areaType,"LLWS")==0) {
    hazType = GFA_HAZARD_LLWS;
  }
  else if (strcmp(pvt->gfa_areaType,"ICE")==0) {
    hazType = GFA_HAZARD_ICE;
  }
  else if (strcmp(pvt->gfa_areaType,"FZLVL")==0) {
    if (flightLevelSig == 20) {
      hazType = GFA_HAZARD_FZLVL_SFC;
    } else {
      hazType = GFA_HAZARD_FZLVL;
    }
  }
  else if (strcmp(pvt->gfa_areaType,"M_FZLVL")==0) {
    hazType = GFA_HAZARD_M_FZLVL;
  } else {
    hazType = GFA_HAZARD_IFR;
  }
  sprintf(pvt->gfa_subType,"%d", hazType*10 + catType);
}

void setProdStat(int prodStat, preVGFtag_t *pvt)
/***************************************************************************
  This routine sets the pvt->gfa_status from these prodStat values:
    0: NRML
    1: COR
    2: AMD
    4: CAN

  Input Parameter:
     prodStat   int            productStatus BUFR Code
  Output parameter:
     pvt        preVGFtag_t *  Pointer to pre VGF structure.

  Log:  L. Hinson  10/07
*****************************************************************************/
{
  switch (prodStat) {
    case 0:
      strcpy(pvt->gfa_status,"NRML"); break;
    case 1:
      strcpy(pvt->gfa_status,"COR"); break;
    case 2:
      strcpy(pvt->gfa_status,"AMD"); break;
    case 4:
      strcpy(pvt->gfa_status,"CAN"); break;
  }
}

void cvg_crthdr ( VG_DBStruct *el, int np, float *lat, float *lon,
                       int *iret )
/************************************************************************
    * cvg_crthdr                                                           *
    *                                                                      *
    * This function builds common header attributes for an VGF element     *
    *                                                                      *
    * cvg_crthdr ( el, np, lat, lon, iret )                                *
    *                                                                      *
    * Input parameters:                                                    *
    *      *el     VG_DBStruct     pointer to an element structure         *
    *       np     int             number of points                        *
    *      *lat    float           latitude to initialize max/min range    *
    *      *lon    float           longitude to initialize max/min range   *
    *                                                                      *
    * Output parameters:                                                   *
    *      *iret   int             Return code                             *
    *                                                                      *
    **                                                                     *
    * Log:                                                                 *
    * J. Wu/GSC    01/01   Created                                         *
 ***********************************************************************/
{
  int         ii;
  /*---------------------------------------------------------------------*/

  *iret = G_NORMAL;

  cvg_initelm( el );

    /*
  *  Find the maximum and minimum range.
    */
  el->hdr.range_min_lon = lon[0];
  el->hdr.range_min_lat = lat[0];
  el->hdr.range_max_lon = lon[0];
  el->hdr.range_max_lat = lat[0];

  for ( ii = 0; ii < np; ii++ ) {
    if ( el->hdr.range_min_lon > lon[ii] )
      el->hdr.range_min_lon = lon[ii];
    if ( el->hdr.range_min_lat > lat[ii] )
      el->hdr.range_min_lat = lat[ii];
    if ( el->hdr.range_max_lon < lon[ii] )
      el->hdr.range_max_lon = lon[ii];
    if ( el->hdr.range_max_lat < lat[ii] )
      el->hdr.range_max_lat = lat[ii];
  }
}

void parsetime_ftostr(float fcsthr, char *fcst_hr)
/********************************************************************************
This routine converts a floating point forecast hour to a string in HH:MM format.
parsetime_ftostr(fcsthr, fcst_hr)
Input Parameters:
      fcsthr    float     forecast hour time expressed as floating value
Output Parameters:
     fcst_hr    char *    forecast hour time expressed as a string HH:MM format.

Log:   L. Hinson 10/07
********************************************************************************/
{
  int hour, minute;
  hour = (int) fcsthr;
  minute = (fcsthr - (int) fcsthr)*60;
  if (minute != 0)
    sprintf(fcst_hr,"%d:%02d",hour,minute);
  else
    sprintf(fcst_hr,"%d",hour);
}

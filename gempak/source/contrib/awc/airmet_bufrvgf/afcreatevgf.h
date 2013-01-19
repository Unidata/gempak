/**********************************************************************
  This header file contains the structures and prototype declarations
  used in afcreatevgf.c
  
**
L. Hinson/AWC      10/07         Created
L. Hinson/AWC      06/08         Add cycle parameter to createVGF
**********************************************************************/
#ifndef _AFCREATEVGF_H
#define _AFCREATEVGF_H

#include "afbufr_structures.h"
#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "vgtag.h"
#include "drwids.h"

typedef struct {
  char gfa_subType[4];
  char gfa_areaType[STD_STRLEN];
  char gfa_lineWidth[3];
  char gfa_cycle[3];
  char gfa_fcstHr[STD_STRLEN];
  char gfa_tag[6];
  char gfa_status[5];
  char frequency[5];
  char severity[5];
  char contour[7];
  char level[4];
  char type[STD_STRLEN];
  char DUE_TO[STD_STRLEN];
  char gfa_top[5];
  char gfa_bottom[5];
  char gfa_fzltop[5];
  char gfa_fzlbottom[5];
  char gfa_fzlrange[STD_STRLEN];
  char gfa_lineElm[32];
  char gfa_lineType[32];
  char gfa_lat[20];
  char gfa_lon[20];
} preVGFtag_t;

/*Note: HZ is a reserved system parameter */
enum obscTypes { FG = 1, BR = 7, HZ1 = 8, FU = 9, SN = 13, CLDS = 14, PCPN =
15 };

void buildSierraIFR(TimePeriod *tp, GFA_IFRCigAndVis_t *IFR, char *fname,
                      VG_DBStruct *el, preVGFtag_t *pvt);
void buildSierraMtnObsc(TimePeriod *tp, GFA_MtnObsc_t *MtnObsc, char *fname,
                        VG_DBStruct *el,preVGFtag_t *pvt);
void cvg_crthdr ( VG_DBStruct *el, int np, float *lat, float *lon,
                           int *iret );
VG_DBStruct *createVGF(GFAByDesignatorInfo *bufr_prerec, enum GFADesignator
    GFADesignatorType, char *filename, char *cycle, Boolean getAllHoursSW, 
    Boolean getAllHazardsSW, Boolean binFcstHoursSW, char fcstHours[50][6],
    int numFcstHours, char hazards[50][10], int numHazards, int *nIn);
void loadEpochTime(YYYYMMDD1 d, HHMM t, time_t *time_epoch);
void setObsc(int obsc, int charObsc, char *prestring, preVGFtag_t *pvt);
void setGFAIdObsFcst(TimePeriod *tp, GFAIdObsOrFcstLoc *giof, preVGFtag_t *pvt,
                     VG_DBStruct *el);
void setDescFeature(DESC_Feature *df, preVGFtag_t *pvt, VG_DBStruct *el);
void setTimePeriod(TimePeriod *tp,TimePeriod *htp, int flightLevelSig,
                    preVGFtag_t *pvt);
void setProdStat(int prodStat, preVGFtag_t *pvt);
void initPreVGFTag(preVGFtag_t *pvt);
void writeVGF(VG_DBStruct *el, preVGFtag_t *pvt, char *fname);

void buildTangoTurb(TimePeriod *tp, GFA_Turbulence_t *Turb, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt);
void buildTangoSSW(TimePeriod *tp, GFA_SSW_t *SSW, char *fname,
                   VG_DBStruct *el, preVGFtag_t *pvt);
void buildTangoLLWS(TimePeriod *tp, GFA_LLWS_t *LLWS, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt);
VG_DBStruct * buildSierraVGF(SIERRA_t *s, char *filename, Boolean getAllHoursSW,
                             int fhour1, int fhour2, Boolean getAllHazardsSW,
                             Boolean binFcstHoursSW, char *hazard, int *nIn,
                             VG_DBStruct *el);
VG_DBStruct * buildTangoVGF(TANGO_t *t, char *filename, Boolean getAllHoursSW,
                            int fhour1, int fhour2, Boolean getAllHazardsSW,
                            Boolean binFcstHoursSW, char *hazard, int *nIn,
                            VG_DBStruct *el);
VG_DBStruct * buildZuluVGF(ZULU_t *z, char *filename, Boolean getAllHoursSW,
                           int fhour1, int fhour2, Boolean getAllHazardsSW,
                           Boolean binFcstHoursSW, char *hazard, int *nIn,
                           VG_DBStruct *el);
void buildZuluIcing(TimePeriod *tp, GFA_Icing_t *Icing, char *fname,
                    VG_DBStruct *el, preVGFtag_t *pvt);
void buildZuluFrzLvl(TimePeriod *tp, GFA_FreezingLvl_t *frzlvl, char *fname,
                     VG_DBStruct *el, preVGFtag_t *pvt);
int impliedFrzlHazard(GFA_FreezingLvl_t *frzlvl);
Boolean gfaObjectValidAtHour(TimePeriod *tp, TimePeriod *htp, int fhour1,
                             int fhour2);
Boolean gfaObjectValidOvrHrRange(TimePeriod *tp, TimePeriod *htp, int fhour1,
                                 int fhour2);
void setCommonTags(VG_DBStruct *el, preVGFtag_t *pvt);

void parsetime_ftostr(float fcsthr, char *fcst_hr);
#endif /* #ifndef _AFCREATEVGF_H */

/*************************************************************
 afreadbufr.h
 
 Header file for the G-Airmet BUFR decoding routines in
 afreadbufr.c.
 
 Log:  L. Hinson   10/07
       L. Hinson   10/08  Sequence change
       
**************************************************************/
 
#include "afbufr_common.h"
#include <time.h>

#define D_SIERRA        316071
#define D_TANGO         316072
#define D_ZULU          316073
#define D_ProdStat      8079
#define D_DataSig       8041
#define D_MetFeature    8011
#define D_DimSig        8007
#define D_FltLvlSig     8040
#define D_Lat           5002
#define D_Lon           6002
#define D_Radius        19007
#define MF_Turbulence   13
#define MF_Phenomena    16
#define DS_IFRCigAndVis 8
#define DS_MtnObsc      9
#define DS_SSW          10

GFAByDesignatorInfo *LoadRec(char* BUFR_File,
                             enum GFADesignator *GFADesType,
                             int *num_vals, int debuglvl);

int getDescInt(int *value);
int getDescStr(char *string);
int getDescFloat(float *value);
int getGFAIdObsOrFcstLoc(GFAIdObsOrFcstLoc *giof, int nv);
int getDescOfFeature(DESC_Feature *df, int nv);
int getHorSctnDescOfFeature(DESC_HorSect *hs, int flightLevelSig, int nv);
int getTimePeriod(TimePeriod *tp, int nv);
int getSierraIFR(GFA_IFRCigAndVis_t *ifr, int prodStat, int dataSig, int nv);
int getSierraMtnObsc(GFA_MtnObsc_t *mtno, int prodStat, int dataSig, int nv);
int getTangoTurb(GFA_Turbulence_t *turb, int prodStat, int metFeature, int nv);
int getTangoSSW(GFA_SSW_t *ssw, int prodStat, int dataSig, int nv);
int getTangoLLWS(GFA_LLWS_t *llws, int prodStat, int metFeature, int nv);
int getZuluIcing(GFA_Icing_t *icg, int prodStat, int metFeature, int nv);
int getZuluFrzLvl(GFA_FreezingLvl_t *frzlvl, int prodStat, int dataSig,
                  int nv);


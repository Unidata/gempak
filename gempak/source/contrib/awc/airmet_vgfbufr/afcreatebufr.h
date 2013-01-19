#ifndef _AFCREATEBUFR_H
#define _AFCREATEBUFR_H

#include <stdio.h>
#include <mel_bufr.h>
/* These 3 includes were added to make use of G_MALLOC, G_REALLOC, G_FREE from GEMPAK */
#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
/* These includes reference XML functions */
#include <libxml/xpath.h>
#define _AFBUFR_GBL
#include "afbufr_structures.h"

/* Primary Loader routine in afcreatebufr.c */
void afcreate_bufr(char *docname, int size, char *outputformat,
                   char *issTimeStr, char *fxyPath, int debuglvl, int *ier);
/* Routine to load Descriptor Sequence Structure of type GFAByDesignatorInfo */
GFAByDesignatorInfo *BuildRec(xmlDocPtr doc,enum GFADesignator GFAType, int *nv);
/* Routine to Initialize BUFR File Creation */
void initBUFR(BUFR_Info_t *bufr_info, char *bufrfilename, char *dateStamp, enum
GFADesignator tp);
/* Routine to load FXY Descriptor Table */
FXY_t* loadFXYs(char *fxyPath, char *filename,int *num_fxys);

/* Routines to Load Primary Descriptor Sequence Structures via BuildRec*/
/* Baseline Routines */
int loadSierraIFR(xmlDocPtr doc, char * xpathExpr, GFA_IFRCigAndVis_t *ifr, int nv);
int loadSierraMtnObsc(xmlDocPtr doc,char * xpathExpr, GFA_MtnObsc_t *mtno, int nv);
int loadTangoTurb(xmlDocPtr doc, char * xpathExpr,
                  enum GFATypes GFAObjectType, GFA_Turbulence_t *turb, int nv);
int loadTangoSSW(xmlDocPtr doc, char * xpathExpr, GFA_SSW_t *ssw, int nv);
int loadTangoLLWS(xmlDocPtr doc, char * xpathExpr, GFA_LLWS_t *llws, int nv);
int loadZuluIcing(xmlDocPtr doc, char * xpathExpr, GFA_Icing_t *icing, int nv);
int loadZuluFrzLvl(xmlDocPtr doc, char * xpathExpr, GFA_FreezingLvl_t *frzlvl,
                   int nv);
int loadTimePeriod(xmlDocPtr doc,const char* xpathExpr,const char* element,
                   TimePeriod *tp, int nv);
int loadGFAIdObsOrFcstLoc(xmlDocPtr doc, const char* xpathExpr,
                          const char* element, GFAIdObsOrFcstLoc *giof, int
nv);
int loadDescOfFeature(xmlDocPtr doc, const char* xpathExpr, const char* element,
                      DESC_Feature *df, int nv);
int loadHorSctnDescOfFeature(xmlDocPtr doc, const char* xpathExpr, const char*
element,
                            DESC_HorSect *hs, int nv);

/* Routines defined above call these routines to extract data from the XML
document */
int get_number_of_nodes(xmlDocPtr doc,const xmlChar* xpathExpr);
int getInfo(xmlDocPtr doc, const xmlChar* xpathExpr,char* output);
void getInfoStr(xmlDocPtr doc, const char* xpathExpr, const char* element, char*
str);
void getInfoStrbySub(xmlDocPtr doc, const char* xpathExpr, const char*
element_cstring,int sub, char* str);
void getInfoInt(xmlDocPtr doc, const char* xpathExpr, const char* element, int*
integer);
void getInfoIntbySub(xmlDocPtr doc, const char* xpathExpr,const char*
element,int sub,int *number);
void getInfoFloat(xmlDocPtr doc, const char* xpathExpr, const char* element,
float* real);
void getInfoFloatbySub(xmlDocPtr doc, const char* xpathExpr,const char* element,
int sub, float *real);
int elementExists(xmlDocPtr doc, const char* xpathExpr, const char* element);

/* Routines to XFR  Primary Descriptor Sequence Structures to MEL BUFR
   Data Arrays of type Data_MixVal_t... */
void fill_arraySIERRA(SIERRA_t *s, Data_MixVal_t *rec, int *num_vals);
void fill_arrayTANGO(TANGO_t *t, Data_MixVal_t *rec, int *num_vals);
void fill_arrayZULU(ZULU_t *z, Data_MixVal_t *rec, int *num_vals);

/* Primary routines defined above call these baseline routines: */
int popSierraIFR(GFA_IFRCigAndVis_t *ifr, Data_MixVal_t *rec, int sp);
int popSierraMtnObsc(GFA_MtnObsc_t *mtno, Data_MixVal_t *rec, int sp);
int popTangoTurb(GFA_Turbulence_t *turb, Data_MixVal_t *rec, int sp);
int popTangoSSW(GFA_SSW_t *ssw, Data_MixVal_t *rec, int sp);
int popTangoLLWS(GFA_LLWS_t *llws, Data_MixVal_t *rec, int sp);
int popZuluIcing(GFA_Icing_t *icing, Data_MixVal_t *rec, int sp);
int popZuluFrzLvl(GFA_FreezingLvl_t *frzlvl, Data_MixVal_t *rec, int sp);
int popTimePeriod(TimePeriod *tp, Data_MixVal_t *rec, int sp);
int popGFAIdObsOrFcst(GFAIdObsOrFcstLoc *giof, Data_MixVal_t *rec, int sp);
int popDescField(DESC_Feature *df, Data_MixVal_t *rec,int sp);
int popHorSect(DESC_HorSect *hs, Data_MixVal_t *rec,int sp);

/* Routines to Free up Memory used by Structures */
void free_prerec_items(GFAByDesignatorInfo *bufr_prerec);
void free_giof_items(GFAIdObsOrFcstLoc * giof);
void free_strings(Data_MixVal_t *bufr_rec, int count);

#endif  /* #ifndef _AFCFREATEBUFR_H */

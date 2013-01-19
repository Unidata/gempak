#include <mel_bufr.h>
#include "afbufr_structures.h"
#include "afreadbufr.h"
#include "geminc.h"
#include "gemprm.h"

int g_debuglvl = 0;

/***************************************************************
afread_bufr.c

This module contains the routines to read the GFA BUFR message.

Contents:
  Functions to Load Table D Descriptors listed in SIGMET proposal
    getHorSctnDescOfFeature - Loads Sequence 3-01-028
    getDescOfFeature - Loads Sequence 3-01-027
    getTimePeriod - Loads Sequence 3-1-014

  Functions to Load Table D Descriptors in GAirmet Proposal 
    getGFAIdObsOrFcstLoc
    getSierraIFR
    getSierraMtnObsc
    getTangoTurb
    getTangoSSW
    getTangoLLWS
    getZuluIcing
    getZuluFrzLvl
    
  Major Functions... 
    LoadRec - Populates structure to contain decoded message contents.
    
  Wrapper functions to get descriptor values
     getDescInt -  Gets the next integer
     getDescFloat - Gets the next float
     getDescString - Gets the next string
     
****************************************************************/
int getHorSctnDescOfFeature(DESC_HorSect *hs, int flightLevelSig, int nv)
/***************************************************************
  getHorSctnDescOfFeature  (Get Horizontal Section Description of Feature)
    (SIGMET Sequence 3-01-028)
  Output Parameter:
    hs              DESC_HorSect *   Horizontal Section Structure
  Input parameters:
    flightLevelSig  int              Flight Level Significance
    nv              int              Starting Number of Values read from
                                     BUFR
  Returns:
    int             Total Number of Values read from BUFR message           
  
  Log:
  L. Hinson        10/07    Created
  L. Hinson         7/08    Add flightLevelSigCnl Sequence
***************************************************************/
{
  int descrip;
  float value;
  hs->flightLevelSig = flightLevelSig;
  getDescInt(&hs->typeLimit);
  getDescInt(&hs->flightLvl);
  if (hs->flightLvl != (int)BUFR_MISSING_VALUE)
    hs->flightLvl = (int) (hs->flightLvl/30.48 + 0.5); /*Meters to Hundreds Feet
                                                       conversion */
  hs->repCountOnCoords=0;
  while ((descrip=getDescFloat(&value))==D_Lat) {
    hs->repCountOnCoords++;
    if (hs->repCountOnCoords == 1) {
      G_MALLOC ( hs->location, Location, hs->repCountOnCoords,
                "loadHorSctnDescOfFeature: hs->location");
    } else {
      G_REALLOC ( hs->location, Location, hs->repCountOnCoords,
                  "loadHorSctnDescOfFeature: hs->location");
    }
    hs->location[hs->repCountOnCoords-1].lat = value;
    getDescFloat(&hs->location[hs->repCountOnCoords-1].lon);
    nv+=2;
  }
  hs->radOfFeature=(int) value;
  getDescInt(&hs->flightLevelSigCnl);
  nv+=6;
  return nv;
}

int getDescOfFeature(DESC_Feature *df, int nv)
/*******************************************************************
  getDescOfFeature (Get Description of Feature)
    (SIGMET Sequence 3-01-027)
  Output Parameter:
    df    DESC_Feature *   Description of Feature Structure
  Input Parameter:
    nv    Starting Number of Values read from BUFR message
  
  Returns:
    int  Number of Values read from BUFR message.
    
  Log:
  L. Hinson    10/07
*******************************************************************/
{
  int descrip,valSigQual;
  getDescInt(&df->dimSig);
  df->repCount=0;
  while ((descrip=getDescInt(&valSigQual))!=D_DimSig) {
    (df->repCount)++;
    if (df->repCount == 1) {
      G_MALLOC(df->hs, DESC_HorSect, df->repCount, "getDescOfFeature: df->hs" );
    } else {
      G_REALLOC(df->hs, DESC_HorSect, df->repCount,
                "getDescOfFeature: df->hs");
    }
    nv=getHorSctnDescOfFeature(&df->hs[df->repCount - 1], valSigQual, nv);
  }
  df->dimSigCnl=valSigQual;
  nv+=3;
  return nv;
}

int getTimePeriod(TimePeriod *tp, int nv)
/******************************************************************
  getTimePeriod (Get Time Period)
    (SIGMET Sequence 3-01-014)
  Output Parameter:
    tp      TimePeriod *   Time Period Structure
  Input Parameter:
    nv      Starting Number of Values read from BUFR message
    
  Returns:
    int     Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
******************************************************************/      
{
  getDescInt(&tp->bd.year);
  getDescInt(&tp->bd.month);
  getDescInt(&tp->bd.day);
  getDescInt(&tp->bt.hour);
  getDescInt(&tp->bt.minute);
  getDescInt(&tp->ed.year);
  getDescInt(&tp->ed.month);
  getDescInt(&tp->ed.day);
  getDescInt(&tp->et.hour);
  getDescInt(&tp->et.minute);
  nv+=10;
  return nv;
}

int getGFAIdObsOrFcstLoc(GFAIdObsOrFcstLoc *giof, int nv)
/*****************************************************************
  getGFAIdObsOrFcstLoc (Get GFA Identifier and Observed/Fcst Location)
   (GFA Sequence 3-16-054)
  Output Parameter:
    giof   GFAIdObsOrFcstLoc *  GFA Identifer & Obs Fcst Loc Structure
  Input Parameter:
    nv     int                  Starting Number of Values read from the 
                                BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  getDescStr(giof->GFASeqId);
  getDescInt(&giof->timeSig);
  nv = getTimePeriod(&giof->tp, nv);
  nv = getDescOfFeature(&giof->df, nv);
  getDescInt(&giof->timeSigCnl);
  nv+=3;
  return nv;
}

int getSierraIFR(GFA_IFRCigAndVis_t *ifr, int prodStat, int dataSig, int nv)
/*****************************************************************
  getSierraIFR (Get Sierra IFR Information)
   (GFA Sequence 3-16-055)
  Output Parameter:
    ifr   GFA_IFRCigAndVis_t *  GFA IFR Cig & Vis Structure
  Input Parameters:
    prodStat int                Product Status (0=Norm, 1=COR, 2=AMD
                                                3=CORAMD)
    dataSig  int                Data Significance (8=IFR Cig&Vis)
    nv       int                Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  ifr->prodStat = prodStat;
  ifr->dataSig = dataSig;
  nv = getGFAIdObsOrFcstLoc(&ifr->giof, nv);
  getDescInt(&ifr->flightRules);
  getDescInt(&ifr->typeLimitCig);
  getDescInt(&ifr->cloudBase);
  if (ifr->cloudBase != (int) BUFR_MISSING_VALUE)
    ifr->cloudBase = (int) (ifr->cloudBase / 30.48 + 0.5);
  getDescInt(&ifr->typeLimitVis);
  getDescInt(&ifr->horVis);
  if (ifr->horVis != (int) BUFR_MISSING_VALUE)
    (ifr->horVis) /= 1609.3;
  getDescInt(&ifr->obsc);
  getDescInt(&ifr->charObsc);
  getDescInt(&ifr->dataSigCnl);
  getDescInt( &ifr->prodStatCnl);
  nv+=11;
  return nv;
}

int getSierraMtnObsc(GFA_MtnObsc_t *mtno, int prodStat, int dataSig, int nv)
/*****************************************************************
  getSierraMtnObsc (Get Mountain Obsc Information)
   (GFA Sequence 3-16-056)
  Output Parameter:
    ifr      GFA_MtnObsc_t *  GFA Mountain Obsc Structure
  Input Parameters:
    prodStat int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    dataSig  int             Data Significance (9=MtnObsc)
    nv       int             Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  mtno->prodStat=prodStat;
  mtno->dataSig=dataSig;
  nv=getGFAIdObsOrFcstLoc(&mtno->giof,nv);
  getDescInt(&mtno->flightRules);
  getDescInt(&mtno->obsc);
  getDescInt(&mtno->charObsc);
  getDescInt(&mtno->dataSigCnl);
  getDescInt(&mtno->prodStatCnl);
  nv+=7;
  return nv;
}

int getTangoTurb(GFA_Turbulence_t *turb, int prodStat, int metFeature, int nv)
/*****************************************************************
  getTangoTurb (Get Turbulence Information)
   (GFA Sequence 3-16-057)
  Output Parameter:
    turb      GFA_Turbulence_t *  GFA Turbulence Structure
  Input Parameters:
    prodStat   int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    metFeature int             Meteor. Feature (13=Turb)
    nv       int               Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  turb->prodStat = prodStat;
  turb->metFeature = metFeature;
  nv = getGFAIdObsOrFcstLoc(&turb->giof, nv);
  getDescInt(&turb->degOfTurb);
  getDescInt(&turb->metFeatureCnl);
  getDescInt(&turb->prodStatCnl);
  nv+=5;
  return nv;
}

int getTangoSSW(GFA_SSW_t *ssw, int prodStat, int dataSig, int nv)
/*****************************************************************
  getTangoSSW (Get Strong Surface Wind Information)
   (GFA Sequence 3-16-058)
  Output Parameter:
    ssw      GFA_SSW_t *  GFA Strong Surface Wind Structure
  Input Parameters:
    prodStat   int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    dataSig    int             Data Significane (10=Strong SFC Wind)
    nv         int               Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  ssw->prodStat = prodStat;
  ssw->dataSig = dataSig;
  nv = getGFAIdObsOrFcstLoc(&ssw->giof, nv);
  getDescInt(&ssw->typeLimitWndSpd);
  getDescInt(&ssw->windSpeed10m);
  ssw->windSpeed10m = (int)(ssw->windSpeed10m / 0.5144 +.5); /* (m/s -> kts) */
  getDescInt(&ssw->dataSigCnl);
  getDescInt(&ssw->prodStatCnl);
  nv+=6;
  return nv;
}

int getTangoLLWS(GFA_LLWS_t *llws, int prodStat, int metFeature, int nv)
/*****************************************************************
  getTangoLLWS (Get Low Level Wind Shear Information)
   (GFA Sequence 3-16-059)
  Output Parameter:
    llws      GFA_LLWS_t *  GFA Low Level Wind Shear Structure
  Input Parameters:
    prodStat   int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    metFeature int             Meteorol. Feature (16=Phenemenon)
    nv         int             Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  llws->prodStat = prodStat;
  llws->metFeature = metFeature;
  nv = getGFAIdObsOrFcstLoc(&llws->giof, nv);
  getDescInt(&llws->othWxPhen);
  getDescInt(&llws->intOthWxPhen);
  getDescInt(&llws->metFeatureCnl);
  getDescInt(&llws->prodStatCnl);
  nv+=8;
  return nv;
}

int getZuluIcing(GFA_Icing_t *icg, int prodStat, int metFeature, int nv)
/*****************************************************************
  getZuluIcing (Get Zulu Icing Information)
   (GFA Sequence 3-16-060)
  Output Parameter:
    icg      GFA_Icing_t *  GFA Icing Structure
  Input Parameters:
    prodStat   int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    metFeature int             Meteorol. Feature (15=AirFrame Icing)
    nv         int             Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  icg->prodStat = prodStat;
  icg->metFeature = metFeature;
  nv = getGFAIdObsOrFcstLoc(&icg->giof, nv);
  getDescInt(&icg->airframeICG);
  getDescInt(&icg->metFeatureCnl);
  getDescInt(&icg->prodStatCnl);
  nv+=5;
  return nv;
}

int getZuluFrzLvl(GFA_FreezingLvl_t *frzlvl, int prodStat, int dataSig,
                  int nv)
/*****************************************************************
  getZuluFrzLvl (Get Freezing Level Information)
   (GFA Sequence 3-16-061)
  Output Parameter:
    frzlvl      GFA_FreezingLvl_t *  GFA Freezing Level Structure
  Input Parameters:
    prodStat   int             Product Status (0=Norm, 1=COR, 2=AMD
                                                3=COR AMD)
    dataSig    int             Data Significance (11=Freezing Lvl)
    nv         int             Starting Number of Values read from the 
                                  BUFR msg
    
  Returns:
    int    Number of Values read from BUFR message
    
  Log:
  L. Hinson   10/07
*****************************************************************/
{
  frzlvl->prodStat = prodStat;
  frzlvl->dataSig = dataSig;
  nv = getGFAIdObsOrFcstLoc(&frzlvl->giof, nv);
  getDescInt(&frzlvl->dataSigCnl);
  getDescInt(&frzlvl->prodStatCnl);
  nv+=4;
  return nv;
}

GFAByDesignatorInfo *LoadRec(char* BUFR_File, enum GFADesignator *GFADesType,
int *num_vals, int debuglvl)
/*****************************************************************************
  LoadRec
  
  This routine is the main driving routine used to populate the structures defined
  in afbufr_structures.h
  
  Input Parameters:
    *BUFR_File      char           Name of BUFR file 
    *GFADesType     GFADesignator  Type of Message: SIERRA, TANGO, ZULU
    *num_vals	    int            Number of BUFR items decoded.
    debuglvl        int            Used for troubleshooting. Values greater than 0 will 			                            print out descriptor values.
  Output Parameters:
    Returns object of type GFAByDesignatorInfo *.   This is a structure containing the
    decoded items of the BUFR message.
    
  Log: L. Hinson    10/07
*****************************************************************************/
{
  GFAByDesignatorInfo *rec=0x0;
  static SIERRA_t sierra;
  static TANGO_t tango;
  static ZULU_t zulu;
  int descrip, prodStat, dataSig;
  BUFR_Info_t BInfo;
  int fxy_array_len;
  FXY_t* fxy_array;
  int nv=0;
  int metFeatureOrDataSig;
  g_debuglvl = debuglvl;
  rec = (GFAByDesignatorInfo *) malloc(sizeof(GFAByDesignatorInfo) * 1);
  /* Init BUFR message structure */
  if ( BUFR_Info_Init(&BInfo) ) {
    printf(">> Could not init BUFR info structure >>\n");
  }
  if (BUFR_Init( &BInfo, BUFR_File, DECODING )) {
    printf(">> Could not decode BUFR Message >>\n");
  }
  fxy_array_len=FXY_List_Get(&fxy_array);  /* Get Unexpanded Descriptors */
  if (fxy_array_len == 1) {
    descrip = FXY_Unpack_Dec(fxy_array[0]);
    switch (descrip) {
      case D_SIERRA:
        rec->td = SIERRA;
        rec->b.sierra = &sierra;
        nv = getTimePeriod(&sierra.tp,nv);
        sierra.repFactorIFR = sierra.repFactorMtnObsc = 0;
        while ((descrip=getDescInt(&prodStat))==D_ProdStat) {
          descrip=getDescInt(&dataSig);
          switch (dataSig) {
            case DS_IFRCigAndVis: /* 8 is IFR Ceiling & VIS */
              sierra.repFactorIFR++;
              if (sierra.repFactorIFR == 1) {
                G_MALLOC(sierra.IFR, GFA_IFRCigAndVis_t, sierra.repFactorIFR,
                        "BuildRec: sierra.IFR" );
              } else {
                G_REALLOC(sierra.IFR, GFA_IFRCigAndVis_t, sierra.repFactorIFR,
                          "BuildRec: sierra.IFR" );
              }

              nv = getSierraIFR(&sierra.IFR[sierra.repFactorIFR-1],
                                  prodStat, dataSig, nv);
              break;
            case DS_MtnObsc: /* 9 is Mountain Obscuration */
              sierra.repFactorMtnObsc++;
              if (sierra.repFactorMtnObsc == 1) {
                G_MALLOC ( sierra.MtnObsc, GFA_MtnObsc_t,
                           sierra.repFactorMtnObsc,
                          "BuildRec: sierra.MtnObsc" );
              } else {
                G_REALLOC (sierra.MtnObsc, GFA_MtnObsc_t,
                           sierra.repFactorMtnObsc,
                          "BuildRec: sierra.MtnObsc" );
              }
              nv = getSierraMtnObsc(&sierra.MtnObsc[sierra.repFactorMtnObsc-1],
                                    prodStat, dataSig, nv);
              break;
          }
        }
        nv+=2;
        break;
      case D_TANGO:
        rec->td = TANGO;
        rec->b.tango = &tango;
        nv = getTimePeriod(&tango.tp,nv);
        tango.repFactorTurb = tango.repFactorSSW = tango.repFactorLLWS = 0;
        while ((descrip=getDescInt(&prodStat))==D_ProdStat) {
          descrip=getDescInt(&metFeatureOrDataSig);
          switch (descrip) {
            case D_MetFeature:
              switch (metFeatureOrDataSig) {
                case MF_Turbulence:
                  tango.repFactorTurb++;
                  if (tango.repFactorTurb == 1) {
                    G_MALLOC ( tango.Turb, GFA_Turbulence_t,
                               tango.repFactorTurb,
                               "BuildRec: tango.Turb" );
                  } else {
                    G_REALLOC ( tango.Turb, GFA_Turbulence_t,
                                tango.repFactorTurb,
                                "BuildRec: tango.Turb" );
                  }
                  nv = getTangoTurb(&tango.Turb[tango.repFactorTurb-1],
                                     prodStat, metFeatureOrDataSig, nv);
                  break;
                case MF_Phenomena:
                  tango.repFactorLLWS++;
                  if (tango.repFactorLLWS == 1) {
                    G_MALLOC ( tango.LLWS, GFA_LLWS_t, tango.repFactorLLWS,
                               "BuildRec: tango.LLWS" );
                  } else {
                    G_REALLOC ( tango.LLWS, GFA_LLWS_t, tango.repFactorLLWS,
                                "BuildRec: tango.LLWS" );
                  }
                  nv=getTangoLLWS(&tango.LLWS[tango.repFactorLLWS-1],
                                   prodStat, metFeatureOrDataSig, nv);
                  break;
              }
              break;

            case D_DataSig:
              switch (metFeatureOrDataSig) {
                case DS_SSW:
                  tango.repFactorSSW++;
                  if (tango.repFactorSSW == 1) {
                    G_MALLOC ( tango.SSW, GFA_SSW_t, tango.repFactorSSW,
                               "BuildRec: tango.SSW" );
                  } else {
                    G_REALLOC ( tango.SSW, GFA_SSW_t, tango.repFactorSSW,
                                "BuildRec: tango.SSW" );
                  }
                  nv=getTangoSSW( &tango.SSW[tango.repFactorSSW-1],
                                   prodStat, metFeatureOrDataSig, nv);
                  break;
             }
             break;
          }
      }
      nv+=3;
      break;
    case D_ZULU:
      rec->td = ZULU;
      rec->b.zulu = &zulu;
      nv = getTimePeriod(&zulu.tp,nv);
      zulu.repFactorIcing = zulu.repFactorFrzLvl = 0;
      while ((descrip=getDescInt(&prodStat))==D_ProdStat) {
        descrip=getDescInt(&metFeatureOrDataSig);
        switch (descrip) {
          case D_MetFeature:
            zulu.repFactorIcing++;
            if (zulu.repFactorIcing == 1) {
              G_MALLOC ( zulu.Icing, GFA_Icing_t, zulu.repFactorIcing,
                         "BuildRec: zulu.Icing" );
            } else {
              G_REALLOC ( zulu.Icing, GFA_Icing_t, zulu.repFactorIcing,
                          "BuildRec: zulu.Icing" );
            }
            nv = getZuluIcing(&zulu.Icing[zulu.repFactorIcing-1],
                               prodStat, metFeatureOrDataSig, nv);
            break;
          case D_DataSig:
            zulu.repFactorFrzLvl++;
            if (zulu.repFactorFrzLvl == 1) {
              G_MALLOC ( zulu.FrzLvl, GFA_FreezingLvl_t, zulu.repFactorFrzLvl,
                         "BuildRec: zulu.FrzLvl" );
            } else {
              G_REALLOC ( zulu.FrzLvl, GFA_FreezingLvl_t, zulu.repFactorFrzLvl,
                          "BuildRec: zulu.FrzLvl" );
            }
            nv=getZuluFrzLvl(&zulu.FrzLvl[zulu.repFactorFrzLvl-1],
                              prodStat, metFeatureOrDataSig, nv);
            break;
        }
      }
    }
  } else {
    printf("Expecting descriptor count of 1... Got %d",fxy_array_len);
    exit (1);
  }
  *GFADesType=rec->td;  /* Set Designator */
  *num_vals=nv;
  free(fxy_array);
  BUFR_Destroy(1);
  return(rec);
}

int getDescInt(int *value)
/****************************************************************************
   getDescInt
   
   This function gets the next BUFR value via BUFR_Get_Value and returns its 
   associated descriptor.  For missing values, *value is set to -9999  .
   If EOM, EOF, or other error on BUFR message, -1 is returned.
   Error Messages to STDOUT:  Missing Values,  Value not of Type Int.  
   If type is float, will set *value to the  (int) value of the float.
   
   Output Parameter:
      int *value    Next BUFR value in integer format.
   
   Returns int   Status indicator of -1 if EOM, EOF set; Descriptor otherwise.
   
   Log: L. Hinson  10/07
***************************************************************************/
{
  BUFR_Val_t bv;
  *value=0;
  /*  while (( n = (int) BUFR_Get_Value( &bv, 0)) == 0) { */
  while ( BUFR_Get_Value(&bv, 0) == BUFR_OK) {
    if (bv.FXY_Val != (FXY_t)IGNORE_FXY) {
      if (bv.Val_Type == DT_INT) {
        *value = bv.Val.int_number;
        if (g_debuglvl > 0)
          printf("Int Descriptor is %s Value = %d Hex =%X\n",
               FXY_String(bv.FXY_Val),*value, *value);
        if (FXY_IsTableB(bv.FXY_Val))
          if (bv.missing_flag == 0)
            if (FXY_UnitsType(bv.FXY_Val) != CODE_TABLE) {
              if (g_debuglvl > 0)
                printf("This is a MISSING VALUE\n");
              *value = (int) BUFR_MISSING_VALUE;
            }
      } else {
        if (g_debuglvl > 0) {
          printf("bv Val_Type is not of type Int\n");
          printf("Descriptor is %s\n",FXY_String(bv.FXY_Val));
        }
        if (bv.Val_Type == DT_DOUBLE) {
          *value = (int) bv.Val.number;
          if (g_debuglvl > 0)
            printf("Float Descriptor is %s Value =%d\n",
               FXY_String(bv.FXY_Val),*value);
        }
      }
      return(FXY_Unpack_Dec(bv.FXY_Val));
    }
  }
  return (-1);
}

int getDescStr(char *string)
/****************************************************************************
   getDescStr
   
   This function gets the next BUFR value via BUFR_Get_Value and returns its 
   associated descriptor.  For missing values, *value is set to NULL.
   If EOM, EOF, or other error on BUFR message, -1 is returned.
   Removes white space on right side of string.
   Error Messages to STDOUT: Value not of Type String.  String set to NULL.
   
   Output Parameter:
      char *string    Next BUFR value of type string.
   
   Returns int   Status indicator of -1 if EOM, EOF set; Descriptor otherwise.
   
   Log: L. Hinson  10/07
***************************************************************************/
{
  BUFR_Val_t bv;
  char *ptr;
  string[0] = '\0';
  while (BUFR_Get_Value( &bv, 0) == BUFR_OK) {
    if (bv.FXY_Val != (FXY_t)IGNORE_FXY) {
      if (bv.Val_Type == DT_STRING) {
        strcpy(string, bv.Val.string);
        /* trim white space off right side */
        ptr=string+strlen(string)-1;
        while (isspace(*ptr)) *ptr-- = '\0';
        if (g_debuglvl > 0)
          printf("String Descriptor is %s Value =%s\n",
               FXY_String(bv.FXY_Val),string);
      } else {
        if (g_debuglvl > 0) {
          printf("bv Val_Type is not of type String\n");
          printf("Descriptor is %s\n",FXY_String(bv.FXY_Val));
        }
      }
      return(FXY_Unpack_Dec(bv.FXY_Val));
    }
  }
  return (-1);
}

int getDescFloat(float *value)
/****************************************************************************
   getDescFloat
   
   This function gets the next BUFR value via BUFR_Get_Value and returns its 
   associated descriptor.  For missing values, *value is set to -9999. (float).
   If EOM, EOF, or other error on BUFR message, -1 is returned.
   Error Messages to STDOUT:  Missing Values,  Value not of Type Float.  
   If type is int, will set *value to the  (float) value of the int.
   
   Output Parameter:
      int *value    Next BUFR value in float format.
   
   Returns int   Status indicator of -1 if EOM, EOF set; Descriptor otherwise.
   
   Log: L. Hinson  10/07
***************************************************************************/
{
  BUFR_Val_t bv;
  *value=0.0;
  while (BUFR_Get_Value( &bv, 0) == BUFR_OK) {
    if (bv.FXY_Val != (FXY_t)IGNORE_FXY) {
      if (bv.Val_Type == DT_DOUBLE) {
        *value= bv.Val.number;
        if (g_debuglvl > 0)
          printf("Float Descriptor is %s Value =%f\n",
               FXY_String(bv.FXY_Val),*value);
      } else {
        if (g_debuglvl > 0) {
          printf("bv Val_Type is not of type Float\n");
          printf("Descriptor is %s\n",FXY_String(bv.FXY_Val));
        }
        if (bv.Val_Type == DT_INT) {
          *value = (float) bv.Val.int_number;
          if (g_debuglvl > 0)
            printf("Is Integer Descriptor Value=%f\n",*value);
          if (FXY_IsTableB(bv.FXY_Val))
            if (bv.missing_flag == 0)
              if (FXY_UnitsType(bv.FXY_Val) != CODE_TABLE) {
                if (g_debuglvl > 0)
                  printf("This is a MISSING VALUE\n");
                *value = BUFR_MISSING_VALUE;
              }
        }
      }
      return(FXY_Unpack_Dec(bv.FXY_Val));
    }
  }
  return (-1);
}

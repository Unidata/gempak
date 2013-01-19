
#ifndef GDGRIB2_H
#define GDGRIB2_H

#include "geminc.h"
#include "gemprm.h"


/*-------------------   Data structures  ------------------*/

/*
 *  gdg2_input struct contains all user input info.
 */
struct gdg2_input {
    char gdfile[LLMXLN];          /*  input GEMPAK file name                 */
    char g2file[LLMXLN];          /*  output GRIB2 file name                 */
    char gfunc[LLMXLN];           /*  Scalar grid                            */
    char gdattim[LLMXLN];         /*  Grid date/time                         */
    char glevel[LLMXLN];          /*  Grid level                             */
    char gvcord[LLMXLN];          /*  Grid vertical coordinate               */
    char grdtyp[LLMXLN];          /*  Diagnostic grid type (Scalar/Vector)   */
    char proj[LLMXLN];            /*  map projection information             */
    char grdarea[LLMXLN];         /*  grid area                              */
    char kxky[LLMXLN];            /*  size of grid                           */
    char cpyfil[LLMXLN];          /*  grid navigation for gempak file       **/
    char g2tbls[LLMXLN];          /*  list of GRIB2 tables                  **/
    char tables[5][LLMXLN];       /*  List of tables (from g2tbls)          **/
    char *tbllist[5];             /*  list of pointers to tables            **/
    char g2is[LLMXLN];            /*  Info for GRIB2 Section 0               */
    char g2ids[LLMXLN];           /*  Info for GRIB2 Section 1               */
    char g2pdt[LLMXLN];           /*  Info for GRIB2 PDT - Section 4         */
    char g2drt[LLMXLN];           /*  Info for GRIB2 DRT - Sections 5, 7     */
    char wmohdr[LLMXLN];          /*  WMO header info                        */
};
typedef struct gdg2_input GDG2_input;


struct gdg2_gemgrid {
    int    kx;                    /*  x dimension of output grid             */
    int    ky;                    /*  y dimension of output grid             */
    char   ctime[2][20];          /*  GEMPAK date/time                       */
    int    level[2];              /*  grid level values                      */
    int    vcord;                 /*  vertical coordinate                    */
    char   param[12];             /*  parameter name                         */
    float  navblk[LLNNAV];        /*  Grid navigation block                  */
    float  *grid;                 /*  Scalar grid                            */
};
typedef struct gdg2_gemgrid GDG2_gemgrid;


/*-------------------   Prototypes  ------------------*/
void gdg2in( GDG2_input *, int *);

void gdgetgrid ( GDG2_input *, GDG2_gemgrid *, int *);

void gdmakeg2 ( GDG2_input *, GDG2_gemgrid *, unsigned char **, int *, int *);

void gdsetnav ( GDG2_input *, float *, int *);

void gdmakepdt ( GDG2_input *, GDG2_gemgrid *, int *,
                 int *, int *, int *, int *);

void gdmakewmo ( GDG2_input *, GDG2_gemgrid *, char *, int *);

void gdsetsect1 ( GDG2_input *, GDG2_gemgrid *, int *, int *);

void gdsetsect4 ( GDG2_input *, GDG2_gemgrid *, int *, int *,
                  int *, int *, int *);

void gdsetsect5 ( GDG2_input *, int *, int *, int *);

#endif   /*  GDGRIB2_H  */

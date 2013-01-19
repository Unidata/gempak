
#ifndef NAGRIB2_H
#define NAGRIB2_H

#include "geminc.h"
#include "gemprm.h"


/*-------------------   Data structures  ------------------*/

/*
 *  g2_input struct contains all user input info.
 */
struct g2_input {
    char g2file[LLMXLN];          /*  input GRIB2 file name                  */
    char gdoutf[LLMXLN];          /*  output GEMPAK file name                */
    int  lstflag;                 /*  Listing only flag                      */
    char proj[LLMXLN];            /*  map projection information             */
    char grdarea[LLMXLN];         /*  grid area                              */
    char kxky[LLMXLN];            /*  size of grid                           */
    char maxgrd[LLMXLN];          /*  max number of grids                   **/
    char cpyfil[LLMXLN];          /*  grid navigation for gempak file       **/
    char garea[LLMXLN];           /*  graphics area                          */
    char output[LLMXLN];          /*  output devices                         */
    char g2tbls[LLMXLN];          /*  list of GRIB2 tables                  **/
    char g2diag[LLMXLN];          /*  GRIB2 sections to dump                **/
    int maxgrid;                  /*  integer version of maxgrd[]            */
    char tables[5][LLMXLN];       /*  List of tables (from g2tbls)          **/
    char *tbllist[5];             /*  list of pointers to tables            **/
    int  g2dglst[MMHDRS];         /*  List of messages to dump diag info     */
    int  overwr;                  /*  Overwrite flag                         */
    int  pdsext;                  /*  Append Ensemble info to parameter name */
};
typedef struct g2_input G2_input;


/*-------------------   Prototypes  ------------------*/
void nag2in( G2_input *, int *);
void  nadiag ( char *, int *, int );

#endif   /*  NAGRIB2_H  */

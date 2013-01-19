/***********************************************************************
**
**  Project.  Master Environmental Library (MEL)
**
**  Filename.  fill_array.c
**
**  Description.  Fills a data structure
**
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fill_array.h"

#if PROTOTYPE_NEEDED
void fill_array(Data_MixVal_t *rec)
#else
void fill_array(rec)
Data_MixVal_t *rec;
#endif
{
    rec[0].Val.ffloat = 1997.;
    rec[0].Val_Type = DT_FLOAT;

    rec[1].Val.ffloat = 8.;
    rec[1].Val_Type = DT_FLOAT;

    rec[2].Val.ffloat = 6.;
    rec[2].Val_Type = DT_FLOAT;

    rec[3].Val.ffloat = 6.;
    rec[3].Val_Type = DT_FLOAT;

    rec[4].Val.ffloat = 32.;
    rec[4].Val_Type = DT_FLOAT;
    
    rec[5].Val.ffloat = 34.4;
    rec[5].Val_Type = DT_FLOAT;

    rec[6].Val.ffloat = 124.4;
    rec[6].Val_Type = DT_FLOAT;

    rec[7].Val.ffloat = 1200.;
    rec[7].Val_Type = DT_FLOAT;

    rec[8].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[8].Val.string = "FNMOCMTR";
    rec[8].Val_Type = DT_STRING;
    
} /* end fill_array() */

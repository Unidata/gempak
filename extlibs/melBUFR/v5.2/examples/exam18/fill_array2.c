/***********************************************************************
**
**  Project.  Master Environmental Library (MEL)
**
**  Filename.  ds2io.c
**
**  Description.
**
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fill_array2.h"

#if PROTOTYPE_NEEDED
void fill_array2(Data_MixVal_t *rec, int num)
#else
void fill_array2(rec, num)
Data_MixVal_t *rec;
int num;
#endif
{
    int i;
    float num_data[3][8];

    num_data[0][0] = 1997.;
    num_data[1][0] = 1997.;
    num_data[2][0] = 1997.;

    num_data[0][1] = 8.;
    num_data[1][1] = 9.;
    num_data[2][1] = 9.;

    num_data[0][2] = 6.;
    num_data[1][2] = 1.;
    num_data[2][2] = 5.;

    num_data[0][3] = 6.;
    num_data[1][3] = 12.;
    num_data[2][3] = 14.;

    num_data[0][4] = 32.;
    num_data[1][4] = 23.;
    num_data[2][4] = 5.;

    num_data[0][5] = 34.4;
    num_data[1][5] = 31.0;
    num_data[2][5] = 37.2;

    num_data[0][6] = 124.4;
    num_data[1][6] = 151.3;
    num_data[2][6] = 138.1;

    num_data[0][7] = 1200.;
    num_data[1][7] = 500.;
    num_data[2][7] = 1400.;

    for(i = 0; i < 8; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

    rec[8].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[8].Val_Type = DT_STRING;

    if(num == 0){
      rec[8].Val.string = "FNMOCMTR";
    } else if(num == 1){
      rec[8].Val.string = "NRLMRY";
    } else 
    {
      rec[8].Val.string = "NEPRPH";
    }
    
} /* end fill_array2() */

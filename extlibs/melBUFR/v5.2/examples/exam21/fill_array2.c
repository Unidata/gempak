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
    float num_data[4][7];

    num_data[0][0] = 35.5;
    num_data[1][0] = 34.4;
    num_data[2][0] = 37.2;
    num_data[3][0] = 32.2;
 
    num_data[0][1] = 120.3;
    num_data[1][1] = 151.3;
    num_data[2][1] = 138.1;
    num_data[3][1] = 142.1;

    num_data[0][2] = 1996.;
    num_data[1][2] = 1997.;
    num_data[2][2] = 1997.;
    num_data[3][2] = 1998.;
 
    num_data[0][3] = 4.;
    num_data[1][3] = 8.;
    num_data[2][3] = 9.;
    num_data[3][3] = 7.;
 
    num_data[0][4] = 2.;
    num_data[1][4] = 1.;
    num_data[2][4] = 5.;
    num_data[3][4] = 9.;

    num_data[0][5] = 13.;
    num_data[1][5] = 12.;
    num_data[2][5] = 14.;
    num_data[3][5] = 7.;

    num_data[0][6] = 5.;
    num_data[1][6] = 23.;
    num_data[2][6] = 32.;
    num_data[3][6] = 12.;

    for(i = 0; i < 7; i++){
      rec[i].Val.ffloat = num_data[num][i];
      rec[i].Val_Type = DT_FLOAT;
      printf(">>> %f  %f\n",rec[i].Val.ffloat, num_data[num][i]);
    }

    rec[7].Val.string = (char *) malloc(sizeof(char) * 9);
    memset( rec[7].Val.string, 0, (uint_t) (9) );
    rec[7].Val_Type = DT_STRING;
    if(num == 0){
      rec[7].Val.string = "FNMOCMTR";
    } else if(num == 1){
      rec[7].Val.string = "CAMROON";
    } else if(num == 2){
      rec[7].Val.string = "DIEGOG";
    } else
    {
      rec[7].Val.string = "INDIGO";
    }

} /* end fill_array2() */

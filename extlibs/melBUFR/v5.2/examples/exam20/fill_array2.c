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
    float num_data[3][23];

    num_data[0][0] = 35.5;
    num_data[1][0] = 34.4;
    num_data[2][0] = 37.2;
 
    num_data[0][1] = 120.3;
    num_data[1][1] = 151.3;
    num_data[2][1] = 138.1;

    num_data[0][2] = 1996.;
    num_data[1][2] = 1997.;
    num_data[2][2] = 1997.;
 
    num_data[0][3] = 4.;
    num_data[1][3] = 8.;
    num_data[2][3] = 9.;
 
    num_data[0][4] = 2.;
    num_data[1][4] = 1.;
    num_data[2][4] = 5.;

    num_data[0][5] = 13.;
    num_data[1][5] = 12.;
    num_data[2][5] = 14.;

    num_data[0][6] = 5.;
    num_data[1][6] = 23.;
    num_data[2][6] = 32.;

    num_data[0][7] = 3.;
    num_data[1][7] = 3.;
    num_data[2][7] = 3.;

    num_data[0][8] = 20.;
    num_data[1][8] = 500.;
    num_data[2][8] = 140.;
 
    num_data[0][9] = 295.2;
    num_data[1][9] = 287.8;
    num_data[2][9] = 291.6;
 
    num_data[0][10] = 180.;
    num_data[1][10] = 173.;
    num_data[2][10] = 197.;
 
    num_data[0][12] = 10.;
    num_data[1][12] = 14.;
    num_data[2][12] = 19.;
 
    num_data[0][13] = 100.;
    num_data[1][13] = 250.;
    num_data[2][13] = 170.;
 
    num_data[0][14] = 293.5;
    num_data[1][14] = 285.3;
    num_data[2][14] = 293.9;
 
    num_data[0][15] = 185.;
    num_data[1][15] = 171.;
    num_data[2][15] = 196.;
 
    num_data[0][17] = 12.;
    num_data[1][17] = 11.;
    num_data[2][17] = 17.;
 
    num_data[0][18] = 1000.;
    num_data[1][18] = 700.;
    num_data[2][18] = 670.;
 
    num_data[0][19] = 290.1;
    num_data[1][19] = 287.5;
    num_data[2][19] = 293.2;
 
    num_data[0][20] = 190.;
    num_data[1][20] = 174.;
    num_data[2][20] = 192.;
 
    num_data[0][22] = 15.;
    num_data[1][22] = 14.;
    num_data[2][22] = 18.;
 
    for(i = 0; i < 11; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

    for(i = 12; i < 16; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

    for(i = 17; i < 21; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }
      rec[22].Val.ffloat = num_data[num][22];;
      rec[22].Val_Type = DT_FLOAT;

    rec[11].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[11].Val_Type = DT_STRING;
    if(num == 0){
      rec[11].Val.string = "FNMOCMTR";
    } else if(num == 1){
      rec[11].Val.string = "CAMROON";
    } else
    {
      rec[11].Val.string = "INDIGO";
    }

    rec[16].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[16].Val_Type = DT_STRING;
    if(num == 0){
      rec[16].Val.string = "NRLMRY";
    } else if(num == 1){
      rec[16].Val.string = "D.GARCIA";
    } else
    {
      rec[16].Val.string = "BISCAY.B";
    }

    rec[21].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[21].Val_Type = DT_STRING;
    if(num == 0){
      rec[21].Val.string = "NEPRPH";   
    } else if(num == 1){
      rec[21].Val.string = "AUKLAND";
    } else
    {
      rec[21].Val.string = "SANJUAN";
    }
} /* end fill_array2() */

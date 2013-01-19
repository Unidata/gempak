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
    float num_data[3][21];

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

    num_data[0][7] = 20.;
    num_data[1][7] = 500.;
    num_data[2][7] = 140.;
 
    num_data[0][8] = 295.2;
    num_data[1][8] = 287.8;
    num_data[2][8] = 291.6;
 
    num_data[0][9] = 180.;
    num_data[1][9] = 173.;
    num_data[2][9] = 197.;
 
    num_data[0][11] = 10.;
    num_data[1][11] = 14.;
    num_data[2][11] = 19.;
 
    num_data[0][12] = 100.;
    num_data[1][12] = 250.;
    num_data[2][12] = 170.;
 
    num_data[0][13] = 293.5;
    num_data[1][13] = 285.3;
    num_data[2][13] = 293.9;
 
    num_data[0][14] = 185.;
    num_data[1][14] = 171.;
    num_data[2][14] = 196.;
 
    num_data[0][16] = 12.;
    num_data[1][16] = 11.;
    num_data[2][16] = 17.;
 
    num_data[0][17] = 1000.;
    num_data[1][17] = 700.;
    num_data[2][17] = 670.;
 
    num_data[0][18] = 290.1;
    num_data[1][18] = 287.5;
    num_data[2][18] = 293.2;
 
    num_data[0][19] = 190.;
    num_data[1][19] = 174.;
    num_data[2][19] = 192.;
 
    num_data[0][21] = 15.;
    num_data[1][21] = 14.;
    num_data[2][21] = 18.;
 
    for(i = 0; i < 10; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

    for(i = 11; i < 15; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

    for(i = 16; i < 20; i++){
      rec[i].Val.ffloat = num_data[num][i];;
      rec[i].Val_Type = DT_FLOAT;
    }

   rec[21].Val.ffloat = num_data[num][21];
   rec[21].Val_Type = DT_FLOAT;

    rec[10].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[10].Val_Type = DT_STRING;
    if(num == 0){
      rec[10].Val.string = "FNMOCMTR";
    } else if(num == 1){
      rec[10].Val.string = "CAMROON";
    } else
    {
      rec[10].Val.string = "INDIGO";
    }

    rec[15].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[15].Val_Type = DT_STRING;
    if(num == 0){
      rec[15].Val.string = "NRLMRY";
    } else if(num == 1){
      rec[15].Val.string = "D.GARCIA";
    } else
    {
      rec[15].Val.string = "BISCAY.B";
    }

    rec[20].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[20].Val_Type = DT_STRING;
    if(num == 0){
      rec[20].Val.string = "NEPRPH";   
    } else if(num == 1){
      rec[20].Val.string = "AUKLAND";
    } else
    {
      rec[20].Val.string = "SANJUAN";
    }
} /* end fill_array2() */

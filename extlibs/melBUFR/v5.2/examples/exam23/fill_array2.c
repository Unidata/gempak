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
    float num_data[4][21];

    num_data[0][0] = 35.5;
    num_data[1][0] = 34.4;
    num_data[2][0] = 37.2;
    num_data[3][0] = 36.1;
 
    num_data[0][1] = 120.3;
    num_data[1][1] = 151.3;
    num_data[2][1] = 138.1;
    num_data[3][1] = 144.7;

    num_data[0][2] = 1996.;
    num_data[1][2] = 1997.;
    num_data[2][2] = 1997.;
    num_data[3][2] = 1998.;
 
    num_data[0][3] = 4.;
    num_data[1][3] = 8.;
    num_data[2][3] = 9.;
    num_data[3][3] = 6.;
 
    num_data[0][4] = 2.;
    num_data[1][4] = 1.;
    num_data[2][4] = 5.;
    num_data[3][4] = 22.;

    num_data[0][5] = 13.;
    num_data[1][5] = 12.;
    num_data[2][5] = 14.;
    num_data[3][5] = 9.;

    num_data[0][6] = 5.;
    num_data[1][6] = 23.;
    num_data[2][6] = 32.;
    num_data[3][6] = 27.;

    num_data[0][7] = 20.;
    num_data[1][7] = 500.;
    num_data[2][7] = 140.;
    num_data[3][7] = 320.;
 
    num_data[0][8] = 295.2;
    num_data[1][8] = 287.8;
    num_data[2][8] = 291.6;
    num_data[3][8] = 281.5;
 
    num_data[0][9] = 180.;
    num_data[1][9] = 173.;
    num_data[2][9] = 197.;
    num_data[3][9] = 195.;
 
    num_data[0][10] = 10.;
    num_data[1][10] = 14.;
    num_data[2][10] = 19.;
    num_data[3][10] = 16.;
 
    num_data[0][12] = 100.;
    num_data[1][12] = 250.;
    num_data[2][12] = 170.;
    num_data[3][12] = 214.;
 
    num_data[0][13] = 293.5;
    num_data[1][13] = 285.3;
    num_data[2][13] = 293.9;
    num_data[3][13] = 291.1;
 
    num_data[0][14] = 185.;
    num_data[1][14] = 171.;
    num_data[2][14] = 196.;
    num_data[3][14] = 174.;
 
    num_data[0][15] = 12.;
    num_data[1][15] = 11.;
    num_data[2][15] = 17.;
    num_data[3][15] = 14.;
 
    num_data[0][17] = 1015.5;
    num_data[1][17] = 715.5;
    num_data[2][17] = 675.5;
    num_data[3][17] = 895.5;
 
    num_data[0][18] = 290.15;
    num_data[1][18] = 287.55;
    num_data[2][18] = 293.25;
    num_data[3][18] = 289.85;
 
    num_data[0][19] = 190.;
    num_data[1][19] = 174.;
    num_data[2][19] = 192.;
    num_data[3][19] = 187.;
 
    num_data[0][20] = 15.;
    num_data[1][20] = 14.;
    num_data[2][20] = 18.;
    num_data[3][20] = 17.;
 
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

    rec[11].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[11].Val_Type = DT_STRING;
    if(num == 0){
      rec[11].Val.string = "FNMOCMTR";
    } else if(num == 1){
      rec[11].Val.string = "CAMROON";
    } else if(num == 2){
      rec[11].Val.string = "KEYWEST";
    } else {
      rec[11].Val.string = "INDIGO";
    }

    rec[16].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[16].Val_Type = DT_STRING;
    if(num == 0){
      rec[16].Val.string = "NRLMRY";
    } else if(num == 1){
      rec[16].Val.string = "D.GARCIA";
    } else if(num == 2){
      rec[16].Val.string = "ATLANTA";
    } else {
      rec[16].Val.string = "BISCAY.B";
    }

    rec[21].Val.string = (char *) malloc(sizeof(char) * 9);
    rec[21].Val_Type = DT_STRING;
    if(num == 0){
      rec[21].Val.string = "NEPRPH";   
    } else if(num == 1){
      rec[21].Val.string = "AUKLAND";
    } else if(num == 2){
      rec[21].Val.string = "VENICE";
    } else {
      rec[21].Val.string = "SANJUAN";
    }
} /* end fill_array2() */

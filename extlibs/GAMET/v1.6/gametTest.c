/*-------------------------------------------------------------------------
*
* Open Source License/Disclaimer, Forecast Systems Laboratory NOAA/OAR/FSL,
* 325 Broadway Boulder, CO 80305
*
* This software is distributed under the Open Source Definition, which may
* be found at http://www.opensource.org/osd.html.
*
* In particular, redistribution and use in source and binary forms, with or
* without modification, are permitted provided that the following conditions
* are met:
*
*     . Redistributions of source code must retain this notice, this list
*     of conditions and the following disclaimer.
*
*     . Redistributions in binary form must provide access to this notice,
*     this list of conditions and the following disclaimer, and the
*     underlying source code.
*
*     . All modifications to this software must be clearly documented, and
*     are solely the responsibility of the agent making the modifications.
*
*     . If significant modifications or enhancements are made to this
*     software, the FSL Software Policy Manager (softwaremgr.fsl@noaa.gov)
*     should be notified.
*
*-------------------------------------------------------------------------
*
* THIS SOFTWARE AND ITS DOCUMENTATION ARE IN THE PUBLIC DOMAIN AND ARE
* FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
* INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
* EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND DOCUMENTATION
* FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR THE USE OF THE
* SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.
*
*-------------------------------------------------------------------------
*
* gametTest.c
*
* Version   1.1   12/22/03  Leslie Ewy   Original Version
* Version   1.2   01/13/04  Leslie Ewy   Incorporate NCEP corrections
* Version   1.3   01/21/04  Leslie Ewy   Modifications for reading area
*                                        states and gamet states
* Version   1.4   02/17/04  Leslie Ewy   Incorporate Scott Jacobs comments
*
* Verstion  1.5   04/20/04  Leslie Ewy   Additions for area movement, and
*                                        Sigmet applicable
*-------------------------------------------------------------------------*/
/*
 * Include files
 */
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#ifdef AIX
#include <strings.h>
#endif
#include "gamet.h"
#include "gametTest.h"

/*************************************************************************
Contains the main routine to build a gamet text creator.
This simplistic driver is written to test the gamet text creator library.
It is by no means fool proof. It was not written to test itself.

It reads input from a file. The file is designed so that the information
in the file can be read and inserted into a GametStruct structure.
The file is created to be read in sections, using the following
rules.
# Begins a comment line
$ Starts the Gamet
% Starts an element type (may be > 1 in a gamet)
! Starts an area (may be > 1 in an element type)
!! Ends an area
%% Ends an element type
$$ Ends the gamet
to execute the test driver:
	gametTest <testFile>

*************************************************************************/

/************************************************************************
main function opens an input file and passes it onto fillGametStruct
  to fill the structure with information contained in the file.
  calls create_gamet_product with the filled structure
  Checks errors string for creation problems
************************************************************************/

int main (int argc, char* argv[])
{
    GametStruct *gamet;
    FILE *fp;
    char product[4096];
    char errors [512];

    if (argc != 2) {
        printf ("Need an input file\n");
        exit (1);
    }
    if ((fp = fopen(argv[1], "r")) == NULL) {
        printf ("Unable to open Input File %s\n", argv[1]);
        exit (1);
    }

    gamet = (GametStruct *)malloc (sizeof(GametStruct));
    if (gamet == (GametStruct *) NULL)
        exit (1);
    if (!fillGametStruct(fp, gamet)) {
        printf("Unable to fill Gamet Structure \n");
        exit (1);
    }
    create_gamet_product (gamet, product, errors);
    printf ("%s\n", product);
    if (strlen (errors)) {
        printf("ERRORS :\n%s\n", errors);
    }
    exit (0);
}

/****************************************************************************
fillGametStruct
  Fills a GametStruct with information it finds in the file.
WARNINIG: this is very crude, and yes it can be tricked easily,

Input Parameter: *fp        FILE
Output Parameter: *gamet    GametStruct
Functions Called: initGamet
                  typeStrToEnum
Returns: TRUE if able to fill GametStruct
***************************************************************************/
int fillGametStruct (FILE *fp, GametStruct *g)
{
    testState state = no_state;
    int gLines, eleLines, areaLines;
    int numEle, numArea;
    char temp[256];
    if(!initGamet(g)){
        printf("Unable to init Gamet Structure \n");
        return FALSE;
    }
    gLines = eleLines = areaLines = 0;
    numEle = numArea = 0;
    while ((fgets(temp, 256, fp)) != NULL) {
        if (temp[0] == '#') {
            continue;
        }
        else if (!strncmp (temp, "$$",2)) {
            state = no_state;
            continue;
        }
        else if (temp[0] == '$') {
            state =  gamet_info;
            continue;
        }
        else if (!strncmp (temp, "%%", 2)) {     /* End Element */
            state = no_state;
            numEle++;
            numArea = 0;
            eleLines = 0;
            continue;
        }
        else if (temp[0] == '%'){
            state = element_info;
            continue;
        }
        else if (!strncmp (temp, "!!", 2)) {     /* End Area */
            state = no_state;
            numArea++;
            areaLines = 0;
            continue;
        }
        else if (temp[0] =='!'){
            state = area_info;
            continue;
        }
        temp[strlen(temp)-1] = '\0';
        switch (state) {
            case gamet_info:
                /* printf ("GAMET INFO: %s", temp); */

                if (gLines == 0) {
                    g->atsunit = (char *) malloc (strlen (temp) +1);
                        if (g->atsunit == NULL)
                            return FALSE;
                    strcpy (g->atsunit, temp);
                }
                else if (gLines == 1) {
                    g->originator = (char *) malloc (strlen (temp) +1);
                        if (g->originator == NULL)
                            return FALSE;
                    strcpy (g->originator, temp);
                }
                else if (gLines == 2) {
                    g->fir = (char *) malloc (strlen (temp) +1);
                        if (g->fir == NULL)
                            return FALSE;
                    strcpy (g->fir, temp);
                }
                else if (gLines == 3)
                    g->issuanceDay = atoi (temp);
                else if (gLines == 4)
                    g->issuanceHour = atoi(temp);
                else if (gLines == 5) {
                    g->allStates = (char *)malloc (strlen(temp) + 1);
                    if (g->allStates == NULL)
                        return FALSE;
                    strcpy(g->allStates, temp);
                }
                else if (gLines == 6)
                    g->numElements = atoi(temp);
                else if (gLines == 7) {
                    g->sigApp = (char *) malloc (strlen (temp) +1);
                        if (g->sigApp == NULL)
                            return FALSE;
                    strcpy (g->sigApp, temp);
                }
                gLines++;
              
                break;
            case element_info:
                /* printf ("ELEMENT INFO: %s", temp); */
                if (eleLines == 0) {
                    g->element[numEle].type = typeStrToEnum (temp);
                }
                else if (eleLines == 1) {
                    g->element[numEle].numAreas = atoi(temp);
                }
                eleLines++;
               break;
            case area_info:
                /* printf("AREA INFO: %s", temp); */
                if (areaLines == 0) {
                    g->element[numEle].area[numArea].vorString = 
                      (char *)malloc (strlen (temp) +1);
                    if (g->element[numEle].area[numArea].vorString == NULL)
                        return FALSE;
                    strcpy(g->element[numEle].area[numArea].vorString, temp);
                }
                if (areaLines == 1) {
                    g->element[numEle].area[numArea].attr = 
                      (char *)malloc (strlen(temp) +1);
                    if (g->element[numEle].area[numArea].attr == NULL)
                        return FALSE;
                    strcpy (g->element[numEle].area[numArea].attr, temp);
                }
                  
                if (areaLines == 2) {
                    sscanf(temp, "%d %d %d", 
                      &(g->element[numEle].area[numArea].snapshot[0]),
                      &(g->element[numEle].area[numArea].snapshot[1]),
                      &(g->element[numEle].area[numArea].snapshot[2]));
                }
                if (areaLines == 3) {
                   g->element[numEle].area[numArea].states = 
                     (char *)malloc (strlen(temp) + 1);
                   if (g->element[numEle].area[numArea].states == NULL)
                       return FALSE;
                   strcpy (g->element[numEle].area[numArea].states, temp);
                }
                if (areaLines == 4) {
                    g->element[numEle].area[numArea].movement =
                      (char *) malloc (strlen(temp) + 1);
                    if (g->element[numEle].area[numArea].movement == NULL)
                        return FALSE;
                    strcpy (g->element[numEle].area[numArea].movement, temp);
                }
                areaLines++;
                   
                break;
            default:
                break;
        }
    }
    return TRUE;
}
/**********************************************************************
initGamet (GametStruct *gamet) initializes structure values to 0
Input/OutPut Parameter GametStruct initialized
Returns TRUE;
**********************************************************************/
int initGamet (GametStruct *gamet)
{
    int i, j, k;
    gamet->fir = '\0';
    gamet->numElements = 0;
    gamet->sigApp = '\0';
    for (i = 0; i < MAX_ELEMENTS; i++) {
        gamet->element[i].type = no_element_type;
        gamet->element[i].numAreas = 0;
        for (j = 0; j < MAX_AREAS; j++) {
            gamet->element[i].area[j].vorString = '\0';
            gamet->element[i].area[j].attr = '\0';
            for (k = 0; k < 3; k++) 
                gamet->element[i].area[j].snapshot[k] = -1;
            }
            gamet->element[i].area[j].movement = '\0';
        }
    return TRUE; 
}


/*************************************************************************
typeStrToEnum (char *s) takes the string read from the file and converts
 it to appropriate enum
Input Parameter:   s*       char
Returns:           enum value for string
************************************************************************/
int typeStrToEnum (char * s)
{

    if (!strncasecmp (s, "SFC WSPD", 8))
        return sfc_wspd;
    if (!strncasecmp (s, "SFC VIS", 7))
        return sfc_vis;
    if (!strncasecmp (s, "SIG WX", 6))
        return sig_wx;
    if (!strncasecmp (s, "MT OBSCN", 8))
        return mt_obscn;
    if (!strncasecmp (s, "SIG CLD", 7))
        return sig_cld;
    if (!strncasecmp (s, "ICE", 3))
        return ice;
    if (!strncasecmp (s, "TURB", 4))
        return turb;
    if (!strncasecmp (s, "MTW", 3))
        return mtw;
    if (!strncasecmp (s, "LLWS", 4))
        return llws;
    if (!strncasecmp (s, "PSYS", 4))
        return psys;
    if (!strncasecmp (s, "CLD", 3))
        return cld;
    if (!strncasecmp (s, "MVIS", 4))
        return mvis;
    if (!strncasecmp (s, "FZLVL", 5))
        return fzlvl;
    if (!strncasecmp (s, "Va", 2))
        return va;
    else
        return no_element_type;
}
    

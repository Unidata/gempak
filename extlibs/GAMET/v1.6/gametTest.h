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
*     software, the FSL Software Policy Manager (softwaremgr@fsl.noaa.gov)
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
* gametTest.h
*
* Version   1.1   12/22/03  Leslie Ewy   Original Version
* Version   1.2   01/13/04  Leslie Ewy   Incorporate NCEP corrections
*-------------------------------------------------------------------------*/
typedef enum {
    no_state,
    gamet_info,
    element_info,
    area_info
    } testState;

#define AREA_ITEMS 3
#define ELEMENT_ITEMS 2
#define GAMET_ITEMS 6



int fillGametStruct (FILE *fp, GametStruct *g);
void printGamet (GametStruct *gamet);
int initGamet (GametStruct *gamet);
int typeStrToEnum (char * s);

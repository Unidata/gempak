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
* gamet.h
* Version   1.1   12/22/03  Leslie Ewy   Original Version
* Version   1.2   01/13/04  Leslie Ewy   Corrections from NCEP
* Version   1.3   01/21/04  Leslie Ewy   Added states to AreaStruct and
*                                        GametStruct
* Version   1.4   02/13/04  Leslie Ewy   Incorporate Scott Jacobs
*                                        comments
* Version   1.5   04/09/04  Leslie Ewy   Mods for special cases
*-------------------------------------------------------------------------*/
#define FALSE 0
#define TRUE 1
#define MAX_AREAS 8              /* Enough ? */
#define MAX_ELEMENTS 14          /* 15 types, but is it possible for one type
                                    to appear more than once in a message? */ 

/* Enumerate the GFA and U.S. Gamet element types */ 
typedef enum {
    sfc_wspd,
    sfc_vis,
    sig_wx,
    mt_obscn,
    sig_cld,
    ice,
    turb,
    mtw,
    llws,
    psys,
    cld,
    mvis,
    fzlvl,
    va,
    no_element_type
    } ElementType;

/* Area Error Definition Masks */
#define AREA_NO_ERROR               0x00000000
#define AREA_BAD_VALID_PERIOD       0x00000001
#define AREA_INVALID_ATTRIBUTE      0x00000002
#define AREA_INVALID_VORSTRING      0x00000004
#define AREA_NULL_STRING            0x00000008
#define AREA_NO_STATE               0x00000010
#define AREA_NO_MOVEMENT            0x00000020

/* Valid Time Error Definition MASKS */
#define HEADER_NO_ERROR             0x00000000
#define HEADER_BAD_VALID_HOUR         0x00000001
#define HEADER_BAD_VALID_DAY          0x00000002
#define HEADER_NO_SYSTEM_TIME         0x00000004
#define HEADER_NULL_STRING            0x00000008
#define HEADER_EMPTY_STRING           0x00000010

/* Define Structures that make up gamet information */

/* Area within an element */
typedef struct {
    char* vorString;          /* vors [dist/bear] describing location */
    char* movement;           /* speed and direction, initially only for psys */
    char* attr;               /* phenomenon for element */
    int snapshot[3];          /* 0,3,6 forecast snapshots; valid = 1, not 0 */
    char* states;             /* States containing area */
}  AreaStruct;


/* Element within a gamet */
typedef struct {
    ElementType type;           /* see enum types below */
    AreaStruct area[MAX_AREAS]; /* vert, horz and information about element */
    int numAreas;               /* num areas in element */
    } ElementStruct; 

/* A Gamet */
typedef struct {
   ElementStruct element[MAX_ELEMENTS];
   char* fir;                  /* Forecast Information Region for Gamet */
   char* allStates;            /* All States mentioned in gamet */
   char *atsunit;
   char *originator;
   char *sigApp;               /* Sigmet Applicable */
   int issuanceDay;                    /* day issued */
   int issuanceHour;                   /* Hour issued */
   int numElements;
   } GametStruct;

/* Functions in gamet.c */

char *element_string (ElementType type);
int valid_period (AreaStruct *a, int hour, char *s);
void gamet_time (int day, int hour, int *retCode, char *gt);
int create_gamet_product (GametStruct* g, char *prod, char *errors);
void create_header (GametStruct *g, int *retCode, char *header);
void create_element (ElementStruct type, int hour,  
  char *elementString, char *errors);
void create_area (AreaStruct area, int hour, int areaNum, ElementType type,
                  int *retCode, char *areaStr);
void add_area_errors (int error_code, ElementType type, int areaNum, 
                      char *errors);
void add_header_errors (int errorCode, char *errorStr);

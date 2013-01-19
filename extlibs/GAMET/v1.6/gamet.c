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
* gamet.c
*
* Version   1.1   12/22/03  Leslie Ewy   Original Version
* Version   1.2   01/13/04  Leslie Ewy   Incorporate NCEP corrections
* Version   1.3   01/21/04  Leslie Ewy   Add State list for area and 
*                                        gamet structure
* Version   1.4   02/16/04  Leslie Ewy   Incorporate NCEP suggestions, bug
*                                        fixes 1) nil string when no
*                                        data for a type
* Version   1.5   04/09/04  Leslie Ewy   Add for psys, fzlvl va special
*                                        cases
* Version   1.6   05/06/04  Leslie Ewy   Add ability to output in order
*-------------------------------------------------------------------------*/
/*
 * Include files
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#ifdef AIX
#include <strings.h>
#endif
#include "gamet.h"


/**********************************************************************
create_gamet_product (GametStruct *gamet, char *prod, char *errors)
    Function creates a gamet product string from the Gamet Structure
    provided.  It is the caller's responsibility to provide
    an output string long enough for the product and for the error list.
    create_header creates a string that is copied to the product string.
    create_element copies the string it created into an array at the
    index of it's enum definition. When all elements have been processed
    it loops through the array to copy into the product string

    The error string is built up as errors are encountered.
    Caller should check return value to see if any errors occurred.
  
Input Parameters:
    *gamet        GametStruct
Output Parameters:
    *prod         char           character string of gamet text
    *errors       char           semi-coloned separated lists of
                                 failed parts
                                 If no errors occurred, errors string
                                 is an empty string
Functions Called:
    create_header
    add_header_errors
    create_element
Returns:  TRUE if a gamet product was created without errors, FALSE
          if not. Caller should check return value for FALSE, and if
          it is, examine the errors string.
**********************************************************************/

int create_gamet_product (GametStruct *g, char *prod, char *errors)
{
    int i;
    char gString[128];    /* char string for gamet header */
    char eString[512];
    char *elements[MAX_ELEMENTS+1];
    int sec2 = FALSE;

    int retCode;
    /* 
     * if either of the output strings are NULL, post message to stderr
     */
    if (prod == NULL)  {
        fprintf(stderr, "Product String is NULL, allocate appropriate space.\n");
        return FALSE;
    }
    if (errors == NULL) {
        fprintf(stderr, "Error String is NULL, allocate appropriate space.\n");
        return FALSE;
    }
    errors[0] = '\0';
    prod[0] = '\0';
    /*
     * create a string with the Gamet information, built from the 
     * individual pieces 
     */
    
    /* build the gamet header */
    create_header (g, &retCode, gString);
    /* 
     * Check retCode for error, and if one exists, call add_header_errors
     */
    if (retCode != HEADER_NO_ERROR) {
        add_header_errors (retCode, errors);
    } 
    /* Copy the header to the Gamet Product String */
    strcpy (prod, gString);

    /* Initialize the product element array */
    for (i = 0; i < MAX_ELEMENTS+1; i++)
        elements[i] = '\0';
    /* For each of the element types, build a string and put in elements
       array, this insures the correct order */
    for (i = 0; i < g->numElements; i++) {
        create_element (g->element[i], g->issuanceHour, eString, errors);
        elements[g->element[i].type] = (char *) malloc (strlen (eString) + 1);
        strcpy (elements[g->element[i].type], eString);
        /* strcat (prod, eString); */
    }
    /* Build the product string from the elements array */
    for (i = 0; i < MAX_ELEMENTS+1; i++) {
        if (elements[i] != NULL) {     /*only if there is information */
           /* add SECN II in the correct spot, if haven't already */
            if (i > llws && !sec2) {
                strcat(prod, "SECN II");
                sec2 = TRUE;
            }
            strcat (prod, elements[i]);
        }
        /* After LLWS copy in SIGMET Information */
        if (i == llws) {
            if (g->sigApp != NULL) {
                sprintf (gString, "\nSIGMET APPLICABLE: %s\n", g->sigApp);
                strcat (prod, gString);
            }
        }
    }
    
    /* If there is anything in errors, an error was encountered */
    if (strlen(errors))
        return FALSE;
    else
        return TRUE;
}


/**********************************************************************
valid_period (AreaStruct *area, int hour, char *valid)
    Function uses the area structures snapshot array to determine valid
    time for the gamet. The function handles time rollover as
    necessary. It is the caller's responsibility to provide
    an adequate character string.
Input Parameters:
    area          AreaStruct
    hour          int
Output Parameters:
    valid         char string of valid time (if it is determined the
                  event is for the entire 6 hour gamet period, the
                  string will be empty)
Returns: TRUE if able to create a valid period string, else FALSE.

**********************************************************************/
int valid_period (AreaStruct *a, int hour, char *vp)
{
    char *s;   /* local string */
    int beg, end, i;
 
    /* if invalid hour, return FALSE */
    if (hour < 0 || hour >= 24)
        return FALSE;
    s = vp;
    s[0] = '\0';
    /*
     * Check what hours are set in the snapshop array, and create
     * a valid period from this
     * The RULES: if snapshot[0] = 1 and snapshot[1] = 0
     *            valid for first 3 hours beg+3
     *            if snapshot[2] = 1 and snapshot[1] = 0
     *            valid for last 3 hours 
     *            if snapshot is all 0 or all 1, no valid period
     *            if any other value in snapshot, return FALSE
     *              for bad valid period
     */
    for (i = 0; i < 3; i++)
        if (a->snapshot[i] != 0 && a->snapshot[i] != 1)
            return (FALSE);
    if (a->snapshot[0] == 1 && a->snapshot[1] == 0) {
        /* hours 0 - 3 are valid */
        beg = hour;
        end = hour +3;
        if (end >= 24)
           end -= 24;
        sprintf(s, "%02d/%02d ", beg, end);
        }
    else if (a->snapshot[2] == 1 && a->snapshot[1] == 0) {
        /*hours 3 - 6 are valid */
        beg = hour + 3;
        end = beg +3;
        if (end >= 24)
           end -= 24;
        if (beg >= 24)
           beg -= 24; 
        sprintf(s, "%02d/%02d ", beg, end);
        }
     
    s[strlen(s)] = '\0';
    vp = s;
    return TRUE;
}

/**********************************************************************
gamet_time (int day, int hour, int *retCode, char *gTime)
    Function uses the issuance day and hour and creates a gamet
    valid period string. As necessary the routine handles time
    rollover.  It is the callers responsibility to provide a
    character string long enough for the valid time.

Input Parameters:
    day           int         issuance Day from Gamet Structure
    hour          int         issuance Hour from Gamet Structure
OutPut Parameters:
    gTime         char        character string of gamet valid time
                                 example: 221800/230000
   retCode        int         Return code. Return code will indicate 
                              any errors encountered during construction 
                              of the valid time string.
                              This is done by or'ing values:
                              HEADER_NO_ERROR               0x00000000
                              HEADER_BAD_VALID_HOUR         0x00000001
                              HEADER_BAD_VALID_DAY          0x00000002
                              HEADER_NO_SYSTEM_TIME         0x00000004
                              HEADER_NULL_STRING            0x00000008
                              HEADER_EMPTY_STRING           0x00000010

Assumptions:
   Gamets are forecasts. Therefore It is assumed that the construction
   of the gamet occurrs before the end hour of the valid period.
   When the current system time is obtained via the "time" and 
   "gmtime" calls that time is before the end hour of the valid period.
Caller needs to check value of retCode, to see if errors occurred.

**********************************************************************/
void gamet_time (int day, int hour, int *retCode, char* gt)
{
    time_t   now;
    struct tm *current;
    int endDay, endHour;

    /* Do initial checks on data, is it reasonable ? */
    if (hour < 0 || hour > 23) 
        *retCode |= HEADER_BAD_VALID_HOUR;
    if (day < 1 || day > 31)
        *retCode |= HEADER_BAD_VALID_DAY; 
       
    if (*retCode != HEADER_NO_ERROR) 
        return ;
    

    /* get the end hour of the valid period */
    endHour = hour + 6;
    if (endHour >= 24) {
        endHour -= 24;
        /* We have rolled over into a new day, check if the
           month may have rolled, get current time and use it
           as a guide to set the endDay to 1 or simply increment */
        if (time(&now) == (time_t)-1) {
            *retCode |= HEADER_NO_SYSTEM_TIME;
            return;
        }
        /* put it in a tm struct */
        current = gmtime (&now);
 
        /* incorporate issue day and hour */
        current->tm_mday = day;
        current->tm_hour = hour;
        /* 30th of April, June, September or November */
        if ((current->tm_mday == 30) && ((current->tm_mon == 3) ||
            (current->tm_mon == 5) || (current->tm_mon == 8) ||
            (current->tm_mon == 10)))
            endDay = 1;
        /* 28th of February, in a non leap year */
        else if ((current->tm_mday == 28) && (current->tm_mon == 1) &&
            (current->tm_year % 4 != 0))
            endDay = 1;
        /* 29 of February in a leap year */
        else if ((current->tm_mday == 29) && (current->tm_mon == 1) &&
            (current->tm_year % 4 == 0))
            endDay = 1;
        /* 31 in any month */
        else if (current->tm_mday == 31)
            endDay = 1;
        /* just increment day by 1 */
        else
            endDay = day + 1;
    }
    /* no need to roll day */
    else
        endDay = day;
    sprintf (gt, "%02d%02d00/%02d%02d00", 
      day, hour, endDay, endHour);
    return;
}

/**********************************************************************
create_header (GametStruct *gamet, int *retCode, char *header)    
    This function uses strings from the gamet structure (atsunit,
    originator and fir) and the valid period created in gamet_time
    and creates the gamet header string.
    Example:
    KZLC GAMET VALID 221800/230000 KKCI
    SALT LAKE CITY FIR BLW FL450..OR ID MT WY NV UT 
    SECN I
   It is the caller's responsibility to provide a string of
   adequate length.

Input Parameters:
    gamet        GametStruct  Values for the following are taken from
                              the gamet struct  
                              atsunit: air traffic services unit
                              originator: met office originating message
                              fir: flight information region or subarea
Output Parameters:
    header       char         string containing constants and input information
    retCode      int          Return code. Return code will indicate 
                              any errors encountered during construction 
                              of the header string.
                              This is done by or'ing values:
                              HEADER_NO_ERROR               0x00000000
                              HEADER_BAD_VALID_HOUR         0x00000001
                              HEADER_BAD_VALID_DAY          0x00000002
                              HEADER_NO_SYSTEM_TIME         0x00000004
                              HEADER_NULL_STRING            0x00000008
                              HEADER_EMPTY_STRING           0x00000010
Function Called:
  gamet_time
It is the caller's responsibility to check the value of retCode for
errors.
**********************************************************************/
void create_header (GametStruct *g, int *retCode, char *header)
{
    char validTime[16];
    *retCode = HEADER_NO_ERROR;
    /* get the valid period of the gamet */
    validTime[0] = '\0';
    gamet_time(g->issuanceDay, g->issuanceHour, retCode, validTime);
    /* if any of the strings are NULL, do not proceed */
    if (g->atsunit == NULL || validTime == NULL || g->originator == NULL ||
      g->fir == NULL || g->allStates == NULL || header == NULL) {
        *retCode |= HEADER_NULL_STRING;
        return;
    }
   
    /* if they are empty, indicate a problem, but go ahead and make
        what can be made of the header */
    if ((strlen (g->atsunit) == (size_t)0) || (strlen (validTime) == (size_t)0) ||
      (strlen(g->originator) == (size_t)0) || (strlen(g->fir) == (size_t)0) ||
      (strlen(g->allStates) == (size_t)0))
        *retCode |= HEADER_EMPTY_STRING;

    sprintf (header, "%s GAMET VALID %s %s\n%s..%s\nSECN I",
      g->atsunit, validTime, g->originator, g->fir, g->allStates);
    return;
}

/**********************************************************************
create_element (ElementStruct element, int issueHour, 
                char *eleStr, char *errorStr)
    This function takes information contained in the
    element structure and creates the string indicating the element
    type and calls create_area for each area of this type.
Input Parameters:
    element      ElementStruct   
    hour         int           issuance hour of gamet
Output Parameters:
    elementString char         string of type and information on each area
    errorStr      char         concatenated string of errors occurred while
                               creating area string
Functions Called:
    element_string
    create_area
    add_area_errors

**********************************************************************/
void create_element (ElementStruct element, int hour, 
  char *elementString, char *errors)
{
    char aString[256];
    char estr[64];
    int j;
    int retCode;
    /* Initial check */
    if (element.type >= MAX_ELEMENTS) {
        if (strlen(errors) == (size_t)0) {
            sprintf(estr, "Unexpected Element Type.");
            strcpy (errors, estr);
        }
        else {
            sprintf (estr, "; Unexpected Element Type.");
            strcat(errors, estr);
        }
        /* encountered error, set type to no type */
        element.type = no_element_type;
    }
    sprintf (elementString, "\n%s: ", element_string (element.type));
    /* For each area in an element, create the string */
    for (j = 0; j < element.numAreas; j++) {
        aString[0] = '\0';
        retCode = AREA_NO_ERROR;
        create_area(element.area[j], hour, j, element.type, &retCode, aString);
        /* check for errors */
        if (retCode != AREA_NO_ERROR)
            add_area_errors (retCode, element.type, j, errors);
        /* put a period between areas */
        if (j < element.numAreas - 1)
            strcat(aString, ". ");
        strcat (elementString, aString);
    }
}

/**********************************************************************
create_area (AreaStruct area, int hour, int areaNum, ElementType type,
             int *retCode, char *areaStr)

    Function creates a string from information in the Area Structure.
    The string is created based on the ElementType
    It is the caller's responsibility to provide a character
    string of adequate length.
Input Parameters:
    area          AreaStruct
    hour          hour gamet issued
    areaNum       number of area
    type          Element Type
Output Parameters:
    areaStr       character string with area information time, location
                  and description of event
    retCode       Return code. Return code will indicate any errors
                  encountered during construction of the area string.
                  This is done by or'ing values:
                  AREA_NO_ERROR               0x00000000
                  AREA_BAD_VALID_PERIOD       0x00000001
                  AREA_INVALID_ATTRIBUTE      0x00000002
                  AREA_INVALID_VOR_STRING     0x00000004
                  AREA_NULL_STRING            0x00000008
                  AREA_NO_STATE               0x00000010
                  AREA_NO_MOVEMENT            0x00000020

             
Function called:
    valid_period
**********************************************************************/
void create_area (AreaStruct area, int hour, int areaNum, ElementType type,
                  int *retCode, char *areaStr)
{
    char valid_str[16];
    valid_str[0] = '\0';
    *retCode = AREA_NO_ERROR;

    /* If any of the needed strings are NULL, don't go any further */
    if (areaStr == NULL || area.attr == NULL || area.vorString == NULL ||
        area.states == NULL) {
        *retCode = AREA_NULL_STRING;
        return;
    }

    /* check if the attribute value and the vor String are empty OR
       there is a NIL in the attribute string, if so copy NIL into 
       the area String  */
    if ( ( (!strlen (area.attr)) && (!strlen (area.vorString)) )
       || (strncasecmp (area.attr, "NIL", 3) == 0)) {
        sprintf(areaStr, "NIL");
        return;
    }

    /* Construct the Area String that will be returned */
    if (strlen(area.states))   /* Any states ? */
        sprintf(areaStr, "%s ", area.states);
 
    /*
       Handle the valid time with these rules : 
       freeze level and volcanoes do not have a valid period
       The first area of a pressure system valid period of the 00
       hour, the issue hour. Other areas of the psys do not.
       All other conditions should check for a valid period, one may
       not exist, then it is valid for entire period 
     */

    if (type == psys) {
        if (areaNum == 0)
            sprintf (valid_str, "%02d ", hour);
    }
    else if (type != fzlvl && type != va) {
        if (!valid_period (&area, hour, valid_str))
            *retCode |= AREA_BAD_VALID_PERIOD;
    }
    
    /* Check for missing values */
    if (!strlen(area.attr))
        *retCode |= AREA_INVALID_ATTRIBUTE;
    if (!strlen(area.vorString))
        *retCode |= AREA_INVALID_VORSTRING;
    if ((!strlen(area.states)) && type != fzlvl && type != psys)
        *retCode |= AREA_NO_STATE;
    if (type == psys){
        if (!strlen(area.movement)) 
            *retCode |= AREA_NO_MOVEMENT;
    }

    /* put what we have in the area string based on element type*/
    /* Volcano order is state, location (vorString), and attribute */
    if (type == va)
        sprintf(areaStr, "%s %s %s",area.states, area.vorString, area.attr);
    
    /* Presure system : valid time, attribute, vorString, movement */
    else if (type == psys) 
        sprintf(areaStr, "%s%s %s MOV %s",
            valid_str, area.attr, area.vorString, area.movement);

    /* Freeze level : attribute, vorString */
    else if (type == fzlvl)
        sprintf(areaStr, "%s %s", area.attr, area.vorString);

    /* All others : states, valid time, attribute, BOUNDED BY vorString */
    else
        sprintf (areaStr, "%s %s%s BOUNDED BY %s", 
          area.states, valid_str, area.attr, area.vorString);
    return;
}

/*********************************************************************
add_area_errors (int errorCode, ElementType type, int areaNum, char *errorStr)
    Based on bits set in errorCode, the function adds information
    to the errorStr string. See "create_area" above for possible
    values of errorCode. Information includes the element type and
    area number containing the error.
    Check the error, and copy or cat on to the error string.
Input Parameters:
    errorCode     int
    type          ElementType
    areaNum       int
Output Parameters:
    errorStr      char
Functions Called: element_string
**********************************************************************/
void add_area_errors (int error_code, ElementType type, int areaNum, 
                      char *errors)
{
    int no_errors;      /* empty or adding to error string */
    char tmp[256];
    char *eT;
    if (strlen (errors))
        no_errors = FALSE;
    else
        no_errors = TRUE;

    eT = element_string (type);
    if (error_code & AREA_BAD_VALID_PERIOD) {
        if (no_errors) {
            sprintf(tmp, "%s, Area %d: Bad Valid Period", eT,
              areaNum+1);
            strcpy (errors, tmp);
        }
        else {
            sprintf(tmp, "; %s, Area %d: Bad Valid Period", eT,
              areaNum+1);
            strcat (errors, tmp);
        }
    no_errors = FALSE;
    }
    if (error_code & AREA_INVALID_ATTRIBUTE) {
        if (no_errors) {
            sprintf(tmp, "%s, Area %d: No Area Attribute", eT, areaNum+1);
            strcpy (errors, tmp);
        }
        else {
            sprintf(tmp, "; %s, Area %d: No Area Attribute", eT,
              areaNum+1);
            strcat (errors, tmp);
        }
    no_errors = FALSE;
    } 
    if (error_code & AREA_INVALID_VORSTRING) {
        if (no_errors) {
            sprintf(tmp, "%s, Area %d: No Area Location Information",
              eT, areaNum+1);
            strcpy (errors, tmp);
        }
        else {
            sprintf(tmp, "; %s, Area %d: No Area Location Information",
              eT, areaNum+1);
            strcat (errors, tmp);
        }
    no_errors = FALSE;
    } 
    if (error_code & AREA_NO_STATE) {
        if (no_errors) {
            sprintf(tmp, "%s, Area %d: No Area State Information",
              eT, areaNum+1);
            strcpy (errors, tmp);
        }
        else {
            sprintf(tmp, "; %s, Area %d: No Area State Information",
              eT, areaNum+1);
            strcat (errors, tmp);
        }
    no_errors = FALSE;
    } 
    if (error_code & AREA_NO_MOVEMENT) {
        if (no_errors) {
            sprintf(tmp, "%s, Area %d: No Area Pressure System Movement Information",
              eT, areaNum+1);
            strcpy (errors, tmp);
        }
        else {
            sprintf(tmp, "; %s, Area %d: No Area Pressure System Movement Information",
              eT, areaNum+1);
            strcat (errors, tmp);
        }
    no_errors = FALSE;
    }
}


/*********************************************************************
add_header_errors (int errorCode, char *errorStr)
    Based on bits set in errorCode, the function adds information
    to the errorStr string. See "create_header" above for values.
Input Parameters:
    errorCode     int   
Output Parameters:
    errorStr      char  

**********************************************************************/
void add_header_errors (int errorCode, char *errorStr)
{
    int error = FALSE;            /* no error encountered yet */
    /*
     * check error code bits, and copy or cat onto the error string
     */
    if (errorCode & HEADER_BAD_VALID_HOUR) {
        strcpy (errorStr, "Gamet Header bad Valid Hour");
        error = TRUE;
    }
    if (errorCode & HEADER_BAD_VALID_DAY) {
        if (error)
            strcat(errorStr, "; Gamet Header bad Valid Day");
        else
            strcpy(errorStr, "Gamet Header bad Valid Day");
        error = TRUE;
    }
    if (errorCode & HEADER_NO_SYSTEM_TIME) {
        if (error)
            strcat(errorStr, "; Unable to get System time");
        else
            strcpy(errorStr, "Unable to get System time");
        error = TRUE;
    }
    if (errorCode & HEADER_NULL_STRING) {
        if (error)
            strcat(errorStr, "; String required for Header was NULL");
        else
            strcpy(errorStr, "String required for Header was NULL");
        error = TRUE;
    }
    if (errorCode & HEADER_EMPTY_STRING) {
        if (error)
            strcat(errorStr, "; String required for Header was empty");
        else
            strcpy(errorStr, "String required for Header was empty");
        error = TRUE;
    }
}
  
/**********************************************************************
element_string (ElementType type)
    Function takes an enum value type of ElementType and returns the
    character string for the type. See gamet.h for enum values.
Input Parameters:
    type          ElementType
Output Parameters:
    string associated with type from above table

**********************************************************************/
char * element_string (ElementType type)
{
  char * ElementStrings[] = {
  "SFC WSPD",
  "SFC VIS",
  "SIG WX",
  "MT OBSCN",
  "SIG CLD",
  "ICE",
  "TURB",
  "MTW",
  "LLWS",
  "PSYS",
  "CLD",
  "MVIS",
  "FZLVL",
  "VA",
  "UNKNOWN"};

    return ElementStrings[type];
}


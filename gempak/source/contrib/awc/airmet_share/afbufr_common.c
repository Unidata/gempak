#include "afbufr_common.h"
#include <string.h>
#include <time.h>
#include <stdio.h>

#define MAXST 128

void setCycleAndType(char *outputFormatIn, char *cycle, char *airmetType,
                    char *outputFormatStr)
/******************************************************************************
  setCycleAndType

  This routine takes the output format string in outputFormatIn, and
  substitutes the contents of either the cycle airmetType for these string literals:
  '<cycle>' and '<type>', should they exist.
  Output is in outputFormatStr

  void setCycleAndType (outputFormatIn, cycle, airmetType, outputFormatStr)

  Input parameters:
    outputFormatIn    char *    Input Format String
    cycle             char *    Typically will be 03Z, 09Z, 15Z, 21Z
    airmetType        char *    Typically will be TANGO, SIERRA, TANGO
  Output parameters:
    outputFormatStr   char *    Output Format String.
  Log:
  L. Hinson/AWC       01/06     Created

******************************************************************************/
{
  char *ptr=NULL;
  if ((ptr=strstr(outputFormatIn,"<cycle>")) !=NULL) {
    strcpy(outputFormatStr,outputFormatIn);
    outputFormatStr[ptr-outputFormatIn]='\0';
    strcat(outputFormatStr,cycle);
    strcat(outputFormatStr,"Z");
    strcat(outputFormatStr,ptr+7);
    strcpy(outputFormatIn, outputFormatStr);
  }
  if ((ptr=strstr(outputFormatIn,"<type>")) != NULL) {
    strcpy(outputFormatStr,outputFormatIn);
    outputFormatStr[ptr-outputFormatIn]='\0';
    strcat(outputFormatStr,airmetType);
    strcat(outputFormatStr,ptr+6);
  } else {
    strcpy(outputFormatStr,outputFormatIn);
  }
}

void buildFilenameAndDateStampFmIssTime(char *issTimeStr, char *outputformat,
                                   char *filename, char *dateStamp) {
    /* Routine uses issue time for setting file name.
       System time (gmtime) is used for the datestamp that gets set in
       the creation of the BUFR message
       to */
    int year, month, day, hr, min;
#ifdef Linux
    struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\0'};
#else
    struct tm trec = {0, 0, 0, 0, 0, 0, 0, 0, 0};
#endif
    time_t rawtime;
    struct tm *timeinfo;
    sscanf(issTimeStr,"%4d%2d%2d%2d%2d", &year, &month, &day, &hr,
           &min);
    trec.tm_year=year-1900;
    trec.tm_mon=month-1;
    trec.tm_mday=day;
    trec.tm_hour=hr;
    trec.tm_min=min;
    strftime(filename,MAXST,outputformat,&trec);
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    strftime(dateStamp,MAXST,"%Y%m%d_%H%M",timeinfo);
    printf("gmtime timestamp=%s\n",dateStamp);
}

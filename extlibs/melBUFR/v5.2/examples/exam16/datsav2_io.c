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
#include "datsav2_io.h"

#if PROTOTYPE_NEEDED
char *substr(char *s, int begin, int length)
#else
char *substr(s, begin, length)
char *s;
int begin;
int length;
#endif
{
  char *ss = (char *) malloc(sizeof(char) * (length + 1));

  int i;

  for (i = 0; i < length; i++) ss[i] = s[begin + i];

  ss[length] = 0x0;

  return ss;

} /* end substr() */

#if PROTOTYPE_NEEDED
int substrtoi(char *s, int begin, int length)
#else
int substrtoi(s, begin, length)

char *s;
int begin;
int length;
#endif
{
  char *ss;

  int i;

  ss = substr(s,begin,length);

  i = atoi(ss);

  free(ss);

  return i;

} /* end substrtoi() */

#if PROTOTYPE_NEEDED
float substrtof(char *s, int begin, int length)
#else
float substrtof(s, begin, length)
char *s;
int begin;
int length;
#endif
{
  char *ss;

  float f;

  ss = substr(s,begin,length);

  f = atof(ss);

  free(ss);

  return f;

} /* end substrtof() */

#if PROTOTYPE_NEEDED
int is_valid(char *Raw_Element)
#else
int is_valid(Raw_Element)
char *Raw_Element;
#endif
{

  char *MISSING = "9999999999";
  int Valid = 0, len = 0;

  len = strlen(Raw_Element);
  
  if (strncmp(MISSING,Raw_Element,len) != 0) 
    Valid = 1;
 
  return Valid;
 
} /* end is_valid() */


#if PROTOTYPE_NEEDED
DS2_Surface_t *DS2_get_surface(FILE *fp_ds2)
#else
DS2_Surface_t *DS2_get_surface(fp_ds2)
FILE *fp_ds2;
#endif
{

  DS2_Surface_t *rec = 0x0;
  static DS2_Surface_t ds2_sfc;

  char *Raw_Element;
  int Type_Of_Station;
  char buffer[1001];
  
  if (fgets(buffer,1000,fp_ds2)) {

    rec = &ds2_sfc;
    /* rec = (DS2_Surface_t *) malloc(sizeof(DS2_Surface_t));  */
      
    rec->WMO_Block = substrtof(buffer,16,2);
    rec->WMO_Station = substrtof(buffer,18,3);
      
    /*
    --  BUFR Code Table 0-02-001 is inferred from the DATSAV2
    --  surface field OB_TYPE (columns 47-48)
    */
    Type_Of_Station = substrtoi(buffer,46,2);
    if (Type_Of_Station <  6) rec->Type_Of_Station = 1.0;
    if (Type_Of_Station == 6) rec->Type_Of_Station = 0.0;
    if (Type_Of_Station >  6) rec->Type_Of_Station = 2.0;
      
    rec->Year = substrtof(buffer,22,4);
    rec->Month = substrtof(buffer,26,2);
    rec->Day = substrtof(buffer,28,2);

    rec->Hour = substrtof(buffer,30,2);
    rec->Minute = substrtof(buffer,32,2);
    
    Raw_Element = substr(buffer,34,5);
    if ( is_valid(Raw_Element) ) 
      rec->Latitude = 
	substrtof(buffer,34,3) + substrtof(buffer,37,2)/60.0;
    else
      rec->Latitude = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    /*
    --  DATSAV2 longitude convention is West(-) and East(+)
    */
    Raw_Element = substr(buffer,39,6);
    if ( is_valid(Raw_Element) ) {

      rec->Longitude = -1.0 * substrtof(buffer,39,4);

      if (rec->Longitude > 0.0) 
	rec->Longitude = rec->Longitude +
	  substrtof(buffer,43,2)/60.0;
      else
	rec->Longitude = rec->Longitude -
	  substrtof(buffer,43,2)/60.0;

    }
    else
      rec->Longitude = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    Raw_Element = substr(buffer,51,5);
    if ( is_valid(Raw_Element) )
      rec->Height = atof(Raw_Element);
    else 
      rec->Height = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    /*
    --  Station pressure 0-10-004 is not encoded in the
    --  DATSAV2 record, only mean sea level pressure 0-10-051.
    */
    rec->Pressure = BUFR_MISSING_VALUE;
    
    Raw_Element = substr(buffer,150,5);
    if ( is_valid(Raw_Element) )
      rec->MSL_Pressure = atof(Raw_Element) * 10.0;
    else
      rec->MSL_Pressure = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    Raw_Element = substr(buffer,156,3);
    if ( is_valid(Raw_Element) )
      rec->Pressure_Change = atof(Raw_Element) * 10.0;
    else
      rec->Pressure_Change = BUFR_MISSING_VALUE;
    
    rec->Pressure_Tendency = substrtof(buffer,155,1);
    free(Raw_Element);
    
    Raw_Element = substr(buffer,142,4);
    if ( is_valid(Raw_Element) )
      rec->Dry_Bulb_Temp = atof(Raw_Element) / 10.0;
    else
      rec->Dry_Bulb_Temp = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    Raw_Element = substr(buffer,146,4);
    if ( is_valid(Raw_Element) )
      rec->Dewpoint_Temp = atof(Raw_Element) / 10.0;
    else
      rec->Dewpoint_Temp = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    /*
    --  Relative humidity 0-13-003 is not encoded in the
    --  DATSAV2 surface record.  Although we might calculate
    --  RH on the fly, we would give the false impression that
    --  this was part of the original DATSAV2 dataset. Thus, we
    --  encode RH as a missing value.
    */
    rec->Relative_Humidity = BUFR_MISSING_VALUE;

    Raw_Element = substr(buffer,116,6);
    if ( is_valid(Raw_Element) )
      rec->Visibility = atof(Raw_Element);
    else
      rec->Visibility = BUFR_MISSING_VALUE;
    if (rec->Visibility > 80000.0) rec->Visibility = 80000.0;   
    free(Raw_Element);

 
    Raw_Element = substr(buffer,123,3);
    if ( is_valid(Raw_Element) ) 
      rec->Present_Weather = atof(Raw_Element);
    else 
      rec->Present_Weather = BUFR_MISSING_VALUE;
    free(Raw_Element);

    Raw_Element = substr(buffer,135,3);
    if ( is_valid(Raw_Element) )
      rec->Past_Weather[0] = atof(Raw_Element);
    else
      rec->Past_Weather[0] = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    Raw_Element = substr(buffer,138,2);
    if ( is_valid(Raw_Element) )
      rec->Past_Weather[1] = atof(Raw_Element);
    else
      rec->Past_Weather[1] = BUFR_MISSING_VALUE;
    free(Raw_Element);


    Raw_Element = substr(buffer,88,3);
    if ( is_valid(Raw_Element) ) 
      rec->Wind_Direction = atof(Raw_Element);
    else
      rec->Wind_Direction = BUFR_MISSING_VALUE;
    free(Raw_Element);

    Raw_Element = substr(buffer,91,4);
    if ( is_valid(Raw_Element) )
      rec->Wind_Speed = atof(Raw_Element) / 10.0;
    else
      rec->Wind_Speed = BUFR_MISSING_VALUE;
    free(Raw_Element);
    
    rec->Total_Cloud_Cover = BUFR_MISSING_VALUE;
    rec->Vert_Signficance = BUFR_MISSING_VALUE;
    rec->Cloud_Amount = BUFR_MISSING_VALUE;
    
    rec->Height_Of_Base = BUFR_MISSING_VALUE;
    
    rec->Cloud_Type[0] = BUFR_MISSING_VALUE;
    rec->Cloud_Type[1] = BUFR_MISSING_VALUE;
    rec->Cloud_Type[2] = BUFR_MISSING_VALUE;
    
    buffer[0] = 0x0;
    
  } /* end if(fgets) */

  return rec;
  
} /* end DS2_get_surface() */


#if PROTOTYPE_NEEDED
void DS2_dump_surface(DS2_Surface_t *rec)
#else
void DS2_dump_surface(rec)
DS2_Surface_t *rec;
#endif
{

  fprintf(stderr,"DATSAV2 Record\n");
  fprintf(stderr,"WMO Number= %02.0f %03.0f\t",
	  rec->WMO_Block,rec->WMO_Station);
  fprintf(stderr,"Date/Time= %4.0f %02.0f %02.0f %02.0f %02.0f\n",
	  rec->Year,rec->Month,rec->Day,rec->Hour,rec->Minute);
  fprintf(stderr,"Latitude                = %f\n",rec->Latitude);
  fprintf(stderr,"Longitude               = %f\n",rec->Longitude);
  fprintf(stderr,"Dry Bulb Temperature    = %f\n",rec->Dry_Bulb_Temp);
  fprintf(stderr,"Dewpoint Temperature    = %f\n",rec->Dewpoint_Temp);
  fprintf(stderr,"Mean Sea Level Pressure = %f\n",rec->MSL_Pressure);
  fprintf(stderr,"Wind Direction          = %f\n",rec->Wind_Direction);
  fprintf(stderr,"Wind Speed              = %f\n",rec->Wind_Speed);
  fprintf(stderr,"\n");

} /* end DS2_dump_surface() */

#if PROTOTYPE_NEEDED
Initialization_Data_t *get_ini_data(char *fn)
#else
Initialization_Data_t *get_ini_data(fn)
char *fn;
#endif
{

  FILE *fp_ini;

  Initialization_Data_t *request;

  char linein[81];

  char *parm,*value;

  int INI_error;

  char *INI_message[20][4]; /* = {
    "BUFR_output_file=",
    "initial_DTG=",
    "final_DTG=",
    "geographic_box="
  }; */

  float swap;
/*  strcpy(INI_message[0], "BUFR_output_file=");
  strcpy(INI_message[1], "initial_DTG=");
  strcpy(INI_message[2], "final_DTG=");
  strcpy(INI_message[3], "geographic_box="); */

printf(" in get_ini_data \n");
   INI_error = 0;
  
  /*
  --  Does the specified initialization file exist?
  */
  if ( !(fp_ini = fopen(fn,"rt")) )  
    error_file_not_found("get_ini_data",fn);

  request = (Initialization_Data_t *) 
    malloc(sizeof(Initialization_Data_t));

  request->BUFR_output_file[0] = 0x0;

  request->initial_DTG.Year = BUFR_MISSING_VALUE;
  request->initial_DTG.Month = BUFR_MISSING_VALUE;
  request->initial_DTG.Day = BUFR_MISSING_VALUE;
  request->initial_DTG.Hour = BUFR_MISSING_VALUE;
  request->initial_DTG.Minute = BUFR_MISSING_VALUE;

  request->final_DTG.Year = BUFR_MISSING_VALUE;
  request->final_DTG.Month = BUFR_MISSING_VALUE;
  request->final_DTG.Day = BUFR_MISSING_VALUE;
  request->final_DTG.Hour = BUFR_MISSING_VALUE;
  request->final_DTG.Minute = BUFR_MISSING_VALUE;

  request->geographic_box.Lower_Latitude = BUFR_MISSING_VALUE;
  request->geographic_box.Upper_Latitude = BUFR_MISSING_VALUE;
  request->geographic_box.Lower_Longitude = BUFR_MISSING_VALUE;
  request->geographic_box.Upper_Longitude = BUFR_MISSING_VALUE;

  while (!feof(fp_ini)) {

    if (fgets(linein,80,fp_ini) == NULL) continue;
    
    if (linein[0] != '#') {

      parm = strtok(linein,"=");
      value = strtok(NULL,"=; ");

      if (strncmp(parm,"BUFR_output_file",16) == 0) {
        value[strlen(value)-1] = '\0';
	strcpy(request->BUFR_output_file,value);
      }
     
      if ( strncmp(parm, "Input_file",  10) == 0 ) {
	  value[strlen(value)-1] = '\0';
	  strcpy(request->Input_file, value);
      }
      
      if (strncmp(parm,"initial_DTG",11) == 0) {
	request->initial_DTG.Year = substrtof(value,0,2);
	request->initial_DTG.Month = substrtof(value,2,2);
	request->initial_DTG.Day = substrtof(value,4,2);
	request->initial_DTG.Hour = substrtof(value,6,2);
	request->initial_DTG.Minute = substrtof(value,8,2);
      }

      if (strncmp(parm,"final_DTG",9) == 0) {
	request->final_DTG.Year = substrtof(value,0,2);
	request->final_DTG.Month = substrtof(value,2,2);
	request->final_DTG.Day = substrtof(value,4,2);
	request->final_DTG.Hour = substrtof(value,6,2);
	request->final_DTG.Minute = substrtof(value,8,2);
      }
	

      if (strncmp(parm,"geographic_box",14) == 0) {
	request->geographic_box.Upper_Latitude = atof(value);
	value = strtok(NULL,"=; ");
	request->geographic_box.Lower_Latitude = atof(value);
	value = strtok(NULL,"=; ");
	request->geographic_box.Upper_Longitude = atof(value);
	value = strtok(NULL,"=; ");
	request->geographic_box.Lower_Longitude = atof(value);
      }

      linein[0] = 0x0;

    } /* end if(linein[0]) */

  } /* end while(!feof) */

  /*
  --  Determine if any parameters are missing.
  */
  if (!request->BUFR_output_file) INI_error = 1;
  
  if ( request->initial_DTG.Year == BUFR_MISSING_VALUE ||
      request->initial_DTG.Month == BUFR_MISSING_VALUE ||
      request->initial_DTG.Day == BUFR_MISSING_VALUE ||
      request->initial_DTG.Hour == BUFR_MISSING_VALUE ||
      request->initial_DTG.Minute == BUFR_MISSING_VALUE )
    INI_error = 2;
  
  if ( request->final_DTG.Year == BUFR_MISSING_VALUE ||
      request->final_DTG.Month == BUFR_MISSING_VALUE ||
      request->final_DTG.Day == BUFR_MISSING_VALUE ||
      request->final_DTG.Hour == BUFR_MISSING_VALUE ||
      request->final_DTG.Minute == BUFR_MISSING_VALUE )
    INI_error = 3;
  
  if ( request->geographic_box.Lower_Latitude == BUFR_MISSING_VALUE ||
      request->geographic_box.Upper_Latitude == BUFR_MISSING_VALUE ||
      request->geographic_box.Lower_Longitude == BUFR_MISSING_VALUE ||
      request->geographic_box.Upper_Longitude == BUFR_MISSING_VALUE )
    INI_error = 4;
  
  if (INI_error != 0) {

    printf("Error in extraction template %s\n",fn);
    printf("Bad or missing %s\n",INI_message[0][INI_error - 1]);

  } /* end if(!INI_error) */

  /*
  --  If the user accidentally swapped upper and lower
  --  values in the geographic box, fix the problem
  --  and move on.
  */
  if (request->geographic_box.Lower_Latitude > 
      request->geographic_box.Upper_Latitude) 
    {
      swap = request->geographic_box.Lower_Latitude;
      request->geographic_box.Lower_Latitude = 
	request->geographic_box.Upper_Latitude;
      request->geographic_box.Upper_Latitude = swap;
    }

  if (request->geographic_box.Lower_Longitude > 
      request->geographic_box.Upper_Longitude) 
    {
      swap = request->geographic_box.Lower_Longitude;
      request->geographic_box.Lower_Longitude = 
	request->geographic_box.Upper_Longitude;
      request->geographic_box.Upper_Longitude = swap;
    }

  return request;

} /* end get_ini_data() */


#if PROTOTYPE_NEEDED
void dump_ini_data(Initialization_Data_t *request)
#else
void dump_ini_data(request)
Initialization_Data_t *request;
#endif
{

  printf("\nINITIALIZATION DATA\n");
  printf("===================\n");
  printf("BUFR_output_file  = %s\n",request->BUFR_output_file);

  printf("initial_DTG       = %02.0f %02.0f %02.0f %02.0f %02.0f\n",
	 request->initial_DTG.Year,
	 request->initial_DTG.Month,
	 request->initial_DTG.Day,
	 request->initial_DTG.Hour,
	 request->initial_DTG.Minute);

  printf("final_DTG         = %02.0f %02.0f %02.0f %02.0f %02.0f\n",
	 request->final_DTG.Year,
	 request->final_DTG.Month,
	 request->final_DTG.Day,
	 request->final_DTG.Hour,
	 request->final_DTG.Minute);

  printf("Geographic Box\n");
  printf("--------------\n");

  printf("\tUpper Latitude  = %6.2f\n",
	 request->geographic_box.Upper_Latitude);
  printf("\tLower Latitude  = %6.2f\n",
	 request->geographic_box.Lower_Latitude);
  printf("\tUpper Longitude = %6.2f\n",
	 request->geographic_box.Upper_Longitude);
  printf("\tLower Longitude = %6.2f\n",
	 request->geographic_box.Lower_Longitude);

} /* end dump_ini_data() */


#if PROTOTYPE_NEEDED
int is_selected(DS2_Surface_t *ds2_rec, 
		Initialization_Data_t *request)
#else
int is_selected(ds2_rec, request)
DS2_Surface_t *ds2_rec;
Initialization_Data_t *request;
#endif
{

  int selected = 1;

  /*
  --  If WMO Number is coded as 99 999, this is a ship observation.
  */
  if (ds2_rec->WMO_Block > 98.0) 
    selected = 0;
  
  if (ds2_rec->Latitude > request->geographic_box.Upper_Latitude ||
      ds2_rec->Latitude < request->geographic_box.Lower_Latitude)
    selected = 0;

  if (ds2_rec->Longitude > request->geographic_box.Upper_Longitude ||
      ds2_rec->Longitude < request->geographic_box.Lower_Longitude)
    selected = 0;

  if (ds2_rec->Hour < request->initial_DTG.Hour ||
      ds2_rec->Hour > request->final_DTG.Hour)
    selected = 0;

  return selected;

} /* end is_selected() */

#if PROTOTYPE_NEEDED
void error_file_not_found(char *where,char *fn)
#else
void error_file_not_found(where, fn)
char *where;
char *fn;
#endif
{

  printf("\nError in %s(),\nCannot find file %s\n\n",where,fn);
  exit(1);

} /* end error_file_not_found() */

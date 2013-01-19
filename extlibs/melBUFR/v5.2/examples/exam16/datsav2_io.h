/***********************************************************************
**
**  PROJECT. Master Environmental Library (MEL)
**
**  FILENAME. datsav2_io.h
**
**  DESCRIPTION.
**
***********************************************************************/
/*
==  
==  SECTION. Includes/Defines
==
*/

#include <stdio.h>
#include <mel_bufr.h>

#ifdef CAST_TC_TEST
#define MEL_DATA_DIRECTORY  "sf"
#else
#define MEL_DATA_DIRECTORY  "./sf"
#endif  /*  CAST_TC_TEST  */

/*
==  
==  SECTION.  Data Structures/Types
==
*/

/*
--
--  TYPE. DS2_Surface_t
--
--    This structure is designed to match data from a DATSAV2 surface
--    record described in 
--
--          DATSAV2 Surface
--          USAFETAC Climatic Database Users' Handbook
--          USAFETAC/UH-86/004
--
--    to a BUFR record of type 3-07-002 described in 
--
--          WMO Manual On Codes
--          Volume 1 (1988 Edition)
--          International Codes
--          Part B -- Binary Codes
--          WMO No. 306
--          Supplement No. 7B (August 1994)
--
*/
#define DS2_RECORD_LENGTH           31     /* 31 float entries */

typedef struct {
/*
--  BUFR 3-07-002               3-01-032   Station Identification
--                              3-02-011   Basic Surface Report
*/

/*
--  BUFR 3-01-32                3-01-001   WMO Number
--                              0-02-001   Type of Station
--                              3-01-011   Date
--                              3-01-012   Time
--                              3-01-024   Lat/Lon/Elevation
*/

/*
--  BUFR 3-01-001
*/
  float WMO_Block;           /* 0-01-001 */     /* 0 */
  float WMO_Station;         /* 0-01-002 */     /* 1 */

  float Type_Of_Station;     /* 0-02-001 */     /* 2 */

/*
--  BUFR 3-01-011
*/
  float Year;                /* 0-04-001 */     /* 3 */
  float Month;               /* 0-04-002 */     /* 4 */
  float Day;                 /* 0-04-003 */     /* 5 */

/*
--  BUFR 3-01-012
*/
  float Hour;                /* 0-04-004 */     /* 6 */
  float Minute;              /* 0-04-005 */     /* 7 */

/*
--  BUFR 3-01-024 
*/
  float Latitude;            /* 0-05-002 */     /* 8 */
  float Longitude;           /* 0-06-002 */     /* 9 */
  float Height;              /* 0-07-001 */     /* 10 */


/*
--  BUFR 3-02-011               3-02-001  Pressure and Pressure Change
--                              3-02-003  Wind, Temperature, Humidity
--                              3-02-004  Signficant Cloud Layer
*/

/*
--  BUFR 3-02-001
*/
  float Pressure;            /* 0-10-004 */     /* 11 */
  float MSL_Pressure;        /* 0-10-051 */     /* 12 */
  float Pressure_Change;     /* 0-10-061 */     /* 13 */

  float Pressure_Tendency;   /* 0-10-063 */     /* 14 */

/*
--  BUFR 3-02-003
*/
  float Wind_Direction;      /* 0-11-011 */     /* 15 */
  float Wind_Speed;          /* 0-11-012 */     /* 16 */
  float Dry_Bulb_Temp;       /* 0-12-004 */     /* 17 */
  float Dewpoint_Temp;       /* 0-12-006 */     /* 18 */
  float Relative_Humidity;   /* 0-13-003 */     /* 19 */
  float Visibility;          /* 0-20-001 */     /* 20 */
  float Present_Weather;     /* 0-20-003 */     /* 21 */
  float Past_Weather[2];     /* 0-20-004 and 0-20-005 */  /* 22,23 */

/*
--  BUFR 3-02-004
*/
  float Total_Cloud_Cover;   /* 0-20-010 */     /* 24 */
  float Vert_Signficance;    /* 0-08-002 */     /* 25 */
  float Cloud_Amount;        /* 0-20-011 */     /* 26 */
  float Height_Of_Base;      /* 0-20-013 */     /* 27 */
  float Cloud_Type[3];       /* 0-20-012 */     /* 28-30 */

} DS2_Surface_t;


typedef struct {

  float Year, Month, Day, Hour, Minute;

} Date_Time_Group_t;

typedef struct {

  float Upper_Latitude, Lower_Latitude;
  float Upper_Longitude, Lower_Longitude;

} Geographic_Box_t;

typedef struct {

  char BUFR_output_file[256];
  char Input_file[60];
  Date_Time_Group_t initial_DTG;
  Date_Time_Group_t final_DTG;
  Geographic_Box_t geographic_box;

} Initialization_Data_t;

/*
==  
==  SECTION.  Function Prototypes
==
*/

#if PROTOTYPE_NEEDED

DS2_Surface_t *DS2_get_surface(FILE *fp_ds2);

void DS2_dump_surface(DS2_Surface_t *rec);

Initialization_Data_t *get_ini_data(char *fn);

char *build_filename (Date_Time_Group_t *initial);

int is_selected(DS2_Surface_t *ds2_rec, Initialization_Data_t *init);

void error_file_not_found(char *where, char *fn);
void dump_ini_data(Initialization_Data_t *request);
#else

DS2_Surface_t *DS2_get_surface();

void DS2_dump_surface( );

Initialization_Data_t *get_ini_data();

char *build_filename ();

int is_selected();

void error_file_not_found();
void dump_ini_data();
#endif

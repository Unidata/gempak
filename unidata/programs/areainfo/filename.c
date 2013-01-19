/*
 * $Id: filename.c,v 1.3 1992/05/04 20:13:28 steve Exp $
 *
 * From Cathy C.
 */

#include <stdio.h>
#include <string.h>
#include "mc_area.h"

extern int ymdh;


static int daytab[2][13] = {
    {0,31,28,31,30,31,30,31,31,30,31,30,31},
    {0,31,29,31,30,31,30,31,31,30,31,30,31}
};


/* month_day: set month, day from day of year */
void
month_day(year,yearday,month,day)
int year;
int yearday;
int *month;
int *day;
{
    int i, leap;
    leap = year%4 == 0 && year%100 != 0 || year%400 == 0;
    for (i = 1; yearday > daytab[leap][i]; i++)
      yearday -= daytab[leap][i];
    *month = i;
    *day = yearday;
}


static char *
gettype(key)
unsigned long key;
{
/* how to distinguish floater, water vapor ??? */
	switch(key) {
	case  0 : return "derv"  ; /* Derived data */
	case  1 : return "test"  ; /* Test Pattern */
	case  2 : return "graf"  ; /* Graphics */
	case  3 : return "misc"  ; /* Miscellaneous */
	case  4 : return "pdmv"  ; /* PDUS METEOSAT Visible */ 
	case  5 : return "pdmi"  ; /* PDUS METEOSAT Infrared */ 
	case  6 : return "pdmw"  ; /* PDUS METEOSAT Water Vapor */ 
	case  7 : return "radr"  ; /* Radar */
	case  8 : return "acft"  ; /* Miscellaneous Aircraft Data */
	case  9 : return "rmet"  ; /* Raw METEOSAT */
	case 10 : return "comp"  ; /* Composite */
	case 12 : return "gmsv"  ; /* GMS Visible */
	case 13 : return "gmsi"  ; /* GMS Infrared */
	case 14 : return "ats6v" ; /* ATS 6 Visible */
	case 15 : return "ats6i" ; /* ATS 6 Infrared */
	case 16 : return "sms1v" ; /* SMS-1 Visible */
	case 17 : return "sms1i" ; /* SMS-1 Infrared */
	case 18 : return "sms2v" ; /* SMS-2 Visible */
	case 19 : return "sms2i" ; /* SMS-2 Infrared */
	case 20 : return "g1vis" ; /* GOES-1 Visible */
	case 21 : return "g1ir"  ; /* GOES-1 Infrared */
	case 22 : return "g2vis" ; /* GOES-2 Visible */
	case 23 : return "g2ir"  ; /* GOES-2 Infrared */
	case 24 : return "g3vis" ; /* GOES-3 Visible */
	case 25 : return "g3ir"  ; /* GOES-3 Infrared */
	case 26 : return "g4vis" ; /* GOES-4 Visible */
	case 27 : return "g4ir"  ; /* GOES-4 Infrared */
	case 28 : return "g5vis" ; /* GOES-5 Visible */
	case 29 : return "g5ir"  ; /* GOES-5 Infrared */
	case 30 : return "g6vis" ; /* GOES-6 Visible */
	case 31 : return "g6ir"  ; /* GOES-6 Infrared */
	case 32 : return "GOES-7" ; /* GOES-7 Visible */
	case 33 : return "GOES-7"  ; /* GOES-7 Infrared */
	case 41 : return "tirn"  ; /* TIROS-N (POES) */
	case 42 : return "noaa6" ; /* NOAA-6 */
	case 43 : return "noaa7" ; /* NOAA-7 */
	case 44 : return "noaa8" ; /* NOAA-8 */
	case 45 : return "noaa9" ; /* NOAA-9 */
	case 46 : return "venus" ; /* VENUS */
	case 47 : return "vgr1"  ; /* Voyager 1 */
	case 48 : return "vgr2"  ; /* Voyager 2 */
	case 49 : return "gal"   ; /* Galileo */
	case 50 : return "hubl"  ; /* Hubble ST */
	case 54 : return "met3"  ; /* METEOSAT 3 */
	case 55 : return "met4"  ; /* METEOSAT 4 */
	case 56 : return "met5"  ; /* METEOSAT 5 */
	case 60 : return "noa10" ; /* NOAA-10 */
	case 61 : return "noa11" ; /* NOAA-11 */
	case 62 : return "noa12" ; /* NOAA-12 */
	case 63 : return "noa13" ; /* NOAA-13 */
	case 64 : return "noa14" ; /* NOAA-14 */
	case 70 : return "GOES-8"; /* GOES-8 (Imager) */
	case 71 : return "g8sn"  ; /* GOES-8 (Sounder) */
	case 72 : return "GOES-9"; /* GOES-9 (Imager) */
	case 73 : return "g9sn"  ; /* GOES-9 (Sounder) */
	case 74 : return "GOES-10";/* GOES-10 (Imager) */
	case 75 : return "g10sn" ; /* GOES-10 (Sounder) */
	case 76 : return "GOES-11";/* GOES-11 (Imager) */
	case 77 : return "g11sn" ; /* GOES-11 (Sounder) */
	case 78 : return "GOES-12";/* GOES-12 (Imager) */
	case 79 : return "g12sn" ; /* GOES-12 (Sounder) */
	case 80 : return "erbe"  ; /* ERBE */
	case 83 : return "gmsa"  ; /* GMSA PBB/Ingexs2*/
	case 87 : return "dms8"  ; /* DMSP F-8 */
	case 88 : return "dms9"  ; /* DMSP F-9 */
	case 89 : return "dms10" ; /* DMSP F-10 */
	case 90 : return "dms11" ; /* DMSP F-11 */
	case 91 : return "dms12" ; /* DMSP F-12 */
	case 95 : return "fy1b"  ; /* FY-1B */
	case 96 : return "fy1c"  ; /* FY-1C */
	case 97 : return "fy1d"  ; /* FY-1D */
	case 180 : return "GOES-13";/* GOES-13 (Imager) */
	case 181 : return "g13sn" ; /* GOES-13 (Sounder) */
	case 182 : return "GOES-14";/* GOES-14 (Imager) */
	case 183 : return "g14sn" ; /* GOES-14 (Sounder) */
	case 184 : return "GOES-15";/* GOES-15 (Imager) */
	case 185 : return "g15sn" ; /* GOES-15 (Sounder) */
	case 186 : return "GOES-16";/* GOES-16 (Imager) */
	case 187 : return "g16sn" ; /* GOES-16 (Sounder) */
	}
        return (char *) key;
}


char *
create_filename(dir)
struct area_dir *dir;
{
    static char filename[27];  /* return */
    int year,yearday,second,minute,hour;
    int month,day,count;
    void month_day();
    char yearstr[5],monthstr[3],daystr[3],hourstr[3], minutestr[3] ;
    char image_type[10],extension[5],NUM[3];
    char *type ;
    
    (void)memset(filename,0,20) ;

    yearday = dir->ndate;
    year = yearday/1000;
    yearday -= 1000*year;
    if (year < 57) year += 2000;
    else year += 1900; 

    month_day(year,yearday,&month,&day);
/*    if (year > 1999)
      year -= 2000;
    else
      year -= 1900;*/
    second = dir->ntime;
    minute = second/10000;
    hour = minute;
    second -= 10000*minute;
    minute = second/100;
    second -= 100*minute;

    (void)sprintf(yearstr,"%04d",year);
    (void)sprintf(monthstr,"%02d",month);
    (void)sprintf(daystr,"%02d",day);
    (void)sprintf(hourstr,"%02d",hour);
    (void)sprintf(minutestr,"%02d",minute);

    

   type = gettype(dir->satid);
   extension[0] = '\0';
   sprintf(extension,"unk\0");

      /* create file name */
	image_type[0] = '\0';
      switch(dir->satid)
         {
         case 70:
	 case 72:
	 case 74:
	 case 76:
 	 case 78:
 	 case 180:
 	 case 182:
 	 case 184:
 	 case 186:
            if(dir->satid == 70) sprintf(NUM,"8\0");  
            if(dir->satid == 72) sprintf(NUM,"9\0");
            if(dir->satid == 74) sprintf(NUM,"10\0");
            if(dir->satid == 76) sprintf(NUM,"11\0");
            if(dir->satid == 78) sprintf(NUM,"12\0"); 
            if(dir->satid == 180) sprintf(NUM,"13\0"); 
            if(dir->satid == 182) sprintf(NUM,"14\0"); 
            if(dir->satid == 184) sprintf(NUM,"15\0"); 
            if(dir->satid == 186) sprintf(NUM,"16\0"); 
	    switch (dir->filtmap)
               {
               case 1:
                  sprintf(image_type,"VIS\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g%sv\0",NUM);
                  break;
               case 2:
                  sprintf(image_type,"3.9\0");
                  break;
               case 4:
                  sprintf(image_type,"WV\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g%sw\0",NUM);
                  break;
               case 8:
                  sprintf(image_type,"IR\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g%si\0",NUM);
                  break;
               case 16:
                  sprintf(image_type,"12.0\0");
                  break;
               case 32:
                  sprintf(image_type,"13.3\0");
                  break;
	       default:
		  sprintf(image_type,"UNK\0");
               }
            break;
         case 32:
            sprintf(image_type,"VIS\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g7v\0");
	    break;
         case 33:
	    switch (dir->filtmap)
               {
               case 128:
                  sprintf(image_type,"IR\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g7i\0");
                  break;
               case 512:
                  sprintf(image_type,"WV\0");
   		  extension[0] = '\0';
		  sprintf(extension,"g7w\0");
                  break;
	       default:
		  sprintf(image_type,"UNK\0");
               }
            break;
	 case 10:
	    switch (dir->filtmap)
               {
               case 1:
                  sprintf(image_type,"G8COMP\0");
                  break;
               case 8:
                  sprintf(image_type,"G9COMP\0");
                  break;
               default:
                  sprintf(image_type,"UNK\0");
               }
            break;
	 default:
	    sprintf(image_type,"UNK\0");
         }
      
      if(ymdh == 1) 
         {
	 sprintf(filename,"%s%s%s%s.%s",yearstr,monthstr,
            daystr,hourstr,extension);
         }
      else
         {   
      (void)sprintf(filename,"%s_%s%s%s_%s%s %s",image_type,
	yearstr,monthstr,daystr,hourstr,minutestr,type);
         }

    return filename ;
}

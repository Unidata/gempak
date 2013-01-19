#include "geminc.h"
#include "gemprm.h"

#include "dccmn.h"

#include "dcuspln.h"
/* James/Unidata        11/08   * Added support for both old and new format
 * 
 * The old format reports multiplicity while the new format reports 
 * each individual stroke along with error ellipse axes and orientation
 *  
 *  OLD FORMAT
 *  
 *  DATE                LAT        LON         SIG   MULT
 *  2008-08-28T17:04:24,40.4041967,-98.5941782,-16.7,1
 *  
 *  SIG  = Signal strength and polarity (+/-)
 *  MULT = Multiplicity of stroke
 *  
 * 
 *  NEW FORMAT
 * 
 *  DATE                    LAT        LON         AMP   E_MJ E_MI OR
 *  2008-03-15T10:00:13.641,35.4391761,-84.2560386,-23.4,0.25,0.25,79
 *
 *  AMP  = Peak amplitude of event with signal polarity
 *  E_MJ = Error ellipse major axis (km) 
 *  E_MI = Error ellipse minor axis (km)
 *  OR   = Orientation of major axis 
 */

void decode_strike(char *line, nldn_file ltgf)
{
static int isinit=!0;
int iret;
char result[256], *cpos;
int maxchar=sizeof(result)-1;
int year, month, day, hour, minute, second;
int mult;
float slat,slon,sgnl,emaj,emin;
nldn_flash flashdat;
time_t obsclock;
struct tm obstime;
int loglev, numerr,axisor;
static char errgrp[] = {"decode_strike"};
char errstr[LLMXLN];
int filver;

/* Split line into result (date string) and cpos (leftovers) */
cpos = cst_split(line,',',maxchar,result,&iret);

/* See if this look like a valid line (check if first char is 2[008] */
if ( ( iret != 0 ) || ( cpos == NULL ) || ( result[0] != '2' ) ) {
   printf("Not a strike line %s\n",line);
   return;
}

/* Check if file is old (filver = 4) or new (filver = 6)  */
filver = sscanf(cpos,"%f,%f,%f,%f,%f,%d", &slat, &slon, &sgnl, &emaj, &emin, &axisor);
switch ( filver ) {
   case 4:
      if ( (iret = sscanf(cpos,"%f,%f,%f,%d", &slat, &slon, &sgnl, &mult)) != 4 ) {
         sprintf(errstr,"Old: Not a strike line %s\0",line);
         loglev = 4;
         numerr = 1;
         dc_wclg(loglev, errgrp, numerr, errstr, &iret);
         return;
      }
   break;
   case 6:
      if ( (iret = sscanf(cpos,"%f,%f,%f,%f,%f,%d", &slat, &slon, &sgnl, &emaj, &emin, &axisor)) != 6 ) {
         sprintf(errstr,"New: Not a strike line %s\0",line);
         loglev = 4;
         numerr = 1;
         dc_wclg(loglev, errgrp, numerr, errstr, &iret);
         return;
      }
   break;
}

/* Check date string is formatted correctly */
if ( (iret = sscanf(result,"%d-%d-%dT%d:%d:%d", &year, &month, &day, &hour, &minute, &second )) != 6 ) {
   printf("Unexpected date string %s\n",result);
   return;
}

/* Initialize time zone information */
if(isinit) {
   isinit = 0;
   putenv("TZ=UTC0");
   tzset();
}

/* Convert time to seconds*/
obstime.tm_sec = second;
obstime.tm_min = minute;
obstime.tm_hour = hour;
obstime.tm_mday = day;
obstime.tm_mon = month - 1;
obstime.tm_year = year - 1900; /* year since 1900 */
obstime.tm_wday = 0;
obstime.tm_yday = 0;
obstime.tm_isdst = 0;
obsclock = mktime ( &obstime );

/* The following are present regardless of file version */
flashdat.sec = (int)obsclock;
flashdat.nsec = 0;
flashdat.lat = slat;
flashdat.lon = slon;
flashdat.sgnl = sgnl;

switch ( filver ) {
   case 4:
      flashdat.mult = mult;
      flashdat.semimaj = RMISSD;
      flashdat.eccent = RMISSD;
      flashdat.angle = RMISSD;
   break;
   case 6:
      flashdat.mult = 1;
      flashdat.semimaj = emaj;
      flashdat.eccent = emin;
      flashdat.angle = axisor;
   break;
}
flashdat.chisqr = RMISSD;

(void)write_point(ltgf, flashdat, &iret);
if ( iret != 0 )
   printf("%d look line %s %f %f %f %d [%d %d %d]\n",iret, result,slat,slon,sgnl,mult,
	obstime.tm_year, obstime.tm_mon, obstime.tm_mday);

}

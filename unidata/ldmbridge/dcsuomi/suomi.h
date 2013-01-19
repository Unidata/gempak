/* SUOMINET data structure for FSL NetCDF files */

#ifndef __suomi__
#define __suomi__

#include <time.h>

typedef struct suomi_obs {
   time_t timeObs;
   int year,month,day,hour,minute,seconds;
   float  pres, tmpc, relh;
   float  duration, pwv, pwv_err;
   float  wet_delay, model_dry_delay, total_delay;
   float  pifact, final_dry_delay;
   int	  met_flag;
   struct suomi_obs *next;
   } suomi_obs;

typedef struct suomi_struct {
   char *stid;
   float Lat,Lon,Alt;
   struct suomi_obs *head;
   struct suomi_struct *next;
   } suomi_struct;


/* Prototypes */
void *decode_suomi(char *infilnam, long miss, int *iret);
void write_gempak( char *ofil, suomi_struct *head, char *logfname, int *iret);
void *decode_ncsuomi(int cdfid, long miss, int *iret);


#endif /* __suomi__ */

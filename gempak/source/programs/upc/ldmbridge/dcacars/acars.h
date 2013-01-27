/* acars data structure for FSL NetCDF files */

#ifndef __acars__
#define __acars__


typedef struct acars_struct {
   char *tailNumber;
   char *rptsta,*orig,*dest;
   float Lat,Lon,Alt;
   double timeObs;
   int year,month,day,hour,minute,seconds;
   float tmpc,dwpc,relh,mixr,sped,drct;
   float vacc,press;
   float mach, heading, medturb, maxturb;
   struct acars_struct *next;
   } acars_struct;

void write_gempak(char *ofil, acars_struct *head, char *logfname, int *iret);
acars_struct *decode_acars(char *infilnam, long miss, int *iret);
acars_struct *decode_ncacars(int cdfid, long miss, int *iret);

void VAPOR_PRES (float T, float *e);
void t_from_e (float e, float *t);

#endif /* __acars__ */

/* cosmic data structure for NetCDF files */

#ifndef __cosmic__
#define __cosmic__


typedef struct cosmic_struct {
   float Lat,Lon,Alt;
   double timeObs;
   int year,month,day,hour,minute,seconds;
   float tmpc,dwpc;
   float press;
   float refa, refo;
   struct cosmic_struct *next;
   } cosmic_struct;

void write_gempak(char *ofil, cosmic_struct *head, char *logfname, int *iret);
cosmic_struct *decode_cosmic(char *infilnam, long miss, int *iret);
cosmic_struct *decode_nccosmic(int cdfid, long miss, int *iret);

void VAPOR_PRES (float T, float *e);
void t_from_e (float e, float *t);

#endif /* __cosmic__ */

#include <stdio.h>


main(argc,argv)
int argc;
char *argv[];
{
char line[256];
int count;
float hour,slat,slon,pres,hght,tmpc;
float dwpc,sstc,sped,drct;
int ymdh;
char gemtime[12];
char filename[80];


FILE *f,*g,*h;

if(argc != 4)
   {
   printf("Usage: flghtgem ymdh infile flightid\n");
   exit(0);
   }

sscanf(argv[1],"%d",&ymdh);
if((f = fopen(argv[2],"r"))==0) /* open input flight data */
   {
   printf("File open failed\n");
   exit(-1);
   }

filename[0] = '\0';
sprintf(filename,"%s.gem\0",argv[3]);
g = fopen(filename,"w");
filename[0] = '\0';
sprintf(filename,"%s.tbl\0",argv[3]);
h =  fopen(filename,"w");

fprintf(g,"PARM = HOUR;PRES;HGHT;TMPC;DWPC;SSTC;SPED;DRCT\n\n");
fprintf(g,"STID     DATTIM         HOUR    PRES    HGHT    TMPC    DWPC    SSTC    SPED    DRCT\n");
count = 0;
while(fgets(line,255,f) != NULL)
   {
   sscanf(line,"%f %f %f %f %f %f %f %f %f %f",
      &hour,&slat,&slon,&pres,&hght,&tmpc,
      &dwpc,&sstc,&sped,&drct);
   gemtime[0] = '\0';
   sprintf(gemtime,"%06d/%02d00\0",ymdh/100,ymdh%100);
   fprintf(g,"F%07d %11s %7.4f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f\n",
      count,gemtime,hour,pres,hght,tmpc,dwpc,sstc,sped,drct);
   fprintf(h,"F%07d  99999 ASTEX FLIGHT TRACK               -- AZ %5.0f %6.0f %5.0f\n",
      count,slat*100,slon*100,hght);
   count = count + 1;
   }

fclose(f);
fclose(g);
fclose(h);

}
/* sample data line
HOUR      SLAT  SLON     PRES       HGHT     TMPC     DWPC     SSTC      SPED   DRCT
21.8083   36.97 -25.17   1013.72     0.41    17.49    11.84    18.61     0.00   360.00
21.8111   36.97 -25.17   1013.69     0.41    17.48    11.92    18.60     0.00   360.00
21.8139   36.97 -25.17   1013.72     0.41    17.50    11.89    18.57     0.00   360.00
21.8167   36.97 -25.17   1013.72     0.41    17.50    11.88    18.65     0.00   360.00
21.8194   36.97 -25.17   1013.72     0.41    17.51    11.90    18.81     0.00   360.00
21.8222   36.97 -25.17   1013.72     0.41    17.42    11.84    18.99     0.00   360.00
21.8250   36.97 -25.17   1013.75     0.41    17.26    11.80    22.57     0.00   360.00
21.8278   36.97 -25.17   1013.85     0.41    17.13    11.79    22.94     0.00   360.00
21.8306   36.97 -25.17   1013.85     0.41    17.25    11.79    22.66     0.00   360.00
21.8333   36.97 -25.17   1013.92     0.41    17.08    11.81    22.76     0.00   360.00
*/

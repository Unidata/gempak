#include <stdio.h>
#include <string.h>
#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define in_bdta	in_bdta_
#define gd_init	gd_init_
#define sfcopn	sfcopn_
#define sf_gtim	sf_gtim_
#endif

#define MAXLEN	72

void usage(char *argv0)
{
printf("Usage: %s filename [dattim]\n",argv0);
printf("Sfctime will display the times contained within a surface file.\n");
printf("File templates are supported. If dattim is supplied, the program will\n");
printf("outout the time if it exists in the file, otherwise, no output will occur\n");
}

int main(argc,argv)
int argc;
char *argv[];

{
int iflno,ifltyp;
int ier;
int maxtim,ntime,i;

char timlst[LLMXTM][17];
char argtime[17];

if(argc < 2) 
   {
   usage(argv[0]);
   exit(-2);
   }

if(argc == 3)
   {
   argtime[0] = '\0';
   strncat(argtime,argv[2],16);
   while(strlen(argtime) < 16) strncat(argtime," ",1);
   }

in_bdta(&ier);
gd_init(&ier);

sfcopn(argv[1],&iflno,&ifltyp,&ier,strlen(argv[1]));
if(ier != 0) exit(ier);

maxtim = LLMXTM;
sf_gtim (&iflno, &maxtim, &ntime, timlst, &ier,17 );

if(ier != 0) exit(ier);

for(i=0;i<ntime;i++)
   {
   timlst[i][16] = '\0';
   if(argc == 3)
      {
      if(strncmp(timlst[i],argtime,16) == 0) 
         {
         printf("%s\n",timlst[i]);
         exit(0);
         }
      }
   else
      printf("%s\n",timlst[i]);
   }

}

#include <stdio.h>

#ifdef UNDERSCORE
#define openrb	openrb_
#define openwb	openwb_
#define crdbfr	crdbfr_
#define cwrbfr	cwrbfr_
#define lenm	lenm_
#endif

int  crdbfr (int *bufr);
int  cwrbfr (int *bufr);
int  lenm   (int *mbay);
void openrb (char *ufile);
void openwb (char *ufile);

FILE *pb;

void openrb (char *ufile)
{
    pb = fopen( ufile , "rb" );
}

void openwb (char *ufile) 
{
    pb = fopen( ufile , "wb" );
}

int  crdbfr (int *bufr)
{
   int  nwrd; int  nb;

   nb = sizeof(bufr);
   if ( ( nwrd = fread(bufr,nb,8/nb,pb) ) != 0 ) {
      nwrd = lenm(bufr);
      fread(bufr+8/nb,nb,nwrd-8/nb,pb);
      return nwrd;
   }
   else {
      return -1;
   }
}

int  cwrbfr (int *bufr)
{
   int  nwrd; int  nb;

   nb = sizeof(bufr);
   nwrd = lenm(bufr);
   fwrite(bufr,nb,nwrd,pb);
   return nwrd;
}

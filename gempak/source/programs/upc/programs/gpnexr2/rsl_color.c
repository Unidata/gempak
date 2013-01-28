#include <stdio.h>

int main(int argc, char *argv[])
{
FILE *rp, *gp, *bp;
char fname[80];
int i, red, green, blue;

sprintf(fname,"red_%s.clr\0",argv[1]);
rp = fopen(fname,"r");

sprintf(fname,"grn_%s.clr\0",argv[1]);
gp = fopen(fname,"r");

sprintf(fname,"blu_%s.clr\0",argv[1]);
bp = fopen(fname,"r");

for(i=0;i<256;i++)
   {
   red = getc(rp); 
   green = getc(gp); 
   blue = getc(bp); 
   printf("look %d %d %d %d\n",i, red, green, blue);
   }

}


#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"



char *next_group(char *bultin, int len)
{
char *cpos;
int i=0;

cpos = bultin;

while((i < len)&&(cpos[i] != '/')) i++;

if(i < len)
   return(cpos+i);
else
   return(NULL);
}



int getstr(char *buf, char *outstr)
{
int len;

len = 0; outstr[0] = '\0';
while(buf[len] > ' ')
   {
   strncat(outstr,buf+len,1);
   len++;
   }

return(len);
}


int findchr(char *buf)
{
int i=0;
while(buf[i] == ' ') i++;
return(i);
}


int isrcm(char *bultin, int lenbul)
{
bultin[lenbul] = '\0';
if(strstr(bultin,"\nRCM") != 0)
   return(0);
else
   return(-1);
}


#define _START  	0
#define _SOH            1
#define _SOH_CR         2
#define _SOH_CR_CR      3
#define _INPROD         4
#define _CR             5
#define _CR_CR          6
#define _CR_CR_NL       7

int get_nextbull(int fp, char *bultin,int *lenbul)
{
int iret=-1;
int DONE=0;
char buf[1];
int state=_START;

*lenbul = 0;
while((DONE == 0)&&(read(fp,buf,1) > 0))
   {
   switch(state)
      {
      case _START:
         if(buf[0] == 1) state++;
         break;
      case _SOH:
         if(buf[0] == 13)
            state++;
         else
            state = _START;
         break;
      case _SOH_CR:
         if(buf[0] == 13)
            state++;
         else
            state = _START;
         break;
      case _SOH_CR_CR:
          if(buf[0] == 10)
             state++;
          else
             state = _START;
         break;
      case _CR:
         bultin[*lenbul] = buf[0];
         *lenbul += 1;
         if(buf[0] == 13)
            state++;
         else
            state = _INPROD;
         break;
      case _CR_CR:
         bultin[*lenbul] = buf[0];
         *lenbul += 1;
         if(buf[0] == 10)
            state++;
         else if(buf[0] != 13)
            state = _INPROD;
         break;
      case _CR_CR_NL:
         bultin[*lenbul] = buf[0];
         *lenbul += 1;
         if(buf[0] == 3)
            DONE = 1;
         else
            state = _INPROD;
         break;
      case _INPROD:
         bultin[*lenbul] = buf[0];
         *lenbul += 1;
         if(buf[0] == 13)
            state++;
         break;
      default:
         state = _START;
      }
   if(*lenbul > (MAXBUL-1) )
      {
      printf("buffer not big enough\n");
      DONE = 1;
      }
   }

if(DONE == 1) iret=0;

return(iret);
}



int get_nextggg(char *cpos, char *buf, int maxint, int *nextoff, int *iret)
{
int i=0;
int DONE=0;
int ilast=0;
char *spos;

buf[0] = '\0'; *iret = 0; *nextoff = 0;
/* get 3 letters */
strncat(buf,cpos,3);
spos = cpos + 3;

while(DONE == 0)
   {
   if(spos[0] != 0)
      {
      if(spos[0] == '/')
         {
         *iret = -1;
         return(i);
         }
      if(spos[0] == ',')
         DONE=1;
      else
         {
         if((spos[0] >= '0')&&(spos[0] <= '9'))
            {
            strncat(buf,spos,1);
            if(spos[0] > '0') i++;
            ilast = (spos[0] - '0');
            }
         else if((spos[0] >= 'A')&&(spos[0] <= 'Z'))
            {
            if(ilast > 0) i = i + (spos[0] - 'A' + 1);
            ilast = 0;
            strncat(buf,spos,1);
            }
         else if (spos[0] != ' ')
            printf("unexpected characater in nextggg %d\n",spos[0]);
         }
      spos++;
      }
   else
      DONE = 1;
   if(i == maxint)
      DONE = 1;
   }

*nextoff = spos - cpos;
return(i);
}

#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"


fd_set readfds;
fd_set exceptfds;

int bufread(fd,buf,bsiz, bread)
int fd;
char *buf;
int bsiz;
int *bread;
{
int width,ready;
unsigned long idle=0;
int binp;
static int TV_SEC=3;
struct timeval timeo;

*bread = 0;
width = fd + 1;

while(*bread < bsiz)
   {
   FD_ZERO(&readfds);
   FD_ZERO(&exceptfds);
   FD_SET(fd, &readfds);
   FD_SET(fd, &exceptfds);

   timeo.tv_sec = TV_SEC;
   timeo.tv_usec = 0;

   ready = select(width, &readfds, 0, &exceptfds, &timeo);

   if(ready <= 0)
      {
      idle += TV_SEC;
      if(idle > DCDFTM)
         {
	 return(-3);
         }
      continue;
      }

   if(FD_ISSET(fd, &readfds) ) /* || FD_ISSET(fd, &exceptfds)) */
      {
      idle = 0;
      binp = read(fd, buf + *bread,bsiz - *bread);
 
      if(binp <= 0)
         {
         printf("End of Input\n");
         return(-2);
         }

      *bread = *bread + binp;
      if(*bread < bsiz)
         {
         FD_CLR(fd, &readfds);
         FD_CLR(fd, &exceptfds);
         }
      }
   else
      {
      /*printf("select returned %d but fd not set\n",ready); not ready on fd */
      idle += TV_SEC;
      if(idle > DCDFTM)
	 return(-3);
      continue;
      }

   }

return(0);
}



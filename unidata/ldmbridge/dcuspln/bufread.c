#include "geminc.h"
#include "gemprm.h"

unsigned long idle = 0;

fd_set readfds;
fd_set exceptfds;

int DONE = 0;

int bufread (int fd, char *buf, int bsiz, int *blen)
{
  int width, ready;
  struct timeval timeo;
  size_t bread = 0;
  static int TV_SEC = 30;

  width = fd + 1;

  while (bread < bsiz)
    {
      timeo.tv_sec = TV_SEC;
      timeo.tv_usec = 0;

      FD_ZERO (&readfds);
      FD_ZERO (&exceptfds);
      FD_SET (fd, &readfds);
      FD_SET (fd, &exceptfds);

      ready = select (width, &readfds, 0, &exceptfds, &timeo);
      /* timeo may be modified, don't rely on value now */

      if (ready < 0)
        {
          if (errno != EINTR)
            printf(/*serror (*/"select\n");
          else
            printf ("select received interupt\n");

          errno = 0;
          continue;
        }


      if (ready == 0)
        {
          idle += TV_SEC;
          if (idle > 600)
            {
              /*if (ulogIsVerbose ())
                uinfo ("Idle for 600 seconds");*/
              idle = 0;
	      return(-2);
            }
          continue;             
        }

      if (FD_ISSET (fd, &readfds) || FD_ISSET (fd, &exceptfds))
        {
          size_t nread;

          idle = 0;
          nread = read (fd, buf + bread, (size_t) bsiz - bread);
          bread = bread + nread;

          if (nread == 0)
            {
              /*if (ulogIsVerbose ())
                uinfo ("End of Input");*/
	      *blen = bread;
              if ( bread == 0 )
                 return(-2);
              else
                 return (0);
            }
          if (bread < (size_t) bsiz)
            {
              FD_CLR (fd, &readfds);
              FD_CLR (fd, &exceptfds);
            }
          else
	    {
	    *blen = bread;
            return (0);
            }

        }
      else
        {
          printf ("select returned %d but fd not set\n", ready);
          idle += TV_SEC;
	  continue;
        }
    }
  *blen = bread;
  return (0);
}

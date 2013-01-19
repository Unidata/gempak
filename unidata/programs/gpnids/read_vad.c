#include <geminc.h>
#include <gemprm.h>

#ifdef UNDERSCORE
#define vad vad_
#endif

#define VAD_ID 48

void
vad (char *filnam, int *iret)
{
  int i, nread, ier;
  int nline;
  unsigned short sbytes;
  FILE *fp;
  static char defdir[] = "";
  unsigned char hbuf[32];

  *iret = 0;

  fp = cfl_ropn (filnam, defdir, &ier);
/*
** See if this is compressed, or has a header to skip over
 */
  if (ier != 0)
    {
      printf ("could not open file %s\n", filnam);
      *iret = -1;
      return;
    }

  nread = fread (hbuf, 1, 32, fp);
  cfl_clos (fp, &ier);

  if (nread < 32)
    {
      printf ("could not read 32 bytes\n");
      *iret = -2;
      return;
    }

  if ((hbuf[0] == 1) && (hbuf[1] == 13) && (hbuf[2] == 13) && (hbuf[3] == 10))
    {
      /* found a FOS header....assume 4 lines before data starts */
      nline = 4;
      vad_head (filnam, &nline, &ier);
    }
  else if ((hbuf[0] == 'S') && (hbuf[1] == 'D') && (hbuf[2] == 'U')
	   && (hbuf[3] == 'S'))
    {
      /* found a WMO header...assume 2 lines before data starts */
      nline = 2;
      vad_head (filnam, &nline, &ier);
    }
  else if (((sbytes = (unsigned short) read_short (hbuf)) ==
	    (sbytes = (unsigned short) read_short (hbuf + 30))) &&
	   (sbytes == VAD_ID))
    {
      /* found a vad, no header */
      nline = 0;
      vad_head (filnam, &nline, &ier);
    }

}

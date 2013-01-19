#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define im_nids_text_output im_nids_text_output_
#endif

extern int
read_int (unsigned char *);

extern short 
read_short(unsigned char *);

void
im_nids_tabular (unsigned char *buf)
{
  int ier;
  int i, k, icnt, nchar;
  int blen, boff, ipag, lpag, npag;
  int packet_code, cval;
  char *cstr;

  if ((int) read_short (buf) != -1)
    {
      printf ("block divider not found\n");
      return;
    }

  if ((int) read_short (buf + 2) != 3)
    {
      printf ("tabular block '3' not found\n");
      return;
    }

  blen = read_int (buf + 4);
#ifdef DEBUG
  printf ("tabular block length %d\n", blen);


  k = 0;
  for (i = 0; i < 9; i++)
    {
      cval = (int) read_short (buf + 8 + k);
      k += 2;
      printf ("second header %d %d\n", i + 1, cval);
    }

  for (i = 0; i < 51; i++)
    {
      cval = (int) read_short (buf + 8 + k);
      k += 2;
      printf ("second pdb %d %d\n", i + 10, cval);
    }

  icnt = (int) read_int (buf + 8 + 108);
  printf ("look icnt %d k=%d\n", icnt, k);
#endif

  k = 120;			/* past the 10 + 50 halfword header */
  boff = k + 8;
#ifdef DEBUG			/* -1 block separator */
  printf ("look block %d\n", (int) read_short (buf + boff));
#endif

  npag = (int) read_short (buf + boff + 2);

  ipag = 0;
  icnt = 4;
  while ((ipag < npag) && (boff + icnt < blen))
    {
      nchar = (int) read_short (buf + boff + icnt);

      if (nchar == -1) 
	{
	  ipag++;
	  icnt = icnt + 2;
#ifdef DEBUG
	  printf ("page separator %d %d\n", boff + icnt, blen);
#endif
	  continue;
	}

#ifdef DEBUG
      printf ("page %d/%d nchar %d total %d len %d\n", ipag, npag, nchar, boff + icnt+2+nchar,blen);

      for (k = 0; k < nchar; k++)
	{
	if ( isprint( buf[boff + icnt + 2 + k]) )
	   printf ("%c", buf[boff + icnt + 2 + k]);
        else
	   printf ("@");
        }

      printf ("\n");

#endif

      cstr = (char *) (buf + boff + icnt + 2);
      im_nids_text_output (cstr, nchar);

      icnt = icnt + nchar + 2;
    }
}

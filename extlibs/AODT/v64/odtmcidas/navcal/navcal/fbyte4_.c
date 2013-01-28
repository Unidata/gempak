/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: fbyte4_.c,v 1.1 2000/07/12 13:12:23 gad Exp $ ****/
/* GAD moved over from mcidas for windco cal 7/6/00 */

#include "mcidas.h"

   /* buffer - INTEGER*4 array to switch bytes  */
   /* n      - INTEGER number of 4 byte switches */

void
fbyte4_(void *buffer, Fint *num)
{
  char *cbuf = (char *)buffer;
  int i, n;

  n = *num;
  for (i = 0; i < n; i++)
  {
    char b;

    b = cbuf[0];
    cbuf[0] = cbuf[3];
    cbuf[3] = b;
    b = cbuf[1];
    cbuf[1] = cbuf[2];
    cbuf[2] = b;
    cbuf += 4;
  }
}

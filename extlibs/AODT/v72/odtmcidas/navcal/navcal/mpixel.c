/**** $Id: mpixel.c,v 1.2 2000/06/28 21:24:58 gad Exp $ ****/

#include "mcidas.h"

static void     crak14(long, unsigned char *, unsigned long *);
static void     crak24(long, unsigned short *, unsigned long *);
static void     crak12(long, unsigned char *, unsigned short *);
static void     pack21(long, unsigned short *, unsigned char *);
static void     pack41(long, unsigned long *, unsigned char *);
static void     pack42(long, unsigned long *, unsigned short *);

/*
*$ Name:
*$      mpixel - Moves 1,2, or 4 byte data into 1,2, or 4 byte fields.
*$               The data is moved within one array, not from one array
*$               to another.
*$
*$ Interface:
*$      subroutine
*$      mpixel (integer n, integer isou, integer ides, character ibuf)
*$
*$ Input:
*$      n       - Number of transfers to be made.
*$      isou    - Byte size of the source fields in the buffer.
*$      ides    - Byte size of the destination fields in the buffer.
*$
*$ Input and Output:
*$      ibuf    - Array containing byte fields to be transferred.
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      utility
*/

void mpixel (long n, long isou, long ides, void *buf)
{
	long            slen = isou;
	long            dlen = ides;

	if (slen == dlen)
		return;

	if (slen == 1 && dlen == 4)
		crak14(n, buf, buf);
	else if (slen == 2 && dlen == 4)
		crak24(n, buf, buf);
	else if (slen == 1 && dlen == 2)
		crak12(n, buf, buf);
	else if (slen == 2 && dlen == 1)
		pack21(n, buf, buf);
	else if (slen == 4 && dlen == 1)
		pack41(n, buf, buf);
	else if (slen == 4 && dlen == 2)
		pack42(n, buf, buf);
}

static void
crak14(long num, unsigned char *inbuf, unsigned long *outbuf)
{
	long            i;

	outbuf += num - 1;
	inbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{

		*outbuf = *inbuf;
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
crak24(long num, unsigned short *inbuf, unsigned long *outbuf)
{
	long            i;

	outbuf += num - 1;
	inbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *inbuf;
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
crak12(long num, unsigned char *inbuf, unsigned short *outbuf)
{
	long            i;

	outbuf += num - 1;
	inbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *inbuf;
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
pack21(long num, unsigned short *inbuf, unsigned char *outbuf)
{
	long            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *inbuf;
		outbuf += 1;
		inbuf += 1;
	}
}

static void
pack41(long num, unsigned long *inbuf, unsigned char *outbuf)
{
	long            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *inbuf;
		outbuf += 1;
		inbuf += 1;
	}
}

static void
pack42(long num, unsigned long *inbuf, unsigned short *outbuf)
{
	long            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *inbuf;
		outbuf += 1;
		inbuf += 1;
	}
}

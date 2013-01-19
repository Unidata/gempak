/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: mgvatb_.c,v 1.1 2001/04/16 20:59:06 daves Exp $ ****/

#include "mcidas.h"

static void     pkta21g(Fint, Mcuint2 *, unsigned char *, Fint *);
static void     trta22g(Fint, Mcuint2 *, Mcint2 *, Fint *);
static void     ckta24g(Fint, Mcuint2 *, Mcint4 *, Fint *);
static void     pkta41g(Fint, Mcuint4 *, unsigned char *, Fint *);
static void     pkta42g(Fint, Mcuint4 *, Mcint2 *, Fint *);
static void     trta44g(Fint, Mcuint4 *, Mcint4 *, Fint *);

/*
 * mgvatb - this routine is called by kbxgvar.dlm to crack/pack pixels
 *          in place, while passing it through a lookup table.
 *   n    : number of pixels to crack/pack
 *   isou : packing of array on input (1, 2, 4) bytes/pixel
 *   ides : packing of array on output
 *   buf  : array of packed pixels
 *   itab : 4 byte lookup table array
 */

void
mgvatb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[])
{
	Fint            slen = *isou;
	Fint            dlen = *ides;
	Fint            num = *n;

	if (slen == 2 && dlen == 1)
		pkta21g(num, (Mcuint2 *) buf, buf, itab);
	else if (slen == 2 && dlen == 2)
		trta22g(num, (Mcuint2 *) buf, buf, itab);
	else if (slen == 2 && dlen == 4)
		ckta24g(num, (Mcuint2 *) buf, buf, itab);
	else if (slen == 4 && dlen == 1)
		pkta41g(num, (Mcuint4 *) buf, buf, itab);
	else if (slen == 4 && dlen == 2)
		pkta42g(num, (Mcuint4 *) buf, buf, itab);
	else if (slen == 4 && dlen == 4)
		trta44g(num, (Mcuint4 *) buf, buf, itab);
}

/* Crack 2 bytes -> 4 bytes */

static void
ckta24g(Fint num, Mcuint2 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;

	inbuf += num - 1;
	outbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *(itab + (*inbuf >> 1));
		outbuf -= 1;
		inbuf -= 1;
	}
}

/* Pack 2 bytes -> 1 byte */

static void
pkta21g(Fint num, Mcuint2 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + (*inbuf >> 1));
		outbuf += 1;
		inbuf += 1;
	}
}

/* Pack 4 bytes -> 1 byte */

static void
pkta41g(Fint num, Mcuint4 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + (*inbuf >> 1));
		outbuf += 1;
		inbuf += 1;
	}
}

/* Pack 4 bytes -> 2 bytes */

static void
pkta42g(Fint num, Mcuint4 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + ((*inbuf) >> 1));
		outbuf += 1;
		inbuf += 1;
	}
}

/* Transfer 2 bytes -> 2 bytes */

static void
trta22g(Fint num, Mcuint2 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + (*inbuf >> 1));
		outbuf += 1;
		inbuf += 1;
	}
}

/* Transfer 4 bytes -> 4 bytes */

static void
trta44g(Fint num, Mcuint4 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + (*inbuf >> 1));
		outbuf += 1;
		inbuf += 1;
	}
}

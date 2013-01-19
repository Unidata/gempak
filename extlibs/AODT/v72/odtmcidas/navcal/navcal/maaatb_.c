/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: maaatb_.c,v 1.13 1997/10/10 20:17:54 dglo Exp $ ****/

#include "mcidas.h"

static void     pkta21(Fint, Mcint2 *, unsigned char *, Fint *);
static void     trta22(Fint, Mcint2 *, Mcint2 *, Fint *);
static void     ckta24(Fint, Mcint2 *, Mcint4 *, Fint *);
static void     pkta41(Fint, Mcint4 *, unsigned char *, Fint *);
static void     pkta42(Fint, Mcint4 *, Mcint2 *, Fint *);
static void     trta44(Fint, Mcint4 *, Mcint4 *, Fint *);

void
maaatb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[])
{
	Fint            slen = *isou;
	Fint            dlen = *ides;

	if (slen == 2 && dlen == 1)
		pkta21(*n, buf, buf, itab);
	else if (slen == 2 && dlen == 2)
		trta22(*n, buf, buf, itab);
	else if (slen == 2 && dlen == 4)
		ckta24(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 1)
		pkta41(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 2)
		pkta42(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 4)
		trta44(*n, buf, buf, itab);
}

static void
ckta24(Fint num, Mcint2 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;
	Mcint2          val;

	inbuf += num - 1;
	outbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32760)
			val = 32760;
		*outbuf = *(itab + (val >> 3));
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
pkta21(Fint num, Mcint2 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;
	Mcint2          val;

	for (i = 0; i < num; i++)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32760)
			val = 32760;
		*outbuf = *(itab + (val >> 3));
		outbuf += 1;
		inbuf += 1;
	}
}

static void
pkta41(Fint num, Mcint4 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;
	Mcint4          val;

	for (i = 0; i < num; i++)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32670)
			val = 32670;
		*outbuf = *(itab + (val >> 3));
		outbuf += 1;
		inbuf += 1;
	}
}

static void
pkta42(Fint num, Mcint4 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;
	Mcint4          val;

	for (i = 0; i < num; i++)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32670)
			val = 32670;
		*outbuf = *(itab + ((*inbuf) >> 3));
		outbuf += 1;
		inbuf += 1;
	}
}

static void
trta22(Fint num, Mcint2 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;
	Mcint2          val;

	for (i = 0; i < num; i++)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32670)
			val = 32670;
		*outbuf = *(itab + (val >> 3));
		outbuf += 1;
		inbuf += 1;
	}
}

static void
trta44(Fint num, Mcint4 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;
	Mcint4          val;

	for (i = 0; i < num; i++)
	{
		val = *inbuf;
		if (val < 0)
			val = 0;
		else if (val > 32670)
			val = 32670;
		*outbuf = *(itab + (val >> 3));
		outbuf += 1;
		inbuf += 1;
	}
}

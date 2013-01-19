/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: mpixtb_.c,v 1.1 2000/07/12 13:12:26 gad Exp $ ****/

#include "mcidas.h"
/*
*$ Name:
*$      mpixtb  - Expands/packs data values with a lookup table.
*$
*$ Interface:
*$      subroutine
*$      mpixtb(integer n, integer isou, integer ides, integer buffer(*),
*$             integer itab(*))
*$
*$ Input:
*$      n       - Number of data values to move.
*$      isou    - Source data length in bytes.
*$      ides    - Destination data length in bytes.
*$      itab    - Lookup table array.
*$
*$ Input and Output:
*$      buffer  - Array of data values.
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      The data length in bytes can be 1, 2, or 4 bytes.
*$
*$      If the data length is going to increase from the source to the
*$      destination, make sure that the array is large enough to hold
*$      the data values.
*$
*$ Categories:
*$      calibration
*/

static void     trt11(Fint, unsigned char *, unsigned char *, Fint *);
static void     ckt12(Fint, unsigned char *, Mcint2 *, Fint *);
static void     ckt14(Fint, unsigned char *, Mcint4 *, Fint *);
static void     pkt21(Fint, Mcuint2 *, unsigned char *, Fint *);
static void     trt22(Fint, Mcuint2 *, Mcint2 *, Fint *);
static void     ckt24(Fint, Mcuint2 *, Mcint4 *, Fint *);
static void     pkt41(Fint, Mcuint4 *, unsigned char *, Fint *);
static void     pkt42(Fint, Mcuint4 *, Mcint2 *, Fint *);
static void     trt44(Fint, Mcuint4 *, Mcint4 *, Fint *);

void
mpixtb_(Fint *n, Fint *isou, Fint *ides, void *buf, Fint itab[])
{
	Fint            slen = *isou;
	Fint            dlen = *ides;

	if (slen == 1 && dlen == 1)
		trt11(*n, buf, buf, itab);
	else if (slen == 1 && dlen == 2)
		ckt12(*n, buf, buf, itab);
	else if (slen == 1 && dlen == 4)
		ckt14(*n, buf, buf, itab);
	else if (slen == 2 && dlen == 1)
		pkt21(*n, buf, buf, itab);
	else if (slen == 2 && dlen == 2)
		trt22(*n, buf, buf, itab);
	else if (slen == 2 && dlen == 4)
		ckt24(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 1)
		pkt41(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 2)
		pkt42(*n, buf, buf, itab);
	else if (slen == 4 && dlen == 4)
		trt44(*n, buf, buf, itab);
}

static void
trt11(Fint num, unsigned char *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}

static void
ckt12(Fint num, unsigned char *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;

	inbuf += num - 1;
	outbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *(itab + *inbuf);
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
ckt14(Fint num, unsigned char *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;

	inbuf += num - 1;
	outbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *(itab + *inbuf);
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
pkt21(Fint num, Mcuint2 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}

static void
trt22(Fint num, Mcuint2 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}

static void
ckt24(Fint num, Mcuint2 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;

	inbuf += num - 1;
	outbuf += num - 1;
	for (i = num - 1; i >= 0; i--)
	{
		*outbuf = *(itab + *inbuf);
		outbuf -= 1;
		inbuf -= 1;
	}
}

static void
pkt41(Fint num, Mcuint4 *inbuf, unsigned char *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}

static void
pkt42(Fint num, Mcuint4 *inbuf, Mcint2 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}

static void
trt44(Fint num, Mcuint4 *inbuf, Mcint4 *outbuf, Fint *itab)
{
	Fint            i;

	for (i = 0; i < num; i++)
	{
		*outbuf = *(itab + *inbuf);
		outbuf += 1;
		inbuf += 1;
	}
}


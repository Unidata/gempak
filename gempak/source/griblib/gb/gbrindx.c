#include "gbcmn.h"

void gb_rindx ( FILE *fp, int *iret )
/************************************************************************
 * gb_rindx								*
 *									*
 * This function reads a GRIB INDEX file into the index file structure.	*
 *									*
 * gb_rindx ( fp, iret )						*
 *									*
 * Input parameters:							*
 *	*fp		FILE		File pointer to GRIB INDEX file	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/96	Created					*
 * M. Linda/GSC		10/97	Added the prologue			*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
int	nbytes, hdrlen, indxlen, ninrec, indx;
unsigned char	buf[256];
char	*tmphdr;

	infile.nmsgs  = 0;
	infile.curmsg = 0;

	cfl_read ( fp, 81, infile.hdr1, &nbytes, iret );
	infile.hdr1[nbytes] = CHNULL;

	cfl_read ( fp, 81, infile.hdr2, &nbytes, iret );
	infile.hdr2[nbytes] = CHNULL;

	tmphdr = (char *) infile.hdr2;

	sscanf( &tmphdr[8], "%d%d%d%s",
		&hdrlen, &indxlen, &ninrec, infile.basename );

	cfl_seek ( fp, hdrlen, SEEK_SET, iret );

	while ( *iret == 0  &&  infile.nmsgs <= MMHDRS ) {

	    cfl_read ( fp, indxlen, buf, &nbytes, iret );

	    if ( *iret == 0 ) {
		indx = 0;
		infile.msg[infile.nmsgs].position= gb_btoi(buf, indx, 4, FALSE);
		indx += 4;
		infile.msg[infile.nmsgs].pdsrpos = gb_btoi(buf, indx, 4, FALSE);
		indx += 4;
		infile.msg[infile.nmsgs].gdsrpos = gb_btoi(buf, indx, 4, FALSE);
		if ( infile.msg[infile.nmsgs].gdsrpos == 0 )
			infile.msg[infile.nmsgs].gdsrpos = IMISSD;
		indx += 4;
		infile.msg[infile.nmsgs].bmsrpos = gb_btoi(buf, indx, 4, FALSE);
		if ( infile.msg[infile.nmsgs].bmsrpos == 0 )
			infile.msg[infile.nmsgs].bmsrpos = IMISSD;
		indx += 4;
		infile.msg[infile.nmsgs].bdsrpos = gb_btoi(buf, indx, 4, FALSE);
		indx += 4;
		infile.msg[infile.nmsgs].length  = gb_btoi(buf, indx, 4, FALSE);
		indx += 4;
		infile.msg[infile.nmsgs].version = gb_btoi(buf, indx, 1, FALSE);
		memcpy ( infile.msg[infile.nmsgs].pds,    &buf[ 25], 28 );
		memcpy ( infile.msg[infile.nmsgs].gds,    &buf[ 53], 42 );
		memcpy ( infile.msg[infile.nmsgs].bms,    &buf[ 95],  6 );
		memcpy ( infile.msg[infile.nmsgs].bds,    &buf[101], 11 );
		memcpy ( infile.msg[infile.nmsgs].pdsext, &buf[112], 40 );
		infile.nmsgs++;
		if ( infile.nmsgs > MMHDRS )
		    printf("Number of GRIB messages exceeds maximum (%d)\n",
			    MMHDRS );
	    }

	}

}

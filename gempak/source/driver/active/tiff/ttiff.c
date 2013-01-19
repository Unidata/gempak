#include "tiffcmn.h"

void ttiff ( int *nbyte, int *iret )
/************************************************************************
 * ttiff								*
 *									*
 * This function puts the TIFF header and Tag information around the	*
 * compressed data in the output array.					*
 *									*
 * ttiff ( nbyte, iret )						*
 *									*
 * Output parameters:							*
 *	*nbyte		int		Number of bytes in the		*
 *					   output array			*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99						*
 * S. Jacobs/NCEP	 9/00	Added tag info for uncompressed data	*
 * S. Jacobs/NCEP	 2/01	Added check for LINUX machine type	*
 * S. Jacobs/NCEP	10/01	Fixed NULL fill of software title string*
 ***********************************************************************/
{

	int		kpos, itag, lent, i, j, ishft, ier;
	int		ii, jbytes, ndtbyt, noffset, nbytcnt;
	char		vers[41], title[41];

	Tfilhdr		filhdr;
	Tnumtags	ntag;
	Ttag		tags[MMTAGS];
	Tdata		data;
	Tstrip		strpinfo;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Start the data at byte 8 in the output array.
 */
	kpos = 8;

	if  ( ktype == 0 )  {
/*
 *	    Compress the image using the Group 4 Fax format.
 */
	    tg4comp ( &kpos, &ier );
	}
	else {
/*
 *	    Write the uncompressed data to the output file.
 */
 	    tuncomp ( &kpos, &ier );
	}

/*
 *	Save the number of data bytes for later use.
 */
 	ndtbyt = kpos - 8;

/*
 *	Set the number of TIFF tags.
 */
	ntag.num = MMTAGS;

/*
 *	Set the values of the file header.
 *	Check for Big and Little Endian.
 */
	if  ( MTMACH == MTULTX ||
	      MTMACH == MTALPH ||
	      MTMACH == MTLNUX )  {
	    filhdr.thdr.ident  = ( 'I' * 256 ) + 'I';
	    ishft = 1;
	}
	else {
	    filhdr.thdr.ident  = ( 'M' * 256 ) + 'M';
	    ishft = TFSHFT;
	}

	filhdr.thdr.ivers  = 42;
	filhdr.thdr.ifdloc = kpos;

	for ( i = 0; i < 8; i++ )  {
	    group4[i] = filhdr.bytes[i];
	}

/*
 *	Image width
 */
	itag = 0;
	tags[itag].tinfo.icode = 0x0100;
	if  ( kbit < MXSHRT )  {
	    tags[itag].tinfo.itype  = 3;
	    tags[itag].tinfo.ivalue = kbit * ishft;
	}
	else {
	    tags[itag].tinfo.itype  = 4;
	    tags[itag].tinfo.ivalue = kbit;
	}
	tags[itag].tinfo.nitem = 1;

/*
 *	Image length
 */
	itag++;
	tags[itag].tinfo.icode = 0x0101;
	if  ( klin < MXSHRT )  {
	    tags[itag].tinfo.itype  = 3;
	    tags[itag].tinfo.ivalue = klin * ishft;
	}
	else {
	    tags[itag].tinfo.itype  = 4;
	    tags[itag].tinfo.ivalue = klin;
	}
	tags[itag].tinfo.nitem = 1;

/*
 *	Bits per sample
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0102;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;

	if  ( ktype == 0 )  {
	    tags[itag].tinfo.ivalue = 1 * ishft ;
	}
	else {
	    tags[itag].tinfo.ivalue = 4 * ishft ;
	}

/*
 *	Compression
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0103;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;

	if  ( ktype == 0 )  {
/*
 *	    Group 4 Fax
 */
	    tags[itag].tinfo.ivalue = 4 * ishft ;
	}
	else {
/*
 *	    None
 */
	    tags[itag].tinfo.ivalue = 1 * ishft ;
	}

/*
 *	Photometric interpretation
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0106;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;

	if  ( ktype == 0 )  {
/*
 *	    White is Zero
 */
	    tags[itag].tinfo.ivalue = 0 * ishft ;
	}
	else {
/*
 *	    Black is Zero
 */
	    tags[itag].tinfo.ivalue = 1 * ishft ;
	}

/*
 *	Fill Order (Left to right)
 */
	itag++;
	tags[itag].tinfo.icode  = 0x010a;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = 1 * ishft ;

/*
 *	Strip offsets
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0111;
	tags[itag].tinfo.itype  = 4;

	if  ( ktype == 0 )  {
	    tags[itag].tinfo.nitem  = 1;
	    tags[itag].tinfo.ivalue = 8;
	    noffset = 0;
	}
	else {
	    tags[itag].tinfo.nitem  = klin;
	    tags[itag].tinfo.ivalue = kpos + 2 + MMTAGS*12 + 4;
	    noffset = 4 * klin;
	}

/*
 *	Orientation (0,0 is top,left)
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0112;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = 1 * ishft;

/*
 *	Samples per pixel
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0115;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = 1 * ishft;

/*
 *	Rows per strip
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0116;
	tags[itag].tinfo.nitem  = 1;

	if  ( ktype == 0 )  {
	    tags[itag].tinfo.itype  = 4;
	    tags[itag].tinfo.ivalue = MXINT;
	}
	else {
	    tags[itag].tinfo.itype  = 3;
	    tags[itag].tinfo.ivalue = 1 * ishft;
	}

/*
 *	Strip byte count
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0117;
	tags[itag].tinfo.itype  = 4;

	if  ( ktype == 0 )  {
	    tags[itag].tinfo.nitem  = 1;
	    tags[itag].tinfo.ivalue = ndtbyt;
	    nbytcnt = 0;
	}
	else {
	    tags[itag].tinfo.nitem  = klin;
	    tags[itag].tinfo.ivalue = kpos + 2 + MMTAGS*12 + 4 +
	    			      noffset;
	    nbytcnt = 4 * klin;
	}

/*
 *	X resolution (pixels/units)
 *
 *	The value is the location in the file of the data.
 *	kpos = the first byte of the tag info
 *	2    = length of the number of tags
 *	MMTAGS*12 = length of the tags
 *	4    = length of the end-of-tags flag
 */
	itag++;
	tags[itag].tinfo.icode  = 0x011a;
	tags[itag].tinfo.itype  = 5;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = kpos + 2 + MMTAGS*12 + 4 +
				  noffset + nbytcnt;

/*
 *	Y resolution (pixels/units)
 *
 *	The value is the location in the file of the data.
 *	kpos = the first byte of the tag info
 *	2    = length of the number of tags
 *	MMTAGS*12 = length of the tags
 *	4    = length of the end-of-tags flag
 *	8    = length of X res data
 */
	itag++;
	tags[itag].tinfo.icode  = 0x011b;
	tags[itag].tinfo.itype  = 5;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = kpos + 2 + MMTAGS*12 + 4 +
				  noffset + nbytcnt + 8;

/*
 *	Planar configuration ("chunky" format)
 */
	itag++;
	tags[itag].tinfo.icode  = 0x011c;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = 1 * ishft;

/*
 *	Resolution units (inches)
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0128;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 1;
	tags[itag].tinfo.ivalue = 2 * ishft;

/*
 *	Page number (1 of 1)
 */
	itag++;
	tags[itag].tinfo.icode  = 0x0129;
	tags[itag].tinfo.itype  = 3;
	tags[itag].tinfo.nitem  = 2;
	tags[itag].tinfo.ivalue = ( 1 * TFSHFT ) + 1;

/*
 *	Software
 *
 *	The value is the location in the file of the data.
 *	kpos = the first byte of the tag info
 *	2    = length of the number of tags
 *	MMTAGS*12 = length of the tags
 *	4    = length of the end-of-tags flag
 *	8    = length of X res data
 *	8    = length of Y res data
 */
	ss_vers ( vers, &ier, sizeof(vers) );
	sprintf ( title, "GEMPAK %s", vers );
	cst_lstr ( title, &lent, &ier );
	lent++;

	itag++;
	tags[itag].tinfo.icode  = 0x0131;
	tags[itag].tinfo.itype  = 2;
	tags[itag].tinfo.nitem  = lent;
	tags[itag].tinfo.ivalue = kpos + 2 + MMTAGS*12 + 4 +
				  noffset + nbytcnt + 8 + 8;

/*
 *	Write the number of tags and the tag information to the
 *	output array.
 */
	for ( j = 0; j < 2; j++ ) {
	    group4[kpos] = ntag.bytes[j];
	    kpos++;
	}

	for ( i = 0; i < MMTAGS; i++ ) {
	    for ( j = 0; j < 12; j++ ) {
		group4[kpos] = tags[i].bytes[j];
		kpos++;
	    }
	}

/*
 *	Add the end-of-tags flag.
 */
	group4[kpos] = 0; kpos++;
	group4[kpos] = 0; kpos++;
	group4[kpos] = 0; kpos++;
	group4[kpos] = 0; kpos++;

/*
 *	Add the tag data that does not fit in one integer.
 *
 */

/*
 *	For uncompressed data add the strip offsets and byte counts.
 */
	if  ( ktype != 0 )  {

	    /* Note: cannot have a partial line
	             one byte per data item
		     ndtbyt = kbit * klin */
	    jbytes = ndtbyt / klin;
	
	    /* Offsets */
	    strpinfo.ivalue = 8;
	    for ( ii = 0; ii < klin; ii++ )  {
		strpinfo.ivalue += jbytes;
		for ( j = 0; j < 4; j++ ) {
		    group4[kpos] = strpinfo.bytes[j];
		    kpos++;
		}
	    }

	    /* Byte counts */
	    strpinfo.ivalue = jbytes;
	    for ( ii = 0; ii < klin; ii++ )  {
		for ( j = 0; j < 4; j++ ) {
		    group4[kpos] = strpinfo.bytes[j];
		    kpos++;
		}
	    }
	}

/*
 *	X resolution; Y resolution
 */
	data.ratio.ival1 = 204 * ishft;
	data.ratio.ival2 =   1 * ishft;
	for ( j = 0; j < 8; j++ ) {
	    group4[kpos] = data.bytes[j];
	    kpos++;
	}

	data.ratio.ival1 = 196 * ishft;
	data.ratio.ival2 =   1 * ishft;
	for ( j = 0; j < 8; j++ ) {
	    group4[kpos] = data.bytes[j];
	    kpos++;
	}

/*
 *	Software title
 *	Add a NULL if the length of the string is odd.
 */
	for ( j = 0; j < lent; j++ )  {
	    group4[kpos] = title[j];
	    kpos++;
	}
	if  ( lent % 2 == 1 )  {
	    group4[kpos] = CHNULL;
	    kpos++;
	}

	*nbyte = kpos;

}

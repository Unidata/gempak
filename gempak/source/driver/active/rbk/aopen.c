#include "ardcmn.h"

#define LENHDR          36
#define MAPDEF          18
#define PRDDEF   	16
#define VGPDEF          15
#define SYSDAT          14  
#define SYSPID           8
#define PRDINF           7
#define SYSVER           5

void aopen ( int *iret )
/************************************************************************
 * aopen								*
 *									*
 * This subroutine opens a file and loads the header for a AWIPS AFOS   *
 * file.  These are blocks that appear only 1 time in a file.  These    *
 * blocks are the product identification block, the product information *
 * block, the vector graphic product definition block, and the map      *
 * background definition block.  Modes and submodes are given as octal  *
 * in the comment section.                                              *
 *									*
 * aopen ( iret )			               			*
 *                                                                      *
 * Output parameters:							*
 *	*iret 		int 		Return Code			*
 **									*
 * Log:									*
 * A. Hardy/GSC          8/98	Modified from utf's uopen       	*
 * A. Hardy/GSC          9/98	Change full pil name read       	*
 * A. Hardy/GSC          3/99	Removed FOR i inital index      	*
 * S. Jacobs/NCEP	12/99	Fixed time in short header (kmm -> khh)	*
 * A. Hardy/GSC          2/00	Changed the WMO date/time (k's -> j's)  *
 * A. Hardy/GSC 	3/00	Increased record length storage space   *
 ***********************************************************************/
{

	unsigned char   hdr[LENHDR], bchar[200], barray[17];
        int             i, m, num, npos, numhdr, ier;
	char            quehdr[17], fives[6], dhm[7];

	int		min1, min2, ierr;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

 /*     
  *	If no file name is specified, return with an error.
  */

	if ( strlen ( filnam ) == (size_t)0 ) {
	    *iret = G_NOUTFL;
	    return;
	}

/*
 *	If the open fails, return immediately.
 */
	flun = cfl_wopn ( filnam, &ierr );

	if  ( ierr != 0 ) {
	    *iret  = G_NOUTFL;
	    opnfil = G_FALSE;
	    return;
	}

/*
 *	Mark file as opened.
 */
	opnfil  = G_TRUE;

        if ( kctype != 2 ) {
/*
 *      Writing the queue descriptor header information translated 
 *      to EBCDIC into the buffer file.  This header information are
 *      the first 80 byte positions. 
 *
 *      Storing the queue header and following nulls.
 */

        strcpy ( quehdr, "QUEUE DESCRIPTOR" );
        cst_lstr ( quehdr, &num, &ier );
        st_atoe  ( quehdr, &num, barray, &ier, strlen(quehdr) );

        for ( i = 0; i <= (num - 1); ++i)
	     bchar[i] = barray[i];

	npos = i + 3;

        for ( ; i <= npos; ++i) 
	     bchar[i] = 0;
 
/*
 *      Saving a place for the record length.  
 */
	bchar[20] = 0;
	bchar[21] = 0;
	bchar[22] = 0;

/*
 *      Storing the header origin information.
 */
        npos = 27;
	for ( i = 23; i <= npos; ++i) 
	     bchar[i] = 0; 
        
        cst_lstr ( wmo, &num, &ier );
        st_atoe  ( wmo, &num, barray, &ier, strlen(wmo) );

	m = 0;
	npos = i + num - 1;
        for ( ; i <= npos; ++i, ++m) 
	     bchar[i] = barray[m];

        cst_lstr ( kloc, &num, &ier );
        st_atoe  ( kloc, &num, barray, &ier, strlen(kloc) );

	npos = i + num - 1;
        for (m=0, i; i <= npos; ++i, ++m) 
	     bchar[i] = barray[m];

/*
 *      Storing the hour and minute that the file was created.
 */
	     bchar[i] =  ( ( khh/10) * 16) + ( khh % 10 );
	     bchar[i+1] = ( ( knn/10) * 16) + ( knn % 10 );
/*
 *      Storing the fives and the rest of the header information.
 */

        strcpy ( fives, "55555" );
        cst_lstr ( fives, &num, &ier );
        st_atoe  ( fives, &num, barray, &ier, strlen(fives) );
	
	i = i + 2;
	npos = i + num -1;
        for ( m = 0, i; i <= npos; ++i, ++m) 
	     bchar[i] = barray[m];
 
	bchar[i] = 0;

	npos = i + 2;
	++i;
        for ( ; i <= npos; ++i) 
	    bchar[i] = 0x01 & BMASK;

	for ( ; i <= 79; ++i) 
	     bchar[i] = 0;
        }

/*
 *     This header information is coded for the AWIPS database
 *     At the moment it is all hard code because it is not known
 *     what bchar[10-13] means.
 */
       if ( kctype == 2 ) { 
	    bchar[0] = 0x40;
	    bchar[1] = 0x0c;
	    bchar[2] = 0x00;
	    bchar[3] = 0x01;
	    bchar[4] = 'R'; 
	    bchar[5] = 'U'; 
	    bchar[6] = 'K'; 
	    bchar[7] = 'W'; 
	    bchar[8] = 'B'; 
	    bchar[9] = 'C'; 
	    bchar[10] = 0x01;
	    bchar[11] = 0x1e;
	    bchar[12] = 0x01; 
	    bchar[13] = 0x01; 
	    bchar[14] = kyy - 1900; 
	    bchar[15] = kmm; 
	    bchar[16] = kdd; 
	    bchar[17] = khh; 
	    bchar[18] = knn; 
	    bchar[19] = kss; 
	    bchar[20] = 'K'; 
	    bchar[21] = 'D'; 
	    bchar[22] = 'E'; 
	    bchar[23] = 'N'; 
	    i = 24;
       }
/*        
 *     Storing the WMO header in ASCII.
 */
        cst_lstr ( wmo, &num, &ier );

        npos = i + num - 1;
        for ( m = 0, i; i <= npos; ++i,++m) 
	     bchar[i] = wmo[m];

        bchar[i] = CHSPAC;

	++i;
        cst_lstr ( kloc, &num, &ier );
	npos = i + num - 1;
	for (m = 0, i; i <= npos; ++i, ++m )
	     bchar[i] = kloc[m];

        bchar[i] = CHSPAC;
/*
 *      Storing the date and time. 
 */

        sprintf (dhm,"%02d%02d%02d",jdd,jhh,jnn);
        cst_lstr ( dhm, &num, &ier );
        npos = i + num;
        ++i;
        for ( m=0, i; i <= npos; ++i, ++m ) 
        bchar[i] = dhm[m] & BMASK;

        bchar[i]   = CHCR;
        bchar[i+1] = CHCR;
        bchar[i+2] = CHLF;

/*
 *      Writing out the array bchar to the output file.
 */

        numhdr = i + 3;
        awrbuf ( bchar, numhdr, &ier);

/*
 *	The product identification block, mode 1 submode 1, 16 byte 
 *      pairs long and the storage scheme is as follows:
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> control block mode 1
 *        hdr[3]         --> identification block submode 1
 *	  hdr[4-7]       --> originator identification
 *	  hdr[8]         --> file classification
 *	  hdr[9]         --> retention time
 *        hdr[10-19]     --> file indicator & product identifier
 *        hdr[20], hdr[21] --> year
 *	  hdr[22]	 --> month
 *	  hdr[23]	 --> day
 *	  hdr[24]	 --> hour
 *	  hdr[25]	 --> minute
 *        hdr[26-31]     --> product identifier continued
 */
	hdr[0] = 0x40;
	hdr[1] = PRDDEF;
	hdr[2] = 0x01; 
	hdr[3] = 0x01; 

	hdr[4] = 0x00;
	hdr[5] = 0x00;
	hdr[6] = 0x00;
	hdr[7] = 0x00;

	hdr[8] = flcls[0]; 
	hdr[9] =  rettim & BMASK;

	hdr[10] = prodid[0];
	hdr[11] = prodid[1];
	hdr[12] = prodid[2];
	hdr[13] = prodid[3];
	hdr[14] = prodid[4];
	hdr[15] = prodid[5];
	hdr[16] = prodid[6];
	hdr[17] = prodid[7];
	hdr[18] = prodid[8];
	hdr[19] = prodid[9];

        hdr[20] = ( jyy>> 8 ) & BMASK;
        hdr[21] =   jyy & BMASK;
	hdr[22] =   jmm & BMASK;
	hdr[23] =   jdd & BMASK;
	hdr[24] =   jhh & BMASK;
	hdr[25] =   jnn & BMASK;

	hdr[26] = extnd[0];
	hdr[27] = extnd[1];
	hdr[28] = extnd[2];
	hdr[29] = extnd[3];
	hdr[30] = extnd[4];
	hdr[31] = extnd[5];

/*
 *	Write the header array to the buffer.
 */
	awrbuf ( hdr, PRDDEF*2, iret );
/*
 *      The system binary version number block, mode 2 submode 5.
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> system block mode 2
 *        hdr[3]         --> version block submode 5
 *        hdr[4-7]       --> System version number 
 *        hdr[8-9]       --> Pad out the block
 */
	hdr[0] = 0x40;
	hdr[1] = SYSVER;
	hdr[2] = 0x02; 
	hdr[3] = 0x05; 

	hdr[4] = 'V'; 
	hdr[5] = '2';
	hdr[6] = '.';
	hdr[7] = '0';

	hdr[8] = 0x00;
	hdr[9] = 0x00;
/*
 *	Write the header array to the buffer.
 */

	awrbuf ( hdr, SYSVER*2, iret );

/*
 *      The system product ID number block, mode 2 submode 1.
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> system block mode 2
 *        hdr[3]         --> product ID block submode 1
 *        hdr[4-13]      --> AFOS product ID number
 *        hdr[14-15]     --> Pad out the block
 */
	hdr[0] = 0x40;
	hdr[1] = SYSPID;
	hdr[2] = 0x02; 
	hdr[3] = 0x01; 

	hdr[4] = awpid[0];
	hdr[5] = awpid[1];
	hdr[6] = awpid[2];
	hdr[7] = awpid[3];
	hdr[8] = awpid[4];
	hdr[9] = awpid[5];
	hdr[10] = awpid[6];
	hdr[11] = awpid[7];
	hdr[12] = awpid[8];
	hdr[13] = awpid[9];

	hdr[14] = 0x00;
	hdr[15] = 0x00;
/*
 *	Write the header array to the buffer.
 */

	awrbuf ( hdr, SYSPID*2, iret );

/*
 *	The product information block, mode 1 submode 6, 
 *      7 byte pairs long and the storage scheme is as follows:
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> product block mode 4
 *        hdr[3]         --> information block submode 6
 *	  hdr[4&5]       --> base hour
 *	  hdr[6&7]       --> base date
 *	  hdr[8&9]       --> base month
 *        hdr[10-11]     --> base year
 *        hdr[12-13]     --> Originating model or program
 */

        sprintf ( basdat, "%02d%02d%02d%02d",
	                   khh, kdd, kmm, kyy%100 );
	hdr[0] = 0x40;
	hdr[1] = PRDINF;
	hdr[2] = 0x01; 
	hdr[3] = 0x06; 

	hdr[4] = basdat[0];
	hdr[5] = basdat[1];
	hdr[6] = basdat[2];
	hdr[7] = basdat[3];
	hdr[8] = basdat[4];
	hdr[9] = basdat[5];
	hdr[10] = basdat[6];
	hdr[11] = basdat[7];

	hdr[12] = 0;
	hdr[13] = 0;
/*
 *	Write the header array to the buffer.
 */

	awrbuf ( hdr, PRDINF*2, iret );

/*
 *      The system data block, mode 2 submode 2.
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> system block mode 2
 *        hdr[3]         --> data block submode 2
 */
	hdr[0] = 0x40;
	hdr[1] = SYSDAT;
	hdr[2] = 0x02; 
	hdr[3] = 0x02; 

/*
 *      Set the header size and load the header.
 *      All output blocks set the same values for the first
 *      four bytes.
 *
 *      Byte 4 is the ADCCP Link Address. (1)
 *      Byte 5 is the ADCCP Control Field. (0)
 *      Bytes 6-7 are the Message Identification.
 */
        hdr[4] = 0x01;
        hdr[5] = 0x00;
        hdr[6] = 0x40;
        hdr[7] = 0x00;

/*
 *      Bytes 8-9 specify the Message Address
 */
        hdr[8]  = 0x7f;
        hdr[9]  = 0x00;

/*
 *      Byte 10 is the Message Type and Priority.
 */
        hdr[10]  = 0x30;

/*
 *      The full PIL is added to the header block in bytes 11-19.
 */
        sprintf ( newname, "NMCGPH%s", pil );
        hdr[11] = newname[0];
        hdr[12] = newname[1];
        hdr[13] = newname[2];
        hdr[14] = newname[3];
        hdr[15] = newname[4];
        hdr[16] = newname[5];
        hdr[17] = newname[6];
        hdr[18] = newname[7];
        hdr[19] = newname[8];

/*
 *      The number of minutes from the beginning of the year are 
 *      stored in bytes 20-22. Each byte can only store numbers 
 *      from 0 to 127.
 */
        hdr[20] = ( minutes / 16384 ) & BMASK;
        min1 = minutes % 16384;
        hdr[21] = ( min1 / 128 ) & BMASK;
        min2 = min1 % 128;
        hdr[22] = min2 & BMASK;

/*
 *      Bytes 23-24 are the Message Valid Time. (0)
 *      Byte 25 is the Product Version Number. (0)
 *      Byte 26 is the Node Forward Counter. (0)
 *      Byte 27 is for padding out the block.
 */
        hdr[23] = 0x00;
        hdr[24] = 0x00;
        hdr[25] = 0x00;
        hdr[26] = 0x00;
        hdr[27] = 0x00;

/*
 *	Write the header array to the buffer.
 */

	awrbuf ( hdr, SYSDAT*2, iret );

/*
 *	The vector graphic product definition block, mode 4 submode 20, 
 *      15 byte pairs long and the storage scheme is as follows:
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> graphic block mode 4
 *        hdr[3]         --> definition block submode 20
 *	  hdr[4&5]       --> projection indicator and coordinate flag
 *	  hdr[6&7]       --> scale factor
 *	  hdr[8&9]       --> area code and label code
 *        hdr[10-13]     --> 1st M and N reference coordinates
 *        hdr[14-17]     --> 2nd M and N reference coordinates
 *        hdr[18-21]     --> 3rd M and N reference coordinates
 *	  hdr[22]	 --> month  - valid time of product
 *	  hdr[23]	 --> day    - valid time of product
 *	  hdr[24]	 --> hour   - valid time of product
 *	  hdr[25]	 --> minute - valid time of product
 *	  hdr[26]	 --> month  - end of valid time 
 *	  hdr[27]	 --> day    - end of valid time 
 *	  hdr[28]	 --> hour   - end of valid time 
 *	  hdr[29]	 --> minute - end of valid time 
 */

	hdr[0] = 0x40;
	hdr[1] = VGPDEF;
	hdr[2] = 0x04; 
	hdr[3] = 0x10; 

	hdr[4] = kmap & BMASK;
	hdr[5] =  crdflg & BMASK;

	hdr[6] = sclint & BMASK;
	hdr[7] = sclfac & BMASK;

	hdr[8] =  areacd & BMASK;
	hdr[9] =  label & BMASK;

	hdr[10] = ( 0 >> 8) & BMASK;
	hdr[11] =  0 & BMASK;
	hdr[12] = (kysize >> 8) & BMASK;
	hdr[13] =  kysize & BMASK;
	hdr[14] = (kxsize >> 8) & BMASK;
	hdr[15] =  kxsize & BMASK;
	hdr[16] = (kysize >> 8) & BMASK;
	hdr[17] =  kysize & BMASK;
	hdr[18] = (kxsize >> 8) & BMASK;
	hdr[19] =  kxsize & BMASK;
	hdr[20] = ( 0 >> 8) & BMASK;
	hdr[21] =  0 & BMASK;

	hdr[22] =  kmm & BMASK;
	hdr[23] =  kdd & BMASK;
	hdr[24] =  khh & BMASK;
	hdr[25] =  knn & BMASK;

	hdr[26] = 0; 
	hdr[27] = 0;
	hdr[28] = 0;
	hdr[29] = 0;


/*
 *	Write the header array to the buffer.
 */

	awrbuf ( hdr, VGPDEF*2, iret );


/*
 *	The map background definition block, mode 4 submode 21, 
 *      18 byte pairs long and the storage scheme is as follows:
 *
 *	  hdr[0]         --> field flag
 *	  hdr[1]         --> length of block
 *	  hdr[2]         --> graphic block mode 4
 *        hdr[3]         --> definition block submode 21
 *	  hdr[4]         --> coordinate flag
 *	  hdr[5]         --> count of reference points
 *	  hdr[6&7]       --> upper left corner latitude 
 *	  hdr[8&9]       --> upper left corner longitude 
 *	  hdr[10&11]     --> upper right corner latitude 
 *	  hdr[12&13]     --> upper right corner longitude 
 *        hdr[14&15]     --> lower right corner latitude 
 *        hdr[16&17]     --> lower right corner longitude
 *        hdr[18&19]     --> lower left corner latitude 
 *        hdr[20&21]     --> lower right corner longitude
 *        hdr[22&23]     --> vertical meridian
 *	  hdr[24&25]	 --> standard latitude of projection
 *	  hdr[26&27]	 --> second standard latitude
 *	  hdr[28-33]	 --> 6 byte background name
 *	  hdr[34&35]	 --> nulls
 */

	hdr[0] = 0x40; 
	hdr[1] = MAPDEF;
	hdr[2] = 0x04; 
	hdr[3] = 0x11; 

	hdr[4] = cordfg & BMASK;
	hdr[5] = nrefpt & BMASK;

	hdr[6] = (ilat[0] >> 8) & BMASK;
	hdr[7] =  ilat[0] & BMASK;
	hdr[8] = (ilon[0] >> 8) & BMASK;
	hdr[9] =  ilon[0] & BMASK;

	hdr[10] = (ilat[1] >> 8) & BMASK;
	hdr[11] =  ilat[1] & BMASK;
	hdr[12] = (ilon[1] >> 8) & BMASK;
	hdr[13] =  ilon[1] & BMASK;

	hdr[14] = (ilat[2] >> 8) & BMASK;
	hdr[15] =  ilat[2] & BMASK;
	hdr[16] = (ilon[2] >> 8) & BMASK;
	hdr[17] =  ilon[2] & BMASK;

	hdr[18] = (ilat[3] >> 8) & BMASK;
	hdr[19] =  ilat[3] & BMASK;
	hdr[20] = (ilon[3] >> 8) & BMASK;
	hdr[21] =  ilon[3] & BMASK;

	hdr[22] = (iangles[0] >> 8) & BMASK;
	hdr[23] =  iangles[0] & BMASK;
	hdr[24] = (iangles[1] >> 8) & BMASK;
	hdr[25] =  iangles[1] & BMASK;
	hdr[26] = (iangles[2] >> 8) & BMASK;
	hdr[27] =  iangles[2] & BMASK;

	hdr[28] = bckgnd[0]; 
	hdr[29] = bckgnd[1]; 
	hdr[30] = bckgnd[2]; 
	hdr[31] = bckgnd[3]; 
	hdr[32] = bckgnd[4]; 
	hdr[33] = bckgnd[5]; 

	hdr[34] = 0x00;
	hdr[35] = 0x00;

/*
 *	Write the header array to the buffer.
 */
	awrbuf ( hdr, MAPDEF*2, iret );

}

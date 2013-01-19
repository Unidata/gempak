#include "utfcmn.h"

#define IBLKSZ	256

void uclosp ( int *iret )
/************************************************************************
 * uclosp								*
 *									*
 * This subroutine closes the plot file.  Before closing the file, all  *
 * buffered data is flushed.						*
 *									*
 * If the AFOS communication format is to be used, the data is moved	*
 * from the outbuf to the output file in 256 byte blocks. Each block	*
 * starts with a header. In the first block the header is 23 bytes	*
 * long, and in each succeeding block it is 4 bytes long. There is a	*
 * maximum of 64 blocks allowed in a file. The file must contain an EXT	*
 * flag of 0x83 in the final block, and if it ends up in the left half	*
 * of a 2 byte word, then the right side of the word must be padded	*
 * with 0xff. The entire file is then padded to a multiple of 1280	*
 * bytes, as this is the length of data read by OSO when parsing AFOS	*
 * files.								*
 *									*
 * If the NAFOS format is to be used, the standard header is loaded,	*
 * and the data is dumped as is (i.e., no block and block header	*
 * structure is used).							*
 *									*
 * uclosp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Intial coding				*
 * E. Safford/GSC	03/97	Removed call to umkttl			*
 * S. Jacobs/NCEP	 8/97	Removed calls to subroutines, added	*
 *				code from these subroutines here	*
 * S. Jacobs/NCEP	 9/97	Reset hdr bytes 2-3 for the first block	*
 * S. Jacobs/NCEP	 9/97	Changed priority and time bytes		*
 * S. Jacobs/NCEP	10/97	Replaced the year-to-date minutes	*
 * S. Jacobs/NCEP	 3/99	Fixed "last block" computation		*
 * S. Jacobs/NCEP	 4/99	Added "ALL" for the PIL header routing	*
 * S. Jacobs/NCEP	 5/99	Added NNNN to end of NAFOS file format	*
 ***********************************************************************/
{

	int		knt, ib, i, ihdrsz, first, ier, numblk,
			min1, min2, icnt;
	unsigned char	block[IBLKSZ];

/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	If there is no data, return.
 */
	if  ( numout <= 0 )  return;

/*
 *      Flush buffer to outfile.
 */
	first  = G_TRUE;
	numblk = 0;
	knt    = 0;

/*
 *	If the output type is not equal to 2, then create output in
 *	the OSO AFOS format.
 */
	if  ( kctype != 2 )  {

/*
 *	    Loop while there is still data in the buffer.
 */
	    while ( knt < numout ) {

/*
 *		Fill the output array with 0's. If a value is not
 *		assigned explicitly, it will have a value of 0.
 */
	        for ( i = 0; i < IBLKSZ; i++ )  block[i] = 0x00;

/*
 *		Set the header size and load the header.
 *		All output blocks set the same values for the first
 *		four bytes.
 *
 *		Byte 0 is the ADCCP Link Address. (1)
 *		Byte 1 is the ADCCP Control Field. (0)
 *		Bytes 2-3 are the Message Identification.
 */
		ihdrsz = 4;
		block[0] = 0x01;
		block[1] = 0x00;
		block[2] = 0x00;
		block[3] = 0x00;

		if  ( first )  {

/*
 *		    For the first block, the header size is 23 bytes.
 */
		    ihdrsz = 23;
		    first  = G_FALSE;

/*
 *		    Reset the Message Identification for the first block.
 */
		    block[2]  = 0x40;
		    block[3]  = 0x00;

/*
 *		    Bytes 4-5 specify the Message Address
 */
		    block[4]  = 0x7f;
		    block[5]  = 0x00;

/*
 *		    Byte 6 is the Message Type and Priority.
 */
		    block[6]  = 0x30;

/*
 *		    The full PIL is added to the header block in
 *		    bytes 7-15.
 */
		    block[7]  = 'N';
		    block[8]  = 'M';
		    block[9]  = 'C';
		    block[10] = 'G';
		    block[11] = 'P';
		    block[12] = 'H';
		    block[13] = pil[0];
		    block[14] = pil[1];
		    block[15] = pil[2];

/*
 *		    The number of minutes from the beginning of the
 *		    year are stored in bytes 16-18.
 *		    Each byte can only store numbers from 0 to 127.
 */
		    block[16] = ( minutes / 16384 ) & BMASK;
		    min1 = minutes % 16384;
		    block[17] = ( min1 / 128 ) & BMASK;
		    min2 = min1 % 128;
		    block[18] = min2 & BMASK;

/*
 *		    Bytes 19-20 are the Message Valid Time. (0)
 *		    Byte 21 is the Product Version Number. (0)
 *		    Byte 22 is the Node Forward Counter. (0)
 */
		    block[19] = 0x00;
		    block[20] = 0x00;
		    block[21] = 0x00;
		    block[22] = 0x00;
		}

/*
 *		If this is the last block of data then add the last
 *		block flag to byte 2 of the header.
 *
 *		Account for the 4-byte flag at the beginning of the
 *		block and the possibility of the 1-byte EOF flag.
 */		
		if  ( (numout - knt) < (IBLKSZ - 5) )
		    block[2] = block[2] | 0x80; 

/*
 *		Write contents of outbuf to block, while the block
 *		is not full and the count is less than the total
 *		contents of outbuf.
 */
		for ( ib = ihdrsz; (ib < IBLKSZ) && (knt <= numout);
		      ib++, knt++ ) {
		    block[ib] = outbuf[knt];
	        }
			
/*  			
 *	        If the end of data has been reached and not the end
 *		of block, then append the EXT flag (0x83) to the block.
 *		If this ends up in the left half of a two byte word,
 *		then the right half must be assigned the PAD value
 *		of 0xff.
 */
		if  ( ( knt >= numout ) && ( ib < IBLKSZ ) )  {
		    block[ib] = 0x83;
		    ib += 1;

		    if  ( ib % 2 )  {
		        block[ib] = 0xff; 
		        ib += 1;
		    }
		}

		else if  ( ( knt >= numout ) && ( ib >= IBLKSZ ) )  {
/*
 *		    Otherwise, check if the end of block and end of
 *		    data occurred simultaneously. If so, must write
 *		    the block, then set up a new block with the block
 *		    header and end of file information.
 */
		    cfl_writ ( flun, IBLKSZ, block, &ier );    
		    numblk += 1;

/*
 *		    Build a new block containing only the header and
 *		    end of file information.
 */
		    block[0] = 0x01;
		    block[1] = 0x00;
		    block[2] = 0x80;
		    block[3] = 0x00;
		    block[4] = 0x83; 
		    block[5] = 0xff;  
		    ib       = 6; 
		}

		if  ( ib < IBLKSZ )  {
		    for ( i = ib; i < IBLKSZ; i++ )  block[i] = 0x20; 
		}
		    
                cfl_writ ( flun, IBLKSZ, block, &ier );
		numblk += 1;

	    }

/*
 *	    Round out to 1280 Byte size.
 */
	    while ( numblk % 5 ) {
	        for ( i = 0; i < IBLKSZ; i++ )  block[i] = 0x00;
                cfl_writ ( flun, IBLKSZ, block, &ier );
		numblk++;
	    }
	
	}

/*
 *	Otherwise, the output type is the NAFOS format.
 */
 	else { 	

/*
 *	    Create the product header and write it to the file.
 */
	    block[0]  = 'Z';
	    block[1]  = 'C';
	    block[2]  = 'Z';
	    block[3]  = 'C';
	    block[4]  = ' ';
	    block[5]  = 'N';
	    block[6]  = 'M';
	    block[7]  = 'C';
	    block[8]  = 'G';
	    block[9]  = 'P';
	    block[10] = 'H';
	    block[11] = pil[0];
	    block[12] = pil[1];
	    block[13] = pil[2];
	    block[14] = ' ';
	    block[15] = 'A';
	    block[16] = 'L';
	    block[17] = 'L';
	    block[18] = 0x0d;
	    block[19] = 0x0a;
            cfl_writ ( flun, 20, block, &ier );

/*
 *	    Write the data to the file.
 */
            cfl_writ ( flun, numout + 1, outbuf, &ier );

/*
 *	    Add the end of file marker to the file.
 */
	    icnt = 0;
	    block[icnt] = 0x83; icnt++;

	    if  ( (20+numout+1+1)%2 == 1 ) {
		block[icnt] = 0xff; icnt++;
	    }

	    block[icnt] = 0x0d; icnt++;
	    block[icnt] = 0x0a; icnt++;
	    block[icnt] = 'N';  icnt++;
	    block[icnt] = 'N';  icnt++;
	    block[icnt] = 'N';  icnt++;
	    block[icnt] = 'N';  icnt++;
	    cfl_writ ( flun, icnt, block, &ier);  

	}

/*
 *	Close the plot file.
 */
	if  ( opnfil )  {
	    cfl_clos ( flun, &ier );
	    opnfil  = G_FALSE; 
	}

        numout = 0;

}

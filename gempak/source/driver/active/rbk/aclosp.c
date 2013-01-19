#include "ardcmn.h"

#define LENHDR    4

void aclosp ( int *iret )
/************************************************************************
 * aclosp								*
 *									*
 * This subroutine closes the plot file.  Before closing the file, all  *
 * data is written to the output file                                   *
 *									*
 * aclosp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC 	9/98		Modified from UCLOSP            *
 * A. Hardy/GSC 	3/99		Corrected LENHDR from 2 -> 4    *
 * A. Hardy/GSC 	3/00		Increased record len storage    *
 * S. Jacobs/NCEP	3/01		Fixed use of LENHDR		*
 ***********************************************************************/
{

        int             j, nbyt, nrec, npad, recln, ier;
	unsigned char	bchar[3], hdr[LENHDR], pad[1280];
    


/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	If there is no data, return.
 */
	if  ( numout <= 0 )  return;

/*
 *	Create the end of file marker. 
 */
	hdr[0] = 0x40; 
	hdr[1] = LENHDR / 2;
	hdr[2] = 0x01;
	hdr[3] = 0x02;

/*
 *      Writing out the last block to the output file.
 */

	awrbuf ( hdr, LENHDR, &ier);  

        if ( kctype != 2 ) {
/*
 *          Storing the record length and adding a pad if necessary.  
 *          The record length is:
 *              record length = 
 *                  ( ( bchar[20]*256 ) + bchar[21] ) * 80  + bchar[22]
 */
            recln = numout + 1;
            if ( (recln % 1280 ) != 0 ){
	        npad = 1280 - ( recln % 1280 ); 

	        for ( j = 0; j < npad; ++j)
	             pad[j] = 0x20;

  	        awrbuf ( pad, npad, &ier);  
            }

	    nrec = recln / 80;
	    nbyt = recln % 80;
            cfl_seek ( flun, 20, 0, iret );
	    bchar[0] = ( nrec >> 8 ) & BMASK;
	    bchar[1] = nrec & BMASK;
	    bchar[2] = nbyt & BMASK;

	    awrbuf ( bchar, 3, &ier);  

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

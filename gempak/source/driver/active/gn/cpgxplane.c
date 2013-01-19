#include "geminc.h"
#include "cpgcmn.h"


int pg_xplane ( int ixsz, int *iysz, char *outl, int start_at, 
						int stop_at, char *bits )
/************************************************************************
 * pg_xplane								*
 *									*
 * This function extracts a plane from an NMC 6-bit product.		*
 *									*
 * int pg_xplane ( ixsz, iysz, outl, start_at, stop_at, bits )		*
 *									*
 * Input parameters:							*
 *    ixsz	int	Size of each scanline in bits of the plane	*
 *    *iysz	int	Number of scanlines in the file			*
 *    *outl 	char	Plane of bits to be read			*
 *    start_at	int	Where the data starts in the plane		*
 *    stop_at	int	Where the data stops in the plane		*
 *									*
 * Output parameters:							*
 *    *bits 	char	Plane to receive the data			*
 *    pg_xplane int	Total bits in plane (total_bits)		*
 **									*
 * Log:									*
 *  	E. Wehner/EAi	6/96	Created					*
 * 	E. Wehner/EAi	12/96	Removed "pow" calls			*
 * 	E. Wehner/EAi	1/97	Bound scanlines if out of range 	*
 * 	E. Wehner/EAi	6/97	Fixed underrun of long scan lines	*
 *	T. Piper/GSC	10/98	Prolog update				*
 * 	R. Tian/SAIC    05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{

    int i;
    int iret;
    char ch;
    char flag;
    char text;
    int scanct = 0;
    int eom = 0;
    int runlen = 0;
    int runval = 2;
    int linelen = 0;
    int ctr = 0;
    int numsbits=  0;
    int total_bits = 0;
    numsbits = (int)((double)(stop_at - start_at) * (8.0/6.0));

    /* 
    * runval - The runvalue represents the type of data that is 
    *	being processed by a particular run of data, either ones,
    *	zero's or alternating ones and zeros.
    */
    
 
    /* for each 6 bit entity, extract it, then process it. */
    for (i=0;i< numsbits; i++) 
    {

        if (eom)	/* if end of message...end of fax file ... */
            break;

        /* pg_getsix extracts the 6 bit entity */
        pg_getsix((outl+start_at), i, &ch, &iret);


        /* isolate the flag and text portion of each 6 bit entity */
        flag = (char)((ch >> 6) & 0x03);
        text = (char)((ch >> 2) & 0x0f);

        switch (flag)
        {
          case 0:

            /* now switch on what kind of run is being processed. */
            switch(runval)
            {
              case 0:
                /* if zeroes, and processing zeroes, add to length of run */
		runlen += ( text << (4*ctr));
                ctr++;
                break;
              case 1:
                /* if zero, and processing ones, set the bits for last run */
                pg_setbits( bits,(total_bits+linelen) ,(runlen*4), 1);
                linelen += (runlen*4);
                runlen = (int)text;
                ctr = 1;
                break;
              case 2:
                /* if previously alternating, set current length as runlen */
                runlen = (int)text;
                ctr = 1;
                break;
              default:
                break;
            }
            if ( (runlen > ixsz) ) eom++;    /* EJW - 010997 */
            runval = 0;
            break;
          case 1:
            switch(runval)
            {
              case 0:
                /* if one and processing zeroes, right out last run */
                pg_setbits( bits,(total_bits+linelen) ,(runlen*4), 0);
                linelen += (runlen*4);
                ctr = 1;
                runlen = (int)text;
                break;
              case 1:
                /* if one and processing ones, shift up the other bits */
		runlen += ( text << (4*ctr));
                ctr++;
                break;
              case 2:
                /* if one and processing alternating, just add it up */
                runlen = (int)text;
                ctr = 1;
                break;
              default:
                break;
            }
            if ( (runlen > ixsz) ) eom++;    /* EJW - 010997 */
            runval = 1;
            break;
          case 2:
            /* 
             * When an alternating sequence flag is received, then 
             * if previously printing a string of ones and zeroes,
             * write out the last runs, then the one and zero sequence
	     */
            switch(runval)
            {
              case 0:
                pg_setbits( bits, (total_bits+linelen),(runlen*4), 0);
                linelen += (runlen*4);
                break;
              case 1:
                pg_setbits( bits,(total_bits+linelen) ,(runlen*4), 1);
                linelen += (runlen*4);
                break;
              case 2:
                break;
              default:
                break;
            }
            pg_setbits(bits, (total_bits+linelen), (int)text, 2);
            linelen += 4;
            if ( (runlen > ixsz) ) eom++;    /* EJW - 010997 */
            runlen = 0;
            runval = 2;
            break;
          case 3:
            switch (text)
            {
              /* if a flag is received, could be either end of scan, or end
               * of message.  Close out the current run (if any), then 
               * handle the flag. */
              case 0:

                switch(runval)
                {
                  case 0:
                    pg_setbits( bits,(total_bits+linelen) ,(runlen*4), 0);
                    linelen += (runlen*4);
                    break;
                  case 1:
                    pg_setbits( bits,(total_bits+linelen) ,(runlen*4), 1);
                    linelen += (runlen*4);
                    break;
                  case 2:
                    break;
                  default:
                    break;
                }
                total_bits += ixsz;
                runval = 2;
		if ( (runlen > ixsz) ) eom++;    /* EJW - 010997 */
                runlen = 0;
                linelen = 0;
                scanct++;
                if ( ( ( *iysz == 0) && (scanct >=7200) ) ||
                     ( ( *iysz > 0) && (scanct > *iysz) ) )
                    eom++;
                break;
              case 3:
                eom++;
                break;
              default:
                break;
            }
            runval = 2;
            break;
          default:
            break;
        }
        
    }

    *iysz = scanct;

    return total_bits;

    
}

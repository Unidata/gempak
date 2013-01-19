#include "geminc.h"
#include "cpgcmn.h"

void pg_setbits ( char *map, int start, int run, int value )
/************************************************************************
 * pg_setbits								*
 *									*
 * This function sets a run of bits either off or on in a bit plane.	*
 *									*
 * pg_setbits ( map, start, run, value )				*
 *									*
 * Input parameters:							*
 *   	start	int	Start bit of run				*
 *	run	int	Length of run					*
 *	value	int	1 or zero (value of the run)			*
 *									*
 * Output parameters:							*
 *	*map 	char	Map plane of data				*
 **									*
 * Log:									*
 *	E. Wehner/EAi	 6/96	Created				 	*
 *	T. Piper/GSC	10/98	Prolog update				*
 ***********************************************************************/
{
    int bit_start;
    int byte_start;
    int active_byte;
    int i;
    int active_bit;

    char temp_byte;
    char bit_set;

   /* set the requested number of bits to the requested value based
    * on the starting position.  To do this, first calculate the start and
    * end position of the run in the bitplane
   */
    bit_start = start % 8;
    byte_start = start / 8;

    active_byte = byte_start;
    active_bit = bit_start;

    /* if value is two, then the "run" value is in fact the value of
    *  exact 4 bits that need to be stored.  If it is one, turn
    *  on the requested number of bits.  Otherwise, value is zero, so
    *  just return, don't do anything with zeroes.  */
    if (value == 1)
    {
        for (i = 0;i < run; i++)
        {

            if (active_bit > 7)
            {
               active_bit = 0;
               active_byte++;
            } 
            temp_byte = *(map+active_byte);

            /* when setting the bit, recall that the 6 bit entity in a 
             * fax map looks like reversed logic -- high order bit in
             * low order position, so switch them by subtracting 7.
            */

            bit_set = (char)(( 0x01 << (7-active_bit)));
            *(map+active_byte) = (char)(bit_set | temp_byte);

            active_bit++;

        } 
    }
    else
    {
        if (value == 2)  /* the bottom 4 bits should be handled verbatim */
        {
            for (i = 3;i >= 0; i--)
            {

                if (active_bit > 7)
                {
                   active_bit = 0;
                   active_byte++;
                } 
                if (((run >> i) & 0x01 ) == 1) /* if the pixel is turned on */
                {
                   temp_byte = *(map+active_byte);

                   bit_set = (char)(( 0x01 << (7-active_bit)));
                   *(map+active_byte) = (char)(bit_set | temp_byte);

                }
                active_bit++;

            } 
        }
    }
}

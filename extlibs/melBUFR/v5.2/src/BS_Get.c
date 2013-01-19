/*
 * BitStream_Get - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Get - Read a value from a bit stream.  Return 1 on error, else 0.
 *
 * POSSIBLY ENDIAN SENSITIVE.
 *
 * NOTE: This byte masking portion of this function may need to be altered
 * for low-endian machines such as a VAX.
 */

#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  MODULE NAME:         BitStream_Get
 
  DESCRIPTION:         Read a value from a bit stream.  Return 1 on error, 
			else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int BitStream_Get( BitStream_t* BS, EncVal_t* EV, int BitWidth )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  BS		     BitStream_t   In/Out	Bit Stream
  EV		     EncVal_t	   Out		Encoded Value
  BitWidth	     int	   In		Width of Value in bits
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Bit Stream is NULL    Write error message to buffer
                        Return with an error
  Bis Steam is empty	Write error message to buffer
                        Return with an error
  Bit width is invalid	Write error message to buffer
                        Return with an error
  Bit width > word	Write error message to buffer
  length                Return with an error
  Bit width > bits	Write error message to buffer
  left                  Return with an error
  Malloc doesn't work	Write error message to buffer
                        Return with an error
  HexStr_SetBit error	Write error message to buffer
			Free memory
                        Return with an error 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  malloc		Allocates memory
  HexStr_SetBit		Sets a given bit within a hexstring
  EncVal_Init		Initializes an encoded value
  free		Frees memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  bits_left	int		bits left in the bitstream
  bits_to_shift	int		Number of bits to shift
  i		int		Size of hex string
  bit_val	uint_t		Single bit from bit stream
  hs		HexStr_t	Hex String
 
  METHOD:
	If the bit stream is Null then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	If the bit Stream is empty then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	If the bit width is less than 1 then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	If the bit width is greater than the word length then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	Calculate the number of bits left in the bit stream
	If the number of bits left is less than the size of the variable then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	Calculate the size of the hex string
	If there is an error while allocating memory for the hex string then
          Perform BUFR_Err_Set to write error message
          Return with error
	Else
	  Initialize the hex string to zero
        Endif
	Loop on each bit in the hex string
	  Get a single bit from the bitstream
	  Place into bit_val while shifting
	  Increment the BS bit number
	  If the bit number has gone over the number of bits in a byte then
	    Adjust the buffer pointer, byte index, and bit index
	  Endif
	  If the bit value is 1 then
	    If there is an error performing HexStr_SetBit then
              Perform BUFR_Err_Set to write error message
	      Free memory
              Return with error
	    Endif
	  Endif
	End Loop
	Perform EncVal_Init store the hex string in the encoded value
	Return with no error
	  
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
     100897 LAH: Added uint_t cast
     102097 LAH: Added uint_t cast
     022498 LAH:  Added prints to bufr_log file.

..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int BitStream_Get( BitStream_t* BS, EncVal_t* EV, int BitWidth )

#else

int BitStream_Get( BS, EV, BitWidth )
BitStream_t* BS;
EncVal_t*    EV;            /* Address of value to get   */
int          BitWidth;      /* Number of bits to get. */

#endif
{
    int    bits_left, bits_to_shift;
    uint_t bit_val;
  
    extern BUFR_Cntl_t BUFR_Cntl;

    int      i;
    HexStr_t hs;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Get", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
	     "BitStream_Get: NULL BitStream_t pointer\n");
        return 1;
    }

    if( BS->byte_num >= BS->size )
    {
        BUFR_Err_Set( "BitStream_Get", "Bit Stream is exhausted" );
	      fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Get: Bit Stream is exhausted\n");
        return 1;
    }

    /* Make sure that BitWidth is valid. */

    if( BitWidth < 1 )
    {
        BUFR_Err_Set( "BitStream_Get", "Bit width < 1" );
        return 1;
    }

    /* 102097 LAH: Added uint_t cast */
    if( (uint_t) BitWidth > BITS_IN_WORD )
    {
        BUFR_Err_Set( "BitStream_Get", "Bit width > word length" );
	      fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Get: Bit width > word length\n");
        return 1;
    }

    /*
     * Make sure there are enough bits left in the bit stream for
     * this value (i.e. this may be the end
     */

    bits_left = (BS->size - BS->byte_num) * BITS_IN_BYTE - BS->bit_num;

    if( BitWidth > bits_left )
    {
        BUFR_Err_Set( "BitStream_Get", "Bit width > bits left in stream" );
	      fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Get: Bit width > bits left in stream\n");
        return 1;
    }

    /* Create the hex string. */

    i = (BitWidth / BITS_IN_BYTE) + ( (BitWidth%BITS_IN_BYTE) != 0 );

    /* 100897 LAH: Added uint_t cast */
    if( (hs = (HexStr_t) malloc(sizeof(HexStr_t) * (uint_t)i )) == NULL )
    {
        BUFR_Err_Set( "BitStream_Get", "Can't create hex string" );
      	fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Get: Can't create hex string\n");
        return 1;
    }
    else    /* Initialize hex string to zeros. */
    {
        /* 100897 LAH: Added uint_t cast */
        (void) memset( (char*)hs, 0, sizeof(HexStr_t) * (uint_t)i );
    }

    /* Set each bit in the hex string */

    for( i=0; i < BitWidth; i++ )
    {
        /* Get a single bit from the bit stream. */

        bits_to_shift = (BITS_IN_BYTE-1) - BS->bit_num;
        bit_val = ( (uint_t) (*BS->bp) >> bits_to_shift) & 1;
        BS->bit_num++;

        /* Adjust buffer pointer, byte index and bit index as needed. */
        while( BS->bit_num >= BITS_IN_BYTE )
        {
            BS->bp++;
            BS->byte_num++;
            BS->bit_num -= BITS_IN_BYTE;
        }

        if( bit_val )
        {
            /* Set the i'th bit in the hex string. */
            if( HexStr_SetBit( hs, i ) )
            {
                BUFR_Err_Log( "BitStream_Get" );
                free( (void*) hs );
                return 1;
            }
        }
    }

    /* Store hex string in the given encoded value. */

    EncVal_Init( EV );

    EV->value = hs;
    EV->nbits = BitWidth;

    return 0;

}

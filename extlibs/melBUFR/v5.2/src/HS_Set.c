/*
 * HexStr_Set - VERSION: %I%  %E% %T%
 */
/*
 * HexStr_Set - Convert the given (positive) decimal value to a left-justified
 * hexadecimal string.  Return NULL on error.
 */

#include <mel_bufr.h>
/***************************************************************************/

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:        HexStr_Set
 
  DESCRIPTION:        Convert the given Reference Value (positive) decimal 
			value to a left-justified hexadecimal string.
			Return NULL on error.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	HexStr_t HexStr_RVSet( double Value, int NumBits )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
    Value	      double	   Input	Reference Value
    NumBits		int	   Input	Size in bits of Value
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  NumBits < 1		Write error message to buffer
                        Return with an error
  Value = missing value	Write error message to buffer
                        Return with an error
  Value = negitive 	Write error message to buffer
                        Return with an error
  hex string = NULL	Write error message to buffer
                        Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  TwoPow		Raise value to a power of two
  BytesInBits		Returns the number of bytes in a given number of bits
  TruncateValue		Truncates a value
  HesStr_SetBit		Sets a given bit in a hex string
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  BM            BUFR_Msg_t      BUFR message structure
  i, n          int             loop counter
  MaxVal	double		Maximum possible value
  nbytes	int		Size of value in bytes
  hex_str	HexStr_t	Hexadecimal converted value
  v2		double		value divided by 2
 
  METHOD:
        If any of the input or output variables are NULL then
          Perform BUFR_Err_Set to write an error message to buffer
          Return with error
        Endif
	If input value < 0 then
          Perform BUFR_Err_Set to write an error message to buffer
          Return with error
	Endif
	If the value > the maximum possible value then
	  Print warning message
	  Set value to Missing_Value
	Endif
	Perform BytesInBits to find the number of bytes required
	If the memory allocation for hex_str fails then
	  Write error message
	  Return with error
	Endif
	If the value equals the Missing_Value then
	  set all appropriate bits to indicate missing data
	  return the hex string
	Endif

	Perform TruncateValue to truncate the value
	Loop on number of bits in value
	  Divide value by two
	  Perform TruncateValue to truncate the value
	  If the result of subtracting the divided value and the truncated
		value is not equal to zero then
	    Perform HexStr_SetBit to set that bit
	  Endif
	End loop
	Return the hex string
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    3/05/1996 LAH
      Incorporated fix first tried by Val to elimitate the NULL return
      if value was to large to fit in specified number of bits.  Caused
      program to crash.  At this time did not want program to crash.  We
      may want to add a system variable to indicate which option (crash
      or missing value) the user wishes.

    100997 LAH:  100997 LAH: Added unit_t cast
    101097 LAH:  Made hex strings 32 bits 
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

HexStr_t HexStr_Set( double Value, int NumBits )

#else

HexStr_t HexStr_Set( Value, NumBits )
double Value;
int    NumBits;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    double MaxVal;

    int   nbytes;

    HexStr_t hex_str;
    BUFR_Msg_t* BM;

    int i;
    int n;

    double v2;

/******************************
    int bit_index, byte_index;
*******************************/

    BM = &BUFR_Msg;

    hex_str = NULL;

    if( NumBits < 1 )
    {
        BUFR_Err_Set( "HexStr_Set", "NumBits < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Set", 
	     "NumBits < 1" );
        return NULL;
    }

    if( (int)Value < 0 && (int)Value != BM->Missing_Value )
    {
        BUFR_Err_Set( "HexStr_Set", "Value < 0" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Set", 
	     "Value < 0" );
        return NULL;
    }

    /* Make sure value will fit in specified number of bits. */
    if ( NumBits > 1)
    {
      MaxVal = TwoPow( NumBits ) - 1.;

      if( Value > MaxVal && Value != BM->Missing_Value )
      {
          /*  Removed NULL return: caused program to crash   */
          /* This fix suggested by Val                       */
          /* Incorporated into code 3/8/96                   */

          fprintf( stderr, "HexStr_Set(): WARNING - Value of %f ", Value );
          fprintf( stderr, "is too large to store\nin %d bits.  ", NumBits );
          fprintf( stderr, "Setting it to missing value.\n" );
          fprintf( BUFR_Cntl.bufr_log, "HexStr_Set(): WARNING - Value of %f ", Value );
          fprintf( BUFR_Cntl.bufr_log, "is too large to store\nin %d bits.  ", NumBits );
          fprintf( BUFR_Cntl.bufr_log, "Setting it to missing value.\n" );

          /*
           * Don't set error, it causes programs to halt. (JRA090496)
           *
          BUFR_Err_Set( "HexStr_Set",
              "Value too large to fit within specified bits" );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Set", 
	       "Value too large to fit within specified bits" );
           */

          Value = BM->Missing_Value;
      }
    } else {
      if ( Value < 1 )
      {
        Value = 0;
      } else {
        Value = 1;
      }
    }
    /* Compute bytes required to store value and allocate memory for it. */

    nbytes = BytesInBits( NumBits );

    /* 100997 LAH: Added unit_t cast */
    if( (hex_str = (HexStr_t) malloc( (uint_t) nbytes )) == NULL )
    {
        BUFR_Err_Set( "HexStr_Set", "Can't allocate memory" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Set", 
	     "Can't allocate memory" );
        return NULL;
    }

    /*
    * SPECIAL CASE: If Value == BM->Missing_Value, then set all appropriate bits
     * to indicate missing data and return the hex string.
     */

    if( Value == BM->Missing_Value )
    {
        n = nbytes - 1;

        for( i=0; i < n; i++ )
            hex_str[i] = 0x000000FF;

        switch( (NumBits % BITS_IN_BYTE) )
        {
            case 0: hex_str[n] = 0x000000FF; break;
            case 1: hex_str[n] = 0x00000080; break;
            case 2: hex_str[n] = 0x000000C0; break;
            case 3: hex_str[n] = 0x000000E0; break;
            case 4: hex_str[n] = 0x000000F0; break;
            case 5: hex_str[n] = 0x000000F8; break;
            case 6: hex_str[n] = 0x000000FC; break;
            case 7: hex_str[n] = 0x000000FE; break;
        }

        return hex_str;
    }

    /*
     * Set bits in hex_str corresponding to 'Value'.  First, clear the byte
     * string and truncate 'Value' to ensure that the process begins with
     * a whole number (this algorithm will fail if it doesn't).  Next,
     * repeatedly determine if 'Value' is an odd number and divide by 2 to
     * effectively shift the value right by 1 bit.  Whenever the value is
     * odd, set the bit corresponding to the current index..
     */

    /* 100997 LAH: Added unit_t cast */
    (void) memset( (char*) hex_str, 0, (uint_t)nbytes );

    Value = TruncateValue( Value );

    /*for( i=0; i < NumBits && Value != 0; i++ )*/
    for( i=NumBits-1; i >= 0 && (int)Value != 0; i-- )
    {
        v2 = Value / 2.0;

        Value = TruncateValue( v2 );

        if( (v2-Value) != 0.0 )
        {
/*******************************************************
            byte_index = (nbytes-1) - i / BITS_IN_BYTE;
            bit_index  = i % BITS_IN_BYTE;

            hex_str[byte_index] |= (1 << bit_index);
*******************************************************/
            HexStr_SetBit( hex_str, i );
        }
    }

    return hex_str;
}

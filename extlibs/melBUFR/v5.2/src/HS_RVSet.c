/*
 * HexStr_RVSet - VERSION: %I%  %E% %T%
 */
/*
 * HexStr_RVSet - Convert the given Reference Value decimal value to a 
 * hexadecimal string.  If positive, normal hex string is created.  If
 * negative, the absolut value is encoded normally and the left most bit
 * is set to one.  Return NULL on error.
 */


#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:        HexStr_RVSet
 
  DESCRIPTION:        Convert the given Reference Value decimal value to a
			hexadecimal string.  If positive, normal hex string 
			is created.  If negative, the absolute value is 
			encoded normally and the left most bit is set to one.  
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
  hex string = NULL	Write error message to buffer
                        Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  TwoPow		Raise value to a power of two
  BUFR_BitWidth		Returns the number of bits in a value
  BytesInBits		Returns the number of bytes in a given number of bits
  TruncateValue		Truncates a value
  HesStr_SetBit		Sets a given bit in a hex string
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  BM            BUFR_Msg_t      BUFR message structure
  i             int             loop counter
  MaxVal	double		Maximum possible value
  abs_val	int		Absolute value
  nbytes	int		Size of value in bytes
  message	char		Error message buffer
  hex_str	HexStr_t	Hexadecimal converted value
  v2		double		value divided by 2
 
  METHOD:
        If any of the input or output variables are NULL then
          Perform BUFR_Err_Set to write an error message to buffer
          Return with error
        Endif
	If input value < 0 then
	  set abs_val to a positive value
	  Perform TwoPow to find the maximum possible value
	Else
	  set abs_val to value
	  Perform TwoPow to find the maximum possible value
	Endif
	If the absolute value > the maximum possible value then
	  Print warning message
	  Perform BUFR_BitWidth to get the value's width again
	  If input value < 0 then
	    set abs_val to a positive value
	    Perform TwoPow to find the maximum possible value
	  Else
	    set abs_val to value
	    Perform TwoPow to find the maximum possible value
	  Endif
	  If the absolute value > the maximum possible value then
	    Write error message
	    return with error
	  Endif
	Endif
	Perform BytesInBits to find the number of bytes required
	If the memory allocation for hex_str fails then
	  Write error message
	  Return with error
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
	If the value is  < zero then
	  Perform HexStr_SetBit to set sign bit
	Endif
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
     030596 LAH:
        Incorporated fix first tried by Val to elimitate the NULL return
        if value was to large to fit in specified number of bits.  Caused
        program to crash.  At this time did not want program to crash.  We
        may want to add a system variable to indicate which option (crash
        or missing value) the user wishes.

    102097 LAH: Removed unused variable n
                Added u_int, int casts
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

HexStr_t HexStr_RVSet( double Value, int NumBits )

#else

HexStr_t HexStr_RVSet( Value, NumBits )
double Value;
int    NumBits;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    double MaxVal;
    int abs_val;

    int   nbytes;
    char  message[120];

    HexStr_t hex_str;
    BUFR_Msg_t* BM;

    int i;
    int N_Bits, count;

    double v2;

/******************************
    int bit_index, byte_index;
*******************************/

    BM = &BUFR_Msg;

    hex_str = NULL;

    if( NumBits < 1 )
    {
        BUFR_Err_Set( "HexStr_Set", "NumBits < 1" );
        return NULL;
    }
    
    if( Value == BM->Missing_Value )
    {
       BUFR_Err_Set("HexStr_RVSet",
           "Reference Value can not be equal to MISSING_VALUE");
       fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_RVSet", 
	     "Reference Value can not be equal to MISSING_VALUE" );
       return NULL;
    }

    if( Value < 0 )
    {
        /* 102097 LAH: Added int cast */
        abs_val = (int)(-Value);
        /* Make sure value will fit in specified number of bits. */
        MaxVal = TwoPow( NumBits -1) - 1;
	/* an extra bit was added to allow for the "sign" bit  */
    } else {
        /* 102097 LAH: Added int cast */
	abs_val = (int) Value;
        /* Make sure value will fit in specified number of bits. */
        MaxVal = TwoPow( NumBits ) - 1;
    }
    
    N_Bits = NumBits;
    
    /* 102097 LAH: Added double cast */
    if( (double) abs_val > MaxVal )
    {
        /*  BUFR_BitWidth should have been called in BUFR_Change_RefVal */
        /*  A warning will be printed and BUFR_BitWidth called and the  */
        /*  redone.  If it fails again, error out.                      */


        fprintf( stderr, "HexStr_RVSet(): WARNING - Value of %f ", Value );
        fprintf( stderr, "is too large to store\nin %d bits.\n", NumBits );
        fprintf( stderr, "This function should be called by EncVal_RVSet ");
        fprintf( stderr, "which should be called \nby BUFR_Change_RefVal. ");
	fprintf( stderr, " BUFR_BitWidth should be called before the call");
	fprintf( stderr, " to EncVal_RVSet. \nIf a different sequence of ");
	fprintf( stderr, "is being used,  besure to call BUFR_BitWidth\n");
	fprintf( stderr, "before calling HexStr_RVSet.\n");
	

        /* 102097 LAH: Added int cast */
        N_Bits = BUFR_BitWidth( (int) Value);
	if( Value < 0 )
        {
           /* 102097 LAH: Added int cast */
           abs_val = (int)(-Value);
           /* Make sure value will fit in specified number of bits. */
           MaxVal = TwoPow( N_Bits -1 ) - 1;
	   /* an extra bit was added to allow for the "sign" bit  */
         } else {
           /* 102097 LAH: Added int cast */
	   abs_val = (int) Value;
           /* Make sure value will fit in specified number of bits. */
           MaxVal = TwoPow( N_Bits ) - 1;
         }

         /* 102097 LAH: Added double cast */
	 if( (double)abs_val > MaxVal ){
	    sprintf(message,"Bitwidth of %d too small for reference value %d.",
	    N_Bits, (int) Value); 
	    BUFR_Err_Set("HexStr_RVSet",message);
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_RVSet", message);
            return NULL;
	 }
    }

    /* Compute bytes required to store value and allocate memory for it. */

    nbytes = BytesInBits( N_Bits );

    /* 102097 LAH: Added uint_t cast */
    if( (hex_str = (HexStr_t) malloc( (uint_t)nbytes )) == NULL )
    {
        BUFR_Err_Set( "HexStr_RVSet", "Can't allocate memory" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_RVSet",
	    "Can't allocate memory" );
        return NULL;
    }

    /*
     * Set bits in hex_str corresponding to abs_val.  First, clear the byte
     * string and truncate abs_val to ensure that the process begins with
     * a whole number (this algorithm will fail if it doesn't).  Next,
     * repeatedly determine if abs_val is an odd number and divide by 2 to
     * effectively shift the value right by 1 bit.  Whenever the value is
     * odd, set the bit corresponding to the current index.  If 'Value'
     * was negative, then set the left most bit of N_Bits to 1.  An extra
     * bit was included in the bit width to allow for this if Value was
     * negative.
     */

    /* 102097 LAH: Added uint_t cast */
    (void) memset( (char*) hex_str, 0, (uint_t) nbytes );

    /* 102097 LAH: Added int & double casts */
    abs_val = (int) TruncateValue( (double) abs_val );

    count = N_Bits;
 /*   if ( Value < 0 ) count = count-1;  */
    for( i=count-1; i >= 0 && abs_val != 0; i-- )
    {
        /* 102097 LAH: Added double cast */
        v2 = (double)abs_val / 2.0;

        /* 102097 LAH: Added int cast */
        abs_val = (int) TruncateValue( v2 );

        /* 102097 LAH: Added double cast */
        if( (v2- (double)abs_val) != 0.0 )
        {
/*******************************************************
            byte_index = (nbytes-1) - i / BITS_IN_BYTE;
            bit_index  = i % BITS_IN_BYTE;

            hex_str[byte_index] |= (1 << bit_index);
*******************************************************/
            HexStr_SetBit( hex_str, i );
        }
    }
    if ( Value < 0 )
    	HexStr_SetBit( hex_str, 0 );
	
       return hex_str;
}

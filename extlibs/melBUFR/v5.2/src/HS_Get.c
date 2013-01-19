/*
 * HexStr_Get - VERSION: %I%  %E% %T%
 */
/*
 * HexStr_Get - Convert the given hexadecimal string to a decimal value.
 * Return 1 on error, otherwise 0.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         HexStr_Get
 
  DESCRIPTION:         HexStr_Get - Convert the given hexadecimal string to 
			a decimal value.  Return 1 on error, otherwise 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin

  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int HexStr_Get( HexStr_t HS, int NumBits, double* Value )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
    HS			HexStr_t   input	Hexidecimal string
    NumBits		int	   input	Size of HS in bits
    Value		double	   output	Decimal value of HS
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  HS is Null		Write error message to buffer
                        Return with error code
  NumBits < 1		Write error message to buffer
                        Return with error code
  Value = NULL		Write error message to buffer
                        Return with error code
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  HexStr_GetBit		Get given bit within a HexStr_t
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  i, p		int		loop counter
  d		double		decimal value
  num_bits_set	int		counter for number of bits processed
 
  METHOD:
	If any of the input or output variables are NULL then
	  Perform BUFR_Err_Set to write an error message to buffer
          Return with error
        Endif
	Set num_bits_set to zero
	Loop on the number of bits in the hex string
	  If a particular bit is set to 1  then
	    increment num_bits_set
	    add to d the position of that bit raised to the power of 2
	  Endif
	End loop
	If all bits were set then
	  return the MISSING_VALUE value
	Else
	  set Value to d
	Endif

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    022498 LAH:  Added prints to bufr_log file.
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int HexStr_Get( HexStr_t HS, int NumBits, double* Value, int* m_flag)

#else

int HexStr_Get( HS, NumBits, Value, m_flag)
HexStr_t HS;
int      NumBits;
double*  Value;
int *m_flag;
#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    int i;
    int p;
    int num_bits_set;

    double d;

    if( HS == NULL )
    {
        BUFR_Err_Set( "HexStr_Get", "NULL HexStr_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Get", 
	     "NULL HexStr_t pointer" );
        return 1;
    }

    if( NumBits < 1 )
    {
        BUFR_Err_Set( "HexStr_Get", "NumBits < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Get", 
	     "NumBits < 1" );
        return 1;
    }

    if( Value == NULL )
    {
        BUFR_Err_Set( "HexStr_Get", "NULL Value pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_Get", 
	     "NULL Value pointer" );
        return 1;
    }

    num_bits_set = 0;

    for( i=0, p=NumBits-1, d=0.0; i < NumBits; i++, p-- )
    {
        if( HexStr_GetBit( HS, i ) == 1 )
        {
            num_bits_set++;
            d += TwoPow( p );
        }
    }

    /* If all bits were set,set missing value flag. */
    /* LAH 112601 - changed to leave value alone and return missing 
     * value flag (m_flag) instead.
     */
     
    *Value = d;
    *m_flag = 1;
    if( (num_bits_set == NumBits) && (NumBits > 1) )
        *m_flag = 0;
   
    return 0;
}

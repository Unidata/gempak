/*
 * EncodeValue - VERSION: %I%  %E% %T%
 */
/*
 * EncodeValue - Encode a Table B value in accordance with BUFR regulations.
 *
 * EncodedValue = Value * pow(10,scale) - ref_val
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         EncodeValue
 
  DESCRIPTION:         Encode a Table B value in accordance with 
			BUFR regulations.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	EncVal_t EncodeValue( void* Value, DataType_t Type, FXY_t FXY_Val )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  Value			void	    In	     Value to be encoded
  Type		      DataType_t    In	     Data type of EncodedValue
  FXY_Val             FXY_t         In       FXY value of EncodedValue
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No Encoded Value      Write error message
                        Return with error
  Bad return from	Write error message
  EncVal_Set            Return with error
  Bad return from	Write error message
  GXY_Get_Value         Return with error
  Bad return from	Write error message
  VoidVal               Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Places error message into a buffer
  VoidVal		Converts void pointer from one type to another
  EncVal_Set		BUFR-encodes the given value
  EncVal_Init		Initializes an encoded value
  FXY_Get_Values	Get scale, reference value, and data width for a
			given FXY value.
  BUFR_Err_Log		Sends an error message to the error log
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  scale		int		scale for a given FXY
  ref_val	int		reference value for a given FXY
  data_width	int		Data width  for a given FXY
  d		double		value converted to a double
  i		int		value converted to an integer
  bad_val	EncVal_t	bad value pointer
  result	EncVal_t	result of encoding
 
  METHOD:
	Perform EncVal_Init to initialize the bad_val pointer
	Perform EncVal_Init to initialize the result pointer
	If the input value is NULL then
	  Perform BUFR_Err_Set to write error message to a buffer
	  return with a bad value set
	Endif
	If the incoming FXY equals the missing value indicator then
	  if error while Performing VoidVal to convert to an integer type then
	    Perform BUFR_Err_Log to write error message to a log
	    return with a bad value set
	  Endif
	  If error while Perform EncVal_Set to BUFR encode the given value  then
	    Perform BUFR_Err_Log to write error message to a log
	  Endif
	  return with the result
	Endif
	If error while Performing FXY_Get_Values then
	  Perform BUFR_Err_Log to write error message to a log
	  return with a bad value set
	Endif
	If error while Performing VoidVal then
	  Perform BUFR_Err_Log to write error message to a log
	  return with a bad value set
	Endif
	If error while Performing EncValSet then
	  Perform BUFR_Err_Log to write error message to a log
	  return with a bad value set
	Endif
	Return with the result

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    100897 LAH: Added double cast 
    102097 LAH: Added double cast
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

EncVal_t EncodeValue( void* Value, DataType_t Type, FXY_t FXY_Val )

#else

EncVal_t EncodeValue( Value, Type, FXY_Val )
void*       Value;
DataType_t  Type;
FXY_t       FXY_Val;

#endif
{
    int scale, ref_val, data_width;
    extern BUFR_Cntl_t BUFR_Cntl;

    double d;
    int    i;

    EncVal_t bad_val;
    EncVal_t result;

    EncVal_Init( &bad_val );
    EncVal_Init( &result );

    if( Value == NULL )
    {
        BUFR_Err_Set( "EncodeValue", "NULL pointer for Value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "EncodeValue", 
            "NULL pointer for Value" );
        return bad_val;
    }

    if( FXY_Val == (FXY_t) NO_FXY_VAL )
    {
        /* Value has no associated FXY.  Encode it as an int. */

        if( VoidVal( Value, Type, (void*)&i, DT_INT ) )
        {
            BUFR_Err_Log( "EncodeValue" );
            return bad_val;
        }

        /* 102097 LAH: Added double cast */
        d = (double) i;
        if( EncVal_Set( &result, d, 0, 0, sizeof(int)*BITS_IN_BYTE ) )
            BUFR_Err_Log( "EncodeValue" );

        return result;
    }

    if( FXY_Get_Values( FXY_Val, &scale, &ref_val, &data_width ) )
    {
        BUFR_Err_Log( "EncodeValue" );
        return bad_val;
    }

    /* Convert 'Value' to a double */

    if( VoidVal( Value, Type, (void*)&d, DT_DOUBLE ) )
    {
        BUFR_Err_Log( "EncodeValue" );
        return bad_val;
    }

    if( EncVal_Set( &result, d, scale, ref_val, data_width ) )
    {
        BUFR_Err_Log( "EncodeValue" );
        return bad_val;
    }

    return result;
}

/*
 * VoidVal - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
 * Convert void pointer from one type to another.
 * Return 1 on error, else 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#if PROTOTYPE_NEEDED

int VoidVal( void* InVal,  DataType_t InType,
             void* OutVal, DataType_t OutType )

#else

int VoidVal( InVal, InType, OutVal, OutType )
void*      InVal;       /* Input value                */
DataType_t InType;      /* Data type for input value  */
void*      OutVal;      /* Output value               */
DataType_t OutType;     /* Data type for output value */

#endif
{
    double d;

    if( InVal == NULL )
    {
        BUFR_Err_Set( "VoidVal", "NULL pointer for input value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "VoidVal", 
	     "NULL pointer for input value" );
        return 1;
    }

    if( OutVal == NULL )
    {
        BUFR_Err_Set( "VoidVal", "NULL pointer for output value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "VoidVal", 
	     "NULL pointer for output value" );
        return 1;
    }

    /* Convert input value to a double */

    switch( InType )
    {
        case DT_CHAR:   d = (double) *((char*)     InVal); break;
        case DT_DOUBLE: d = (double) *((double*)   InVal); break;
        case DT_FLOAT:  d = (double) *((float*)    InVal); break;
        case DT_LONG:   d = (double) *((long*)     InVal); break;
        case DT_SHORT:  d = (double) *((short*)    InVal); break;
        case DT_UCHAR:  d = (double) *((uchar_t*)  InVal); break;
        case DT_ULONG:  d = (double) *((ulong_t*)  InVal); break;
        case DT_USHORT: d = (double) *((ushort_t*) InVal); break;

        default:
            BUFR_Err_Set( "VoidVal", "Invalid input DataType_t" );
            return 1;
    }

    switch( OutType )
    {
        case DT_CHAR:   *((char*)     OutVal) = (char)     d; break;
        case DT_DOUBLE: *((double*)   OutVal) =            d; break;
        case DT_FLOAT:  *((float*)    OutVal) = (float)    d; break;
        case DT_LONG:   *((long*)     OutVal) = (long)     d; break;
        case DT_SHORT:  *((short*)    OutVal) = (short)    d; break;
        case DT_UCHAR:  *((uchar_t*)  OutVal) = (uchar_t)  d; break;
        case DT_ULONG:  *((ulong_t*)  OutVal) = (ulong_t)  d; break;
        case DT_USHORT: *((ushort_t*) OutVal) = (ushort_t) d; break;

        default:
            BUFR_Err_Set( "VoidVal", "Invalid output DataType_t" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "VoidVal", 
	        "Invalid output DataType_t" );
            return 1;
    }

    return 0;
}

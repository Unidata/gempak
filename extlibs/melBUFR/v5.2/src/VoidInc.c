/*
 * VoidInc - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/* Increment void pointer by appropriate amount. */

#if PROTOTYPE_NEEDED

void VoidInc( void** VoidPtr, DataType_t DataType )

#else

void VoidInc( VoidPtr, DataType )
void**     VoidPtr;     /* Pointer to be incremented */
DataType_t DataType;    /* What VoidPtr points to */

#endif
{
    char* cp;

    cp = (char*) *VoidPtr;

    if( cp == NULL )
        return;

    switch( DataType )
    {
        case DT_CHAR:   cp += sizeof( char     ); break;
        case DT_UCHAR:  cp += sizeof( uchar_t  ); break;
        case DT_SHORT:  cp += sizeof( short    ); break;
        case DT_USHORT: cp += sizeof( ushort_t ); break;
        case DT_LONG:   cp += sizeof( long     ); break;
        case DT_ULONG:  cp += sizeof( ulong_t  ); break;
        case DT_FLOAT:  cp += sizeof( float    ); break;
        case DT_DOUBLE: cp += sizeof( double   ); break;

        default:
            return;
    }

    *VoidPtr = (void*) cp;
}
